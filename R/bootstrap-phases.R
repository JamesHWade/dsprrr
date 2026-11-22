# BootstrapFewShot orchestration phases
#
# The public compiler entry points live in teleprompter-bootstrap.R. These
# helpers keep validation/planning, candidate execution, checkpointing, and
# finalization separate while retaining the original transition order.

bootstrap_validate_inputs <- function(teleprompter, program, trainset) {
  if (!inherits(program, "Module")) {
    cli::cli_abort(
      "BootstrapFewShot currently only supports Module objects",
      call = rlang::caller_env()
    )
  }
  bootstrap_assert_demo_eligible(program)

  if (!is.data.frame(trainset)) {
    cli::cli_abort(
      "{.arg trainset} must be a data frame",
      call = rlang::caller_env()
    )
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn(
      "Empty trainset provided, returning unmodified program",
      call = rlang::caller_env()
    )
    return(list(empty = TRUE))
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort(
      "BootstrapFewShot requires a metric function",
      call = rlang::caller_env()
    )
  }

  list(empty = FALSE, pipeline = inherits(program, "PipelineModule"))
}

bootstrap_begin_checkpoint <- function(
  teleprompter,
  program,
  trainset,
  valset,
  .llm,
  control,
  checkpoint_context,
  .optimizer_budget,
  mode,
  teacher_settings = NULL
) {
  if (
    is.null(checkpoint_context) &&
      is.null(.optimizer_budget) &&
      optimizer_checkpoint_enabled(control)
  ) {
    effective_runtime <- optimizer_checkpoint_effective_runtime_identity(
      program,
      .llm = .llm,
      registry = artifact_validate_registry(control@checkpoint_registry),
      path = "effective_runtime"
    )
    config <- if (identical(mode, "module")) {
      list(
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        max_rounds = teleprompter@max_rounds,
        metric_threshold = teleprompter@metric_threshold,
        teacher_settings = teacher_settings,
        seed = teleprompter@seed,
        effective_runtime = effective_runtime,
        mode = mode
      )
    } else {
      list(
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        max_rounds = teleprompter@max_rounds,
        metric_threshold = teleprompter@metric_threshold,
        seed = teleprompter@seed,
        effective_runtime = effective_runtime,
        mode = mode
      )
    }

    initial_state <- if (identical(mode, "module")) {
      list(
        kind = "module",
        bootstrapped_demos = list(),
        lineage = list()
      )
    } else {
      list(
        kind = "pipeline",
        step_demos = list(),
        lineage = list()
      )
    }
    checkpoint_context <- optimizer_checkpoint_begin(
      optimizer_name = "BootstrapFewShot",
      optimizer_version = 1L,
      program = program,
      data = list(trainset = trainset, valset = valset),
      metric = teleprompter@metric,
      config = config,
      control = control,
      initial_state = initial_state,
      initial_phase = "bootstrap"
    )
    if (identical(checkpoint_context$phase, "complete")) {
      return(list(
        complete = TRUE,
        context = checkpoint_context,
        program = checkpoint_context$best_program
      ))
    }
  }

  list(complete = FALSE, context = checkpoint_context, program = NULL)
}

bootstrap_initialize_trial_log <- function(teleprompter) {
  if (is.null(teleprompter@log_dir)) {
    return(NULL)
  }
  TrialLog$new(
    optimizer_name = "BootstrapFewShot",
    log_dir = teleprompter@log_dir
  )
}

bootstrap_module_plan <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  control = NULL,
  .optimizer_budget = NULL,
  .checkpoint_context = NULL,
  .checkpoint_namespace = "bootstrap",
  .resume_state = NULL,
  .checkpoint_callback = NULL
) {
  teacher_settings <- teleprompter@teacher_settings %||%
    list(temperature = 0.7)
  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control
  )
  checkpoint <- bootstrap_begin_checkpoint(
    teleprompter = teleprompter,
    program = program,
    trainset = trainset,
    valset = valset,
    .llm = .llm,
    control = control,
    checkpoint_context = .checkpoint_context,
    .optimizer_budget = .optimizer_budget,
    mode = "module",
    teacher_settings = teacher_settings
  )
  if (checkpoint$complete) {
    return(list(
      complete = TRUE,
      program = checkpoint$program,
      checkpoint_context = checkpoint$context
    ))
  }
  checkpoint_context <- checkpoint$context

  trial_log <- bootstrap_initialize_trial_log(teleprompter)
  trainset <- sample_dataset(trainset, n = NULL, seed = teleprompter@seed)
  student <- copy_module(program)
  teacher <- copy_module(program)
  input_names <- vapply(
    program$signature@inputs,
    function(x) x$name,
    character(1)
  )
  metric_field <- get_metric_field(teleprompter@metric)
  output_col <- metric_field %||% find_output_column(trainset, input_names)

  if (is.null(output_col)) {
    cli::cli_warn(
      c(
        "No output column found in trainset",
        "i" = "Expected one of: 'output', 'label', 'answer', 'response', 'result', 'y'",
        "i" = "Or any column not in inputs: {.val {input_names}}",
        "i" = "Available columns: {.val {names(trainset)}}",
        "!" = "Metric will receive NULL as expected value for all examples"
      ),
      class = "dsprrr_missing_output_column",
      call = rlang::caller_env()
    )
  }

  n_labeled <- min(teleprompter@max_labeled_demos, nrow(trainset))
  labeled_demos <- list()
  if (n_labeled > 0) {
    labeled_data <- trainset[seq_len(n_labeled), , drop = FALSE]
    labeled_demos <- format_trainset_as_demos(
      labeled_data,
      program$signature,
      output_col = output_col
    )
    for (i in seq_along(labeled_demos)) {
      labeled_demos[[i]]$source <- "labeled"
      labeled_demos[[i]]$score <- NA_real_
      labeled_demos[[i]]$round <- 0L
    }
  }
  bootstrap_indices <- if (n_labeled < nrow(trainset)) {
    seq(n_labeled + 1, nrow(trainset))
  } else {
    integer(0)
  }

  checkpoint_state <- checkpoint_context$search_state %||%
    .resume_state %||%
    list()
  if (
    length(checkpoint_state) > 0L &&
      !identical(checkpoint_state$kind %||% "module", "module")
  ) {
    cli::cli_abort(
      "Bootstrap checkpoint has the wrong search-state kind",
      class = "dsprrr_optimizer_checkpoint_malformed",
      call = rlang::caller_env()
    )
  }
  bootstrapped_demos <- checkpoint_state$bootstrapped_demos %||% list()
  bootstrap_lineage <- checkpoint_state$lineage %||% list()
  budget <- .optimizer_budget %||%
    checkpoint_context$budget %||%
    new_optimizer_budget(control)

  list(
    complete = FALSE,
    teleprompter = teleprompter,
    program = program,
    trainset = trainset,
    valset = function() valset,
    llm = function() .llm,
    control = control,
    checkpoint_context = checkpoint_context,
    checkpoint_namespace = function() .checkpoint_namespace,
    checkpoint_callback = function() .checkpoint_callback,
    teacher_settings = teacher_settings,
    trial_log = trial_log,
    student = student,
    teacher = teacher,
    input_names = input_names,
    metric_field = metric_field,
    output_col = output_col,
    labeled_demos = labeled_demos,
    n_labeled = n_labeled,
    bootstrap_indices = bootstrap_indices,
    bootstrapped_demos = bootstrapped_demos,
    bootstrap_lineage = bootstrap_lineage,
    budget = budget
  )
}

bootstrap_pipeline_plan <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  control = NULL,
  .optimizer_budget = NULL,
  .checkpoint_context = NULL,
  .checkpoint_namespace = "bootstrap_pipeline",
  .resume_state = NULL,
  .checkpoint_callback = NULL
) {
  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control
  )
  checkpoint <- bootstrap_begin_checkpoint(
    teleprompter = teleprompter,
    program = program,
    trainset = trainset,
    valset = valset,
    .llm = .llm,
    control = control,
    checkpoint_context = .checkpoint_context,
    .optimizer_budget = .optimizer_budget,
    mode = "pipeline"
  )
  if (checkpoint$complete) {
    return(list(
      complete = TRUE,
      program = checkpoint$program,
      checkpoint_context = checkpoint$context
    ))
  }
  checkpoint_context <- checkpoint$context

  trial_log <- bootstrap_initialize_trial_log(teleprompter)
  trainset <- sample_dataset(trainset, n = NULL, seed = teleprompter@seed)
  teacher <- copy_module(program)
  student <- copy_module(program)
  pipeline_inputs <- vapply(
    program$signature@inputs,
    function(x) x$name,
    character(1)
  )
  metric_field <- get_metric_field(teleprompter@metric)
  output_col <- metric_field %||%
    find_output_column(trainset, pipeline_inputs)

  if (is.null(output_col)) {
    cli::cli_warn(
      c(
        "No output column found in trainset",
        "i" = "Expected one of: 'output', 'label', 'answer', 'response', 'result', 'y'",
        "i" = "Or any column not in inputs: {.val {pipeline_inputs}}",
        "!" = "Metric will receive NULL as expected value for all examples"
      ),
      class = "dsprrr_missing_output_column",
      call = rlang::caller_env()
    )
  }

  n_steps <- length(program$steps)
  demo_steps <- which(vapply(
    program$steps,
    function(step) "demos" %in% names(step@module),
    logical(1)
  ))
  if (length(demo_steps) == 0) {
    cli::cli_warn(c(
      "No pipeline step supports demonstrations",
      "i" = "BootstrapFewShot requires at least one step module with a {.field demos} field",
      "!" = "Returning unmodified program"
    ))
    return(list(
      complete = TRUE,
      program = program,
      checkpoint_context = checkpoint_context
    ))
  }

  labeled_demos <- stats::setNames(
    rep(list(list()), n_steps),
    as.character(seq_len(n_steps))
  )
  final_step <- n_steps
  n_labeled <- 0L
  if (teleprompter@max_labeled_demos > 0 && final_step %in% demo_steps) {
    final_module <- program$steps[[final_step]]@module
    final_inputs <- vapply(
      final_module$signature@inputs,
      function(x) x$name,
      character(1)
    )
    if (all(final_inputs %in% names(trainset))) {
      n_labeled <- min(teleprompter@max_labeled_demos, nrow(trainset))
      labeled_data <- trainset[seq_len(n_labeled), , drop = FALSE]
      demos <- format_trainset_as_demos(
        labeled_data,
        final_module$signature,
        output_col = output_col
      )
      for (i in seq_along(demos)) {
        demos[[i]]$source <- "labeled"
        demos[[i]]$score <- NA_real_
        demos[[i]]$round <- 0L
      }
      labeled_demos[[as.character(final_step)]] <- demos
    }
  }
  bootstrap_indices <- if (n_labeled < nrow(trainset)) {
    seq(n_labeled + 1, nrow(trainset))
  } else {
    integer(0)
  }

  checkpoint_state <- checkpoint_context$search_state %||%
    .resume_state %||%
    list()
  if (
    length(checkpoint_state) > 0L &&
      !identical(checkpoint_state$kind %||% "pipeline", "pipeline")
  ) {
    cli::cli_abort(
      "Bootstrap checkpoint has the wrong pipeline search-state kind",
      class = "dsprrr_optimizer_checkpoint_malformed",
      call = rlang::caller_env()
    )
  }
  step_demos <- checkpoint_state$step_demos
  if (is.null(step_demos) || length(step_demos) == 0L) {
    step_demos <- stats::setNames(
      rep(list(list()), n_steps),
      as.character(seq_len(n_steps))
    )
  }
  if (!identical(names(step_demos), as.character(seq_len(n_steps)))) {
    cli::cli_abort(
      "Bootstrap pipeline checkpoint step state is incompatible",
      class = "dsprrr_optimizer_checkpoint_malformed",
      call = rlang::caller_env()
    )
  }
  bootstrap_lineage <- checkpoint_state$lineage %||% list()
  budget <- .optimizer_budget %||%
    checkpoint_context$budget %||%
    new_optimizer_budget(control)

  list(
    complete = FALSE,
    teleprompter = teleprompter,
    program = program,
    trainset = trainset,
    valset = function() valset,
    llm = function() .llm,
    control = control,
    checkpoint_context = checkpoint_context,
    checkpoint_namespace = function() .checkpoint_namespace,
    checkpoint_callback = function() .checkpoint_callback,
    trial_log = trial_log,
    teacher = teacher,
    student = student,
    pipeline_inputs = pipeline_inputs,
    metric_field = metric_field,
    output_col = output_col,
    n_steps = n_steps,
    demo_steps = demo_steps,
    final_step = final_step,
    labeled_demos = labeled_demos,
    n_labeled = n_labeled,
    bootstrap_indices = bootstrap_indices,
    step_demos = step_demos,
    bootstrap_lineage = bootstrap_lineage,
    budget = budget
  )
}

bootstrap_write_checkpoint <- function(
  kind,
  student,
  labeled_demos,
  bootstrapped_demos = NULL,
  step_demos = NULL,
  demo_steps = integer(),
  bootstrap_lineage,
  checkpoint_context,
  checkpoint_callback,
  budget,
  phase = "bootstrap"
) {
  partial <- copy_module(student)
  if (identical(kind, "module")) {
    partial$demos <- c(labeled_demos, bootstrapped_demos)
    state <- list(
      kind = "module",
      bootstrapped_demos = bootstrapped_demos,
      lineage = bootstrap_lineage
    )
  } else {
    for (i in demo_steps) {
      key <- as.character(i)
      partial$steps[[i]]@module$demos <- c(
        labeled_demos[[key]],
        step_demos[[key]]
      )
    }
    state <- list(
      kind = "pipeline",
      step_demos = step_demos,
      lineage = bootstrap_lineage
    )
  }
  if (!is.null(checkpoint_context)) {
    optimizer_checkpoint_write(
      checkpoint_context,
      phase = phase,
      search_state = state,
      lineage = bootstrap_lineage,
      best_program = partial
    )
  }
  if (is.function(checkpoint_callback)) {
    checkpoint_callback(
      state = state,
      phase = phase,
      best_program = partial,
      budget = budget
    )
  }
  invisible(NULL)
}

bootstrap_threshold_pass <- function(teleprompter, score) {
  if (is.na(score)) {
    return(FALSE)
  }
  if (!is.null(teleprompter@metric_threshold)) {
    score >= teleprompter@metric_threshold
  } else {
    score > 0
  }
}

bootstrap_module_execute <- function(plan) {
  teleprompter <- plan$teleprompter
  trainset <- plan$trainset
  teacher <- plan$teacher
  labeled_demos <- plan$labeled_demos
  teacher_settings <- plan$teacher_settings
  input_names <- plan$input_names
  metric_field <- plan$metric_field
  output_col <- plan$output_col
  bootstrap_indices <- plan$bootstrap_indices
  bootstrapped_demos <- plan$bootstrapped_demos
  bootstrap_lineage <- plan$bootstrap_lineage
  budget <- plan$budget

  for (round in seq_len(teleprompter@max_rounds)) {
    if (
      length(bootstrap_indices) == 0 ||
        length(bootstrapped_demos) >= teleprompter@max_bootstrapped_demos ||
        optimizer_budget_stopped(budget)
    ) {
      break
    }
    teacher$demos <- c(labeled_demos, bootstrapped_demos)

    for (idx in bootstrap_indices) {
      if (optimizer_budget_stopped(budget)) {
        break
      }
      unit_id <- paste0(
        plan$checkpoint_namespace(),
        ":round:",
        round,
        ":row:",
        idx
      )
      if (optimizer_budget_unit_completed(budget, unit_id)) {
        next
      }
      min_provider_calls <- optimizer_min_provider_calls(teacher)
      planned <- list(trials = 1L, metric_calls = 1L)
      if (min_provider_calls > 0L) {
        planned$provider_calls <- min_provider_calls
        planned$input_tokens <- 1L
        planned$output_tokens <- 1L
        planned$total_tokens <- 1L
      }
      if (
        !optimizer_budget_preflight(
          budget,
          stage = "bootstrap_attempt",
          planned = planned,
          unit_id = unit_id,
          work_unit = "bootstrap_training_row",
          max_started = 0L
        )
      ) {
        break
      }

      row <- trainset[idx, , drop = FALSE]
      example_inputs <- list()
      for (name in input_names) {
        if (name %in% names(row)) {
          example_inputs[[name]] <- row[[name]]
        }
      }
      expected <- if (!is.null(metric_field) && metric_field %in% names(row)) {
        row
      } else if (!is.null(output_col) && output_col %in% names(row)) {
        row[[output_col]]
      } else {
        NULL
      }

      teacher_condition <- NULL
      trace_count_before <- evaluation_trace_cursor(teacher)
      result <- tryCatch(
        run_with_settings(
          teacher,
          example_inputs,
          .llm = plan$llm(),
          settings = teacher_settings
        ),
        error = function(e) {
          teacher_condition <<- e
          cli::cli_warn(
            c(
              "Bootstrap attempt failed",
              "x" = conditionMessage(e),
              "i" = "The training-row attempt will count as failed"
            ),
            class = "dsprrr_bootstrap_warning"
          )
          NULL
        }
      )
      if (is.null(result)) {
        record_optimizer_usage(
          budget,
          optimizer_unknown_provider_usage(),
          stage = "bootstrap_teacher",
          unit_id = unit_id,
          work_unit = "bootstrap_training_row",
          max_started = 1L
        )
        optimizer_budget_count_trial(budget, "bootstrap_teacher", unit_id)
        record_optimizer_outcome(
          budget,
          success = FALSE,
          stage = "bootstrap_teacher",
          condition = teacher_condition
        )
        optimizer_budget_complete_unit(budget, unit_id)
        bootstrap_write_checkpoint(
          kind = "module",
          student = plan$student,
          labeled_demos = labeled_demos,
          bootstrapped_demos = bootstrapped_demos,
          bootstrap_lineage = bootstrap_lineage,
          checkpoint_context = plan$checkpoint_context,
          checkpoint_callback = plan$checkpoint_callback(),
          budget = budget
        )
        if (optimizer_budget_stopped(budget)) {
          break
        }
        next
      }

      trace_events <- new_evaluation_trace_events(teacher, trace_count_before)
      result_metadata <- if (
        is.data.frame(result) &&
          "metadata" %in% names(result) &&
          nrow(result) > 0L
      ) {
        result$metadata[[1L]]
      } else {
        list()
      }
      result_metadata <- utils::modifyList(
        program_trace_event_metadata(trace_events),
        result_metadata
      )
      program_trace <- new_program_trace(
        events = trace_events,
        metadata = result_metadata,
        row_id = idx,
        epoch = round
      )
      prediction <- optimizer_forward_output(result)
      metric_prediction <- if (
        !is.null(metric_field) &&
          !metric_field %in% (names(result) %||% character())
      ) {
        stats::setNames(list(prediction), metric_field)
      } else {
        result
      }
      metric_condition <- NULL
      score <- tryCatch(
        normalize_metric_result(
          invoke_metric(
            teleprompter@metric,
            metric_prediction,
            expected,
            program_trace
          )
        )$score,
        error = function(e) {
          metric_condition <<- e
          cli::cli_warn(
            c(
              "Metric evaluation failed for example {idx}",
              "x" = conditionMessage(e),
              "i" = "This example will be skipped for bootstrapping"
            ),
            class = "dsprrr_metric_warning"
          )
          NA_real_
        }
      )
      usage <- optimizer_forward_usage(teacher, result)
      usage$metric_calls <- 1L
      record_optimizer_usage(
        budget,
        usage,
        stage = if (is.null(metric_condition)) {
          "bootstrap_attempt"
        } else {
          "bootstrap_metric"
        },
        unit_id = unit_id,
        work_unit = "bootstrap_training_row",
        max_started = 1L
      )
      optimizer_budget_count_trial(budget, "bootstrap_attempt", unit_id)
      record_optimizer_outcome(
        budget,
        success = is.null(metric_condition),
        stage = if (is.null(metric_condition)) {
          "bootstrap_attempt"
        } else {
          "bootstrap_metric"
        },
        condition = metric_condition
      )

      if (bootstrap_threshold_pass(teleprompter, score)) {
        demo <- list(
          inputs = example_inputs,
          output = prediction,
          source = "bootstrapped",
          score = score,
          round = round
        )
        bootstrapped_demos <- append(bootstrapped_demos, list(demo))
        bootstrap_lineage[[length(bootstrap_lineage) + 1L]] <- list(
          child = paste0("demo:", length(bootstrapped_demos)),
          parent = unit_id,
          round = as.integer(round),
          row = as.integer(idx)
        )
      }
      optimizer_budget_complete_unit(budget, unit_id)
      bootstrap_write_checkpoint(
        kind = "module",
        student = plan$student,
        labeled_demos = labeled_demos,
        bootstrapped_demos = bootstrapped_demos,
        bootstrap_lineage = bootstrap_lineage,
        checkpoint_context = plan$checkpoint_context,
        checkpoint_callback = plan$checkpoint_callback(),
        budget = budget
      )
      if (length(bootstrapped_demos) >= teleprompter@max_bootstrapped_demos) {
        break
      }
      if (optimizer_budget_stopped(budget)) {
        break
      }
    }
    if (
      length(bootstrapped_demos) >= teleprompter@max_bootstrapped_demos ||
        optimizer_budget_stopped(budget)
    ) {
      break
    }
  }
  list(
    bootstrapped_demos = bootstrapped_demos,
    bootstrap_lineage = bootstrap_lineage,
    budget = budget
  )
}

bootstrap_pipeline_all_steps_full <- function(plan, step_demos) {
  all(vapply(
    plan$demo_steps,
    function(i) {
      length(step_demos[[as.character(i)]]) >=
        plan$teleprompter@max_bootstrapped_demos
    },
    logical(1)
  ))
}

bootstrap_pipeline_execute <- function(plan) {
  teleprompter <- plan$teleprompter
  teacher <- plan$teacher
  labeled_demos <- plan$labeled_demos
  step_demos <- plan$step_demos
  bootstrap_lineage <- plan$bootstrap_lineage
  budget <- plan$budget
  bootstrap_indices <- plan$bootstrap_indices
  demo_steps <- plan$demo_steps

  for (round in seq_len(teleprompter@max_rounds)) {
    if (
      length(bootstrap_indices) == 0 ||
        bootstrap_pipeline_all_steps_full(plan, step_demos) ||
        optimizer_budget_stopped(budget)
    ) {
      break
    }
    for (i in demo_steps) {
      key <- as.character(i)
      teacher_module <- teacher$steps[[i]]@module
      teacher_module$demos <- c(labeled_demos[[key]], step_demos[[key]])
    }

    for (idx in bootstrap_indices) {
      if (optimizer_budget_stopped(budget)) {
        break
      }
      unit_id <- paste0(
        plan$checkpoint_namespace(),
        ":round:",
        round,
        ":row:",
        idx
      )
      if (optimizer_budget_unit_completed(budget, unit_id)) {
        next
      }
      min_provider_calls <- optimizer_min_provider_calls(teacher)
      planned <- list(trials = 1L, metric_calls = 1L)
      if (min_provider_calls > 0L) {
        planned$provider_calls <- min_provider_calls
        planned$input_tokens <- 1L
        planned$output_tokens <- 1L
        planned$total_tokens <- 1L
      }
      if (
        !optimizer_budget_preflight(
          budget,
          stage = "bootstrap_pipeline_attempt",
          planned = planned,
          unit_id = unit_id,
          work_unit = "bootstrap_pipeline_row",
          max_started = 0L
        )
      ) {
        break
      }

      row <- plan$trainset[idx, , drop = FALSE]
      example_inputs <- list()
      for (name in plan$pipeline_inputs) {
        if (name %in% names(row)) {
          example_inputs[[name]] <- row[[name]]
        }
      }
      expected <- if (
        !is.null(plan$metric_field) && plan$metric_field %in% names(row)
      ) {
        row
      } else if (!is.null(plan$output_col) && plan$output_col %in% names(row)) {
        row[[plan$output_col]]
      } else {
        NULL
      }

      teacher_condition <- NULL
      result <- tryCatch(
        teacher$forward(example_inputs, .llm = plan$llm(), trace = TRUE),
        error = function(e) {
          teacher_condition <<- e
          cli::cli_warn(
            c(
              "Bootstrap attempt failed",
              "x" = conditionMessage(e),
              "i" = "The training-row attempt will count as failed"
            ),
            class = "dsprrr_bootstrap_warning"
          )
          NULL
        }
      )
      if (is.null(result)) {
        record_optimizer_usage(
          budget,
          optimizer_unknown_provider_usage(),
          stage = "bootstrap_pipeline_teacher",
          unit_id = unit_id,
          work_unit = "bootstrap_pipeline_row",
          max_started = 1L
        )
        optimizer_budget_count_trial(
          budget,
          "bootstrap_pipeline_teacher",
          unit_id
        )
        record_optimizer_outcome(
          budget,
          success = FALSE,
          stage = "bootstrap_pipeline_teacher",
          condition = teacher_condition
        )
        optimizer_budget_complete_unit(budget, unit_id)
        bootstrap_write_checkpoint(
          kind = "pipeline",
          student = plan$student,
          labeled_demos = labeled_demos,
          step_demos = step_demos,
          demo_steps = demo_steps,
          bootstrap_lineage = bootstrap_lineage,
          checkpoint_context = plan$checkpoint_context,
          checkpoint_callback = plan$checkpoint_callback(),
          budget = budget
        )
        if (optimizer_budget_stopped(budget)) {
          break
        }
        next
      }

      trace_entry <- if (length(teacher$state$traces) > 0L) {
        teacher$state$traces[[length(teacher$state$traces)]]
      } else {
        NULL
      }
      teacher$state$traces <- list()
      final_output <- result$output[[1]]
      result_metadata <- result$metadata[[1L]] %||% list()
      program_trace <- new_program_trace(
        events = if (is.null(trace_entry)) list() else list(trace_entry),
        metadata = result_metadata,
        row_id = idx,
        epoch = round
      )
      metric_condition <- NULL
      score <- tryCatch(
        normalize_metric_result(
          invoke_metric(
            teleprompter@metric,
            final_output,
            expected,
            program_trace
          )
        )$score,
        error = function(e) {
          metric_condition <<- e
          cli::cli_warn(
            c(
              "Metric evaluation failed for example {idx}",
              "x" = conditionMessage(e),
              "i" = "This example will be skipped for bootstrapping"
            ),
            class = "dsprrr_metric_warning"
          )
          NA_real_
        }
      )
      usage <- optimizer_forward_usage(teacher, result)
      usage$metric_calls <- 1L
      record_optimizer_usage(
        budget,
        usage,
        stage = if (is.null(metric_condition)) {
          "bootstrap_pipeline_attempt"
        } else {
          "bootstrap_pipeline_metric"
        },
        unit_id = unit_id,
        work_unit = "bootstrap_pipeline_row",
        max_started = 1L
      )
      optimizer_budget_count_trial(
        budget,
        "bootstrap_pipeline_attempt",
        unit_id
      )
      record_optimizer_outcome(
        budget,
        success = is.null(metric_condition),
        stage = if (is.null(metric_condition)) {
          "bootstrap_pipeline_attempt"
        } else {
          "bootstrap_pipeline_metric"
        },
        condition = metric_condition
      )

      if (bootstrap_threshold_pass(teleprompter, score)) {
        for (i in demo_steps) {
          key <- as.character(i)
          if (
            length(step_demos[[key]]) >= teleprompter@max_bootstrapped_demos
          ) {
            next
          }
          step_in <- trace_entry$step_inputs[[i]]
          step_out <- trace_entry$step_outputs[[i]]
          if (is.null(step_in) || is.null(step_out)) {
            next
          }
          demo <- list(
            inputs = step_in,
            output = step_out,
            source = "bootstrapped",
            score = score,
            round = round
          )
          step_demos[[key]] <- append(step_demos[[key]], list(demo))
          bootstrap_lineage[[length(bootstrap_lineage) + 1L]] <- list(
            child = paste0(
              "step:",
              i,
              ":demo:",
              length(step_demos[[key]])
            ),
            parent = unit_id,
            round = as.integer(round),
            row = as.integer(idx)
          )
        }
      }
      optimizer_budget_complete_unit(budget, unit_id)
      bootstrap_write_checkpoint(
        kind = "pipeline",
        student = plan$student,
        labeled_demos = labeled_demos,
        step_demos = step_demos,
        demo_steps = demo_steps,
        bootstrap_lineage = bootstrap_lineage,
        checkpoint_context = plan$checkpoint_context,
        checkpoint_callback = plan$checkpoint_callback(),
        budget = budget
      )
      if (bootstrap_pipeline_all_steps_full(plan, step_demos)) {
        break
      }
      if (optimizer_budget_stopped(budget)) {
        break
      }
    }
    if (
      bootstrap_pipeline_all_steps_full(plan, step_demos) ||
        optimizer_budget_stopped(budget)
    ) {
      break
    }
  }
  list(
    step_demos = step_demos,
    bootstrap_lineage = bootstrap_lineage,
    budget = budget
  )
}

bootstrap_finalize_module <- function(plan, state) {
  bootstrapped_demos <- state$bootstrapped_demos
  if (length(bootstrapped_demos) > plan$teleprompter@max_bootstrapped_demos) {
    scores <- vapply(
      bootstrapped_demos,
      function(d) d$score %||% 0,
      numeric(1)
    )
    top_indices <- order(scores, decreasing = TRUE)[
      seq_len(plan$teleprompter@max_bootstrapped_demos)
    ]
    bootstrapped_demos <- bootstrapped_demos[top_indices]
  }
  plan$student$demos <- c(plan$labeled_demos, bootstrapped_demos)

  budget_summary <- optimizer_budget_summary(state$budget)
  details <- list(
    n_labeled_demos = length(plan$labeled_demos),
    n_bootstrapped_demos = length(bootstrapped_demos),
    max_rounds = plan$teleprompter@max_rounds,
    rounds_completed = min(
      plan$teleprompter@max_rounds,
      ceiling(budget_summary$attempts / max(1, length(plan$bootstrap_indices)))
    )
  )
  if (!is.null(plan$trial_log)) {
    bootstrap_log_trial(
      trial_log = plan$trial_log,
      student = plan$student,
      valset = plan$valset(),
      metric = plan$teleprompter@metric,
      .llm = plan$llm(),
      control = plan$control,
      budget = state$budget,
      params = list(
        max_bootstrapped_demos = plan$teleprompter@max_bootstrapped_demos,
        max_labeled_demos = plan$teleprompter@max_labeled_demos,
        max_rounds = plan$teleprompter@max_rounds,
        metric_threshold = plan$teleprompter@metric_threshold
      ),
      stage = "bootstrap_log_validation",
      unit_id = paste0(plan$checkpoint_namespace(), ":log-validation")
    )
  }
  bootstrap_record_result(
    plan$student,
    details,
    state$budget,
    plan$trial_log
  )

  expected_units <- unlist(lapply(
    seq_len(plan$teleprompter@max_rounds),
    function(round) {
      paste0(
        plan$checkpoint_namespace(),
        ":round:",
        round,
        ":row:",
        plan$bootstrap_indices
      )
    }
  ))
  bootstrap_complete <-
    length(bootstrapped_demos) >= plan$teleprompter@max_bootstrapped_demos ||
    length(expected_units) == 0L ||
    all(vapply(
      expected_units,
      function(id) optimizer_budget_unit_completed(state$budget, id),
      logical(1)
    ))
  bootstrap_write_checkpoint(
    kind = "module",
    student = plan$student,
    labeled_demos = plan$labeled_demos,
    bootstrapped_demos = bootstrapped_demos,
    bootstrap_lineage = state$bootstrap_lineage,
    checkpoint_context = plan$checkpoint_context,
    checkpoint_callback = plan$checkpoint_callback(),
    budget = state$budget,
    phase = if (bootstrap_complete) "complete" else "bootstrap"
  )
  plan$student
}
bootstrap_finalize_pipeline <- function(plan, state) {
  step_demos <- state$step_demos
  n_bootstrapped_total <- 0L
  for (i in plan$demo_steps) {
    key <- as.character(i)
    demos_i <- step_demos[[key]]
    if (length(demos_i) > plan$teleprompter@max_bootstrapped_demos) {
      scores <- vapply(demos_i, function(d) d$score %||% 0, numeric(1))
      top_idx <- order(scores, decreasing = TRUE)[
        seq_len(plan$teleprompter@max_bootstrapped_demos)
      ]
      demos_i <- demos_i[top_idx]
    }
    n_bootstrapped_total <- n_bootstrapped_total + length(demos_i)
    student_module <- plan$student$steps[[i]]@module
    student_module$demos <- c(plan$labeled_demos[[key]], demos_i)
  }

  details <- list(
    joint_pipeline = TRUE,
    n_steps = plan$n_steps,
    demo_steps = plan$demo_steps,
    n_labeled_demos = length(
      plan$labeled_demos[[as.character(plan$final_step)]]
    ),
    n_bootstrapped_demos = n_bootstrapped_total,
    demos_per_step = stats::setNames(
      lapply(plan$demo_steps, function(i) {
        length(plan$student$steps[[i]]@module$demos)
      }),
      as.character(plan$demo_steps)
    ),
    max_rounds = plan$teleprompter@max_rounds
  )
  if (!is.null(plan$trial_log)) {
    bootstrap_log_trial(
      trial_log = plan$trial_log,
      student = plan$student,
      valset = plan$valset(),
      metric = plan$teleprompter@metric,
      .llm = plan$llm(),
      control = plan$control,
      budget = state$budget,
      params = list(
        joint_pipeline = TRUE,
        max_bootstrapped_demos = plan$teleprompter@max_bootstrapped_demos,
        max_labeled_demos = plan$teleprompter@max_labeled_demos,
        max_rounds = plan$teleprompter@max_rounds,
        metric_threshold = plan$teleprompter@metric_threshold
      ),
      stage = "bootstrap_pipeline_log_validation",
      unit_id = paste0(plan$checkpoint_namespace(), ":log-validation")
    )
  }
  bootstrap_record_result(
    plan$student,
    details,
    state$budget,
    plan$trial_log
  )

  expected_units <- unlist(lapply(
    seq_len(plan$teleprompter@max_rounds),
    function(round) {
      paste0(
        plan$checkpoint_namespace(),
        ":round:",
        round,
        ":row:",
        plan$bootstrap_indices
      )
    }
  ))
  pipeline_complete <- bootstrap_pipeline_all_steps_full(plan, step_demos) ||
    length(expected_units) == 0L ||
    all(vapply(
      expected_units,
      function(id) optimizer_budget_unit_completed(state$budget, id),
      logical(1)
    ))
  bootstrap_write_checkpoint(
    kind = "pipeline",
    student = plan$student,
    labeled_demos = plan$labeled_demos,
    step_demos = step_demos,
    demo_steps = plan$demo_steps,
    bootstrap_lineage = state$bootstrap_lineage,
    checkpoint_context = plan$checkpoint_context,
    checkpoint_callback = plan$checkpoint_callback(),
    budget = state$budget,
    phase = if (pipeline_complete) "complete" else "bootstrap"
  )
  plan$student
}
