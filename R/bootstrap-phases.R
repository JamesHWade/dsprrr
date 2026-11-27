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
      # Checkpoint admission must precede resolving the runtime identity.
      config = c(
        list(
          max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
          max_labeled_demos = teleprompter@max_labeled_demos,
          max_rounds = teleprompter@max_rounds,
          metric_threshold = teleprompter@metric_threshold
        ),
        if (identical(mode, "module")) {
          list(teacher_settings = teacher_settings)
        },
        list(
          seed = teleprompter@seed,
          effective_runtime = optimizer_checkpoint_effective_runtime_identity(
            program,
            .llm = .llm,
            registry = artifact_validate_registry(control@checkpoint_registry),
            path = "effective_runtime"
          ),
          mode = mode
        )
      ),
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
  teleprompter_init_trial_log(teleprompter@log_dir, "BootstrapFewShot")
}

bootstrap_format_labeled_demos <- function(
  trainset,
  signature,
  output_col,
  n_labeled
) {
  if (n_labeled <= 0L) {
    return(list())
  }
  labeled_data <- trainset[seq_len(n_labeled), , drop = FALSE]
  demos <- format_trainset_as_demos(
    labeled_data,
    signature,
    output_col = output_col
  )
  for (i in seq_along(demos)) {
    demos[[i]]$source <- "labeled"
    demos[[i]]$score <- NA_real_
    demos[[i]]$round <- 0L
  }
  demos
}

bootstrap_plan_common <- function(
  teleprompter,
  program,
  trainset,
  valset,
  .llm,
  control,
  .optimizer_budget,
  .checkpoint_context,
  mode,
  teacher_settings = NULL,
  include_available_cols = TRUE,
  caller = rlang::caller_env()
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
    mode = mode,
    teacher_settings = teacher_settings
  )
  if (checkpoint$complete) {
    return(list(
      complete = TRUE,
      program = checkpoint$program,
      checkpoint_context = checkpoint$context
    ))
  }

  trial_log <- bootstrap_initialize_trial_log(teleprompter)
  trainset <- sample_dataset(trainset, n = NULL, seed = teleprompter@seed)
  student <- copy_module(program)
  teacher <- copy_module(program)
  input_names <- get_input_names(program$signature)
  metric_field <- get_metric_field(teleprompter@metric)
  output_col <- metric_field %||% find_output_column(trainset, input_names)

  if (is.null(output_col)) {
    cli::cli_warn(
      c(
        "No output column found in trainset",
        "i" = "Expected one of: 'output', 'label', 'answer', 'response', 'result', 'y'",
        "i" = "Or any column not in inputs: {.val {input_names}}",
        if (isTRUE(include_available_cols)) {
          c("i" = "Available columns: {.val {names(trainset)}}")
        },
        "!" = "Metric will receive NULL as expected value for all examples"
      ),
      class = "dsprrr_missing_output_column",
      call = caller
    )
  }

  list(
    complete = FALSE,
    control = control,
    checkpoint_context = checkpoint$context,
    trial_log = trial_log,
    trainset = trainset,
    student = student,
    teacher = teacher,
    input_names = input_names,
    metric_field = metric_field,
    output_col = output_col
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
  caller <- rlang::caller_env()
  teacher_settings <- teleprompter@teacher_settings %||%
    list(temperature = 0.7)
  common <- bootstrap_plan_common(
    teleprompter = teleprompter,
    program = program,
    trainset = trainset,
    valset = valset,
    .llm = .llm,
    control = control,
    .optimizer_budget = .optimizer_budget,
    .checkpoint_context = .checkpoint_context,
    mode = "module",
    teacher_settings = teacher_settings,
    include_available_cols = TRUE,
    caller = caller
  )
  if (common$complete) {
    return(common)
  }

  n_labeled <- min(teleprompter@max_labeled_demos, nrow(common$trainset))
  labeled_demos <- bootstrap_format_labeled_demos(
    common$trainset,
    program$signature,
    output_col = common$output_col,
    n_labeled = n_labeled
  )
  bootstrap_indices <- if (n_labeled < nrow(common$trainset)) {
    seq(n_labeled + 1, nrow(common$trainset))
  } else {
    integer(0)
  }

  checkpoint_state <- common$checkpoint_context$search_state %||%
    .resume_state %||%
    list()
  if (
    length(checkpoint_state) > 0L &&
      !identical(checkpoint_state$kind %||% "module", "module")
  ) {
    cli::cli_abort(
      "Bootstrap checkpoint has the wrong search-state kind",
      class = "dsprrr_optimizer_checkpoint_malformed",
      call = caller
    )
  }
  bootstrapped_demos <- checkpoint_state$bootstrapped_demos %||% list()
  bootstrap_lineage <- checkpoint_state$lineage %||% list()
  budget <- .optimizer_budget %||%
    common$checkpoint_context$budget %||%
    new_optimizer_budget(common$control)

  list(
    complete = FALSE,
    teleprompter = teleprompter,
    program = program,
    trainset = common$trainset,
    valset = function() valset,
    llm = function() .llm,
    control = common$control,
    checkpoint_context = common$checkpoint_context,
    checkpoint_namespace = function() .checkpoint_namespace,
    checkpoint_callback = function() .checkpoint_callback,
    teacher_settings = teacher_settings,
    trial_log = common$trial_log,
    student = common$student,
    teacher = common$teacher,
    input_names = common$input_names,
    metric_field = common$metric_field,
    output_col = common$output_col,
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
  caller <- rlang::caller_env()
  common <- bootstrap_plan_common(
    teleprompter = teleprompter,
    program = program,
    trainset = trainset,
    valset = valset,
    .llm = .llm,
    control = control,
    .optimizer_budget = .optimizer_budget,
    .checkpoint_context = .checkpoint_context,
    mode = "pipeline",
    include_available_cols = FALSE,
    caller = caller
  )
  if (common$complete) {
    return(common)
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
      checkpoint_context = common$checkpoint_context
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
    final_inputs <- get_input_names(final_module$signature)
    if (all(final_inputs %in% names(common$trainset))) {
      n_labeled <- min(teleprompter@max_labeled_demos, nrow(common$trainset))
      labeled_demos[[as.character(
        final_step
      )]] <- bootstrap_format_labeled_demos(
        common$trainset,
        final_module$signature,
        output_col = common$output_col,
        n_labeled = n_labeled
      )
    }
  }
  bootstrap_indices <- if (n_labeled < nrow(common$trainset)) {
    seq(n_labeled + 1, nrow(common$trainset))
  } else {
    integer(0)
  }

  checkpoint_state <- common$checkpoint_context$search_state %||%
    .resume_state %||%
    list()
  if (
    length(checkpoint_state) > 0L &&
      !identical(checkpoint_state$kind %||% "pipeline", "pipeline")
  ) {
    cli::cli_abort(
      "Bootstrap checkpoint has the wrong pipeline search-state kind",
      class = "dsprrr_optimizer_checkpoint_malformed",
      call = caller
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
      call = caller
    )
  }
  bootstrap_lineage <- checkpoint_state$lineage %||% list()
  budget <- .optimizer_budget %||%
    common$checkpoint_context$budget %||%
    new_optimizer_budget(common$control)

  list(
    complete = FALSE,
    teleprompter = teleprompter,
    program = program,
    trainset = common$trainset,
    valset = function() valset,
    llm = function() .llm,
    control = common$control,
    checkpoint_context = common$checkpoint_context,
    checkpoint_namespace = function() .checkpoint_namespace,
    checkpoint_callback = function() .checkpoint_callback,
    trial_log = common$trial_log,
    teacher = common$teacher,
    student = common$student,
    pipeline_inputs = common$input_names,
    metric_field = common$metric_field,
    output_col = common$output_col,
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

bootstrap_execute_loop <- function(plan, demos, adapter) {
  teleprompter <- plan$teleprompter
  trainset <- plan$trainset
  teacher <- plan$teacher
  bootstrap_indices <- plan$bootstrap_indices
  bootstrap_lineage <- plan$bootstrap_lineage
  budget <- plan$budget
  input_names <- adapter$input_names
  stage_attempt <- paste0(adapter$stage_prefix, "_attempt")
  stage_teacher <- paste0(adapter$stage_prefix, "_teacher")
  stage_metric <- paste0(adapter$stage_prefix, "_metric")

  for (round in seq_len(teleprompter@max_rounds)) {
    if (
      length(bootstrap_indices) == 0 ||
        adapter$is_full(demos) ||
        optimizer_budget_stopped(budget)
    ) {
      break
    }
    adapter$prepare_teacher(teacher, demos)

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
          stage = stage_attempt,
          planned = planned,
          unit_id = unit_id,
          work_unit = adapter$work_unit,
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
      before_run <- adapter$before_run(teacher)
      result <- tryCatch(
        adapter$run_teacher(teacher, example_inputs),
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
          stage = stage_teacher,
          unit_id = unit_id,
          work_unit = adapter$work_unit,
          max_started = 1L
        )
        optimizer_budget_count_trial(budget, stage_teacher, unit_id)
        record_optimizer_outcome(
          budget,
          success = FALSE,
          stage = stage_teacher,
          condition = teacher_condition
        )
        optimizer_budget_complete_unit(budget, unit_id)
        adapter$write_checkpoint(demos, bootstrap_lineage)
        if (optimizer_budget_stopped(budget)) {
          break
        }
        next
      }

      extracted <- adapter$extract(teacher, result, before_run, idx, round)
      metric_condition <- NULL
      score <- tryCatch(
        normalize_metric_result(
          invoke_metric(
            teleprompter@metric,
            extracted$metric_prediction,
            expected,
            extracted$program_trace
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
      outcome_stage <- if (is.null(metric_condition)) {
        stage_attempt
      } else {
        stage_metric
      }
      record_optimizer_usage(
        budget,
        usage,
        stage = outcome_stage,
        unit_id = unit_id,
        work_unit = adapter$work_unit,
        max_started = 1L
      )
      optimizer_budget_count_trial(budget, stage_attempt, unit_id)
      record_optimizer_outcome(
        budget,
        success = is.null(metric_condition),
        stage = outcome_stage,
        condition = metric_condition
      )

      if (bootstrap_threshold_pass(teleprompter, score)) {
        accepted <- adapter$accept(
          demos = demos,
          lineage = bootstrap_lineage,
          extracted = extracted,
          example_inputs = example_inputs,
          score = score,
          round = round,
          unit_id = unit_id,
          idx = idx
        )
        demos <- accepted$demos
        bootstrap_lineage <- accepted$lineage
      }
      optimizer_budget_complete_unit(budget, unit_id)
      adapter$write_checkpoint(demos, bootstrap_lineage)
      if (adapter$is_full(demos) || optimizer_budget_stopped(budget)) {
        break
      }
    }
    if (adapter$is_full(demos) || optimizer_budget_stopped(budget)) {
      break
    }
  }

  list(
    demos = demos,
    bootstrap_lineage = bootstrap_lineage,
    budget = budget
  )
}

bootstrap_module_execute <- function(plan) {
  out <- bootstrap_execute_loop(
    plan = plan,
    demos = plan$bootstrapped_demos,
    adapter = list(
      stage_prefix = "bootstrap",
      work_unit = "bootstrap_training_row",
      input_names = plan$input_names,
      is_full = function(demos) {
        length(demos) >= plan$teleprompter@max_bootstrapped_demos
      },
      prepare_teacher = function(teacher, demos) {
        teacher$demos <- c(plan$labeled_demos, demos)
      },
      before_run = function(teacher) evaluation_trace_cursor(teacher),
      run_teacher = function(teacher, example_inputs) {
        run_with_settings(
          teacher,
          example_inputs,
          .llm = plan$llm(),
          settings = plan$teacher_settings
        )
      },
      extract = function(teacher, result, trace_count_before, idx, round) {
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
          !is.null(plan$metric_field) &&
            !plan$metric_field %in% (names(result) %||% character())
        ) {
          stats::setNames(list(prediction), plan$metric_field)
        } else {
          result
        }
        list(
          metric_prediction = metric_prediction,
          program_trace = program_trace,
          prediction = prediction
        )
      },
      accept = function(
        demos,
        lineage,
        extracted,
        example_inputs,
        score,
        round,
        unit_id,
        idx
      ) {
        demo <- list(
          inputs = example_inputs,
          output = extracted$prediction,
          source = "bootstrapped",
          score = score,
          round = round
        )
        demos <- append(demos, list(demo))
        lineage[[length(lineage) + 1L]] <- list(
          child = paste0("demo:", length(demos)),
          parent = unit_id,
          round = as.integer(round),
          row = as.integer(idx)
        )
        list(demos = demos, lineage = lineage)
      },
      write_checkpoint = function(demos, lineage) {
        bootstrap_write_checkpoint(
          kind = "module",
          student = plan$student,
          labeled_demos = plan$labeled_demos,
          bootstrapped_demos = demos,
          bootstrap_lineage = lineage,
          checkpoint_context = plan$checkpoint_context,
          checkpoint_callback = plan$checkpoint_callback(),
          budget = plan$budget
        )
      }
    )
  )
  list(
    bootstrapped_demos = out$demos,
    bootstrap_lineage = out$bootstrap_lineage,
    budget = out$budget
  )
}

bootstrap_pipeline_execute <- function(plan) {
  out <- bootstrap_execute_loop(
    plan = plan,
    demos = plan$step_demos,
    adapter = list(
      stage_prefix = "bootstrap_pipeline",
      work_unit = "bootstrap_pipeline_row",
      input_names = plan$pipeline_inputs,
      is_full = function(demos) {
        bootstrap_pipeline_all_steps_full(plan, demos)
      },
      prepare_teacher = function(teacher, demos) {
        for (i in plan$demo_steps) {
          key <- as.character(i)
          teacher$steps[[i]]@module$demos <- c(
            plan$labeled_demos[[key]],
            demos[[key]]
          )
        }
      },
      before_run = function(teacher) NULL,
      run_teacher = function(teacher, example_inputs) {
        teacher$forward(example_inputs, .llm = plan$llm(), trace = TRUE)
      },
      extract = function(teacher, result, before_run, idx, round) {
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
        list(
          metric_prediction = final_output,
          program_trace = program_trace,
          trace_entry = trace_entry
        )
      },
      accept = function(
        demos,
        lineage,
        extracted,
        example_inputs,
        score,
        round,
        unit_id,
        idx
      ) {
        for (i in plan$demo_steps) {
          key <- as.character(i)
          if (
            length(demos[[key]]) >= plan$teleprompter@max_bootstrapped_demos
          ) {
            next
          }
          step_in <- extracted$trace_entry$step_inputs[[i]]
          step_out <- extracted$trace_entry$step_outputs[[i]]
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
          demos[[key]] <- append(demos[[key]], list(demo))
          lineage[[length(lineage) + 1L]] <- list(
            child = paste0("step:", i, ":demo:", length(demos[[key]])),
            parent = unit_id,
            round = as.integer(round),
            row = as.integer(idx)
          )
        }
        list(demos = demos, lineage = lineage)
      },
      write_checkpoint = function(demos, lineage) {
        bootstrap_write_checkpoint(
          kind = "pipeline",
          student = plan$student,
          labeled_demos = plan$labeled_demos,
          step_demos = demos,
          demo_steps = plan$demo_steps,
          bootstrap_lineage = lineage,
          checkpoint_context = plan$checkpoint_context,
          checkpoint_callback = plan$checkpoint_callback(),
          budget = plan$budget
        )
      }
    )
  )
  list(
    step_demos = out$demos,
    bootstrap_lineage = out$bootstrap_lineage,
    budget = out$budget
  )
}

bootstrap_trim_top_demos <- function(demos, max_demos) {
  if (length(demos) <= max_demos) {
    return(demos)
  }
  scores <- vapply(demos, function(d) d$score %||% 0, numeric(1))
  demos[order(scores, decreasing = TRUE)[seq_len(max_demos)]]
}

bootstrap_units_complete <- function(plan, budget) {
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
  length(expected_units) == 0L ||
    all(vapply(
      expected_units,
      function(id) optimizer_budget_unit_completed(budget, id),
      logical(1)
    ))
}

bootstrap_finalize_common <- function(
  plan,
  state,
  kind,
  details,
  log_params,
  log_stage,
  is_full,
  bootstrapped_demos = NULL,
  step_demos = NULL
) {
  if (!is.null(plan$trial_log)) {
    bootstrap_log_trial(
      trial_log = plan$trial_log,
      student = plan$student,
      valset = plan$valset(),
      metric = plan$teleprompter@metric,
      .llm = plan$llm(),
      control = plan$control,
      budget = state$budget,
      params = log_params,
      stage = log_stage,
      unit_id = paste0(plan$checkpoint_namespace(), ":log-validation")
    )
  }
  bootstrap_record_result(
    plan$student,
    details,
    state$budget,
    plan$trial_log
  )

  is_complete <- is_full || bootstrap_units_complete(plan, state$budget)
  bootstrap_write_checkpoint(
    kind = kind,
    student = plan$student,
    labeled_demos = plan$labeled_demos,
    bootstrapped_demos = bootstrapped_demos,
    step_demos = step_demos,
    demo_steps = plan$demo_steps %||% integer(),
    bootstrap_lineage = state$bootstrap_lineage,
    checkpoint_context = plan$checkpoint_context,
    checkpoint_callback = plan$checkpoint_callback(),
    budget = state$budget,
    phase = if (is_complete) "complete" else "bootstrap"
  )
  plan$student
}

bootstrap_finalize_module <- function(plan, state) {
  bootstrapped_demos <- bootstrap_trim_top_demos(
    state$bootstrapped_demos,
    plan$teleprompter@max_bootstrapped_demos
  )
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
  log_params <- list(
    max_bootstrapped_demos = plan$teleprompter@max_bootstrapped_demos,
    max_labeled_demos = plan$teleprompter@max_labeled_demos,
    max_rounds = plan$teleprompter@max_rounds,
    metric_threshold = plan$teleprompter@metric_threshold
  )
  bootstrap_finalize_common(
    plan = plan,
    state = state,
    kind = "module",
    details = details,
    log_params = log_params,
    log_stage = "bootstrap_log_validation",
    is_full = length(bootstrapped_demos) >=
      plan$teleprompter@max_bootstrapped_demos,
    bootstrapped_demos = bootstrapped_demos
  )
}

bootstrap_finalize_pipeline <- function(plan, state) {
  step_demos <- state$step_demos
  n_bootstrapped_total <- 0L
  for (i in plan$demo_steps) {
    key <- as.character(i)
    demos_i <- bootstrap_trim_top_demos(
      step_demos[[key]],
      plan$teleprompter@max_bootstrapped_demos
    )
    n_bootstrapped_total <- n_bootstrapped_total + length(demos_i)
    plan$student$steps[[i]]@module$demos <- c(
      plan$labeled_demos[[key]],
      demos_i
    )
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
  log_params <- list(
    joint_pipeline = TRUE,
    max_bootstrapped_demos = plan$teleprompter@max_bootstrapped_demos,
    max_labeled_demos = plan$teleprompter@max_labeled_demos,
    max_rounds = plan$teleprompter@max_rounds,
    metric_threshold = plan$teleprompter@metric_threshold
  )
  bootstrap_finalize_common(
    plan = plan,
    state = state,
    kind = "pipeline",
    details = details,
    log_params = log_params,
    log_stage = "bootstrap_pipeline_log_validation",
    is_full = bootstrap_pipeline_all_steps_full(plan, step_demos),
    step_demos = step_demos
  )
}
