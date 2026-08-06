# BootstrapFewShot Teleprompter
#
# DSPy-style demonstration bootstrapping optimizer.
# Uses a teacher model to generate demonstrations from training examples,
# selecting those that pass a metric threshold.

#' BootstrapFewShot Teleprompter
#'
#' @include teleprompter.R optimizer-core.R
#'
#' @description
#' A teleprompter that bootstraps demonstrations by having a teacher model
#' generate predictions on training examples and selecting successful ones
#' as demonstrations. This is DSPy's foundational optimization approach.
#'
#' The optimizer:
#' 1. Starts with optional labeled demonstrations from the training set
#' 2. Uses a teacher model to generate predictions on remaining examples
#' 3. Evaluates predictions using the provided metric
#' 4. Selects top-scoring predictions as bootstrapped demonstrations
#' 5. Optionally runs multiple rounds, updating the teacher with new demos
#'
#' @details
#' ## Joint pipeline compilation
#'
#' When `program` is a pipeline (built with [pipeline()] or [`%>>%`]),
#' BootstrapFewShot compiles the whole program jointly, like DSPy: the
#' teacher pipeline runs end-to-end on each training example, the *final*
#' output is scored with the metric, and when a run passes the threshold
#' every step's `(inputs, output)` pair from that trace is harvested as a
#' demonstration for the corresponding step module. Intermediate steps
#' therefore receive demos even though the training set only labels the
#' final output. Labeled demos (`max_labeled_demos`) are applied to the
#' final step only, and only when its input fields exist in the trainset.
#'
#' @param metric A metric function for evaluating predictions (required).
#' @param metric_threshold Minimum score for a demo to be accepted.
#'   If NULL, accepts any successful prediction. Default is NULL.
#' @param max_errors Maximum number of errors allowed during optimization.
#'   Default is 5.
#' @param max_bootstrapped_demos Maximum number of bootstrapped demonstrations
#'   to include. Default is 4.
#' @param max_labeled_demos Maximum number of labeled demonstrations from
#'   the training set. Default is 16.
#' @param max_rounds Number of bootstrap rounds to perform. Default is 1.
#' @param teacher_settings List of settings for the teacher model, such as
#'   `temperature` or `model`. If NULL, defaults to `list(temperature = 0.7)`.
#' @param seed Random seed for reproducibility. Default is NULL.
#' @param log_dir Directory for trial logging. Default is NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a BootstrapFewShot teleprompter
#' tp <- BootstrapFewShot(
#'   metric = metric_exact_match(field = "answer"),
#'   max_bootstrapped_demos = 4L,
#'   max_labeled_demos = 8L
#' )
#'
#' # Compile a module
#' compiled <- compile(tp, qa_module, trainset, .llm = llm)
#' }
BootstrapFewShot <- S7::new_class(
  "BootstrapFewShot",
  parent = Teleprompter,
  properties = list(
    max_bootstrapped_demos = S7::new_property(
      S7::class_integer,
      default = 4L,
      validator = function(value) {
        if (value < 0) {
          return("max_bootstrapped_demos must be non-negative")
        }
        NULL
      }
    ),
    max_labeled_demos = S7::new_property(
      S7::class_integer,
      default = 16L,
      validator = function(value) {
        if (value < 0) {
          return("max_labeled_demos must be non-negative")
        }
        NULL
      }
    ),
    max_rounds = S7::new_property(
      S7::class_integer,
      default = 1L,
      validator = function(value) {
        if (value < 1) {
          return("max_rounds must be at least 1")
        }
        NULL
      }
    ),
    teacher_settings = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is.list(value)) {
          return("teacher_settings must be a list or NULL")
        }
        NULL
      }
    ),
    seed = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && (!is.numeric(value) || length(value) != 1)) {
          return("seed must be a single numeric value or NULL")
        }
        NULL
      }
    ),
    log_dir = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is.character(value)) {
          return("log_dir must be a character string or NULL")
        }
        NULL
      }
    )
  )
)

bootstrap_budget_eval_result <- function(budget) {
  summary <- optimizer_budget_summary(budget)
  token_usage_unknown <- any(
    c(
      summary$unknown_usage$input_tokens,
      summary$unknown_usage$output_tokens,
      summary$unknown_usage$total_tokens
    ) >
      0L
  )

  EvalResult(
    mean_score = NA_real_,
    n_evaluated = as.integer(summary$successes),
    n_errors = as.integer(summary$total_errors),
    input_tokens = as.integer(summary$input_tokens),
    output_tokens = as.integer(summary$output_tokens),
    total_tokens = as.integer(summary$total_tokens),
    total_cost = summary$total_cost,
    provider_calls = as.integer(summary$provider_calls),
    metric_calls = as.integer(summary$metric_calls),
    provider_usage_unknown = summary$unknown_usage$provider_calls > 0L,
    token_usage_unknown = token_usage_unknown,
    total_latency_ms = summary$elapsed_seconds * 1000
  )
}

bootstrap_log_trial <- function(
  trial_log,
  student,
  valset,
  metric,
  .llm,
  control,
  budget,
  params,
  stage,
  unit_id
) {
  trial <- create_trial(
    optimizer_name = "BootstrapFewShot",
    params = params
  )
  eval_result <- if (is.null(valset)) {
    bootstrap_budget_eval_result(budget)
  } else {
    optimizer_eval_candidate(
      student,
      valset,
      metric,
      .llm = .llm,
      control = control,
      budget = budget,
      stage = stage,
      unit_id = unit_id
    )
  }
  trial <- complete_trial(
    trial,
    eval_result,
    compiled_artifact_ref = student,
    notes = if (is.null(valset)) {
      "Search-ledger summary; no additional validation was run for logging."
    } else {
      "Validation was metered through the shared optimizer budget."
    }
  )
  trial_log$add_trial(trial)
  invisible(eval_result)
}

#' Compile method for BootstrapFewShot
#' @noRd
compile_bootstrap <- function(
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
  .checkpoint_callback = NULL,
  ...
) {
  # Validate inputs
  if (!inherits(program, "Module")) {
    cli::cli_abort("BootstrapFewShot currently only supports Module objects")
  }
  bootstrap_assert_demo_eligible(program)

  if (!is.data.frame(trainset)) {
    cli::cli_abort("{.arg trainset} must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort("BootstrapFewShot requires a metric function")
  }

  # Pipelines are compiled jointly: the whole program runs end-to-end and
  # every step harvests demos from traces that pass the metric
  if (inherits(program, "PipelineModule")) {
    return(compile_bootstrap_pipeline(
      teleprompter,
      program,
      trainset,
      valset = valset,
      .llm = .llm,
      control = control,
      .optimizer_budget = .optimizer_budget,
      .checkpoint_context = .checkpoint_context,
      .checkpoint_namespace = .checkpoint_namespace,
      .resume_state = .resume_state,
      .checkpoint_callback = .checkpoint_callback,
      ...
    ))
  }

  # Apply default teacher settings if not provided
  teacher_settings <- teleprompter@teacher_settings %||% list(temperature = 0.7)

  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control
  )

  checkpoint_context <- .checkpoint_context
  if (
    is.null(checkpoint_context) &&
      is.null(.optimizer_budget) &&
      optimizer_checkpoint_enabled(control)
  ) {
    checkpoint_context <- optimizer_checkpoint_begin(
      optimizer_name = "BootstrapFewShot",
      optimizer_version = 1L,
      program = program,
      data = list(trainset = trainset, valset = valset),
      metric = teleprompter@metric,
      config = list(
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        max_rounds = teleprompter@max_rounds,
        metric_threshold = teleprompter@metric_threshold,
        teacher_settings = teacher_settings,
        seed = teleprompter@seed,
        effective_runtime = optimizer_checkpoint_effective_runtime_identity(
          program,
          .llm = .llm,
          registry = artifact_validate_registry(control@checkpoint_registry),
          path = "effective_runtime"
        ),
        mode = "module"
      ),
      control = control,
      initial_state = list(
        kind = "module",
        bootstrapped_demos = list(),
        lineage = list()
      ),
      initial_phase = "bootstrap"
    )
    if (identical(checkpoint_context$phase, "complete")) {
      return(checkpoint_context$best_program)
    }
  }

  # Initialize trial logging if log_dir specified
  trial_log <- if (!is.null(teleprompter@log_dir)) {
    TrialLog$new(
      optimizer_name = "BootstrapFewShot",
      log_dir = teleprompter@log_dir
    )
  } else {
    NULL
  }

  # Sample trainset with seed for reproducibility
  trainset <- sample_dataset(
    trainset,
    n = NULL, # Use full dataset
    seed = teleprompter@seed
  )

  # Create optimized copy of the program (student)
  student <- copy_module(program)

  # Create teacher (defaults to student if no special settings)
  teacher <- copy_module(program)

  # Get input names from signature

  input_names <- vapply(
    program$signature@inputs,
    function(x) x$name,
    character(1)
  )

  # Determine output column name
  # First try to get field from metric, then fall back to auto-detection
  metric_field <- get_metric_field(teleprompter@metric)
  output_col <- metric_field %||%
    find_output_column(trainset, input_names)

  if (is.null(output_col)) {
    cli::cli_warn(
      c(
        "No output column found in trainset",
        "i" = "Expected one of: 'output', 'label', 'answer', 'response', 'result', 'y'",
        "i" = "Or any column not in inputs: {.val {input_names}}",
        "i" = "Available columns: {.val {names(trainset)}}",
        "!" = "Metric will receive NULL as expected value for all examples"
      ),
      class = "dsprrr_missing_output_column"
    )
  }

  # Phase 1: Select labeled demos from trainset
  n_labeled <- min(teleprompter@max_labeled_demos, nrow(trainset))
  labeled_demos <- list()

  if (n_labeled > 0) {
    labeled_data <- trainset[seq_len(n_labeled), , drop = FALSE]
    labeled_demos <- format_trainset_as_demos(
      labeled_data,
      program$signature,
      output_col = output_col
    )

    # Add source metadata
    for (i in seq_along(labeled_demos)) {
      labeled_demos[[i]]$source <- "labeled"
      labeled_demos[[i]]$score <- NA_real_
      labeled_demos[[i]]$round <- 0L
    }
  }

  # Remaining examples for bootstrapping
  bootstrap_indices <- if (n_labeled < nrow(trainset)) {
    seq(n_labeled + 1, nrow(trainset))
  } else {
    integer(0)
  }

  # Phase 2: Bootstrap demonstrations
  checkpoint_state <- checkpoint_context$search_state %||%
    .resume_state %||%
    list()
  if (
    length(checkpoint_state) > 0L &&
      !identical(checkpoint_state$kind %||% "module", "module")
  ) {
    cli::cli_abort(
      "Bootstrap checkpoint has the wrong search-state kind",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  bootstrapped_demos <- checkpoint_state$bootstrapped_demos %||% list()
  bootstrap_lineage <- checkpoint_state$lineage %||% list()
  budget <- .optimizer_budget %||%
    checkpoint_context$budget %||%
    new_optimizer_budget(control)

  if (!is.null(checkpoint_context) && isTRUE(checkpoint_context$resumed)) {
    outer_rng <- optimizer_checkpoint_capture_rng()
    optimizer_checkpoint_restore_rng(checkpoint_context$rng)
    on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
  }

  write_bootstrap_checkpoint <- function(phase = "bootstrap") {
    partial <- copy_module(student)
    partial$demos <- c(labeled_demos, bootstrapped_demos)
    state <- list(
      kind = "module",
      bootstrapped_demos = bootstrapped_demos,
      lineage = bootstrap_lineage
    )
    if (!is.null(checkpoint_context)) {
      optimizer_checkpoint_write(
        checkpoint_context,
        phase = phase,
        search_state = state,
        lineage = bootstrap_lineage,
        best_program = partial
      )
    }
    if (is.function(.checkpoint_callback)) {
      .checkpoint_callback(
        state = state,
        phase = phase,
        best_program = partial,
        budget = budget
      )
    }
    invisible(NULL)
  }

  for (round in seq_len(teleprompter@max_rounds)) {
    if (
      length(bootstrap_indices) == 0 ||
        length(bootstrapped_demos) >= teleprompter@max_bootstrapped_demos ||
        optimizer_budget_stopped(budget)
    ) {
      break
    }

    # Update teacher with current demos for this round
    current_demos <- c(labeled_demos, bootstrapped_demos)
    teacher$demos <- current_demos

    # Process each bootstrap candidate
    for (idx in bootstrap_indices) {
      if (optimizer_budget_stopped(budget)) {
        break
      }

      unit_id <- paste0(
        .checkpoint_namespace,
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

      # Extract inputs for this example
      example_inputs <- list()
      for (name in input_names) {
        if (name %in% names(row)) {
          example_inputs[[name]] <- row[[name]]
        }
      }

      # Get expected output. Field-aware metrics extract their field from
      # `expected` themselves (as in evaluate()), so they must receive the full
      # row, not the bare cell value -- otherwise extract_field() errors, the
      # metric is scored NA, and no demos are ever harvested.
      expected <- if (!is.null(metric_field) && metric_field %in% names(row)) {
        row
      } else if (!is.null(output_col) && output_col %in% names(row)) {
        row[[output_col]]
      } else {
        NULL
      }

      # Run teacher with temperature for diversity
      teacher_condition <- NULL
      trace_count_before <- length(teacher$state$traces %||% list())
      result <- tryCatch(
        {
          # Apply teacher settings (like temperature)
          run_with_settings(
            teacher,
            example_inputs,
            .llm = .llm,
            settings = teacher_settings
          )
        },
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
        write_bootstrap_checkpoint()
        if (optimizer_budget_stopped(budget)) {
          break
        }
        next
      }

      # Evaluate with metric (feedback metrics return list(score, feedback))
      trace_events <- new_evaluation_trace_events(
        teacher,
        trace_count_before
      )
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
        {
          normalize_metric_result(
            # Field-aware metrics need the structured run result for field
            # extraction; only the harvested demo stores the scalar output.
            invoke_metric(
              teleprompter@metric,
              metric_prediction,
              expected,
              program_trace
            )
          )$score
        },
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

      # Check threshold
      passes_threshold <- if (!is.na(score)) {
        if (!is.null(teleprompter@metric_threshold)) {
          score >= teleprompter@metric_threshold
        } else {
          score > 0
        }
      } else {
        FALSE
      }

      if (passes_threshold) {
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
      write_bootstrap_checkpoint()

      # Check if we have enough bootstrapped demos
      if (length(bootstrapped_demos) >= teleprompter@max_bootstrapped_demos) {
        break
      }

      if (optimizer_budget_stopped(budget)) {
        break
      }
    }

    # Early exit if we have enough demos
    if (
      length(bootstrapped_demos) >= teleprompter@max_bootstrapped_demos ||
        optimizer_budget_stopped(budget)
    ) {
      break
    }
  }

  # Phase 3: Select top bootstrapped demos by score
  if (length(bootstrapped_demos) > teleprompter@max_bootstrapped_demos) {
    scores <- vapply(
      bootstrapped_demos,
      function(d) d$score %||% 0,
      numeric(1)
    )
    top_indices <- order(scores, decreasing = TRUE)[
      seq_len(teleprompter@max_bootstrapped_demos)
    ]
    bootstrapped_demos <- bootstrapped_demos[top_indices]
  }

  # Combine labeled and bootstrapped demos for student
  final_demos <- c(labeled_demos, bootstrapped_demos)
  student$demos <- final_demos

  # Update student state
  student$state$compiled <- TRUE
  student$config$compiled <- TRUE
  student$config$teleprompter <- "BootstrapFewShot"
  budget_summary <- optimizer_budget_summary(budget)
  student$config$optimizer <- list(
    n_labeled_demos = length(labeled_demos),
    n_bootstrapped_demos = length(bootstrapped_demos),
    total_attempts = budget_summary$attempts,
    error_count = budget_summary$total_errors,
    max_rounds = teleprompter@max_rounds,
    rounds_completed = min(
      teleprompter@max_rounds,
      ceiling(budget_summary$attempts / max(1, length(bootstrap_indices)))
    ),
    budget_summary = budget_summary,
    stop_reason = budget_summary$stop_reason
  )

  # Log trial if logging enabled
  if (!is.null(trial_log)) {
    bootstrap_log_trial(
      trial_log = trial_log,
      student = student,
      valset = valset,
      metric = teleprompter@metric,
      .llm = .llm,
      control = control,
      budget = budget,
      params = list(
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        max_rounds = teleprompter@max_rounds,
        metric_threshold = teleprompter@metric_threshold
      ),
      stage = "bootstrap_log_validation",
      unit_id = paste0(.checkpoint_namespace, ":log-validation")
    )
    budget_summary <- optimizer_budget_summary(budget)
    student$config$optimizer$total_attempts <- budget_summary$attempts
    student$config$optimizer$error_count <- budget_summary$total_errors
    student$config$optimizer$budget_summary <- budget_summary
    student$config$optimizer$stop_reason <- budget_summary$stop_reason
  }

  expected_units <- unlist(lapply(
    seq_len(teleprompter@max_rounds),
    function(round) {
      paste0(
        .checkpoint_namespace,
        ":round:",
        round,
        ":row:",
        bootstrap_indices
      )
    }
  ))
  bootstrap_complete <-
    length(bootstrapped_demos) >= teleprompter@max_bootstrapped_demos ||
    length(expected_units) == 0L ||
    all(vapply(
      expected_units,
      function(id) {
        optimizer_budget_unit_completed(budget, id)
      },
      logical(1)
    ))
  write_bootstrap_checkpoint(
    if (bootstrap_complete) "complete" else "bootstrap"
  )

  student
}

bootstrap_flex_paths <- function(program, path = "$") {
  if (inherits(program, "FlexModule")) {
    return(path)
  }
  if (!inherits(program, "PipelineModule")) {
    return(character())
  }

  unlist(
    lapply(seq_along(program$steps), function(index) {
      bootstrap_flex_paths(
        program$steps[[index]]@module,
        paste0(path, "/steps/", index)
      )
    }),
    use.names = FALSE
  )
}

bootstrap_assert_demo_eligible <- function(program) {
  paths <- bootstrap_flex_paths(program)
  if (length(paths) == 0L) {
    return(invisible(program))
  }

  cli::cli_abort(
    c(
      "BootstrapFewShot cannot optimize Flex demonstrations",
      "x" = "Flex constructs fresh inner predictors for every invocation, so assigning demos to the outer module has no effect.",
      "i" = "Unsupported Flex path{?s}: {.path {paths}}.",
      "i" = "Use GEPA for Flex structure and instruction optimization."
    ),
    class = c(
      "dsprrr_flex_demo_unsupported_error",
      "dsprrr_optimizer_ineligible_error"
    ),
    paths = paths
  )
}

#' Joint compile method for BootstrapFewShot on pipelines
#'
#' Implements DSPy-style whole-program compilation: the teacher pipeline is
#' executed end-to-end on each training example, the final output is scored
#' with the metric, and when a run passes the threshold every step's
#' (inputs, output) pair from that trace becomes a candidate demonstration
#' for the corresponding step module.
#'
#' @noRd
compile_bootstrap_pipeline <- function(
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
  .checkpoint_callback = NULL,
  ...
) {
  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control
  )

  checkpoint_context <- .checkpoint_context
  if (
    is.null(checkpoint_context) &&
      is.null(.optimizer_budget) &&
      optimizer_checkpoint_enabled(control)
  ) {
    checkpoint_context <- optimizer_checkpoint_begin(
      optimizer_name = "BootstrapFewShot",
      optimizer_version = 1L,
      program = program,
      data = list(trainset = trainset, valset = valset),
      metric = teleprompter@metric,
      config = list(
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        max_rounds = teleprompter@max_rounds,
        metric_threshold = teleprompter@metric_threshold,
        seed = teleprompter@seed,
        effective_runtime = optimizer_checkpoint_effective_runtime_identity(
          program,
          .llm = .llm,
          registry = artifact_validate_registry(control@checkpoint_registry),
          path = "effective_runtime"
        ),
        mode = "pipeline"
      ),
      control = control,
      initial_state = list(
        kind = "pipeline",
        step_demos = list(),
        lineage = list()
      ),
      initial_phase = "bootstrap"
    )
    if (identical(checkpoint_context$phase, "complete")) {
      return(checkpoint_context$best_program)
    }
  }

  trial_log <- if (!is.null(teleprompter@log_dir)) {
    TrialLog$new(
      optimizer_name = "BootstrapFewShot",
      log_dir = teleprompter@log_dir
    )
  } else {
    NULL
  }

  trainset <- sample_dataset(trainset, n = NULL, seed = teleprompter@seed)

  # Independent copies: teacher generates traces, student receives demos
  teacher <- program$deepcopy()
  student <- program$deepcopy()

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
      class = "dsprrr_missing_output_column"
    )
  }

  # Steps whose modules can hold demonstrations (e.g., PredictModule)
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
    return(program)
  }

  # Phase 1: labeled demos for the final step only — its inputs/output are
  # the only ones the trainset can label directly. Intermediate steps get
  # demos exclusively from bootstrapped traces.
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

  # Examples not consumed as labeled demos are bootstrap candidates
  bootstrap_indices <- if (n_labeled < nrow(trainset)) {
    seq(n_labeled + 1, nrow(trainset))
  } else {
    integer(0)
  }

  # Phase 2: bootstrap demos for every demo-capable step from passing traces
  checkpoint_state <- checkpoint_context$search_state %||%
    .resume_state %||%
    list()
  if (
    length(checkpoint_state) > 0L &&
      !identical(checkpoint_state$kind %||% "pipeline", "pipeline")
  ) {
    cli::cli_abort(
      "Bootstrap checkpoint has the wrong pipeline search-state kind",
      class = "dsprrr_optimizer_checkpoint_malformed"
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
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  bootstrap_lineage <- checkpoint_state$lineage %||% list()
  budget <- .optimizer_budget %||%
    checkpoint_context$budget %||%
    new_optimizer_budget(control)

  if (!is.null(checkpoint_context) && isTRUE(checkpoint_context$resumed)) {
    outer_rng <- optimizer_checkpoint_capture_rng()
    optimizer_checkpoint_restore_rng(checkpoint_context$rng)
    on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
  }

  write_pipeline_checkpoint <- function(phase = "bootstrap") {
    partial <- student$deepcopy()
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
    if (!is.null(checkpoint_context)) {
      optimizer_checkpoint_write(
        checkpoint_context,
        phase = phase,
        search_state = state,
        lineage = bootstrap_lineage,
        best_program = partial
      )
    }
    if (is.function(.checkpoint_callback)) {
      .checkpoint_callback(
        state = state,
        phase = phase,
        best_program = partial,
        budget = budget
      )
    }
    invisible(NULL)
  }

  all_steps_full <- function() {
    all(vapply(
      demo_steps,
      function(i) {
        length(step_demos[[as.character(i)]]) >=
          teleprompter@max_bootstrapped_demos
      },
      logical(1)
    ))
  }

  for (round in seq_len(teleprompter@max_rounds)) {
    if (
      length(bootstrap_indices) == 0 ||
        all_steps_full() ||
        optimizer_budget_stopped(budget)
    ) {
      break
    }

    # Give the teacher the demos collected so far for this round
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
        .checkpoint_namespace,
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

      row <- trainset[idx, , drop = FALSE]

      example_inputs <- list()
      for (name in pipeline_inputs) {
        if (name %in% names(row)) {
          example_inputs[[name]] <- row[[name]]
        }
      }

      # Field-aware metrics extract their field from `expected` themselves
      # (as in evaluate()), so they receive the full row, not the bare value
      expected <- if (!is.null(metric_field) && metric_field %in% names(row)) {
        row
      } else if (!is.null(output_col) && output_col %in% names(row)) {
        row[[output_col]]
      } else {
        NULL
      }

      teacher_condition <- NULL
      result <- tryCatch(
        teacher$forward(example_inputs, .llm = .llm, trace = TRUE),
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
        write_pipeline_checkpoint()
        if (optimizer_budget_stopped(budget)) {
          break
        }
        next
      }

      # Each forward(trace = TRUE) appends a trace; keep only the one for
      # this attempt so memory does not grow with the number of attempts
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

      passes_threshold <- if (!is.na(score)) {
        if (!is.null(teleprompter@metric_threshold)) {
          score >= teleprompter@metric_threshold
        } else {
          score > 0
        }
      } else {
        FALSE
      }

      if (passes_threshold) {
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
      write_pipeline_checkpoint()

      if (all_steps_full()) {
        break
      }

      if (optimizer_budget_stopped(budget)) {
        break
      }
    }

    if (all_steps_full() || optimizer_budget_stopped(budget)) {
      break
    }
  }

  # Phase 3: assign top-scoring demos to the student's step modules
  n_bootstrapped_total <- 0L
  for (i in demo_steps) {
    key <- as.character(i)
    demos_i <- step_demos[[key]]

    if (length(demos_i) > teleprompter@max_bootstrapped_demos) {
      scores <- vapply(demos_i, function(d) d$score %||% 0, numeric(1))
      top_idx <- order(scores, decreasing = TRUE)[
        seq_len(teleprompter@max_bootstrapped_demos)
      ]
      demos_i <- demos_i[top_idx]
    }

    n_bootstrapped_total <- n_bootstrapped_total + length(demos_i)
    student_module <- student$steps[[i]]@module
    student_module$demos <- c(labeled_demos[[key]], demos_i)
  }

  student$state$compiled <- TRUE
  student$config$compiled <- TRUE
  student$config$teleprompter <- "BootstrapFewShot"
  budget_summary <- optimizer_budget_summary(budget)
  student$config$optimizer <- list(
    joint_pipeline = TRUE,
    n_steps = n_steps,
    demo_steps = demo_steps,
    n_labeled_demos = length(labeled_demos[[as.character(final_step)]]),
    n_bootstrapped_demos = n_bootstrapped_total,
    demos_per_step = stats::setNames(
      lapply(demo_steps, function(i) {
        length(student$steps[[i]]@module$demos)
      }),
      as.character(demo_steps)
    ),
    total_attempts = budget_summary$attempts,
    error_count = budget_summary$total_errors,
    max_rounds = teleprompter@max_rounds,
    budget_summary = budget_summary,
    stop_reason = budget_summary$stop_reason
  )

  if (!is.null(trial_log)) {
    bootstrap_log_trial(
      trial_log = trial_log,
      student = student,
      valset = valset,
      metric = teleprompter@metric,
      .llm = .llm,
      control = control,
      budget = budget,
      params = list(
        joint_pipeline = TRUE,
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        max_rounds = teleprompter@max_rounds,
        metric_threshold = teleprompter@metric_threshold
      ),
      stage = "bootstrap_pipeline_log_validation",
      unit_id = paste0(.checkpoint_namespace, ":log-validation")
    )
    budget_summary <- optimizer_budget_summary(budget)
    student$config$optimizer$total_attempts <- budget_summary$attempts
    student$config$optimizer$error_count <- budget_summary$total_errors
    student$config$optimizer$budget_summary <- budget_summary
    student$config$optimizer$stop_reason <- budget_summary$stop_reason
  }

  expected_units <- unlist(lapply(
    seq_len(teleprompter@max_rounds),
    function(round) {
      paste0(
        .checkpoint_namespace,
        ":round:",
        round,
        ":row:",
        bootstrap_indices
      )
    }
  ))
  pipeline_complete <- all_steps_full() ||
    length(expected_units) == 0L ||
    all(vapply(
      expected_units,
      function(id) {
        optimizer_budget_unit_completed(budget, id)
      },
      logical(1)
    ))
  write_pipeline_checkpoint(if (pipeline_complete) "complete" else "bootstrap")

  student
}

#' Run module with specific settings
#' @noRd
run_with_settings <- function(module, inputs, .llm = NULL, settings = list()) {
  # For now, we pass settings through the run call

  # Future: could modify the LLM or module config temporarily

  # Convert inputs list to named arguments
  args <- inputs
  args$.llm <- .llm

  # If temperature is specified, we would ideally pass it to the LLM

  # For now, we rely on the default module behavior
  # TODO: Add support for per-call temperature override in ellmer

  do.call(run, c(list(module), args))
}

#' Find output column in trainset
#' @noRd
find_output_column <- function(trainset, input_names) {
  # Try common names first
  possible_output_names <- c(
    "output",
    "label",
    "answer",
    "response",
    "result",
    "y"
  )

  for (col in possible_output_names) {
    if (col %in% names(trainset)) {
      return(col)
    }
  }

  # Use any column not in inputs
  remaining_cols <- setdiff(names(trainset), input_names)
  if (length(remaining_cols) > 0) {
    return(remaining_cols[1])
  }

  NULL
}

#' Print method for BootstrapFewShot
#' @param x A BootstrapFewShot object
#' @param ... Additional arguments (unused)
#' @export
print.BootstrapFewShot <- function(x, ...) {
  cli::cli_h3("BootstrapFewShot Teleprompter")

  cli::cli_text("{.field max_bootstrapped_demos}: {x@max_bootstrapped_demos}")
  cli::cli_text("{.field max_labeled_demos}: {x@max_labeled_demos}")
  cli::cli_text("{.field max_rounds}: {x@max_rounds}")

  if (!is.null(x@metric_threshold)) {
    cli::cli_text("{.field metric_threshold}: {x@metric_threshold}")
  }

  if (!is.null(x@seed)) {
    cli::cli_text("{.field seed}: {x@seed}")
  }

  invisible(x)
}

# Register S7 print method
S7::method(print, BootstrapFewShot) <- print.BootstrapFewShot
