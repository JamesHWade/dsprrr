# BootstrapFewShot Teleprompter
#
# DSPy-style demonstration bootstrapping optimizer.
# Uses a teacher model to generate demonstrations from training examples,
# selecting those that pass a metric threshold.

#' BootstrapFewShot: keep the program's own successful outputs as demos
#'
#' @include teleprompter.R optimizer-core.R
#'
#' @description
#' `BootstrapFewShot()` runs the program on training rows, scores each output
#' with `metric`, and keeps the outputs that pass as few-shot demonstrations.
#' Labeled training rows can be added as demonstrations too. This is DSPy's
#' basic demonstration optimizer and a building block of
#' [BootstrapFewShotWithRandomSearch()] and [MIPROv2()].
#'
#' @details
#' Compilation works through the training rows in their original order:
#'
#' 1. The first `max_labeled_demos` rows become labeled demonstrations, copied
#'    from the data.
#' 2. A copy of the program (the teacher) runs on each remaining row, using
#'    the demonstrations collected so far. An output that passes the metric
#'    becomes a demonstration. This stops once `max_bootstrapped_demos` are
#'    collected.
#' 3. With `max_rounds` above `1L`, step 2 repeats over the same rows until
#'    enough demonstrations are collected.
#'
#' The compiled copy gets the labeled demonstrations followed by the
#' bootstrapped ones. Rows used as labeled demonstrations are never
#' bootstrapped, so with the default `max_labeled_demos = 16L` a training set of
#' 16 rows or fewer yields labeled demonstrations only and makes no model
#' calls. Set `max_labeled_demos = 0L` to bootstrap from every row.
#'
#' The teacher runs with the `.llm` passed to [compile()], or with the chat the
#' program would otherwise use.
#'
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
#' Programs containing Flex or an RLM, at the root or nested, are rejected.
#' Flex constructs its inner predictors per invocation; RLM root examples do
#' not match its children's `state -> ...` signatures. Use GEPA for these
#' programs, or instruction-only MIPROv2 for an RLM graph.
#'
#' @param metric A metric function (required). Use a field-aware metric such
#'   as `metric_exact_match(field = "answer")`: like [evaluate()], it receives
#'   the whole training row as `expected`, and its `field` names the column
#'   that supplies labeled demonstrations. A metric without a `field` receives
#'   only the bare value of the label column, which is the first column named
#'   `output`, `label`, `answer`, `response`, `result` or `y`, or else the
#'   first non-input column.
#' @param metric_threshold Minimum score for a bootstrapped output to become a
#'   demonstration. `NULL` (the default) keeps any output that scores above 0.
#' @param max_errors Integer; stop after this many consecutive failed attempts
#'   when [compile()] gets no `control` (default `5L`).
#' @param max_bootstrapped_demos Integer maximum number of bootstrapped
#'   demonstrations (default `4L`).
#' @param max_labeled_demos Integer number of leading training rows used as
#'   labeled demonstrations (default `16L`). See Details.
#' @param max_rounds Integer number of passes over the remaining rows
#'   (default `1L`).
#' @param teacher_settings A list of settings meant for the teacher, such as
#'   `list(temperature = 0.7)`. It is currently not applied: the teacher runs
#'   with the same chat and settings as the program.
#' @param seed Recorded with the run, but it does not currently change the
#'   result: the training rows are used in their original order. Shuffle the
#'   training set yourself to vary which rows are used.
#' @param log_dir Directory for a [TrialLog] of the run, or `NULL` (the
#'   default) for none. When `valset` is passed to [compile()], the compiled
#'   program is scored on it for the log.
#'
#' @return A `BootstrapFewShot` object to pass to [compile()].
#' @family teleprompters
#' @export
#'
#' @examples
#' tp <- BootstrapFewShot(
#'   metric = metric_exact_match(field = "answer"),
#'   max_bootstrapped_demos = 2L,
#'   max_labeled_demos = 0L
#' )
#' tp
#'
#' \dontrun{
#' qa <- module(signature("question -> answer"))
#' trainset <- data.frame(
#'   question = c(
#'     "Capital of France?", "Capital of Japan?", "Capital of Peru?"
#'   ),
#'   answer = c("Paris", "Tokyo", "Lima")
#' )
#' compiled <- compile(
#'   qa,
#'   tp,
#'   trainset,
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' best_demos(compiled, as_tibble = TRUE)
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
    total_latency_ms = summary$elapsed_seconds * 1000,
    trace_context = current_trace_context()
  )
}

bootstrap_record_result <- function(student, details, budget, trial_log) {
  budget_summary <- optimizer_budget_summary(budget)
  trials <- if (is.null(trial_log)) {
    tibble::tibble()
  } else {
    trial_log$as_tibble()
  }
  scores <- if ("mean_score" %in% names(trials)) {
    trials$mean_score
  } else if ("score" %in% names(trials)) {
    trials$score
  } else {
    numeric()
  }
  valid_scores <- which(!is.na(scores))
  best_trial <- if (length(valid_scores) > 0L) {
    valid_scores[[which.max(scores[valid_scores])]]
  } else {
    NULL
  }
  best_score <- if (is.null(best_trial)) NULL else scores[[best_trial]]
  best_params <- list(
    n_labeled_demos = details$n_labeled_demos,
    n_bootstrapped_demos = details$n_bootstrapped_demos
  )

  record_optimization_result(
    student,
    optimizer = "BootstrapFewShot",
    status = if (optimizer_budget_stopped(budget)) "partial" else "completed",
    best_score = best_score,
    best_trial = best_trial,
    best_params = best_params,
    trials = trials,
    budget = budget_summary,
    stop_reason = optimization_stop_reason(budget_summary),
    extensions = details
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
  validation <- bootstrap_validate_inputs(teleprompter, program, trainset)
  if (validation$empty) {
    return(program)
  }

  if (validation$pipeline) {
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

  plan <- bootstrap_module_plan(
    teleprompter = teleprompter,
    program = program,
    trainset = trainset,
    valset = valset,
    .llm = .llm,
    control = control,
    .optimizer_budget = .optimizer_budget,
    .checkpoint_context = .checkpoint_context,
    .checkpoint_namespace = .checkpoint_namespace,
    .resume_state = .resume_state,
    .checkpoint_callback = .checkpoint_callback
  )
  if (plan$complete) {
    return(plan$program)
  }

  if (
    !is.null(plan$checkpoint_context) &&
      isTRUE(plan$checkpoint_context$resumed)
  ) {
    outer_rng <- optimizer_checkpoint_capture_rng()
    optimizer_checkpoint_restore_rng(plan$checkpoint_context$rng)
    on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
  }

  state <- bootstrap_module_execute(plan)
  bootstrap_finalize_module(plan, state)
}

bootstrap_flex_paths <- function(program) {
  modules <- named_modules(
    program,
    include_root = TRUE,
    boundaries = "respect"
  )
  names(modules)[vapply(
    modules,
    inherits,
    logical(1),
    what = "FlexModule"
  )]
}

bootstrap_assert_demo_eligible <- function(
  program,
  optimizer_name = "BootstrapFewShot"
) {
  paths <- bootstrap_flex_paths(program)
  if (length(paths) > 0L) {
    cli::cli_abort(
      c(
        "{optimizer_name} cannot optimize Flex demonstrations",
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

  modules <- named_modules(
    program,
    include_root = TRUE,
    boundaries = "respect"
  )
  rlm_paths <- names(modules)[vapply(
    modules,
    inherits,
    logical(1),
    what = "RLMModule"
  )]
  if (length(rlm_paths) > 0L) {
    cli::cli_abort(
      c(
        "{optimizer_name} cannot derive demos for RLM predictors",
        "x" = "Root training examples do not match the child predictor signatures.",
        "i" = "Unsupported RLM path{?s}: {.path {rlm_paths}}.",
        "i" = "Use GEPA for RLM child instructions, or MIPROv2 with {.code max_bootstrapped_demos = 0L}."
      ),
      class = c(
        "dsprrr_bootstrap_graph_unsupported",
        "dsprrr_optimizer_ineligible_error"
      ),
      paths = rlm_paths
    )
  }

  invisible(program)
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
  plan <- bootstrap_pipeline_plan(
    teleprompter = teleprompter,
    program = program,
    trainset = trainset,
    valset = valset,
    .llm = .llm,
    control = control,
    .optimizer_budget = .optimizer_budget,
    .checkpoint_context = .checkpoint_context,
    .checkpoint_namespace = .checkpoint_namespace,
    .resume_state = .resume_state,
    .checkpoint_callback = .checkpoint_callback
  )
  if (plan$complete) {
    return(plan$program)
  }

  if (
    !is.null(plan$checkpoint_context) &&
      isTRUE(plan$checkpoint_context$resumed)
  ) {
    outer_rng <- optimizer_checkpoint_capture_rng()
    optimizer_checkpoint_restore_rng(plan$checkpoint_context$rng)
    on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
  }

  state <- bootstrap_pipeline_execute(plan)
  bootstrap_finalize_pipeline(plan, state)
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

# Print a BootstrapFewShot object through its S7 method.
print_bootstrap_few_shot <- function(x, ...) {
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
