# Optimizer Core Infrastructure
#
# Shared infrastructure for all optimizers:
# - Evaluation runner
# - Candidate generation
# - Cost tracking
# - Reproducibility (seed support)
# - Budget/stopping conditions

optimizer_limit_validator <- function(
  value,
  name,
  integer = FALSE,
  positive = FALSE
) {
  if (is.null(value)) {
    return(NULL)
  }
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < (if (positive) 1 else 0) ||
      (integer && value != as.integer(value))
  ) {
    qualifier <- if (positive) "positive" else "non-negative"
    kind <- if (integer) "integer" else "number"
    return(sprintf("%s must be a %s %s or NULL", name, qualifier, kind))
  }
  NULL
}

#' Internal optimizer-control record class
#' @noRd
OptimizerControl <- S7::new_class(
  "OptimizerControl",
  properties = list(
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
    max_trials = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && (!is.numeric(value) || value < 1)) {
          return("max_trials must be a positive integer or NULL")
        }
        NULL
      }
    ),
    max_errors = S7::new_property(
      S7::class_integer,
      default = 5L,
      validator = function(value) {
        if (value < 0) {
          return("max_errors must be non-negative")
        }
        NULL
      }
    ),
    max_metric_calls = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_metric_calls", integer = TRUE)
      }
    ),
    max_provider_calls = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_provider_calls", integer = TRUE)
      }
    ),
    max_input_tokens = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_input_tokens", integer = TRUE)
      }
    ),
    max_output_tokens = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_output_tokens", integer = TRUE)
      }
    ),
    max_total_tokens = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_total_tokens", integer = TRUE)
      }
    ),
    max_cost = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) optimizer_limit_validator(value, "max_cost")
    ),
    max_elapsed_seconds = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_elapsed_seconds")
      }
    ),
    num_threads = S7::new_property(
      S7::class_integer,
      default = 1L,
      validator = function(value) {
        if (value < 1) {
          return("num_threads must be at least 1")
        }
        NULL
      }
    ),
    progress = S7::new_property(
      S7::class_logical,
      default = NA,
      validator = function(value) {
        if (length(value) != 1) {
          return("progress must be a single logical value")
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
    ),
    checkpoint_path = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (
          !is.null(value) &&
            (!is.character(value) ||
              length(value) != 1L ||
              is.na(value) ||
              !nzchar(value))
        ) {
          return("checkpoint_path must be one non-empty string or NULL")
        }
        NULL
      }
    ),
    resume = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = function(value) {
        if (length(value) != 1L || is.na(value)) {
          return("resume must be TRUE or FALSE")
        }
        NULL
      }
    ),
    checkpoint_registry = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        if (
          length(value) > 0L &&
            (is.null(names(value)) ||
              anyNA(names(value)) ||
              !all(nzchar(names(value))) ||
              anyDuplicated(names(value)))
        ) {
          return("checkpoint_registry must have unique, non-empty names")
        }
        NULL
      }
    ),
    verbose = S7::new_property(
      S7::class_logical,
      default = FALSE
    )
  )
)

#' Create Optimizer Control
#'
#' @description
#' Configure optimizer behavior with consistent defaults across optimizer types.
#'
#' @param seed Random seed for reproducibility. Default is `NULL` (no seed).
#' @param max_trials Maximum number of trials to run. Default is `NULL`
#'   (unlimited).
#' @param max_errors Non-negative integer error budget. Optimizers report total
#'   errors while stopping on a separate consecutive-error streak; each success
#'   resets only that streak. A positive value stops on the failure that reaches
#'   the limit. Zero permits work to begin but stops after the first failure.
#'   When a completed evaluation returns multiple outcomes, all are included in
#'   the final counters even if the stop boundary was crossed partway through;
#'   the first stop reason remains unchanged and prevents scheduling new work.
#' @param max_metric_calls Maximum metric calls, or `NULL` for unlimited.
#' @param max_provider_calls Maximum verified provider calls, or `NULL` for
#'   unlimited. Ambiguous provider usage stops a run that has this cap.
#' @param max_input_tokens Maximum verified input tokens, or `NULL` for unlimited.
#' @param max_output_tokens Maximum verified output tokens, or `NULL` for
#'   unlimited.
#' @param max_total_tokens Maximum verified input plus output tokens, or `NULL`
#'   for unlimited.
#' @param max_cost Maximum known provider cost in US dollars, or `NULL` for
#'   unlimited. Unknown cost stops a run that has this cap.
#' @param max_elapsed_seconds Maximum active optimizer elapsed time in seconds,
#'   or `NULL` for unlimited. Checkpoint downtime is excluded.
#' @param num_threads Number of threads for parallel evaluation. Default is 1.
#' @param progress Whether to display progress. Default is `TRUE` in interactive
#'   sessions.
#' @param log_dir Directory for trial logging. Default is `NULL` (no logging).
#' @param checkpoint_path Optional optimizer checkpoint file.
#' @param resume Whether to resume from `checkpoint_path`.
#' @param checkpoint_registry Named runtime registry used by safe program
#'   artifacts stored in checkpoints.
#' @param verbose Whether to print detailed output. Default is `FALSE`.
#' @details
#' Finite metric, provider, token, cost, and elapsed-time limits switch
#' optimizer evaluation to row-sized work units. The maximum postflight
#' overshoot is one already-started evaluation row, or one already-started
#' direct provider request for optimizer-side generation. Unknown provider,
#' token, or cost usage stops safely when the corresponding cap is finite.
#'
#' BootstrapFewShot and MIPROv2 support deterministic checkpoint resume. GEPA,
#' SIMBA, and COPRO currently provide the shared ledger and return the best
#' partial program, but reject `resume = TRUE` until their fine-grained search
#' state is supported.
#'
#' @return An optimizer control object.
#' @export
#'
#' @examples
#' # Default control
#' ctrl <- optimizer_control()
#'
#' # With specific settings
#' ctrl <- optimizer_control(seed = 42L, max_trials = 100L, log_dir = "logs/")
optimizer_control <- function(
  seed = NULL,
  max_trials = NULL,
  max_errors = 5L,
  max_metric_calls = NULL,
  max_provider_calls = NULL,
  max_input_tokens = NULL,
  max_output_tokens = NULL,
  max_total_tokens = NULL,
  max_cost = NULL,
  max_elapsed_seconds = NULL,
  num_threads = 1L,
  progress = NA,
  log_dir = NULL,
  checkpoint_path = NULL,
  resume = FALSE,
  checkpoint_registry = list(),
  verbose = FALSE
) {
  # Resolve progress default
  if (is.na(progress)) {
    progress <- interactive()
  }

  integer_limits <- list(
    max_metric_calls = max_metric_calls,
    max_provider_calls = max_provider_calls,
    max_input_tokens = max_input_tokens,
    max_output_tokens = max_output_tokens,
    max_total_tokens = max_total_tokens
  )
  for (name in names(integer_limits)) {
    message <- optimizer_limit_validator(
      integer_limits[[name]],
      name,
      integer = TRUE
    )
    if (!is.null(message)) {
      cli::cli_abort(message, class = "dsprrr_optimizer_control_error")
    }
  }

  control <- OptimizerControl(
    seed = seed,
    max_trials = if (!is.null(max_trials)) as.integer(max_trials) else NULL,
    max_errors = as.integer(max_errors),
    max_metric_calls = if (!is.null(max_metric_calls)) {
      as.integer(max_metric_calls)
    } else {
      NULL
    },
    max_provider_calls = if (!is.null(max_provider_calls)) {
      as.integer(max_provider_calls)
    } else {
      NULL
    },
    max_input_tokens = if (!is.null(max_input_tokens)) {
      as.integer(max_input_tokens)
    } else {
      NULL
    },
    max_output_tokens = if (!is.null(max_output_tokens)) {
      as.integer(max_output_tokens)
    } else {
      NULL
    },
    max_total_tokens = if (!is.null(max_total_tokens)) {
      as.integer(max_total_tokens)
    } else {
      NULL
    },
    max_cost = max_cost,
    max_elapsed_seconds = max_elapsed_seconds,
    num_threads = as.integer(num_threads),
    progress = progress,
    log_dir = log_dir,
    checkpoint_path = checkpoint_path,
    resume = resume,
    checkpoint_registry = checkpoint_registry,
    verbose = verbose
  )

  if (isTRUE(resume) && is.null(checkpoint_path)) {
    cli::cli_abort(
      c(
        "Cannot resume without an optimizer checkpoint",
        "i" = "Supply {.arg checkpoint_path} when {.code resume = TRUE}."
      ),
      class = "dsprrr_optimizer_checkpoint_config_error"
    )
  }

  control
}

optimizer_control_for_teleprompter <- function(
  teleprompter,
  control = NULL,
  num_threads = 1L
) {
  if (!is.null(control)) {
    if (!inherits(control, "dsprrr::OptimizerControl")) {
      cli::cli_abort(
        "{.arg control} must be created by {.fn optimizer_control}",
        class = "dsprrr_optimizer_control_error"
      )
    }
    return(control)
  }
  optimizer_control(
    seed = tryCatch(teleprompter@seed, error = function(e) NULL),
    max_errors = teleprompter@max_errors,
    num_threads = num_threads,
    log_dir = tryCatch(teleprompter@log_dir, error = function(e) NULL)
  )
}

optimizer_require_ledger_only_checkpoint <- function(control, optimizer) {
  if (isTRUE(control@resume)) {
    cli::cli_abort(
      c(
        "{optimizer} does not yet support fine-grained checkpoint resume",
        "i" = "Resource budgets and best-partial return are supported.",
        "i" = "Start without {.code resume = TRUE}; resume support is tracked separately."
      ),
      class = "dsprrr_optimizer_checkpoint_unsupported_optimizer"
    )
  }
  if (!is.null(control@checkpoint_path)) {
    cli::cli_warn(
      c(
        "{optimizer} is running without checkpoint persistence",
        "i" = "Its resource ledger and best partial program remain available in memory."
      ),
      class = "dsprrr_optimizer_checkpoint_unsupported_optimizer"
    )
  }
  invisible(control)
}

#' Evaluation Result
#'
#' @description
#' S7 class representing the result of evaluating a program on a dataset.
#' Contains per-example results plus aggregated summary statistics.
#'
#' @noRd
EvalResult <- S7::new_class(
  "EvalResult",
  properties = list(
    # Per-example data
    examples = S7::new_property(
      S7::class_data.frame,
      default = quote(data.frame())
    ),
    # Aggregated summary
    mean_score = S7::new_property(S7::class_numeric, default = NA_real_),
    std_error = S7::new_property(S7::class_any, default = NA_real_),
    n_evaluated = S7::new_property(S7::class_integer, default = 0L),
    n_errors = S7::new_property(S7::class_integer, default = 0L),
    # Cost tracking
    input_tokens = S7::new_property(S7::class_integer, default = 0L),
    output_tokens = S7::new_property(S7::class_integer, default = 0L),
    total_tokens = S7::new_property(S7::class_integer, default = 0L),
    total_cost = S7::new_property(S7::class_numeric, default = 0),
    provider_calls = S7::new_property(S7::class_integer, default = 0L),
    metric_calls = S7::new_property(S7::class_integer, default = 0L),
    provider_usage_unknown = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    token_usage_unknown = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    # Timing
    total_latency_ms = S7::new_property(S7::class_numeric, default = 0),
    start_time = S7::new_property(S7::class_any, default = NULL),
    end_time = S7::new_property(S7::class_any, default = NULL),
    # Epochs support (for multi-epoch evaluation)
    epochs = S7::new_property(S7::class_integer, default = 1L),
    epoch_scores = S7::new_property(S7::class_list, default = list()),
    score_std = S7::new_property(S7::class_any, default = NA_real_),
    ci_lower = S7::new_property(S7::class_any, default = NA_real_),
    ci_upper = S7::new_property(S7::class_any, default = NA_real_),
    trace_context = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        trace_context_validate(value, arg = "trace_context")
        NULL
      }
    )
  )
)

#' Evaluate a Program on a Dataset
#'
#' @description
#' Standard evaluation function for optimizers. Executes a module on a dataset,
#' applies a metric to each example, and returns detailed per-example results
#' plus aggregated statistics.
#'
#' This is the core evaluation function used by all optimizers. It wraps
#' [evaluate()] with enhanced output including:
#' - Per-example timing and error information
#' - Aggregated cost tracking
#' - Standard error computation
#' - Multi-epoch evaluation for statistical significance (when epochs > 1)
#'
#' @param program A DSPrrr module to evaluate.
#' @param dataset A data frame containing test examples.
#' @param metric A metric function for scoring predictions.
#' @param .llm Optional ellmer Chat object for LLM calls.
#' @param control An object created by [optimizer_control()], or `NULL` for
#'   defaults.
#' @param epochs Integer; number of times to repeat evaluation for statistical
#'   significance. Defaults to 1L. When > 1, computes std and confidence intervals.
#' @param ... Additional arguments passed to [evaluate()].
#' @param .trace_context A named JSON-compatible correlation context copied to
#'   the returned `EvalResult` and all program execution traces.
#'
#' @return An EvalResult object containing:
#'   - `examples`: tibble with per-example row_id, score, error, predicted,
#'     feedback (textual feedback from feedback-aware metrics, see
#'     [metric_with_feedback()]), and input columns (prefixed with input_*)
#'   - `mean_score`: mean score across successful evaluations
#'   - `std_error`: standard error of per-example scores (SD / sqrt(n))
#'   - `n_evaluated`: number of successful evaluations
#'   - `n_errors`: number of failed evaluations
#'   - `total_tokens`: total tokens used
#'   - `total_cost`: total cost in USD
#'   - `total_latency_ms`: total time in milliseconds
#'   - `trace_context`: the validated correlation context for this evaluation
#'
#'   When `epochs > 1`, additional fields:
#'   - `epochs`: number of epochs run
#'   - `epoch_scores`: list of score vectors, one per epoch
#'   - `score_std`: standard deviation of mean scores across epochs
#'   - `ci_lower`, `ci_upper`: 95% confidence interval bounds
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sig <- signature("question -> answer")
#' mod <- module(sig)
#'
#' dataset <- tibble::tibble(
#'   question = c("What is 2+2?", "What is 3+3?"),
#'   answer = c("4", "6")
#' )
#'
#' result <- eval_program(
#'   mod,
#'   dataset,
#'   metric = metric_exact_match(field = "answer"),
#'   .llm = ellmer::chat_openai()
#' )
#'
#' result@mean_score
#' result@examples
#' }
eval_program <- function(
  program,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  epochs = 1L,
  ...,
  .trace_context = list()
) {
  # Validate inputs
  if (!inherits(program, "Module")) {
    cli::cli_abort("{.arg program} must be a DSPrrr Module object")
  }

  trace_context_supplied <- !missing(.trace_context)
  trace_context <- trace_context_resolve(
    .trace_context,
    supplied = trace_context_supplied
  )
  previous_trace_context <- trace_context_enter(
    trace_context,
    program = program,
    inherit_program_id = !trace_context_supplied
  )
  on.exit(trace_context_restore(previous_trace_context), add = TRUE)

  if (!is.data.frame(dataset)) {
    cli::cli_abort("{.arg dataset} must be a data frame or tibble")
  }

  if (!is.function(metric)) {
    cli::cli_abort("{.arg metric} must be a function")
  }

  # Validate epochs
  epochs <- as.integer(epochs)
  if (length(epochs) != 1 || epochs < 1) {
    cli::cli_abort(c(
      "{.arg epochs} must be a positive integer",
      "x" = "Got {epochs}",
      "i" = "Use epochs = 1L for single evaluation or epochs > 1 for multiple runs"
    ))
  }

  # Use default control if not provided
  if (is.null(control)) {
    control <- optimizer_control()
  }

  # Handle empty dataset
  if (nrow(dataset) == 0) {
    return(EvalResult(
      examples = tibble::tibble(),
      mean_score = NA_real_,
      std_error = NA_real_,
      n_evaluated = 0L,
      n_errors = 0L,
      trace_context = trace_context
    ))
  }

  start_time <- Sys.time()

  # Clear traces before evaluation to get accurate cost for this run
  # Use copy_module to properly preserve custom class behavior (e.g., mock modules in tests)
  program_copy <- copy_module(program)

  # Run evaluation using existing evaluate() function with error handling
  # OptimizerControl@max_errors is enforced by the optimizer outcome ledger.
  # Batch max_errors has different row-cancellation semantics and must not
  # truncate an optimizer evaluation before that ledger observes the result.
  concurrency <- concurrency_control(
    max_active = control@num_threads,
    max_errors = Inf
  )
  eval_result <- tryCatch(
    {
      evaluate(
        program_copy,
        data = dataset,
        metric = metric,
        .llm = .llm,
        .concurrency = concurrency,
        .progress = control@progress,
        .return_format = "structured",
        epochs = epochs,
        ...
      )
    },
    error = function(e) {
      provider_condition <- run_provider_error_condition(e)
      if (!is.null(provider_condition)) {
        stop(provider_condition)
      }
      cli::cli_warn(
        "Evaluation failed: {conditionMessage(e)}",
        class = "dsprrr_eval_error"
      )
      # Return a result structure indicating total failure
      list(
        scores = rep(NA_real_, nrow(dataset)),
        predictions = rep(NA_character_, nrow(dataset)),
        errors = rep(conditionMessage(e), nrow(dataset)),
        mean_score = NA_real_,
        n_evaluated = 0L,
        n_errors = nrow(dataset),
        epoch_scores = NULL,
        score_std = NA_real_,
        ci_95 = c(NA_real_, NA_real_)
      )
    }
  )

  end_time <- Sys.time()
  total_latency_ms <- as.numeric(difftime(
    end_time,
    start_time,
    units = "secs"
  )) *
    1000

  # Prefer the canonical per-row execution metadata returned by evaluate().
  # Traces are only a fallback: one program trace is not necessarily one
  # provider call for composite or iterative modules.
  cost_info <- extract_optimizer_usage(
    program_copy,
    eval_result$metadata %||% NULL,
    epochs = epochs
  )

  # Build per-example tibble
  n <- nrow(dataset)
  ordered_errors <- rep(NA_character_, n)
  compact_errors <- eval_result$errors %||% character(0)
  if (length(compact_errors) == n) {
    ordered_errors <- compact_errors
  } else if (length(compact_errors) > 0L) {
    failed_rows <- which(is.na(eval_result$scores))
    assign_count <- min(length(compact_errors), length(failed_rows))
    if (assign_count > 0L) {
      ordered_errors[failed_rows[seq_len(assign_count)]] <-
        compact_errors[seq_len(assign_count)]
    }
  }

  examples <- tibble::tibble(
    row_id = seq_len(n),
    score = eval_result$scores,
    error = ordered_errors,
    predicted = eval_result$predictions,
    feedback = if (length(eval_result$feedbacks %||% character(0)) == n) {
      eval_result$feedbacks
    } else {
      rep(NA_character_, n)
    },
    program_trace = if (length(eval_result$traces %||% list()) == n) {
      eval_result$traces
    } else {
      rep(list(NULL), n)
    }
  )

  # Add input columns from dataset
  input_names <- get_input_names(program$signature)
  for (name in input_names) {
    if (name %in% names(dataset)) {
      examples[[paste0("input_", name)]] <- dataset[[name]]
    }
  }

  # Compute standard error
  valid_scores <- eval_result$scores[!is.na(eval_result$scores)]
  std_error <- if (length(valid_scores) > 1) {
    stats::sd(valid_scores) / sqrt(length(valid_scores))
  } else {
    NA_real_
  }

  # Extract epoch-specific fields if present
  epoch_count <- as.integer(epochs)
  epoch_scores_list <- if (!is.null(eval_result$epoch_scores)) {
    eval_result$epoch_scores
  } else {
    list()
  }
  score_std <- if (!is.null(eval_result$score_std)) {
    eval_result$score_std
  } else {
    NA_real_
  }
  ci_values <- if (!is.null(eval_result$ci_95)) {
    eval_result$ci_95
  } else {
    c(NA_real_, NA_real_)
  }

  EvalResult(
    examples = examples,
    mean_score = eval_result$mean_score,
    std_error = std_error,
    n_evaluated = as.integer(eval_result$n_evaluated),
    n_errors = as.integer(eval_result$n_errors),
    input_tokens = as.integer(cost_info$tokens_in),
    output_tokens = as.integer(cost_info$tokens_out),
    total_tokens = as.integer(cost_info$total_tokens),
    total_cost = cost_info$total_cost,
    provider_calls = as.integer(cost_info$provider_calls),
    metric_calls = as.integer(cost_info$metric_calls),
    provider_usage_unknown = cost_info$provider_usage_unknown,
    token_usage_unknown = cost_info$token_usage_unknown,
    total_latency_ms = total_latency_ms,
    start_time = start_time,
    end_time = end_time,
    epochs = epoch_count,
    epoch_scores = epoch_scores_list,
    score_std = score_std,
    ci_lower = ci_values[1],
    ci_upper = ci_values[2],
    trace_context = eval_result$trace_context %||% current_trace_context()
  )
}

optimizer_usage_scalar <- function(value, integer = FALSE) {
  if (
    is.null(value) ||
      length(value) != 1L ||
      !is.numeric(value) ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0
  ) {
    return(if (integer) NA_integer_ else NA_real_)
  }
  if (integer) as.integer(value) else as.numeric(value)
}

optimizer_usage_sum <- function(values, integer = FALSE) {
  values <- unlist(values, use.names = FALSE)
  if (length(values) == 0L) {
    return(if (integer) 0L else 0)
  }
  if (anyNA(values)) {
    return(if (integer) NA_integer_ else NA_real_)
  }
  total <- sum(values)
  if (integer) as.integer(total) else total
}

optimizer_metadata_provider_calls <- function(program, metadata) {
  if ("provider_calls" %in% names(metadata)) {
    return(optimizer_usage_scalar(
      metadata$provider_calls,
      integer = TRUE
    ))
  }

  if (inherits(program, "FlexModule")) {
    predictor_calls <- optimizer_usage_scalar(
      metadata$predictor_calls,
      integer = TRUE
    )
    step_metadata <- metadata$steps %||% list()
    if (
      is.na(predictor_calls) ||
        length(step_metadata) != predictor_calls
    ) {
      return(predictor_calls)
    }
    calls <- vapply(
      step_metadata,
      function(step) {
        item <- step$metadata %||% list()
        if ("provider_calls" %in% names(item)) {
          return(optimizer_usage_scalar(
            item$provider_calls,
            integer = TRUE
          ))
        }
        if (identical(item$cache %||% "unknown", "hit")) 0L else 1L
      },
      integer(1)
    )
    return(optimizer_usage_sum(calls, integer = TRUE))
  }

  if (identical(class(program)[1L], "PredictModule")) {
    cache_status <- metadata$cache %||% "unknown"
    if (identical(cache_status, "hit")) {
      return(0L)
    }
    # The base Predict batch/scalar path invokes exactly one structured Chat
    # request. Specialized subclasses are handled below as unknown unless they
    # report an explicit count.
    return(1L)
  }

  if (inherits(program, "PipelineModule")) {
    step_metadata <- metadata$step_metadata %||%
      metadata$aggregated$step_metadata %||%
      list()
    if (length(step_metadata) != length(program$steps)) {
      return(NA_integer_)
    }
    calls <- vapply(
      seq_along(program$steps),
      function(i) {
        optimizer_metadata_provider_calls(
          program$steps[[i]]@module,
          step_metadata[[i]]
        )
      },
      integer(1)
    )
    return(optimizer_usage_sum(calls, integer = TRUE))
  }

  if (inherits(program, "FnModule") && is.null(metadata$model)) {
    return(0L)
  }

  NA_integer_
}

optimizer_metadata_usage <- function(program, metadata) {
  if (!is.list(metadata)) {
    return(list(
      tokens_in = NA_integer_,
      tokens_out = NA_integer_,
      total_tokens = NA_integer_,
      total_cost = NA_real_,
      provider_calls = NA_integer_
    ))
  }

  provider_calls <- optimizer_metadata_provider_calls(program, metadata)
  if (identical(metadata$cache %||% NULL, "hit")) {
    return(list(
      tokens_in = 0L,
      tokens_out = 0L,
      total_tokens = 0L,
      total_cost = 0,
      provider_calls = provider_calls
    ))
  }

  tokens_in <- optimizer_usage_scalar(metadata$input_tokens, integer = TRUE)
  tokens_out <- optimizer_usage_scalar(metadata$output_tokens, integer = TRUE)
  total_tokens <- optimizer_usage_scalar(metadata$total_tokens, integer = TRUE)
  if (is.na(total_tokens) && !anyNA(c(tokens_in, tokens_out))) {
    total_tokens <- tokens_in + tokens_out
  }

  list(
    tokens_in = tokens_in,
    tokens_out = tokens_out,
    total_tokens = total_tokens,
    total_cost = optimizer_usage_scalar(
      metadata$cost,
      integer = FALSE
    ),
    provider_calls = provider_calls
  )
}

# Extract only usage that can be proved from canonical current-call metadata.
# Ambiguous provider or token usage is represented explicitly rather than as 0.
extract_optimizer_usage <- function(program, metadata, epochs = 1L) {
  if (is.list(metadata) && length(metadata) > 0L && epochs == 1L) {
    rows <- lapply(metadata, function(item) {
      optimizer_metadata_usage(program, item)
    })
    tokens_in <- optimizer_usage_sum(
      lapply(rows, `[[`, "tokens_in"),
      integer = TRUE
    )
    tokens_out <- optimizer_usage_sum(
      lapply(rows, `[[`, "tokens_out"),
      integer = TRUE
    )
    total_tokens <- optimizer_usage_sum(
      lapply(rows, `[[`, "total_tokens"),
      integer = TRUE
    )
    total_cost <- optimizer_usage_sum(lapply(rows, `[[`, "total_cost"))
    provider_calls <- optimizer_usage_sum(
      lapply(rows, `[[`, "provider_calls"),
      integer = TRUE
    )
    metric_calls <- sum(vapply(
      metadata,
      function(item) {
        !optimizer_error_present(item$error %||% NULL)
      },
      logical(1)
    ))

    return(list(
      tokens_in = tokens_in,
      tokens_out = tokens_out,
      total_tokens = total_tokens,
      total_cost = total_cost,
      provider_calls = provider_calls,
      metric_calls = as.integer(metric_calls),
      provider_usage_unknown = is.na(provider_calls),
      token_usage_unknown = anyNA(c(tokens_in, tokens_out, total_tokens))
    ))
  }

  fallback <- extract_cost_from_module(program)
  list(
    tokens_in = fallback$tokens_in,
    tokens_out = fallback$tokens_out,
    total_tokens = fallback$total_tokens,
    total_cost = fallback$total_cost,
    provider_calls = NA_integer_,
    metric_calls = NA_integer_,
    provider_usage_unknown = TRUE,
    token_usage_unknown = anyNA(c(
      fallback$tokens_in,
      fallback$tokens_out,
      fallback$total_tokens
    ))
  )
}

#' Sample from a Dataset Deterministically
#'
#' @description
#' Sample rows from a dataset with optional seed for reproducibility.
#' Used by optimizers for consistent train/validation splits and
#' demo selection.
#'
#' @param dataset A data frame to sample from.
#' @param n Number of rows to sample. If NULL or greater than nrow(dataset),
#'   returns the full dataset.
#' @param seed Random seed for reproducibility. If NULL, sampling is random.
#' @param replace Whether to sample with replacement. Default is FALSE.
#'
#' @return A data frame containing the sampled rows.
#' @export
#'
#' @examples
#' df <- tibble::tibble(x = 1:10, y = letters[1:10])
#'
#' # Deterministic sampling
#' sample1 <- sample_dataset(df, n = 5, seed = 42)
#' sample2 <- sample_dataset(df, n = 5, seed = 42)
#' identical(sample1, sample2)  # TRUE
#'
#' # Random sampling (different each time)
#' sample3 <- sample_dataset(df, n = 5)
sample_dataset <- function(dataset, n = NULL, seed = NULL, replace = FALSE) {
  if (!is.data.frame(dataset)) {
    cli::cli_abort("{.arg dataset} must be a data frame")
  }

  total_rows <- nrow(dataset)

  # Handle edge cases
  if (total_rows == 0) {
    return(dataset)
  }

  if (is.null(n) || n >= total_rows) {
    if (!replace) {
      return(dataset)
    }
  }

  # Sample with proper RNG state management
  if (!is.null(seed)) {
    # Save current RNG state
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }

    set.seed(seed)

    # Restore RNG state on exit
    on.exit(
      {
        if (is.null(old_seed)) {
          rm(".Random.seed", envir = globalenv())
        } else {
          assign(".Random.seed", old_seed, envir = globalenv())
        }
      },
      add = TRUE
    )
  }

  # Sample indices
  indices <- sample(total_rows, size = min(n, total_rows), replace = replace)

  dataset[indices, , drop = FALSE]
}

#' Split Dataset into Train and Validation Sets
#'
#' @description
#' Split a dataset into training and validation portions with optional
#' seed for reproducibility.
#'
#' @param dataset A data frame to split.
#' @param prop Proportion of data for training. Default is 0.8.
#' @param seed Random seed for reproducibility.
#'
#' @return A list with `train` and `val` data frames.
#' @export
#'
#' @examples
#' df <- tibble::tibble(x = 1:100)
#' split <- split_dataset(df, prop = 0.8, seed = 42)
#' nrow(split$train)  # ~80
#' nrow(split$val)    # ~20
split_dataset <- function(dataset, prop = 0.8, seed = NULL) {
  if (!is.data.frame(dataset)) {
    cli::cli_abort("{.arg dataset} must be a data frame")
  }

  if (prop <= 0 || prop >= 1) {
    cli::cli_abort("{.arg prop} must be between 0 and 1 (exclusive)")
  }

  n <- nrow(dataset)

  if (n == 0) {
    return(list(train = dataset, val = dataset))
  }

  # Split with proper RNG state management
  if (!is.null(seed)) {
    # Save current RNG state
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }

    set.seed(seed)

    # Restore RNG state on exit
    on.exit(
      {
        if (is.null(old_seed)) {
          rm(".Random.seed", envir = globalenv())
        } else {
          assign(".Random.seed", old_seed, envir = globalenv())
        }
      },
      add = TRUE
    )
  }

  n_train <- floor(n * prop)
  train_indices <- sample(n, n_train)
  val_indices <- setdiff(seq_len(n), train_indices)

  list(
    train = dataset[train_indices, , drop = FALSE],
    val = dataset[val_indices, , drop = FALSE]
  )
}

#' Extract Cost Information from a Module
#'
#' @description
#' Extract token usage and cost information from a module's traces.
#' Used internally by optimizers for cost tracking.
#'
#' @param module A DSPrrr module with recorded traces.
#'
#' @return A list with:
#'   - `tokens_in`: total input tokens
#'   - `tokens_out`: total output tokens
#'   - `total_tokens`: total tokens (in + out)
#'   - `total_cost`: total cost in USD (may be NA if not available)
#'
#' @noRd
extract_cost_from_module <- function(module) {
  if (!inherits(module, "Module")) {
    return(list(
      tokens_in = 0L,
      tokens_out = 0L,
      total_tokens = 0L,
      total_cost = NA_real_
    ))
  }

  summary <- module$trace_summary()

  list(
    tokens_in = as.integer(summary$total_input_tokens %||% 0),
    tokens_out = as.integer(summary$total_output_tokens %||% 0),
    total_tokens = as.integer(summary$total_tokens %||% 0),
    total_cost = summary$total_cost %||% NA_real_
  )
}

#' Cost Summary
#'
#' @description
#' S7 class for tracking cumulative costs across optimizer trials.
#'
#' @noRd
CostSummary <- S7::new_class(
  "CostSummary",
  properties = list(
    tokens_in = S7::new_property(S7::class_integer, default = 0L),
    tokens_out = S7::new_property(S7::class_integer, default = 0L),
    total_tokens = S7::new_property(S7::class_integer, default = 0L),
    total_cost = S7::new_property(S7::class_numeric, default = 0),
    n_calls = S7::new_property(S7::class_integer, default = 0L)
  )
)

#' Update Cost Summary
#'
#' @description
#' Add cost information from a module to a cumulative CostSummary.
#'
#' @param summary A CostSummary object.
#' @param module A Module with traces, or a cost list from extract_cost_from_module.
#'
#' @return Updated CostSummary object.
#' @noRd
update_cost_summary <- function(summary, module) {
  if (inherits(module, "Module")) {
    cost <- extract_cost_from_module(module)
  } else if (is.list(module)) {
    cost <- module
  } else {
    return(summary)
  }

  # Helper to safely get numeric counters, treating NULL and NA as 0
  safe_num <- function(x, default = 0) {
    if (is.null(x) || length(x) == 0 || is.na(x)) default else x
  }

  cost_value <- cost$total_cost
  next_total_cost <- if (
    is.null(cost_value) ||
      length(cost_value) == 0 ||
      is.na(cost_value) ||
      is.na(summary@total_cost)
  ) {
    NA_real_
  } else {
    summary@total_cost + cost_value
  }

  CostSummary(
    tokens_in = summary@tokens_in + as.integer(safe_num(cost$tokens_in, 0L)),
    tokens_out = summary@tokens_out + as.integer(safe_num(cost$tokens_out, 0L)),
    total_tokens = summary@total_tokens +
      as.integer(safe_num(cost$total_tokens, 0L)),
    total_cost = next_total_cost,
    n_calls = summary@n_calls + 1L
  )
}

# Resource limits are intentionally independent from execution concurrency.
# Optimizers schedule bounded work units and record the effective in-flight
# count reported by the shared concurrency contract.
optimizer_budget_limit_map <- function() {
  c(
    max_trials = "trials",
    max_metric_calls = "metric_calls",
    max_provider_calls = "provider_calls",
    max_input_tokens = "input_tokens",
    max_output_tokens = "output_tokens",
    max_total_tokens = "total_tokens",
    max_cost = "known_cost",
    max_elapsed_seconds = "elapsed_seconds"
  )
}

optimizer_control_limits <- function(control) {
  list(
    max_trials = control@max_trials,
    max_metric_calls = control@max_metric_calls,
    max_provider_calls = control@max_provider_calls,
    max_input_tokens = control@max_input_tokens,
    max_output_tokens = control@max_output_tokens,
    max_total_tokens = control@max_total_tokens,
    max_cost = control@max_cost,
    max_elapsed_seconds = control@max_elapsed_seconds
  )
}

optimizer_monotonic_clock <- function() {
  unname(proc.time()[["elapsed"]])
}

optimizer_budget_counter_names <- function() {
  c(
    "trials",
    "metric_calls",
    "provider_calls",
    "input_tokens",
    "output_tokens",
    "total_tokens",
    "known_cost"
  )
}

# Create mutable state for one optimizer-wide monotonic resource ledger.
new_optimizer_budget <- function(control = NULL, state = NULL, clock = NULL) {
  if (is.null(control)) {
    control <- optimizer_control()
  }
  if (!inherits(control, "dsprrr::OptimizerControl")) {
    cli::cli_abort(
      "{.arg control} must be created by {.fn optimizer_control}",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (is.null(clock)) {
    clock <- getOption("dsprrr.optimizer_clock", optimizer_monotonic_clock)
  }
  if (!is.function(clock)) {
    cli::cli_abort(
      "The optimizer monotonic clock must be a function",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  budget <- new.env(parent = emptyenv())
  budget$max_errors <- as.integer(control@max_errors)
  budget$limits <- optimizer_control_limits(control)
  budget$attempts <- 0L
  budget$successes <- 0L
  budget$total_errors <- 0L
  budget$consecutive_errors <- 0L
  for (name in optimizer_budget_counter_names()) {
    budget[[name]] <- if (name == "known_cost") 0 else 0L
  }
  budget$unknown_metric_calls <- 0L
  budget$unknown_provider_calls <- 0L
  budget$unknown_input_tokens <- 0L
  budget$unknown_output_tokens <- 0L
  budget$unknown_total_tokens <- 0L
  budget$unknown_cost_calls <- 0L
  budget$trial_units <- character()
  budget$completed_units <- character()
  budget$overshoots <- list()
  budget$stop_reason <- NULL
  budget$clock <- clock
  budget$started_tick <- as.numeric(clock())
  budget$elapsed_offset <- 0
  budget$last_elapsed <- 0
  class(budget) <- c("dsprrr_optimizer_budget", "environment")

  if (!is.null(state)) {
    optimizer_budget_restore_state(budget, state)
    optimizer_budget_reconcile_current_limits(budget)
  }

  budget
}

optimizer_budget_elapsed <- function(budget) {
  if (!inherits(budget, "dsprrr_optimizer_budget")) {
    cli::cli_abort(
      "{.arg budget} must be an optimizer budget",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  tick <- as.numeric(budget$clock())
  delta <- max(0, tick - budget$started_tick)
  elapsed <- max(budget$last_elapsed, budget$elapsed_offset + delta)
  budget$last_elapsed <- elapsed
  elapsed
}

optimizer_budget_reason <- function(
  code,
  stage,
  resource,
  limit,
  observed,
  unit_id = NULL,
  overshoot = 0,
  work_unit = "optimizer_work_unit",
  max_started = 1L,
  message = NULL
) {
  if (is.null(message)) {
    message <- sprintf("Reached %s limit (%s)", code, format(limit))
  }
  structure(
    list(
      code = code,
      stage = as.character(stage)[1L],
      resource = resource,
      limit = limit,
      observed = observed,
      unit_id = unit_id,
      overshoot = list(
        amount = overshoot,
        unit = work_unit,
        max_started = as.integer(max_started)
      ),
      message = message
    ),
    class = c("dsprrr_optimizer_stop_reason", "list")
  )
}

optimizer_budget_set_stop <- function(budget, reason) {
  if (is.null(budget$stop_reason)) {
    budget$stop_reason <- reason
  }
  invisible(budget)
}

optimizer_budget_validate_stage <- function(stage) {
  if (
    !is.character(stage) ||
      length(stage) != 1L ||
      is.na(stage) ||
      !nzchar(stage)
  ) {
    cli::cli_abort(
      "{.arg stage} must be a single non-empty string",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  stage
}

optimizer_budget_increment_abort <- function(
  resource,
  stage,
  current,
  increment
) {
  cli::cli_abort(
    c(
      "Optimizer budget counter {.field {resource}} cannot be incremented",
      "x" = "The increment would exceed its representable range.",
      "i" = "The budget ledger was not changed."
    ),
    class = c(
      "dsprrr_optimizer_budget_overflow",
      "dsprrr_optimizer_invariant_error"
    ),
    resource = resource,
    stage = stage,
    current = current,
    increment = increment
  )
}

optimizer_budget_checked_increment <- function(
  current,
  increment,
  resource,
  stage,
  integer = TRUE
) {
  if (
    !is.numeric(increment) ||
      length(increment) != 1L ||
      is.na(increment) ||
      !is.finite(increment) ||
      increment < 0 ||
      (integer &&
        (increment != floor(increment) ||
          increment > .Machine$integer.max))
  ) {
    cli::cli_abort(
      "Optimizer budget increment for {.field {resource}} is invalid",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  if (integer) {
    increment <- as.integer(increment)
    if (
      !is.integer(current) ||
        length(current) != 1L ||
        is.na(current) ||
        current < 0L
    ) {
      cli::cli_abort(
        "Optimizer budget counter {.field {resource}} is invalid",
        class = "dsprrr_optimizer_invariant_error"
      )
    }
    if (increment > .Machine$integer.max - current) {
      optimizer_budget_increment_abort(
        resource,
        stage,
        current,
        increment
      )
    }
    return(current + increment)
  }

  if (
    !is.numeric(current) ||
      length(current) != 1L ||
      is.na(current) ||
      !is.finite(current) ||
      current < 0
  ) {
    cli::cli_abort(
      "Optimizer budget amount {.field {resource}} is invalid",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (increment > .Machine$double.xmax - current) {
    optimizer_budget_increment_abort(resource, stage, current, increment)
  }
  current + increment
}

optimizer_budget_require_outcome_capacity <- function(
  budget,
  successes,
  total_errors,
  stage
) {
  attempts <- optimizer_budget_checked_increment(
    budget$attempts,
    successes,
    "attempts",
    stage
  )
  optimizer_budget_checked_increment(
    attempts,
    total_errors,
    "attempts",
    stage
  )
  optimizer_budget_checked_increment(
    budget$successes,
    successes,
    "successes",
    stage
  )
  optimizer_budget_checked_increment(
    budget$total_errors,
    total_errors,
    "total_errors",
    stage
  )
  streak <- if (successes > 0) 0L else budget$consecutive_errors
  optimizer_budget_checked_increment(
    streak,
    total_errors,
    "consecutive_errors",
    stage
  )
  invisible(budget)
}

optimizer_budget_limit_reason <- function(
  budget,
  limit_name,
  stage,
  observed,
  unit_id,
  overshoot = 0,
  work_unit = "optimizer_work_unit",
  max_started = 1L
) {
  resource <- optimizer_budget_limit_map()[[limit_name]]
  optimizer_budget_reason(
    code = limit_name,
    stage = stage,
    resource = resource,
    limit = budget$limits[[limit_name]],
    observed = observed,
    unit_id = unit_id,
    overshoot = overshoot,
    work_unit = work_unit,
    max_started = max_started
  )
}

# Check every known counter before scheduling an expensive work unit. Planned
# values are conservative lower bounds; postflight accounting may cross a cap
# only within the explicitly named, already-started work-unit bound.
optimizer_budget_preflight <- function(
  budget,
  stage,
  planned = list(),
  unit_id = NULL,
  work_unit = "optimizer_work_unit",
  max_started = 1L,
  planned_outcomes = 1L
) {
  optimizer_budget_validate_stage(stage)
  if (!inherits(budget, "dsprrr_optimizer_budget")) {
    cli::cli_abort(
      "{.arg budget} must be an optimizer budget",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (optimizer_budget_stopped(budget)) {
    return(FALSE)
  }

  optimizer_budget_require_outcome_capacity(
    budget,
    successes = planned_outcomes,
    total_errors = 0L,
    stage = stage
  )

  allowed <- optimizer_budget_counter_names()
  if (length(setdiff(names(planned), allowed)) > 0L) {
    cli::cli_abort(
      "{.arg planned} contains unknown optimizer resources",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  limit_map <- optimizer_budget_limit_map()
  projected <- list()
  amounts <- list()
  for (limit_name in setdiff(names(limit_map), "max_elapsed_seconds")) {
    resource <- limit_map[[limit_name]]
    amount <- planned[[resource]] %||% 0
    if (
      !is.numeric(amount) ||
        length(amount) != 1L ||
        is.na(amount) ||
        !is.finite(amount) ||
        amount < 0 ||
        (resource != "known_cost" && amount != floor(amount))
    ) {
      cli::cli_abort(
        "Planned optimizer resource {.field {resource}} is invalid",
        class = "dsprrr_optimizer_invariant_error"
      )
    }
    observed <- budget[[resource]]
    amounts[[limit_name]] <- amount
    projected[[limit_name]] <- optimizer_budget_checked_increment(
      observed,
      amount,
      resource,
      stage,
      integer = resource != "known_cost"
    )
  }

  elapsed <- optimizer_budget_elapsed(budget)
  elapsed_limit <- budget$limits$max_elapsed_seconds
  if (!is.null(elapsed_limit) && elapsed >= elapsed_limit) {
    optimizer_budget_set_stop(
      budget,
      optimizer_budget_limit_reason(
        budget,
        "max_elapsed_seconds",
        stage,
        observed = elapsed,
        unit_id = unit_id,
        work_unit = work_unit,
        max_started = 0L
      )
    )
    return(FALSE)
  }

  for (limit_name in names(projected)) {
    limit <- budget$limits[[limit_name]]
    resource <- limit_map[[limit_name]]
    observed <- budget[[resource]]
    if (
      !is.null(limit) &&
        amounts[[limit_name]] > 0 &&
        projected[[limit_name]] > limit
    ) {
      optimizer_budget_set_stop(
        budget,
        optimizer_budget_limit_reason(
          budget,
          limit_name,
          stage,
          observed = observed,
          unit_id = unit_id,
          work_unit = work_unit,
          max_started = 0L
        )
      )
      return(FALSE)
    }
  }

  TRUE
}

optimizer_budget_note_unknown <- function(
  budget,
  resource,
  stage,
  unit_id,
  work_unit,
  max_started
) {
  limit_name <- names(optimizer_budget_limit_map())[
    optimizer_budget_limit_map() == sub("_calls$", "", resource)
  ]
  if (resource == "cost_calls") {
    limit_name <- "max_cost"
  } else if (resource == "metric_calls") {
    limit_name <- "max_metric_calls"
  } else if (resource == "provider_calls") {
    limit_name <- "max_provider_calls"
  } else if (resource == "input_tokens") {
    limit_name <- "max_input_tokens"
  } else if (resource == "output_tokens") {
    limit_name <- "max_output_tokens"
  } else if (resource == "total_tokens") {
    limit_name <- "max_total_tokens"
  }
  limit_name <- limit_name[[1L]] %||% NULL
  if (!is.null(limit_name) && !is.null(budget$limits[[limit_name]])) {
    code <- paste0("unknown_", sub("_calls$", "", resource))
    if (resource == "cost_calls") {
      code <- "unknown_cost"
    }
    optimizer_budget_set_stop(
      budget,
      optimizer_budget_reason(
        code = code,
        stage = stage,
        resource = resource,
        limit = budget$limits[[limit_name]],
        observed = NA_real_,
        unit_id = unit_id,
        overshoot = NA_real_,
        work_unit = work_unit,
        max_started = max_started,
        message = sprintf(
          "Cannot enforce %s because the completed work unit reported unknown usage",
          limit_name
        )
      )
    )
  }
  invisible(budget)
}

# Record verified postflight usage. NA is a first-class unknown value and is
# never silently converted to zero.
record_optimizer_usage <- function(
  budget,
  usage,
  stage,
  unit_id = NULL,
  work_unit = "optimizer_work_unit",
  max_started = 1L
) {
  optimizer_budget_validate_stage(stage)
  if (!inherits(budget, "dsprrr_optimizer_budget") || !is.list(usage)) {
    cli::cli_abort(
      "Optimizer usage requires a budget and a named list",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  updates <- list()
  unknown_resources <- character()
  for (resource in optimizer_budget_counter_names()) {
    if (!resource %in% names(usage)) {
      next
    }
    value <- usage[[resource]]
    if (is.null(value) || length(value) != 1L || is.na(value)) {
      if (resource == "trials") {
        cli::cli_abort(
          "Observed optimizer resource {.field trials} cannot be unknown",
          class = "dsprrr_optimizer_invariant_error"
        )
      }
      unknown_name <- if (resource == "known_cost") {
        "cost_calls"
      } else {
        resource
      }
      counter <- paste0("unknown_", unknown_name)
      updates[[counter]] <- optimizer_budget_checked_increment(
        budget[[counter]],
        1L,
        counter,
        stage,
        integer = TRUE
      )
      unknown_resources <- c(unknown_resources, unknown_name)
      next
    }
    if (
      !is.numeric(value) ||
        !is.finite(value) ||
        value < 0 ||
        (resource != "known_cost" && value != floor(value))
    ) {
      cli::cli_abort(
        "Observed optimizer resource {.field {resource}} is invalid",
        class = "dsprrr_optimizer_invariant_error"
      )
    }
    updates[[resource]] <- optimizer_budget_checked_increment(
      budget[[resource]],
      value,
      resource,
      stage,
      integer = resource != "known_cost"
    )
  }

  for (name in names(updates)) {
    budget[[name]] <- updates[[name]]
  }
  for (resource in unknown_resources) {
    optimizer_budget_note_unknown(
      budget,
      resource,
      stage,
      unit_id,
      work_unit,
      max_started
    )
  }

  elapsed <- optimizer_budget_elapsed(budget)
  limit_map <- optimizer_budget_limit_map()
  for (limit_name in names(limit_map)) {
    resource <- limit_map[[limit_name]]
    observed <- if (resource == "elapsed_seconds") {
      elapsed
    } else {
      budget[[resource]]
    }
    limit <- budget$limits[[limit_name]]
    if (!is.null(limit) && observed >= limit) {
      overshoot <- max(0, observed - limit)
      if (overshoot > 0) {
        budget$overshoots[[length(budget$overshoots) + 1L]] <- list(
          resource = resource,
          amount = overshoot,
          unit = work_unit,
          max_started = as.integer(max_started),
          unit_id = unit_id
        )
      }
      optimizer_budget_set_stop(
        budget,
        optimizer_budget_limit_reason(
          budget,
          limit_name,
          stage,
          observed = observed,
          unit_id = unit_id,
          overshoot = overshoot,
          work_unit = work_unit,
          max_started = max_started
        )
      )
    }
  }

  invisible(budget)
}

optimizer_budget_complete_unit <- function(budget, unit_id) {
  if (
    !is.character(unit_id) ||
      length(unit_id) != 1L ||
      is.na(unit_id) ||
      !nzchar(unit_id)
  ) {
    cli::cli_abort(
      "{.arg unit_id} must be one non-empty string",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (!unit_id %in% budget$completed_units) {
    budget$completed_units <- c(budget$completed_units, unit_id)
  }
  invisible(budget)
}

optimizer_budget_count_trial <- function(budget, stage, unit_id) {
  if (!unit_id %in% budget$trial_units) {
    record_optimizer_usage(
      budget,
      list(trials = 1L),
      stage,
      unit_id = unit_id,
      work_unit = "optimizer_trial",
      max_started = 1L
    )
    budget$trial_units <- c(budget$trial_units, unit_id)
  }
  invisible(budget)
}

optimizer_budget_unit_completed <- function(budget, unit_id) {
  unit_id %in% budget$completed_units
}

optimizer_budget_state <- function(budget) {
  elapsed <- optimizer_budget_elapsed(budget)
  list(
    attempts = as.integer(budget$attempts),
    successes = as.integer(budget$successes),
    total_errors = as.integer(budget$total_errors),
    consecutive_errors = as.integer(budget$consecutive_errors),
    trials = as.integer(budget$trials),
    metric_calls = as.integer(budget$metric_calls),
    provider_calls = as.integer(budget$provider_calls),
    input_tokens = as.integer(budget$input_tokens),
    output_tokens = as.integer(budget$output_tokens),
    total_tokens = as.integer(budget$total_tokens),
    known_cost = as.numeric(budget$known_cost),
    unknown_metric_calls = as.integer(budget$unknown_metric_calls),
    unknown_provider_calls = as.integer(budget$unknown_provider_calls),
    unknown_input_tokens = as.integer(budget$unknown_input_tokens),
    unknown_output_tokens = as.integer(budget$unknown_output_tokens),
    unknown_total_tokens = as.integer(budget$unknown_total_tokens),
    unknown_cost_calls = as.integer(budget$unknown_cost_calls),
    elapsed_seconds = as.numeric(elapsed),
    trial_units = budget$trial_units,
    completed_units = budget$completed_units,
    overshoots = budget$overshoots,
    stop_reason = if (is.null(budget$stop_reason)) {
      NULL
    } else {
      unclass(budget$stop_reason)
    }
  )
}

optimizer_budget_checkpoint_abort <- function(message) {
  cli::cli_abort(
    message,
    class = "dsprrr_optimizer_checkpoint_malformed"
  )
}

optimizer_budget_checkpoint_text <- function(value) {
  is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    nzchar(value)
}

optimizer_budget_checkpoint_unit_id <- function(value) {
  is.null(value) || optimizer_budget_checkpoint_text(value)
}

optimizer_budget_has_exact_outcome_partition <- function(
  attempts,
  successes,
  total_errors
) {
  # Subtraction is safe after non-negative integer validation; adding the two
  # partition counters could overflow before an inconsistent state is rejected.
  successes <= attempts &&
    total_errors <= attempts &&
    identical(attempts - successes, total_errors)
}

optimizer_budget_validate_overshoot <- function(record, nested = FALSE) {
  expected <- if (nested) {
    c("amount", "unit", "max_started")
  } else {
    c("resource", "amount", "unit", "max_started", "unit_id")
  }
  if (
    !artifact_is_plain_list(record) ||
      !identical(names(record), expected)
  ) {
    return(FALSE)
  }
  amount <- record$amount
  if (
    !is.numeric(amount) ||
      length(amount) != 1L ||
      (!is.na(amount) && (!is.finite(amount) || amount < 0)) ||
      !optimizer_budget_checkpoint_text(record$unit) ||
      !is.integer(record$max_started) ||
      length(record$max_started) != 1L ||
      is.na(record$max_started) ||
      record$max_started < 0L
  ) {
    return(FALSE)
  }
  if (nested) {
    return(TRUE)
  }
  optimizer_budget_checkpoint_text(record$resource) &&
    record$resource %in% unname(optimizer_budget_limit_map()) &&
    !is.na(amount) &&
    amount > 0 &&
    optimizer_budget_checkpoint_unit_id(record$unit_id)
}

optimizer_budget_validate_stop_reason <- function(reason) {
  if (is.null(reason)) {
    return(TRUE)
  }
  if (!artifact_is_plain_list(reason)) {
    return(FALSE)
  }
  if (identical(reason$code, "max_errors")) {
    expected <- c(
      "code",
      "stage",
      "limit",
      "observed",
      "total_errors",
      "attempts",
      "message"
    )
    if ("condition_class" %in% names(reason)) {
      expected <- c(expected, "condition_class")
    }
    counts <- reason[c("limit", "observed", "total_errors", "attempts")]
    return(
      identical(names(reason), expected) &&
        optimizer_budget_checkpoint_text(reason$stage) &&
        optimizer_budget_checkpoint_text(reason$message) &&
        all(vapply(
          counts,
          function(value) {
            is.integer(value) &&
              length(value) == 1L &&
              !is.na(value) &&
              value >= 0L
          },
          logical(1)
        )) &&
        reason$total_errors <= reason$attempts &&
        reason$observed <= reason$total_errors &&
        reason$observed >= max(1L, reason$limit) &&
        (!"condition_class" %in% names(reason) ||
          optimizer_budget_checkpoint_text(reason$condition_class))
    )
  }

  expected <- c(
    "code",
    "stage",
    "resource",
    "limit",
    "observed",
    "unit_id",
    "overshoot",
    "message"
  )
  if (!identical(names(reason), expected)) {
    return(FALSE)
  }
  unknown_resources <- c(
    unknown_cost = "cost_calls",
    unknown_provider = "provider_calls",
    unknown_metric = "metric_calls",
    unknown_input_tokens = "input_tokens",
    unknown_output_tokens = "output_tokens",
    unknown_total_tokens = "total_tokens"
  )
  limit_resources <- optimizer_budget_limit_map()
  allowed_codes <- c(names(limit_resources), names(unknown_resources))
  if (
    !optimizer_budget_checkpoint_text(reason$code) ||
      !reason$code %in% allowed_codes ||
      !optimizer_budget_checkpoint_text(reason$stage) ||
      !optimizer_budget_checkpoint_text(reason$resource) ||
      !is.numeric(reason$limit) ||
      length(reason$limit) != 1L ||
      is.na(reason$limit) ||
      !is.finite(reason$limit) ||
      reason$limit < 0 ||
      !is.numeric(reason$observed) ||
      length(reason$observed) != 1L ||
      !optimizer_budget_checkpoint_unit_id(reason$unit_id) ||
      !optimizer_budget_validate_overshoot(reason$overshoot, nested = TRUE) ||
      !optimizer_budget_checkpoint_text(reason$message)
  ) {
    return(FALSE)
  }
  expected_resource <- if (reason$code %in% names(limit_resources)) {
    unname(limit_resources[[reason$code]])
  } else {
    unname(unknown_resources[[reason$code]])
  }
  is_unknown <- reason$code %in% names(unknown_resources)
  identical(reason$resource, expected_resource) &&
    if (is_unknown) {
      is.na(reason$observed) && is.na(reason$overshoot$amount)
    } else {
      !is.na(reason$observed) &&
        is.finite(reason$observed) &&
        reason$observed >= 0 &&
        !is.na(reason$overshoot$amount)
    }
}

optimizer_budget_restore_state <- function(budget, state) {
  count_names <- c(
    "attempts",
    "successes",
    "total_errors",
    "consecutive_errors",
    setdiff(optimizer_budget_counter_names(), "known_cost"),
    "unknown_metric_calls",
    "unknown_provider_calls",
    "unknown_input_tokens",
    "unknown_output_tokens",
    "unknown_total_tokens",
    "unknown_cost_calls"
  )
  expected_names <- c(
    "attempts",
    "successes",
    "total_errors",
    "consecutive_errors",
    optimizer_budget_counter_names(),
    "unknown_metric_calls",
    "unknown_provider_calls",
    "unknown_input_tokens",
    "unknown_output_tokens",
    "unknown_total_tokens",
    "unknown_cost_calls",
    "elapsed_seconds",
    "trial_units",
    "completed_units",
    "overshoots",
    "stop_reason"
  )
  if (
    !artifact_is_plain_list(state) ||
      !identical(names(state), expected_names)
  ) {
    optimizer_budget_checkpoint_abort(
      "Optimizer budget checkpoint has unknown, missing, or reordered fields"
    )
  }
  for (name in count_names) {
    value <- state[[name]]
    if (
      !is.integer(value) ||
        length(value) != 1L ||
        is.na(value) ||
        value < 0L
    ) {
      optimizer_budget_checkpoint_abort(
        paste0("Optimizer budget checkpoint count ", name, " is invalid")
      )
    }
  }
  for (name in c("known_cost", "elapsed_seconds")) {
    value <- state[[name]]
    if (
      !is.double(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !is.finite(value) ||
        value < 0
    ) {
      optimizer_budget_checkpoint_abort(
        paste0("Optimizer budget checkpoint amount ", name, " is invalid")
      )
    }
  }
  if (
    !optimizer_budget_has_exact_outcome_partition(
      state$attempts,
      state$successes,
      state$total_errors
    ) ||
      state$consecutive_errors > state$total_errors
  ) {
    optimizer_budget_checkpoint_abort(
      "Optimizer budget outcome counters are inconsistent"
    )
  }
  valid_unit_ids <- function(value) {
    is.character(value) &&
      !anyNA(value) &&
      !any(!nzchar(value)) &&
      !anyDuplicated(value)
  }
  if (!valid_unit_ids(state$trial_units)) {
    optimizer_budget_checkpoint_abort(
      "Optimizer checkpoint trial unit IDs are invalid"
    )
  }
  if (!valid_unit_ids(state$completed_units)) {
    optimizer_budget_checkpoint_abort(
      "Optimizer checkpoint completed unit IDs are invalid"
    )
  }
  if (
    !artifact_is_plain_list(state$overshoots) ||
      !is.null(names(state$overshoots)) ||
      !all(vapply(
        state$overshoots,
        optimizer_budget_validate_overshoot,
        logical(1)
      ))
  ) {
    optimizer_budget_checkpoint_abort(
      "Optimizer checkpoint overshoot records are invalid"
    )
  }
  if (!optimizer_budget_validate_stop_reason(state$stop_reason)) {
    optimizer_budget_checkpoint_abort(
      "Optimizer checkpoint stop reason is invalid"
    )
  }
  if (!is.null(state$stop_reason) && state$stop_reason$code == "max_errors") {
    reason <- state$stop_reason
    if (
      reason$attempts > state$attempts ||
        reason$total_errors > state$total_errors ||
        state$total_errors - reason$total_errors >
          state$attempts - reason$attempts
    ) {
      optimizer_budget_checkpoint_abort(
        "Optimizer checkpoint stop reason counters are inconsistent"
      )
    }
  }
  for (name in c(count_names, "known_cost")) {
    budget[[name]] <- state[[name]]
  }
  budget$trial_units <- state$trial_units
  budget$completed_units <- state$completed_units
  budget$overshoots <- state$overshoots
  budget$elapsed_offset <- state$elapsed_seconds
  budget$last_elapsed <- state$elapsed_seconds
  budget$started_tick <- as.numeric(budget$clock())
  budget$stop_reason <- if (is.null(state$stop_reason)) {
    NULL
  } else {
    structure(
      state$stop_reason,
      class = c("dsprrr_optimizer_stop_reason", "list")
    )
  }
  invisible(budget)
}

optimizer_budget_clear_resumable_stop <- function(budget) {
  reason <- budget$stop_reason
  if (is.null(reason)) {
    return(invisible(budget))
  }
  code <- reason$code %||% ""
  clear <- FALSE
  if (identical(code, "max_errors")) {
    clear <- budget$consecutive_errors < max(1L, budget$max_errors)
  } else if (code %in% names(budget$limits)) {
    limit <- budget$limits[[code]]
    resource <- optimizer_budget_limit_map()[[code]]
    observed <- if (resource == "elapsed_seconds") {
      budget$elapsed_offset
    } else {
      budget[[resource]]
    }
    clear <- is.null(limit) || observed < limit
  } else {
    unknown_limits <- c(
      unknown_cost = "max_cost",
      unknown_provider = "max_provider_calls",
      unknown_metric = "max_metric_calls",
      unknown_input_tokens = "max_input_tokens",
      unknown_output_tokens = "max_output_tokens",
      unknown_total_tokens = "max_total_tokens"
    )
    limit_name <- unknown_limits[[code]] %||% NULL
    clear <- !is.null(limit_name) && is.null(budget$limits[[limit_name]])
  }
  if (clear) {
    budget$stop_reason <- NULL
  }
  invisible(budget)
}

# Reconcile restored counters against the *current* control before any new
# work can start. This permits raised/removed caps while failing closed for
# stricter caps and for unknown restored usage under a newly finite cap.
optimizer_budget_reconcile_current_limits <- function(
  budget,
  stage = "checkpoint_resume"
) {
  restored_stop <- budget$stop_reason
  budget$stop_reason <- NULL

  # A stop reached inside a completed batch remains sticky even when a later,
  # already-started success reset the final streak. Checkpoint state does not
  # retain the order of outcomes after the stop, so use the largest peak still
  # possible from the recorded counters and fail closed when a raised cap could
  # already have been reached.
  post_stop_errors <- if (
    !is.null(restored_stop) &&
      identical(restored_stop$code, "max_errors")
  ) {
    max(0, budget$total_errors - restored_stop$total_errors)
  } else {
    0
  }
  possible_error_peak <- if (
    !is.null(restored_stop) &&
      identical(restored_stop$code, "max_errors")
  ) {
    as.numeric(restored_stop$observed) + as.numeric(post_stop_errors)
  } else {
    0
  }
  if (
    !is.null(restored_stop) &&
      identical(restored_stop$code, "max_errors") &&
      budget$max_errors <= possible_error_peak
  ) {
    budget$stop_reason <- restored_stop
    return(invisible(budget))
  }

  if (budget$consecutive_errors >= max(1L, budget$max_errors)) {
    optimizer_budget_set_stop(
      budget,
      new_optimizer_stop_reason(budget, stage)
    )
    return(invisible(budget))
  }

  unknown_limits <- list(
    max_metric_calls = list(
      counter = "unknown_metric_calls",
      code = "unknown_metric",
      resource = "metric_calls"
    ),
    max_provider_calls = list(
      counter = "unknown_provider_calls",
      code = "unknown_provider",
      resource = "provider_calls"
    ),
    max_input_tokens = list(
      counter = "unknown_input_tokens",
      code = "unknown_input_tokens",
      resource = "input_tokens"
    ),
    max_output_tokens = list(
      counter = "unknown_output_tokens",
      code = "unknown_output_tokens",
      resource = "output_tokens"
    ),
    max_total_tokens = list(
      counter = "unknown_total_tokens",
      code = "unknown_total_tokens",
      resource = "total_tokens"
    ),
    max_cost = list(
      counter = "unknown_cost_calls",
      code = "unknown_cost",
      resource = "cost_calls"
    )
  )
  for (limit_name in names(unknown_limits)) {
    entry <- unknown_limits[[limit_name]]
    limit <- budget$limits[[limit_name]]
    if (!is.null(limit) && budget[[entry$counter]] > 0L) {
      optimizer_budget_set_stop(
        budget,
        optimizer_budget_reason(
          code = entry$code,
          stage = stage,
          resource = entry$resource,
          limit = limit,
          observed = NA_real_,
          overshoot = NA_real_,
          work_unit = "checkpoint_history",
          max_started = 0L,
          message = sprintf(
            "Cannot enforce %s because restored work reported unknown usage",
            limit_name
          )
        )
      )
      return(invisible(budget))
    }
  }

  elapsed <- optimizer_budget_elapsed(budget)
  limit_map <- optimizer_budget_limit_map()
  for (limit_name in names(limit_map)) {
    limit <- budget$limits[[limit_name]]
    if (is.null(limit)) {
      next
    }
    resource <- limit_map[[limit_name]]
    observed <- if (identical(resource, "elapsed_seconds")) {
      elapsed
    } else {
      budget[[resource]]
    }
    if (observed >= limit) {
      optimizer_budget_set_stop(
        budget,
        optimizer_budget_limit_reason(
          budget,
          limit_name,
          stage,
          observed = observed,
          unit_id = NULL,
          overshoot = max(0, observed - limit),
          work_unit = "checkpoint_history",
          max_started = 0L
        )
      )
      return(invisible(budget))
    }
  }

  invisible(budget)
}

new_optimizer_stop_reason <- function(budget, stage, condition = NULL) {
  limit <- budget$max_errors
  message <- if (limit == 0L) {
    "Stopped after the first error because max_errors is 0"
  } else {
    sprintf(
      "Reached max_errors limit (%d consecutive errors)",
      limit
    )
  }

  reason <- list(
    code = "max_errors",
    stage = as.character(stage)[1L],
    limit = limit,
    observed = budget$consecutive_errors,
    total_errors = budget$total_errors,
    attempts = budget$attempts,
    message = message
  )

  if (inherits(condition, "condition")) {
    reason$condition_class <- class(condition)[1L]
  }

  structure(
    reason,
    class = c("dsprrr_optimizer_stop_reason", "list")
  )
}

# Record one ordered optimizer outcome and update the sticky stop reason.
record_optimizer_outcome <- function(
  budget,
  success,
  stage,
  condition = NULL
) {
  if (!inherits(budget, "dsprrr_optimizer_budget")) {
    cli::cli_abort(
      "{.arg budget} must be an optimizer budget",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  if (!is.logical(success) || length(success) != 1L || is.na(success)) {
    cli::cli_abort(
      "{.arg success} must be a single non-missing logical value",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  if (
    !is.character(stage) ||
      length(stage) != 1L ||
      is.na(stage) ||
      !nzchar(stage)
  ) {
    cli::cli_abort(
      "{.arg stage} must be a single non-empty string",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  # A completed batch can cross the boundary before all returned rows are
  # recorded. Keep accounting those executed outcomes while preserving the
  # first stop reason; callers use that sticky reason to prevent new work.
  next_attempts <- optimizer_budget_checked_increment(
    budget$attempts,
    1L,
    "attempts",
    stage
  )

  if (isTRUE(success)) {
    next_successes <- optimizer_budget_checked_increment(
      budget$successes,
      1L,
      "successes",
      stage
    )
    budget$attempts <- next_attempts
    budget$successes <- next_successes
    budget$consecutive_errors <- 0L
    return(invisible(budget))
  }

  next_total_errors <- optimizer_budget_checked_increment(
    budget$total_errors,
    1L,
    "total_errors",
    stage
  )
  next_consecutive_errors <- optimizer_budget_checked_increment(
    budget$consecutive_errors,
    1L,
    "consecutive_errors",
    stage
  )
  budget$attempts <- next_attempts
  budget$total_errors <- next_total_errors
  budget$consecutive_errors <- next_consecutive_errors

  effective_limit <- max(1L, budget$max_errors)
  if (
    is.null(budget$stop_reason) &&
      budget$consecutive_errors >= effective_limit
  ) {
    budget$stop_reason <- new_optimizer_stop_reason(
      budget,
      stage = stage,
      condition = condition
    )
  }

  invisible(budget)
}

optimizer_error_present <- function(error) {
  if (is.null(error) || length(error) == 0L) {
    return(FALSE)
  }

  if (inherits(error, "condition")) {
    return(TRUE)
  }

  if (length(error) == 1L && is.atomic(error) && is.na(error)) {
    return(FALSE)
  }

  if (is.character(error)) {
    return(any(nzchar(trimws(error[!is.na(error)]))))
  }

  TRUE
}

optimizer_error_condition <- function(error) {
  if (inherits(error, "condition")) {
    return(error)
  }

  simpleError(paste(as.character(error), collapse = ", "))
}

# Record the ordered row outcomes represented by an EvalResult.
record_eval_result_outcomes <- function(budget, eval_result, stage) {
  if (!inherits(eval_result, "dsprrr::EvalResult")) {
    cli::cli_abort(
      "{.arg eval_result} must be an EvalResult",
      class = "dsprrr_optimizer_invariant_error"
    )
  }

  examples <- eval_result@examples
  reported_successes <- as.integer(eval_result@n_evaluated)
  if (length(reported_successes) != 1L || is.na(reported_successes)) {
    reported_successes <- 0L
  }
  reported_successes <- max(0L, reported_successes)

  reported_errors <- as.integer(eval_result@n_errors)
  if (length(reported_errors) != 1L || is.na(reported_errors)) {
    reported_errors <- 0L
  }
  reported_errors <- max(0L, reported_errors)

  has_ordered_errors <- is.data.frame(examples) &&
    nrow(examples) > 0L &&
    "error" %in% names(examples)

  if (has_ordered_errors) {
    row_errors <- examples$error
    error_flags <- vapply(row_errors, optimizer_error_present, logical(1))
    if ("score" %in% names(examples)) {
      error_flags <- error_flags | is.na(examples$score)
    }

    # Reconcile summaries inside the known row set. Never synthesize extra
    # attempts: if detail omitted some failures, promote trailing clean rows.
    missing_failures <- max(0L, reported_errors - sum(error_flags))
    if (missing_failures > 0L) {
      clean_rows <- which(!error_flags)
      promote_count <- min(missing_failures, length(clean_rows))
      if (promote_count > 0L) {
        promoted <- utils::tail(clean_rows, promote_count)
        error_flags[promoted] <- TRUE
      }
    }

    optimizer_budget_require_outcome_capacity(
      budget,
      successes = sum(!error_flags),
      total_errors = sum(error_flags),
      stage = stage
    )

    for (index in seq_along(error_flags)) {
      is_error <- error_flags[[index]]
      condition <- if (is_error) {
        if (optimizer_error_present(row_errors[[index]])) {
          optimizer_error_condition(row_errors[[index]])
        } else {
          simpleError("Evaluation row failed without error detail")
        }
      } else {
        NULL
      }
      record_optimizer_outcome(
        budget,
        success = !is_error,
        stage = stage,
        condition = condition
      )
    }

    return(invisible(budget))
  }

  if (reported_successes == 0L && reported_errors == 0L) {
    optimizer_budget_require_outcome_capacity(
      budget,
      successes = 1L,
      total_errors = 0L,
      stage = stage
    )
    record_optimizer_outcome(budget, TRUE, stage)
    return(invisible(budget))
  }

  optimizer_budget_require_outcome_capacity(
    budget,
    successes = reported_successes,
    total_errors = reported_errors,
    stage = stage
  )

  # Without row detail, the actual ordering is unknowable. Record successes
  # first and group failures at the end so the reported failures form the
  # conservative terminal streak.
  if (reported_successes > 0L) {
    for (index in seq_len(reported_successes)) {
      record_optimizer_outcome(budget, TRUE, stage)
    }
  }

  for (index in seq_len(reported_errors)) {
    record_optimizer_outcome(budget, FALSE, stage)
  }

  invisible(budget)
}

optimizer_min_provider_calls <- function(program) {
  if (inherits(program, "FnModule")) {
    return(0L)
  }
  if (inherits(program, "FlexModule")) {
    return(flex_predictor_call_count(program))
  }
  if (inherits(program, "PipelineModule")) {
    calls <- vapply(
      program$steps,
      function(step) {
        optimizer_min_provider_calls(step@module)
      },
      integer(1)
    )
    return(sum(calls))
  }
  if (inherits(program, "MultiChainComparisonModule")) {
    return(as.integer(program$M + 1L))
  }
  if (inherits(program, "ReActModule")) {
    return(2L)
  }
  # All other current optimizer-supported Module implementations perform at
  # least one provider operation. Exact postflight counts still come only from
  # canonical metadata; this is a conservative preflight lower bound.
  1L
}

optimizer_eval_usage <- function(eval_result) {
  list(
    metric_calls = if (is.na(eval_result@metric_calls)) {
      NA_integer_
    } else {
      eval_result@metric_calls
    },
    provider_calls = if (isTRUE(eval_result@provider_usage_unknown)) {
      NA_integer_
    } else {
      eval_result@provider_calls
    },
    input_tokens = if (isTRUE(eval_result@token_usage_unknown)) {
      NA_integer_
    } else {
      eval_result@input_tokens
    },
    output_tokens = if (isTRUE(eval_result@token_usage_unknown)) {
      NA_integer_
    } else {
      eval_result@output_tokens
    },
    total_tokens = if (isTRUE(eval_result@token_usage_unknown)) {
      NA_integer_
    } else {
      eval_result@total_tokens
    },
    known_cost = eval_result@total_cost
  )
}

optimizer_forward_output <- function(result) {
  if (
    is.data.frame(result) &&
      "output" %in% names(result) &&
      nrow(result) > 0L
  ) {
    return(result$output[[1L]])
  }
  result
}

optimizer_forward_usage <- function(program, result) {
  metadata <- if (
    is.data.frame(result) &&
      "metadata" %in% names(result) &&
      nrow(result) > 0L
  ) {
    result$metadata[[1L]]
  } else {
    NULL
  }
  usage <- optimizer_metadata_usage(program, metadata)
  list(
    provider_calls = usage$provider_calls,
    input_tokens = usage$tokens_in,
    output_tokens = usage$tokens_out,
    total_tokens = usage$total_tokens,
    known_cost = usage$total_cost
  )
}

optimizer_unknown_provider_usage <- function() {
  list(
    provider_calls = NA_integer_,
    input_tokens = NA_integer_,
    output_tokens = NA_integer_,
    total_tokens = NA_integer_,
    known_cost = NA_real_
  )
}

# Run one direct optimizer-side Chat request as a bounded work unit. A direct
# request is one known provider call, but token/cost usage is only known when a
# verified Chat turn delta exposes it.
optimizer_budgeted_provider_call <- function(
  budget,
  model,
  stage,
  unit_id,
  call,
  success = function(value, condition) is.null(condition),
  work_unit = "optimizer_provider_call"
) {
  optimizer_budget_validate_stage(stage)
  if (!is.function(call) || !is.function(success)) {
    cli::cli_abort(
      "{.arg call} and {.arg success} must be functions",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (is.null(model)) {
    return(list(started = FALSE, value = NULL, condition = NULL))
  }
  model <- assert_ellmer_chat(model, arg = "model")
  if (
    !optimizer_budget_preflight(
      budget,
      stage = stage,
      planned = list(
        provider_calls = 1L,
        input_tokens = 1L,
        output_tokens = 1L,
        total_tokens = 1L
      ),
      unit_id = unit_id,
      work_unit = work_unit,
      max_started = 0L
    )
  ) {
    return(list(started = FALSE, value = NULL, condition = NULL))
  }

  turns_before <- batch_chat_turns(model)
  condition <- NULL
  value <- tryCatch(
    call(),
    error = function(e) {
      condition <<- e
      NULL
    }
  )
  metadata <- chat_usage_metadata(model, turns_before = turns_before)
  record_optimizer_usage(
    budget,
    list(
      provider_calls = 1L,
      input_tokens = metadata$input_tokens,
      output_tokens = metadata$output_tokens,
      total_tokens = metadata$total_tokens,
      known_cost = metadata$cost
    ),
    stage = stage,
    unit_id = unit_id,
    work_unit = work_unit,
    max_started = 1L
  )
  record_optimizer_outcome(
    budget,
    success = isTRUE(success(value, condition)),
    stage = stage,
    condition = condition
  )
  optimizer_budget_complete_unit(budget, unit_id)
  list(started = TRUE, value = value, condition = condition)
}

optimizer_eval_row_record <- function(eval_result, row_index) {
  example <- eval_result@examples[1L, , drop = FALSE]
  list(
    row_index = as.integer(row_index),
    score = as.numeric(example$score[[1L]] %||% NA_real_),
    error = as.character(example$error[[1L]] %||% NA_character_),
    predicted = if ("predicted" %in% names(example)) {
      example$predicted[[1L]]
    } else {
      NA
    },
    feedback = as.character(example$feedback[[1L]] %||% NA_character_),
    program_trace = if ("program_trace" %in% names(example)) {
      example$program_trace[[1L]]
    } else {
      NULL
    },
    input_tokens = eval_result@input_tokens,
    output_tokens = eval_result@output_tokens,
    total_tokens = eval_result@total_tokens,
    total_cost = eval_result@total_cost,
    provider_calls = eval_result@provider_calls,
    metric_calls = eval_result@metric_calls,
    provider_usage_unknown = eval_result@provider_usage_unknown,
    tokens_unknown = eval_result@token_usage_unknown,
    latency_ms = eval_result@total_latency_ms
  )
}

optimizer_eval_checkpoint_records <- function(records) {
  lapply(records, function(record) {
    record$predicted <- NULL
    # Raw trace events can contain prompts and model responses. They are useful
    # during live optimization, but never belong in resumable checkpoints.
    record$program_trace <- NULL
    record
  })
}

optimizer_eval_restore_records <- function(records) {
  if (is.null(records)) {
    return(list())
  }
  if (!is.list(records)) {
    cli::cli_abort(
      "Partial optimizer evaluation state must be a list",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  lapply(records, function(record) {
    if (!is.list(record) || is.null(record$row_index)) {
      cli::cli_abort(
        "Partial optimizer row state is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    record$predicted <- record$predicted %||% NA
    record$program_trace <- record$program_trace %||% NULL
    record
  })
}

optimizer_eval_known_sum <- function(records, field, integer = FALSE) {
  values <- vapply(
    records,
    function(record) {
      value <- record[[field]]
      if (is.null(value) || length(value) != 1L) NA_real_ else as.numeric(value)
    },
    numeric(1)
  )
  if (length(values) == 0L || anyNA(values)) {
    return(if (integer) NA_integer_ else NA_real_)
  }
  if (integer) as.integer(sum(values)) else sum(values)
}

optimizer_combine_eval_records <- function(records, dataset) {
  if (length(records) == 0L) {
    return(EvalResult(
      examples = tibble::tibble(),
      mean_score = NA_real_,
      n_evaluated = 0L,
      n_errors = 0L,
      trace_context = current_trace_context()
    ))
  }
  order_index <- order(vapply(records, `[[`, integer(1), "row_index"))
  records <- records[order_index]
  row_indices <- vapply(records, `[[`, integer(1), "row_index")
  scores <- vapply(records, function(record) record$score, numeric(1))
  errors <- vapply(
    records,
    function(record) {
      record$error %||% NA_character_
    },
    character(1)
  )
  feedback <- vapply(
    records,
    function(record) {
      record$feedback %||% NA_character_
    },
    character(1)
  )
  predictions <- lapply(records, `[[`, "predicted")
  traces <- lapply(records, `[[`, "program_trace")
  examples <- tibble::tibble(
    row_id = row_indices,
    score = scores,
    error = errors,
    predicted = predictions,
    feedback = feedback,
    program_trace = traces
  )
  input_names <- intersect(names(dataset), names(dataset))
  for (name in input_names) {
    examples[[paste0("input_", name)]] <- dataset[[name]][row_indices]
  }
  valid_scores <- scores[!is.na(scores)]
  std_error <- if (length(valid_scores) > 1L) {
    stats::sd(valid_scores) / sqrt(length(valid_scores))
  } else {
    NA_real_
  }
  provider_unknown <- any(vapply(
    records,
    function(record) {
      isTRUE(record$provider_usage_unknown)
    },
    logical(1)
  ))
  token_unknown <- any(vapply(
    records,
    function(record) {
      isTRUE(record$tokens_unknown)
    },
    logical(1)
  ))
  total_cost <- optimizer_eval_known_sum(records, "total_cost")
  provider_calls <- optimizer_eval_known_sum(
    records,
    "provider_calls",
    integer = TRUE
  )
  metric_calls <- optimizer_eval_known_sum(
    records,
    "metric_calls",
    integer = TRUE
  )

  EvalResult(
    examples = examples,
    mean_score = failure_adjusted_mean(scores),
    std_error = std_error,
    n_evaluated = as.integer(sum(!is.na(scores))),
    n_errors = as.integer(sum(is.na(scores))),
    input_tokens = optimizer_eval_known_sum(
      records,
      "input_tokens",
      integer = TRUE
    ),
    output_tokens = optimizer_eval_known_sum(
      records,
      "output_tokens",
      integer = TRUE
    ),
    total_tokens = optimizer_eval_known_sum(
      records,
      "total_tokens",
      integer = TRUE
    ),
    total_cost = total_cost,
    provider_calls = provider_calls,
    metric_calls = metric_calls,
    provider_usage_unknown = provider_unknown || is.na(provider_calls),
    token_usage_unknown = token_unknown,
    total_latency_ms = optimizer_eval_known_sum(records, "latency_ms"),
    trace_context = current_trace_context()
  )
}

optimizer_budget_requires_row_units <- function(budget) {
  limits <- budget$limits[c(
    "max_metric_calls",
    "max_provider_calls",
    "max_input_tokens",
    "max_output_tokens",
    "max_total_tokens",
    "max_cost",
    "max_elapsed_seconds"
  )]
  !all(vapply(limits, is.null, logical(1)))
}

# Evaluate one candidate with the whole-evaluation path when
# no fine-grained resource cap is active. Any metric/provider/token/cost/time cap
# switches to row units so its only postflight overshoot is one started row.
optimizer_eval_candidate <- function(
  program,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  budget = NULL,
  stage,
  unit_id,
  ...
) {
  control <- control %||% optimizer_control()
  budget <- budget %||% new_optimizer_budget(control)
  if (optimizer_budget_requires_row_units(budget)) {
    return(optimizer_eval_program(
      program,
      dataset,
      metric,
      .llm = .llm,
      control = control,
      budget = budget,
      stage = stage,
      unit_id = unit_id,
      ...
    ))
  }
  if (
    !optimizer_budget_preflight(
      budget,
      stage = stage,
      planned = list(trials = 1L),
      unit_id = unit_id,
      work_unit = "optimizer_trial",
      max_started = 0L,
      planned_outcomes = max(1L, nrow(dataset))
    )
  ) {
    return(EvalResult(trace_context = current_trace_context()))
  }
  result <- eval_program(
    program,
    dataset,
    metric,
    .llm = .llm,
    control = control,
    ...
  )
  record_optimizer_usage(
    budget,
    optimizer_eval_usage(result),
    stage = stage,
    unit_id = unit_id,
    work_unit = "evaluation_dataset",
    max_started = 1L
  )
  record_eval_result_outcomes(budget, result, stage)
  optimizer_budget_count_trial(budget, stage, unit_id)
  optimizer_budget_complete_unit(budget, unit_id)
  result
}

# Evaluate one optimizer candidate in row-sized work units. The maximum
# postflight overshoot is therefore the usage of one already-started evaluation
# row in the current sequential implementation. When the shared concurrency
# contract supplies `effective_workers`, callers may raise `max_started` to that
# exact value without changing ledger semantics.
optimizer_eval_program <- function(
  program,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  budget,
  stage,
  unit_id,
  partial_records = list(),
  on_progress = NULL,
  ...
) {
  if (is.null(control)) {
    control <- optimizer_control()
  }
  if (!is.data.frame(dataset)) {
    cli::cli_abort("{.arg dataset} must be a data frame")
  }
  optimizer_budget_validate_stage(stage)
  records <- optimizer_eval_restore_records(partial_records)
  completed_rows <- if (length(records) == 0L) {
    integer()
  } else {
    vapply(records, `[[`, integer(1), "row_index")
  }
  started_trial <- length(records) > 0L

  if (!started_trial) {
    can_start <- optimizer_budget_preflight(
      budget,
      stage,
      planned = list(trials = 1L),
      unit_id = unit_id,
      work_unit = "optimizer_trial",
      max_started = 0L
    )
    if (!can_start) {
      return(optimizer_combine_eval_records(records, dataset))
    }
  }

  min_provider_calls <- optimizer_min_provider_calls(program)
  planned <- list(metric_calls = 1L)
  if (min_provider_calls > 0L) {
    planned$provider_calls <- min_provider_calls
    planned$input_tokens <- 1L
    planned$output_tokens <- 1L
    planned$total_tokens <- 1L
  }

  for (row_index in seq_len(nrow(dataset))) {
    if (row_index %in% completed_rows) {
      next
    }
    row_unit_id <- paste0(unit_id, ":row:", row_index)
    if (
      !optimizer_budget_preflight(
        budget,
        stage,
        planned = planned,
        unit_id = row_unit_id,
        work_unit = "evaluation_row",
        max_started = 0L
      )
    ) {
      break
    }
    started_trial <- TRUE
    row_result <- eval_program(
      program,
      dataset[row_index, , drop = FALSE],
      metric,
      .llm = .llm,
      control = optimizer_control(
        seed = control@seed,
        max_errors = control@max_errors,
        num_threads = 1L,
        progress = FALSE,
        log_dir = control@log_dir
      ),
      .trace_row_ids = row_index,
      ...
    )
    record_optimizer_usage(
      budget,
      optimizer_eval_usage(row_result),
      stage,
      unit_id = row_unit_id,
      work_unit = "evaluation_row",
      max_started = 1L
    )
    record_eval_result_outcomes(budget, row_result, stage)
    records[[length(records) + 1L]] <- optimizer_eval_row_record(
      row_result,
      row_index
    )
    optimizer_budget_complete_unit(budget, row_unit_id)
    if (is.function(on_progress)) {
      on_progress(
        optimizer_eval_checkpoint_records(records),
        row_index,
        budget
      )
    }
    if (optimizer_budget_stopped(budget)) {
      break
    }
  }

  result <- optimizer_combine_eval_records(records, dataset)
  if (started_trial) {
    optimizer_budget_count_trial(budget, stage, unit_id)
  }
  if (length(records) == nrow(dataset) && nrow(dataset) > 0L) {
    optimizer_budget_complete_unit(budget, unit_id)
  }
  if (is.function(on_progress)) {
    on_progress(
      optimizer_eval_checkpoint_records(records),
      if (length(records) == 0L) {
        0L
      } else {
        max(vapply(
          records,
          `[[`,
          integer(1),
          "row_index"
        ))
      },
      budget
    )
  }
  result
}

optimizer_budget_stopped <- function(budget) {
  !is.null(budget$stop_reason)
}

optimizer_budget_summary <- function(budget) {
  elapsed <- optimizer_budget_elapsed(budget)
  list(
    attempts = budget$attempts,
    successes = budget$successes,
    total_errors = budget$total_errors,
    consecutive_errors = budget$consecutive_errors,
    max_errors = budget$max_errors,
    trials = budget$trials,
    metric_calls = if (budget$unknown_metric_calls > 0L) {
      NA_integer_
    } else {
      as.integer(budget$metric_calls)
    },
    known_metric_calls = as.integer(budget$metric_calls),
    provider_calls = if (budget$unknown_provider_calls > 0L) {
      NA_integer_
    } else {
      as.integer(budget$provider_calls)
    },
    known_provider_calls = as.integer(budget$provider_calls),
    input_tokens = if (budget$unknown_input_tokens > 0L) {
      NA_integer_
    } else {
      as.integer(budget$input_tokens)
    },
    known_input_tokens = as.integer(budget$input_tokens),
    output_tokens = if (budget$unknown_output_tokens > 0L) {
      NA_integer_
    } else {
      as.integer(budget$output_tokens)
    },
    known_output_tokens = as.integer(budget$output_tokens),
    total_tokens = if (budget$unknown_total_tokens > 0L) {
      NA_integer_
    } else {
      as.integer(budget$total_tokens)
    },
    known_total_tokens = as.integer(budget$total_tokens),
    total_cost = if (budget$unknown_cost_calls > 0L) {
      NA_real_
    } else {
      budget$known_cost
    },
    known_cost = budget$known_cost,
    unknown_usage = list(
      metric_calls = budget$unknown_metric_calls,
      provider_calls = budget$unknown_provider_calls,
      input_tokens = budget$unknown_input_tokens,
      output_tokens = budget$unknown_output_tokens,
      total_tokens = budget$unknown_total_tokens,
      cost = budget$unknown_cost_calls
    ),
    elapsed_seconds = elapsed,
    limits = budget$limits,
    completed_units = budget$completed_units,
    overshoots = budget$overshoots,
    stopped = optimizer_budget_stopped(budget),
    stop_reason = budget$stop_reason
  )
}

#' Check Budget Stopping Condition
#'
#' @description
#' Check if an optimization run should stop based on budget constraints.
#'
#' @param trial_count Current number of trials completed.
#' @param error_count Current number of consecutive errors.
#' @param control An object created by [optimizer_control()] with budget settings.
#'
#' @return A list with:
#'   - `should_stop`: logical indicating if optimization should stop
#'   - `reason`: character explaining why (or NULL if not stopping)
#'
#' @noRd
check_budget <- function(trial_count, error_count, control) {
  # Check max trials
  if (!is.null(control@max_trials) && !is.na(control@max_trials)) {
    if (trial_count >= control@max_trials) {
      return(list(
        should_stop = TRUE,
        reason = sprintf("Reached max_trials limit (%d)", control@max_trials)
      ))
    }
  }

  # Check max errors
  reached_error_limit <- if (control@max_errors == 0L) {
    error_count > 0L
  } else {
    error_count >= control@max_errors
  }

  if (reached_error_limit) {
    reason <- if (control@max_errors == 0L) {
      "Reached max_errors limit (first error with max_errors = 0)"
    } else {
      sprintf(
        "Reached max_errors limit (%d consecutive errors)",
        control@max_errors
      )
    }

    return(list(
      should_stop = TRUE,
      reason = reason
    ))
  }

  list(should_stop = FALSE, reason = NULL)
}

#' Generate Unique Trial ID
#'
#' @description
#' Generate a unique identifier for a trial, combining timestamp and
#' random suffix.
#'
#' @param prefix Optional prefix for the ID.
#'
#' @return A character string trial ID.
#' @noRd
generate_trial_id <- function(prefix = "trial") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  random_suffix <- paste0(
    sample(c(letters, 0:9), 6, replace = TRUE),
    collapse = ""
  )
  paste(prefix, timestamp, random_suffix, sep = "_")
}

# Helper to get input names from signature
get_input_names <- function(signature) {
  if (!inherits(signature, "dsprrr::Signature")) {
    return(character(0))
  }
  vapply(signature@inputs, function(x) x$name, character(1))
}

# Print an EvalResult object through its S7 method.
print_eval_result <- function(x, ...) {
  cli::cli_h3("Evaluation Result")

  if (is.na(x@mean_score)) {
    cli::cli_alert_warning("No successful evaluations")
  } else {
    cli::cli_alert_success("Mean Score: {round(x@mean_score, 4)}")
    if (!is.na(x@std_error)) {
      cli::cli_text("  SE: {round(x@std_error, 4)}")
    }

    # Show epoch statistics if available
    if (!is.na(x@score_std) && x@epochs > 1) {
      cli::cli_text("  SD (across epochs): {round(x@score_std, 4)}")
    }
    if (!is.na(x@ci_lower) && !is.na(x@ci_upper) && x@epochs > 1) {
      cli::cli_text(
        "  95% CI: [{round(x@ci_lower, 4)}, {round(x@ci_upper, 4)}]"
      )
    }
  }

  # Show epoch count if multi-epoch
  if (x@epochs > 1) {
    cli::cli_text("{.field Epochs}: {x@epochs}")
  }

  cli::cli_text(
    "{.field Evaluated}: {x@n_evaluated} / {x@n_evaluated + x@n_errors}"
  )
  if (x@n_errors > 0) {
    cli::cli_alert_warning("{.field Errors}: {x@n_errors}")
  }

  if (x@total_tokens > 0) {
    cli::cli_text("{.field Tokens}: {x@total_tokens}")
  }

  if (!is.na(x@total_cost) && x@total_cost > 0) {
    cli::cli_text("{.field Cost}: ${format(x@total_cost, digits = 4)}")
  }

  cli::cli_text("{.field Latency}: {round(x@total_latency_ms / 1000, 2)}s")

  invisible(x)
}

# Register S7 print method
S7::method(print, EvalResult) <- print_eval_result
