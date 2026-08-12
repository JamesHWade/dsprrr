#' Evaluate a DSPrrr module
#'
#' @description
#' Generic evaluation entry point for DSPrrr modules. Executes the module on a
#' dataset, applies a metric to each example, and returns aggregate statistics
#' together with the predictions and metadata required for downstream analysis.
#'
#' @param module A DSPrrr module created with [module()].
#' @param ... Arguments passed to methods:
#'   - `data`: A data frame or tibble containing columns that match the
#'     module's signature inputs plus any expected fields used by metric.
#'   - `metric`: A function applied per example with signature
#'     `metric(prediction, expected_row)`, or a trace-aware metric created with
#'     [metric_with_trace()].
#'
#'   Additional arguments passed to [run_dataset()]:
#'   - `.llm`: Optional ellmer chat object
#'   - `.parallel`: Logical; whether to allow parallel execution
#'   - `.concurrency`: A policy created by [concurrency_control()]. Do not also
#'     pass `.parallel` when using an explicit policy.
#'   - `.progress`: Logical; whether to display progress while evaluating
#'   - `.return_format`: Character; `"simple"` returns just scores and predictions,
#'     `"structured"` (default) includes full metadata and data
#'   - `epochs`: Integer; number of times to repeat evaluation for statistical
#'     significance (default = 1L). When > 1, each sample is evaluated multiple
#'     times to quantify variation.
#'
#' @return A list with elements. When `.return_format = "structured"` (default):
#'   - `mean_score`: numeric mean over all attempted rows, with run or metric
#'     failures contributing zero. With repeated epochs, the mean covers every
#'     attempted row-epoch.
#'   - `scores`: per-example numeric scores (coerced from logical metrics).
#'   - `predictions`: list of model outputs.
#'   - `metadata`: list of metadata captured from [run()].
#'   - `n_evaluated`: number of successful evaluations.
#'   - `n_errors`: number of rows with run or metric failures.
#'   - `errors`: character vector with all error messages, when any.
#'   - `n_run_errors`, `run_errors`: count and messages for module/LLM failures.
#'   - `n_metric_errors`, `metric_errors`: count and messages for metric failures.
#'   - `total_cost`: total evaluation cost, or `NA` when any call's cost is unknown.
#'   - `feedbacks`: per-example textual feedback when the metric returns
#'     `list(score = , feedback = )` (see [metric_with_feedback()]);
#'     `NA` otherwise.
#'   - `traces`: per-example trace envelopes supplied to trace-aware metrics.
#'     Each contains `row_id`, `epoch`, `status`, ordered module `events`, and
#'     per-row `metadata`. Trace events can contain prompts, inputs, and model
#'     responses, so treat them as potentially sensitive.
#'   - `data`: input data augmented with prediction metadata.
#'
#'   When `epochs > 1`, additional fields are included:
#'   - `epoch_scores`: list of numeric vectors, one per epoch
#'   - `epoch_traces`: list of row-aligned trace lists, one per epoch
#'   - `score_std`: standard deviation of mean scores across epochs
#'   - `ci_95`: 95% confidence interval for the mean score (numeric vector of length 2)
#'
#'   When `.return_format = "simple"`:
#'   - `mean_score`, `scores`, `predictions`, `n_evaluated`, `n_errors`, `errors`
#'   (omits `metadata` and `data` for lighter-weight results)
#'
#' @seealso
#' * [run()] for executing without metrics
#' * [run_dataset()] for batch execution without metrics
#' * [optimize_grid()] for parameter optimization
#' * [metric_exact_match()], [metric_contains()] for built-in metrics
#' @examples
#' \dontrun{
#' classifier <- module(
#'   signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
#'   type = "predict"
#' )
#'
#' testset <- dsp_trainset(
#'   text = c("I love it!", "Awful.", "It's fine."),
#'   sentiment = c("positive", "negative", "neutral")
#' )
#'
#' result <- evaluate(
#'   classifier,
#'   data = testset,
#'   metric = metric_exact_match(field = "sentiment"),
#'   .llm = ellmer::chat_openai()
#' )
#'
#' result$mean_score # accuracy across the test set
#' result$scores # per-example scores
#' result$n_errors # examples where the metric failed
#' }
#' @export
evaluate <- function(module, ...) {
  UseMethod("evaluate")
}

# Capture a trace boundary that also works for modules with bounded retention.
evaluation_trace_cursor <- function(module) {
  traces <- module$state$traces %||% list()
  sequence <- module$state$trace_sequence %||% NULL
  if (
    is.numeric(sequence) &&
      length(sequence) == 1L &&
      !is.na(sequence) &&
      is.finite(sequence) &&
      sequence >= 0
  ) {
    return(list(length = length(traces), sequence = as.numeric(sequence)))
  }
  length(traces)
}


# Return trace indices added after a cursor, including when old retained traces
# were evicted and list length therefore stayed constant.
evaluation_trace_indices <- function(module, cursor) {
  traces <- module$state$traces %||% list()
  if (is.list(cursor) && !is.null(cursor$sequence)) {
    sequence <- module$state$trace_sequence %||% cursor$sequence
    added <- as.numeric(sequence) - as.numeric(cursor$sequence)
    if (!is.finite(added) || added <= 0 || length(traces) == 0L) {
      return(integer())
    }
    retained <- min(length(traces), floor(added))
    return(seq.int(length(traces) - retained + 1L, length(traces)))
  }
  trace_count_before <- as.integer(cursor)
  if (length(traces) <= trace_count_before) {
    return(integer())
  }
  seq.int(trace_count_before + 1L, length(traces))
}


# Return only traces recorded during one evaluation epoch. The boundary is
# important for reused modules and cache hits: no earlier trace can leak into a
# later metric invocation.
new_evaluation_trace_events <- function(module, trace_count_before) {
  traces <- module$state$traces %||% list()
  indices <- evaluation_trace_indices(module, trace_count_before)
  if (length(indices) == 0L) {
    return(list())
  }
  traces[indices]
}

# Project the most recent module event onto the row-level metadata surface used
# by trace-aware metrics. Some internal callers execute `run()` in simple mode,
# where structured result metadata is unavailable even though the trace event
# contains the same observability fields.
program_trace_event_metadata <- function(events) {
  if (!is.list(events) || length(events) == 0L) {
    return(list())
  }
  event <- events[[length(events)]]
  if (!is.list(event)) {
    return(list())
  }

  metadata <- event$metadata %||% list()
  if (!is.list(metadata)) {
    metadata <- list()
  }
  fields <- c(
    "prompt_length",
    "input_tokens",
    "output_tokens",
    "cached_input_tokens",
    "total_tokens",
    "cost",
    "duration_s",
    "latency_ms",
    "model"
  )
  token_fields <- c(
    "input_tokens",
    "output_tokens",
    "cached_input_tokens",
    "total_tokens"
  )
  for (field in fields) {
    value <- event[[field]]
    if (is.null(value) && field %in% token_fields && is.list(event$tokens)) {
      value <- event$tokens[[field]]
    }
    if (is.null(metadata[[field]]) && !is.null(value)) {
      metadata[[field]] <- value
    }
  }
  if (is.null(metadata$prompt_length) && is.character(event$prompt)) {
    metadata$prompt_length <- nchar(paste(event$prompt, collapse = "\n"))
  }
  metadata
}

# Extract the row index added by run_dataset() to each top-level trace. Pipeline
# and iterative modules keep their component calls inside that row event.
evaluation_trace_row <- function(event) {
  if (!is.list(event)) {
    return(NA_integer_)
  }
  candidate <- event$metadata$batch_index %||%
    event$aggregated$batch_index %||%
    NA_integer_
  if (length(candidate) != 1L || is.na(candidate)) {
    return(NA_integer_)
  }
  suppressWarnings(as.integer(candidate))
}

# Group ordered top-level trace events by dataset row. All normal execution
# paths provide batch_index. The positional fallback covers older/custom
# modules that emit exactly one unindexed event per still-unmatched row.
align_evaluation_trace_events <- function(events, n_rows) {
  aligned <- vector("list", n_rows)
  if (n_rows == 0L || length(events) == 0L) {
    return(aligned)
  }

  event_rows <- vapply(events, evaluation_trace_row, integer(1))
  indexed <- which(!is.na(event_rows) & event_rows >= 1L & event_rows <= n_rows)
  for (event_index in indexed) {
    row_index <- event_rows[[event_index]]
    aligned[[row_index]] <- append(
      aligned[[row_index]],
      list(events[[event_index]])
    )
  }

  unindexed <- which(is.na(event_rows) | event_rows < 1L | event_rows > n_rows)
  if (length(indexed) == 0L && length(unindexed) == n_rows) {
    for (offset in seq_len(n_rows)) {
      aligned[[offset]] <- list(events[[unindexed[[offset]]]])
    }
  } else if (n_rows == 1L && length(unindexed) > 0L) {
    aligned[[1L]] <- append(aligned[[1L]], events[unindexed])
  }

  aligned
}

# Aggregate attempted scores without rewarding failures. Raw score vectors keep
# NA for diagnostics, but every NA contributes zero to performance summaries.
failure_adjusted_mean <- function(scores) {
  if (length(scores) == 0) {
    return(NA_real_)
  }

  adjusted_scores <- scores
  adjusted_scores[is.na(adjusted_scores)] <- 0
  result <- mean(adjusted_scores)

  if (is.finite(result)) result else NA_real_
}

# Compute every multi-epoch performance statistic from the same estimand: each
# attempted row-epoch is one observation and failures contribute zero. Epochs
# are required to have equal lengths before this helper is called, so the mean
# of epoch means is also the mean over all attempted row-epoch observations.
summarize_epoch_scores <- function(epoch_scores) {
  epoch_means <- vapply(
    epoch_scores,
    failure_adjusted_mean,
    numeric(1)
  )

  mean_score <- if (length(epoch_means) == 0 || anyNA(epoch_means)) {
    NA_real_
  } else {
    mean(epoch_means)
  }

  no_uncertainty <- list(
    epoch_means = epoch_means,
    mean_score = mean_score,
    score_std = NA_real_,
    ci_95 = c(lower = NA_real_, upper = NA_real_)
  )

  if (length(epoch_means) < 2 || is.na(mean_score)) {
    return(no_uncertainty)
  }

  score_std <- stats::sd(epoch_means)
  if (!is.finite(score_std)) {
    return(no_uncertainty)
  }

  standard_error <- score_std / sqrt(length(epoch_means))
  degrees_freedom <- length(epoch_means) - 1L
  margin <- stats::qt(0.975, df = degrees_freedom) * standard_error
  ci_95 <- c(
    lower = mean_score - margin,
    upper = mean_score + margin
  )

  if (!all(is.finite(ci_95))) {
    ci_95[] <- NA_real_
  }

  list(
    epoch_means = epoch_means,
    mean_score = mean_score,
    score_std = score_std,
    ci_95 = ci_95
  )
}

#' Evaluate an R6 Module
#'
#' @details
#' Parallel execution is conservative by default to avoid reusing non-
#' serialisable LLM client objects across workers. When `.parallel = TRUE`, a
#' fresh client is created per worker only if `.llm` is `NULL`; otherwise the
#' call falls back to sequential execution with a warning.
#'
#' @exportS3Method
#' @noRd
evaluate.Module <- function(
  module,
  data,
  metric,
  .llm = NULL,
  .parallel = FALSE,
  .concurrency = NULL,
  .progress = TRUE,
  .return_format = c("structured", "simple"),
  epochs = 1L,
  .trace_row_ids = NULL,
  .propagate_provider_errors = FALSE,
  ...
) {
  parallel_missing <- missing(.parallel)
  concurrency_missing <- missing(.concurrency)
  explicit_concurrency <- !concurrency_missing && !is.null(.concurrency)
  if (explicit_concurrency && !parallel_missing) {
    cli::cli_abort(
      c(
        "{.arg .concurrency} cannot be combined with {.arg .parallel}",
        "i" = "Configure workers and backend with {.fn concurrency_control} only."
      ),
      class = "dsprrr_concurrency_argument_conflict"
    )
  }
  if (explicit_concurrency) {
    .concurrency <- validate_concurrency_control(.concurrency)
  }
  .return_format <- match.arg(.return_format)
  if (
    !is.logical(.propagate_provider_errors) ||
      length(.propagate_provider_errors) != 1L ||
      is.na(.propagate_provider_errors)
  ) {
    cli::cli_abort(
      "{.arg .propagate_provider_errors} must be TRUE or FALSE",
      class = "dsprrr_evaluation_argument_error"
    )
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

  if (!is.data.frame(data)) {
    cli::cli_abort(c(
      "{.arg data} must be a data frame or tibble",
      "x" = "Got {.cls {class(data)[1]}}",
      "i" = "Use {.code data.frame()} or {.code tibble::tibble()} to create one"
    ))
  }
  if (is.null(.trace_row_ids)) {
    .trace_row_ids <- seq_len(nrow(data))
  }
  valid_trace_row_ids <- is.numeric(.trace_row_ids) &&
    length(.trace_row_ids) == nrow(data) &&
    !anyNA(.trace_row_ids) &&
    all(is.finite(.trace_row_ids)) &&
    all(.trace_row_ids == floor(.trace_row_ids)) &&
    all(.trace_row_ids >= 1) &&
    all(.trace_row_ids <= .Machine$integer.max) &&
    !anyDuplicated(.trace_row_ids)
  if (!valid_trace_row_ids) {
    cli::cli_abort(
      "Internal trace row IDs must be unique positive integers aligned with {.arg data}",
      class = "dsprrr_evaluation_trace_row_error"
    )
  }
  .trace_row_ids <- as.integer(.trace_row_ids)
  if (!is.function(metric)) {
    cli::cli_abort(c(
      "{.arg metric} must be a function",
      "x" = "Got {.cls {class(metric)[1]}}",
      "i" = "Use a metric from {.code metric_exact_match()}, {.code metric_contains()}, etc.",
      "i" = "Or provide a custom function: {.code function(prediction, row) ...}"
    ))
  }

  if (nrow(data) == 0) {
    cli::cli_warn("Empty data provided")
    return(list(
      mean_score = NA_real_,
      scores = numeric(0),
      predictions = list(),
      metadata = list(),
      n_evaluated = 0L,
      n_errors = 0L,
      errors = character(),
      n_run_errors = 0L,
      run_errors = character(),
      n_metric_errors = 0L,
      metric_errors = character(),
      total_cost = 0,
      feedbacks = character(),
      traces = list(),
      data = data
    ))
  }

  # Safety: disallow parallel reuse of custom LLM clients
  parallel_allowed <- .parallel
  if (!explicit_concurrency && .parallel && !is.null(.llm)) {
    cli::cli_warn(
      "Parallel execution requires a NULL .llm so each worker can create its own client; falling back to sequential processing"
    )
    parallel_allowed <- FALSE
  }

  # Run evaluation for each epoch
  epoch_results <- vector("list", epochs)

  for (epoch in seq_len(epochs)) {
    # Show progress for multi-epoch runs
    if (epochs > 1 && .progress) {
      cli::cli_alert_info("Running epoch {epoch}/{epochs}")
    }

    # Execute module with error handling
    execution_args <- if (explicit_concurrency) {
      list(.concurrency = .concurrency)
    } else {
      list(.parallel = parallel_allowed)
    }
    trace_count_before <- evaluation_trace_cursor(module)
    evaluated <- tryCatch(
      {
        evaluated <- do.call(
          run_dataset,
          c(
            list(
              module = module,
              data = data,
              .llm = .llm,
              .progress = .progress && epochs == 1,
              .return_format = "structured"
            ),
            execution_args,
            list(...)
          )
        )
        if (isTRUE(.propagate_provider_errors)) {
          conditions <- attr(
            evaluated,
            "dsprrr_error_conditions",
            exact = TRUE
          ) %||%
            list()
          for (condition in conditions) {
            provider_condition <- run_provider_error_condition(condition)
            if (!is.null(provider_condition)) {
              stop(provider_condition)
            }
          }
        }
        evaluated
      },
      error = function(e) {
        provider_condition <- run_provider_error_condition(e)
        if (!is.null(provider_condition)) {
          stop(provider_condition)
        }
        cli::cli_abort(c(
          "Epoch {epoch}/{epochs} failed during module execution",
          "x" = conditionMessage(e),
          "i" = "Successfully completed {epoch - 1} epoch(s) before failure",
          "i" = "Consider reducing dataset size or disabling parallel processing"
        ))
      }
    )

    predictions <- evaluated$result
    metadata <- if (".metadata" %in% names(evaluated)) {
      evaluated$.metadata
    } else {
      replicate(nrow(evaluated), list(), simplify = FALSE)
    }
    row_trace_events <- attr(
      evaluated,
      "dsprrr_row_trace_events",
      exact = TRUE
    )
    if (
      !is.list(row_trace_events) || length(row_trace_events) != nrow(evaluated)
    ) {
      row_trace_events <- align_evaluation_trace_events(
        new_evaluation_trace_events(module, trace_count_before),
        nrow(evaluated)
      )
    }

    scores <- numeric(nrow(evaluated))
    run_errors <- vapply(
      metadata,
      function(item) {
        error <- item$error %||% ""
        if (length(error) == 0 || is.na(error[[1]])) {
          ""
        } else {
          as.character(error[[1]])
        }
      },
      character(1)
    )
    metric_errors <- character(nrow(evaluated))
    errors <- run_errors
    feedbacks <- rep(NA_character_, nrow(evaluated))
    traces <- vector("list", nrow(evaluated))
    total_cost <- sum_cost_values(vapply(
      metadata,
      function(item) item$cost %||% NA_real_,
      numeric(1)
    ))

    for (i in seq_len(nrow(evaluated))) {
      expected_row <- data[i, , drop = FALSE]
      prediction <- predictions[[i]]
      program_trace <- new_program_trace(
        events = row_trace_events[[i]] %||% list(),
        metadata = metadata[[i]],
        row_id = .trace_row_ids[[i]],
        epoch = epoch
      )
      traces[[i]] <- program_trace

      # Preserve the primary module/provider failure. Calling the metric with an
      # NA prediction would replace the useful run error with a secondary one.
      if (nzchar(run_errors[i])) {
        scores[i] <- NA_real_
        next
      }

      scores[i] <- tryCatch(
        {
          normalized <- normalize_metric_result(
            invoke_metric(
              metric,
              prediction,
              expected_row,
              program_trace
            )
          )
          feedbacks[i] <- normalized$feedback
          normalized$score
        },
        error = function(e) {
          metric_errors[i] <<- e$message
          errors[i] <<- e$message

          # Include epoch context in error message
          epoch_context <- if (epochs > 1) {
            paste0(" (epoch ", epoch, "/", epochs, ")")
          } else {
            ""
          }

          cli::cli_warn(
            c(
              "Metric evaluation failed for row {i}{epoch_context}",
              "x" = e$message
            ),
            class = "dsprrr_metric_error"
          )
          NA_real_
        }
      )
    }

    # Store this epoch's results
    epoch_results[[epoch]] <- list(
      scores = scores,
      predictions = predictions,
      metadata = metadata,
      errors = errors,
      run_errors = run_errors,
      metric_errors = metric_errors,
      total_cost = total_cost,
      feedbacks = feedbacks,
      traces = traces,
      evaluated = evaluated
    )
  }

  # Aggregate across epochs
  # For single epoch, use the epoch 1 results directly
  # For multiple epochs, aggregate scores and use last epoch's predictions/metadata
  if (epochs == 1) {
    scores <- epoch_results[[1]]$scores
    predictions <- epoch_results[[1]]$predictions
    metadata <- epoch_results[[1]]$metadata
    errors <- epoch_results[[1]]$errors
    run_errors <- epoch_results[[1]]$run_errors
    metric_errors <- epoch_results[[1]]$metric_errors
    feedbacks <- epoch_results[[1]]$feedbacks
    traces <- epoch_results[[1]]$traces
    evaluated <- epoch_results[[1]]$evaluated
  } else {
    # Extract scores from all epochs (computed once, reused later)
    all_epoch_scores <- lapply(epoch_results, function(x) x$scores)

    # Validate all epochs have same length
    epoch_lengths <- lengths(all_epoch_scores)
    if (length(unique(epoch_lengths)) > 1) {
      cli::cli_abort(c(
        "Epochs produced inconsistent result lengths",
        "x" = "Expected all epochs to evaluate {nrow(data)} examples",
        "i" = "Epoch lengths: {paste(epoch_lengths, collapse = ', ')}",
        "i" = "This indicates a bug in run_dataset() or data corruption"
      ))
    }

    # Compute mean score per example across epochs
    # Mark as NA if ANY epoch failed (conservative approach to not hide intermittent failures)
    scores <- vapply(
      seq_len(nrow(data)),
      function(i) {
        epoch_scores_i <- vapply(all_epoch_scores, function(s) s[i], numeric(1))
        # If any epoch failed, mark this row as failed
        if (anyNA(epoch_scores_i)) {
          NA_real_
        } else {
          mean(epoch_scores_i)
        }
      },
      numeric(1)
    )

    # Aggregate error channels across all epochs. A row has an error if it
    # failed in any epoch, while preserving whether execution or scoring failed.
    aggregate_epoch_errors <- function(field) {
      all_errors <- character(nrow(data))
      for (i in seq_len(nrow(data))) {
        epoch_errors_i <- character(0)
        for (epoch_idx in seq_along(epoch_results)) {
          err <- epoch_results[[epoch_idx]][[field]][i]
          if (!is.na(err) && err != "") {
            epoch_errors_i <- c(
              epoch_errors_i,
              paste0("Epoch ", epoch_idx, ": ", err)
            )
          }
        }
        if (length(epoch_errors_i) > 0) {
          all_errors[i] <- paste(epoch_errors_i, collapse = "; ")
        }
      }
      all_errors
    }
    errors <- aggregate_epoch_errors("errors")
    run_errors <- aggregate_epoch_errors("run_errors")
    metric_errors <- aggregate_epoch_errors("metric_errors")

    # Use last epoch's predictions, metadata, and feedback for the result
    predictions <- epoch_results[[epochs]]$predictions
    metadata <- epoch_results[[epochs]]$metadata
    feedbacks <- epoch_results[[epochs]]$feedbacks
    traces <- epoch_results[[epochs]]$traces
    evaluated <- epoch_results[[epochs]]$evaluated
  }

  total_cost <- sum_cost_values(vapply(
    epoch_results,
    function(result) result$total_cost,
    numeric(1)
  ))

  epoch_summary <- if (epochs > 1) {
    summarize_epoch_scores(all_epoch_scores)
  } else {
    NULL
  }

  # Optimizers select on `mean_score`, so failures must contribute zero instead
  # of disappearing. For multiple epochs, use the same row-epoch estimand as
  # the uncertainty statistics rather than the diagnostic per-row `scores`.
  mean_score <- if (epochs > 1) {
    epoch_summary$mean_score
  } else {
    failure_adjusted_mean(scores)
  }
  n_evaluated <- sum(!is.na(scores))
  n_errors <- sum(is.na(scores))
  n_run_errors <- sum(nzchar(run_errors))
  n_metric_errors <- sum(nzchar(metric_errors))

  # Build result based on return format
  result <- list(
    mean_score = mean_score,
    scores = scores,
    predictions = predictions,
    n_evaluated = n_evaluated,
    n_errors = n_errors,
    errors = errors[errors != ""],
    n_run_errors = n_run_errors,
    run_errors = run_errors[run_errors != ""],
    n_metric_errors = n_metric_errors,
    metric_errors = metric_errors[metric_errors != ""],
    total_cost = total_cost,
    feedbacks = feedbacks
  )

  # Add epoch-specific statistics when epochs > 1
  if (epochs > 1) {
    # Use all_epoch_scores already computed during aggregation
    result$epoch_scores <- all_epoch_scores
    result$score_std <- epoch_summary$score_std
    result$ci_95 <- epoch_summary$ci_95

    degrees_freedom <- epochs - 1L
    if (degrees_freedom < 3L && !is.na(result$score_std)) {
      cli::cli_warn(c(
        "Confidence intervals computed with only {epochs} epochs (df={degrees_freedom})",
        "i" = "Consider using epochs >= 5 for reliable statistical inference",
        "i" = "Current CI may be extremely wide and unreliable"
      ))
    }
  }

  # Add metadata and data for structured format
  if (.return_format == "structured") {
    result$metadata <- metadata
    result$traces <- traces
    if (epochs > 1) {
      result$epoch_traces <- lapply(epoch_results, `[[`, "traces")
    }
    result$data <- evaluated
  }

  structure(result, class = "dsprrr_evaluation")
}

#' Print method for dsprrr_evaluation
#' @param x A dsprrr_evaluation object
#' @param ... Additional arguments (unused)
#' @export
print.dsprrr_evaluation <- function(x, ...) {
  cli::cli_h3("DSPrrr Evaluation Results")

  if (is.na(x$mean_score)) {
    cli::cli_alert_warning("No successful evaluations")
  } else {
    cli::cli_alert_success("Mean Score: {round(x$mean_score, 4)}")

    # Show epoch statistics if available
    if (!is.null(x$score_std)) {
      cli::cli_text("  SD: {round(x$score_std, 4)}")
    }
    if (!is.null(x$ci_95) && !anyNA(x$ci_95)) {
      cli::cli_text(
        "  95% CI: [{round(x$ci_95[1], 4)}, {round(x$ci_95[2], 4)}]"
      )
    }
  }

  # Show epoch info if multi-epoch evaluation
  if (!is.null(x$epoch_scores)) {
    cli::cli_text("{.field Epochs}: {length(x$epoch_scores)}")
  }

  cli::cli_text("{.field Evaluated}: {x$n_evaluated}")
  if (x$n_errors > 0) {
    cli::cli_alert_warning("{.field Errors}: {x$n_errors}")
    if (!is.null(x$n_run_errors) && x$n_run_errors > 0) {
      cli::cli_text("  Run: {x$n_run_errors}")
    }
    if (!is.null(x$n_metric_errors) && x$n_metric_errors > 0) {
      cli::cli_text("  Metric: {x$n_metric_errors}")
    }
  }

  if (length(x$scores) <= 10) {
    cli::cli_text(
      "{.field Scores}: {paste(round(x$scores, 3), collapse = ', ')}"
    )
  } else {
    cli::cli_text(
      "{.field Scores}: {paste(round(x$scores[1:5], 3), collapse = ', ')}, ... ({length(x$scores)} total)"
    )
  }

  invisible(x)
}
