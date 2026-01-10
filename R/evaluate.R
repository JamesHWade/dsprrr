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
#'     `metric(prediction, expected_row)`.
#'
#'   Additional arguments passed to [run_dataset()]:
#'   - `.llm`: Optional ellmer chat object
#'   - `.parallel`: Logical; whether to allow parallel execution
#'   - `.progress`: Logical; whether to display progress while evaluating
#'   - `.return_format`: Character; `"simple"` returns just scores and predictions,
#'     `"structured"` (default) includes full metadata and data
#'   - `epochs`: Integer; number of times to repeat evaluation for statistical
#'     significance (default = 1L). When > 1, each sample is evaluated multiple
#'     times to quantify variation.
#'
#' @return A list with elements. When `.return_format = "structured"` (default):
#'   - `mean_score`: numeric mean over all successful metric evaluations.
#'   - `scores`: per-example numeric scores (coerced from logical metrics).
#'   - `predictions`: list of model outputs.
#'   - `metadata`: list of metadata captured from [run()].
#'   - `n_evaluated`: number of successful evaluations.
#'   - `n_errors`: number of metric failures.
#'   - `errors`: character vector with error messages, when any.
#'   - `data`: input data augmented with prediction metadata.
#'
#'   When `epochs > 1`, additional fields are included:
#'   - `epoch_scores`: list of numeric vectors, one per epoch
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
#' @export
evaluate <- function(module, ...) {
  UseMethod("evaluate")
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
  .progress = TRUE,
  .return_format = c("structured", "simple"),
  epochs = 1L,
  ...
) {
  .return_format <- match.arg(.return_format)

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
      data = data
    ))
  }

  # Safety: disallow parallel reuse of custom LLM clients
  parallel_allowed <- .parallel
  if (.parallel && !is.null(.llm)) {
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
    evaluated <- tryCatch(
      {
        run_dataset(
          module,
          data,
          .llm = .llm,
          .parallel = parallel_allowed,
          .progress = .progress && epochs == 1,
          .return_format = "structured",
          ...
        )
      },
      error = function(e) {
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

    scores <- numeric(nrow(evaluated))
    errors <- character(nrow(evaluated))

    for (i in seq_len(nrow(evaluated))) {
      expected_row <- data[i, , drop = FALSE]
      prediction <- predictions[[i]]

      scores[i] <- tryCatch(
        {
          score <- metric(prediction, expected_row)
          if (is.logical(score)) {
            as.numeric(score)
          } else if (is.numeric(score)) {
            score
          } else {
            cli::cli_abort(c(
              "Metric must return logical or numeric values",
              "i" = "Got {.cls {class(score)[1]}}"
            ))
          }
        },
        error = function(e) {
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
    evaluated <- epoch_results[[1]]$evaluated
  } else {
    # Extract scores from all epochs (computed once, reused later)
    all_epoch_scores <- lapply(epoch_results, function(x) x$scores)

    # Validate all epochs have same length
    epoch_lengths <- vapply(all_epoch_scores, length, integer(1))
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
        if (any(is.na(epoch_scores_i))) {
          NA_real_
        } else {
          mean(epoch_scores_i, na.rm = TRUE)
        }
      },
      numeric(1)
    )

    # Aggregate errors across all epochs
    # A row has an error if it failed in ANY epoch
    all_errors <- character(nrow(data))
    for (i in seq_len(nrow(data))) {
      epoch_errors_i <- character(0)
      for (epoch_idx in seq_along(epoch_results)) {
        err <- epoch_results[[epoch_idx]]$errors[i]
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
    errors <- all_errors

    # Use last epoch's predictions and metadata for the result
    predictions <- epoch_results[[epochs]]$predictions
    metadata <- epoch_results[[epochs]]$metadata
    evaluated <- epoch_results[[epochs]]$evaluated
  }

  mean_score <- mean(scores, na.rm = TRUE)
  n_evaluated <- sum(!is.na(scores))
  n_errors <- sum(is.na(scores))

  # Build result based on return format
  result <- list(
    mean_score = mean_score,
    scores = scores,
    predictions = predictions,
    n_evaluated = n_evaluated,
    n_errors = n_errors,
    errors = errors[errors != ""]
  )

  # Add epoch-specific statistics when epochs > 1
  if (epochs > 1) {
    # Use all_epoch_scores already computed during aggregation
    result$epoch_scores <- all_epoch_scores

    # Compute mean score for each epoch
    epoch_means <- vapply(
      all_epoch_scores,
      function(s) mean(s, na.rm = TRUE),
      numeric(1)
    )

    # Standard deviation of epoch means with validation
    score_std_raw <- stats::sd(epoch_means, na.rm = TRUE)

    # Validate statistical results
    if (is.nan(score_std_raw) || is.infinite(score_std_raw)) {
      cli::cli_warn(c(
        "Invalid standard deviation computed across epochs",
        "x" = "Got {score_std_raw}",
        "i" = "Epoch means: {paste(round(epoch_means, 4), collapse = ', ')}",
        "i" = "This may indicate metric computation errors"
      ))
      result$score_std <- NA_real_
    } else {
      result$score_std <- score_std_raw
    }

    # 95% confidence interval using t-distribution
    # Appropriate for any sample size, especially small n
    if (
      length(epoch_means) > 1 &&
        !all(is.na(epoch_means)) &&
        !is.na(result$score_std)
    ) {
      se <- result$score_std / sqrt(length(epoch_means))

      # Validate SE
      if (is.nan(se) || is.infinite(se)) {
        cli::cli_warn(c(
          "Invalid standard error: {se}",
          "i" = "Cannot compute valid confidence intervals"
        ))
        result$ci_95 <- c(lower = NA_real_, upper = NA_real_)
      } else {
        df <- length(epoch_means) - 1

        # Warn about low degrees of freedom
        if (df < 3) {
          cli::cli_warn(c(
            "Confidence intervals computed with only {epochs} epochs (df={df})",
            "i" = "Consider using epochs >= 5 for reliable statistical inference",
            "i" = "Current CI may be extremely wide and unreliable"
          ))
        }

        t_crit <- stats::qt(0.975, df = df)
        margin <- t_crit * se
        result$ci_95 <- c(
          lower = mean(epoch_means, na.rm = TRUE) - margin,
          upper = mean(epoch_means, na.rm = TRUE) + margin
        )

        # Validate final CI
        if (any(is.nan(result$ci_95)) || any(is.infinite(result$ci_95))) {
          cli::cli_warn(
            "Invalid confidence interval computed: [{result$ci_95[1]}, {result$ci_95[2]}]"
          )
          result$ci_95 <- c(lower = NA_real_, upper = NA_real_)
        }
      }
    } else {
      result$ci_95 <- c(lower = NA_real_, upper = NA_real_)
    }
  }

  # Add metadata and data for structured format
  if (.return_format == "structured") {
    result$metadata <- metadata
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
    if (!is.null(x$ci_95) && !any(is.na(x$ci_95))) {
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
