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
    ...
) {
  .return_format <- match.arg(.return_format)
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

  # Execute module
  evaluated <- run_dataset(
    module,
    data,
    .llm = .llm,
    .parallel = parallel_allowed,
    .progress = .progress,
    .return_format = "structured",
    ...
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
        cli::cli_warn(
          c("Metric evaluation failed for row {i}",
            "x" = e$message),
          class = "dsprrr_metric_error"
        )
        NA_real_
      }
    )
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
  }

  cli::cli_text("{.field Evaluated}: {x$n_evaluated}")
  if (x$n_errors > 0) {
    cli::cli_alert_warning("{.field Errors}: {x$n_errors}")
  }

  if (length(x$scores) <= 10) {
    cli::cli_text("{.field Scores}: {paste(round(x$scores, 3), collapse = ', ')}")
  } else {
    cli::cli_text(
      "{.field Scores}: {paste(round(x$scores[1:5], 3), collapse = ', ')}, ... ({length(x$scores)} total)"
    )
  }

  invisible(x)
}
