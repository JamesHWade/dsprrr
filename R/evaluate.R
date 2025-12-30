#' Evaluate a DSPrrr module
#'
#' @description
#' Generic evaluation entry point for DSPrrr modules. Executes the module on a
#' dataset, applies a metric to each example, and returns aggregate statistics
#' together with the predictions and metadata required for downstream analysis.
#'
#' @param module A DSPrrr module created with [module()].
#' @param ... Additional arguments including:
#'   - dataset: A data frame or tibble containing columns that match the
#'     module's signature inputs plus any expected fields used by metric
#'   - metric: A function applied per example with signature
#'     metric(prediction, expected_row)
#'   - .llm: Optional ellmer chat object supplied to run()
#'   - .parallel: Logical; whether to allow parallel execution
#'   - .progress: Logical; whether to display progress while evaluating
#'
#' @return A list with elements
#'   - `mean_score`: numeric mean over all successful metric evaluations.
#'   - `scores`: per-example numeric scores (coerced from logical metrics).
#'   - `predictions`: list of model outputs.
#'   - `metadata`: list of metadata captured from [run()].
#'   - `n_evaluated`: number of successful evaluations.
#'   - `n_errors`: number of metric failures.
#'   - `errors`: character vector with error messages, when any.
#'   - `dataset`: input dataset augmented with prediction metadata.
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
  dataset,
  metric,
  .llm = NULL,
  .parallel = FALSE,
  .progress = TRUE,
  ...
) {
  if (!is.data.frame(dataset)) {
    cli::cli_abort("dataset must be a data frame or tibble")
  }
  if (!is.function(metric)) {
    cli::cli_abort("metric must be a function")
  }

  if (nrow(dataset) == 0) {
    cli::cli_warn("Empty dataset provided")
    return(list(
      mean_score = NA_real_,
      scores = numeric(0),
      predictions = list(),
      metadata = list(),
      n_evaluated = 0L,
      n_errors = 0L,
      errors = character(),
      dataset = dataset
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
    dataset,
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
    expected_row <- dataset[i, , drop = FALSE]
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
        errors[i] <- e$message
        NA_real_
      }
    )
  }

  mean_score <- mean(scores, na.rm = TRUE)
  n_evaluated <- sum(!is.na(scores))
  n_errors <- sum(is.na(scores))

  list(
    mean_score = mean_score,
    scores = scores,
    predictions = predictions,
    metadata = metadata,
    n_evaluated = n_evaluated,
    n_errors = n_errors,
    errors = errors[errors != ""],
    dataset = evaluated
  )
}
