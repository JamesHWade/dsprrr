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

#' @export
evaluate.Module <- function(module, dataset, metric,
                             .llm = NULL, .parallel = FALSE,
                             .progress = TRUE, ...) {
  if (!is.data.frame(dataset)) {
    cli::cli_abort("dataset must be a data frame or tibble")
  }

  metric_spec <- resolve_metric_spec(metric)

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
    cli::cli_warn("Parallel execution requires a NULL .llm so each worker can create its own client; falling back to sequential processing")
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
  metadata <- if (".metadata" %in% names(evaluated)) evaluated$.metadata else replicate(nrow(evaluated), list(), simplify = FALSE)

  per_example_fn <- metric_spec$per_example
  scores <- numeric()
  errors <- character()

  if (!is.null(per_example_fn)) {
    scores <- numeric(nrow(evaluated))
    errors <- character(nrow(evaluated))

    for (i in seq_len(nrow(evaluated))) {
      expected_row <- dataset[i, , drop = FALSE]
      prediction <- predictions[[i]]

      scores[i] <- tryCatch({
        score <- per_example_fn(prediction, expected_row)
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
      }, error = function(e) {
        errors[i] <- e$message
        NA_real_
      })
    }
  }

  aggregate_result <- metric_spec$aggregate(
    scores = scores,
    predictions = predictions,
    dataset = dataset,
    metadata = metadata,
    errors = errors
  )

  mean_score <- aggregate_result$mean_score %||% if (length(scores)) mean(scores, na.rm = TRUE) else NA_real_
  final_scores <- aggregate_result$scores %||% scores
  n_evaluated <- aggregate_result$n_evaluated %||% if (length(scores)) sum(!is.na(scores)) else nrow(dataset)
  n_errors <- aggregate_result$n_errors %||% if (length(scores)) sum(is.na(scores)) else 0L
  metrics_tbl <- aggregate_result$metrics %||% tibble::tibble()
  metric_metadata <- aggregate_result$metadata %||% metric_spec$metadata %||% list()
  combined_errors <- aggregate_result$errors %||% errors[errors != ""]

  list(
    mean_score = mean_score,
    scores = final_scores,
    predictions = predictions,
    metadata = metadata,
    n_evaluated = n_evaluated,
    n_errors = n_errors,
    errors = combined_errors,
    dataset = evaluated,
    metrics = metrics_tbl,
    metric_metadata = metric_metadata
  )
}
