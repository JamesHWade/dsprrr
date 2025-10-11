#' Convert a dsprrr module into a vitals solver
#'
#' @description
#' Creates a function compatible with vitals Tasks that executes a DSPrrr
#' module against batches of inputs. The solver automatically converts vitals'
#' vector input (from `dataset$input`) to a properly-named data frame matching
#' the module's signature, forwards it to [run_dataset()], and returns
#' vitals-friendly objects containing results, chat logs, and metadata.
#'
#' @param module A DSPrrr module (e.g., created via [module()]).
#' @param .llm Optional ellmer chat object. When `NULL`, each invocation will
#'   create a fresh default client.
#' @param .parallel Logical; forwarded to [run_dataset()]. Defaults to `FALSE`
#'   to avoid sharing LLM state across workers.
#' @param .return_format One of `"structured"` (default) or `"simple"`.
#' @param ... Additional arguments forwarded to [run_dataset()].
#'
#' @return A function accepting either a vector (from vitals `dataset$input`) or
#'   a data frame of inputs, and returning a list with components `result`,
#'   `solver_chat`, and `metadata`.
#' @export
as_vitals_solver <- function(module, .llm = NULL, .parallel = FALSE,
                             .return_format = "structured", ...) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("as_vitals_solver() requires an R6 Module object")
  }

  .return_format <- match.arg(.return_format, c("simple", "structured"))

  function(inputs, ...) {
    # vitals passes a vector from dataset$input, but we need a data frame
    # with the correct column name matching the module's signature
    if (!is.data.frame(inputs)) {
      # Get the first input name from the module signature
      if (length(module$signature@inputs) > 0) {
        input_name <- module$signature@inputs[[1]]$name
        inputs <- stats::setNames(data.frame(inputs, stringsAsFactors = FALSE), input_name)
      } else {
        # Fallback: create generic column name
        inputs <- data.frame(input = inputs, stringsAsFactors = FALSE)
      }
    }

    results <- run_dataset(
      module,
      inputs,
      .llm = .llm,
      .parallel = .parallel,
      .progress = FALSE,
      .return_format = .return_format,
      ...
    )

    if (.return_format == "simple") {
      list(
        result = results$result,
        solver_chat = replicate(nrow(results), NULL, simplify = FALSE),
        metadata = replicate(nrow(results), list(), simplify = FALSE)
      )
    } else {
      list(
        result = results$result,
        solver_chat = results$.chat,
        metadata = results$.metadata
      )
    }
  }
}

#' Adapt a vitals scorer for use as a dsprrr metric
#'
#' @description
#' Converts a vitals scorer or yardstick metric into a dsprrr-compatible
#' specification. Vitals scorers are wrapped as per-example metric functions,
#' while yardstick metrics return an aggregate specification that produces tidy
#' summaries for optimisation workflows.
#'
#' @param vitals_scorer A function that accepts a tibble/data frame and returns
#'   a tibble with a `score` column (following vitals conventions).
#' @param input_column Name of the column to populate with the example input.
#'   If the column is absent in `expected_row`, `NA` is supplied.
#' @param target_column Name of the column holding the ground-truth label inside
#'   the vitals sample tibble.
#' @param result_column Name of the column that receives the model prediction.
#'
#' @param truth Column in the dataset representing the ground truth when
#'   wrapping yardstick metrics. Defaults to `target_column`.
#' @param estimate Name of the column added to the dataset containing model
#'   predictions when wrapping yardstick metrics.
#' @param transform Optional function applied to each prediction before passing
#'   it to yardstick metrics. Must return a scalar compatible with the truth
#'   column (e.g., a factor with matching levels).
#' @param metric_name When wrapping a yardstick metric set, optionally select a
#'   specific metric by name from the results.
#' @param estimator Optional estimator identifier used to filter yardstick
#'   results (for metrics that return multiple estimators).
#' @param metric_args Named list of additional arguments supplied to the
#'   yardstick metric function via `rlang::exec()`.
#' @param aggregate How to combine multiple yardstick scores into a single value
#'   for optimisation. Either `"mean"` (default) or `"median"`.
#' @param type Force interpretation as a `"vitals"` scorer or `"yardstick"`
#'   metric. Defaults to `"auto"`, which chooses based on the object supplied.
#'
#' @return Either a per-example metric function (for vitals scorers) or a metric
#'   specification compatible with dsprrr optimisation helpers (for yardstick
#'   metrics).
#' @export
as_dsprrr_metric <- function(vitals_scorer,
                             input_column = "input",
                             target_column = "target",
                             result_column = "result",
                             truth = target_column,
                             estimate = ".prediction",
                             transform = NULL,
                             metric_name = NULL,
                             estimator = NULL,
                             metric_args = list(),
                             aggregate = c("mean", "median"),
                             type = c("auto", "vitals", "yardstick")) {
  aggregate <- match.arg(aggregate)
  type <- match.arg(type)

  if (identical(type, "auto")) {
    is_yardstick <- inherits(vitals_scorer, "metric_set") || inherits(vitals_scorer, "yardstick_metric")
    type <- if (is_yardstick) "yardstick" else "vitals"
  }

  if (identical(type, "yardstick")) {
    rlang::check_installed("yardstick", reason = "to wrap yardstick metrics")

    metric_fn <- if (inherits(vitals_scorer, "metric_set")) {
      vitals_scorer
    } else {
      yardstick::metric_set(vitals_scorer)
    }

    truth_sym <- rlang::ensym(truth)
    estimate_sym <- rlang::sym(estimate)
    transform_fn <- transform %||% base::identity

    aggregate_fn <- function(scores, predictions, dataset, metadata, errors, ...) {
      if (!length(predictions)) {
        return(list(
          mean_score = NA_real_,
          scores = numeric(),
          metrics = tibble::tibble(),
          n_evaluated = 0L,
          n_errors = 0L
        ))
      }

      # Extract single predictions from list wrappers if needed
      # (predictions from forward() come as list(value))
      unwrapped_preds <- lapply(predictions, function(p) {
        if (is.list(p) && length(p) == 1 && !is.data.frame(p)) {
          p[[1]]
        } else {
          p
        }
      })

      # Apply transform to each prediction
      transformed_preds <- lapply(unwrapped_preds, transform_fn)

      # Check that all transforms return consistent length
      lengths <- vapply(transformed_preds, length, integer(1))
      if (length(unique(lengths)) > 1) {
        cli::cli_abort("transform must return a consistent length for yardstick metrics")
      }

      # For single values, extract them preserving type
      if (all(lengths == 1)) {
        pred_vec <- unlist(transformed_preds, recursive = FALSE, use.names = FALSE)
      } else {
        # For multi-value predictions, we'd need different handling
        pred_vec <- transformed_preds
      }

      data <- dataset
      data[[estimate]] <- pred_vec

      # Validate data before calling yardstick
      truth_col <- rlang::as_string(truth_sym)
      if (!truth_col %in% names(data)) {
        cli::cli_warn("Truth column '{truth_col}' not found in dataset for yardstick metric")
        return(list(
          mean_score = NA_real_,
          scores = rep(NA_real_, nrow(dataset)),
          metrics = tibble::tibble(),
          n_evaluated = nrow(dataset),
          n_errors = nrow(dataset),
          metadata = list(type = "yardstick", truth = truth_col, estimate = estimate, error = "truth column not found")
        ))
      }

      if (nrow(data) == 0) {
        cli::cli_warn("Empty dataset provided to yardstick metric")
        return(list(
          mean_score = NA_real_,
          scores = numeric(),
          metrics = tibble::tibble(),
          n_evaluated = 0L,
          n_errors = 0L,
          metadata = list(type = "yardstick", truth = truth_col, estimate = estimate, error = "empty dataset")
        ))
      }

      # Build the arguments list with the truth and estimate symbols
      args <- c(
        list(data = data, truth = truth_sym, estimate = estimate_sym),
        metric_args
      )

      # Call yardstick metric with detailed error handling
      results <- tryCatch(
        rlang::exec(metric_fn, !!!args),
        error = function(e) {
          # Provide diagnostic information
          cli::cli_warn(c(
            "Failed to compute yardstick metric",
            "i" = "Data has {nrow(data)} rows",
            "i" = "Truth column: {rlang::as_string(truth_sym)}",
            "i" = "Estimate column: {estimate}",
            "x" = "Error: {e$message}"
          ))
          tibble::tibble()
        }
      )

      if (!is.null(metric_name)) {
        results <- results[results$.metric == metric_name, , drop = FALSE]
      }
      if (!is.null(estimator) && ".estimator" %in% names(results)) {
        results <- results[results$.estimator == estimator, , drop = FALSE]
      }

      # Yardstick metrics compute aggregate scores, not per-example scores
      # For the optimization to work, we need per-example scores
      # We'll compute binary correctness for each example
      truth_col <- rlang::as_string(truth_sym)
      if (truth_col %in% names(data)) {
        per_example_scores <- as.numeric(data[[truth_col]] == data[[estimate]])
      } else {
        per_example_scores <- numeric(nrow(dataset))
      }

      # The aggregate metric value from yardstick
      aggregate_score <- if (nrow(results) > 0 && ".estimate" %in% names(results)) {
        results$.estimate[1]
      } else {
        NA_real_
      }

      list(
        mean_score = aggregate_score,
        scores = per_example_scores,
        metrics = results,
        n_evaluated = nrow(dataset),
        n_errors = sum(is.na(per_example_scores)),
        metadata = list(type = "yardstick", truth = rlang::as_string(truth_sym), estimate = estimate)
      )
    }

    return(new_metric_spec(
      per_example = NULL,
      aggregate = aggregate_fn,
      metadata = list(type = "yardstick", truth = rlang::as_string(truth_sym), estimate = estimate, metric = metric_name)
    ))
  }

  if (!is.function(vitals_scorer)) {
    cli::cli_abort("vitals_scorer must be a function")
  }

  metric_fn <- function(prediction, expected_row) {
    sample <- tibble::tibble(
      !!input_column := list(if (input_column %in% names(expected_row)) expected_row[[input_column]] else NA),
      !!target_column := list(if (target_column %in% names(expected_row)) expected_row[[target_column]] else NA),
      !!result_column := list(prediction)
    )

    scores <- vitals_scorer(sample)

    if (!is.data.frame(scores) || nrow(scores) == 0) {
      cli::cli_warn("vitals scorer returned no results; treating as NA")
      return(NA_real_)
    }

    score_val <- scores$score[[1]]

    if (is.numeric(score_val)) {
      return(score_val)
    }

    if (is.logical(score_val)) {
      return(as.numeric(score_val))
    }

    if (is.character(score_val)) {
      score_lower <- tolower(score_val)
      if (score_lower %in% c("c", "correct", "pass")) {
        return(1)
      }
      if (score_lower %in% c("i", "incorrect", "fail")) {
        return(0)
      }
      suppressWarnings(num <- as.numeric(score_val))
      if (!is.na(num)) {
        return(num)
      }
    }

    cli::cli_warn("Unrecognised vitals score value ({score_val}); returning NA")
    NA_real_
  }

  spec <- metric_spec_from_function(metric_fn)
  attr(metric_fn, "dsprrr_metric_spec") <- spec
  metric_fn
}
