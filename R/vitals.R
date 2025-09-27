#' Convert a dsprrr module into a vitals solver
#'
#' @description
#' Creates a function compatible with vitals Tasks that executes a DSPrrr
#' module against batches of inputs. The solver forwards arguments to
#' [run_dataset()] and returns vitals-friendly objects containing results,
#' chat logs, and metadata.
#'
#' @param module A DSPrrr module (e.g., created via [module()]).
#' @param .llm Optional ellmer chat object. When `NULL`, each invocation will
#'   create a fresh default client.
#' @param .parallel Logical; forwarded to [run_dataset()]. Defaults to `FALSE`
#'   to avoid sharing LLM state across workers.
#' @param .return_format One of `"structured"` (default) or `"simple"`.
#' @param ... Additional arguments forwarded to [run_dataset()].
#'
#' @return A function accepting a data frame of inputs and returning a list with
#'   components `result`, `solver_chat`, and `metadata`.
#' @export
as_vitals_solver <- function(module, .llm = NULL, .parallel = FALSE,
                             .return_format = "structured", ...) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("as_vitals_solver() requires an R6 Module object")
  }

  .return_format <- match.arg(.return_format, c("simple", "structured"))

  function(inputs, ...) {
    if (!is.data.frame(inputs)) {
      inputs <- as.data.frame(inputs)
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
#' Converts a vitals scorer function into a per-example metric compatible with
#' DSPrrr compilation and evaluation. The scorer is invoked on a single-row
#' tibble constructed from the prediction and the expected row.
#'
#' @param vitals_scorer A function that accepts a tibble/data frame and returns
#'   a tibble with a `score` column (following vitals conventions).
#' @param input_column Name of the column to populate with the example input.
#'   If the column is absent in `expected_row`, `NA` is supplied.
#' @param target_column Name of the column holding the ground-truth label inside
#'   the vitals sample tibble.
#' @param result_column Name of the column that receives the model prediction.
#'
#' @return A metric function with signature `function(prediction, expected_row)`
#'   returning numeric values in `[0, 1]` or `NA` when the scorer output cannot
#'   be interpreted.
#' @export
as_dsprrr_metric <- function(vitals_scorer,
                             input_column = "input",
                             target_column = "target",
                             result_column = "result") {
  if (!is.function(vitals_scorer)) {
    cli::cli_abort("vitals_scorer must be a function")
  }

  function(prediction, expected_row) {
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
}
