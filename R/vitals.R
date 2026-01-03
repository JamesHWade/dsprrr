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
as_vitals_solver <- function(
  module,
  .llm = NULL,
  .parallel = FALSE,
  .return_format = "structured",
  ...
) {
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
as_dsprrr_metric <- function(
  vitals_scorer,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  if (!is.function(vitals_scorer)) {
    cli::cli_abort("vitals_scorer must be a function")
  }

  function(prediction, expected_row) {
    sample <- tibble::tibble(
      !!input_column := list(
        if (input_column %in% names(expected_row)) {
          expected_row[[input_column]]
        } else {
          NA
        }
      ),
      !!target_column := list(
        if (target_column %in% names(expected_row)) {
          expected_row[[target_column]]
        } else {
          NA
        }
      ),
      !!result_column := list(prediction)
    )

    scores <- vitals_scorer(sample)

    # Handle both list (vitals native) and data.frame (mock) return formats
    if (is.list(scores) && !is.data.frame(scores)) {
      # vitals scorers return a list with $score element
      if (is.null(scores$score)) {
        cli::cli_warn("vitals scorer returned no score; treating as NA")
        return(NA_real_)
      }
      score_val <- scores$score
    } else if (is.data.frame(scores) && nrow(scores) > 0) {
      # Mock scorers return a data.frame with score column
      score_val <- scores$score[[1]]
    } else {
      cli::cli_warn("vitals scorer returned no results; treating as NA")
      return(NA_real_)
    }

    if (is.numeric(score_val)) {
      return(score_val)
    }

    if (is.logical(score_val)) {
      return(as.numeric(score_val))
    }

    # Handle factor (vitals uses ordered factors like "C", "I", "P")
    if (is.factor(score_val)) {
      score_val <- as.character(score_val)
    }

    if (is.character(score_val)) {
      score_lower <- tolower(score_val)
      if (score_lower %in% c("c", "correct", "pass")) {
        return(1)
      }
      if (score_lower %in% c("i", "incorrect", "fail")) {
        return(0)
      }
      if (score_lower %in% c("p", "partial")) {
        return(0.5)
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

#' Pre-built Vitals-backed Metrics
#'
#' @description
#' These functions wrap common vitals scorers for direct use as dsprrr metrics,
#' eliminating the need to manually call `as_dsprrr_metric()`.
#'
#' @name vitals_metrics
#' @param template Grading template (glue string with `input`, `answer`,
#'   `criterion`, `instructions` substitutions)
#' @param instructions Grading instructions
#' @param grade_pattern Regex pattern to extract grade from judge response
#' @param partial_credit Whether to allow partial credit
#' @param scorer_chat An ellmer chat for grading (e.g., `ellmer::chat_openai()`)
#' @param input_column Column name for input in vitals sample
#' @param target_column Column name for target in vitals sample
#' @param result_column Column name for result in vitals sample
#'
#' @return A metric function with signature `function(prediction, expected_row)`
#' @export
#'
#' @examples
#' \dontrun{
#' # Model-graded QA metric
#' metric <- metric_model_graded_qa(scorer_chat = ellmer::chat_openai())
#' score <- metric("Paris", data.frame(target = "Paris"))
#'
#' # With custom grading chat
#' metric <- metric_model_graded_fact(
#'   scorer_chat = ellmer::chat_claude(),
#'   partial_credit = TRUE
#' )
#' }
metric_model_graded_qa <- function(
  template = NULL,
  instructions = NULL,

  grade_pattern = "(?i)GRADE\\s*:\\s*([CPI])(.*)$",
  partial_credit = FALSE,
  scorer_chat = NULL,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for model_graded_qa scorer")

  scorer <- vitals::model_graded_qa(
    template = template,
    instructions = instructions,
    grade_pattern = grade_pattern,
    partial_credit = partial_credit,
    scorer_chat = scorer_chat
  )

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}

#' @rdname vitals_metrics
#' @export
metric_model_graded_fact <- function(
  template = NULL,
  instructions = NULL,
  grade_pattern = "(?i)GRADE\\s*:\\s*([CPI])(.*)$",
  partial_credit = FALSE,
  scorer_chat = NULL,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for model_graded_fact scorer")

  scorer <- vitals::model_graded_fact(
    template = template,
    instructions = instructions,
    grade_pattern = grade_pattern,
    partial_credit = partial_credit,
    scorer_chat = scorer_chat
  )

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}

#' @rdname vitals_metrics
#' @param location Where to look for the target in the result: "end", "begin",
#'   "any", or "exact"
#' @param case_sensitive Whether matching is case-sensitive
#' @export
#' @examples
#' \dontrun{
#' # String detection metrics
#' metric <- metric_detect_match(location = "end")
#' metric("The answer is Paris", data.frame(target = "Paris"))  # 1
#'
#' metric <- metric_detect_includes()
#' metric("Paris is the capital", data.frame(target = "Paris"))  # 1
#' }
metric_detect_match <- function(
  location = c("end", "begin", "any", "exact"),
  case_sensitive = FALSE,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for detect_match scorer")
  location <- match.arg(location)

  scorer <- vitals::detect_match(
    location = location,
    case_sensitive = case_sensitive
  )

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}

#' @rdname vitals_metrics
#' @export
metric_detect_includes <- function(
  case_sensitive = FALSE,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for detect_includes scorer")

  scorer <- vitals::detect_includes(case_sensitive = case_sensitive)

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}

#' @rdname vitals_metrics
#' @param pattern Regex pattern with capture groups. The captured groups are
#'   extracted from the result and checked against the target. Use parentheses
#'   to define capture groups, e.g., `"([0-9]+)"` to extract numbers.
#' @param all Whether all captured groups must match the target (TRUE) or
#'   just one (FALSE, default).
#' @export
metric_detect_pattern <- function(
  pattern,
  case_sensitive = FALSE,
  all = FALSE,
  input_column = "input",
  target_column = "target",
  result_column = "result"
) {
  rlang::check_installed("vitals", reason = "for detect_pattern scorer")

  scorer <- vitals::detect_pattern(
    pattern = pattern,
    case_sensitive = case_sensitive,
    all = all
  )

  as_dsprrr_metric(
    scorer,
    input_column = input_column,
    target_column = target_column,
    result_column = result_column
  )
}
