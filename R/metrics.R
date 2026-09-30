#' Exact-match metric
#'
#' @description
#' `metric_exact_match()` makes a metric that is `TRUE` when the prediction
#' equals the expected value, compared as text, and `FALSE` otherwise.
#'
#' @param field Name of the output field to compare. `evaluate()`,
#'   `optimize_grid()` and `compile()` pass the whole data row as `expected`;
#'   when `field` is `NULL`, the metric compares the one prediction field that
#'   is also a column of that row, and errors if there is not exactly one.
#' @param ignore_case If `TRUE`, compare without regard to case.
#' @param normalize If `TRUE` (the default), trim white space at both ends and
#'   collapse runs of white space before comparing.
#'
#' @return A function `function(prediction, expected)` returning `TRUE` or
#'   `FALSE`, for [evaluate()], [compile()] and the optimizers. `field` is
#'   stored in its `"field"` attribute.
#' @export
#' @family metrics
#' @examples
#' match_sentiment <- metric_exact_match(field = "sentiment")
#'
#' # How evaluate() calls it: the prediction, then the whole data row
#' row <- data.frame(text = "Great!", sentiment = "positive")
#' match_sentiment(list(sentiment = "positive"), row)
#' match_sentiment(list(sentiment = "negative"), row)
#'
#' # Called directly on two values
#' metric_exact_match(ignore_case = TRUE)("Paris ", "paris")
metric_exact_match <- function(
  field = NULL,
  ignore_case = FALSE,
  normalize = TRUE
) {
  fn <- function(prediction, expected) {
    values <- metric_values(prediction, expected, field)

    # Convert to character for comparison
    pred_str <- as.character(values$prediction)
    exp_str <- as.character(values$expected)

    # Normalize whitespace if requested
    if (normalize) {
      pred_str <- normalize_whitespace(pred_str)
      exp_str <- normalize_whitespace(exp_str)
    }

    # Case insensitive comparison if requested
    if (ignore_case) {
      pred_str <- tolower(pred_str)
      exp_str <- tolower(exp_str)
    }

    pred_str == exp_str
  }

  # Store field as attribute for use by teleprompters
  attr(fn, "field") <- field
  fn
}

#' Token-overlap F1 metric
#'
#' @description
#' `metric_f1()` makes a metric that scores how many words the prediction and
#' the expected text share, as the F1 score (harmonic mean of precision and
#' recall) of their word counts. It gives partial credit, which suits
#' free-text answers.
#'
#' @inheritParams metric_exact_match
#' @param normalize If `TRUE` (the default), lower-case both texts and replace
#'   punctuation with spaces before splitting them into words.
#'
#' @return A function `function(prediction, expected)` returning a number
#'   between 0 and 1. Two empty texts score 1. `field` is stored in its
#'   `"field"` attribute.
#' @export
#' @family metrics
#' @examples
#' f1 <- metric_f1(field = "answer")
#' row <- data.frame(question = "Where is the Louvre?", answer = "in Paris, France")
#' f1(list(answer = "Paris"), row)
#' f1(list(answer = "It is in Paris, France"), row)
metric_f1 <- function(field = NULL, normalize = TRUE) {
  fn <- function(prediction, expected) {
    values <- metric_values(prediction, expected, field)

    # Convert to character
    pred_str <- as.character(values$prediction)
    exp_str <- as.character(values$expected)

    # Normalize if requested
    if (normalize) {
      pred_str <- normalize_text(pred_str)
      exp_str <- normalize_text(exp_str)
    }

    # Tokenize
    pred_tokens <- unlist(strsplit(pred_str, "\\s+"), use.names = FALSE)
    exp_tokens <- unlist(strsplit(exp_str, "\\s+"), use.names = FALSE)
    pred_tokens <- pred_tokens[nzchar(pred_tokens)]
    exp_tokens <- exp_tokens[nzchar(exp_tokens)]

    # Handle edge cases
    if (length(pred_tokens) == 0 && length(exp_tokens) == 0) {
      return(1.0)
    }
    if (length(pred_tokens) == 0 || length(exp_tokens) == 0) {
      return(0.0)
    }

    # Count token occurrences rather than distinct vocabulary items. This is
    # the standard bag-of-words F1 contract and prevents repeated tokens from
    # being over- or under-counted.
    pred_counts <- table(pred_tokens)
    exp_counts <- table(exp_tokens)
    common_tokens <- intersect(names(pred_counts), names(exp_counts))
    num_common <- sum(pmin(
      pred_counts[common_tokens],
      exp_counts[common_tokens]
    ))

    if (num_common == 0) {
      return(0.0)
    }

    # Calculate precision and recall
    precision <- num_common / length(pred_tokens)
    recall <- num_common / length(exp_tokens)

    # Calculate F1
    f1 <- 2 * precision * recall / (precision + recall)
    f1
  }

  # Store field as attribute for use by teleprompters
  attr(fn, "field") <- field
  fn
}

#' Metric that checks for a pattern in the output
#'
#' @description
#' `metric_contains()` makes a metric that is `TRUE` when the prediction
#' contains `pattern`. The pattern is fixed when you create the metric: the
#' metric ignores its `expected` argument, so every row is checked for the
#' same pattern. To compare against a column of the data, use
#' [metric_exact_match()], [metric_f1()] or a custom metric.
#'
#' @param pattern The text to look for; a regular expression when
#'   `fixed = FALSE`.
#' @param field The output field to search. Give it for outputs with more than
#'   one field; with `NULL`, the whole prediction is searched.
#' @param ignore_case If `TRUE`, ignore case.
#' @param fixed If `TRUE` (the default), match `pattern` as plain text.
#'
#' @return A function `function(prediction, expected = NULL)` returning
#'   `TRUE` or `FALSE`. `field` is stored in its `"field"` attribute.
#' @export
#' @family metrics
#' @examples
#' cites_source <- metric_contains("[source]", field = "answer")
#' cites_source(list(answer = "Paris [source]"))
#' cites_source(list(answer = "Paris"))
#'
#' # The expected value is ignored
#' cites_source(list(answer = "Paris [source]"), data.frame(answer = "Lyon"))
#'
#' # A regular expression
#' has_number <- metric_contains("[0-9]+", field = "answer", fixed = FALSE)
#' has_number(list(answer = "The answer is 42"))
metric_contains <- function(
  pattern,
  field = NULL,
  ignore_case = FALSE,
  fixed = TRUE
) {
  fn <- function(prediction, expected = NULL) {
    # Extract field if specified
    if (!is.null(field)) {
      prediction <- extract_field(prediction, field)
    }

    # Convert to character
    pred_str <- as.character(prediction)

    # Search for pattern
    if (fixed && ignore_case) {
      # For fixed strings with case-insensitive matching, convert both to lower
      grepl(tolower(pattern), tolower(pred_str), fixed = TRUE)
    } else if (fixed) {
      # For fixed strings with case-sensitive matching
      grepl(pattern, pred_str, fixed = TRUE)
    } else {
      # For regex patterns (can use ignore.case)
      grepl(pattern, pred_str, ignore.case = ignore_case, fixed = FALSE)
    }
  }

  # Store field as attribute for use by teleprompters
  attr(fn, "field") <- field
  fn
}

#' Wrap a custom metric function
#'
#' @description
#' `metric_custom()` wraps your own scoring function so that its errors name
#' the metric and its numeric scores stay between 0 and 1. Any function
#' `function(prediction, expected)` already works as a metric; the wrapper
#' only adds these checks.
#'
#' @param fn A function called as `fn(prediction, expected)`, where
#'   `prediction` is the output (a named list) and `expected` the whole data
#'   row, as a one-row data frame. It returns one logical or numeric score,
#'   or `list(score = , feedback = )`.
#' @param name A name used in error messages and warnings.
#'
#' @return A metric function with the same return values as `fn`, except that
#'   numeric scores outside 0 to 1 are clipped to that range with a warning,
#'   and errors are re-raised with the metric's name.
#' @export
#' @family metrics
#' @examples
#' # Partial credit for answers that are close in length
#' length_ratio <- metric_custom(
#'   function(prediction, expected) {
#'     nchar(prediction$answer) / nchar(expected$answer)
#'   },
#'   name = "length_ratio"
#' )
#' row <- data.frame(question = "Capital of France?", answer = "Paris")
#' length_ratio(list(answer = "Pa"), row)
#' length_ratio(list(answer = "Paris, France"), row)
metric_custom <- function(fn, name = NULL) {
  if (!is.function(fn)) {
    cli::cli_abort("fn must be a function")
  }

  metric_name <- name %||% "custom_metric"

  function(prediction, expected) {
    tryCatch(
      {
        result <- fn(prediction, expected)

        normalized <- normalize_metric_result(result)
        score <- normalized$score

        # Ensure numeric metrics are in [0, 1]
        if (!is.na(score) && (score < 0 || score > 1)) {
          cli::cli_warn(c(
            "Metric {.fn {metric_name}} returned value outside [0, 1]",
            "i" = "Value: {score}"
          ))
          score <- max(0, min(1, score))
        }

        if (is.list(result)) {
          return(list(score = score, feedback = normalized$feedback))
        }
        if (is.logical(result)) {
          return(as.logical(score))
        }
        score
      },
      error = function(e) {
        cli::cli_abort(
          c(
            paste0("Error in metric ", metric_name),
            "x" = e$message
          ),
          parent = e
        )
      }
    )
  }
}

#' Metric that compares several output fields
#'
#' @description
#' `metric_field_match()` makes a metric that compares several fields of the
#' prediction with the columns of the same names in the expected row.
#' Values must be exactly equal (numbers may differ in storage type only).
#'
#' @param fields Names of the fields to compare.
#' @param require_all If `TRUE` (the default), every field must match; if
#'   `FALSE`, one match is enough.
#'
#' @return A function `function(prediction, expected)` returning `TRUE` or
#'   `FALSE`. A field missing from either side is an error.
#' @export
#' @family metrics
#' @examples
#' both <- metric_field_match(c("city", "country"))
#' row <- data.frame(question = "Where is the Louvre?", city = "Paris", country = "France")
#' both(list(city = "Paris", country = "France"), row)
#' both(list(city = "Paris", country = "Belgium"), row)
#'
#' either <- metric_field_match(c("city", "country"), require_all = FALSE)
#' either(list(city = "Paris", country = "Belgium"), row)
metric_field_match <- function(fields, require_all = TRUE) {
  if (!is.character(fields) || length(fields) == 0) {
    cli::cli_abort("fields must be a non-empty character vector")
  }

  function(prediction, expected) {
    matches <- vapply(
      fields,
      function(field) {
        pred_val <- extract_field(prediction, field)
        exp_val <- extract_field(expected, field)
        # Ignore integer-vs-double storage mode, but keep names, dimensions,
        # and values exact. Optimizer metrics must not silently accept nearby
        # numerics or differently labelled structures.
        isTRUE(all.equal(pred_val, exp_val, tolerance = 0))
      },
      logical(1)
    )

    if (require_all) {
      all(matches)
    } else {
      any(matches)
    }
  }
}

# Helper functions (internal)

#' Extract field from potentially nested structure
#' @noRd
extract_field <- function(x, field) {
  if (is.null(field)) {
    return(x)
  }

  if (is.list(x) || is.environment(x)) {
    present <- if (is.environment(x)) {
      exists(field, envir = x, inherits = FALSE)
    } else {
      field %in% names(x)
    }
    if (!present) {
      cli::cli_abort(
        "Requested metric field {.field {field}} is missing",
        class = "dsprrr_metric_field_error"
      )
    }
    x[[field]]
  } else {
    cli::cli_abort(
      c(
        "Cannot extract field from non-list object",
        "i" = "Object class: {.cls {class(x)}}"
      ),
      class = "dsprrr_metric_field_error"
    )
  }
}

#' Resolve the values a field-aware metric compares
#'
#' `evaluate()`, `optimize_grid()`, `compile()` and `eval_program()` pass the
#' whole data row as `expected`. With no explicit `field`, compare the single
#' prediction field that is also a column of that row. Values that are not a
#' data row (direct calls such as `metric("a", "a")`) are compared as given.
#' @noRd
metric_values <- function(prediction, expected, field = NULL) {
  if (is.null(field) && is.data.frame(expected)) {
    field <- infer_metric_field(prediction, expected)
  }
  if (is.null(field)) {
    return(list(prediction = prediction, expected = expected))
  }
  list(
    prediction = extract_field(prediction, field),
    expected = extract_field(expected, field)
  )
}

#' @noRd
infer_metric_field <- function(prediction, expected) {
  shared <- if (is.list(prediction) && !is.null(names(prediction))) {
    intersect(names(prediction), names(expected))
  } else {
    character()
  }
  if (length(shared) == 1) {
    return(shared)
  }
  hint <- "Pass {.arg field} to name the column that holds the expected answer."
  if (length(shared) > 1) {
    cli::cli_abort(
      c(
        "Can't tell which field this metric should compare.",
        "i" = "The prediction and the data share {.field {shared}}.",
        "i" = hint
      ),
      class = "dsprrr_metric_field_error"
    )
  }
  cli::cli_abort(
    c(
      "Can't tell which field this metric should compare.",
      "i" = "No prediction field matches a column of the data ({.field {names(expected)}}).",
      "i" = hint
    ),
    class = "dsprrr_metric_field_error"
  )
}

#' Normalize whitespace in text
#' @noRd
normalize_whitespace <- function(text) {
  text <- trimws(text)
  gsub("\\s+", " ", text)
}

#' Normalize text for comparison (removes punctuation, lowercases)
#' @noRd
normalize_text <- function(text) {
  text <- tolower(text)
  text <- gsub("[[:punct:]]", " ", text)
  text <- normalize_whitespace(text)
  text
}

#' Extract field attribute from a metric function
#'
#' @description
#' Retrieves the field attribute that was stored on a metric function
#' when it was created. This is used by teleprompters to determine
#' which column in the training data contains the expected output.
#'
#' @param metric A metric function (e.g., from `metric_exact_match()`)
#' @return The field name as a character string, or NULL if not set
#' @noRd
get_metric_field <- function(metric) {
  if (is.null(metric)) {
    return(NULL)
  }
  if (!is.function(metric)) {
    cli::cli_warn(c(
      "Expected metric to be a function",
      "i" = "Got: {.cls {class(metric)}}",
      "!" = "Falling back to auto-detection for output column"
    ))
    return(NULL)
  }
  attr(metric, "field")
}

#' Metric that returns a score and feedback
#'
#' @description
#' `metric_with_feedback()` marks a metric whose function returns textual
#' feedback along with its score, as
#' `list(score = , feedback = "what went wrong")`. Feedback-aware optimizers
#' such as [GEPA()] use the feedback to guide their reflection step, as in
#' DSPy's GEPA. Everywhere else, including [evaluate()], only the score is
#' used; `evaluate()` also returns the feedback in `feedbacks`.
#'
#' @param fn A function called as `fn(prediction, expected)`, where
#'   `prediction` is the output (a named list) and `expected` the whole data
#'   row, as a one-row data frame. It returns a logical or numeric score, or
#'   `list(score = , feedback = )` with `feedback` a single string.
#' @param field The name of the data column that holds the expected output.
#'   It is only stored, in the metric's `"field"` attribute, for optimizers
#'   that look it up; `fn` still receives the whole row and prediction.
#'
#' @return A metric function of class `dsprrr_feedback_metric`.
#' @export
#' @family metrics
#' @examples
#' graded <- metric_with_feedback(
#'   function(prediction, expected) {
#'     if (identical(prediction$answer, expected$answer)) {
#'       list(score = 1, feedback = "Correct.")
#'     } else {
#'       list(
#'         score = 0,
#'         feedback = paste0("Expected '", expected$answer, "' but got '", prediction$answer, "'.")
#'       )
#'     }
#'   },
#'   field = "answer"
#' )
#' row <- data.frame(question = "What is 2 + 2?", answer = "4")
#' graded(list(answer = "4"), row)
#' graded(list(answer = "5"), row)
metric_with_feedback <- function(fn, field = NULL) {
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function")
  }
  if (!is.null(field) && (!is.character(field) || length(field) != 1)) {
    cli::cli_abort("{.arg field} must be a single character string or NULL")
  }

  metric <- function(prediction, expected) {
    fn(prediction, expected)
  }
  attr(metric, "field") <- field
  class(metric) <- c("dsprrr_feedback_metric", class(metric))
  metric
}

#' Check whether a metric is feedback-aware
#' @noRd
is_feedback_metric <- function(metric) {
  inherits(metric, "dsprrr_feedback_metric")
}

#' Metric that also sees the execution trace
#'
#' @description
#' `metric_with_trace()` makes a metric that scores both what a program
#' returned and how it got there. Besides the prediction and the expected
#' row, the function receives a `program_trace`: the row and epoch numbers,
#' a `status` (`"ok"`, `"error"` or `"untraced"`), the module's execution
#' `events` in order, and the row's call `metadata` (tokens, cost, latency).
#' Use it to penalize token use, latency, iterations or tool calls alongside
#' correctness.
#'
#' Trace-aware metrics work with [evaluate()] and the optimizers that use it,
#' including [GEPA()]. Called directly without a trace, they are an error.
#'
#' @param fn A function called as `fn(prediction, expected, program_trace)`.
#'   It returns a logical or numeric score, or `list(score = , feedback = )`.
#'   A formal argument named `program_trace` receives the trace by name
#'   (even after `...`); otherwise the trace is the third positional
#'   argument. Any other arguments need defaults.
#' @param field The name of the data column that holds the expected output,
#'   stored in the metric's `"field"` attribute for optimizers.
#'
#' @return A metric function of class `dsprrr_trace_metric`.
#' @export
#' @family metrics
#' @examples
#' # Correctness, minus up to 0.1 for token use
#' efficient <- metric_with_trace(
#'   function(prediction, expected, program_trace) {
#'     correct <- identical(prediction$answer, expected$answer)
#'     tokens <- program_trace$metadata$total_tokens
#'     if (is.null(tokens) || is.na(tokens)) tokens <- 0
#'     as.numeric(correct) - min(tokens / 10000, 0.1)
#'   },
#'   field = "answer"
#' )
#'
#' # A function-backed module records no tokens, so only correctness counts
#' rule <- module_fn("question -> answer", function(question) "4")
#' quiz <- data.frame(question = c("2 + 2?", "3 + 3?"), answer = c("4", "6"))
#' evaluate(rule, quiz, metric = efficient)$scores
metric_with_trace <- function(fn, field = NULL) {
  if (!is.function(fn)) {
    cli::cli_abort("{.arg fn} must be a function")
  }
  if (
    !is.null(field) &&
      (!is.character(field) || length(field) != 1L || is.na(field))
  ) {
    cli::cli_abort("{.arg field} must be a single character string or NULL")
  }

  metric_formals <- formals(fn)
  accepts_trace <- !is.null(metric_formals) &&
    ("..." %in% names(metric_formals) || length(metric_formals) >= 3L)
  if (!accepts_trace) {
    cli::cli_abort(
      c(
        "{.arg fn} must accept a third {.arg program_trace} argument",
        "i" = "Use {.code function(prediction, expected, program_trace) ...}."
      ),
      class = "dsprrr_trace_metric_signature_error"
    )
  }

  formal_names <- names(metric_formals)
  named_trace <- "program_trace" %in% formal_names
  ellipsis_position <- match(
    "...",
    formal_names,
    nomatch = length(formal_names) + 1L
  )
  positional_formals <- seq_len(ellipsis_position - 1L)
  if (named_trace) {
    positional_formals <- positional_formals[
      formal_names[positional_formals] != "program_trace"
    ]
  }
  supplied <- rep(FALSE, length(metric_formals))
  if (named_trace) {
    supplied[formal_names == "program_trace"] <- TRUE
  }
  positional_count <- if (named_trace) 2L else 3L
  supplied[utils::head(positional_formals, positional_count)] <- TRUE
  required <- vapply(metric_formals, rlang::is_missing, logical(1))
  unsupplied <- formal_names[required & !supplied & formal_names != "..."]
  if (length(unsupplied) > 0L) {
    cli::cli_abort(
      c(
        "{.arg fn} has required arguments the trace metric cannot supply",
        "x" = "Unsupplied argument{?s}: {.arg {unsupplied}}",
        "i" = "The callback receives prediction, expected, and program_trace."
      ),
      class = "dsprrr_trace_metric_signature_error"
    )
  }

  metric <- function(prediction, expected, program_trace) {
    if (!inherits(program_trace, "dsprrr_program_trace")) {
      cli::cli_abort(
        "{.arg program_trace} must be supplied by {.fn evaluate}",
        class = "dsprrr_program_trace_error"
      )
    }
    if ("program_trace" %in% names(metric_formals)) {
      do.call(
        fn,
        c(
          list(prediction, expected),
          list(
            program_trace = program_trace
          )
        )
      )
    } else {
      do.call(fn, list(prediction, expected, program_trace))
    }
  }
  attr(metric, "field") <- field
  class(metric) <- c("dsprrr_trace_metric", class(metric))
  metric
}

#' Check whether a metric requests a program trace
#' @noRd
is_trace_metric <- function(metric) {
  inherits(metric, "dsprrr_trace_metric")
}

#' Build the stable trace envelope passed to trace-aware metrics
#' @noRd
new_program_trace <- function(events = list(), metadata, row_id, epoch) {
  if (is.null(events)) {
    events <- list()
  } else if (!is.list(events)) {
    cli::cli_abort(
      "Internal program trace events must be a list",
      class = "dsprrr_program_trace_contract_error"
    )
  }
  if (is.null(metadata)) {
    metadata <- list()
  } else if (!is.list(metadata)) {
    metadata <- list(value = metadata)
  }

  error <- metadata$error %||% NA_character_
  failed <- length(error) > 0L && !is.na(error[[1L]]) && nzchar(error[[1L]])
  status <- if (failed) {
    "error"
  } else if (length(events) == 0L) {
    "untraced"
  } else {
    "ok"
  }

  structure(
    list(
      row_id = as.integer(row_id),
      epoch = as.integer(epoch),
      status = status,
      events = events,
      program_artifact_id = current_trace_program_artifact_id(),
      trace_context = current_trace_context(),
      metadata = trace_context_annotate_metadata(metadata)
    ),
    class = c("dsprrr_program_trace", "list")
  )
}

#' Invoke a metric without changing the ordinary two-argument protocol
#' @noRd
invoke_metric <- function(metric, prediction, expected, program_trace) {
  if (is_trace_metric(metric)) {
    metric(prediction, expected, program_trace)
  } else {
    metric(prediction, expected)
  }
}

#' Normalize a raw metric return value to score + feedback
#'
#' Accepts numeric, logical, or `list(score = , feedback = )` returns and
#' produces a consistent `list(score = <numeric>, feedback = <character>)`.
#' Feedback is `NA_character_` when not supplied.
#'
#' @noRd
normalize_metric_result <- function(raw) {
  if (is.list(raw)) {
    if (!"score" %in% names(raw)) {
      cli::cli_abort(c(
        "Metric returned a list without a {.field score} element",
        "i" = "Feedback metrics must return {.code list(score = , feedback = )}"
      ))
    }
    score <- raw$score
    feedback <- raw$feedback
  } else {
    score <- raw
    feedback <- NULL
  }

  if (is.logical(score)) {
    score <- as.numeric(score)
  }
  if (!is.numeric(score) || length(score) != 1) {
    cli::cli_abort(c(
      "Metric must return a single logical or numeric score",
      "i" = "Got {.cls {class(score)[1]}} of length {length(score)}"
    ))
  }

  if (is.null(feedback) || (length(feedback) == 1 && is.na(feedback))) {
    feedback <- NA_character_
  } else if (!is.character(feedback) || length(feedback) != 1) {
    cli::cli_abort(c(
      "Metric feedback must be a single character string",
      "i" = "Got {.cls {class(feedback)[1]}} of length {length(feedback)}"
    ))
  }

  list(score = score, feedback = feedback)
}

#' Turn a numeric metric into pass/fail
#'
#' @description
#' `metric_threshold()` wraps a metric so that it returns `TRUE` when the
#' score passes `threshold` and `FALSE` otherwise. Use it where a yes/no
#' judgement is needed, for example to count only answers with an F1 score
#' of at least 0.8 as correct.
#'
#' @param metric A metric returning a logical or numeric score, or
#'   `list(score = , feedback = )`.
#' @param threshold The score to compare against.
#' @param comparison How to compare the score with `threshold`: one of
#'   `">="` (the default), `">"`, `"=="`, `"<"` or `"<="`.
#'
#' @return A metric returning `TRUE` or `FALSE`. If `metric` returns
#'   feedback, the result is `list(score = TRUE/FALSE, feedback = )`. The
#'   `field` attribute and trace-aware and feedback classes of `metric` are
#'   kept.
#' @export
#' @family metrics
#' @examples
#' f1 <- metric_f1(field = "answer")
#' good_enough <- metric_threshold(f1, threshold = 0.8)
#' row <- data.frame(question = "Where is the Louvre?", answer = "The Louvre is in Paris")
#'
#' f1(list(answer = "In Paris"), row)
#' good_enough(list(answer = "In Paris"), row)
#' good_enough(list(answer = "The Louvre is in Paris, France"), row)
metric_threshold <- function(metric, threshold = 0.5, comparison = ">=") {
  if (!is.function(metric)) {
    cli::cli_abort("metric must be a function")
  }
  if (
    !is.numeric(threshold) ||
      length(threshold) != 1L ||
      is.na(threshold) ||
      !is.finite(threshold)
  ) {
    cli::cli_abort(c(
      "threshold must be a single numeric value",
      "x" = "Missing and infinite values are not supported"
    ))
  }

  comparison <- match.arg(comparison, c(">=", ">", "==", "<", "<="))

  threshold_metric <- function(prediction, expected, program_trace = NULL) {
    normalized <- normalize_metric_result(
      invoke_metric(metric, prediction, expected, program_trace)
    )
    score <- normalized$score

    result <- switch(
      comparison,
      ">=" = score >= threshold,
      ">" = score > threshold,
      "==" = score == threshold,
      "<" = score < threshold,
      "<=" = score <= threshold
    )

    if (!is.na(normalized$feedback)) {
      return(list(score = result, feedback = normalized$feedback))
    }
    result
  }

  attr(threshold_metric, "field") <- get_metric_field(metric)
  if (is_trace_metric(metric)) {
    class(threshold_metric) <- c(
      "dsprrr_trace_metric",
      class(threshold_metric)
    )
  }
  if (is_feedback_metric(metric)) {
    class(threshold_metric) <- c(
      "dsprrr_feedback_metric",
      class(threshold_metric)
    )
  }
  threshold_metric
}
