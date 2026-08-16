#' Create an Exact Match Metric
#'
#' @description
#' Creates a metric function that checks for exact string match between
#' predicted and expected values. Can optionally extract a specific field
#' from structured outputs.
#'
#' @param field Optional field name to extract from structured outputs
#' @param ignore_case Logical, whether to ignore case when comparing
#' @param normalize Logical, whether to normalize whitespace
#'
#' @return A function with signature function(prediction, expected) -> logical
#' @export
#' @examples
#' # Simple exact match
#' metric <- metric_exact_match()
#' metric("hello", "hello")  # TRUE
#' metric("hello", "world")  # FALSE
#'
#' # Field extraction for structured outputs
#' metric <- metric_exact_match(field = "sentiment")
#' metric(list(sentiment = "positive"), list(sentiment = "positive"))  # TRUE
#'
#' # Case insensitive matching
#' metric <- metric_exact_match(ignore_case = TRUE)
#' metric("Hello", "hello")  # TRUE
metric_exact_match <- function(
  field = NULL,
  ignore_case = FALSE,
  normalize = TRUE
) {
  fn <- function(prediction, expected) {
    # Extract field if specified
    if (!is.null(field)) {
      prediction <- extract_field(prediction, field)
      expected <- extract_field(expected, field)
    }

    # Convert to character for comparison
    pred_str <- as.character(prediction)
    exp_str <- as.character(expected)

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

#' Create an F1 Score Metric
#'
#' @description
#' Creates a metric function that calculates the F1 score between predicted
#' and expected text based on token overlap.
#'
#' @param field Optional field name to extract from structured outputs
#' @param normalize Logical, whether to normalize text before tokenization
#'
#' @return A function with signature function(prediction, expected) -> numeric
#' @export
#' @examples
#' # Token-based F1 score
#' metric <- metric_f1()
#' metric("the quick brown fox", "the fast brown fox")  # 0.75
#'
#' # Field extraction
#' metric <- metric_f1(field = "answer")
#' metric(
#'   list(answer = "the quick brown fox"),
#'   list(answer = "the fast brown fox")
#' )
metric_f1 <- function(field = NULL, normalize = TRUE) {
  fn <- function(prediction, expected) {
    # Extract field if specified
    if (!is.null(field)) {
      prediction <- extract_field(prediction, field)
      expected <- extract_field(expected, field)
    }

    # Convert to character
    pred_str <- as.character(prediction)
    exp_str <- as.character(expected)

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

#' Create a Contains Metric
#'
#' @description
#' Creates a metric function that checks if the prediction contains
#' a specific substring or pattern.
#'
#' @param pattern The pattern to search for (can be a regex)
#' @param field Optional field name to extract from structured outputs
#' @param ignore_case Logical, whether to ignore case
#' @param fixed Logical, whether pattern is a fixed string (not regex)
#'
#' @return A function with signature function(prediction, expected) -> logical
#' @export
#' @examples
#' # Check for substring
#' metric <- metric_contains("positive", ignore_case = TRUE)
#' metric("The result is POSITIVE", NULL)  # TRUE
#'
#' # Regex pattern
#' metric <- metric_contains("\\d+", fixed = FALSE)
#' metric("The answer is 42", NULL)  # TRUE
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

#' Create a Custom Metric
#'
#' @description
#' Wrapper for creating custom metric functions with consistent interface
#' and error handling. The function may return one logical or numeric score, or
#' `list(score = , feedback = )` for feedback-aware optimization.
#'
#' @param fn A two-argument metric function returning one logical/numeric score
#'   or `list(score = , feedback = )`.
#' @param name Optional name for the metric (for debugging)
#'
#' @return A metric function with enhanced error handling
#' @export
#' @examples
#' # Custom length comparison
#' length_metric <- metric_custom(function(pred, exp) {
#'   nchar(as.character(pred)) == nchar(as.character(exp))
#' }, name = "length_match")
#'
#' # Custom scoring function
#' score_metric <- metric_custom(function(pred, exp) {
#'   # Return value between 0 and 1
#'   min(nchar(pred) / nchar(exp), 1)
#' })
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

#' Create a Field Equality Metric
#'
#' @description
#' Creates a metric that checks equality of multiple fields in
#' structured outputs.
#'
#' @param fields Character vector of field names to compare
#' @param require_all Logical, whether all fields must match (AND) or any (OR)
#'
#' @return A function with signature function(prediction, expected) -> logical
#' @export
#' @examples
#' # Check multiple fields
#' metric <- metric_field_match(c("sentiment", "confidence"))
#' metric(
#'   list(sentiment = "positive", confidence = 0.9),
#'   list(sentiment = "positive", confidence = 0.9)
#' )  # TRUE
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

#' Create a Metric with Textual Feedback
#'
#' @description
#' Wraps a metric function so it can return both a numeric score and
#' textual feedback explaining the score. Feedback-aware optimizers such
#' as [GEPA] use this feedback to guide their reflection step, mirroring
#' DSPy's GEPA feedback-metric protocol.
#'
#' The wrapped function must return either:
#' - a single numeric (or logical) score, or
#' - a list with elements `score` (numeric or logical) and optionally
#'   `feedback` (a single character string).
#'
#' Feedback metrics work everywhere ordinary metrics do: [evaluate()] and
#' optimizers simply use the `score` element. Optimizers that understand
#' feedback additionally collect the `feedback` strings.
#'
#' @param fn A function with signature `function(prediction, expected)`
#'   returning a score or a `list(score = , feedback = )`.
#' @param field Optional name of the column in training data containing the
#'   expected output (stored as the metric's `field` attribute, like other
#'   built-in metrics).
#'
#' @return A metric function classed as `dsprrr_feedback_metric`.
#' @export
#' @examples
#' metric <- metric_with_feedback(
#'   function(prediction, expected) {
#'     if (identical(prediction, expected)) {
#'       list(score = 1, feedback = "Correct.")
#'     } else {
#'       list(
#'         score = 0,
#'         feedback = paste0("Expected '", expected, "' but got '", prediction, "'.")
#'       )
#'     }
#'   },
#'   field = "answer"
#' )
#' metric("4", "4")
#' metric("5", "4")
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

#' Create a Trace-Aware Metric
#'
#' @description
#' Wraps a metric so it can score both what a program returned and how the
#' result was produced. Trace-aware metrics receive a third `program_trace`
#' argument containing the row and epoch identifiers, the module's ordered
#' execution events, and per-row metadata. They work with [evaluate()] and
#' every optimizer that delegates to it, including [GEPA].
#'
#' This makes quality-efficiency objectives explicit. For example, a metric can
#' penalize excessive token use, latency, iterations, or tool calls while still
#' returning textual feedback for reflective optimizers.
#'
#' @param fn A function with signature
#'   `function(prediction, expected, program_trace)`. It must return a numeric
#'   or logical score, or `list(score = , feedback = )`. An explicit
#'   `program_trace` formal (including after `...`) is matched by name;
#'   otherwise the trace is supplied as the third positional argument. Any
#'   additional formals must have defaults.
#' @param field Optional expected-output column name, stored like the `field`
#'   attribute on built-in metrics.
#'
#' @return A metric function classed as `dsprrr_trace_metric`.
#' @export
#' @examples
#' metric <- metric_with_trace(function(prediction, expected, program_trace) {
#'   correct <- identical(prediction$answer, expected$answer)
#'   tokens <- program_trace$metadata$total_tokens
#'   if (is.null(tokens)) tokens <- 0
#'   as.numeric(correct) - min(tokens / 10000, 0.1)
#' }, field = "answer")
#'
#' # evaluate(module, data, metric, .llm = llm)
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

#' Create a Threshold Metric
#'
#' @description
#' Wraps a metric to return TRUE/FALSE based on a threshold. Logical, numeric,
#' feedback, and trace-aware metrics all use the package-wide metric protocol;
#' feedback and trace dispatch are preserved by the wrapper.
#'
#' @param metric A metric function returning a logical/numeric score or
#'   `list(score = , feedback = )`.
#' @param threshold The threshold value for success
#' @param comparison One of ">=", ">", "==", "<", "<="
#'
#' @return A metric function returning logical scores. Trace-aware and feedback
#'   protocols are preserved when present on `metric`.
#' @export
#' @examples
#' # F1 score with threshold
#' metric <- metric_threshold(metric_f1(), threshold = 0.8)
#' metric("the quick brown fox", "the fast brown fox")  # FALSE (0.75 < 0.8)
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
