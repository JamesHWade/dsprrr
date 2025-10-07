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
metric_exact_match <- function(field = NULL, ignore_case = FALSE, normalize = TRUE) {
  function(prediction, expected) {
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
  function(prediction, expected) {
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
    pred_tokens <- unlist(strsplit(pred_str, "\\s+"))
    exp_tokens <- unlist(strsplit(exp_str, "\\s+"))

    # Calculate overlap
    common <- intersect(pred_tokens, exp_tokens)
    num_common <- length(common)

    # Handle edge cases
    if (length(pred_tokens) == 0 && length(exp_tokens) == 0) {
      return(1.0)
    }
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
metric_contains <- function(pattern, field = NULL, ignore_case = FALSE, fixed = TRUE) {
  function(prediction, expected = NULL) {
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
}

#' Create a Custom Metric
#'
#' @description
#' Wrapper for creating custom metric functions with consistent interface
#' and error handling.
#'
#' @param fn A function with signature function(prediction, expected) -> numeric/logical
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
    tryCatch({
      result <- fn(prediction, expected)

      # Validate result
      if (!is.logical(result) && !is.numeric(result)) {
        cli::cli_abort(c(
          "Metric {.fn {metric_name}} must return logical or numeric value",
          "x" = "Got {.cls {class(result)}}"
        ))
      }

      # Ensure numeric metrics are in [0, 1]
      if (is.numeric(result)) {
        if (result < 0 || result > 1) {
          cli::cli_warn(c(
            "Metric {.fn {metric_name}} returned value outside [0, 1]",
            "i" = "Value: {result}"
          ))
          result <- max(0, min(1, result))
        }
      }

      result
    }, error = function(e) {
      cli::cli_abort(c(
        paste0("Error in metric ", metric_name),
        "x" = e$message
      ), parent = e)
    })
  }
}

# Metric specifications ---------------------------------------------------

new_metric_spec <- function(per_example = NULL, aggregate = NULL, metadata = list()) {
  structure(
    list(
      per_example = per_example,
      aggregate = aggregate,
      metadata = metadata
    ),
    class = "dsprrr_metric_spec"
  )
}

is_metric_spec <- function(x) {
  inherits(x, "dsprrr_metric_spec")
}

metric_spec_from_function <- function(fn) {
  new_metric_spec(
    per_example = fn,
    aggregate = function(scores, predictions, dataset, metadata, errors, ...) {
      list(
        mean_score = if (length(scores)) mean(scores, na.rm = TRUE) else NA_real_,
        scores = scores,
        metrics = tibble::tibble(),
        n_evaluated = sum(!is.na(scores)),
        n_errors = sum(is.na(scores))
      )
    },
    metadata = list(type = "per_example")
  )
}

resolve_metric_spec <- function(metric) {
  if (is_metric_spec(metric)) {
    return(metric)
  }

  spec <- attr(metric, "dsprrr_metric_spec")
  if (is_metric_spec(spec)) {
    return(spec)
  }

  if (is.function(metric)) {
    return(metric_spec_from_function(metric))
  }

  cli::cli_abort("metric must be a function, yardstick metric set, or dsprrr metric specification")
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
    matches <- vapply(fields, function(field) {
      pred_val <- extract_field(prediction, field)
      exp_val <- extract_field(expected, field)
      identical(pred_val, exp_val)
    }, logical(1))

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
    x[[field]]
  } else {
    cli::cli_abort(c(
      "Cannot extract field from non-list object",
      "i" = "Object class: {.cls {class(x)}}"
    ))
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

#' Create a Threshold Metric
#'
#' @description
#' Wraps a numeric metric to return TRUE/FALSE based on a threshold.
#'
#' @param metric A metric function that returns numeric values
#' @param threshold The threshold value for success
#' @param comparison One of ">=", ">", "==", "<", "<="
#'
#' @return A function with signature function(prediction, expected) -> logical
#' @export
#' @examples
#' # F1 score with threshold
#' metric <- metric_threshold(metric_f1(), threshold = 0.8)
#' metric("the quick brown fox", "the fast brown fox")  # FALSE (0.75 < 0.8)
metric_threshold <- function(metric, threshold = 0.5, comparison = ">=") {
  if (!is.function(metric)) {
    cli::cli_abort("metric must be a function")
  }
  if (!is.numeric(threshold) || length(threshold) != 1) {
    cli::cli_abort("threshold must be a single numeric value")
  }

  comparison <- match.arg(comparison, c(">=", ">", "==", "<", "<="))

  function(prediction, expected) {
    score <- metric(prediction, expected)

    if (!is.numeric(score)) {
      cli::cli_abort("Base metric must return numeric value for threshold comparison")
    }

    result <- switch(comparison,
      ">=" = score >= threshold,
      ">" = score > threshold,
      "==" = score == threshold,
      "<" = score < threshold,
      "<=" = score <= threshold
    )

    result
  }
}