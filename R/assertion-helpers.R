#' Assertion Helper Functions
#'
#' @description
#' Convenience functions for creating common assertion patterns.
#' These are shorthand for `assert_output()` with pre-built condition functions.
#'
#' @name assertion-helpers
NULL

#' Assert Output Length
#'
#' @description
#' Create an assertion that validates the character length of a field.
#'
#' @param field The output field to check. If NULL, checks the entire output.
#' @param min Minimum length (inclusive). Default NULL (no minimum).
#' @param max Maximum length (inclusive). Default NULL (no maximum).
#' @param type "assert" for hard assertion (default), "suggest" for soft suggestion.
#'
#' @return An Assertion object
#'
#' @export
#' @examples
#' \dontrun{
#' # Max 100 characters
#' assert_length("answer", max = 100)
#'
#' # Between 10 and 200 characters
#' assert_length("summary", min = 10, max = 200)
#'
#' # Soft suggestion for length
#' assert_length("answer", max = 50, type = "suggest")
#' }
assert_length <- function(
  field = NULL,
  min = NULL,
  max = NULL,
  type = c("assert", "suggest")
) {
  type <- match.arg(type)

  if (is.null(min) && is.null(max)) {
    cli::cli_abort("At least one of {.arg min} or {.arg max} must be specified")
  }

  # Build message
  if (!is.null(min) && !is.null(max)) {
    msg <- sprintf("Length must be between %d and %d characters", min, max)
  } else if (!is.null(min)) {
    msg <- sprintf("Length must be at least %d characters", min)
  } else {
    msg <- sprintf("Length must be at most %d characters", max)
  }

  if (!is.null(field)) {
    msg <- paste0(field, ": ", msg)
  }

  # Build condition function
  condition <- function(x) {
    value <- if (!is.null(field) && is.list(x)) x[[field]] else x
    if (is.null(value)) {
      return(FALSE)
    }

    len <- nchar(as.character(value))
    passes_min <- is.null(min) || len >= min
    passes_max <- is.null(max) || len <= max
    passes_min && passes_max
  }

  if (type == "assert") {
    assert_output(condition, msg, field = NULL) # field already handled in condition
  } else {
    suggest_output(condition, msg, field = NULL)
  }
}

#' Assert Output Contains Substring
#'
#' @description
#' Create an assertion that validates the output contains a specific substring.
#'
#' @param field The output field to check. If NULL, checks the entire output.
#' @param pattern The substring that must be present.
#' @param ignore_case Logical. If TRUE, comparison is case-insensitive. Default FALSE.
#' @param type "assert" for hard assertion (default), "suggest" for soft suggestion.
#'
#' @return An Assertion object
#'
#' @export
#' @examples
#' \dontrun{
#' # Must contain "important"
#' assert_contains("answer", "important")
#'
#' # Case-insensitive check
#' assert_contains("summary", "conclusion", ignore_case = TRUE)
#' }
assert_contains <- function(
  field = NULL,
  pattern,
  ignore_case = FALSE,
  type = c("assert", "suggest")
) {
  type <- match.arg(type)

  msg <- if (!is.null(field)) {
    sprintf("%s must contain '%s'", field, pattern)
  } else {
    sprintf("Output must contain '%s'", pattern)
  }

  condition <- function(x) {
    value <- if (!is.null(field) && is.list(x)) x[[field]] else x
    if (is.null(value)) {
      return(FALSE)
    }

    value_str <- as.character(value)
    if (ignore_case) {
      grepl(tolower(pattern), tolower(value_str), fixed = TRUE)
    } else {
      grepl(pattern, value_str, fixed = TRUE)
    }
  }

  if (type == "assert") {
    assert_output(condition, msg, field = NULL)
  } else {
    suggest_output(condition, msg, field = NULL)
  }
}

#' Assert Output Does Not Contain Substring
#'
#' @description
#' Create an assertion that validates the output does not contain a specific substring.
#'
#' @param field The output field to check. If NULL, checks the entire output.
#' @param pattern The substring that must NOT be present.
#' @param ignore_case Logical. If TRUE, comparison is case-insensitive. Default FALSE.
#' @param type "assert" for hard assertion (default), "suggest" for soft suggestion.
#'
#' @return An Assertion object
#'
#' @export
#' @examples
#' \dontrun{
#' # Must not contain profanity (simple example)
#' assert_not_contains("answer", "badword")
#'
#' # Must not reveal internal details
#' assert_not_contains("response", "internal_api_key")
#' }
assert_not_contains <- function(
  field = NULL,
  pattern,
  ignore_case = FALSE,
  type = c("assert", "suggest")
) {
  type <- match.arg(type)

  msg <- if (!is.null(field)) {
    sprintf("%s must not contain '%s'", field, pattern)
  } else {
    sprintf("Output must not contain '%s'", pattern)
  }

  condition <- function(x) {
    value <- if (!is.null(field) && is.list(x)) x[[field]] else x
    if (is.null(value)) {
      return(TRUE)
    } # NULL doesn't contain anything

    value_str <- as.character(value)
    if (ignore_case) {
      !grepl(tolower(pattern), tolower(value_str), fixed = TRUE)
    } else {
      !grepl(pattern, value_str, fixed = TRUE)
    }
  }

  if (type == "assert") {
    assert_output(condition, msg, field = NULL)
  } else {
    suggest_output(condition, msg, field = NULL)
  }
}

#' Assert Output Matches Pattern
#'
#' @description
#' Create an assertion that validates the output matches a regular expression.
#'
#' @param field The output field to check. If NULL, checks the entire output.
#' @param pattern The regular expression pattern to match.
#' @param message Optional custom error message. If NULL, generates a default.
#' @param ignore_case Logical. If TRUE, matching is case-insensitive. Default FALSE.
#' @param type "assert" for hard assertion (default), "suggest" for soft suggestion.
#'
#' @return An Assertion object
#'
#' @export
#' @examples
#' \dontrun{
#' # Must start with capital letter
#' assert_matches("answer", "^[A-Z]", "Must start with capital letter")
#'
#' # Must be a valid email format (simple check)
#' assert_matches("email", "^[^@]+@[^@]+\\.[^@]+$", "Must be valid email")
#'
#' # Must end with period
#' assert_matches("summary", "\\.$", "Must end with period")
#' }
assert_matches <- function(
  field = NULL,
  pattern,
  message = NULL,
  ignore_case = FALSE,
  type = c("assert", "suggest")
) {
  type <- match.arg(type)

  msg <- message %||%
    {
      if (!is.null(field)) {
        sprintf("%s must match pattern '%s'", field, pattern)
      } else {
        sprintf("Output must match pattern '%s'", pattern)
      }
    }

  condition <- function(x) {
    value <- if (!is.null(field) && is.list(x)) x[[field]] else x
    if (is.null(value)) {
      return(FALSE)
    }

    grepl(pattern, as.character(value), ignore.case = ignore_case, perl = TRUE)
  }

  if (type == "assert") {
    assert_output(condition, msg, field = NULL)
  } else {
    suggest_output(condition, msg, field = NULL)
  }
}

#' Assert Output Does Not Match Pattern
#'
#' @description
#' Create an assertion that validates the output does NOT match a regular expression.
#'
#' @param field The output field to check. If NULL, checks the entire output.
#' @param pattern The regular expression pattern that must NOT match.
#' @param message Optional custom error message. If NULL, generates a default.
#' @param ignore_case Logical. If TRUE, matching is case-insensitive. Default FALSE.
#' @param type "assert" for hard assertion (default), "suggest" for soft suggestion.
#'
#' @return An Assertion object
#'
#' @export
#' @examples
#' \dontrun{
#' # Must not contain URLs
#' assert_not_matches("answer", "https?://", "Must not contain URLs")
#'
#' # Must not contain code blocks
#' assert_not_matches("summary", "```", "Must not contain code blocks")
#' }
assert_not_matches <- function(
  field = NULL,
  pattern,
  message = NULL,
  ignore_case = FALSE,
  type = c("assert", "suggest")
) {
  type <- match.arg(type)

  msg <- message %||%
    {
      if (!is.null(field)) {
        sprintf("%s must not match pattern '%s'", field, pattern)
      } else {
        sprintf("Output must not match pattern '%s'", pattern)
      }
    }

  condition <- function(x) {
    value <- if (!is.null(field) && is.list(x)) x[[field]] else x
    if (is.null(value)) {
      return(TRUE)
    } # NULL doesn't match anything

    !grepl(pattern, as.character(value), ignore.case = ignore_case, perl = TRUE)
  }

  if (type == "assert") {
    assert_output(condition, msg, field = NULL)
  } else {
    suggest_output(condition, msg, field = NULL)
  }
}

#' Assert Output is One Of
#'
#' @description
#' Create an assertion that validates the output is one of a set of allowed values.
#'
#' @param field The output field to check. If NULL, checks the entire output.
#' @param values Character vector of allowed values.
#' @param ignore_case Logical. If TRUE, comparison is case-insensitive. Default FALSE.
#' @param type "assert" for hard assertion (default), "suggest" for soft suggestion.
#'
#' @return An Assertion object
#'
#' @export
#' @examples
#' \dontrun{
#' # Must be a valid sentiment
#' assert_one_of("sentiment", c("positive", "negative", "neutral"))
#'
#' # Case-insensitive check
#' assert_one_of("category", c("A", "B", "C"), ignore_case = TRUE)
#' }
assert_one_of <- function(
  field = NULL,
  values,
  ignore_case = FALSE,
  type = c("assert", "suggest")
) {
  type <- match.arg(type)

  values_str <- paste(values, collapse = ", ")
  msg <- if (!is.null(field)) {
    sprintf("%s must be one of: %s", field, values_str)
  } else {
    sprintf("Output must be one of: %s", values_str)
  }

  condition <- function(x) {
    value <- if (!is.null(field) && is.list(x)) x[[field]] else x
    if (is.null(value)) {
      return(FALSE)
    }

    value_str <- as.character(value)
    if (ignore_case) {
      tolower(value_str) %in% tolower(values)
    } else {
      value_str %in% values
    }
  }

  if (type == "assert") {
    assert_output(condition, msg, field = NULL)
  } else {
    suggest_output(condition, msg, field = NULL)
  }
}

#' Assert Custom Condition
#'
#' @description
#' Create a custom assertion with a user-defined condition function.
#' This is a convenience wrapper around `assert_output()` with clearer semantics.
#'
#' @param condition A function or formula that takes the output and returns TRUE/FALSE.
#' @param message Error message when assertion fails.
#' @param field Optional. The specific output field to validate.
#' @param type "assert" for hard assertion (default), "suggest" for soft suggestion.
#'
#' @return An Assertion object
#'
#' @export
#' @examples
#' \dontrun{
#' # Custom validation: answer must have exactly 3 sentences
#' assert_custom(
#'   ~ length(gregexpr("\\.", .x$answer)[[1]]) == 3,
#'   "Answer must have exactly 3 sentences"
#' )
#'
#' # Custom validation: summary must be shorter than original text
#' assert_custom(
#'   function(x) nchar(x$summary) < nchar(x$original),
#'   "Summary must be shorter than original"
#' )
#' }
assert_custom <- function(
  condition,
  message,
  field = NULL,
  type = c("assert", "suggest")
) {
  type <- match.arg(type)

  if (type == "assert") {
    assert_output(condition, message, field = field)
  } else {
    suggest_output(condition, message, field = field)
  }
}

#' Assert Output is Not Empty
#'
#' @description
#' Create an assertion that validates the output is not empty or whitespace-only.
#'
#' @param field The output field to check. If NULL, checks the entire output.
#' @param type "assert" for hard assertion (default), "suggest" for soft suggestion.
#'
#' @return An Assertion object
#'
#' @export
#' @examples
#' \dontrun{
#' # Answer must not be empty
#' assert_not_empty("answer")
#'
#' # All fields must have content
#' assert_not_empty("summary")
#' assert_not_empty("conclusion")
#' }
assert_not_empty <- function(field = NULL, type = c("assert", "suggest")) {
  type <- match.arg(type)

  msg <- if (!is.null(field)) {
    sprintf("%s must not be empty", field)
  } else {
    "Output must not be empty"
  }

  condition <- function(x) {
    value <- if (!is.null(field) && is.list(x)) x[[field]] else x
    if (is.null(value)) {
      return(FALSE)
    }

    trimmed <- trimws(as.character(value))
    nchar(trimmed) > 0
  }

  if (type == "assert") {
    assert_output(condition, msg, field = NULL)
  } else {
    suggest_output(condition, msg, field = NULL)
  }
}

#' Assert Numeric Value in Range
#'
#' @description
#' Create an assertion that validates a numeric output value is within a range.
#'
#' @param field The output field to check. If NULL, checks the entire output.
#' @param min Minimum value (inclusive). Default NULL (no minimum).
#' @param max Maximum value (inclusive). Default NULL (no maximum).
#' @param type "assert" for hard assertion (default), "suggest" for soft suggestion.
#'
#' @return An Assertion object
#'
#' @export
#' @examples
#' \dontrun{
#' # Score must be between 0 and 100
#' assert_range("score", min = 0, max = 100)
#'
#' # Confidence must be positive
#' assert_range("confidence", min = 0)
#' }
assert_range <- function(
  field = NULL,
  min = NULL,
  max = NULL,
  type = c("assert", "suggest")
) {
  type <- match.arg(type)

  if (is.null(min) && is.null(max)) {
    cli::cli_abort("At least one of {.arg min} or {.arg max} must be specified")
  }

  # Build message
  if (!is.null(min) && !is.null(max)) {
    msg <- sprintf("Value must be between %s and %s", min, max)
  } else if (!is.null(min)) {
    msg <- sprintf("Value must be at least %s", min)
  } else {
    msg <- sprintf("Value must be at most %s", max)
  }

  if (!is.null(field)) {
    msg <- paste0(field, ": ", msg)
  }

  condition <- function(x) {
    value <- if (!is.null(field) && is.list(x)) x[[field]] else x
    if (is.null(value)) {
      return(FALSE)
    }

    num <- suppressWarnings(as.numeric(value))
    if (is.na(num)) {
      return(FALSE)
    }

    passes_min <- is.null(min) || num >= min
    passes_max <- is.null(max) || num <= max
    passes_min && passes_max
  }

  if (type == "assert") {
    assert_output(condition, msg, field = NULL)
  } else {
    suggest_output(condition, msg, field = NULL)
  }
}
