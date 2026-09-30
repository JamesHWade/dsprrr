#' Assertion helpers
#'
#' Shorthand constructors for common [assert_output()] conditions.
#'
#' @name assertion-helpers
#' @noRd
NULL

#' Assert the length of an output
#'
#' @description
#' `assert_length()` checks that the number of characters in an output field
#' is within `min` and `max`. Like the other `assert_*()` helpers, it is
#' shorthand for [assert_output()] (or [suggest_output()]) with a ready-made
#' condition.
#'
#' @param field The output field to check. With `NULL`, the whole output is
#'   checked, which suits outputs with a single field. A missing field fails
#'   the check.
#' @param min,max Inclusive bounds on the number of characters. Give at least
#'   one.
#' @param type `"assert"` (the default) for a hard assertion, which makes
#'   [with_assertions()] retry when it fails, or `"suggest"` for a soft one,
#'   which only gives a warning.
#'
#' @return An assertion for [assertion_set()] or [with_assertions()].
#'
#' @export
#' @family assertions
#' @examples
#' assert_length("answer", max = 100)
#' assert_length("summary", min = 10, max = 200, type = "suggest")
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

#' Assert that an output contains a string
#'
#' @description
#' `assert_contains()` checks that an output field contains `pattern`,
#' matched as a fixed string, not a regular expression. Use
#' [assert_matches()] for regular expressions.
#'
#' @inheritParams assert_length
#' @param pattern The string that must appear.
#' @param ignore_case If `TRUE`, ignore case when matching.
#'
#' @inherit assert_length return
#'
#' @export
#' @family assertions
#' @examples
#' assert_contains("answer", "Paris")
#' assert_contains("summary", "conclusion", ignore_case = TRUE)
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

#' Assert that an output does not contain a string
#'
#' @description
#' `assert_not_contains()` checks that an output field does not contain
#' `pattern`, matched as a fixed string. A missing field passes.
#'
#' @inheritParams assert_contains
#' @param pattern The string that must not appear.
#'
#' @inherit assert_length return
#'
#' @export
#' @family assertions
#' @examples
#' assert_not_contains("answer", "As an AI")
#' assert_not_contains("reply", "password", ignore_case = TRUE)
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

#' Assert that an output matches a regular expression
#'
#' @description
#' `assert_matches()` checks that an output field matches a Perl-compatible
#' regular expression.
#'
#' @inheritParams assert_length
#' @param pattern A regular expression (Perl syntax).
#' @param message The message shown when the check fails, which
#'   [with_assertions()] also sends back to the model on a retry. With `NULL`,
#'   a message naming the field and pattern.
#' @param ignore_case If `TRUE`, ignore case when matching.
#'
#' @inherit assert_length return
#'
#' @export
#' @family assertions
#' @examples
#' assert_matches("answer", "^[A-Z]", "Start with a capital letter")
#' assert_matches("email", "^[^@]+@[^@]+\\.[^@]+$", "Return a valid email address")
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

#' Assert that an output does not match a regular expression
#'
#' @description
#' `assert_not_matches()` checks that an output field does not match a
#' Perl-compatible regular expression. A missing field passes.
#'
#' @inheritParams assert_matches
#' @param pattern A regular expression (Perl syntax) that must not match.
#'
#' @inherit assert_length return
#'
#' @export
#' @family assertions
#' @examples
#' assert_not_matches("answer", "https?://", "Do not include links")
#' assert_not_matches("summary", "```", "Do not include code blocks")
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

#' Assert that an output is one of a set of values
#'
#' @description
#' `assert_one_of()` checks that an output field equals one of `values`.
#'
#' @inheritParams assert_length
#' @param values A character vector of allowed values.
#' @param ignore_case If `TRUE`, ignore case when comparing.
#'
#' @inherit assert_length return
#'
#' @export
#' @family assertions
#' @examples
#' assert_one_of("sentiment", c("positive", "negative", "neutral"))
#' assert_one_of("grade", c("A", "B", "C"), ignore_case = TRUE)
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

#' Assert a custom condition
#'
#' @description
#' `assert_custom()` builds an assertion from your own condition. It is
#' [assert_output()] or [suggest_output()], chosen by `type`.
#'
#' @inheritParams assert_length
#' @param condition A function, or a formula using `.x`, that takes the output
#'   (or the `field`) and returns `TRUE` or `FALSE`.
#' @param message The message shown when the check fails, which
#'   [with_assertions()] also sends back to the model on a retry.
#' @param field The output field passed to `condition`. With `NULL` (the
#'   default), the whole output (a named list) is passed.
#'
#' @inherit assert_length return
#'
#' @export
#' @family assertions
#' @examples
#' # Exactly three sentences
#' assert_custom(
#'   ~ lengths(regmatches(.x$answer, gregexpr("[.!?]", .x$answer))) == 3,
#'   "Answer in exactly three sentences"
#' )
#'
#' # Compare two output fields
#' assert_custom(
#'   function(x) nchar(x$summary) < nchar(x$details),
#'   "The summary must be shorter than the details"
#' )
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

#' Assert that an output is not empty
#'
#' @description
#' `assert_not_empty()` checks that an output field has at least one
#' character that is not white space.
#'
#' @inheritParams assert_length
#'
#' @inherit assert_length return
#'
#' @export
#' @family assertions
#' @examples
#' assert_not_empty("answer")
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

#' Assert that a numeric output is within a range
#'
#' @description
#' `assert_range()` checks that an output field, converted to a number, lies
#' within `min` and `max`. A value that cannot be converted fails the check.
#'
#' @inheritParams assert_length
#' @param min,max Inclusive bounds. Give at least one.
#'
#' @inherit assert_length return
#'
#' @export
#' @family assertions
#' @examples
#' assert_range("score", min = 0, max = 100)
#' assert_range("confidence", min = 0, type = "suggest")
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
