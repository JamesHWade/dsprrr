# Assertions Framework
# ====================
# S7 classes for defining output validation with backtracking support.
# Follows DSPy assertions pattern where assertions are hard constraints
# that trigger retries, while suggestions are soft constraints that log warnings.

#' Define output assertions
#'
#' @description
#' Assertions are checks on a module's output. `assert_output()` makes a hard
#' assertion: when it fails, [with_assertions()] retries the module with
#' feedback. `suggest_output()` makes a soft one: a failure only gives a
#' warning. `assertion_set()` groups assertions. The `assert_*()` helpers,
#' such as [assert_length()], build common conditions for you.
#'
#' @details
#' A condition is a function, or a formula using `.x`, that receives the
#' output (a named list) or, with `field`, that field's value, and returns
#' `TRUE` or `FALSE`. A condition that errors, returns `NA` or returns
#' anything other than a single logical value counts as failed and gives a
#' warning; so does a `field` that is missing from the output.
#'
#' @return `assert_output()` and `suggest_output()` return an assertion, and
#'   `assertion_set()` a set of assertions, for [with_assertions()].
#' @family assertions
#' @examples
#' short <- assert_output(~ nchar(.x$answer) <= 100, "Keep the answer under 100 characters")
#' short
#'
#' capitalized <- suggest_output(
#'   ~ grepl("^[A-Z]", .x),
#'   "Start with a capital letter",
#'   field = "answer"
#' )
#'
#' assertion_set(short, capitalized)
#' @name assertions
NULL

#' Internal output-assertion record class
#' @noRd
Assertion <- S7::new_class(
  "Assertion",
  properties = list(
    condition = S7::new_property(
      S7::class_function,
      validator = function(value) {
        if (!is.function(value)) {
          return("condition must be a function")
        }
        NULL
      }
    ),
    message = S7::new_property(
      S7::class_character,
      default = "Assertion failed",
      validator = function(value) {
        if (!is.character(value) || length(value) != 1) {
          return("message must be a single character string")
        }
        if (nchar(value) == 0) {
          return("message must not be empty")
        }
        NULL
      }
    ),
    field = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && (!is.character(value) || length(value) != 1)) {
          return("field must be a single character string or NULL")
        }
        NULL
      }
    ),
    type = S7::new_property(
      S7::class_character,
      default = "assert",
      validator = function(value) {
        if (!is.character(value) || length(value) != 1) {
          return("type must be a single character string")
        }
        if (!value %in% c("assert", "suggest")) {
          return("type must be 'assert' or 'suggest'")
        }
        NULL
      }
    )
  )
)

#' Print method for Assertion
#' @noRd
print_assertion <- function(x, ...) {
  type_label <- if (x@type == "assert") "Hard Assertion" else "Soft Suggestion"
  field_info <- if (is.null(x@field)) {
    "any field"
  } else {
    x@field
  }

  cli::cli_h3("{type_label}")
  cli::cli_li("Field: {.field {field_info}}")
  cli::cli_li("Message: {.val {x@message}}")
  invisible(x)
}

#' Internal assertion-set record class
#' @noRd
AssertionSet <- S7::new_class(
  "AssertionSet",
  properties = list(
    assertions = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        if (!is.list(value)) {
          return("assertions must be a list")
        }
        for (i in seq_along(value)) {
          if (!S7::S7_inherits(value[[i]], Assertion)) {
            return(sprintf("Element %d must be an Assertion object", i))
          }
        }
        NULL
      }
    )
  )
)

#' Print method for AssertionSet
#' @noRd
print_assertion_set <- function(x, ...) {
  n_assert <- sum(vapply(
    x@assertions,
    function(a) a@type == "assert",
    logical(1)
  ))
  n_suggest <- sum(vapply(
    x@assertions,
    function(a) a@type == "suggest",
    logical(1)
  ))

  cli::cli_h2("AssertionSet")
  cli::cli_li("{n_assert} hard assertion{?s}")
  cli::cli_li("{n_suggest} soft suggestion{?s}")

  if (length(x@assertions) > 0) {
    cli::cli_h3("Details")
    for (assertion in x@assertions) {
      print(assertion)
    }
  }

  invisible(x)
}

# Helper functions for creating assertions
# ----------------------------------------

#' @param condition A function, or a formula using `.x` such as
#'   `~ nchar(.x$answer) <= 100`, that takes the output (or the `field`) and
#'   returns `TRUE` or `FALSE`.
#' @param message The message shown when the check fails. [with_assertions()]
#'   also sends it back to the model on a retry, so phrase it as an
#'   instruction.
#' @param field The output field passed to `condition`. With `NULL` (the
#'   default), the whole output is passed.
#' @rdname assertions
#' @export
assert_output <- function(
  condition,
  message = "Assertion failed",
  field = NULL
) {
  cond_fn <- as_condition_function(condition)

  Assertion(
    condition = cond_fn,
    message = message,
    field = field,
    type = "assert"
  )
}

#' @rdname assertions
#' @export
suggest_output <- function(
  condition,
  message = "Suggestion not met",
  field = NULL
) {
  cond_fn <- as_condition_function(condition)

  Assertion(
    condition = cond_fn,
    message = message,
    field = field,
    type = "suggest"
  )
}

#' Convert condition to function
#' @noRd
as_condition_function <- function(condition) {
  if (rlang::is_formula(condition)) {
    # Convert formula to function
    rlang::as_function(condition)
  } else if (is.function(condition)) {
    condition
  } else {
    cli::cli_abort(
      "condition must be a formula (e.g., ~ nchar(.x) < 100) or a function"
    )
  }
}

#' Evaluate an assertion against output
#' @noRd
evaluate_assertion <- function(assertion, output) {
  # Extract the relevant value
  value <- if (is.null(assertion@field)) {
    output
  } else {
    if (is.list(output) && assertion@field %in% names(output)) {
      output[[assertion@field]]
    } else {
      # Missing field is a failure - typos or schema changes should not pass silently
      cli::cli_warn(c(
        "Field {.field {assertion@field}} not found in output",
        "i" = "Available fields: {.field {names(output)}}",
        "!" = "Assertion failed due to missing field"
      ))
      return(list(
        passed = FALSE,
        message = sprintf(
          "%s (field '%s' not found in output)",
          assertion@message,
          assertion@field
        ),
        type = assertion@type
      ))
    }
  }

  # Evaluate the condition
  result <- tryCatch(
    {
      assertion@condition(value)
    },
    error = function(e) {
      cli::cli_warn(c(
        "Assertion condition raised error: {e$message}",
        "i" = "Field: {.field {assertion@field %||% 'entire output'}}",
        "i" = "This may indicate a bug in the assertion condition"
      ))
      # Return a special marker to distinguish from assertion failure
      structure(FALSE, condition_error = TRUE, error_message = e$message)
    }
  )

  # Handle NA values explicitly
  if (is.logical(result) && length(result) == 1 && is.na(result)) {
    cli::cli_warn(c(
      "Assertion condition returned NA (unknown)",
      "!" = "Treating as FALSE - NA typically indicates missing data or invalid input"
    ))
    result <- FALSE
  } else if (!is.logical(result) || length(result) != 1) {
    cli::cli_warn(c(
      "Assertion condition must return a single logical value",
      "i" = "Got: {.cls {class(result)}} of length {length(result)}",
      "!" = "Treating as FALSE - consider fixing your condition function"
    ))
    result <- FALSE
  }

  # Build the message, including condition error context if applicable
  final_message <- if (!isTRUE(result)) {
    if (!is.null(attr(result, "condition_error"))) {
      sprintf(
        "%s (condition error: %s)",
        assertion@message,
        attr(result, "error_message")
      )
    } else {
      assertion@message
    }
  } else {
    NULL
  }

  list(
    passed = isTRUE(result),
    message = final_message,
    type = assertion@type
  )
}

#' Evaluate all assertions in an AssertionSet
#' @noRd
evaluate_assertion_set <- function(assertion_set, output) {
  results <- lapply(assertion_set@assertions, function(assertion) {
    evaluate_assertion(assertion, output)
  })

  # Separate hard assertions from suggestions
  hard_failures <- Filter(
    function(r) !r$passed && r$type == "assert",
    results
  )
  soft_failures <- Filter(
    function(r) !r$passed && r$type == "suggest",
    results
  )

  list(
    all_passed = length(hard_failures) == 0,
    hard_failures = hard_failures,
    soft_failures = soft_failures,
    n_hard_failed = length(hard_failures),
    n_soft_failed = length(soft_failures)
  )
}

#' @param ... For `assertion_set()`: assertions made with `assert_output()`,
#'   `suggest_output()` or the `assert_*()` helpers, or one list of them. An
#'   empty set gives a warning.
#' @rdname assertions
#' @export
assertion_set <- function(...) {
  args <- list(...)

  # Handle case where a single list is passed
  if (
    length(args) == 1 &&
      is.list(args[[1]]) &&
      !S7::S7_inherits(args[[1]], Assertion)
  ) {
    args <- args[[1]]
  }

  # Validate all are Assertions
  for (i in seq_along(args)) {
    if (!S7::S7_inherits(args[[i]], Assertion)) {
      cli::cli_abort(
        "Element {i} must be an Assertion object (use assert_output() or suggest_output())"
      )
    }
  }

  # Warn if empty assertion set (likely unintended)
  if (length(args) == 0) {
    cli::cli_warn(c(
      "Creating empty AssertionSet with no assertions",
      "i" = "This will pass all outputs without validation",
      "i" = "Add assertions using {.fn assert_output} or {.fn suggest_output}"
    ))
  }

  AssertionSet(assertions = args)
}
