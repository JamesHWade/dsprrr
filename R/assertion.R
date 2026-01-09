# Assertions Framework
# ====================
# S7 classes for defining output validation with backtracking support.
# Follows DSPy assertions pattern where assertions are hard constraints
# that trigger retries, while suggestions are soft constraints that log warnings.

#' Assertions for Output Validation
#'
#' @description
#' S7 classes and helper functions for defining output validation constraints
#' with backtracking support. Hard assertions trigger retries when they fail,
#' while soft suggestions log warnings but allow execution to continue.
#'
#' @param condition A function or formula that takes the output and returns TRUE/FALSE.
#'   For formulas, use `.x` to reference the output (e.g., `~ nchar(.x$answer) <= 100`).
#' @param message Error message to display when the assertion fails.
#' @param field Optional. The specific output field to validate. If NULL, the entire
#'   output is passed to the condition.
#' @param type For `Assertion` class: "assert" for hard assertion, "suggest" for soft
#'   suggestion. Use the helper functions `assert_output()` and `suggest_output()`
#'   instead of setting this directly.
#' @param assertions For `AssertionSet` class: A list of Assertion objects.
#' @param ... For `assertion_set()`: Assertion objects to combine into a set.
#'
#' @name assertions
NULL

#' @rdname assertions
#' @export
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
S7::method(print, Assertion) <- function(x, ...) {
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

#' @rdname assertions
#' @export
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
S7::method(print, AssertionSet) <- function(x, ...) {
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

#' Create Output Assertions
#'
#' @description
#' Define validation constraints for module outputs. Hard assertions (`assert_output`)
#' trigger retries when they fail, while soft suggestions (`suggest_output`) log
#' warnings but allow execution to continue.
#'
#' @param condition A formula or function that takes the output and returns TRUE/FALSE.
#'   For formulas, use `.x` to reference the output (e.g., `~ nchar(.x$answer) <= 100`).
#' @param message Error message to display when the assertion fails.
#' @param field Optional. The specific output field to validate. If NULL, the entire
#'   output is passed to the condition.
#'
#' @return An Assertion object.
#'
#' @details
#' ## Backtracking Behavior
#'
#' When wrapped with `with_assertions()`, modules will:
#' 1. Run the module normally
#' 2. Evaluate all assertions against the output
#' 3. If hard assertions fail and retries remain, inject feedback and retry
#' 4. If max retries exceeded, raise an error (or warning if configured)
#' 5. Soft suggestions always log but never trigger retries
#'
#' ## Condition Functions
#'
#' Conditions can be specified as:
#' - **Formulas**: `~ nchar(.x$answer) <= 100` - `.x` is the output
#' - **Functions**: `function(x) nchar(x$answer) <= 100`
#'
#' @examples
#' \dontrun{
#' # Hard assertion - must be satisfied
#' assert_output(~ nchar(.x$answer) <= 100, "Answer must be 100 chars or less")
#'
#' # Soft suggestion - logs warning but continues
#' suggest_output(~ grepl("^[A-Z]", .x$answer), "Should start with capital")
#'
#' # Field-specific assertion
#' assert_output(~ nchar(.x) <= 50, "Too long", field = "summary")
#' }
#'
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

#' Create an AssertionSet from a list of assertions
#'
#' @param ... Assertion objects or a list of Assertion objects.
#' @return An AssertionSet object.
#'
#' @examples
#' \dontrun{
#' assertions <- assertion_set(
#'   assert_output(~ nchar(.x$answer) <= 100, "Too long"),
#'   suggest_output(~ grepl("^[A-Z]", .x$answer), "Should capitalize")
#' )
#' }
#'
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
