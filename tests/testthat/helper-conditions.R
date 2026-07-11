# Capture all warnings from an expression without leaking them to testthat.
capture_test_conditions <- function(expr) {
  warnings <- list()
  error <- NULL
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings[[length(warnings) + 1L]] <<- w
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error <<- e
      NULL
    }
  )

  list(value = value, warnings = warnings, error = error)
}

expect_test_warnings <- function(expr, regexp) {
  captured <- capture_test_conditions(expr)
  if (!is.null(captured$error)) {
    stop(captured$error)
  }

  messages <- vapply(captured$warnings, conditionMessage, character(1))
  testthat::expect_gt(length(messages), 0L)
  testthat::expect_true(all(grepl(regexp, messages)))
  captured$value
}

expect_error_with_warnings <- function(expr, warning_regexp, error_regexp) {
  captured <- capture_test_conditions(expr)
  messages <- vapply(captured$warnings, conditionMessage, character(1))

  testthat::expect_gt(length(messages), 0L)
  testthat::expect_true(all(grepl(warning_regexp, messages)))
  testthat::expect_s3_class(captured$error, "error")
  testthat::expect_match(conditionMessage(captured$error), error_regexp)
  invisible(captured)
}
