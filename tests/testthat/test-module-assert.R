# Tests for AssertModule (with_assertions wrapper)

# Helper: create a mock module for testing
create_mock_module <- function(
  responses = list(list(answer = "test response"))
) {
  MockModule <- R6::R6Class(
    "MockModule",
    inherit = Module,
    public = list(
      responses = NULL,
      call_count = 0,
      initialize = function(responses) {
        self$responses <- responses
        super$initialize(
          signature = signature("question -> answer"),
          config = list()
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$call_count <- self$call_count + 1
        idx <- min(self$call_count, length(self$responses))
        response <- self$responses[[idx]]

        tibble::tibble(
          output = list(response),
          chat = list(NULL),
          metadata = list(list(
            timestamp = Sys.time(),
            model = "mock",
            total_tokens = 10,
            cost = 0.001,
            provider_calls = 1L
          ))
        )
      },
      reset_copy = function() {
        create_mock_module(self$responses)
      }
    )
  )
  MockModule$new(responses)
}

# Test with_assertions factory function
test_that("with_assertions creates AssertModule", {
  mock_mod <- create_mock_module()
  assertions <- list(
    assert_output(~TRUE, "Always passes")
  )

  assert_mod <- with_assertions(mock_mod, assertions)
  expect_s3_class(assert_mod, "AssertModule")
  expect_s3_class(assert_mod, "Module")
})

test_that("with_assertions validates module argument", {
  expect_error(
    with_assertions("not a module", list()),
    "must be a Module object"
  )
})

test_that("with_assertions accepts AssertionSet", {
  mock_mod <- create_mock_module()
  set <- assertion_set(
    assert_output(~TRUE, "OK")
  )

  assert_mod <- with_assertions(mock_mod, set)
  expect_true(S7::S7_inherits(assert_mod$assertion_set, AssertionSet))
})

test_that("with_assertions validates on_failure argument", {
  mock_mod <- create_mock_module()
  assertions <- list(assert_output(~TRUE, "OK"))

  expect_no_error(with_assertions(mock_mod, assertions, on_failure = "error"))
  expect_no_error(with_assertions(mock_mod, assertions, on_failure = "warn"))
})

# Test basic execution
test_that("AssertModule returns result when assertions pass", {
  mock_mod <- create_mock_module(list(list(answer = "Good answer")))
  assertions <- list(
    assert_output(~ nchar(.x$answer) > 5, "Must have content")
  )

  assert_mod <- with_assertions(mock_mod, assertions)
  result <- assert_mod$forward(list(question = "test"))

  expect_equal(result$output[[1]]$answer, "Good answer")
  expect_equal(result$metadata[[1]]$assertions_passed, TRUE)
  expect_equal(result$metadata[[1]]$n_attempts, 1)
})

test_that("AssertModule retries when assertions fail", {
  # First response fails, second passes
  mock_mod <- create_mock_module(list(
    list(answer = "x"), # Too short
    list(answer = "Good answer") # Long enough
  ))

  assertions <- list(
    assert_output(~ nchar(.x$answer) > 5, "Must be longer than 5 chars")
  )

  assert_mod <- with_assertions(mock_mod, assertions, max_retries = 3)
  result <- assert_mod$forward(list(question = "test"))

  expect_equal(result$output[[1]]$answer, "Good answer")
  expect_equal(result$metadata[[1]]$n_attempts, 2)
  expect_true(result$metadata[[1]]$assertions_passed)
})

test_that("AssertModule respects max_retries", {
  # All responses fail
  mock_mod <- create_mock_module(list(
    list(answer = "a"),
    list(answer = "b"),
    list(answer = "c"),
    list(answer = "d")
  ))

  assertions <- list(
    assert_output(~ nchar(.x$answer) > 100, "Must be very long")
  )

  assert_mod <- with_assertions(
    mock_mod,
    assertions,
    max_retries = 2,
    on_failure = "error"
  )

  expect_error(
    assert_mod$forward(list(question = "test")),
    "Assertions failed after"
  )

  # Should have tried 3 times (1 initial + 2 retries)
  expect_equal(mock_mod$call_count, 3)
})

test_that("AssertModule warns instead of errors when on_failure='warn'", {
  mock_mod <- create_mock_module(list(list(answer = "short")))
  assertions <- list(
    assert_output(~ nchar(.x$answer) > 100, "Must be long")
  )

  assert_mod <- with_assertions(
    mock_mod,
    assertions,
    max_retries = 0,
    on_failure = "warn"
  )

  expect_warning(
    result <- assert_mod$forward(list(question = "test")),
    "Assertions failed"
  )

  # Should still return the best result
  expect_equal(result$output[[1]]$answer, "short")
})

# Test soft suggestions
test_that("AssertModule logs soft suggestion failures but doesn't retry", {
  mock_mod <- create_mock_module(list(list(answer = "hello")))
  assertions <- list(
    suggest_output(~ grepl("^[A-Z]", .x$answer), "Should start with capital")
  )

  assert_mod <- with_assertions(mock_mod, assertions)

  expect_warning(
    result <- assert_mod$forward(list(question = "test")),
    "Suggestion not met"
  )

  # Should only call once (no retry for soft suggestions)
  expect_equal(mock_mod$call_count, 1)
  expect_true(result$metadata[[1]]$assertions_passed) # Soft don't block
})

test_that("AssertModule handles mixed hard and soft assertions", {
  mock_mod <- create_mock_module(list(
    list(answer = "good enough"), # Passes hard, fails soft
    list(answer = "Good enough") # Passes both
  ))

  assertions <- list(
    assert_output(~ nchar(.x$answer) > 5, "Hard: must have length"),
    suggest_output(~ grepl("^[A-Z]", .x$answer), "Soft: should capitalize")
  )

  assert_mod <- with_assertions(mock_mod, assertions, max_retries = 3)

  # First response passes hard assertions, so should return immediately
  # (soft suggestions don't trigger retries)
  expect_warning(
    result <- assert_mod$forward(list(question = "test")),
    "Suggestion not met"
  )

  expect_equal(result$output[[1]]$answer, "good enough")
  expect_equal(mock_mod$call_count, 1) # No retry needed
})

# Test feedback injection
test_that("AssertModule injects feedback on retry", {
  call_batches <- list()
  mock_mod <- R6::R6Class(
    "MockModuleWithCapture",
    inherit = Module,
    public = list(
      responses = NULL,
      call_count = 0,
      captured_batches = list(),
      initialize = function(responses) {
        self$responses <- responses
        super$initialize(
          signature = signature("question -> answer"),
          config = list()
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$call_count <- self$call_count + 1
        self$captured_batches <- append(self$captured_batches, list(batch))
        idx <- min(self$call_count, length(self$responses))
        response <- self$responses[[idx]]

        tibble::tibble(
          output = list(response),
          chat = list(NULL),
          metadata = list(list(timestamp = Sys.time(), model = "mock"))
        )
      },
      reset_copy = function() {
        MockModuleWithCapture$new(self$responses)
      }
    )
  )$new(list(
    list(answer = "x"), # Fails
    list(answer = "Good answer") # Passes
  ))

  assertions <- list(
    assert_output(~ nchar(.x$answer) > 5, "Must be longer")
  )

  assert_mod <- with_assertions(mock_mod, assertions)
  result <- assert_mod$forward(list(question = "test"))

  # Second call should have feedback injected
  expect_true(length(mock_mod$captured_batches) >= 2)
  second_batch <- mock_mod$captured_batches[[2]]
  expect_true("assertion_feedback" %in% names(second_batch))
  expect_true(grepl(
    "Must be longer",
    second_batch$assertion_feedback,
    fixed = TRUE
  ))
})

# Test get_attempts method
test_that("AssertModule tracks attempts", {
  mock_mod <- create_mock_module(list(
    list(answer = "x"),
    list(answer = "Good answer")
  ))

  assertions <- list(
    assert_output(~ nchar(.x$answer) > 5, "Must be longer")
  )

  assert_mod <- with_assertions(mock_mod, assertions)
  result <- assert_mod$forward(list(question = "test"))

  attempts <- assert_mod$get_attempts()
  expect_s3_class(attempts, "tbl_df")
  expect_equal(nrow(attempts), 2)
  expect_true("hard_passed" %in% names(attempts))
  expect_false(attempts$hard_passed[1]) # First attempt failed
  expect_true(attempts$hard_passed[2]) # Second attempt passed
})

test_that("AssertModule get_attempts(all = TRUE) returns all runs", {
  mock_mod <- create_mock_module(list(list(answer = "Good answer")))
  assertions <- list(assert_output(~TRUE, "OK"))

  assert_mod <- with_assertions(mock_mod, assertions)

  # Run twice
  assert_mod$forward(list(question = "test1"))
  mock_mod$call_count <- 0 # Reset for second run
  assert_mod$forward(list(question = "test2"))

  all_attempts <- assert_mod$get_attempts(all = TRUE)
  expect_equal(nrow(all_attempts), 2) # One attempt per run
  expect_equal(all_attempts$run, c(1L, 2L))
})

# Test print method
test_that("AssertModule print method works", {
  mock_mod <- create_mock_module()
  assertions <- list(
    assert_output(~TRUE, "A"),
    suggest_output(~TRUE, "B")
  )

  assert_mod <- with_assertions(mock_mod, assertions, max_retries = 5)

  # cli output may not be captured by expect_output
  expect_no_error(print(assert_mod))
})

# Test reset_copy method
test_that("AssertModule reset_copy creates fresh module", {
  mock_mod <- create_mock_module(list(list(answer = "test")))
  assertions <- list(assert_output(~TRUE, "OK"))

  assert_mod <- with_assertions(mock_mod, assertions)
  assert_mod$forward(list(question = "test"))

  copy <- assert_mod$reset_copy()
  expect_s3_class(copy, "AssertModule")
  expect_length(copy$state$traces, 0) # Reset state
})

# Test metadata accumulation
test_that("AssertModule accumulates tokens and cost across retries", {
  mock_mod <- create_mock_module(list(
    list(answer = "x"),
    list(answer = "Good answer")
  ))

  assertions <- list(
    assert_output(~ nchar(.x$answer) > 5, "Must be longer")
  )

  assert_mod <- with_assertions(mock_mod, assertions)
  result <- assert_mod$forward(list(question = "test"))

  # Should accumulate tokens from both attempts
  expect_equal(result$metadata[[1]]$total_tokens, 20) # 10 per call
  expect_equal(result$metadata[[1]]$cost, 0.002) # 0.001 per call
  expect_identical(result$metadata[[1]]$provider_calls, 2L)
  expect_false("total_cost" %in% names(result$metadata[[1]]))
})

# Test custom feedback template
test_that("AssertModule uses custom feedback template", {
  call_batches <- list()
  mock_mod <- R6::R6Class(
    "MockModuleCapture2",
    inherit = Module,
    public = list(
      captured_batches = list(),
      call_count = 0,
      initialize = function() {
        super$initialize(
          signature = signature("question -> answer"),
          config = list()
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$call_count <- self$call_count + 1
        self$captured_batches <- append(self$captured_batches, list(batch))

        tibble::tibble(
          output = list(list(
            answer = if (self$call_count == 1) "x" else "Good"
          )),
          chat = list(NULL),
          metadata = list(list(timestamp = Sys.time(), model = "mock"))
        )
      },
      reset_copy = function() self
    )
  )$new()

  assertions <- list(
    assert_output(~ nchar(.x$answer) > 3, "Must be longer")
  )

  custom_template <- "CUSTOM FEEDBACK: {failures}"
  assert_mod <- with_assertions(
    mock_mod,
    assertions,
    feedback_template = custom_template
  )

  result <- assert_mod$forward(list(question = "test"))

  # Check that custom template was used
  second_batch <- mock_mod$captured_batches[[2]]
  expect_true(grepl(
    "CUSTOM FEEDBACK:",
    second_batch$assertion_feedback,
    fixed = TRUE
  ))
})

# Test edge cases
test_that("AssertModule handles empty assertions list with warning", {
  mock_mod <- create_mock_module(list(list(answer = "test")))
  assertions <- list() # Empty

  # Should warn about empty assertion set
  expect_warning(
    set <- assertion_set(assertions),
    "empty AssertionSet"
  )

  # with_assertions should work with empty set (warns during creation)
  expect_warning(
    assert_mod <- with_assertions(mock_mod, assertion_set()),
    "empty AssertionSet"
  )
  result <- assert_mod$forward(list(question = "test"))
  expect_true(result$metadata[[1]]$assertions_passed)
})

test_that("AssertModule returns best result on max retries exceeded", {
  # Create module where second response is "better" (fewer failures)
  mock_mod <- R6::R6Class(
    "MockModuleBestOfBad",
    inherit = Module,
    public = list(
      call_count = 0,
      initialize = function() {
        super$initialize(
          signature = signature("question -> answer"),
          config = list()
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$call_count <- self$call_count + 1
        # All fail the >100 check, but second is longer (better)
        answer <- if (self$call_count == 1) "short" else "a bit longer"

        tibble::tibble(
          output = list(list(answer = answer)),
          chat = list(NULL),
          metadata = list(list(timestamp = Sys.time(), model = "mock"))
        )
      },
      reset_copy = function() self
    )
  )$new()

  assertions <- list(
    assert_output(~ nchar(.x$answer) > 100, "Must be very long")
  )

  assert_mod <- with_assertions(
    mock_mod,
    assertions,
    max_retries = 1,
    on_failure = "warn"
  )

  expect_warning(
    result <- assert_mod$forward(list(question = "test")),
    "Assertions failed"
  )

  # Should return best attempt (all same # of failures, but tracking works)
  expect_true(is.list(result$output[[1]]))
})

# Test apply_optimization_params
test_that("AssertModule apply_optimization_params updates max_retries", {
  mock_mod <- create_mock_module()
  assertions <- list(assert_output(~TRUE, "OK"))

  assert_mod <- with_assertions(mock_mod, assertions, max_retries = 3)
  expect_equal(assert_mod$max_retries, 3)

  assert_mod$apply_optimization_params(list(max_retries = 5))
  expect_equal(assert_mod$max_retries, 5)
})

# Test max_retries=0 with on_failure=error
test_that("AssertModule with max_retries=0 errors immediately on assertion failure", {
  mock_mod <- create_mock_module(list(list(answer = "x"))) # Too short
  assertions <- list(
    assert_output(~ nchar(.x$answer) > 10, "Must be longer than 10 chars")
  )

  assert_mod <- with_assertions(
    mock_mod,
    assertions,
    max_retries = 0,
    on_failure = "error"
  )

  expect_error(
    assert_mod$forward(list(question = "test")),
    "Assertions failed after 1 attempt"
  )

  # Should have only tried once
  expect_equal(mock_mod$call_count, 1)
})

# Test all module attempts throwing exceptions
test_that("AssertModule handles all attempts throwing exceptions", {
  # Module that always throws errors
  error_module <- R6::R6Class(
    "ErrorModule",
    inherit = Module,
    public = list(
      call_count = 0,
      initialize = function() {
        super$initialize(
          signature = signature("question -> answer"),
          config = list()
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$call_count <- self$call_count + 1
        stop(sprintf("Simulated error on attempt %d", self$call_count))
      },
      reset_copy = function() self
    )
  )$new()

  assertions <- list(assert_output(~TRUE, "OK"))
  assert_mod <- with_assertions(error_module, assertions, max_retries = 2)

  # Should error with tracked error messages
  expect_error(
    suppressWarnings(assert_mod$forward(list(question = "test"))),
    "All attempts failed"
  )

  # Should have tried 3 times (1 initial + 2 retries)
  expect_equal(error_module$call_count, 3)
})

# Test batch processing with multiple rows
test_that("AssertModule processes first row of data frame batch", {
  mock_mod <- create_mock_module(list(list(answer = "Good answer")))
  assertions <- list(
    assert_output(~ nchar(.x$answer) > 5, "Must have content")
  )

  assert_mod <- with_assertions(mock_mod, assertions)

  # Pass a data frame with multiple rows
  batch_df <- data.frame(
    question = c("Q1", "Q2", "Q3"),
    stringsAsFactors = FALSE
  )

  result <- assert_mod$forward(batch_df)
  expect_equal(result$output[[1]]$answer, "Good answer")
  expect_equal(result$metadata[[1]]$n_attempts, 1)
})

# Test partial failure recovery (error then success)
test_that("AssertModule recovers from error followed by assertion failure then success", {
  # Scenario:
  # 1. First attempt: module throws error
  # 2. Second attempt: module succeeds but assertion fails
  # 3. Third attempt: module succeeds and assertion passes
  # This tests realistic transient failure scenarios

  recovery_module <- R6::R6Class(
    "RecoveryModule",
    inherit = Module,
    public = list(
      call_count = 0,
      initialize = function() {
        super$initialize(
          signature = signature("question -> answer"),
          config = list()
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$call_count <- self$call_count + 1

        # First call: throw error (simulating network timeout)
        if (self$call_count == 1) {
          stop("Simulated network timeout")
        }

        # Second call: succeed but return short answer (fails assertion)
        if (self$call_count == 2) {
          return(tibble::tibble(
            output = list(list(answer = "short")),
            chat = list(NULL),
            metadata = list(list(
              timestamp = Sys.time(),
              model = "mock",
              total_tokens = 10,
              cost = 0.001,
              provider_calls = 1L
            ))
          ))
        }

        # Third call and beyond: succeed with good answer
        tibble::tibble(
          output = list(list(answer = "This is a good, detailed answer")),
          chat = list(NULL),
          metadata = list(list(
            timestamp = Sys.time(),
            model = "mock",
            total_tokens = 10,
            cost = 0.001,
            provider_calls = 1L
          ))
        )
      },
      reset_copy = function() self
    )
  )$new()

  assertions <- list(
    assert_output(
      ~ nchar(.x$answer) > 10,
      "Answer must be longer than 10 characters"
    )
  )

  assert_mod <- with_assertions(recovery_module, assertions, max_retries = 3)

  # Should warn about the first error, then succeed
  expect_warning(
    result <- assert_mod$forward(list(question = "test")),
    "Attempt 1 failed"
  )

  # Should have called 3 times total
  expect_equal(recovery_module$call_count, 3)

  # Should return the successful result from third attempt
  expect_equal(result$output[[1]]$answer, "This is a good, detailed answer")
  expect_true(result$metadata[[1]]$assertions_passed)

  # n_attempts only counts successful attempts (attempts that return results)
  # The first attempt threw an error, so it doesn't count in the attempt list
  expect_equal(result$metadata[[1]]$n_attempts, 2)

  # The failed attempt may have reached the provider before throwing.
  expect_true(is.na(result$metadata[[1]]$total_tokens))
  expect_true(is.na(result$metadata[[1]]$cost))
  expect_true(is.na(result$metadata[[1]]$provider_calls))
})

# Test that signature validation works
test_that("AssertModule rejects module with NULL signature", {
  # Create a mock module without proper signature
  bad_module <- list(
    signature = NULL,
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      tibble::tibble(output = list(list(answer = "test")))
    }
  )
  class(bad_module) <- c("BadModule", "Module", "R6")

  assertions <- list(assert_output(~TRUE, "OK"))

  expect_error(
    with_assertions(bad_module, assertions),
    "Wrapped module must have a valid signature"
  )
})

# Test that empty message validation works
test_that("Assertion rejects empty message", {
  expect_error(
    Assertion(
      condition = function(x) TRUE,
      message = ""
    ),
    "message must not be empty"
  )
})
