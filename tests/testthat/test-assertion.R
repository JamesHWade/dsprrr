# Tests for assertions framework

# Test Assertion S7 class
test_that("Assertion class can be created with formula condition", {
  assertion <- assert_output(~ nchar(.x) < 100, "Too long")
  expect_true(S7::S7_inherits(assertion, Assertion))
  expect_equal(assertion@message, "Too long")
  expect_equal(assertion@type, "assert")
  expect_null(assertion@field)
})

test_that("Assertion class can be created with function condition", {
  assertion <- assert_output(function(x) nchar(x) < 100, "Too long")
  expect_true(S7::S7_inherits(assertion, Assertion))
  expect_true(is.function(assertion@condition))
})

test_that("Assertion class can have a specific field", {
  assertion <- assert_output(~ nchar(.x) < 100, "Too long", field = "answer")
  expect_equal(assertion@field, "answer")
})

test_that("suggest_output creates suggestion type assertions", {
  assertion <- suggest_output(~ nchar(.x) < 100, "Should be shorter")
  expect_equal(assertion@type, "suggest")
})

test_that("Assertion validates type argument", {
  assertion <- Assertion(
    condition = function(x) TRUE,
    message = "test",
    type = "assert"
  )
  expect_equal(assertion@type, "assert")

  assertion2 <- Assertion(
    condition = function(x) TRUE,
    message = "test",
    type = "suggest"
  )
  expect_equal(assertion2@type, "suggest")
})

test_that("Assertion rejects invalid types", {
  expect_error(
    Assertion(
      condition = function(x) TRUE,
      message = "test",
      type = "invalid"
    ),
    "must be 'assert' or 'suggest'"
  )
})

test_that("assert_output rejects invalid condition", {
  expect_error(
    assert_output("not a function", "message"),
    "must be a formula"
  )
})

# Test AssertionSet class
test_that("AssertionSet can be created from list", {
  a1 <- assert_output(~ nchar(.x) < 100, "Too long")
  a2 <- suggest_output(~ grepl("^[A-Z]", .x), "Should capitalize")

  set <- assertion_set(a1, a2)
  expect_true(S7::S7_inherits(set, AssertionSet))
  expect_length(set@assertions, 2)
})

test_that("AssertionSet validates that all elements are Assertions", {
  a1 <- assert_output(~TRUE, "OK")

  expect_error(
    assertion_set(a1, "not an assertion"),
    "must be an Assertion object"
  )
})

test_that("assertion_set accepts a list of assertions", {
  assertions <- list(
    assert_output(~TRUE, "OK1"),
    assert_output(~TRUE, "OK2")
  )

  set <- assertion_set(assertions)
  expect_length(set@assertions, 2)
})

# Test evaluate_assertion helper
test_that("evaluate_assertion returns pass for satisfied condition", {
  assertion <- assert_output(~ nchar(.x) < 100, "Too long")
  result <- dsprrr:::evaluate_assertion(assertion, "short text")

  expect_true(result$passed)
  expect_null(result$message)
})

test_that("evaluate_assertion returns failure for unsatisfied condition", {
  assertion <- assert_output(~ nchar(.x) < 5, "Too long")
  result <- dsprrr:::evaluate_assertion(assertion, "this is long")

  expect_false(result$passed)
  expect_equal(result$message, "Too long")
  expect_equal(result$type, "assert")
})

test_that("evaluate_assertion handles field extraction", {
  assertion <- assert_output(
    ~ nchar(.x) < 10,
    "Field too long",
    field = "answer"
  )
  result <- dsprrr:::evaluate_assertion(assertion, list(answer = "short"))

  expect_true(result$passed)
})

test_that("evaluate_assertion handles missing field gracefully", {
  assertion <- assert_output(~TRUE, "Test", field = "missing_field")

  # Should warn and return passed=TRUE (skipped)
  expect_warning(
    result <- dsprrr:::evaluate_assertion(assertion, list(other = "value")),
    "not found in output"
  )
  expect_true(result$passed)
})

test_that("evaluate_assertion handles condition errors gracefully", {
  assertion <- assert_output(~ stop("deliberate error"), "Test")

  expect_warning(
    result <- dsprrr:::evaluate_assertion(assertion, "input"),
    "raised error"
  )
  expect_false(result$passed)
})

# Test evaluate_assertion_set helper
test_that("evaluate_assertion_set correctly separates hard and soft failures", {
  set <- assertion_set(
    assert_output(~FALSE, "Hard fail"),
    suggest_output(~FALSE, "Soft fail"),
    assert_output(~TRUE, "Hard pass")
  )

  result <- dsprrr:::evaluate_assertion_set(set, "test")

  expect_false(result$all_passed)
  expect_equal(result$n_hard_failed, 1)
  expect_equal(result$n_soft_failed, 1)
  expect_length(result$hard_failures, 1)
  expect_length(result$soft_failures, 1)
})

test_that("evaluate_assertion_set returns all_passed when hard assertions pass", {
  set <- assertion_set(
    assert_output(~TRUE, "Hard pass"),
    suggest_output(~FALSE, "Soft fail") # Doesn't affect all_passed
  )

  result <- dsprrr:::evaluate_assertion_set(set, "test")

  expect_true(result$all_passed)
  expect_equal(result$n_soft_failed, 1)
})

# Test assertion helper functions
test_that("assert_length validates min/max length", {
  # Max only
  assertion <- assert_length("answer", max = 10)
  expect_true(S7::S7_inherits(assertion, Assertion))

  result <- dsprrr:::evaluate_assertion(assertion, list(answer = "short"))
  expect_true(result$passed)

  result <- dsprrr:::evaluate_assertion(
    assertion,
    list(answer = "this is way too long")
  )
  expect_false(result$passed)

  # Min only
  assertion_min <- assert_length("answer", min = 5)
  result <- dsprrr:::evaluate_assertion(assertion_min, list(answer = "hi"))
  expect_false(result$passed)

  # Both min and max
  assertion_both <- assert_length("answer", min = 2, max = 10)
  result <- dsprrr:::evaluate_assertion(assertion_both, list(answer = "good"))
  expect_true(result$passed)
})

test_that("assert_length requires at least min or max", {
  expect_error(
    assert_length("answer"),
    "At least one of"
  )
})

test_that("assert_contains validates substring presence", {
  assertion <- assert_contains("answer", "important")

  result <- dsprrr:::evaluate_assertion(
    assertion,
    list(answer = "This is important information")
  )
  expect_true(result$passed)

  result <- dsprrr:::evaluate_assertion(
    assertion,
    list(answer = "Nothing here")
  )
  expect_false(result$passed)
})

test_that("assert_contains supports case-insensitive matching", {
  assertion <- assert_contains("answer", "IMPORTANT", ignore_case = TRUE)

  result <- dsprrr:::evaluate_assertion(
    assertion,
    list(answer = "This is important")
  )
  expect_true(result$passed)
})

test_that("assert_not_contains validates substring absence", {
  assertion <- assert_not_contains("answer", "secret")

  result <- dsprrr:::evaluate_assertion(assertion, list(answer = "public info"))
  expect_true(result$passed)

  result <- dsprrr:::evaluate_assertion(assertion, list(answer = "secret data"))
  expect_false(result$passed)
})

test_that("assert_matches validates regex pattern", {
  assertion <- assert_matches("answer", "^[A-Z]", "Must start with capital")

  result <- dsprrr:::evaluate_assertion(assertion, list(answer = "Hello"))
  expect_true(result$passed)

  result <- dsprrr:::evaluate_assertion(assertion, list(answer = "hello"))
  expect_false(result$passed)
})

test_that("assert_not_matches validates pattern absence", {
  assertion <- assert_not_matches("url", "https?://", "Must not contain URLs")

  result <- dsprrr:::evaluate_assertion(assertion, list(url = "no url here"))
  expect_true(result$passed)

  result <- dsprrr:::evaluate_assertion(
    assertion,
    list(url = "visit https://example.com")
  )
  expect_false(result$passed)
})

test_that("assert_one_of validates against allowed values", {
  assertion <- assert_one_of("sentiment", c("positive", "negative", "neutral"))

  result <- dsprrr:::evaluate_assertion(assertion, list(sentiment = "positive"))
  expect_true(result$passed)

  result <- dsprrr:::evaluate_assertion(assertion, list(sentiment = "unknown"))
  expect_false(result$passed)
})

test_that("assert_one_of supports case-insensitive matching", {
  assertion <- assert_one_of("category", c("A", "B", "C"), ignore_case = TRUE)

  result <- dsprrr:::evaluate_assertion(assertion, list(category = "a"))
  expect_true(result$passed)
})

test_that("assert_custom allows custom conditions", {
  assertion <- assert_custom(
    ~ length(strsplit(.x$answer, " ")[[1]]) <= 3,
    "Answer must be at most 3 words"
  )

  result <- dsprrr:::evaluate_assertion(
    assertion,
    list(answer = "Three words here")
  )
  expect_true(result$passed)

  result <- dsprrr:::evaluate_assertion(
    assertion,
    list(answer = "This has more than three words")
  )
  expect_false(result$passed)
})

test_that("assert_not_empty validates non-empty content", {
  assertion <- assert_not_empty("answer")

  result <- dsprrr:::evaluate_assertion(assertion, list(answer = "content"))
  expect_true(result$passed)

  result <- dsprrr:::evaluate_assertion(assertion, list(answer = ""))
  expect_false(result$passed)

  result <- dsprrr:::evaluate_assertion(assertion, list(answer = "   "))
  expect_false(result$passed)
})

test_that("assert_range validates numeric range", {
  assertion <- assert_range("score", min = 0, max = 100)

  result <- dsprrr:::evaluate_assertion(assertion, list(score = 50))
  expect_true(result$passed)

  result <- dsprrr:::evaluate_assertion(assertion, list(score = -1))
  expect_false(result$passed)

  result <- dsprrr:::evaluate_assertion(assertion, list(score = 101))
  expect_false(result$passed)
})

test_that("assert_range handles non-numeric gracefully", {
  assertion <- assert_range("score", min = 0, max = 100)

  result <- dsprrr:::evaluate_assertion(assertion, list(score = "not a number"))
  expect_false(result$passed)
})

test_that("assert_range requires at least min or max", {
  expect_error(
    assert_range("score"),
    "At least one of"
  )
})

test_that("assertion helpers support suggest type", {
  # Test that type = "suggest" works for helper functions
  assertion <- assert_length("answer", max = 10, type = "suggest")
  expect_equal(assertion@type, "suggest")

  assertion <- assert_contains("answer", "x", type = "suggest")
  expect_equal(assertion@type, "suggest")
})

# Test print methods
test_that("Assertion print method works", {
  assertion <- assert_output(~TRUE, "Test message", field = "answer")
  # S7 print methods use cli which may not be captured by expect_output
  expect_no_error(print(assertion))
})

test_that("AssertionSet print method works", {
  set <- assertion_set(
    assert_output(~TRUE, "A"),
    suggest_output(~TRUE, "B")
  )
  # S7 print methods use cli which may not be captured by expect_output
  expect_no_error(print(set))
})
