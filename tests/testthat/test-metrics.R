test_that("metric_exact_match works correctly", {
  metric <- metric_exact_match()

  # Basic string matching
  expect_true(metric("hello", "hello"))
  expect_false(metric("hello", "world"))

  # Numeric conversion
  expect_true(metric(42, "42"))
  expect_true(metric("42", 42))

  # Whitespace normalization
  expect_true(metric("  hello  world  ", "hello world"))

  # Case sensitivity
  metric_case <- metric_exact_match(ignore_case = TRUE)
  expect_true(metric_case("Hello", "hello"))
  expect_true(metric_case("WORLD", "world"))

  metric_case_sensitive <- metric_exact_match(ignore_case = FALSE)
  expect_false(metric_case_sensitive("Hello", "hello"))
})

test_that("metric_exact_match with field extraction", {
  metric <- metric_exact_match(field = "sentiment")

  pred <- list(sentiment = "positive", confidence = 0.9)
  exp <- list(sentiment = "positive", confidence = 0.8)
  expect_true(metric(pred, exp))

  pred2 <- list(sentiment = "negative", confidence = 0.9)
  expect_false(metric(pred2, exp))

  # Error on non-list when field specified
  expect_error(metric("positive", exp), "Cannot extract field")
})

test_that("metric_f1 calculates correct scores", {
  metric <- metric_f1()

  # Identical strings
  expect_equal(metric("the quick brown fox", "the quick brown fox"), 1.0)

  # Partial overlap
  score <- metric("the quick brown fox", "the fast brown fox")
  expect_true(score > 0.5 && score < 1.0)

  # No overlap
  expect_equal(metric("hello world", "goodbye universe"), 0.0)

  # Empty strings
  expect_equal(metric("", ""), 1.0)

  # Normalization
  metric_normalized <- metric_f1(normalize = TRUE)
  score_norm <- metric_normalized("THE QUICK, BROWN FOX!", "the quick brown fox")
  expect_equal(score_norm, 1.0)
})

test_that("metric_f1 with field extraction", {
  metric <- metric_f1(field = "answer")

  pred <- list(answer = "the quick brown fox", confidence = 0.9)
  exp <- list(answer = "the quick brown fox", other = "data")
  expect_equal(metric(pred, exp), 1.0)

  pred2 <- list(answer = "completely different", confidence = 0.9)
  expect_true(metric(pred2, exp) < 0.5)
})

test_that("metric_contains works with patterns", {
  # Fixed string
  metric_fixed <- metric_contains("positive")
  expect_true(metric_fixed("The result is positive", NULL))
  expect_false(metric_fixed("The result is negative", NULL))

  # Case insensitive
  metric_case <- metric_contains("positive", ignore_case = TRUE)
  expect_true(metric_case("The result is POSITIVE", NULL))

  # Regex pattern
  metric_regex <- metric_contains("\\d+", fixed = FALSE)
  expect_true(metric_regex("The answer is 42", NULL))
  expect_false(metric_regex("No numbers here", NULL))

  # With field extraction
  metric_field <- metric_contains("pos", field = "sentiment")
  pred <- list(sentiment = "positive", score = 0.9)
  expect_true(metric_field(pred, NULL))
})

test_that("metric_custom validates and wraps functions", {
  # Valid custom metric
  length_metric <- metric_custom(function(pred, exp) {
    nchar(as.character(pred)) == nchar(as.character(exp))
  }, name = "length_match")

  expect_true(length_metric("hello", "world"))
  expect_false(length_metric("hi", "world"))

  # Numeric metric
  score_metric <- metric_custom(function(pred, exp) {
    0.75
  })
  expect_equal(score_metric("anything", "anything"), 0.75)

  # Invalid return type
  bad_metric <- metric_custom(function(pred, exp) {
    list(score = 0.5)
  })
  expect_error(bad_metric("test", "test"), "must return logical or numeric")

  # Error in custom function
  error_metric <- metric_custom(function(pred, exp) {
    stop("Custom error")
  }, name = "error_metric")
  expect_error(error_metric("test", "test"), "Error in metric error_metric")

  # Non-function input
  expect_error(metric_custom("not a function"), "fn must be a function")
})

test_that("metric_field_match checks multiple fields", {
  # Single field
  metric_single <- metric_field_match("sentiment")
  pred <- list(sentiment = "positive", confidence = 0.9)
  exp <- list(sentiment = "positive", confidence = 0.8)
  expect_true(metric_single(pred, exp))

  pred2 <- list(sentiment = "negative", confidence = 0.9)
  expect_false(metric_single(pred2, exp))

  # Multiple fields with AND
  metric_all <- metric_field_match(c("sentiment", "confidence"), require_all = TRUE)
  expect_false(metric_all(pred, exp))  # confidence differs

  pred3 <- list(sentiment = "positive", confidence = 0.8)
  expect_true(metric_all(pred3, exp))  # both match

  # Multiple fields with OR
  metric_any <- metric_field_match(c("sentiment", "confidence"), require_all = FALSE)
  expect_true(metric_any(pred, exp))  # sentiment matches

  # Invalid input
  expect_error(metric_field_match(character(0)), "non-empty character vector")
  expect_error(metric_field_match(123), "non-empty character vector")
})

test_that("metric_threshold converts numeric to logical", {
  # Basic threshold
  base_metric <- metric_f1()
  metric <- metric_threshold(base_metric, threshold = 0.8)

  expect_false(metric("the quick fox", "the fast brown fox"))
  expect_true(metric("the quick brown fox", "the quick brown fox"))

  # Different comparisons
  metric_gt <- metric_threshold(base_metric, 0.5, ">")
  expect_true(metric_gt("the quick brown", "the quick fox"))

  metric_eq <- metric_threshold(base_metric, 1.0, "==")
  expect_true(metric_eq("exact match", "exact match"))
  expect_false(metric_eq("not exact", "exact match"))

  # Invalid inputs
  expect_error(metric_threshold("not a function"), "must be a function")
  expect_error(metric_threshold(base_metric, "not numeric"), "single numeric value")

  # Non-numeric base metric
  bool_metric <- metric_exact_match()
  threshold_bool <- metric_threshold(bool_metric, 0.5)
  expect_error(threshold_bool("test", "test"), "must return numeric")
})

test_that("helper functions work correctly", {
  # normalize_whitespace
  expect_equal(normalize_whitespace("  hello   world  "), "hello world")
  expect_equal(normalize_whitespace("\t\nhello\n\tworld\n"), "hello world")

  # normalize_text
  expect_equal(normalize_text("Hello, World!"), "hello world")
  expect_equal(normalize_text("Test-123."), "test 123")

  # extract_field
  data <- list(a = 1, b = 2, c = list(nested = 3))
  expect_equal(extract_field(data, "a"), 1)
  expect_equal(extract_field(data, "c"), list(nested = 3))
  expect_null(extract_field(data, "missing"))
  expect_error(extract_field("not a list", "field"), "Cannot extract field")
})