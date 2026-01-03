# --- as_vitals_solver tests ---

test_that("as_vitals_solver returns vitals-compatible results", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Repeat"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, ...) {
        lines <- strsplit(prompt, "\n")[[1]]
        tail(lines, 1L)
      }
    ),
    class = "Chat"
  )

  solver <- as_vitals_solver(mod, .llm = mock_llm)

  inputs <- data.frame(text = c("foo", "bar"), stringsAsFactors = FALSE)
  result <- solver(inputs)

  expect_equal(result$result, list("foo", "bar"))
  expect_equal(length(result$solver_chat), 2)
  expect_true(all(vapply(
    result$solver_chat,
    inherits,
    logical(1),
    what = "Chat"
  )))
  expect_equal(length(result$metadata), 2)
  expect_true(all(vapply(
    result$metadata,
    function(x) "prompt" %in% names(x),
    logical(1)
  )))

  simple_solver <- as_vitals_solver(
    mod,
    .llm = mock_llm,
    .return_format = "simple"
  )
  simple_result <- simple_solver(inputs)
  expect_equal(simple_result$result, list("foo", "bar"))
  expect_equal(simple_result$solver_chat, list(NULL, NULL))
  expect_equal(simple_result$metadata, list(list(), list()))
})

test_that("as_vitals_solver rejects non-module input", {
  expect_error(
    as_vitals_solver("not a module"),
    "requires an R6 Module object"
  )

  expect_error(
    as_vitals_solver(list(a = 1)),
    "requires an R6 Module object"
  )

  expect_error(
    as_vitals_solver(NULL),
    "requires an R6 Module object"
  )
})

test_that("as_vitals_solver handles single-row inputs", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "echoed"),
    class = "Chat"
  )

  solver <- as_vitals_solver(mod, .llm = mock_llm)
  result <- solver(data.frame(text = "single", stringsAsFactors = FALSE))

  expect_equal(length(result$result), 1)
  expect_equal(result$result[[1]], "echoed")
})

test_that("as_vitals_solver converts list inputs to data frame", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "result"),
    class = "Chat"
  )

  solver <- as_vitals_solver(mod, .llm = mock_llm)

  # Pass list instead of data frame
  result <- solver(list(text = c("a", "b")))

  expect_equal(length(result$result), 2)
})

test_that("as_vitals_solver structured format includes metadata", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "response"),
    class = "Chat"
  )

  solver <- as_vitals_solver(
    mod,
    .llm = mock_llm,
    .return_format = "structured"
  )
  result <- solver(data.frame(text = "test", stringsAsFactors = FALSE))

  expect_true("latency_ms" %in% names(result$metadata[[1]]))
  expect_true("prompt" %in% names(result$metadata[[1]]))
  expect_true("timestamp" %in% names(result$metadata[[1]]))
})

# --- as_dsprrr_metric tests ---

test_that("as_dsprrr_metric wraps vitals scorers", {
  vitals_scorer <- function(samples) {
    tibble::tibble(
      score = ifelse(
        vapply(samples$result, `[[`, character(1), 1) ==
          vapply(samples$target, `[[`, character(1), 1),
        "C",
        "I"
      )
    )
  }

  metric <- as_dsprrr_metric(vitals_scorer)

  expected <- data.frame(target = "answer", stringsAsFactors = FALSE)
  expect_equal(metric("answer", expected), 1)
  expect_equal(metric("wrong", expected), 0)

  # Numeric scorer
  numeric_scorer <- function(samples) tibble::tibble(score = 0.4)
  metric_num <- as_dsprrr_metric(numeric_scorer)
  expect_equal(metric_num("any", expected), 0.4)
})

test_that("as_dsprrr_metric rejects non-function input", {
  expect_error(
    as_dsprrr_metric("not a function"),
    "vitals_scorer must be a function"
  )

  expect_error(
    as_dsprrr_metric(list(score = 1)),
    "vitals_scorer must be a function"
  )
})

test_that("as_dsprrr_metric handles logical scores", {
  logical_scorer <- function(samples) {
    tibble::tibble(score = TRUE)
  }

  metric <- as_dsprrr_metric(logical_scorer)
  result <- metric("prediction", data.frame(target = "target"))

  expect_equal(result, 1)

  # FALSE case
  false_scorer <- function(samples) tibble::tibble(score = FALSE)
  metric_false <- as_dsprrr_metric(false_scorer)
  expect_equal(metric_false("prediction", data.frame(target = "target")), 0)
})

test_that("as_dsprrr_metric handles numeric string scores", {
  string_numeric_scorer <- function(samples) {
    tibble::tibble(score = "0.75")
  }

  metric <- as_dsprrr_metric(string_numeric_scorer)
  result <- metric("prediction", data.frame(target = "target"))

  expect_equal(result, 0.75)
})

test_that("as_dsprrr_metric handles various correct/incorrect labels", {
  # Test "correct"
  correct_scorer <- function(samples) tibble::tibble(score = "correct")
  metric <- as_dsprrr_metric(correct_scorer)
  expect_equal(metric("p", data.frame()), 1)

  # Test "CORRECT" (case insensitive)
  upper_correct <- function(samples) tibble::tibble(score = "CORRECT")
  metric2 <- as_dsprrr_metric(upper_correct)
  expect_equal(metric2("p", data.frame()), 1)

  # Test "pass"
  pass_scorer <- function(samples) tibble::tibble(score = "pass")
  metric3 <- as_dsprrr_metric(pass_scorer)
  expect_equal(metric3("p", data.frame()), 1)

  # Test "incorrect"
  incorrect_scorer <- function(samples) tibble::tibble(score = "incorrect")
  metric4 <- as_dsprrr_metric(incorrect_scorer)
  expect_equal(metric4("p", data.frame()), 0)

  # Test "fail"
  fail_scorer <- function(samples) tibble::tibble(score = "fail")
  metric5 <- as_dsprrr_metric(fail_scorer)
  expect_equal(metric5("p", data.frame()), 0)

  # Test "I" (shorthand for incorrect)
  i_scorer <- function(samples) tibble::tibble(score = "I")
  metric6 <- as_dsprrr_metric(i_scorer)
  expect_equal(metric6("p", data.frame()), 0)
})

test_that("as_dsprrr_metric warns on unrecognized score values", {
  weird_scorer <- function(samples) {
    tibble::tibble(score = "unknown_value")
  }

  metric <- as_dsprrr_metric(weird_scorer)

  expect_warning(
    result <- metric("prediction", data.frame(target = "target")),
    "Unrecognised vitals score value"
  )

  expect_true(is.na(result))
})

test_that("as_dsprrr_metric warns on empty scorer results", {
  empty_scorer <- function(samples) {
    tibble::tibble(score = character(0))
  }

  metric <- as_dsprrr_metric(empty_scorer)

  expect_warning(
    result <- metric("prediction", data.frame(target = "target")),
    "vitals scorer returned no results"
  )

  expect_true(is.na(result))
})

test_that("as_dsprrr_metric handles custom column names", {
  custom_scorer <- function(samples) {
    # Check that custom column names are used
    expect_true("my_input" %in% names(samples))
    expect_true("my_target" %in% names(samples))
    expect_true("my_result" %in% names(samples))
    tibble::tibble(score = 0.9)
  }

  metric <- as_dsprrr_metric(
    custom_scorer,
    input_column = "my_input",
    target_column = "my_target",
    result_column = "my_result"
  )

  expected <- data.frame(my_input = "input", my_target = "target")
  result <- metric("prediction", expected)

  expect_equal(result, 0.9)
})

test_that("as_dsprrr_metric uses NA for missing columns in expected_row", {
  column_check_scorer <- function(samples) {
    # input_column should be NA when not in expected_row
    expect_true(is.na(samples$input[[1]]))
    # target should be present
    expect_equal(samples$target[[1]], "expected")
    tibble::tibble(score = 1)
  }

  metric <- as_dsprrr_metric(column_check_scorer)

  # expected_row without 'input' column
  expected <- data.frame(target = "expected", other = "value")
  result <- metric("prediction", expected)

  expect_equal(result, 1)
})

test_that("as_dsprrr_metric handles NULL scorer return", {
  null_scorer <- function(samples) NULL

  metric <- as_dsprrr_metric(null_scorer)

  expect_warning(
    result <- metric("prediction", data.frame(target = "t")),
    "vitals scorer returned no results"
  )

  expect_true(is.na(result))
})

test_that("as_dsprrr_metric handles vitals-style list return", {
  # vitals scorers return lists with $score element
  list_scorer <- function(samples) list(score = 1)

  metric <- as_dsprrr_metric(list_scorer)
  result <- metric("prediction", data.frame(target = "t"))

  expect_equal(result, 1)
})

test_that("as_dsprrr_metric handles factor scores", {
  # vitals uses ordered factors with levels I < P < C
  correct_factor <- function(samples) {
    list(score = factor("C", levels = c("I", "P", "C"), ordered = TRUE))
  }
  metric_c <- as_dsprrr_metric(correct_factor)
  expect_equal(metric_c("p", data.frame()), 1)

  incorrect_factor <- function(samples) {
    list(score = factor("I", levels = c("I", "P", "C"), ordered = TRUE))
  }
  metric_i <- as_dsprrr_metric(incorrect_factor)
  expect_equal(metric_i("p", data.frame()), 0)

  partial_factor <- function(samples) {
    list(score = factor("P", levels = c("I", "P", "C"), ordered = TRUE))
  }
  metric_p <- as_dsprrr_metric(partial_factor)
  expect_equal(metric_p("p", data.frame()), 0.5)
})

test_that("as_dsprrr_metric handles partial credit scores", {
  # Test "P" shorthand
  p_scorer <- function(samples) tibble::tibble(score = "P")
  metric_p <- as_dsprrr_metric(p_scorer)
  expect_equal(metric_p("p", data.frame()), 0.5)

  # Test "partial" full word
  partial_scorer <- function(samples) tibble::tibble(score = "partial")
  metric_partial <- as_dsprrr_metric(partial_scorer)
  expect_equal(metric_partial("p", data.frame()), 0.5)

  # Test case insensitive
  upper_partial <- function(samples) tibble::tibble(score = "PARTIAL")
  metric_upper <- as_dsprrr_metric(upper_partial)
  expect_equal(metric_upper("p", data.frame()), 0.5)
})

test_that("as_dsprrr_metric warns on list return without score element", {
  no_score_scorer <- function(samples) list(explanation = "good answer")
  metric <- as_dsprrr_metric(no_score_scorer)

  expect_warning(
    result <- metric("prediction", data.frame(target = "t")),
    "vitals scorer returned no score"
  )
  expect_true(is.na(result))
})

test_that("as_dsprrr_metric preserves numeric precision", {
  precision_scorer <- function(samples) {
    tibble::tibble(score = 0.123456789)
  }

  metric <- as_dsprrr_metric(precision_scorer)
  result <- metric("prediction", data.frame(target = "t"))

  expect_equal(result, 0.123456789)
})

test_that("as_dsprrr_metric works with complex predictions", {
  # Test with list prediction
  list_scorer <- function(samples) {
    pred <- samples$result[[1]]
    score <- if (is.list(pred) && pred$correct) 1 else 0
    tibble::tibble(score = score)
  }

  metric <- as_dsprrr_metric(list_scorer)

  # List prediction
  result <- metric(
    list(correct = TRUE, value = "test"),
    data.frame(target = "t")
  )
  expect_equal(result, 1)

  result2 <- metric(
    list(correct = FALSE, value = "test"),
    data.frame(target = "t")
  )
  expect_equal(result2, 0)
})

# --- Pre-built vitals metrics tests ---

test_that("metric_model_graded_qa requires vitals package", {
  skip_if_not_installed("vitals")

  # Should create a function
  metric <- metric_model_graded_qa()
  expect_true(is.function(metric))
})

test_that("metric_model_graded_fact requires vitals package", {
  skip_if_not_installed("vitals")

  # Should create a function
  metric <- metric_model_graded_fact()
  expect_true(is.function(metric))
})

test_that("metric_detect_match requires vitals and validates location", {
  skip_if_not_installed("vitals")

  # Default location "end"
  metric <- metric_detect_match()
  expect_true(is.function(metric))

  # Explicit locations
  metric_begin <- metric_detect_match(location = "begin")
  expect_true(is.function(metric_begin))

  metric_any <- metric_detect_match(location = "any")
  expect_true(is.function(metric_any))

  metric_exact <- metric_detect_match(location = "exact")
  expect_true(is.function(metric_exact))

  # Invalid location should error
  expect_error(
    metric_detect_match(location = "invalid"),
    "should be one of"
  )
})

test_that("metric_detect_includes requires vitals package", {
  skip_if_not_installed("vitals")

  metric <- metric_detect_includes()
  expect_true(is.function(metric))

  # Case sensitive option
  metric_cs <- metric_detect_includes(case_sensitive = TRUE)
  expect_true(is.function(metric_cs))
})

test_that("metric_detect_pattern requires vitals and pattern", {
  skip_if_not_installed("vitals")

  metric <- metric_detect_pattern(pattern = "\\d+")
  expect_true(is.function(metric))

  # With case_sensitive option
  metric_cs <- metric_detect_pattern(pattern = "foo", case_sensitive = TRUE)
  expect_true(is.function(metric_cs))
})

test_that("vitals metrics accept custom column names", {
  skip_if_not_installed("vitals")

  metric <- metric_detect_match(
    input_column = "question",
    target_column = "answer",
    result_column = "prediction"
  )
  expect_true(is.function(metric))
})

test_that("metric_detect_match scores correctly", {
  skip_if_not_installed("vitals")

  # Test "end" location - target should be at end of result
  metric <- metric_detect_match(location = "end")

  # Should score 1 when target at end
  score <- metric("The capital is Paris", data.frame(target = "Paris"))
  expect_equal(score, 1)

  # Should score 0 when target not at end
  score2 <- metric("Paris is the capital", data.frame(target = "Paris"))
  expect_equal(score2, 0)
})

test_that("metric_detect_includes scores correctly", {
  skip_if_not_installed("vitals")

  metric <- metric_detect_includes()

  # Should score 1 when target is included anywhere
  score <- metric("Paris is the capital", data.frame(target = "Paris"))
  expect_equal(score, 1)

  # Should score 0 when target not included
  score2 <- metric("London is the capital", data.frame(target = "Paris"))
  expect_equal(score2, 0)
})

test_that("metric_detect_pattern scores correctly", {
  skip_if_not_installed("vitals")

  # detect_pattern requires capture groups - extracts them and checks against target
  metric <- metric_detect_pattern(pattern = "([0-9]+)")

  # Should score 1 when captured group matches target
  score <- metric("The answer is 42", data.frame(target = "42"))
  expect_equal(score, 1)

  # Should score 0 when captured group doesn't match target
  score2 <- metric("The answer is 42", data.frame(target = "99"))
  expect_equal(score2, 0)
})

# --- as_vitals_task tests ---

test_that("as_vitals_task requires vitals package", {
  skip_if_not_installed("vitals")

  sig <- Signature(
    inputs = list(input(name = "input", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict")

  dataset <- tibble::tibble(
    input = c("hello", "world"),
    target = c("hello", "world")
  )

  task <- as_vitals_task(
    module = mod,
    dataset = dataset,
    scorer = vitals::detect_includes()
  )

  expect_s3_class(task, "Task")
})

test_that("as_vitals_task rejects non-module input", {
  skip_if_not_installed("vitals")

  dataset <- tibble::tibble(input = "test", target = "test")

  expect_error(
    as_vitals_task(module = "not a module", dataset = dataset),
    "requires a dsprrr Module"
  )

  expect_error(
    as_vitals_task(module = list(a = 1), dataset = dataset),
    "requires a dsprrr Module"
  )
})

test_that("as_vitals_task rejects non-dataframe dataset", {
  skip_if_not_installed("vitals")

  sig <- Signature(
    inputs = list(input(name = "input", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict")

  expect_error(
    as_vitals_task(module = mod, dataset = "not a dataframe"),
    "dataset must be a data frame"
  )

  expect_error(
    as_vitals_task(module = mod, dataset = list(input = "a", target = "b")),
    "dataset must be a data frame"
  )
})

test_that("as_vitals_task requires input and target columns", {
  skip_if_not_installed("vitals")

  sig <- Signature(
    inputs = list(input(name = "input", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict")

  # Missing target
  expect_error(
    as_vitals_task(
      module = mod,
      dataset = data.frame(input = "test")
    ),
    "Missing.*target"
  )

  # Missing input
  expect_error(
    as_vitals_task(
      module = mod,
      dataset = data.frame(target = "test")
    ),
    "Missing.*input"
  )

  # Missing both
  expect_error(
    as_vitals_task(
      module = mod,
      dataset = data.frame(other = "test")
    ),
    "Missing"
  )
})

test_that("as_vitals_task accepts custom parameters", {
  skip_if_not_installed("vitals")

  sig <- Signature(
    inputs = list(input(name = "input", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict")

  dataset <- tibble::tibble(
    input = c("hello"),
    target = c("hello")
  )

  # Create task with custom epochs and name
  task <- as_vitals_task(
    module = mod,
    dataset = dataset,
    scorer = vitals::detect_includes(),
    name = "custom_name",
    epochs = 3L
  )

  expect_s3_class(task, "Task")
})

test_that("as_vitals_task uses default scorer when not provided", {
  skip_if_not_installed("vitals")

  sig <- Signature(
    inputs = list(input(name = "input", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict")

  dataset <- tibble::tibble(
    input = c("hello"),
    target = c("hello")
  )

  # Should not error when scorer is NULL (uses default)
  task <- as_vitals_task(
    module = mod,
    dataset = dataset
  )

  expect_s3_class(task, "Task")
})
