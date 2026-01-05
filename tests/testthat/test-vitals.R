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

test_that("as_vitals_task requires signature inputs and target columns", {
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

  # Missing signature input column
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

test_that("as_vitals_task works with non-standard input column names", {
  skip_if_not_installed("vitals")

  # Module with "question" input instead of "input"
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig, type = "predict")

  # Should work with question + target columns
  dataset <- tibble::tibble(
    question = c("What is 2+2?", "What is the capital of France?"),
    target = c("4", "Paris")
  )

  task <- as_vitals_task(
    module = mod,
    dataset = dataset,
    scorer = vitals::detect_includes()
  )

  expect_s3_class(task, "Task")
  samples <- task$get_samples()
  expect_equal(nrow(samples), 2)

  # Should error if using wrong column name
  expect_error(
    as_vitals_task(
      module = mod,
      dataset = data.frame(input = "test", target = "test")
    ),
    "Missing.*question"
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
    input = c("hello", "world"),
    target = c("hello", "world")
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

  # Verify Task configuration was passed through correctly
  # Access private fields to verify configuration
  task_private <- task$.__enclos_env__$private
  expect_equal(task_private$epochs, 3L)

  # Verify dataset was passed through
  samples <- task$get_samples()
  expect_equal(nrow(samples), 2)
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

# --- as_vitals_samples tests ---

test_that("as_vitals_samples converts traces to vitals format", {
  mock_traces <- tibble::tibble(
    timestamp = Sys.time() - c(0, 1, 2),
    latency_ms = c(100, 150, 200),
    model = c("gpt-4o-mini", "gpt-4o-mini", "claude-3-5-sonnet"),
    input_tokens = c(100L, 200L, 150L),
    output_tokens = c(50L, 100L, 75L),
    total_tokens = c(150L, 300L, 225L),
    cost = c(0.01, 0.02, 0.03),
    prompt_length = c(50L, 100L, 75L),
    prompt = c("Hello world", "How are you?", "What is AI?"),
    output = list("Hi!", "I'm good!", "Artificial intelligence")
  )

  result <- as_vitals_samples(mock_traces)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)
  expect_equal(
    names(result),
    c("id", "input", "result", "solver_metadata", "model", "epoch")
  )

  # Check IDs
  expect_equal(result$id, c("trace_0001", "trace_0002", "trace_0003"))

  # Check inputs come from prompt field
  expect_equal(result$input, mock_traces$prompt)

  # Check results are lists
  expect_true(is.list(result$result))
  expect_equal(result$result[[1]], "Hi!")

  # Check solver_metadata contains trace fields
  expect_true(is.list(result$solver_metadata[[1]]))
  expect_equal(result$solver_metadata[[1]]$latency_ms, 100)
  expect_equal(result$solver_metadata[[1]]$input_tokens, 100L)

  # Check epoch is always 1
  expect_equal(unique(result$epoch), 1L)
})

test_that("as_vitals_samples handles empty traces", {
  empty_traces <- tibble::tibble(
    timestamp = as.POSIXct(character(0)),
    latency_ms = numeric(0),
    model = character(0),
    input_tokens = integer(0),
    output_tokens = integer(0),
    cost = numeric(0)
  )

  result <- as_vitals_samples(empty_traces)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_equal(
    names(result),
    c("id", "input", "result", "solver_metadata", "model", "epoch")
  )
})

test_that("as_vitals_samples respects input_column parameter", {
  mock_traces <- tibble::tibble(
    timestamp = Sys.time(),
    latency_ms = 100,
    model = "gpt-4o-mini",
    input_tokens = 50L,
    output_tokens = 25L,
    cost = 0.01,
    custom_input = "My custom input"
  )

  result <- as_vitals_samples(mock_traces, input_column = "custom_input")

  expect_equal(result$input, "My custom input")
})

test_that("as_vitals_samples includes chat objects when requested", {
  mock_chat <- structure(list(), class = "Chat")

  mock_traces <- tibble::tibble(
    timestamp = Sys.time(),
    latency_ms = 100,
    model = "gpt-4o-mini",
    input_tokens = 50L,
    output_tokens = 25L,
    cost = 0.01,
    solver_chat = list(mock_chat)
  )

  result_no_chats <- as_vitals_samples(mock_traces, include_chats = FALSE)
  expect_false("solver_chat" %in% names(result_no_chats))

  result_with_chats <- as_vitals_samples(mock_traces, include_chats = TRUE)
  expect_true("solver_chat" %in% names(result_with_chats))
})

test_that("as_vitals_samples errors on non-dataframe input", {
  expect_error(
    as_vitals_samples("not a data frame"),
    "traces must be a data frame"
  )

  expect_error(
    as_vitals_samples(list(a = 1)),
    "traces must be a data frame"
  )
})

# --- as_dsprrr_traces tests ---

test_that("as_dsprrr_traces converts vitals samples to traces format", {
  mock_samples <- tibble::tibble(
    id = c("sample_1", "sample_2"),
    input = c("Hello", "World"),
    result = list("Hi there!", "Hello World!"),
    solver_metadata = list(
      list(
        latency_ms = 100,
        input_tokens = 50L,
        output_tokens = 25L,
        cost = 0.01
      ),
      list(
        latency_ms = 150,
        input_tokens = 75L,
        output_tokens = 40L,
        cost = 0.02
      )
    ),
    model = c("gpt-4o-mini", "claude-3-5-sonnet")
  )

  result <- as_dsprrr_traces(mock_samples)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)

  # Check expected columns
  expect_true("timestamp" %in% names(result))
  expect_true("latency_ms" %in% names(result))
  expect_true("input_tokens" %in% names(result))
  expect_true("output_tokens" %in% names(result))
  expect_true("total_tokens" %in% names(result))
  expect_true("cost" %in% names(result))
  expect_true("model" %in% names(result))
  expect_true("prompt" %in% names(result))
  expect_true("output" %in% names(result))

  # Check values extracted from metadata
  expect_equal(result$latency_ms, c(100, 150))
  expect_equal(result$input_tokens, c(50L, 75L))
  expect_equal(result$output_tokens, c(25L, 40L))
  expect_equal(result$cost, c(0.01, 0.02))

  # Check total_tokens calculated
  expect_equal(result$total_tokens, c(75L, 115L))

  # Check prompt comes from input
  expect_equal(result$prompt, c("Hello", "World"))
})

test_that("as_dsprrr_traces handles empty samples", {
  empty_samples <- tibble::tibble(
    id = character(0),
    input = character(0),
    result = list(),
    solver_metadata = list()
  )

  result <- as_dsprrr_traces(empty_samples)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true("timestamp" %in% names(result))
  expect_true("latency_ms" %in% names(result))
})

test_that("as_dsprrr_traces respects include_prompts and include_outputs", {
  mock_samples <- tibble::tibble(
    id = "sample_1",
    input = "Hello",
    result = list("Hi!"),
    solver_metadata = list(list(latency_ms = 100))
  )

  result_no_prompts <- as_dsprrr_traces(mock_samples, include_prompts = FALSE)
  expect_false("prompt" %in% names(result_no_prompts))

  result_no_outputs <- as_dsprrr_traces(mock_samples, include_outputs = FALSE)
  expect_false("output" %in% names(result_no_outputs))

  result_full <- as_dsprrr_traces(mock_samples)
  expect_true("prompt" %in% names(result_full))
  expect_true("output" %in% names(result_full))
})

test_that("as_dsprrr_traces errors on non-dataframe input", {
  expect_error(
    as_dsprrr_traces("not a data frame"),
    "samples must be a data frame"
  )
})

# --- summarize_traces_df tests ---

test_that("summarize_traces_df provides summary statistics", {
  mock_traces <- tibble::tibble(
    timestamp = Sys.time() - c(0, 1, 2),
    latency_ms = c(100, 150, 200),
    model = c("gpt-4o-mini", "gpt-4o-mini", "claude-3-5-sonnet"),
    input_tokens = c(100L, 200L, 150L),
    output_tokens = c(50L, 100L, 75L),
    total_tokens = c(150L, 300L, 225L),
    cost = c(0.01, 0.02, 0.03)
  )

  result <- summarize_traces_df(mock_traces)

  expect_s3_class(result, "dsprrr_trace_summary")
  expect_equal(result$n_traces, 3L)
  expect_equal(result$total_tokens, 675L)
  expect_equal(result$total_input_tokens, 450L)
  expect_equal(result$total_output_tokens, 225L)
  expect_equal(result$total_cost, 0.06)
  expect_equal(result$total_latency_ms, 450)
  expect_equal(result$avg_latency_ms, 150)

  # Check model usage
  expect_equal(nrow(result$model_usage), 2)
})

test_that("summarize_traces_df handles empty traces", {
  empty_traces <- tibble::tibble(
    timestamp = as.POSIXct(character(0)),
    latency_ms = numeric(0),
    input_tokens = integer(0),
    output_tokens = integer(0),
    total_tokens = integer(0),
    cost = numeric(0)
  )

  result <- summarize_traces_df(empty_traces)

  expect_s3_class(result, "dsprrr_trace_summary")
  expect_equal(result$n_traces, 0L)
  expect_equal(result$total_tokens, 0L)
  expect_true(is.na(result$avg_latency_ms))
})

test_that("summarize_traces_df errors on non-dataframe input", {
  expect_error(
    summarize_traces_df("not a data frame"),
    "traces must be a data frame"
  )
})

# --- Round-trip conversion tests ---

test_that("traces survive round-trip conversion", {
  # Create original traces
  original_traces <- tibble::tibble(
    timestamp = Sys.time() - c(0, 1),
    latency_ms = c(100, 150),
    model = c("gpt-4o-mini", "claude-3-5-sonnet"),
    input_tokens = c(100L, 150L),
    output_tokens = c(50L, 75L),
    total_tokens = c(150L, 225L),
    cost = c(0.01, 0.02),
    prompt_length = c(10L, 15L),
    prompt = c("Hello", "World"),
    output = list("Hi!", "Bye!")
  )

  # Convert to vitals samples
  samples <- as_vitals_samples(original_traces)
  expect_equal(nrow(samples), 2)

  # Convert back to traces
  recovered_traces <- as_dsprrr_traces(samples)
  expect_equal(nrow(recovered_traces), 2)

  # Check key fields preserved
  expect_equal(recovered_traces$model, original_traces$model)
  expect_equal(recovered_traces$latency_ms, original_traces$latency_ms)
  expect_equal(recovered_traces$input_tokens, original_traces$input_tokens)
  expect_equal(recovered_traces$output_tokens, original_traces$output_tokens)
  expect_equal(recovered_traces$cost, original_traces$cost)
  expect_equal(recovered_traces$prompt, original_traces$prompt)
})
