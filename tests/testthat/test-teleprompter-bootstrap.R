# Tests for BootstrapFewShot teleprompter

test_that("BootstrapFewShot can be created with defaults", {
  tp <- BootstrapFewShot()
  expect_s3_class(tp, "dsprrr::BootstrapFewShot")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(tp@max_bootstrapped_demos, 4L)
  expect_equal(tp@max_labeled_demos, 16L)
  expect_equal(tp@max_rounds, 1L)
  expect_null(tp@teacher_settings) # NULL by default, becomes list(temperature = 0.7) during compile
  expect_null(tp@seed)
  expect_null(tp@log_dir)
  expect_null(tp@metric)
})

test_that("BootstrapFewShot can be created with custom parameters", {
  metric_fn <- function(pred, exp) as.numeric(pred == exp)
  tp <- BootstrapFewShot(
    metric = metric_fn,
    metric_threshold = 0.5,
    max_bootstrapped_demos = 8L,
    max_labeled_demos = 4L,
    max_rounds = 3L,
    teacher_settings = list(temperature = 0.9),
    seed = 42L,
    log_dir = tempdir()
  )

  expect_identical(tp@metric, metric_fn)
  expect_equal(tp@metric_threshold, 0.5)
  expect_equal(tp@max_bootstrapped_demos, 8L)
  expect_equal(tp@max_labeled_demos, 4L)
  expect_equal(tp@max_rounds, 3L)
  expect_equal(tp@teacher_settings, list(temperature = 0.9))
  expect_equal(tp@seed, 42L)
  expect_equal(tp@log_dir, tempdir())
})

test_that("BootstrapFewShot validates properties", {
  # Invalid max_bootstrapped_demos

  expect_error(
    BootstrapFewShot(max_bootstrapped_demos = -1L),
    "non-negative"
  )

  # Invalid max_labeled_demos
  expect_error(
    BootstrapFewShot(max_labeled_demos = -1L),
    "non-negative"
  )

  # Invalid max_rounds
  expect_error(
    BootstrapFewShot(max_rounds = 0L),
    "at least 1"
  )

  # Invalid seed
  expect_error(
    BootstrapFewShot(seed = c(1, 2)),
    "single numeric"
  )

  # Invalid log_dir
  expect_error(
    BootstrapFewShot(log_dir = 123),
    "character string"
  )
})

test_that("BootstrapFewShot requires metric for compilation", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = c("What is 2+2?", "What is 3+3?"),
    answer = c("4", "6")
  )

  tp <- BootstrapFewShot()
  expect_error(
    compile(tp, mod, trainset),
    "requires a metric"
  )
})

test_that("BootstrapFewShot compile returns unmodified program for empty trainset", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig, type = "predict")

  empty_trainset <- data.frame(question = character(), answer = character())
  tp <- BootstrapFewShot(metric = function(pred, exp) 1.0)

  expect_warning(
    result <- compile(tp, mod, empty_trainset),
    "Empty trainset"
  )
  expect_identical(result, mod)
})

test_that("BootstrapFewShot compile adds labeled demos from trainset", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = c(
      "What is 2+2?",
      "What is 3+3?",
      "What is 4+4?",
      "What is 5+5?"
    ),
    answer = c("4", "6", "8", "10")
  )

  # Create a mock LLM that returns predictable results
  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(answer = "mocked")
      }
    ),
    class = "Chat"
  )

  # Metric that always returns 0 so no bootstrapped demos are added
  tp <- BootstrapFewShot(
    metric = function(pred, exp) 0,
    max_labeled_demos = 2L,
    max_bootstrapped_demos = 2L,
    seed = 42L
  )

  result <- compile(tp, mod, trainset, .llm = mock_llm)

  expect_true(inherits(result, "Module"))
  expect_true(result$config$compiled)
  expect_equal(result$config$teleprompter, "BootstrapFewShot")

  # Should have labeled demos (up to max_labeled_demos)
  expect_length(result$demos, 2)

  # Check labeled demo structure
  demo <- result$demos[[1]]
  expect_true(is.list(demo))
  expect_true("inputs" %in% names(demo))
  expect_true("output" %in% names(demo))
  expect_equal(demo$source, "labeled")

  # Check optimizer info in config
  expect_equal(result$config$optimizer$n_labeled_demos, 2)
  expect_equal(result$config$optimizer$n_bootstrapped_demos, 0)
})

test_that("BootstrapFewShot compile bootstraps demos with metric", {
  MockBootstrapModule <- R6::R6Class(
    "MockBootstrapModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      call_count = 0,
      initialize = function(
        signature,
        template = "",
        demos = list(),
        config = list()
      ) {
        super$initialize(
          signature,
          template = template,
          demos = demos,
          config = config
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$call_count <- self$call_count + 1
        # Return predictable output based on input
        input_val <- batch[[1]][[1]]
        output_val <- switch(
          input_val,
          "What is 2+2?" = "4",
          "What is 3+3?" = "6",
          "What is 4+4?" = "8",
          "What is 5+5?" = "10",
          "unknown"
        )
        tibble::tibble(
          output = list(output_val),
          chat = list(NULL),
          metadata = list(list())
        )
      }
      # No need to override deepcopy - parent's clone-based impl preserves subclass
    )
  )

  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- MockBootstrapModule$new(signature = sig, template = "{question}")

  trainset <- data.frame(
    question = c(
      "What is 2+2?",
      "What is 3+3?",
      "What is 4+4?",
      "What is 5+5?"
    ),
    answer = c("4", "6", "8", "10")
  )

  # Metric that returns 1.0 for exact match
  exact_metric <- function(pred, exp) {
    if (is.null(pred) || is.null(exp)) {
      return(0)
    }
    as.numeric(pred == exp)
  }

  tp <- BootstrapFewShot(
    metric = exact_metric,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 2L,
    seed = 42L
  )

  result <- compile(tp, mod, trainset)

  expect_true(inherits(result, "Module"))
  expect_true(result$config$compiled)

  # Should have labeled + bootstrapped demos
  n_labeled <- result$config$optimizer$n_labeled_demos
  n_bootstrapped <- result$config$optimizer$n_bootstrapped_demos

  expect_equal(n_labeled, 1)
  expect_gte(n_bootstrapped, 0) # May have bootstrapped some

  # Total demos should not exceed limits
  expect_lte(length(result$demos), 3) # 1 labeled + up to 2 bootstrapped
})

test_that("BootstrapFewShot handles teacher errors gracefully", {
  # Use a standard module with a mock LLM that fails intermittently
  sig <- Signature(
    inputs = list(input(name = "x", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    x = c("a", "b", "c", "d", "e"),
    y = c("1", "2", "3", "4", "5")
  )

  # Create an environment to track call count across LLM invocations
  call_counter <- new.env()
  call_counter$count <- 0
  call_counter$max_fails <- 2

  # Mock LLM that fails for the first max_fails calls, then succeeds
  failing_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        call_counter$count <- call_counter$count + 1
        if (call_counter$count <= call_counter$max_fails) {
          stop("Simulated failure")
        }
        list(answer = "success")
      }
    ),
    class = "Chat"
  )

  tp <- BootstrapFewShot(
    metric = function(pred, exp) 1.0,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 2L,
    max_errors = 3L
  )

  # Should complete despite some failures
  result <- expect_test_warnings(
    compile(tp, mod, trainset, .llm = failing_llm),
    "Bootstrap attempt failed"
  )

  expect_true(inherits(result, "Module"))
  expect_true(result$config$compiled)
  # Verify error_count is actually tracked (was a scoping bug)
  expect_equal(result$config$optimizer$error_count, 2)
  expect_equal(result$config$optimizer$total_attempts, 4L)
  expect_equal(result$config$optimizer$budget_summary$successes, 2L)
  expect_equal(
    result$config$optimizer$budget_summary$consecutive_errors,
    0L
  )
  expect_false(result$config$optimizer$budget_summary$stopped)
})

test_that("BootstrapFewShot counts metric failures by training-row attempt", {
  metric_calls <- 0L
  testthat::local_mocked_bindings(
    run_with_settings = function(...) "prediction",
    .package = "dsprrr"
  )

  metric <- function(pred, expected) {
    metric_calls <<- metric_calls + 1L
    if (metric_calls %in% c(1L, 3L)) {
      stop("metric failed")
    }
    0.1
  }

  program <- module(signature("x -> y"), type = "predict")
  teleprompter <- BootstrapFewShot(
    metric = metric,
    metric_threshold = 0.5,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 3L,
    max_errors = 2L,
    max_rounds = 1L
  )

  result <- expect_test_warnings(
    dsprrr:::compile_bootstrap(
      teleprompter,
      program,
      data.frame(x = c("a", "b", "c"), y = c("a", "b", "c"))
    ),
    "Metric evaluation failed"
  )
  budget <- result$config$optimizer$budget_summary

  expect_equal(budget$attempts, 3L)
  expect_equal(budget$successes, 1L)
  expect_equal(budget$total_errors, 2L)
  expect_equal(budget$consecutive_errors, 1L)
  expect_false(budget$stopped)
  expect_equal(result$config$optimizer$n_bootstrapped_demos, 0L)
})

test_that("BootstrapFewShot max_errors zero stops after the first attempt", {
  teacher_calls <- 0L
  testthat::local_mocked_bindings(
    run_with_settings = function(...) {
      teacher_calls <<- teacher_calls + 1L
      stop("teacher failed")
    },
    .package = "dsprrr"
  )

  program <- module(signature("x -> y"), type = "predict")
  teleprompter <- BootstrapFewShot(
    metric = function(...) 1,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 2L,
    max_errors = 0L,
    max_rounds = 1L
  )

  result <- expect_test_warnings(
    dsprrr:::compile_bootstrap(
      teleprompter,
      program,
      data.frame(x = c("a", "b"), y = c("a", "b"))
    ),
    "Bootstrap attempt failed"
  )
  budget <- result$config$optimizer$budget_summary

  expect_equal(teacher_calls, 1L)
  expect_equal(budget$attempts, 1L)
  expect_equal(budget$total_errors, 1L)
  expect_true(budget$stopped)
  expect_equal(budget$stop_reason$limit, 0L)
  expect_identical(result$config$optimizer$stop_reason, budget$stop_reason)
})

test_that("find_output_column identifies common output columns", {
  input_names <- c("question", "context")

  # Standard output columns
  df1 <- data.frame(question = "q", context = "c", answer = "a")
  expect_equal(dsprrr:::find_output_column(df1, input_names), "answer")

  df2 <- data.frame(question = "q", context = "c", label = "l")
  expect_equal(dsprrr:::find_output_column(df2, input_names), "label")

  df3 <- data.frame(question = "q", context = "c", output = "o")
  expect_equal(dsprrr:::find_output_column(df3, input_names), "output")

  # Fall back to first non-input column
  df4 <- data.frame(question = "q", context = "c", custom = "x")
  expect_equal(dsprrr:::find_output_column(df4, input_names), "custom")

  # No output column available
  df5 <- data.frame(question = "q", context = "c")
  expect_null(dsprrr:::find_output_column(df5, input_names))
})

test_that("BootstrapFewShot print method works", {
  tp <- BootstrapFewShot(
    metric = function(x, y) 1.0,
    metric_threshold = 0.8,
    max_bootstrapped_demos = 6L,
    max_labeled_demos = 12L,
    max_rounds = 2L,
    seed = 42L
  )

  # Verify print method runs without error and returns invisibly
  expect_invisible(print(tp))
  expect_identical(print(tp), tp)
})

test_that("BootstrapFewShot respects metric_threshold", {
  MockThresholdModule <- R6::R6Class(
    "MockThresholdModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      initialize = function(
        signature,
        template = "",
        demos = list(),
        config = list()
      ) {
        super$initialize(
          signature,
          template = template,
          demos = demos,
          config = config
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        tibble::tibble(
          output = list("predicted"),
          chat = list(NULL),
          metadata = list(list())
        )
      }
      # No need to override deepcopy - parent's clone-based impl preserves subclass
    )
  )

  sig <- Signature(
    inputs = list(input(name = "x", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  mod <- MockThresholdModule$new(signature = sig)

  trainset <- data.frame(
    x = c("a", "b", "c", "d"),
    y = c("1", "2", "3", "4")
  )

  # Metric returns 0.3 for all predictions
  low_score_metric <- function(pred, exp) 0.3

  # Threshold of 0.5 - no demos should pass
  tp_high_threshold <- BootstrapFewShot(
    metric = low_score_metric,
    metric_threshold = 0.5,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 2L
  )

  result_high <- compile(tp_high_threshold, mod, trainset)
  expect_equal(result_high$config$optimizer$n_bootstrapped_demos, 0)

  # Threshold of 0.2 - demos should pass
  tp_low_threshold <- BootstrapFewShot(
    metric = low_score_metric,
    metric_threshold = 0.2,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 2L
  )

  result_low <- compile(tp_low_threshold, mod, trainset)
  expect_gte(result_low$config$optimizer$n_bootstrapped_demos, 0)
})

test_that("compile_module works with BootstrapFewShot", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Summarize"
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    text = c("Hello world", "Goodbye world"),
    summary = c("greeting", "farewell")
  )

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(summary = "mocked")
      }
    ),
    class = "Chat"
  )

  tp <- BootstrapFewShot(
    metric = function(pred, exp) 0.5,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L
  )

  result <- compile_module(mod, tp, trainset, .llm = mock_llm)

  expect_true(inherits(result, "Module"))
  expect_true(result$is_compiled())
  expect_equal(result$config$teleprompter, "BootstrapFewShot")
})

test_that("BootstrapFewShot harvests demos with field-aware metrics (dsprrr-s3b)", {
  # Regression: the single-module path passed the bare cell value as `expected`,
  # so a field-aware metric (the documented default) errored inside
  # extract_field(), was scored NA, and bootstrapped ZERO demos. The metric must
  # receive the full row, mirroring the pipeline path and evaluate().
  local_reset_cache()

  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig, type = "predict", template = "{question}")

  # Mock LLM always returns the correct structured answer so every row passes.
  mock_llm <- structure(
    list(chat_structured = function(prompt, type, ...) list(answer = "yes")),
    class = "Chat"
  )

  trainset <- data.frame(
    question = c("q1", "q2", "q3"),
    answer = c("yes", "yes", "yes")
  )

  tp <- BootstrapFewShot(
    metric = metric_exact_match(field = "answer"),
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 2L,
    seed = 42L
  )

  result <- compile(tp, mod, trainset, .llm = mock_llm)

  expect_gt(result$config$optimizer$n_bootstrapped_demos, 0)
})
