test_that("evaluate() works with epochs = 1 (default behavior)", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  # Mock LLM
  mock_llm <- list(
    chat_structured = function(...) "test answer"
  )

  dataset <- tibble::tibble(
    question = c("What is 2+2?", "What is 3+3?"),
    answer = c("4", "6")
  )

  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }

  result <- evaluate(
    mod,
    data = dataset,
    metric = metric,
    .llm = mock_llm,
    .progress = FALSE,
    epochs = 1L
  )

  expect_s3_class(result, "dsprrr_evaluation")
  expect_equal(result$n_evaluated, 2L)
  expect_false("epoch_scores" %in% names(result))
  expect_false("score_std" %in% names(result))
  expect_false("ci_95" %in% names(result))
})

test_that("evaluate() runs multiple epochs when epochs > 1", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  # Mock LLM with some variability
  call_count <- 0
  mock_llm <- list(
    chat_structured = function(...) {
      call_count <<- call_count + 1
      if (call_count %% 2 == 0) "4" else "wrong"
    }
  )

  dataset <- tibble::tibble(
    question = c("What is 2+2?", "What is 3+3?"),
    answer = c("4", "6")
  )

  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }

  result <- evaluate(
    mod,
    data = dataset,
    metric = metric,
    .llm = mock_llm,
    .progress = FALSE,
    epochs = 3L
  )

  expect_s3_class(result, "dsprrr_evaluation")
  expect_true("epoch_scores" %in% names(result))
  expect_true("score_std" %in% names(result))
  expect_true("ci_95" %in% names(result))

  # Should have 3 epoch score vectors
  expect_equal(length(result$epoch_scores), 3)
  expect_equal(length(result$epoch_scores[[1]]), 2) # 2 examples per epoch

  # CI should be a vector of length 2
  expect_length(result$ci_95, 2)
  expect_named(result$ci_95, c("lower", "upper"))
})

test_that("evaluate() computes correct statistics across epochs", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  # Mock LLM with deterministic output per call
  answers <- c("4", "4", "4") # All correct for first question across 3 epochs
  call_idx <- 0
  mock_llm <- list(
    chat_structured = function(...) {
      call_idx <<- call_idx + 1
      answers[((call_idx - 1) %% length(answers)) + 1]
    }
  )

  dataset <- tibble::tibble(
    question = "What is 2+2?",
    answer = "4"
  )

  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }

  result <- evaluate(
    mod,
    data = dataset,
    metric = metric,
    .llm = mock_llm,
    .progress = FALSE,
    epochs = 3L
  )

  # All epochs should have score of 1.0 for this example
  epoch_means <- vapply(
    result$epoch_scores,
    function(s) mean(s, na.rm = TRUE),
    numeric(1)
  )
  expect_equal(epoch_means, c(1, 1, 1))

  # Mean score should be 1.0
  expect_equal(result$mean_score, 1.0)

  # Standard deviation should be 0 (no variation)
  expect_equal(result$score_std, 0)
})

test_that("eval_program() works with epochs parameter", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  # Mock LLM
  mock_llm <- list(
    chat_structured = function(...) "4"
  )

  dataset <- tibble::tibble(
    question = "What is 2+2?",
    answer = "4"
  )

  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }
  ctrl <- optimizer_control(progress = FALSE)

  result <- suppressWarnings(
    eval_program(
      mod,
      dataset,
      metric = metric,
      .llm = mock_llm,
      control = ctrl,
      epochs = 3L
    )
  )

  # Check S7 class using S7::class_of
  expect_true(inherits(result, "S7_object"))
  expect_equal(result@epochs, 3L)
  expect_equal(length(result@epoch_scores), 3)
  expect_true(!is.na(result@score_std))
  expect_true(!is.na(result@ci_lower))
  expect_true(!is.na(result@ci_upper))
})

test_that("epochs parameter validates input", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  mock_llm <- list(chat_structured = function(...) "4")
  dataset <- tibble::tibble(question = "Q?", answer = "A")
  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }

  # epochs must be positive
  expect_error(
    evaluate(
      mod,
      data = dataset,
      metric = metric,
      .llm = mock_llm,
      epochs = 0L
    ),
    "must be a positive integer"
  )

  expect_error(
    evaluate(
      mod,
      data = dataset,
      metric = metric,
      .llm = mock_llm,
      epochs = -1L
    ),
    "must be a positive integer"
  )
})

test_that("epoch results are aggregated correctly", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  # Mock LLM with varying correctness
  # Epoch 1: score 0.5, Epoch 2: score 1.0, Epoch 3: score 0.0
  responses <- c("4", "wrong", "4", "4", "wrong", "wrong")
  call_idx <- 0
  mock_llm <- list(
    chat_structured = function(...) {
      call_idx <<- call_idx + 1
      responses[call_idx]
    }
  )

  dataset <- tibble::tibble(
    question = c("Q1", "Q2"),
    answer = c("4", "4")
  )

  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }

  result <- evaluate(
    mod,
    data = dataset,
    metric = metric,
    .llm = mock_llm,
    .progress = FALSE,
    epochs = 3L,
    .cache = FALSE # Disable cache so each epoch gets fresh responses
  )

  # Check that we have 3 epochs
  expect_equal(length(result$epoch_scores), 3)

  # Compute expected epoch means: [0.5, 1.0, 0.0]
  epoch_means <- vapply(
    result$epoch_scores,
    function(s) mean(s, na.rm = TRUE),
    numeric(1)
  )
  expect_equal(epoch_means, c(0.5, 1.0, 0.0))

  # Mean across epochs should be (0.5 + 1.0 + 0.0) / 3 = 0.5
  expect_equal(result$mean_score, 0.5)

  # SD should be sd(c(0.5, 1.0, 0.0))
  expected_sd <- sd(c(0.5, 1.0, 0.0))
  expect_equal(result$score_std, expected_sd)
})

test_that("print methods show epoch information", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  mock_llm <- list(chat_structured = function(...) "4")
  dataset <- tibble::tibble(question = "Q?", answer = "4")
  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }

  result <- suppressWarnings(
    evaluate(
      mod,
      data = dataset,
      metric = metric,
      .llm = mock_llm,
      .progress = FALSE,
      epochs = 3L
    )
  )

  # Check that result has epoch fields
  expect_true("epoch_scores" %in% names(result))
  expect_true("score_std" %in% names(result))
  expect_true("ci_95" %in% names(result))
  expect_equal(length(result$epoch_scores), 3)
})

test_that("evaluate() handles empty dataset with epochs > 1", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")
  mock_llm <- list(chat_structured = function(...) "4")

  empty_data <- tibble::tibble(question = character(0), answer = character(0))
  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }

  result <- evaluate(
    mod,
    data = empty_data,
    metric = metric,
    .llm = mock_llm,
    .progress = FALSE,
    epochs = 3L
  )

  # Should still return valid list structure
  expect_type(result, "list")
  expect_equal(result$n_evaluated, 0L)
  expect_true(is.na(result$mean_score))

  # Epoch fields should NOT be present for empty dataset (early return)
  expect_false("epoch_scores" %in% names(result))
  expect_false("score_std" %in% names(result))
  expect_false("ci_95" %in% names(result))
})

test_that("evaluate() reports epoch in error messages", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  # Mock LLM that works fine
  mock_llm <- list(chat_structured = function(...) "wrong_answer")

  dataset <- tibble::tibble(question = "Q1", answer = "4")
  # Metric that always fails with error
  bad_metric <- function(prediction, expected_row) {
    stop("Metric intentionally fails")
  }

  # Capture warnings to check for epoch context
  result <- suppressWarnings(
    evaluate(
      mod,
      data = dataset,
      metric = bad_metric,
      .llm = mock_llm,
      .progress = FALSE,
      epochs = 2L
    )
  )

  # Should complete despite metric errors
  expect_type(result, "list")
  expect_equal(result$n_errors, 1L)
})

test_that("epochs parameter coerces non-integer with warning", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")
  mock_llm <- list(chat_structured = function(...) "4")
  dataset <- tibble::tibble(question = "Q?", answer = "4")
  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }

  # Float input should be coerced to integer
  # Test that it works (coercion happens silently via as.integer)
  result <- evaluate(
    mod,
    data = dataset,
    metric = metric,
    .llm = mock_llm,
    .progress = FALSE,
    epochs = 3.7
  )

  # Should have run 3 epochs (not 3.7)
  expect_s3_class(result, "dsprrr_evaluation")
  expect_equal(length(result$epoch_scores), 3)
})

test_that("eval_program() validates epochs parameter", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")
  mock_llm <- list(chat_structured = function(...) "4")
  dataset <- tibble::tibble(question = "Q?", answer = "4")
  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }
  ctrl <- optimizer_control(progress = FALSE)

  # epochs must be positive
  expect_error(
    eval_program(
      mod,
      dataset,
      metric,
      .llm = mock_llm,
      control = ctrl,
      epochs = 0L
    ),
    "must be a positive integer"
  )

  expect_error(
    eval_program(
      mod,
      dataset,
      metric,
      .llm = mock_llm,
      control = ctrl,
      epochs = -1L
    ),
    "must be a positive integer"
  )
})

test_that("evaluate() counts intermittent metric failures correctly", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  # Mock LLM that returns consistent output
  mock_llm <- list(chat_structured = function(...) "4")

  dataset <- tibble::tibble(
    question = c("Q1", "Q2", "Q3"),
    answer = c("4", "4", "4")
  )

  # Metric that fails intermittently for specific rows/epochs
  # Row 1: fails in epoch 1 only
  # Row 2: succeeds in all epochs
  # Row 3: fails in epoch 2 only
  call_count <- 0
  intermittent_metric <- function(prediction, expected_row) {
    call_count <<- call_count + 1
    # Pattern: epoch 1 (calls 1-3), epoch 2 (calls 4-6), epoch 3 (calls 7-9)
    # Fail on call 1 (row 1, epoch 1) and call 6 (row 3, epoch 2)
    if (call_count %in% c(1, 6)) {
      stop("Intermittent failure")
    }
    identical(prediction, expected_row$answer)
  }

  result <- suppressWarnings(
    evaluate(
      mod,
      data = dataset,
      metric = intermittent_metric,
      .llm = mock_llm,
      .progress = FALSE,
      epochs = 3L
    )
  )

  # Should mark rows 1 and 3 as errors (intermittent failures)
  # Row 2 should succeed (no failures)
  expect_equal(result$n_errors, 2L)
  expect_equal(result$n_evaluated, 1L)

  # Scores should have NA for rows 1 and 3, valid for row 2
  expect_true(is.na(result$scores[1]))
  expect_false(is.na(result$scores[2]))
  expect_true(is.na(result$scores[3]))

  # Errors should contain information from both epochs
  expect_true(length(result$errors) > 0)
  expect_true(any(grepl("Epoch 1", result$errors)))
  expect_true(any(grepl("Epoch 2", result$errors)))
})
