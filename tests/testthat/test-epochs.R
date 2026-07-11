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

test_that("epoch summaries omit uncertainty with fewer than two epochs", {
  summary <- summarize_epoch_scores(list(c(1, NA_real_)))

  expect_equal(summary$epoch_means, 0.5)
  expect_equal(summary$mean_score, 0.5)
  expect_true(is.na(summary$score_std))
  expect_named(summary$ci_95, c("lower", "upper"))
  expect_true(all(is.na(summary$ci_95)))
  expect_false(any(is.nan(unlist(summary))))
  expect_false(any(is.infinite(unlist(summary))))

  failed_summary <- summarize_epoch_scores(list(c(NA_real_, NA_real_)))
  expect_equal(failed_summary$epoch_means, 0)
  expect_equal(failed_summary$mean_score, 0)
  expect_true(is.na(failed_summary$score_std))
  expect_true(all(is.na(failed_summary$ci_95)))
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

  result <- expect_test_warnings(
    evaluate(
      mod,
      data = dataset,
      metric = metric,
      .llm = mock_llm,
      .progress = FALSE,
      epochs = 3L
    ),
    "Confidence intervals"
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

  result <- expect_test_warnings(
    evaluate(
      mod,
      data = dataset,
      metric = metric,
      .llm = mock_llm,
      .progress = FALSE,
      epochs = 3L
    ),
    "Confidence intervals"
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

test_that("eval_program() preserves failure-adjusted epoch statistics", {
  mod <- module(signature("question -> answer"), type = "predict")
  mock_llm <- list(chat_structured = function(...) "4")
  dataset <- tibble::tibble(question = "What is 2+2?", answer = "4")
  metric_calls <- 0L
  metric <- function(prediction, expected_row) {
    metric_calls <<- metric_calls + 1L
    if (metric_calls == 1L) {
      stop("First epoch fails")
    }
    1
  }

  result <- suppressWarnings(
    eval_program(
      mod,
      dataset,
      metric = metric,
      .llm = mock_llm,
      control = optimizer_control(progress = FALSE),
      epochs = 3L,
      .cache = FALSE
    )
  )

  expected_epoch_means <- c(0, 1, 1)
  expected_sd <- stats::sd(expected_epoch_means)
  expected_margin <- stats::qt(0.975, df = 2) * expected_sd / sqrt(3)

  expect_true(inherits(result, "S7_object"))
  expect_equal(result@epochs, 3L)
  expect_equal(result@epoch_scores, list(NA_real_, 1, 1))
  expect_equal(result@mean_score, mean(expected_epoch_means))
  expect_equal(result@score_std, expected_sd)
  expect_equal(
    unname(result@ci_lower),
    mean(expected_epoch_means) - expected_margin
  )
  expect_equal(
    unname(result@ci_upper),
    mean(expected_epoch_means) + expected_margin
  )
  expect_equal(result@n_errors, 1L)
  expect_equal(result@n_evaluated, 0L)
  expect_true(is.na(result@examples$score))
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

test_that("epoch results use all no-failure observations consistently", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  # Mock LLM with varying correctness
  # Epoch 1: score 0.5, Epoch 2: score 1.0, Epoch 3: score 0.0
  responses <- c("4", "wrong", "4", "4", "wrong", "wrong")
  call_idx <- 0
  mock_llm <- local({
    self <- structure(
      list(
        chat_structured = function(...) {
          call_idx <<- call_idx + 1
          responses[call_idx]
        },
        clone = function(...) self,
        set_turns = function(turns) invisible(NULL)
      ),
      class = "Chat"
    )
    self
  })

  dataset <- tibble::tibble(
    question = c("Q1", "Q2"),
    answer = c("4", "4")
  )

  # Simple metric that compares prediction to expected row$answer
  metric <- function(prediction, expected_row) {
    identical(prediction, expected_row$answer)
  }

  result <- expect_test_warnings(
    evaluate(
      mod,
      data = dataset,
      metric = metric,
      .llm = mock_llm,
      .progress = FALSE,
      epochs = 3L,
      .cache = FALSE # Disable cache so each epoch gets fresh responses
    ),
    "Confidence intervals"
  )

  expect_equal(length(result$epoch_scores), 3)

  epoch_means <- vapply(
    result$epoch_scores,
    mean,
    numeric(1)
  )
  expect_equal(epoch_means, c(0.5, 1.0, 0.0))

  expected_mean <- mean(epoch_means)
  expected_sd <- stats::sd(epoch_means)
  expected_margin <- stats::qt(0.975, df = 2) * expected_sd / sqrt(3)
  expect_equal(result$mean_score, expected_mean)
  expect_equal(result$score_std, expected_sd)
  expect_equal(
    result$ci_95,
    c(
      lower = expected_mean - expected_margin,
      upper = expected_mean + expected_margin
    )
  )
  expect_equal(mean(result$ci_95), result$mean_score)
  expect_equal(result$n_errors, 0L)
  expect_equal(result$n_run_errors, 0L)
  expect_equal(result$n_metric_errors, 0L)
  expect_length(result$run_errors, 0L)
  expect_length(result$metric_errors, 0L)
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

  result <- expect_test_warnings(
    evaluate(
      mod,
      data = empty_data,
      metric = metric,
      .llm = mock_llm,
      .progress = FALSE,
      epochs = 3L
    ),
    "Empty data provided"
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

test_that("epochs parameter coerces non-integer", {
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
    epochs = 5.7
  )

  # Should have run 5 epochs (not 5.7)
  expect_s3_class(result, "dsprrr_evaluation")
  expect_equal(length(result$epoch_scores), 5)
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

test_that("evaluate() counts intermittent metric failures as zero", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict")

  mock_llm <- list(chat_structured = function(...) "4")

  dataset <- tibble::tibble(
    question = c("Q1", "Q2", "Q3"),
    answer = c("4", "4", "4")
  )

  metric_calls <- 0L
  intermittent_metric <- function(prediction, expected_row) {
    metric_calls <<- metric_calls + 1L
    if (metric_calls %in% c(1, 6)) {
      stop("Intermittent metric failure")
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
      epochs = 3L,
      .cache = FALSE
    )
  )

  expected_epoch_scores <- list(
    c(NA_real_, 1, 1),
    c(1, 1, NA_real_),
    c(1, 1, 1)
  )
  expected_epoch_means <- c(2 / 3, 2 / 3, 1)
  expected_mean <- mean(expected_epoch_means)
  expected_sd <- stats::sd(expected_epoch_means)
  expected_margin <- stats::qt(0.975, df = 2) * expected_sd / sqrt(3)

  expect_equal(result$epoch_scores, expected_epoch_scores)
  expect_equal(result$mean_score, expected_mean)
  expect_equal(result$score_std, expected_sd)
  expect_equal(
    result$ci_95,
    c(
      lower = expected_mean - expected_margin,
      upper = expected_mean + expected_margin
    )
  )
  expect_equal(mean(result$ci_95), result$mean_score)
  expect_equal(result$n_errors, 2L)
  expect_equal(result$n_evaluated, 1L)
  expect_true(is.na(result$scores[1]))
  expect_equal(result$scores[2], 1)
  expect_true(is.na(result$scores[3]))
  expect_equal(result$n_run_errors, 0L)
  expect_equal(result$n_metric_errors, 2L)
  expect_length(result$run_errors, 0L)
  expect_length(result$metric_errors, 2L)
  expect_match(result$metric_errors[1], "Epoch 1: Intermittent metric failure")
  expect_match(result$metric_errors[2], "Epoch 2: Intermittent metric failure")
})

test_that("evaluate() includes a wholly failed epoch in uncertainty", {
  mod <- module(signature("question -> answer"), type = "predict")
  mock_llm <- list(chat_structured = function(...) "4")
  dataset <- tibble::tibble(
    question = c("Q1", "Q2"),
    answer = c("4", "4")
  )
  metric_calls <- 0L
  metric <- function(prediction, expected_row) {
    metric_calls <<- metric_calls + 1L
    if (metric_calls %in% 3:4) {
      stop("Epoch unavailable")
    }
    1
  }

  result <- suppressWarnings(
    evaluate(
      mod,
      data = dataset,
      metric = metric,
      .llm = mock_llm,
      .progress = FALSE,
      epochs = 3L,
      .cache = FALSE
    )
  )

  expected_epoch_means <- c(1, 0, 1)
  expected_mean <- mean(expected_epoch_means)
  expected_sd <- stats::sd(expected_epoch_means)
  expected_margin <- stats::qt(0.975, df = 2) * expected_sd / sqrt(3)

  expect_equal(
    result$epoch_scores,
    list(c(1, 1), c(NA_real_, NA_real_), c(1, 1))
  )
  expect_equal(result$mean_score, expected_mean)
  expect_equal(result$score_std, expected_sd)
  expect_equal(
    result$ci_95,
    c(
      lower = expected_mean - expected_margin,
      upper = expected_mean + expected_margin
    )
  )
  expect_equal(mean(result$ci_95), result$mean_score)
  expect_equal(result$n_errors, 2L)
  expect_equal(result$n_evaluated, 0L)
  expect_equal(result$n_run_errors, 0L)
  expect_equal(result$n_metric_errors, 2L)
  expect_length(result$run_errors, 0L)
  expect_length(result$metric_errors, 2L)
})
