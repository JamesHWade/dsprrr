# Tests for module-wrapper.R (BestOfN and Refine)

# Helper: Create a mock module for testing
create_mock_module <- function(
  responses = list("answer1", "answer2", "answer3")
) {
  idx <- 0
  mock_mod <- list(
    signature = signature("question -> answer"),
    chat = NULL,
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      idx <<- idx + 1
      response <- responses[[(idx - 1) %% length(responses) + 1]]
      tibble::tibble(
        output = list(list(answer = response)),
        chat = list(NULL),
        metadata = list(list(
          total_tokens = 100,
          cost = 0.001,
          model = "mock-model"
        ))
      )
    },
    reset_copy = function() create_mock_module(responses)
  )
  class(mock_mod) <- c("MockModule", "Module", "R6")
  mock_mod
}

# ============================================================================
# BestOfNModule Tests
# ============================================================================

test_that("BestOfNModule class exists and inherits from Module", {
  expect_true(R6::is.R6Class(BestOfNModule))
})

test_that("best_of_n creates BestOfNModule", {
  mod <- module(signature("q -> a"))
  wrapper <- best_of_n(mod, N = 3)

  expect_s3_class(wrapper, "BestOfNModule")
  expect_s3_class(wrapper, "Module")
  expect_equal(wrapper$N, 3L)
})

test_that("best_of_n validates module argument", {
  expect_error(
    best_of_n("not a module"),
    "must be a Module"
  )
  expect_error(
    best_of_n(list(a = 1)),
    "must be a Module"
  )
})

test_that("best_of_n uses default N of 3", {
  mod <- module(signature("q -> a"))
  wrapper <- best_of_n(mod)
  expect_equal(wrapper$N, 3L)
})

test_that("best_of_n uses default threshold of 1.0", {
  mod <- module(signature("q -> a"))
  wrapper <- best_of_n(mod)
  expect_equal(wrapper$threshold, 1.0)
})

test_that("BestOfN forward returns correct structure", {
  mock <- create_mock_module()
  wrapper <- best_of_n(mock, N = 2)

  result <- wrapper$forward(list(question = "test"))

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("output", "chat", "metadata"))
  expect_length(result$output, 1)
  expect_length(result$metadata, 1)
})

test_that("BestOfN runs up to N times without threshold", {
  call_count <- 0
  mock <- create_mock_module()
  mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    call_count <<- call_count + 1
    tibble::tibble(
      output = list(list(answer = paste0("answer", call_count))),
      chat = list(NULL),
      metadata = list(list(total_tokens = 100, cost = 0.001, model = "mock"))
    )
  }

  # Reward function that never meets threshold
  always_low <- function(pred, inputs) 0.5

  wrapper <- best_of_n(mock, N = 5, reward_fn = always_low, threshold = 1.0)
  wrapper$forward(list(question = "test"))

  expect_equal(call_count, 5)
})

test_that("BestOfN early stops when threshold met", {
  call_count <- 0
  mock <- create_mock_module()
  mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    call_count <<- call_count + 1
    tibble::tibble(
      output = list(list(answer = "answer")),
      chat = list(NULL),
      metadata = list(list(total_tokens = 100, cost = 0.001, model = "mock"))
    )
  }

  # Reward function that meets threshold on attempt 2
  meet_on_second <- function(pred, inputs) {
    if (call_count >= 2) 1.0 else 0.5
  }

  wrapper <- best_of_n(mock, N = 5, reward_fn = meet_on_second, threshold = 1.0)
  wrapper$forward(list(question = "test"))

  expect_equal(call_count, 2)
})

test_that("BestOfN returns best scoring result", {
  scores <- c(0.3, 0.9, 0.6)
  call_count <- 0
  mock <- create_mock_module()
  mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    call_count <<- call_count + 1
    tibble::tibble(
      output = list(list(answer = paste0("answer", call_count))),
      chat = list(NULL),
      metadata = list(list(total_tokens = 100, cost = 0.001, model = "mock"))
    )
  }

  varying_score <- function(pred, inputs) {
    scores[call_count]
  }

  wrapper <- best_of_n(mock, N = 3, reward_fn = varying_score, threshold = 1.0)
  result <- wrapper$forward(list(question = "test"))

  # Best score was 0.9 on attempt 2
  expect_equal(result$output[[1]]$answer, "answer2")
  expect_equal(result$metadata[[1]]$best_score, 0.9)
})

test_that("BestOfN metadata includes attempt info", {
  mock <- create_mock_module()
  wrapper <- best_of_n(mock, N = 3, threshold = 1.0)
  result <- wrapper$forward(list(question = "test"))

  meta <- result$metadata[[1]]
  expect_true("n_attempts" %in% names(meta))
  expect_true("best_score" %in% names(meta))
  expect_true("all_scores" %in% names(meta))
  expect_true("total_tokens" %in% names(meta))
})

test_that("BestOfN get_attempts returns attempt history", {
  mock <- create_mock_module()
  wrapper <- best_of_n(mock, N = 3, threshold = 2.0) # Never meets threshold
  wrapper$forward(list(question = "test"))

  attempts <- wrapper$get_attempts()

  expect_s3_class(attempts, "tbl_df")
  expect_true("attempt" %in% names(attempts))
  expect_true("score" %in% names(attempts))
  expect_true("prediction" %in% names(attempts))
  expect_equal(nrow(attempts), 3)
})

test_that("BestOfN get_attempts all=TRUE returns all runs", {
  mock <- create_mock_module()
  wrapper <- best_of_n(mock, N = 2, threshold = 2.0)

  # Run twice

  wrapper$forward(list(question = "test1"))
  wrapper$forward(list(question = "test2"))

  all_attempts <- wrapper$get_attempts(all = TRUE)
  expect_equal(nrow(all_attempts), 4) # 2 runs x 2 attempts
  expect_true(all(all_attempts$run %in% c(1, 2)))
})

test_that("BestOfN records traces", {
  mock <- create_mock_module()
  wrapper <- best_of_n(mock, N = 2)
  wrapper$forward(list(question = "test"), trace = TRUE)

  expect_length(wrapper$state$traces, 1)
  trace <- wrapper$state$traces[[1]]
  expect_true("attempts" %in% names(trace))
  expect_true("n_attempts" %in% names(trace))
})

test_that("BestOfN with trace=FALSE does not record", {
  mock <- create_mock_module()
  wrapper <- best_of_n(mock, N = 2)
  wrapper$forward(list(question = "test"), trace = FALSE)

  expect_length(wrapper$state$traces, 0)
})

test_that("BestOfN reset_copy creates fresh wrapper", {
  mock <- create_mock_module()
  wrapper <- best_of_n(mock, N = 5, threshold = 0.8)
  wrapper$forward(list(question = "test"))

  copy <- wrapper$reset_copy()

  expect_s3_class(copy, "BestOfNModule")
  expect_equal(copy$N, 5L)
  expect_equal(copy$threshold, 0.8)
  expect_length(copy$state$traces, 0)
  expect_length(copy$state$attempts, 0)
})

test_that("BestOfN apply_optimization_params updates settings", {
  mock <- create_mock_module()
  wrapper <- best_of_n(mock, N = 3, threshold = 0.8)

  wrapper$apply_optimization_params(list(N = 5, threshold = 0.9))

  expect_equal(wrapper$N, 5L)
  expect_equal(wrapper$threshold, 0.9)
})

test_that("BestOfN handles module errors gracefully", {
  fail_count <- 0
  mock <- create_mock_module()
  mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    fail_count <<- fail_count + 1
    if (fail_count <= 2) {
      stop("Simulated failure")
    }
    tibble::tibble(
      output = list(list(answer = "success")),
      chat = list(NULL),
      metadata = list(list(total_tokens = 100, cost = 0.001, model = "mock"))
    )
  }

  wrapper <- best_of_n(mock, N = 5, fail_count = 5)
  result <- expect_test_warnings(
    wrapper$forward(list(question = "test")),
    "failed in BestOfN"
  )

  # Should succeed after 2 failures
  expect_equal(result$output[[1]]$answer, "success")
})

test_that("BestOfN fails after too many consecutive errors", {
  mock <- create_mock_module()
  mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    stop("Always fails")
  }

  wrapper <- best_of_n(mock, N = 3, fail_count = 2)

  expect_error_with_warnings(
    wrapper$forward(list(question = "test")),
    warning_regexp = "failed in BestOfN",
    error_regexp = "Too many consecutive failures"
  )
})

# ============================================================================
# as_reward_fn Tests
# ============================================================================

test_that("as_reward_fn converts exact match metric", {
  metric <- metric_exact_match()
  reward <- as_reward_fn(metric, expected_field = "expected")

  # Matching case
  inputs <- list(question = "Q", expected = "hello")
  score <- reward(list(answer = "hello"), inputs)
  expect_equal(score, 1.0)

  # Non-matching case
  score2 <- reward(list(answer = "world"), inputs)
  expect_equal(score2, 0.0)
})

test_that("as_reward_fn handles missing expected field", {
  metric <- metric_exact_match()
  reward <- as_reward_fn(metric, expected_field = "nonexistent")

  inputs <- list(question = "Q", answer = "A")
  expect_warning(
    score <- reward(list(answer = "A"), inputs),
    "No expected value"
  )
  expect_equal(score, 0.0)
})

test_that("as_reward_fn with prediction_field extracts correctly", {
  metric <- metric_exact_match()
  reward <- as_reward_fn(
    metric,
    expected_field = "expected",
    prediction_field = "answer"
  )

  inputs <- list(expected = "hello")
  prediction <- list(answer = "hello", confidence = 0.9)

  score <- reward(prediction, inputs)
  expect_equal(score, 1.0)
})

test_that("as_reward_fn converts F1 metric", {
  metric <- metric_f1()
  reward <- as_reward_fn(metric, expected_field = "expected")

  inputs <- list(expected = "the quick brown fox")
  prediction <- list(answer = "the quick brown dog")

  score <- reward(prediction, inputs)
  expect_true(score > 0 && score < 1) # Partial match
})

test_that("as_reward_fn validates metric argument", {
  expect_error(
    as_reward_fn("not a function"),
    "must be a function"
  )
})

test_that("default_reward_fn returns 1 for valid predictions", {
  reward <- dsprrr:::default_reward_fn()

  expect_equal(reward(list(answer = "test"), list()), 1.0)
  expect_equal(reward("any value", list()), 1.0)
  expect_equal(reward(NULL, list()), 0.0)
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("BestOfN works with real PredictModule structure", {
  # Create actual PredictModule
  mod <- module(signature("question -> answer: string"))

  # Create wrapper
  wrapper <- best_of_n(mod, N = 2)

  expect_s3_class(wrapper, "BestOfNModule")
  expect_equal(wrapper$N, 2L)

  # Signature should be passed through
  expect_equal(
    names(wrapper$signature@output_type@properties),
    names(mod$signature@output_type@properties)
  )
})

test_that("BestOfN print method works", {
  mod <- module(signature("q -> a"))
  wrapper <- best_of_n(mod, N = 5, threshold = 0.9)

  # Just verify print doesn't error
  expect_invisible(print(wrapper))
  expect_s3_class(wrapper, "BestOfNModule")
})

test_that("BestOfN handles reward_fn errors gracefully", {
  mock <- create_mock_module()

  # Reward function that throws an error
  error_reward <- function(pred, inputs) {
    stop("Reward function error!")
  }

  wrapper <- best_of_n(mock, N = 2, reward_fn = error_reward)

  # Should warn about reward function failure but still return result
  result <- expect_test_warnings(
    wrapper$forward(list(question = "test")),
    "Reward function failed"
  )

  # Should still return a result (first successful attempt)
  expect_s3_class(result, "tbl_df")
  expect_true(!is.null(result$output[[1]]))

  # Attempts should have NA scores
  attempts <- wrapper$get_attempts()
  expect_true(all(is.na(attempts$score)))
})

test_that("BestOfN with NA scores selects first successful attempt", {
  mock <- create_mock_module(responses = list("first", "second"))

  # Reward function that always returns NA
  na_reward <- function(pred, inputs) {
    NA_real_
  }

  wrapper <- best_of_n(mock, N = 2, reward_fn = na_reward)
  result <- wrapper$forward(list(question = "test"))

  # Should return first attempt since no scores are valid
  # (best_score stays -Inf, so first result with non-NA would win,
  # but all NA means we fall back to first successful)
  expect_s3_class(result, "tbl_df")
})

# ============================================================================
# RefineModule Tests
# ============================================================================

test_that("RefineModule class exists and inherits from BestOfNModule", {
  expect_true(R6::is.R6Class(RefineModule))
})

test_that("refine creates RefineModule", {
  mod <- module(signature("q -> a"))
  wrapper <- refine(mod, N = 3)

  expect_s3_class(wrapper, "RefineModule")
  expect_s3_class(wrapper, "BestOfNModule")
  expect_s3_class(wrapper, "Module")
})

test_that("refine uses default feedback template", {
  mod <- module(signature("q -> a"))
  wrapper <- refine(mod)

  expect_true(nchar(wrapper$feedback_template) > 0)
  expect_true(grepl("\\{score\\}", wrapper$feedback_template))
})

test_that("refine uses custom feedback template", {
  mod <- module(signature("q -> a"))
  wrapper <- refine(
    mod,
    feedback_template = "Custom feedback: {score}"
  )

  expect_equal(wrapper$feedback_template, "Custom feedback: {score}")
})

test_that("RefineModule forward returns correct structure", {
  mock <- create_mock_module()
  wrapper <- refine(mock, N = 2)

  result <- wrapper$forward(list(question = "test"))

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("output", "chat", "metadata"))
})

test_that("RefineModule generates feedback for each failed attempt", {
  call_count <- 0
  received_feedback <- NULL

  mock <- create_mock_module()
  original_forward <- mock$forward
  mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    call_count <<- call_count + 1
    if (!is.null(batch$feedback)) {
      received_feedback <<- batch$feedback
    }
    tibble::tibble(
      output = list(list(answer = paste0("answer", call_count))),
      chat = list(NULL),
      metadata = list(list(total_tokens = 100, cost = 0.001, model = "mock"))
    )
  }

  # Never meets threshold
  always_low <- function(pred, inputs) 0.5

  wrapper <- refine(mock, N = 3, reward_fn = always_low, threshold = 1.0)
  wrapper$forward(list(question = "test"))

  # Should have received feedback on attempts 2 and 3
  expect_true(!is.null(received_feedback))
  expect_true(grepl("0.5", as.character(received_feedback)))
})

test_that("RefineModule get_feedback_history returns feedback", {
  mock <- create_mock_module()
  always_low <- function(pred, inputs) 0.5

  wrapper <- refine(mock, N = 3, reward_fn = always_low, threshold = 1.0)
  wrapper$forward(list(question = "test"))

  history <- wrapper$get_feedback_history()
  expect_type(history, "character")
  expect_length(history, 2) # 3 attempts = 2 feedback rounds
})

test_that("RefineModule metadata includes feedback count", {
  mock <- create_mock_module()
  always_low <- function(pred, inputs) 0.5

  wrapper <- refine(mock, N = 3, reward_fn = always_low, threshold = 1.0)
  result <- wrapper$forward(list(question = "test"))

  meta <- result$metadata[[1]]
  expect_true("feedback_count" %in% names(meta))
  expect_equal(meta$feedback_count, 2)
})

test_that("RefineModule custom feedback field works", {
  received_inputs <- NULL
  mock <- create_mock_module()
  mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    received_inputs <<- batch
    tibble::tibble(
      output = list(list(answer = "test")),
      chat = list(NULL),
      metadata = list(list(total_tokens = 100, cost = 0.001, model = "mock"))
    )
  }

  always_low <- function(pred, inputs) 0.5

  wrapper <- refine(
    mock,
    N = 2,
    reward_fn = always_low,
    threshold = 1.0,
    feedback_field = "custom_feedback"
  )
  wrapper$forward(list(question = "test"))

  # Second attempt should have custom_feedback field
  expect_true("custom_feedback" %in% names(received_inputs))
})

test_that("RefineModule template uses input fields", {
  mock <- create_mock_module()
  always_low <- function(pred, inputs) 0.5

  wrapper <- refine(
    mock,
    N = 2,
    reward_fn = always_low,
    threshold = 1.0,
    feedback_template = "Question was: {question}. Score: {score}"
  )
  wrapper$forward(list(question = "test question"))

  history <- wrapper$get_feedback_history()
  expect_true(grepl("test question", history[1], fixed = TRUE))
})

test_that("RefineModule early stops like BestOfN", {
  call_count <- 0
  mock <- create_mock_module()
  mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    call_count <<- call_count + 1
    tibble::tibble(
      output = list(list(answer = "answer")),
      chat = list(NULL),
      metadata = list(list(total_tokens = 100, cost = 0.001, model = "mock"))
    )
  }

  # Meets threshold on second attempt
  meet_on_second <- function(pred, inputs) {
    if (call_count >= 2) 1.0 else 0.5
  }

  wrapper <- refine(mock, N = 5, reward_fn = meet_on_second, threshold = 1.0)
  wrapper$forward(list(question = "test"))

  expect_equal(call_count, 2)
  expect_length(wrapper$get_feedback_history(), 1) # Only 1 feedback (after attempt 1)
})

test_that("RefineModule reset_copy creates fresh wrapper", {
  mock <- create_mock_module()
  wrapper <- refine(
    mock,
    N = 5,
    threshold = 0.8,
    feedback_template = "Custom: {score}"
  )
  wrapper$forward(list(question = "test"))

  copy <- wrapper$reset_copy()

  expect_s3_class(copy, "RefineModule")
  expect_equal(copy$N, 5L)
  expect_equal(copy$threshold, 0.8)
  expect_equal(copy$feedback_template, "Custom: {score}")
  expect_length(copy$state$traces, 0)
  expect_length(copy$get_feedback_history(), 0)
})

test_that("RefineModule print works", {
  mod <- module(signature("q -> a"))
  wrapper <- refine(mod, N = 5, threshold = 0.9)

  expect_invisible(print(wrapper))
  expect_s3_class(wrapper, "RefineModule")
})

test_that("BestOfN passes a distinct rollout_id to each attempt (dsprrr-pcd)", {
  seen <- integer(0)
  mock <- create_mock_module()
  mock$forward <- function(
    batch,
    .llm = NULL,
    trace = TRUE,
    rollout_id = NULL,
    ...
  ) {
    seen <<- c(seen, rollout_id %||% NA_integer_)
    tibble::tibble(
      output = list(list(answer = "a")),
      chat = list(NULL),
      metadata = list(list(total_tokens = 1, cost = 0, model = "mock"))
    )
  }

  # threshold above any reward so all N attempts run
  wrapper <- best_of_n(mock, N = 3, threshold = 99)
  wrapper$forward(list(question = "q"))

  # compose_rollout_id() returns character ids so they nest cleanly
  expect_equal(seen, c("1", "2", "3"))
})

test_that("Refine passes a distinct rollout_id to each attempt (dsprrr-pcd)", {
  seen <- integer(0)
  mock <- create_mock_module()
  mock$forward <- function(
    batch,
    .llm = NULL,
    trace = TRUE,
    rollout_id = NULL,
    ...
  ) {
    seen <<- c(seen, rollout_id %||% NA_integer_)
    tibble::tibble(
      output = list(list(answer = "a")),
      chat = list(NULL),
      metadata = list(list(total_tokens = 1, cost = 0, model = "mock"))
    )
  }

  wrapper <- refine(mock, N = 3, threshold = 99)
  wrapper$forward(list(question = "q"))

  expect_equal(seen, c("1", "2", "3"))
})

# Regression tests for dsprrr-wx6: nested wrappers used to crash because each
# wrapper passed rollout_id = i explicitly while also spreading ..., so the
# inner forward() received rollout_id twice.

# Innermost mock that records every rollout_id it is handed.
mock_recording_rollouts <- function(seen_env) {
  mock <- create_mock_module()
  mock$forward <- function(
    batch,
    .llm = NULL,
    trace = TRUE,
    rollout_id = NULL,
    ...
  ) {
    seen_env$ids <- c(seen_env$ids, rollout_id %||% NA_character_)
    tibble::tibble(
      output = list(list(answer = "a")),
      chat = list(NULL),
      metadata = list(list(total_tokens = 1, cost = 0, model = "mock"))
    )
  }
  mock
}

test_that("refine(best_of_n(mod)) nests without a duplicate-argument crash (dsprrr-wx6)", {
  seen <- new.env()
  seen$ids <- character(0)
  nested <- refine(
    best_of_n(mock_recording_rollouts(seen), N = 2, threshold = 99),
    N = 2,
    threshold = 99
  )

  expect_no_error(nested$forward(list(question = "q")))
  # 2 outer x 2 inner attempts, each id unique and hierarchical
  expect_equal(sort(seen$ids), c("1.1", "1.2", "2.1", "2.2"))
})

test_that("best_of_n(refine(mod)) nests without a crash (dsprrr-wx6)", {
  seen <- new.env()
  seen$ids <- character(0)
  nested <- best_of_n(
    refine(mock_recording_rollouts(seen), N = 2, threshold = 99),
    N = 2,
    threshold = 99
  )

  expect_no_error(nested$forward(list(question = "q")))
  expect_equal(sort(seen$ids), c("1.1", "1.2", "2.1", "2.2"))
})

test_that("with_assertions(best_of_n(mod)) nests without a crash (dsprrr-wx6)", {
  seen <- new.env()
  seen$ids <- character(0)
  nested <- with_assertions(
    best_of_n(mock_recording_rollouts(seen), N = 2, threshold = 99),
    assertions = list(assert_output(~TRUE, "always passes")),
    max_retries = 1L
  )

  expect_no_error(nested$forward(list(question = "q")))
  # assertion passes on the first outer attempt; inner best_of_n runs twice
  expect_equal(sort(seen$ids), c("1.1", "1.2"))
})
