# Tests for feedback-aware metrics (metric_with_feedback) and the
# score+feedback protocol used by GEPA

test_that("metric_with_feedback creates a classed metric with field attr", {
  metric <- metric_with_feedback(
    function(prediction, expected) list(score = 1, feedback = "ok"),
    field = "answer"
  )

  expect_true(is.function(metric))
  expect_s3_class(metric, "dsprrr_feedback_metric")
  expect_equal(attr(metric, "field"), "answer")

  result <- metric("a", "a")
  expect_equal(result$score, 1)
  expect_equal(result$feedback, "ok")
})

test_that("metric_with_feedback validates inputs", {
  expect_error(metric_with_feedback("not a function"), "must be a function")
  expect_error(
    metric_with_feedback(function(p, e) 1, field = 123),
    "single character"
  )
})

test_that("normalize_metric_result handles all return shapes", {
  norm <- dsprrr:::normalize_metric_result

  # Plain numeric
  expect_equal(norm(0.5), list(score = 0.5, feedback = NA_character_))

  # Logical coerced to numeric
  expect_equal(norm(TRUE), list(score = 1, feedback = NA_character_))

  # List with score and feedback
  expect_equal(
    norm(list(score = 0, feedback = "wrong")),
    list(score = 0, feedback = "wrong")
  )

  # List with score only
  expect_equal(
    norm(list(score = 1)),
    list(score = 1, feedback = NA_character_)
  )

  # List without score errors
  expect_error(norm(list(feedback = "no score")), "score")

  # Non-numeric score errors
  expect_error(norm("high"), "logical or numeric")

  # Invalid feedback type errors
  expect_error(norm(list(score = 1, feedback = c("a", "b"))), "feedback")
  expect_error(norm(list(score = 1, feedback = character(0))), "feedback")
  expect_error(norm(list(score = 1, feedback = c(NA, "b"))), "feedback")
  expect_error(norm(list(score = 1, feedback = 42)), "feedback")

  # Scalar NA feedback is treated as absent
  expect_equal(
    norm(list(score = 1, feedback = NA)),
    list(score = 1, feedback = NA_character_)
  )
})

test_that("evaluate collects feedback from feedback metrics", {
  local_reset_cache()

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(a = "mocked")
      }
    ),
    class = "Chat"
  )

  sig <- signature("q -> a")
  mod <- module(sig, type = "predict")

  skip_if_not(
    tryCatch(
      {
        mod$forward(list(q = "test"), .llm = mock_llm)
        TRUE
      },
      error = function(e) FALSE
    ),
    "Mock LLM not compatible with module"
  )

  dataset <- tibble::tibble(
    q = c("Q1", "Q2"),
    a = c("mocked", "different")
  )

  metric <- metric_with_feedback(
    function(prediction, expected) {
      pred <- if (is.list(prediction)) prediction$a else prediction
      exp <- if (is.data.frame(expected)) expected$a else expected
      if (identical(as.character(pred), as.character(exp))) {
        list(score = 1, feedback = "Correct.")
      } else {
        list(score = 0, feedback = paste0("Expected '", exp, "'."))
      }
    },
    field = "a"
  )

  result <- evaluate(mod, dataset, metric, .llm = mock_llm, .progress = FALSE)

  expect_equal(result$scores, c(1, 0))
  expect_equal(result$feedbacks, c("Correct.", "Expected 'different'."))
})

test_that("plain metrics produce NA feedback in evaluate", {
  local_reset_cache()

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(a = "mocked")
      }
    ),
    class = "Chat"
  )

  sig <- signature("q -> a")
  mod <- module(sig, type = "predict")

  skip_if_not(
    tryCatch(
      {
        mod$forward(list(q = "test"), .llm = mock_llm)
        TRUE
      },
      error = function(e) FALSE
    ),
    "Mock LLM not compatible with module"
  )

  dataset <- tibble::tibble(q = "Q1", a = "mocked")

  result <- evaluate(
    mod,
    dataset,
    metric_exact_match(field = "a"),
    .llm = mock_llm,
    .progress = FALSE
  )

  expect_equal(result$feedbacks, NA_character_)
})

test_that("eval_program propagates feedback into examples tibble", {
  local_reset_cache()

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(a = "mocked")
      }
    ),
    class = "Chat"
  )

  sig <- signature("q -> a")
  mod <- module(sig, type = "predict")

  skip_if_not(
    tryCatch(
      {
        mod$forward(list(q = "test"), .llm = mock_llm)
        TRUE
      },
      error = function(e) FALSE
    ),
    "Mock LLM not compatible with module"
  )

  dataset <- tibble::tibble(
    q = c("Q1", "Q2"),
    a = c("mocked", "different")
  )

  metric <- metric_with_feedback(
    function(prediction, expected) {
      pred <- if (is.list(prediction)) prediction$a else prediction
      exp <- if (is.data.frame(expected)) expected$a else expected
      list(
        score = as.numeric(identical(as.character(pred), as.character(exp))),
        feedback = "explained"
      )
    },
    field = "a"
  )

  result <- eval_program(mod, dataset, metric, .llm = mock_llm)

  expect_true("feedback" %in% names(result@examples))
  expect_equal(result@examples$feedback, c("explained", "explained"))
})

test_that("gepa_failed_examples carries feedback into reflection input", {
  examples <- tibble::tibble(
    row_id = 1:2,
    score = c(1, 0),
    error = NA_character_,
    predicted = list("4", "7"),
    feedback = c("Correct.", "Expected 6, arithmetic was wrong.")
  )

  eval_result <- dsprrr:::EvalResult(
    examples = examples,
    mean_score = 0.5,
    n_evaluated = 2L,
    n_errors = 0L
  )

  dataset <- tibble::tibble(
    question = c("What is 2+2?", "What is 3+3?"),
    answer = c("4", "6")
  )

  sig <- signature("question -> answer")

  failed <- dsprrr:::gepa_failed_examples(
    eval_result,
    dataset,
    sig,
    threshold = 1
  )

  expect_length(failed, 1)
  expect_equal(failed[[1]]$feedback, "Expected 6, arithmetic was wrong.")

  formatted <- dsprrr:::gepa_format_failures(failed)
  expect_match(formatted, "Feedback: Expected 6")
})

test_that("gepa_format_failures omits feedback when absent", {
  failed <- list(
    list(
      inputs = list(question = "Q"),
      expected = "A",
      predicted = "B"
    )
  )

  formatted <- dsprrr:::gepa_format_failures(failed)
  expect_match(formatted, "Expected: A")
  expect_false(grepl("Feedback:", formatted, fixed = TRUE))
})
