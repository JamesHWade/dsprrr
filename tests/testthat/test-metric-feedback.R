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

test_that("metric_with_trace creates a classed three-argument metric", {
  metric <- metric_with_trace(
    function(prediction, expected, program_trace) {
      list(
        score = as.numeric(identical(prediction, expected)),
        feedback = paste("epoch", program_trace$epoch)
      )
    },
    field = "answer"
  )
  trace <- dsprrr:::new_program_trace(
    events = list(list(call = "predict")),
    metadata = list(total_tokens = 12L),
    row_id = 2L,
    epoch = 3L
  )

  expect_s3_class(metric, "dsprrr_trace_metric")
  expect_identical(attr(metric, "field"), "answer")
  expect_s3_class(trace, "dsprrr_program_trace")
  expect_identical(trace$row_id, 2L)
  expect_identical(trace$metadata$total_tokens, 12L)
  expect_identical(trace$status, "ok")
  expect_length(trace$events, 1L)
  expect_identical(
    metric("a", "a", trace),
    list(score = 1, feedback = "epoch 3")
  )
})

test_that("metric_with_trace validates its contract", {
  expect_snapshot(metric_with_trace("not a function"), error = TRUE)
  expect_snapshot(
    metric_with_trace(function(prediction, expected) 1),
    error = TRUE
  )
  expect_snapshot(
    metric_with_trace(
      function(prediction, expected, program_trace) 1,
      field = c("answer", "other")
    ),
    error = TRUE
  )
  expect_error(
    metric_with_trace(function(prediction, expected, trace, extra) 1),
    class = "dsprrr_trace_metric_signature_error"
  )
  expect_error(
    metric_with_trace(function(prediction, expected, ..., extra) 1),
    class = "dsprrr_trace_metric_signature_error"
  )
})

test_that("metric_with_trace supports named arguments around ellipsis", {
  trace <- dsprrr:::new_program_trace(
    metadata = list(),
    row_id = 1L,
    epoch = 1L
  )
  after <- metric_with_trace(function(prediction, ..., program_trace) {
    as.numeric(prediction == program_trace$row_id)
  })
  before <- metric_with_trace(function(..., program_trace) {
    as.numeric(program_trace$epoch == 1L)
  })
  trailing <- metric_with_trace(function(p, e, ...) {
    as.numeric(p == e)
  })
  positional <- metric_with_trace(function(p, e, trace, ...) {
    as.numeric(p == e && trace$row_id == 1L)
  })

  expect_equal(after(1L, NULL, trace), 1)
  expect_equal(before(1L, NULL, trace), 1)
  expect_equal(trailing("same", "same", trace), 1)
  expect_equal(positional("same", "same", trace), 1)
})

test_that("trace-aware threshold metrics preserve trace dispatch", {
  seen <- NULL
  base <- metric_with_trace(
    function(prediction, expected, program_trace) {
      seen <<- program_trace
      list(score = 0.7, feedback = "costly")
    }
  )
  metric <- metric_threshold(base, 0.8)
  trace <- dsprrr:::new_program_trace(
    events = list(list(call = "predict")),
    metadata = list(total_tokens = 100L),
    row_id = 1L,
    epoch = 1L
  )

  expect_s3_class(metric, "dsprrr_trace_metric")
  expect_identical(
    metric("prediction", "expected", trace),
    list(score = FALSE, feedback = "costly")
  )
  expect_identical(seen, trace)
})

test_that("trace events align by batch index, not completion order", {
  events <- list(
    list(metadata = list(batch_index = 2L), output = "second"),
    list(metadata = list(batch_index = 1L), output = "first")
  )

  aligned <- dsprrr:::align_evaluation_trace_events(events, 2L)

  expect_identical(aligned[[1L]][[1L]]$output, "first")
  expect_identical(aligned[[2L]][[1L]]$output, "second")
})

test_that("mixed indexed traces never use unsafe positional fallback", {
  events <- list(
    list(metadata = list(batch_index = 2L), output = "second"),
    list(metadata = list(), output = "also second but unindexed"),
    list(metadata = list(), output = "unknown")
  )

  aligned <- dsprrr:::align_evaluation_trace_events(events, 3L)

  expect_length(aligned[[1L]], 0L)
  expect_identical(aligned[[2L]][[1L]]$output, "second")
  expect_length(aligned[[3L]], 0L)
})

test_that("duplicate indexed traces stay ordered on their declared row", {
  events <- list(
    list(metadata = list(batch_index = 2L), output = "first"),
    list(metadata = list(batch_index = 2L), output = "second"),
    list(metadata = list(batch_index = 99L), output = "out of range")
  )

  aligned <- dsprrr:::align_evaluation_trace_events(events, 3L)

  expect_identical(
    vapply(aligned[[2L]], `[[`, character(1), "output"),
    c("first", "second")
  )
  expect_length(aligned[[1L]], 0L)
  expect_length(aligned[[3L]], 0L)
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

test_that("evaluate supplies row-level traces to metrics and optimizers", {
  local_reset_cache()

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) list(a = "mocked")
    ),
    class = "Chat"
  )
  mod <- module(signature("q -> a"), type = "predict")
  dataset <- tibble::tibble(q = "Q1", a = "mocked")
  seen <- list()
  metric <- metric_with_trace(
    function(prediction, expected, program_trace) {
      seen[[length(seen) + 1L]] <<- program_trace
      list(
        score = as.numeric(identical(prediction$a, expected$a)),
        feedback = paste("prompt chars", program_trace$metadata$prompt_length)
      )
    },
    field = "a"
  )

  result <- expect_test_warnings(
    evaluate(
      mod,
      dataset,
      metric,
      .llm = mock_llm,
      .progress = FALSE,
      .cache = FALSE,
      epochs = 2L
    ),
    "Confidence intervals"
  )

  expect_length(seen, 2L)
  expect_identical(vapply(seen, `[[`, integer(1), "epoch"), 1:2)
  expect_identical(vapply(seen, `[[`, integer(1), "row_id"), c(1L, 1L))
  expect_true(all(vapply(
    seen,
    inherits,
    logical(1),
    "dsprrr_program_trace"
  )))
  expect_gt(seen[[1]]$metadata$prompt_length, 0L)
  expect_identical(result$traces[[1]]$epoch, 2L)
  expect_length(result$epoch_traces, 2L)
  expect_match(result$feedbacks, "prompt chars")

  optimized_view <- eval_program(
    mod,
    dataset,
    metric,
    .llm = mock_llm,
    control = optimizer_control(progress = FALSE),
    .cache = FALSE
  )
  expect_true("program_trace" %in% names(optimized_view@examples))
  expect_s3_class(
    optimized_view@examples$program_trace[[1]],
    "dsprrr_program_trace"
  )
})

test_that("simple evaluation results do not expose raw traces", {
  local_reset_cache()
  llm <- structure(
    list(chat_structured = function(...) list(a = "mocked")),
    class = "Chat"
  )
  mod <- module(signature("q -> a"), type = "predict")
  dataset <- tibble::tibble(q = "Q1", a = "mocked")
  metric <- metric_with_trace(function(prediction, expected, program_trace) {
    as.numeric(length(program_trace$events) > 0L)
  })

  one <- evaluate(
    mod,
    dataset,
    metric,
    .llm = llm,
    .progress = FALSE,
    .cache = FALSE,
    .return_format = "simple"
  )
  two <- expect_warning(
    evaluate(
      mod,
      dataset,
      metric,
      .llm = llm,
      .progress = FALSE,
      .cache = FALSE,
      .return_format = "simple",
      epochs = 2L
    ),
    "Confidence intervals"
  )

  expect_false(any(c("traces", "epoch_traces") %in% names(one)))
  expect_false(any(c("traces", "epoch_traces") %in% names(two)))
})

test_that("row-budgeted optimizer metrics retain dataset row IDs", {
  local_reset_cache()

  mock_llm <- structure(
    list(chat_structured = function(prompt, type, ...) list(a = "mocked")),
    class = "Chat"
  )
  mod <- module(signature("q -> a"), type = "predict")
  dataset <- tibble::tibble(
    q = c("Q1", "Q2"),
    a = c("mocked", "mocked")
  )
  seen_row_ids <- integer()
  metric <- metric_with_trace(function(prediction, expected, program_trace) {
    seen_row_ids <<- c(seen_row_ids, program_trace$row_id)
    1
  })
  control <- optimizer_control(
    max_metric_calls = 2L,
    num_threads = 1L,
    progress = FALSE
  )
  budget <- dsprrr:::new_optimizer_budget(control)

  result <- dsprrr:::optimizer_eval_program(
    mod,
    dataset,
    metric,
    .llm = mock_llm,
    control = control,
    budget = budget,
    stage = "search",
    unit_id = "trace-row-ids",
    .cache = FALSE
  )

  expect_identical(seen_row_ids, 1:2)
  expect_identical(
    vapply(
      result@examples$program_trace,
      `[[`,
      integer(1),
      "row_id"
    ),
    1:2
  )
})

test_that("evaluate validates internal trace row mappings", {
  mod <- module(signature("q -> a"), type = "predict")
  dataset <- tibble::tibble(q = c("Q1", "Q2"), a = c("A1", "A2"))

  expect_error(
    evaluate(
      mod,
      dataset,
      metric = function(...) 1,
      .trace_row_ids = c(1L, 1L),
      .progress = FALSE
    ),
    class = "dsprrr_evaluation_trace_row_error"
  )
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

test_that("gepa_failed_examples honors the primary metric field", {
  eval_result <- dsprrr:::EvalResult(
    examples = tibble::tibble(
      row_id = 1L,
      score = 0,
      error = NA_character_,
      predicted = list("wrong"),
      feedback = "Trace found an inefficient call."
    ),
    mean_score = 0,
    n_evaluated = 1L,
    n_errors = 0L
  )
  dataset <- tibble::tibble(
    question = "q",
    answer = "DECOY",
    gold = "TRUTH"
  )

  failed <- dsprrr:::gepa_failed_examples(
    eval_result,
    dataset,
    signature("question -> gold"),
    threshold = 1,
    output_col = "gold"
  )

  expect_identical(failed[[1L]]$expected, "TRUTH")
  expect_identical(
    failed[[1L]]$feedback,
    "Trace found an inefficient call."
  )
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
