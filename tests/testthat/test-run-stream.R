# Tests for run_stream() and stream_listener()

test_that("stream_listener validates inputs", {
  expect_error(stream_listener(1, function(x) x), "single non-empty string")
  expect_error(stream_listener("", function(x) x), "single non-empty string")
  expect_error(
    stream_listener("answer", "not a function"),
    "must be a function"
  )

  listener <- stream_listener("answer", function(chunk) chunk)
  expect_s3_class(listener, "dsprrr_stream_listener")
  expect_equal(listener$field, "answer")
})

test_that("run_stream validates module and listeners", {
  expect_error(run_stream("not a module"), "must be a dsprrr Module")

  sig <- signature("q -> a")
  mod <- module(sig, type = "predict")

  expect_error(
    run_stream(mod, q = "x", listeners = list("bad")),
    "stream_listener"
  )
  expect_error(
    run_stream(mod, q = "x", on_status = "bad"),
    "must be a function"
  )
})

test_that("streamable_output_field detects single string outputs", {
  detect <- dsprrr:::streamable_output_field

  # Bare string type
  res <- detect(ellmer::type_string())
  expect_equal(res$field, "output")
  expect_false(res$wrap)

  # Object with one string property
  res <- detect(ellmer::type_object(answer = ellmer::type_string()))
  expect_equal(res$field, "answer")
  expect_true(res$wrap)

  # Object with multiple properties is not token-streamable
  expect_null(detect(ellmer::type_object(
    a = ellmer::type_string(),
    b = ellmer::type_string()
  )))

  # Non-string single property is not token-streamable
  expect_null(detect(ellmer::type_object(n = ellmer::type_number())))
})

test_that("run_stream token-streams single string field outputs", {
  skip_if_not_installed("coro")
  local_reset_cache()

  sig <- signature("question -> story")
  mod <- module(sig, type = "predict")

  mock_llm <- structure(
    list(
      stream = function(prompt, ...) {
        coro::generator(function() {
          for (chunk in c("Once", " upon", " a time")) {
            coro::yield(chunk)
          }
        })()
      }
    ),
    class = "Chat"
  )

  chunks <- character()
  events <- list()

  result <- run_stream(
    mod,
    question = "Tell me a story",
    .llm = mock_llm,
    listeners = stream_listener("story", function(chunk) {
      chunks <<- c(chunks, chunk)
    }),
    on_status = function(ev) {
      events[[length(events) + 1]] <<- ev
    }
  )

  expect_equal(chunks, c("Once", " upon", " a time"))
  expect_equal(result$story, "Once upon a time")

  types <- vapply(events, function(ev) ev$type, character(1))
  expect_equal(types, c("step_start", "field_start", "field_end", "step_end"))
})

test_that("run_stream falls back to one-shot events for structured output", {
  local_reset_cache()

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(answer = "42", confidence = "high")
      }
    ),
    class = "Chat"
  )

  sig <- signature("question -> answer, confidence")
  mod <- module(sig, type = "predict")

  skip_if_not(
    tryCatch(
      {
        mod$forward(list(question = "test"), .llm = mock_llm)
        TRUE
      },
      error = function(e) FALSE
    ),
    "Mock LLM not compatible with module"
  )

  received <- character()
  events <- list()

  result <- run_stream(
    mod,
    question = "What is the answer?",
    .llm = mock_llm,
    listeners = stream_listener("answer", function(chunk) {
      received <<- c(received, chunk)
    }),
    on_status = function(ev) {
      events[[length(events) + 1]] <<- ev
    }
  )

  # Listener fired exactly once with the completed value
  expect_equal(received, "42")
  expect_equal(result$answer, "42")

  types <- vapply(events, function(ev) ev$type, character(1))
  expect_equal(types, c("step_start", "field_complete", "step_end"))
})

test_that("field_complete fires once per field with multiple listeners", {
  local_reset_cache()

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(answer = "42", confidence = "high")
      }
    ),
    class = "Chat"
  )

  sig <- signature("question -> answer, confidence")
  mod <- module(sig, type = "predict")

  first <- character()
  second <- character()
  events <- list()

  run_stream(
    mod,
    question = "What is the answer?",
    .llm = mock_llm,
    listeners = list(
      stream_listener("answer", function(chunk) {
        first <<- c(first, chunk)
      }),
      stream_listener("answer", function(chunk) {
        second <<- c(second, chunk)
      })
    ),
    on_status = function(ev) {
      events[[length(events) + 1]] <<- ev
    }
  )

  # Both listeners fired, but only one field_complete event was emitted
  expect_equal(first, "42")
  expect_equal(second, "42")
  types <- vapply(events, function(ev) ev$type, character(1))
  expect_equal(sum(types == "field_complete"), 1)
})

test_that("run_stream fallback execution bypasses the response cache", {
  local_reset_cache()

  calls <- 0
  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        calls <<- calls + 1
        list(answer = as.character(calls), confidence = "high")
      }
    ),
    class = "Chat"
  )

  sig <- signature("question -> answer, confidence")
  mod <- module(sig, type = "predict")

  r1 <- run_stream(mod, question = "same question", .llm = mock_llm)
  r2 <- run_stream(mod, question = "same question", .llm = mock_llm)

  # Each streaming call hits the provider; nothing is served from cache
  expect_equal(r1$answer, "1")
  expect_equal(r2$answer, "2")
})

test_that("run_stream works across pipeline steps with status events", {
  local_reset_cache()

  MockStreamStep <- R6::R6Class(
    "MockStreamStep",
    inherit = dsprrr:::PredictModule,
    public = list(
      fn = NULL,
      initialize = function(signature, fn) {
        super$initialize(
          signature,
          template = "",
          demos = list(),
          config = list()
        )
        self$fn <- fn
      },
      forward = function(batch, .llm = NULL, trace = TRUE, .cache = NULL, ...) {
        if (is.data.frame(batch)) {
          batch <- as.list(batch[1, , drop = FALSE])
        }
        tibble::tibble(
          output = list(self$fn(batch)),
          chat = list(NULL),
          metadata = list(list())
        )
      }
    )
  )

  sig1 <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_object(
      draft = ellmer::type_string(),
      notes = ellmer::type_string()
    ),
    instructions = "Draft"
  )
  sig2 <- Signature(
    inputs = list(input(name = "draft", class = S7::class_character)),
    output_type = ellmer::type_object(
      answer = ellmer::type_string(),
      certainty = ellmer::type_string()
    ),
    instructions = "Answer"
  )

  mod1 <- MockStreamStep$new(sig1, function(b) {
    list(draft = paste0("draft of ", b$question), notes = "n")
  })
  mod2 <- MockStreamStep$new(sig2, function(b) {
    list(answer = paste0("final: ", b$draft), certainty = "high")
  })

  p <- mod1 %>>% mod2

  draft_chunks <- character()
  answer_chunks <- character()
  events <- list()

  result <- run_stream(
    p,
    question = "Q1",
    listeners = list(
      stream_listener("draft", function(chunk) {
        draft_chunks <<- c(draft_chunks, chunk)
      }),
      stream_listener("answer", function(chunk) {
        answer_chunks <<- c(answer_chunks, chunk)
      })
    ),
    on_status = function(ev) {
      events[[length(events) + 1]] <<- ev
    }
  )

  # Listeners fired at their respective steps
  expect_equal(draft_chunks, "draft of Q1")
  expect_equal(answer_chunks, "final: draft of Q1")
  expect_equal(result$answer, "final: draft of Q1")

  # Status events cover both steps in order
  steps <- vapply(events, function(ev) ev$step, integer(1))
  types <- vapply(events, function(ev) ev$type, character(1))
  expect_equal(steps[types == "step_start"], c(1L, 2L))
  expect_equal(sum(types == "field_complete"), 2)

  # No traces recorded during streaming execution
  expect_length(p$state$traces, 0)
})
