# Tests for async functionality

test_that("run_async function exists", {
  expect_true(is.function(run_async))
})

test_that("stream_async function exists", {
  expect_true(is.function(stream_async))
})

test_that("run_async method exists on Module", {
  sig <- signature("text -> result")
  mod <- module(sig, type = "predict")

  expect_true("run_async" %in% names(mod))
  expect_true(is.function(mod$run_async))
})

test_that("stream_async method exists on Module", {
  sig <- signature("text -> result")
  mod <- module(sig, type = "predict")

  expect_true("stream_async" %in% names(mod))
  expect_true(is.function(mod$stream_async))
})

test_that("run_async validates module argument", {
  expect_error(
    run_async("not a module", text = "test"),
    "must be a dsprrr Module"
  )
})

test_that("stream_async validates module argument", {
  expect_error(
    stream_async("not a module", text = "test"),
    "must be a dsprrr Module"
  )
})

test_that("direct async paths fail closed for composite modules", {
  ordinary <- module(signature("question -> draft"))
  specialized <- suppressWarnings(flex("draft -> answer"))
  nested <- pipeline(ordinary, specialized)

  operations <- list(
    run_async = function() run_async(nested, question = "Why?"),
    stream_async = function() stream_async(nested, question = "Why?")
  )

  for (operation in names(operations)) {
    condition <- expect_error(
      operations[[operation]](),
      class = "dsprrr_specialized_async_unsupported"
    )
    expect_identical(condition$operation, operation)
    expect_identical(condition$module_class, "PipelineModule")
    expect_identical(condition$module_path, "$")
  }
})

test_that("run_stream preflights unsafe token steps before provider work", {
  skip_if_not_installed("coro")

  ordinary <- module(signature("question -> draft"))
  specialized <- suppressWarnings(flex("draft -> answer"))
  nested <- pipeline(ordinary, specialized)
  provider_calls <- 0L
  chat <- structure(
    list(
      chat_structured = function(...) {
        provider_calls <<- provider_calls + 1L
        list(draft = "draft")
      },
      stream = function(...) {
        provider_calls <<- provider_calls + 1L
        stop("provider must not be reached")
      }
    ),
    class = "Chat"
  )

  condition <- expect_error(
    run_stream(
      nested,
      question = "Why?",
      .llm = chat,
      listeners = stream_listener("answer", function(chunk) NULL)
    ),
    class = "dsprrr_specialized_async_unsupported"
  )
  expect_identical(condition$module_class, "FlexModule")
  expect_match(condition$module_path, "steps")
  expect_identical(provider_calls, 0L)
})

test_that("async rejection traverses module wrappers", {
  nested <- best_of_n(suppressWarnings(flex("question -> answer")), N = 1L)

  condition <- expect_error(
    run_async(nested, question = "Why?"),
    class = "dsprrr_specialized_async_unsupported"
  )
  expect_identical(condition$module_class, "BestOfNModule")
  expect_identical(condition$module_path, "$")
})

test_that("direct async paths reject React before provider work", {
  agent <- module(signature("question -> answer"), type = "react")
  provider_calls <- 0L
  chat <- structure(
    list(
      chat_structured_async = function(...) {
        provider_calls <<- provider_calls + 1L
      },
      stream_async = function(...) {
        provider_calls <<- provider_calls + 1L
      },
      stream = function(...) {
        provider_calls <<- provider_calls + 1L
      }
    ),
    class = "Chat"
  )

  operations <- list(
    run_async = function() run_async(agent, question = "Why?", .llm = chat),
    stream_async = function() {
      stream_async(agent, question = "Why?", .llm = chat)
    },
    stream = function() agent$stream(question = "Why?", .llm = chat)
  )
  for (operation in names(operations)) {
    condition <- expect_error(
      operations[[operation]](),
      class = "dsprrr_specialized_async_unsupported"
    )
    expect_identical(condition$operation, operation)
    expect_identical(condition$module_class, "ReactModule")
  }
  expect_identical(provider_calls, 0L)
})

test_that("build_simple_prompt creates expected format", {
  inputs <- list(name = "Alice", age = 30)
  specs <- list(
    list(name = "name"),
    list(name = "age")
  )

  result <- dsprrr:::build_simple_prompt(inputs, specs)

  expect_true(grepl("Input:", result, fixed = TRUE))
  expect_true(grepl("name: Alice", result, fixed = TRUE))
  expect_true(grepl("age: 30", result, fixed = TRUE))
})

test_that("build_simple_prompt handles empty inputs", {
  result <- dsprrr:::build_simple_prompt(list(), list())
  expect_equal(result, "")
})

test_that("build_simple_prompt handles missing inputs", {
  inputs <- list(name = "Alice")
  specs <- list(
    list(name = "name"),
    list(name = "age") # Not provided in inputs
  )

  result <- dsprrr:::build_simple_prompt(inputs, specs)

  expect_true(grepl("name: Alice", result, fixed = TRUE))
  expect_false(grepl("age:", result, fixed = TRUE)) # Should not include missing input
})

# Integration tests (require ellmer with async support)
test_that("run_async returns a promise", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("promises")

  # Check if chat_structured_async exists
  chat <- tryCatch(
    ellmer::chat_openai(),
    error = function(e) NULL
  )

  skip_if(is.null(chat), "Could not create chat")
  skip_if(
    !("chat_structured_async" %in% names(chat)),
    "chat_structured_async not available"
  )

  sig <- signature("text -> result")
  mod <- module(sig, type = "predict", chat = chat)

  # run_async should return a promise
  result <- mod$run_async(text = "Hello")
  expect_s3_class(result, "promise")
})

test_that("run, run_async, and stream_async share prompt assembly", {
  captured <- new.env(parent = emptyenv())

  mock_chat <- structure(
    list(
      get_turns = function() list(),
      last_turn = function(...) NULL,
      chat_structured = function(prompt, type, ...) {
        captured$run <- prompt
        "ok"
      },
      chat_structured_async = function(prompt, type, ...) {
        captured$async <- prompt
        "async"
      },
      stream_async = function(prompt, ...) {
        captured$stream <- prompt
        "stream"
      }
    ),
    class = "Chat"
  )

  mod <- module(
    signature("text -> result", instructions = "Be concise"),
    type = "predict",
    template = "Text: {text}",
    chat = mock_chat
  )

  run(mod, text = "hello")
  run_async(mod, text = "hello")
  stream_async(mod, text = "hello")

  expect_identical(captured$run, captured$async)
  expect_identical(captured$run, captured$stream)
})
