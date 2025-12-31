# Tests for streaming functionality

test_that("stream method exists on Module", {
  sig <- signature("text -> result")
  mod <- module(sig, type = "predict")

  expect_true("stream" %in% names(mod))
  expect_true(is.function(mod$stream))
})

test_that("stream method exists on ReactModule", {
  sig <- signature("text -> result")
  mod <- module(sig, type = "react")

  expect_true("stream" %in% names(mod))
  expect_true(is.function(mod$stream))
})

test_that("stream with callback consumes chunks", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("coro")

  # Create a mock Chat with stream method
  chunks_received <- character()
  mock_gen <- coro::gen({
    coro::yield("Hello ")
    coro::yield("World")
    coro::yield("!")
  })

  mock_chat <- structure(
    list(
      stream = function(...) mock_gen
    ),
    class = "Chat"
  )

  sig <- signature("text -> result")
  mod <- module(sig, type = "predict", chat = mock_chat)

  result <- mod$stream(
    text = "test",
    callback = function(chunk) {
      chunks_received <<- c(chunks_received, chunk)
    }
  )

  expect_equal(chunks_received, c("Hello ", "World", "!"))
  expect_equal(result, "Hello World!")
})

test_that("stream without callback returns generator", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("coro")

  mock_gen <- coro::gen({
    coro::yield("test")
  })

  mock_chat <- structure(
    list(
      stream = function(...) mock_gen
    ),
    class = "Chat"
  )

  sig <- signature("text -> result")
  mod <- module(sig, type = "predict", chat = mock_chat)

  result <- mod$stream(text = "test")

  # Should return something callable (coro generator is a function)
  expect_true(is.function(result) || inherits(result, "coro_generator"))
})

test_that("stream validates callback is function", {
  mock_chat <- structure(
    list(
      stream = function(...) NULL
    ),
    class = "Chat"
  )

  sig <- signature("text -> result")
  mod <- module(sig, type = "predict", chat = mock_chat)

  expect_error(
    mod$stream(text = "test", callback = "not a function"),
    "must be a function"
  )
})

test_that("stream includes instructions in prompt", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("coro")

  prompt_received <- NULL
  mock_gen <- coro::gen({
    coro::yield("done")
  })

  mock_chat <- structure(
    list(
      stream = function(prompt, ...) {
        prompt_received <<- prompt
        mock_gen
      }
    ),
    class = "Chat"
  )

  sig <- signature(
    "text -> result",
    instructions = "Be very helpful"
  )
  mod <- module(sig, type = "predict", chat = mock_chat)

  mod$stream(text = "test", callback = function(x) {})

  expect_true(grepl("Be very helpful", prompt_received, fixed = TRUE))
})
