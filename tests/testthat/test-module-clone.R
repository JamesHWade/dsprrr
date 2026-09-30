# Tests for Module copy functionality

test_that("copy() creates independent module", {
  sig <- signature("text -> result")

  mock_chat <- new_test_chat(
    model = "gpt-4",
    turns = list("prior turn"),
    chat_structured = function(...) list(result = "test")
  )

  mod1 <- module(sig, chat = mock_chat)
  mod2 <- mod1$copy()

  # Should be different objects
  expect_false(identical(mod1, mod2))

  # Signature should be same values
  expect_equal(mod1$signature@inputs, mod2$signature@inputs)
  expect_equal(mod1$signature@instructions, mod2$signature@instructions)
  expect_false(identical(mod1$chat, mod2$chat))
  expect_identical(mod1$chat$get_turns(), list("prior turn"))
  expect_identical(mod2$chat$get_turns(), list())
})

test_that("copy() fails closed when Chat cloning fails", {
  chat <- new_test_chat()
  override_test_chat_method(chat, "clone", function(...) stop("cannot clone"))
  mod <- module(signature("text -> result"), chat = chat)

  expect_error(
    mod$copy(),
    "Failed to deep-clone",
    class = "dsprrr_chat_clone_error"
  )
})

test_that("copy(deep = TRUE) preserves independent config and demos", {
  mod <- module(signature("text -> result"))
  mod$config$params <- list(temperature = 0.2)
  mod$demos <- list(list(text = "before", result = "original"))

  copied <- mod$copy(deep = TRUE)
  copied$config$params$temperature <- 0.9
  copied$demos[[1L]]$result <- "changed"

  expect_identical(mod$config$params$temperature, 0.2)
  expect_identical(mod$demos[[1L]]$result, "original")
  expect_identical(copied$config$params$temperature, 0.9)
  expect_identical(copied$demos[[1L]]$result, "changed")
})

test_that("copy() resets state", {
  sig <- signature("text -> result")
  mod1 <- module(sig)

  # Add some state
  mod1$state$traces <- list(list(test = TRUE))
  mod1$state$compiled <- TRUE
  mod1$state$best_score <- 0.9

  mod2 <- mod1$copy()

  # State should be reset
  expect_length(mod2$state$traces, 0)
  expect_false(mod2$state$compiled)
  expect_null(mod2$state$best_score)
})

test_that("copy() copies config with deep=TRUE", {
  sig <- signature("text -> result")
  mod1 <- module(sig)
  mod1$config$temperature <- 0.5
  mod1$config$max_tokens <- 1000

  mod2 <- mod1$copy(deep = TRUE)

  expect_equal(mod2$config$temperature, 0.5)
  expect_equal(mod2$config$max_tokens, 1000)
})

test_that("copy() with deep=FALSE still copies config values", {
  sig <- signature("text -> result")
  mod1 <- module(sig)
  mod1$config$temperature <- 0.5

  mod2 <- mod1$copy(deep = FALSE)

  # Config values are copied
  expect_equal(mod2$config$temperature, 0.5)

  # But modifying one does NOT affect the other (shallow copy creates new list)
  mod2$config$temperature <- 0.7
  expect_equal(mod1$config$temperature, 0.5) # Original unchanged
  expect_equal(mod2$config$temperature, 0.7) # New value in copy
})

test_that("PredictModule copy preserves template", {
  sig <- signature("text -> result")
  mod1 <- module(sig, template = "Custom: {text}")

  mod2 <- mod1$copy()

  expect_equal(mod2$template, "Custom: {text}")
})

test_that("ReactModule copy preserves tools and max_iterations", {
  skip_if_not_installed("ellmer")

  # Create a simple tool for testing
  test_fn <- function(x) x
  test_tool <- tryCatch(
    ellmer::tool(
      test_fn,
      name = "test_tool",
      description = "A test tool",
      arguments = list(x = ellmer::type_string())
    ),
    error = function(e) NULL
  )

  skip_if(is.null(test_tool), "Could not create test tool")

  sig <- signature("text -> result")
  mod1 <- react(
    sig,
    tools = list(test_tool),
    max_iterations = 15L
  )

  mod2 <- mod1$copy()

  expect_length(mod2$tools, 1)
  expect_equal(mod2$max_iterations, 15L)
})
