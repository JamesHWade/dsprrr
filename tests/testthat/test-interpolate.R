# Tests for template interpolation

test_that("glue-style { } interpolation works", {
  mock_chat <- structure(
    list(
      chat_structured = function(prompt, ...) {
        list(result = prompt)
      }
    ),
    class = "Chat"
  )

  sig <- signature("name, age -> result")
  mod <- module(
    sig,
    type = "predict",
    template = "Hello {name}, you are {age} years old",
    chat = mock_chat
  )

  result <- run(mod, name = "Alice", age = 30)

  # The result should contain the interpolated template

  expect_true(grepl("Hello Alice", result$result, fixed = TRUE))
  expect_true(grepl("you are 30 years old", result$result, fixed = TRUE))
})

test_that("ellmer-style {{ }} interpolation works", {
  skip_if_not_installed("ellmer")

  mock_chat <- structure(
    list(
      chat_structured = function(prompt, ...) {
        list(result = prompt)
      }
    ),
    class = "Chat"
  )

  sig <- signature("name, age -> result")
  mod <- module(
    sig,
    type = "predict",
    template = "Hello {{name}}, you are {{age}} years old",
    chat = mock_chat
  )

  result <- run(mod, name = "Bob", age = 25)

  # The result should contain the interpolated template
  expect_true(grepl("Hello Bob", result$result, fixed = TRUE))
  expect_true(grepl("you are 25 years old", result$result, fixed = TRUE))
})

test_that("mixed template detection uses ellmer for {{ }}", {
  skip_if_not_installed("ellmer")

  mock_chat <- structure(
    list(
      chat_structured = function(prompt, ...) {
        list(result = prompt)
      }
    ),
    class = "Chat"
  )

  # Template with {{ }} should use ellmer interpolate
  sig <- signature("x -> result")
  mod <- module(
    sig,
    type = "predict",
    template = "Value is {{x}}",
    chat = mock_chat
  )

  result <- run(mod, x = "test_value")
  expect_true(grepl("test_value", result$result, fixed = TRUE))
})

test_that("template without braces still works", {
  mock_chat <- structure(
    list(
      chat_structured = function(prompt, ...) {
        list(result = prompt)
      }
    ),
    class = "Chat"
  )

  sig <- signature("text -> result")
  mod <- module(
    sig,
    type = "predict",
    template = "Analyze the following text",
    chat = mock_chat
  )

  # Should not error even without interpolation variables
  result <- run(mod, text = "hello")
  expect_type(result$result, "character")
})

test_that("empty template uses auto-generated format", {
  mock_chat <- structure(
    list(
      chat_structured = function(prompt, ...) {
        list(result = prompt)
      }
    ),
    class = "Chat"
  )

  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict", template = "", chat = mock_chat)

  result <- run(mod, question = "What is 2+2?")

  # Should contain the input name and value
  expect_true(grepl("question", result$result, fixed = TRUE))
  expect_true(grepl("What is 2\\+2", result$result))
})
