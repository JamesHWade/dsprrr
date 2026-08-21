# Tests for template interpolation

test_that("glue-style { } interpolation works", {
  mock_chat <- new_test_chat(
    chat_structured = function(prompt, ...) {
      list(result = prompt)
    }
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

test_that("double-brace placeholders are rejected before provider work", {
  calls <- 0L
  mock_chat <- new_test_chat(
    chat_structured = function(...) {
      calls <<- calls + 1L
      list(result = "unexpected")
    }
  )
  mod <- module(
    signature("name -> result"),
    type = "predict",
    template = "Hello {{name}}",
    chat = mock_chat
  )

  condition <- rlang::catch_cnd(run(mod, name = "Alice"))

  expect_s3_class(condition, "dsprrr_template_syntax_error")
  expect_match(conditionMessage(condition), "single-brace placeholders")
  expect_identical(calls, 0L)
})

test_that("template without braces still works", {
  mock_chat <- new_test_chat(
    chat_structured = function(prompt, ...) {
      list(result = prompt)
    }
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
  mock_chat <- new_test_chat(
    chat_structured = function(prompt, ...) {
      list(result = prompt)
    }
  )

  sig <- signature("question -> answer")
  mod <- module(sig, type = "predict", template = "", chat = mock_chat)

  result <- run(mod, question = "What is 2+2?")

  # Should contain the input name and value
  expect_true(grepl("question", result$result, fixed = TRUE))
  expect_true(grepl("What is 2\\+2", result$result))
})
