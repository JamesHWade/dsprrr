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

test_that("build_simple_prompt creates expected format", {
  inputs <- list(name = "Alice", age = 30)
  specs <- list(
    list(name = "name"),
    list(name = "age")
  )

  result <- dsprrr:::build_simple_prompt(inputs, specs)

  expect_true(grepl("Input:", result))
  expect_true(grepl("name: Alice", result))
  expect_true(grepl("age: 30", result))
})

test_that("build_simple_prompt handles empty inputs", {
  result <- dsprrr:::build_simple_prompt(list(), list())
  expect_equal(result, "")
})

test_that("build_simple_prompt handles missing inputs", {
  inputs <- list(name = "Alice")
  specs <- list(
    list(name = "name"),
    list(name = "age")  # Not provided in inputs
  )

  result <- dsprrr:::build_simple_prompt(inputs, specs)

  expect_true(grepl("name: Alice", result))
  expect_false(grepl("age:", result))  # Should not include missing input
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
  skip_if(!("chat_structured_async" %in% names(chat)),
          "chat_structured_async not available")

  sig <- signature("text -> result")
  mod <- module(sig, type = "predict", chat = chat)

  # run_async should return a promise
  result <- mod$run_async(text = "Hello")
  expect_s3_class(result, "promise")
})
