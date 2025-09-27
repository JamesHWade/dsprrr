test_that("PredictModule can be created with valid signature", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify the text"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}"
  )

  expect_true(inherits(pred, "PredictModule"))
  expect_true(inherits(pred, "Module"))
  expect_true(inherits(pred, "R6"))
  expect_s3_class(pred$signature, "dsprrr::Signature")
  expect_equal(pred$template, "Text: {text}")
})

test_that("module() validates signature must be Signature object", {
  expect_error(
    module(
      signature = "not a signature",
      type = "predict",
      template = "Template"
    ),
    "First argument must be a Signature object"
  )
})

test_that("PredictModule validates template must be character", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  expect_error(
    PredictModule$new(
      signature = sig,
      template = 123
    ),
    "template must be a single character string"
  )
})

test_that("PredictModule accepts demos list", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify sentiment"
  )

  demos <- list(
    list(
      inputs = list(text = "This is great!"),
      output = "positive"
    ),
    list(
      inputs = list(text = "This is terrible!"),
      output = "negative"
    )
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}",
    demos = demos
  )

  expect_equal(length(pred$demos), 2)
  expect_equal(pred$demos[[1]]$inputs$text, "This is great!")
  expect_equal(pred$demos[[1]]$output, "positive")
})

test_that("PredictModule accepts config list", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  config <- list(
    temperature = 0.5,
    max_tokens = 100,
    model = "gpt-4o-mini"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    config = config
  )

  expect_equal(pred$config$temperature, 0.5)
  expect_equal(pred$config$max_tokens, 100)
  expect_equal(pred$config$model, "gpt-4o-mini")
})

test_that("PredictModule print method works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify the text"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}"
  )

  output <- capture.output(print(pred), type = "message")
  # Check for headers - they appear in message output
  expect_true(any(grepl("PredictModule", output)))
  expect_true(any(grepl("Signature", output)))
  expect_true(any(grepl("Template", output)))
})

test_that("PredictModule with empty template works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify the text"
  )

  pred <- module(
    signature = sig,
    type = "predict"
  )

  expect_equal(pred$template, "")
})

test_that("PredictModule reset_copy works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}",
    demos = list(list(inputs = list(text = "test"), output = "result")),
    config = list(temperature = 0.7)
  )

  reset_pred <- pred$reset_copy()

  expect_true(inherits(reset_pred, "PredictModule"))
  expect_equal(reset_pred$template, pred$template)
  expect_equal(length(reset_pred$demos), 0)
  expect_equal(length(reset_pred$config), 0)
})

test_that("PredictModule deepcopy works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}",
    demos = list(list(inputs = list(text = "test"), output = "result")),
    config = list(temperature = 0.7)
  )

  copied_pred <- pred$deepcopy()

  expect_true(inherits(copied_pred, "PredictModule"))
  expect_equal(copied_pred$template, pred$template)
  expect_equal(length(copied_pred$demos), 1)
  expect_equal(copied_pred$config$temperature, 0.7)

  # Verify it's a true copy, not a reference
  copied_pred$config$temperature <- 0.9
  expect_equal(pred$config$temperature, 0.7)
})

test_that("PredictModule is_compiled works", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  pred <- module(
    signature = sig,
    type = "predict"
  )

  expect_false(pred$is_compiled())

  pred$state$compiled <- TRUE
  expect_true(pred$is_compiled())
})