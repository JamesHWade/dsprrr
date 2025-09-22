test_that("Predict can be created with valid signature", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify the text"
  )

  pred <- Predict(
    signature = sig,
    template = "Text: {text}"
  )

  expect_s3_class(pred, "dsprrr::Predict")
  expect_s3_class(pred@signature, "dsprrr::Signature")
  expect_equal(pred@template, "Text: {text}")
})

test_that("Predict validates signature must be Signature object", {
  expect_error(
    Predict(
      signature = "not a signature",
      template = "Template"
    ),
    "signature must be a Signature object"
  )
})

test_that("Predict validates template must be character", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  expect_error(
    Predict(
      signature = sig,
      template = 123
    ),
    "must be <character>"
  )
})

test_that("Predict accepts demos list", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  demos <- list(
    list(
      inputs = list(text = "Great product!"),
      output = "positive"
    ),
    list(
      inputs = list(text = "Terrible service"),
      output = "negative"
    )
  )

  pred <- Predict(
    signature = sig,
    template = "Text: {text}",
    demos = demos
  )

  expect_s3_class(pred, "dsprrr::Predict")
  expect_length(pred@demos, 2)
})

test_that("Predict accepts config list", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  config <- list(
    temperature = 0.7,
    max_tokens = 100
  )

  pred <- Predict(
    signature = sig,
    template = "",
    config = config
  )

  expect_s3_class(pred, "dsprrr::Predict")
  expect_equal(pred@config$temperature, 0.7)
  expect_equal(pred@config$max_tokens, 100)
})

test_that("Predict print method works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- Predict(
    signature = sig,
    template = "Text: {text}",
    demos = list(list(inputs = list(text = "test"), output = "result"))
  )

  # Just verify the print method runs without error
  expect_no_error(print(pred))
})

test_that("Predict with empty template works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- Predict(
    signature = sig,
    template = ""
  )

  expect_s3_class(pred, "dsprrr::Predict")
  expect_equal(pred@template, "")
})