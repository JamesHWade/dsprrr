test_that("Signature can be created with valid inputs", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify the text"
  )

  expect_s3_class(sig, "dsprrr::Signature")
  expect_length(sig@inputs, 1)
  expect_true(is_ellmer_type(sig@output_type))
  expect_equal(sig@instructions, "Classify the text")
})

test_that("Signature validates inputs must be created with input()", {
  expect_error(
    Signature(
      inputs = list("not an input"),
      output_type = ellmer::type_string()
    ),
    "Input 1 must be created with input\\(\\) function"
  )
})

test_that("Signature validates output_type must be ellmer type", {
  expect_error(
    Signature(
      inputs = list(),
      output_type = "not a type"
    ),
    "output_type must be an ellmer type object"
  )
})

test_that("Signature requires output_type", {
  expect_error(
    Signature(
      inputs = list(),
      output_type = NULL
    ),
    "output_type cannot be NULL"
  )
})

test_that("Signature validates instructions must be character", {
  expect_error(
    Signature(
      inputs = list(),
      output_type = ellmer::type_string(),
      instructions = 123
    ),
    "must be <character>"
  )
})

test_that("Signature requires either inputs or instructions", {
  expect_error(
    Signature(
      inputs = list(),
      output_type = ellmer::type_string(),
      instructions = ""
    ),
    "Signature must have either inputs or instructions defined"
  )
})

test_that("Signature with complex output types works", {
  sig <- Signature(
    inputs = list(
      input(name = "data", class = S7::class_character)
    ),
    output_type = ellmer::type_object(
      sentiment = ellmer::type_enum(
        values = c("positive", "negative", "neutral"),
        description = "The sentiment"
      ),
      confidence = ellmer::type_number(
        description = "Confidence score"
      )
    ),
    instructions = "Analyze sentiment"
  )

  expect_s3_class(sig, "dsprrr::Signature")
  expect_true(is_ellmer_type(sig@output_type))
})

test_that("Signature print method works", {
  sig <- Signature(
    inputs = list(
      input(
        name = "text",
        class = S7::class_character,
        description = "Input text"
      )
    ),
    output_type = ellmer::type_string(),
    instructions = "Process the text"
  )

  # Just verify the print method runs without error
  expect_no_error(print(sig))
})
