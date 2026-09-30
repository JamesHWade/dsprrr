test_that("Signature can be created with valid inputs", {
  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string")
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

test_that("Signature rejects missing instructions", {
  expect_snapshot(
    Signature(
      inputs = list(input(name = "text", type = "string")),
      output_type = ellmer::type_string(),
      instructions = NA_character_
    ),
    error = TRUE
  )
})

test_that("Signature rejects ambiguous field namespaces", {
  expect_error(signature("q, q -> answer"), "input field names must be unique")
  expect_error(
    signature("q -> answer, answer"),
    "output field names must be unique"
  )
  expect_error(signature("value -> value"), "must be disjoint")
  expect_error(
    signature(
      inputs = list(input("answer")),
      output_type = ellmer::type_string(),
      instructions = "Return an answer"
    ),
    "must be disjoint"
  )
  expect_error(
    signature("bad-name -> answer"),
    "valid, non-missing R field name"
  )
  expect_error(signature("q -> ..."), class = "dsprrr_signature_field_error")
})

test_that("signature validates instruction shape consistently", {
  for (instructions in list(NA_character_, character(), c("one", "two"))) {
    expect_error(
      signature("question -> answer", instructions = instructions),
      class = "dsprrr_signature_instruction_error"
    )
  }
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
      input(name = "data", type = "string")
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
        type = "string",
        description = "Input text"
      )
    ),
    output_type = ellmer::type_string(),
    instructions = "Process the text"
  )

  # Just verify the print method runs without error
  expect_no_error(print(sig))
})
