test_that("signature_to_json_schema converts object outputs", {
  schema <- signature_to_json_schema(
    "question -> answer, confidence: number, citations: array(string)"
  )

  expect_equal(schema$type, "object")
  expect_named(schema$properties, c("answer", "confidence", "citations"))
  expect_equal(schema$properties$answer$type, "string")
  expect_equal(schema$properties$confidence$type, "number")
  expect_equal(schema$properties$citations$type, "array")
  expect_equal(schema$properties$citations$items$type, "string")
  expect_equal(schema$required, c("answer", "confidence", "citations"))
  expect_false(schema$additionalProperties)
})

test_that("signature_to_json_schema converts enum and optional fields", {
  sig <- Signature(
    inputs = list(input("text")),
    output_type = ellmer::type_object(
      label = ellmer::type_enum(c("yes", "no")),
      rationale = ellmer::type_string(required = FALSE)
    )
  )

  schema <- signature_to_json_schema(sig)

  expect_equal(schema$properties$label$type, "string")
  expect_equal(schema$properties$label$enum, c("yes", "no"))
  expect_equal(schema$required, "label")
})

test_that("signature_to_json_schema converts scalar output types", {
  sig <- Signature(
    inputs = list(input("text")),
    output_type = ellmer::type_boolean()
  )

  schema <- signature_to_json_schema(sig)

  expect_equal(schema$type, "boolean")
})
