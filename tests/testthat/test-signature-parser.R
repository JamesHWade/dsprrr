test_that("parse_signature handles simple input -> output", {
  sig <- parse_signature("text -> sentiment")

  expect_s7_class(sig, Signature)
  expect_length(sig@inputs, 1)
  expect_equal(sig@inputs[[1]]$name, "text")
  expect_true(inherits(sig@output_type, "ellmer::Type"))
})

test_that("parse_signature handles multiple inputs", {
  sig <- parse_signature("context, question -> answer")

  expect_length(sig@inputs, 2)
  expect_equal(sig@inputs[[1]]$name, "context")
  expect_equal(sig@inputs[[2]]$name, "question")
})

test_that("parse_signature handles output type annotations", {
  # Float type
  sig1 <- parse_signature("question -> answer: float")
  expect_true(inherits(sig1@output_type, "ellmer::TypeBasic"))

  # Enum type
  sig2 <- parse_signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  expect_true(inherits(sig2@output_type, "ellmer::TypeEnum"))

  # String with constraints
  sig3 <- parse_signature("text -> summary: string[50, 200]")
  expect_true(inherits(sig3@output_type, "ellmer::TypeBasic"))
})

test_that("parse_signature handles Literal notation", {
  sig <- parse_signature("text -> label: Literal['a', 'b', 'c']")
  expect_true(inherits(sig@output_type, "ellmer::TypeEnum"))
})

test_that("parse_signature handles array types", {
  # Array notation
  sig1 <- parse_signature("text -> tags: array(string)")
  expect_true(inherits(sig1@output_type, "ellmer::TypeArray"))

  # List notation
  sig2 <- parse_signature("text -> items: list[string]")
  expect_true(inherits(sig2@output_type, "ellmer::TypeArray"))

  # Bracket notation
  sig3 <- parse_signature("text -> words: string[]")
  expect_true(inherits(sig3@output_type, "ellmer::TypeArray"))
})

test_that("parse_signature handles numeric bounds", {
  sig1 <- parse_signature("essay -> score: number[0, 100]")
  expect_true(inherits(sig1@output_type, "ellmer::TypeBasic"))

  sig2 <- parse_signature("data -> probability: float(0, 1)")
  expect_true(inherits(sig2@output_type, "ellmer::TypeBasic"))
})

test_that("parse_signature accepts instructions", {
  sig <- parse_signature(
    "text -> summary",
    instructions = "Summarize the key points"
  )

  expect_equal(sig@instructions, "Summarize the key points")
})


test_that("signature() constructor handles string notation", {
  sig <- signature("text -> sentiment: enum('positive', 'negative')")

  expect_s7_class(sig, Signature)
  expect_length(sig@inputs, 1)
  expect_true(inherits(sig@output_type, "ellmer::TypeEnum"))
})

test_that("signature() constructor still handles explicit notation", {
  sig <- signature(
    inputs = list(
      input("text", S7::class_character, "Text to analyze")
    ),
    output_type = ellmer::type_string(),
    instructions = "Analyze the text"
  )

  expect_s7_class(sig, Signature)
  expect_length(sig@inputs, 1)
  expect_equal(sig@inputs[[1]]$name, "text")
})

test_that("parse_signature handles edge cases", {
  # Empty input
  expect_error(parse_signature(""), "must have format")

  # Missing arrow
  expect_error(parse_signature("text sentiment"), "must have format")

  # No inputs - should provide instructions to be valid
  sig <- parse_signature(" -> output", instructions = "Generate output")
})

test_that("parse_enum_values handles various formats", {
  # Single quotes
  values1 <- parse_enum_values("'a', 'b', 'c'")
  expect_equal(values1, c("a", "b", "c"))

  # Double quotes
  values2 <- parse_enum_values('"x", "y", "z"')
  expect_equal(values2, c("x", "y", "z"))

  # No quotes
  values3 <- parse_enum_values("positive, negative, neutral")
  expect_equal(values3, c("positive", "negative", "neutral"))

  # Mixed spacing
  values4 <- parse_enum_values("  'a' ,  'b',   'c'  ")
  expect_equal(values4, c("a", "b", "c"))
})


test_that("complex signatures parse correctly", {
  # RAG-style signature
  sig <- parse_signature("context, question -> response")
  expect_length(sig@inputs, 2)
  expect_equal(sig@inputs[[1]]$name, "context")
  expect_equal(sig@inputs[[2]]$name, "question")

  # Classification with confidence
  sig2 <- signature("sentence -> sentiment: enum('positive', 'negative', 'neutral')")
  expect_true(inherits(sig2@output_type, "ellmer::TypeEnum"))

  # Math problem
  sig3 <- signature("problem -> answer: float")
  expect_true(inherits(sig3@output_type, "ellmer::TypeBasic"))
})

test_that("multiple output fields parse correctly", {
  # Simple multiple outputs
  sig1 <- parse_signature("question -> answer, confidence")
  expect_true(inherits(sig1@output_type, "ellmer::TypeObject"))
  expect_true("answer" %in% names(sig1@output_type@properties))
  expect_true("confidence" %in% names(sig1@output_type@properties))

  # Multiple outputs with types
  sig2 <- parse_signature("text -> sentiment: string, confidence: float")
  expect_true(inherits(sig2@output_type, "ellmer::TypeObject"))
  expect_true("sentiment" %in% names(sig2@output_type@properties))
  expect_true("confidence" %in% names(sig2@output_type@properties))

  # Multiple outputs with complex types
  sig3 <- parse_signature("question, choices: list[string] -> reasoning: string, selection: int")
  expect_length(sig3@inputs, 2)
  expect_true(inherits(sig3@output_type, "ellmer::TypeObject"))
  expect_true("reasoning" %in% names(sig3@output_type@properties))
  expect_true("selection" %in% names(sig3@output_type@properties))
})

test_that("Optional and Union types parse correctly", {
  # Optional type
  sig1 <- parse_signature("text -> summary: Optional[string]")
  expect_true(inherits(sig1@output_type, "ellmer::TypeBasic"))
  # Note: We set required to FALSE for Optional types
  expect_false(sig1@output_type@required)

  # Union type (simplified - uses first type)
  suppressWarnings({
    sig2 <- parse_signature("data -> result: Union[string, int]")
  })
  expect_true(inherits(sig2@output_type, "ellmer::TypeBasic"))
})

test_that("dict types parse correctly", {
  # Dict notation
  sig1 <- parse_signature("text -> metadata: dict[string, string]")
  expect_true(inherits(sig1@output_type, "ellmer::TypeObject"))

  # Dictionary notation
  sig2 <- parse_signature("data -> info: dictionary[str, list[str]]")
  expect_true(inherits(sig2@output_type, "ellmer::TypeObject"))
})

test_that("complex nested types parse correctly", {
  # List of dicts
  sig1 <- parse_signature("text -> entities: list[dict[str, str]]")
  expect_true(inherits(sig1@output_type, "ellmer::TypeArray"))

  # Nested structures in multiple outputs
  sig2 <- parse_signature("doc -> title: string, sections: list[string], metadata: dict[str, str]")
  expect_true(inherits(sig2@output_type, "ellmer::TypeObject"))
  expect_equal(length(names(sig2@output_type@properties)), 3)
})