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

test_that("parse_signature marks only annotated input types explicit", {
  sig <- parse_signature("context, age: integer -> answer")

  expect_false(sig@inputs[[1]]$.type_explicit)
  expect_true(sig@inputs[[2]]$.type_explicit)
})

test_that("parse_signature handles output type annotations", {
  # Float type - wrapped in TypeObject
  sig1 <- parse_signature("question -> answer: float")
  expect_true(inherits(sig1@output_type, "ellmer::TypeObject"))
  # Check the field type
  expect_true(inherits(sig1@output_type@properties$answer, "ellmer::TypeBasic"))

  # Enum type - wrapped in TypeObject
  sig2 <- parse_signature(
    "text -> sentiment: enum('positive', 'negative', 'neutral')"
  )
  expect_true(inherits(sig2@output_type, "ellmer::TypeObject"))
  # Check the field type
  expect_true(inherits(
    sig2@output_type@properties$sentiment,
    "ellmer::TypeEnum"
  ))

  # String with constraints - wrapped in TypeObject
  sig3 <- parse_signature("text -> summary: string[50, 200]")
  expect_true(inherits(sig3@output_type, "ellmer::TypeObject"))
  # Check the field type
  expect_true(inherits(
    sig3@output_type@properties$summary,
    "ellmer::TypeBasic"
  ))
})

test_that("parse_signature handles Literal notation", {
  sig <- parse_signature("text -> label: Literal['a', 'b', 'c']")
  expect_true(inherits(sig@output_type, "ellmer::TypeObject"))
  expect_true(inherits(sig@output_type@properties$label, "ellmer::TypeEnum"))
})

test_that("parse_signature handles array types", {
  # Array notation - wrapped in TypeObject
  sig1 <- parse_signature("text -> tags: array(string)")
  expect_true(inherits(sig1@output_type, "ellmer::TypeObject"))
  expect_true(inherits(sig1@output_type@properties$tags, "ellmer::TypeArray"))

  # List notation - wrapped in TypeObject
  sig2 <- parse_signature("text -> items: list[string]")
  expect_true(inherits(sig2@output_type, "ellmer::TypeObject"))
  expect_true(inherits(sig2@output_type@properties$items, "ellmer::TypeArray"))

  # Bracket notation - wrapped in TypeObject
  sig3 <- parse_signature("text -> words: string[]")
  expect_true(inherits(sig3@output_type, "ellmer::TypeObject"))
  expect_true(inherits(sig3@output_type@properties$words, "ellmer::TypeArray"))
})

test_that("parse_signature handles numeric bounds", {
  sig1 <- parse_signature("essay -> score: number[0, 100]")
  expect_true(inherits(sig1@output_type, "ellmer::TypeObject"))
  expect_true(inherits(sig1@output_type@properties$score, "ellmer::TypeBasic"))

  sig2 <- parse_signature("data -> probability: float(0, 1)")
  expect_true(inherits(sig2@output_type, "ellmer::TypeObject"))
  expect_true(inherits(
    sig2@output_type@properties$probability,
    "ellmer::TypeBasic"
  ))
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
  expect_true(inherits(sig@output_type, "ellmer::TypeObject"))
  expect_true(inherits(
    sig@output_type@properties$sentiment,
    "ellmer::TypeEnum"
  ))
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
  # Empty input - now has better error message
  expect_error(parse_signature(""), "cannot be empty")

  # Missing arrow - now suggests fix
  expect_error(parse_signature("text sentiment"), "Missing.*->.*separator")

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
  sig2 <- signature(
    "sentence -> sentiment: enum('positive', 'negative', 'neutral')"
  )
  expect_true(inherits(sig2@output_type, "ellmer::TypeObject"))
  expect_true(inherits(
    sig2@output_type@properties$sentiment,
    "ellmer::TypeEnum"
  ))

  # Math problem
  sig3 <- signature("problem -> answer: float")
  expect_true(inherits(sig3@output_type, "ellmer::TypeObject"))
  expect_true(inherits(sig3@output_type@properties$answer, "ellmer::TypeBasic"))
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

  dotted <- parse_signature(
    "question -> first: string, second.field: number"
  )
  expect_identical(
    names(dotted@output_type@properties),
    c("first", "second.field")
  )

  colon_values <- parse_signature(
    "question -> link: enum('http://a', '12:30'), note: string"
  )
  expect_identical(
    colon_values@output_type@properties$link@values,
    c("http://a", "12:30")
  )

  # Multiple outputs with complex types
  sig3 <- parse_signature(
    "question, choices: list[string] -> reasoning: string, selection: int"
  )
  expect_length(sig3@inputs, 2)
  expect_true(inherits(sig3@output_type, "ellmer::TypeObject"))
  expect_true("reasoning" %in% names(sig3@output_type@properties))
  expect_true("selection" %in% names(sig3@output_type@properties))
})

test_that("Optional and Union types parse correctly", {
  # Optional type - wrapped in TypeObject
  sig1 <- parse_signature("text -> summary: Optional[string]")
  expect_true(inherits(sig1@output_type, "ellmer::TypeObject"))
  # The field should have required=FALSE
  expect_true(inherits(
    sig1@output_type@properties$summary,
    "ellmer::TypeBasic"
  ))
  expect_false(sig1@output_type@properties$summary@required)

  # Union type (simplified - uses first type) - wrapped in TypeObject
  suppressWarnings({
    sig2 <- parse_signature("data -> result: Union[string, int]")
  })
  expect_true(inherits(sig2@output_type, "ellmer::TypeObject"))
  expect_true(inherits(sig2@output_type@properties$result, "ellmer::TypeBasic"))
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
  # List of dicts - wrapped in TypeObject
  sig1 <- parse_signature("text -> entities: list[dict[str, str]]")
  expect_true(inherits(sig1@output_type, "ellmer::TypeObject"))
  expect_true(inherits(
    sig1@output_type@properties$entities,
    "ellmer::TypeArray"
  ))

  # Nested structures in multiple outputs
  sig2 <- parse_signature(
    "doc -> title: string, sections: list[string], metadata: dict[str, str]"
  )
  expect_true(inherits(sig2@output_type, "ellmer::TypeObject"))
  expect_equal(length(names(sig2@output_type@properties)), 3)
})

# ============================================================================
# Error Handling Tests
# ============================================================================

test_that("parse_signature errors on non-string input", {
  expect_error(
    parse_signature(123),
    "must be a single character string"
  )

  expect_error(
    parse_signature(c("a -> b", "c -> d")),
    "must be a single character string"
  )
  expect_error(
    parse_signature(NA_character_),
    "must be a single character string"
  )
})

test_that("parse_signature validates instructions before generating defaults", {
  for (instructions in list(NA_character_, character(), c("one", "two"))) {
    expect_error(
      parse_signature("question -> answer", instructions = instructions),
      class = "dsprrr_signature_instruction_error"
    )
  }
})

test_that("parse_signature errors on empty string", {
  expect_error(
    parse_signature(""),
    "cannot be empty"
  )

  expect_error(
    parse_signature("   "),
    "cannot be empty"
  )
})

test_that("parse_signature detects wrong arrow types", {
  # Fat arrow
  expect_error(
    parse_signature("text => sentiment"),
    "Use.*->.*not.*=>"
  )

  # Double dash arrow
  expect_error(
    parse_signature("text --> sentiment"),
    "Use.*->.*not.*-->"
  )

  # Left arrow
  expect_error(
    parse_signature("text <- sentiment"),
    "Use.*->.*not.*<-"
  )
})

test_that("parse_signature suggests fix for missing arrow", {
  # Two words without arrow
  expect_error(
    parse_signature("question answer"),
    "Did you mean.*question -> answer"
  )

  # Multiple words without arrow
  expect_error(
    parse_signature("context question answer"),
    "Did you mean"
  )
})

test_that("parse_signature errors on multiple arrows", {
  expect_error(
    parse_signature("a -> b -> c"),
    "Multiple.*separators"
  )
})

# ============================================================================
# Helper Function Tests
# ============================================================================

test_that("detect_arrow_mistake identifies common mistakes", {
  # Fat arrow
  result <- dsprrr:::detect_arrow_mistake("text => output")
  expect_false(is.null(result))
  expect_equal(result$corrected, "text -> output")

  # Double dash
  result <- dsprrr:::detect_arrow_mistake("text --> output")
  expect_false(is.null(result))
  expect_equal(result$corrected, "text -> output")

  # Correct arrow returns NULL
  result <- dsprrr:::detect_arrow_mistake("text -> output")
  expect_null(result)
})

test_that("suggest_signature_fix provides helpful suggestions", {
  # Two words
  result <- dsprrr:::suggest_signature_fix("question answer")
  expect_match(result, "question -> answer")

  # Single word
  result <- dsprrr:::suggest_signature_fix("question")
  expect_match(result, "->")
})

test_that("find_closest_match works correctly", {
  candidates <- c("text", "question", "context")

  # Exact match
  expect_equal(dsprrr:::find_closest_match("text", candidates), "text")

  # Close match (typo)
  expect_equal(dsprrr:::find_closest_match("tect", candidates), "text")
  expect_equal(dsprrr:::find_closest_match("questoin", candidates), "question")

  # No close match
  expect_null(dsprrr:::find_closest_match("zzzzz", candidates))
})

test_that("unknown / misspelled types raise an error instead of silently becoming strings (dsprrr-47p)", {
  # Regression: validate_type_string() existed but was never called, so typo'd
  # types silently fell through to the type_string() default.
  expect_error(parse_signature("question -> answer: interger"), "Unknown type")
  expect_error(
    parse_signature("question -> answer: frobnicate"),
    "Unknown type"
  )
  expect_error(parse_signature("question -> answer: numbr"), "Unknown type")

  # The error suggests the closest valid type.
  expect_error(parse_signature("q -> a: integ"), "Did you mean")
})

test_that("valid types still parse after type validation is wired in", {
  expect_no_error(parse_signature("q -> a: string"))
  expect_no_error(parse_signature("q -> a: integer"))
  expect_no_error(parse_signature("q -> a: number"))
  expect_no_error(parse_signature("q -> a: boolean"))
  expect_no_error(parse_signature("q -> a: enum('x', 'y')"))
  expect_no_error(parse_signature("q -> a: array(string)"))
  # Python-style aliases remain valid.
  expect_no_error(parse_signature("q -> a: str"))
  expect_no_error(parse_signature("q -> a: int"))
})
