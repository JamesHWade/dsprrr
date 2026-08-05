# Tests for signature-transforms.R

test_that("instruction transforms are immutable and composable", {
  original <- signature(
    "text -> summary",
    instructions = "Summarize the text."
  )

  appended <- append_instructions(original, "Use at most 30 words.")
  chained <- append_instructions(appended, "Preserve named entities.")
  replaced <- with_instructions(original, "Return one sentence.")

  expect_identical(original@instructions, "Summarize the text.")
  expect_identical(
    appended@instructions,
    "Summarize the text.\n\nUse at most 30 words."
  )
  expect_identical(
    chained@instructions,
    paste(
      "Summarize the text.",
      "Use at most 30 words.",
      "Preserve named entities.",
      sep = "\n\n"
    )
  )
  expect_identical(replaced@instructions, "Return one sentence.")
  expect_identical(appended@inputs, original@inputs)
  expect_identical(appended@output_type, original@output_type)
  expect_false(identical(appended, original))
})

test_that("module optimization replaces its signature copy-on-write", {
  original <- signature("question -> answer", instructions = "Original")
  mod <- module(original, type = "predict")

  mod$apply_optimization_params(list(instructions = "Optimized"))

  expect_identical(original@instructions, "Original")
  expect_identical(mod$signature@instructions, "Optimized")
  expect_false(identical(mod$signature, original))
})

test_that("instruction transforms accept strings and handle empty text", {
  appended <- append_instructions("question -> answer", "Cite the evidence.")
  unchanged <- append_instructions(appended, "")
  cleared <- with_instructions(appended, "")

  expect_match(appended@instructions, "Cite the evidence\\.$")
  expect_identical(unchanged@instructions, appended@instructions)
  expect_identical(cleared@instructions, "")

  instruction_only <- Signature(
    inputs = list(),
    output_type = ellmer::type_object(answer = ellmer::type_string()),
    instructions = "Return an answer."
  )
  expect_error(
    with_instructions(instruction_only, ""),
    "Signature must have either inputs or instructions"
  )
})

test_that("instruction transforms reject ambiguous inputs", {
  sig <- signature("question -> answer")

  expect_snapshot(append_instructions(sig, NA_character_), error = TRUE)
  expect_snapshot(with_instructions(sig, c("one", "two")), error = TRUE)
  expect_snapshot(append_instructions(list(sig), "More detail."), error = TRUE)
})

test_that("with_reasoning transforms string signature", {
  sig <- with_reasoning("question -> answer")

  expect_s3_class(sig, "dsprrr::Signature")
  expect_true(has_reasoning(sig))

  # Check output has reasoning field first
  fields <- names(sig@output_type@properties)
  expect_equal(fields[1], "reasoning")
  expect_equal(fields[2], "answer")
})

test_that("with_reasoning transforms Signature object", {
  original <- signature("context, question -> answer: string")
  cot_sig <- with_reasoning(original)

  expect_true(has_reasoning(cot_sig))

  # Original inputs preserved
  expect_length(cot_sig@inputs, 2)
  expect_equal(cot_sig@inputs[[1]]$name, "context")
  expect_equal(cot_sig@inputs[[2]]$name, "question")
})

test_that("with_reasoning includes appropriate description", {
  sig <- with_reasoning("question -> answer")

  reasoning_type <- sig@output_type@properties$reasoning
  desc <- reasoning_type@description

  expect_true(grepl("think step by step", desc, ignore.case = TRUE))
  expect_true(grepl("answer", desc, fixed = TRUE))
})

test_that("with_reasoning allows custom prefix", {
  sig <- with_reasoning(
    "math_problem -> solution",
    prefix = "Let me solve this carefully:"
  )

  desc <- sig@output_type@properties$reasoning@description
  expect_true(grepl("solve this carefully", desc, fixed = TRUE))
})

test_that("with_reasoning allows custom reasoning field name", {
  sig <- with_reasoning(
    "question -> answer",
    reasoning_field = "thinking"
  )

  fields <- names(sig@output_type@properties)
  expect_true("thinking" %in% fields)
  expect_false("reasoning" %in% fields)

  expect_true(has_reasoning(sig, reasoning_field = "thinking"))
  expect_false(has_reasoning(sig, reasoning_field = "reasoning"))
})

test_that("with_reasoning preserves multiple output fields", {
  sig <- with_reasoning("input -> answer, confidence: number")

  fields <- names(sig@output_type@properties)
  expect_equal(fields, c("reasoning", "answer", "confidence"))
})

test_that("with_reasoning updates instructions", {
  sig <- with_reasoning("question -> answer")
  expect_true(grepl("step by step", sig@instructions, fixed = TRUE))

  # With custom instructions
  sig2 <- with_reasoning(
    "question -> answer",
    instructions = "Custom instructions here"
  )
  expect_equal(sig2@instructions, "Custom instructions here")
})

test_that("with_reasoning rejects invalid input", {
  expect_error(with_reasoning(123), "Invalid input")
  expect_error(with_reasoning(list(a = 1)), "Invalid input")
})

test_that("has_reasoning correctly identifies CoT signatures", {
  plain <- signature("q -> a")
  cot <- with_reasoning("q -> a")

  expect_false(has_reasoning(plain))
  expect_true(has_reasoning(cot))

  # Non-signature input

  expect_false(has_reasoning("not a signature"))
  expect_false(has_reasoning(NULL))
})

test_that("without_reasoning removes reasoning field", {
  cot_sig <- with_reasoning("question -> answer")
  expect_true(has_reasoning(cot_sig))

  plain_sig <- without_reasoning(cot_sig)
  expect_false(has_reasoning(plain_sig))

  fields <- names(plain_sig@output_type@properties)
  expect_equal(fields, "answer")
})

test_that("without_reasoning is idempotent on plain signatures", {
  plain <- signature("q -> a")
  result <- without_reasoning(plain)

  # Should return unchanged (same structure)
  expect_false(has_reasoning(result))
  expect_equal(
    names(plain@output_type@properties),
    names(result@output_type@properties)
  )
})

test_that("without_reasoning preserves other fields", {
  cot_sig <- with_reasoning("input -> answer, score: number")
  plain_sig <- without_reasoning(cot_sig)

  fields <- names(plain_sig@output_type@properties)
  expect_equal(fields, c("answer", "score"))
})

test_that("chain_of_thought creates module with reasoning", {
  mod <- chain_of_thought("question -> answer")

  expect_s3_class(mod, "PredictModule")
  expect_true(has_reasoning(mod$signature))
})

test_that("chain_of_thought passes arguments to module", {
  mod <- chain_of_thought(
    "question -> answer",
    template = "Custom template: {question}"
  )

  expect_equal(mod$template, "Custom template: {question}")
})

test_that("extract_output_fields handles various types", {
  # Simple string type
  simple <- ellmer::type_string()
  fields <- dsprrr:::extract_output_fields(simple)
  expect_equal(names(fields), "answer")

  # Object type
  obj <- ellmer::type_object(
    field1 = ellmer::type_string(),
    field2 = ellmer::type_number()
  )
  fields2 <- dsprrr:::extract_output_fields(obj)
  expect_equal(names(fields2), c("field1", "field2"))

  # NULL
  fields3 <- dsprrr:::extract_output_fields(NULL)
  expect_equal(names(fields3), "answer")
})

test_that("describe_output_fields formats correctly", {
  # Single field
  single <- list(answer = ellmer::type_string())
  expect_equal(dsprrr:::describe_output_fields(single), "answer")

  # Two fields
  two <- list(answer = ellmer::type_string(), score = ellmer::type_number())
  expect_equal(dsprrr:::describe_output_fields(two), "answer and score")

  # Three fields
  three <- list(
    a = ellmer::type_string(),
    b = ellmer::type_string(),
    c = ellmer::type_string()
  )
  expect_equal(dsprrr:::describe_output_fields(three), "a, b, and c")

  # Empty
  expect_equal(dsprrr:::describe_output_fields(list()), "output")
})

test_that("with_reasoning works with enum output types", {
  sig <- with_reasoning(
    "text -> sentiment: enum('positive', 'negative', 'neutral')"
  )

  expect_true(has_reasoning(sig))
  fields <- names(sig@output_type@properties)
  expect_equal(fields, c("reasoning", "sentiment"))
})

test_that("chain-of-thought roundtrip preserves structure", {
  # Create CoT signature, then remove reasoning
  original <- signature("context, question -> answer: string")
  cot <- with_reasoning(original)
  restored <- without_reasoning(cot)

  # Should have same inputs
  expect_equal(length(restored@inputs), length(original@inputs))

  # Should have same output field
  expect_equal(
    names(restored@output_type@properties),
    names(original@output_type@properties)
  )
})
