# Integration tests with real LLMs (using vcr for recording)

test_that("basic LLM integration works", {
  skip_if_not_installed("vcr")
  skip_if_not(use_real_api() || file.exists(file.path("_vcr", "integration-basic.yml")))

  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Echo the text back"
  )

  pred <- Predict(
    signature = sig,
    template = "Please repeat: {text}"
  )

  vcr::use_cassette("integration-basic", {
    llm <- ellmer::chat_openai(model = "gpt-4o-mini")
    result <- run(pred, text = "Hello world", .llm = llm)
    expect_type(result, "character")
    expect_true(grepl("Hello", result, ignore.case = TRUE))
  })
})

test_that("structured output works with real LLM", {
  skip_if_not_installed("vcr")
  skip_if_not(use_real_api() || file.exists(file.path("_vcr", "integration-structured.yml")))

  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_object(
      sentiment = ellmer::type_enum(values = c("positive", "negative", "neutral")),
      confidence = ellmer::type_number(description = "Confidence score between 0 and 1")
    ),
    instructions = "Analyze sentiment"
  )

  pred <- Predict(
    signature = sig,
    template = "Text: {text}"
  )

  vcr::use_cassette("integration-structured", {
    llm <- ellmer::chat_openai(model = "gpt-4o-mini")
    result <- run(pred, text = "I love this package!", .llm = llm)

    expect_type(result, "list")
    expect_named(result, c("sentiment", "confidence"))
    expect_true(result$sentiment %in% c("positive", "negative", "neutral"))
    expect_true(result$confidence >= 0 && result$confidence <= 1)
  })
})

test_that("batch processing works with real LLM", {
  skip_if_not_installed("vcr")
  skip_if_not(use_real_api() || file.exists(file.path("_vcr", "integration-batch.yml")))

  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify as positive or negative"
  )

  pred <- Predict(
    signature = sig,
    template = "Text: {text}\nSentiment:"
  )

  vcr::use_cassette("integration-batch", {
    llm <- ellmer::chat_openai(model = "gpt-4o-mini")
    results <- run(pred,
                  text = c("Great!", "Terrible!"),
                  .llm = llm,
                  .progress = FALSE)

    expect_length(results, 2)
    expect_type(results, "list")
    # Results should be character or NA (if API call failed)
    expect_true(all(sapply(results, function(x) is.character(x) || is.na(x))))
  })
})
