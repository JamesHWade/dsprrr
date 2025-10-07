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

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Please repeat: {text}"
  )

  vcr::local_cassette("integration-basic")
  llm <- ellmer::chat_openai(model = "gpt-5-mini")
  result <- run(pred, text = "Hello world", .llm = llm)
  expect_type(result, "character")
  expect_true(grepl("Hello", result, ignore.case = TRUE))
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

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}"
  )

  vcr::local_cassette("integration-structured")
  llm <- ellmer::chat_openai(model = "gpt-5-mini")
  result <- run(pred, text = "I love this package!", .llm = llm)

  expect_type(result, "list")
  expect_named(result, c("sentiment", "confidence"))
  expect_true(result$sentiment %in% c("positive", "negative", "neutral"))
  expect_true(result$confidence >= 0 && result$confidence <= 1)
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

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}\nSentiment:"
  )

  vcr::local_cassette("integration-batch")
  llm <- ellmer::chat_openai(model = "gpt-5-mini")
  results <- run(pred,
                text = c("Great!", "Terrible!"),
                .llm = llm,
                .progress = FALSE)

  expect_length(results, 2)
  expect_type(results, "list")
  # Results should be character or NA (if API call failed)
  expect_true(all(sapply(results, function(x) is.character(x) || is.na(x))))
})

test_that("optimize_grid integrates with real LLM", {
  skip_if_not_installed("vcr")
  cassette_name <- "integration-optimize-grid"
  cassette_path <- file.path("_vcr", paste0(cassette_name, ".yml"))
  skip_if_not(use_real_api() || file.exists(cassette_path))

  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Return the sentiment label as a single word."
  )

  mod <- module(
    signature = sig,
    type = "predict",
    template = "Sentence: {text}\nLabel:"
  )

  devset <- tibble::tibble(
    text = "Absolutely wonderful experience.",
    target = "positive"
  )

  metric <- function(prediction, expected_row) {
    as.numeric(tolower(prediction) == tolower(expected_row$target))
  }

  vcr::local_cassette(cassette_name)
  optimize_grid(
    mod,
    devset = devset,
    metric = metric,
    parameters = list(prompt_style = c("baseline", "energetic")),
    control = list(progress = FALSE, parallel = FALSE)
  )

  expect_true(mod$is_compiled())
  expect_equal(nrow(mod$state$trials), 2)
  expect_false(all(is.na(mod$state$trials$score)))
  expect_true(is.numeric(mod$state$best_score) || is.logical(mod$state$best_score))

  skip_if_not_installed("dials")
  param_set <- module_parameter_set(mod)
  expect_true("prompt_style" %in% param_set$id)

  summary <- module_trials_summary(mod)
  expect_equal(summary$n_trials, 2)
  expect_equal(summary$best_trial, mod$state$best_trial)
  expect_equal(summary$best_params[[1]]$prompt_style, mod$config$prompt_style)
})
