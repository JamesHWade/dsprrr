# Integration tests with real LLMs (using vcr for recording)

test_that("basic LLM integration works", {
  skip_if_not_installed("vcr")
  cassette_file <- vcr::vcr_test_path("_vcr", "integration-basic.yml")
  skip_if_not(file.exists(cassette_file), "VCR cassette not recorded")

  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string")
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
  llm <- ellmer::chat_openai(model = "gpt-4o-mini")
  result <- run(pred, text = "Hello world", .llm = llm)
  expect_type(result, "character")
  expect_true(grepl("Hello", result, ignore.case = TRUE))
})

test_that("structured output works with real LLM", {
  skip_if_not_installed("vcr")
  cassette_file <- vcr::vcr_test_path("_vcr", "integration-structured.yml")
  skip_if_not(file.exists(cassette_file), "VCR cassette not recorded")

  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string")
    ),
    output_type = ellmer::type_object(
      sentiment = ellmer::type_enum(
        values = c("positive", "negative", "neutral")
      ),
      confidence = ellmer::type_number(
        description = "Confidence score between 0 and 1"
      )
    ),
    instructions = "Analyze sentiment"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}"
  )

  vcr::local_cassette("integration-structured")
  llm <- ellmer::chat_openai(model = "gpt-4o-mini")
  result <- run(pred, text = "I love this package!", .llm = llm)

  expect_type(result, "list")
  expect_named(result, c("sentiment", "confidence"))
  expect_true(result$sentiment %in% c("positive", "negative", "neutral"))
  expect_true(result$confidence >= 0 && result$confidence <= 1)
})

test_that("batch processing works with real LLM", {
  skip_if_not_installed("vcr")
  cassette_file <- vcr::vcr_test_path("_vcr", "integration-batch.yml")
  skip_if_not(file.exists(cassette_file), "VCR cassette not recorded")

  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string")
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
  llm <- ellmer::chat_openai(model = "gpt-4o-mini")
  results <- run(
    pred,
    text = c("Great!", "Terrible!"),
    .llm = llm,
    .progress = FALSE
  )

  expect_length(results, 2)
  expect_type(results, "list")
  # Results should be character or NA (if API call failed)
  expect_true(all(sapply(results, function(x) is.character(x) || is.na(x))))
})

test_that("optimize_grid integrates with real LLM", {
  skip_if_not_installed("vcr")
  cassette_name <- "integration-optimize-grid"
  cassette_path <- vcr::vcr_test_path("_vcr", paste0(cassette_name, ".yml"))
  skip_if_not(file.exists(cassette_path), "VCR cassette not recorded")

  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string")
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
  llm <- ellmer::chat_openai(model = "gpt-4o-mini")
  optimize_grid(
    mod,
    data = devset,
    metric = metric,
    parameters = list(prompt_style = c("baseline", "energetic")),
    .llm = llm,
    control = list(progress = FALSE, parallel = FALSE)
  )

  expect_true(mod$is_compiled())
  expect_equal(nrow(mod$state$trials), 2)
  expect_false(all(is.na(mod$state$trials$score)))
  expect_true(
    is.numeric(mod$state$best_score) || is.logical(mod$state$best_score)
  )

  skip_if_not_installed("dials")
  skip_if_not_installed("yardstick")
  param_set <- module_parameters(mod)
  expect_true(all(c("prompt_style", "temperature") %in% param_set$id))

  summary <- module_trials(mod)
  expect_equal(summary$n_trials, 2)
  expect_equal(summary$best_trial, mod$state$best_trial)
  expect_equal(summary$best_params[[1]]$prompt_style, mod$config$prompt_style)

  metrics <- module_metrics(mod)
  expect_equal(nrow(metrics), 2)
  expect_equal(
    metrics$params[[mod$state$best_trial]]$prompt_style,
    mod$config$prompt_style
  )

  yard_metrics <- module_metrics(
    mod,
    metrics = list(yardstick::accuracy),
    truth = "target",
    estimate = "result"
  )
  expect_true(
    is.null(yard_metrics$yardstick[[mod$state$best_trial]]) ||
      inherits(yard_metrics$yardstick[[mod$state$best_trial]], "tbl_df")
  )
})

# --- finetune integration tests ---

test_that("finetune::tune_race_anova() workflow is compatible", {
  skip_on_cran()
  skip_if_not_installed("finetune")
  skip_if_not_installed("rsample")
  skip_if_not_installed("dials")
  skip_if_not_installed("yardstick")

  # Create a deterministic mock module for testing the finetune workflow
  # This validates that our parameter set and trial structures are compatible
  MockTunableModule <- R6::R6Class(
    "MockTunableModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      initialize = function(signature, config = list()) {
        super$initialize(
          signature,
          template = "",
          demos = list(),
          config = config
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        inputs <- if (is.data.frame(batch)) {
          as.list(batch[1, , drop = FALSE])
        } else {
          batch
        }

        # Deterministic prediction based on temperature config
        # Higher temperature = more likely to predict "positive"
        temp <- self$config$temperature %||% 0.5
        prediction <- if (temp > 0.5) "positive" else "negative"

        tibble::tibble(
          output = list(prediction),
          chat = list(NULL),
          metadata = list(list(
            temperature = temp,
            inputs = inputs
          ))
        )
      }
    )
  )

  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string")
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify sentiment"
  )

  mod <- MockTunableModule$new(
    signature = sig,
    config = list(temperature = 0.5)
  )

  # Create a development set with known labels
  devset <- tibble::tibble(
    text = c(
      "I love this!",
      "This is great!",
      "Wonderful experience",
      "Terrible product",
      "Worst ever",
      "Disappointing"
    ),
    target = c(
      "positive",
      "positive",
      "positive",
      "negative",
      "negative",
      "negative"
    )
  )

  metric <- function(prediction, expected_row) {
    as.numeric(prediction == expected_row$target)
  }

  mock_llm <- local({
    self <- structure(
      list(
        chat_structured = function(...) "unused",
        clone = function(...) self
      ),
      class = "MockChat"
    )
    self
  })

  # Run optimization with temperature grid
  optimize_grid(
    mod,
    data = devset,
    metric = metric,
    parameters = list(temperature = c(0.1, 0.3, 0.5, 0.7, 0.9)),
    .llm = mock_llm,
    control = list(progress = FALSE, parallel = FALSE)
  )

  # Verify optimization completed

  expect_equal(nrow(mod$state$trials), 5)

  # Extract parameter set - this is what finetune would use
  param_set <- module_parameters(mod, include = "temperature")
  expect_s3_class(param_set, "parameters")
  expect_true("temperature" %in% param_set$id)

  # Verify the parameter range was derived from trials
  temp_param <- param_set$object[[which(param_set$id == "temperature")]]
  expect_true(inherits(temp_param, "quant_param"))

  # Test that module_metric_summary produces yardstick-compatible output
  metric_summary <- module_metrics(mod)
  expect_equal(nrow(metric_summary), 5)
  expect_true(all(
    c("trial_id", "score", "mean_score", "params") %in% names(metric_summary)
  ))

  # Verify trial structure is compatible with tune_race_anova expectations
  # The trials tibble should have the structure finetune expects
  trials <- mod$state$trials
  expect_true("trial_id" %in% names(trials))
  expect_true("score" %in% names(trials))
  expect_true("parameters" %in% names(trials))

  # Verify we can create rsample resamples from the devset
  # This is a prerequisite for tune_race_anova
  resamples <- rsample::vfold_cv(devset, v = 2)
  expect_s3_class(resamples, "vfold_cv")

  # Verify that dials grid generation works with our parameter set
  grid <- dials::grid_regular(param_set, levels = 3)
  expect_equal(nrow(grid), 3)
  expect_true("temperature" %in% names(grid))
})

test_that("module_parameter_set works with finetune grid functions", {
  skip_on_cran()
  skip_if_not_installed("finetune")
  skip_if_not_installed("dials")

  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string")
    ),
    output_type = ellmer::type_string(),
    instructions = ""
  )

  mod <- module(signature = sig, type = "predict")
  mod$config$temperature <- 0.5
  mod$config$top_p <- 0.9

  # Create trials structure manually to test parameter extraction
  mod$state$trials <- tibble::tibble(
    trial_id = 1:3,
    score = c(0.6, 0.8, 0.7),
    parameters = list(
      list(temperature = 0.3, top_p = 0.8),
      list(temperature = 0.5, top_p = 0.9),
      list(temperature = 0.7, top_p = 1.0)
    ),
    n_evaluated = rep(10L, 3),
    n_errors = rep(0L, 3),
    evaluation = list(
      list(mean_score = 0.6, scores = rep(0.6, 10)),
      list(mean_score = 0.8, scores = rep(0.8, 10)),
      list(mean_score = 0.7, scores = rep(0.7, 10))
    )
  )
  mod$state$best_trial <- 2
  mod$state$best_score <- 0.8
  mod$config$compiled <- TRUE

  # Extract parameter set
  param_set <- module_parameters(mod, include = c("temperature", "top_p"))

  # Verify it works with finetune's grid functions
  regular_grid <- dials::grid_regular(param_set, levels = 3)
  expect_equal(nrow(regular_grid), 9) # 3 * 3

  random_grid <- dials::grid_random(param_set, size = 10)
  expect_equal(nrow(random_grid), 10)

  # Verify space-filling grids work (used by tune_race_anova)
  lhs_grid <- dials::grid_space_filling(param_set, size = 5)
  expect_equal(nrow(lhs_grid), 5)
  expect_true(all(c("temperature", "top_p") %in% names(lhs_grid)))
})
