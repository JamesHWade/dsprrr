test_that("PredictModule can be created with valid signature", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify the text"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}"
  )

  expect_true(inherits(pred, "PredictModule"))
  expect_true(inherits(pred, "Module"))
  expect_true(inherits(pred, "R6"))
  expect_s3_class(pred$signature, "dsprrr::Signature")
  expect_equal(pred$template, "Text: {text}")
})

test_that("module() validates signature must be Signature object", {
  expect_error(
    module(
      signature = "not a signature",
      type = "predict",
      template = "Template"
    ),
    "First argument must be a Signature object"
  )
})

test_that("PredictModule validates template must be character", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  expect_error(
    PredictModule$new(
      signature = sig,
      template = 123
    ),
    "template must be a single character string"
  )
})

test_that("PredictModule accepts demos list", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify sentiment"
  )

  demos <- list(
    list(
      inputs = list(text = "This is great!"),
      output = "positive"
    ),
    list(
      inputs = list(text = "This is terrible!"),
      output = "negative"
    )
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}",
    demos = demos
  )

  expect_equal(length(pred$demos), 2)
  expect_equal(pred$demos[[1]]$inputs$text, "This is great!")
  expect_equal(pred$demos[[1]]$output, "positive")
})

test_that("PredictModule accepts config list", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  config <- list(
    temperature = 0.5,
    max_tokens = 100,
    model = "gpt-5-mini"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    config = config
  )

  expect_equal(pred$config$temperature, 0.5)
  expect_equal(pred$config$max_tokens, 100)
  expect_equal(pred$config$model, "gpt-5-mini")
})

test_that("PredictModule print method works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify the text"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}"
  )

  output <- capture.output(print(pred), type = "message")
  # Check for headers - they appear in message output
  expect_true(any(grepl("PredictModule", output, fixed = TRUE)))
  expect_true(any(grepl("Signature", output, fixed = TRUE)))
  expect_true(any(grepl("Template", output, fixed = TRUE)))
})

test_that("PredictModule with empty template works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify the text"
  )

  pred <- module(
    signature = sig,
    type = "predict"
  )

  expect_equal(pred$template, "")
})

test_that("PredictModule reset_copy works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}",
    demos = list(list(inputs = list(text = "test"), output = "result")),
    config = list(temperature = 0.7)
  )

  reset_pred <- pred$reset_copy()

  expect_true(inherits(reset_pred, "PredictModule"))
  expect_equal(reset_pred$template, pred$template)
  expect_equal(length(reset_pred$demos), 0)
  expect_equal(length(reset_pred$config), 0)
})

test_that("PredictModule deepcopy works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}",
    demos = list(list(inputs = list(text = "test"), output = "result")),
    config = list(temperature = 0.7)
  )

  copied_pred <- pred$deepcopy()

  expect_true(inherits(copied_pred, "PredictModule"))
  expect_equal(copied_pred$template, pred$template)
  expect_equal(length(copied_pred$demos), 1)
  expect_equal(copied_pred$config$temperature, 0.7)

  # Verify it's a true copy, not a reference
  copied_pred$config$temperature <- 0.9
  expect_equal(pred$config$temperature, 0.7)
})

test_that("PredictModule is_compiled works", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  pred <- module(
    signature = sig,
    type = "predict"
  )

  expect_false(pred$is_compiled())

  pred$state$compiled <- TRUE
  expect_true(pred$is_compiled())
})

test_that("module_demos_as_tibble converts simple demos to tibble", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    demos = list(
      list(inputs = list(text = "hello"), output = "positive"),
      list(inputs = list(text = "goodbye"), output = "negative")
    )
  )

  result <- module_demos_as_tibble(pred)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$text, c("hello", "goodbye"))
  expect_equal(result$output, c("positive", "negative"))
})

test_that("module_demos_as_tibble handles nested outputs", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    demos = list(
      list(
        inputs = list(text = "hello"),
        output = list(classification = "positive", confidence = 0.9)
      ),
      list(
        inputs = list(text = "goodbye"),
        output = list(classification = "negative", confidence = 0.8)
      )
    )
  )

  result <- module_demos_as_tibble(pred)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(result$text, c("hello", "goodbye"))
  expect_equal(result$classification, c("positive", "negative"))
  expect_equal(result$confidence, c(0.9, 0.8))
  # Should NOT have an "output" column - fields are flattened
  expect_false("output" %in% names(result))
})

test_that("module_demos_as_tibble returns empty tibble for no demos", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- module(signature = sig, type = "predict")

  result <- module_demos_as_tibble(pred)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("demo_table active binding works", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    demos = list(
      list(inputs = list(text = "test"), output = "result")
    )
  )

  # Active binding should return same as function
  expect_equal(pred$demo_table, module_demos_as_tibble(pred))
  expect_s3_class(pred$demo_table, "tbl_df")
  expect_equal(nrow(pred$demo_table), 1)
})
