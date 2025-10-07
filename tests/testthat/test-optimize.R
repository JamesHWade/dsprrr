test_that("optimize_grid updates module configuration with best parameters", {
  skip_on_cran()

  BiasPredictModule <- R6::R6Class(
    "BiasPredictModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      initialize = function(signature, config = list()) {
        super$initialize(signature, template = "", demos = list(), config = config)
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        inputs <- if (is.data.frame(batch)) {
          as.list(batch[1, , drop = FALSE])
        } else {
          batch
        }

        bias <- self$config$bias %||% 0
        prediction <- if (bias >= 0.5) "positive" else "negative"

        tibble::tibble(
          output = list(prediction),
          chat = list(NULL),
          metadata = list(list(
            bias = bias,
            inputs = batch
          ))
        )
      }
    )
  )

  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Return sentiment"
  )

  mod <- BiasPredictModule$new(signature = sig, config = list(bias = 0))

  devset <- tibble::tibble(
    text = "sample",
    target = "positive"
  )

  mock_llm <- structure(
    list(
      chat_structured = function(...) "unused"
    ),
    class = "MockChat"
  )

  metric <- function(prediction, expected_row) {
    as.numeric(prediction == expected_row$target)
  }

  optimized <- optimize_grid(
    mod,
    devset = devset,
    metric = metric,
    parameters = list(bias = c(0, 1)),
    .llm = mock_llm,
    control = list(progress = FALSE, parallel = FALSE)
  )

  expect_identical(optimized, mod)
  expect_equal(mod$config$bias, 1)
  expect_equal(mod$state$best_score, 1)
  expect_equal(nrow(mod$state$trials), 2)
  expect_equal(mod$state$best_trial, 2)
})

test_that("optimize_grid accepts explicit grid data frames", {
  skip_on_cran()

  ConcatPredictModule <- R6::R6Class(
    "ConcatPredictModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      initialize = function(signature, config = list()) {
        super$initialize(signature, template = "", demos = list(), config = config)
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        inputs <- if (is.data.frame(batch)) {
          as.list(batch[1, , drop = FALSE])
        } else {
          batch
        }

        multiplier <- self$config$multiplier %||% 1
        suffix <- self$config$suffix %||% ""
        base <- paste0(rep("positive", multiplier), collapse = "")
        tibble::tibble(
          output = list(paste0(base, suffix)),
          chat = list(NULL),
          metadata = list(list(
            multiplier = multiplier,
            suffix = suffix,
            inputs = inputs
          ))
        )
      }
    )
  )

  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Return label"
  )

  mod <- ConcatPredictModule$new(signature = sig, config = list(multiplier = 1, suffix = ""))

  devset <- tibble::tibble(
    text = "a",
    target = "positive"
  )

  mock_llm <- structure(
    list(
      chat_structured = function(...) "unused"
    ),
    class = "MockChat"
  )

  metric <- function(prediction, expected_row) {
    as.numeric(prediction == expected_row$target)
  }

  grid <- tibble::tibble(
    multiplier = c(1, 1),
    suffix = c("", "positive")
  )

  optimize_grid(
    mod,
    devset = devset,
    metric = metric,
    grid = grid,
    .llm = mock_llm,
    control = list(progress = FALSE, parallel = FALSE)
  )

  expect_equal(mod$config$suffix, "")
  expect_equal(mod$state$best_trial, 1)
  expect_equal(mod$state$best_score, 1)
})
