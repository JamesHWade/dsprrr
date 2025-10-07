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

  skip_if_not_installed("dials")
  param_set <- module_parameter_set(mod)
  expect_s3_class(param_set, "parameters")
  expect_true(all(c("bias", "temperature") %in% param_set$id))

  trial_summary <- module_trials_summary(mod)
  expect_equal(trial_summary$n_trials, 2)
  expect_equal(trial_summary$best_trial, 2)
  expect_equal(trial_summary$best_params[[1]]$bias, 1)
  expect_true(is.data.frame(trial_summary$trials[[1]]))

  metrics <- module_metric_summary(mod)
  expect_equal(nrow(metrics), 2)
  expect_equal(metrics$trial_id, c(1, 2))
  expect_equal(metrics$params[[2]]$bias, 1)
  expect_true(all(vapply(metrics$metrics, is.data.frame, logical(1))))
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

  skip_if_not_installed("dials")
  param_set <- module_parameter_set(mod)
  expect_true(all(c("multiplier", "suffix", "temperature") %in% param_set$id))

  trial_summary <- module_trials_summary(mod)
  expect_equal(trial_summary$n_trials, 2)
  expect_equal(trial_summary$best_trial, 1)
  expect_equal(trial_summary$best_params[[1]]$suffix, "")

  metrics <- module_metric_summary(mod)
  expect_equal(nrow(metrics), 2)
  expect_equal(metrics$trial_id[[1]], 1)
  expect_equal(metrics$params[[1]]$suffix, "")
  expect_true(all(vapply(metrics$metrics, is.data.frame, logical(1))))
})

test_that("module_register_parameters seeds tidymodels parameter sets", {
  skip_on_cran()
  skip_if_not_installed("dials")

  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Return label"
  )

  mod <- module(signature = sig, type = "predict")

  module_register_parameters(mod, list(
    temperature = list(type = "double", range = c(0, 1), label = "Temperature"),
    prompt_style = list(type = "categorical", values = c("baseline", "energetic"))
  ))

  params <- module_parameter_set(mod)
  expect_true(all(c("temperature", "prompt_style") %in% params$id))
})

test_that("optimize_grid supports yardstick metrics with resamples", {
  skip_on_cran()
  skip_if_not_installed("yardstick")
  skip_if_not_installed("rsample")

  BiasPredictModule <- R6::R6Class(
    "BiasPredictModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      initialize = function(signature, config = list()) {
        super$initialize(signature, template = "", demos = list(), config = config)
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        bias <- self$config$bias %||% 0
        prediction <- if (bias >= 0.5) "positive" else "negative"

        tibble::tibble(
          output = list(prediction),
          chat = list(NULL),
          metadata = list(list(bias = bias))
        )
      }
    )
  )

  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Return sentiment"
  )

  mod <- BiasPredictModule$new(signature = sig, config = list(bias = 0))

  devset <- tibble::tibble(
    text = c("a", "b", "c", "d"),
    target = factor(c("positive", "positive", "negative", "positive"), levels = c("negative", "positive"))
  )

  resamples <- rsample::vfold_cv(devset, v = 2)

  metric <- as_dsprrr_metric(
    yardstick::metric_set(yardstick::accuracy),
    truth = "target",
    estimate = "prediction",
    transform = function(pred) factor(pred, levels = c("negative", "positive")),
    type = "yardstick"
  )

  mock_llm <- structure(list(chat_structured = function(...) "unused"), class = "MockChat")

  optimize_grid(
    mod,
    devset = devset,
    metric = metric,
    resamples = resamples,
    parameters = list(bias = c(0, 1)),
    .llm = mock_llm,
    control = list(progress = FALSE)
  )

  expect_equal(mod$config$bias, 1)
  expect_s3_class(mod$state$last_resamples, "rset")

  trials <- mod$state$trials
  best_eval <- trials$evaluation[[mod$state$best_trial]]
  expect_true("resamples" %in% names(best_eval))
  expect_equal(nrow(best_eval$resamples), nrow(resamples))
  expect_true(all(vapply(best_eval$metrics$metrics, is.data.frame, logical(1))))

  summary <- module_metric_summary(mod)
  expect_true(all(vapply(summary$metrics, is.data.frame, logical(1))))
})
