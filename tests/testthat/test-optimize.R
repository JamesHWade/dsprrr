test_that("optimize_grid updates module configuration with best parameters", {
  skip_on_cran()

  BiasPredictModule <- R6::R6Class(
    "BiasPredictModule",
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
      input(name = "text", type = "string")
    ),
    output_type = ellmer::type_string(),
    instructions = "Return sentiment"
  )

  mod <- BiasPredictModule$new(signature = sig, config = list(bias = 0))

  devset <- tibble::tibble(
    text = "sample",
    target = "positive"
  )

  mock_llm <- new_test_chat(
    chat_structured = function(...) "unused"
  )

  metric <- function(prediction, expected_row) {
    as.numeric(prediction == expected_row$target)
  }

  optimized <- optimize_grid(
    mod,
    data = devset,
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
  skip_if_not_installed("yardstick")
  param_set <- module_parameters(mod)
  expect_s3_class(param_set, "parameters")
  expect_true(all(c("bias", "temperature") %in% param_set$id))

  trial_summary <- module_trials(mod)
  expect_equal(trial_summary$n_trials, 2)
  expect_equal(trial_summary$best_trial, 2)
  expect_equal(trial_summary$best_params[[1]]$bias, 1)
  expect_true(is.data.frame(trial_summary$trials[[1]]))

  metrics <- module_metrics(mod)
  expect_equal(nrow(metrics), 2)
  expect_equal(metrics$trial_id, c(1, 2))
  expect_equal(metrics$params[[2]]$bias, 1)

  yard_metrics <- module_metrics(
    mod,
    metrics = list(yardstick::accuracy),
    truth = "target",
    estimate = "result"
  )
  expect_true(
    is.null(yard_metrics$yardstick[[2]]) ||
      inherits(yard_metrics$yardstick[[2]], "tbl_df")
  )
})

test_that("optimize_grid accepts explicit grid data frames", {
  skip_on_cran()

  ConcatPredictModule <- R6::R6Class(
    "ConcatPredictModule",
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
      input(name = "text", type = "string")
    ),
    output_type = ellmer::type_string(),
    instructions = "Return label"
  )

  mod <- ConcatPredictModule$new(
    signature = sig,
    config = list(multiplier = 1, suffix = "")
  )

  devset <- tibble::tibble(
    text = "a",
    target = "positive"
  )

  mock_llm <- new_test_chat(
    chat_structured = function(...) "unused"
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
    data = devset,
    metric = metric,
    grid = grid,
    .llm = mock_llm,
    control = list(progress = FALSE, parallel = FALSE)
  )

  expect_equal(mod$config$suffix, "")
  expect_equal(mod$state$best_trial, 1)
  expect_equal(mod$state$best_score, 1)

  skip_if_not_installed("dials")
  skip_if_not_installed("yardstick")
  param_set <- module_parameters(mod)
  expect_true(all(c("multiplier", "suffix", "temperature") %in% param_set$id))

  trial_summary <- module_trials(mod)
  expect_equal(trial_summary$n_trials, 2)
  expect_equal(trial_summary$best_trial, 1)
  expect_equal(trial_summary$best_params[[1]]$suffix, "")

  metrics <- module_metrics(mod)
  expect_equal(nrow(metrics), 2)
  expect_equal(metrics$trial_id[[1]], 1)
  expect_equal(metrics$params[[1]]$suffix, "")

  yard_metrics <- module_metrics(
    mod,
    metrics = list(yardstick::accuracy),
    truth = "target",
    estimate = "result"
  )
  expect_true(
    is.null(yard_metrics$yardstick[[1]]) ||
      inherits(yard_metrics$yardstick[[1]], "tbl_df")
  )
})

test_that("module_parameter_set derives defaults from signature", {
  skip_on_cran()
  skip_if_not_installed("dials")

  sig <- Signature(
    inputs = list(
      input(
        name = "mode",
        type = ellmer::type_enum(values = c("prod", "debug"))
      )
    ),
    output_type = ellmer::type_string(),
    instructions = ""
  )

  mod <- module(signature = sig)
  params <- module_parameters(mod)
  expect_true("input_mode" %in% params$id)
})

test_that("module_metric_summary handles modules without trials", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = ""
  )
  mod <- module(signature = sig)

  metrics <- module_metrics(mod)
  expect_equal(nrow(metrics), 0)
})

test_that("module_trials() warns and returns an inspectable summary when all trials fail (dsprrr-hew)", {
  # Regression: which.max() on all-NA scores returns integer(0), so indexing
  # threw "attempt to select less than one element" instead of a clear message.
  mod <- module(signature("question -> answer"))
  mod$state$trials <- tibble::tibble(
    trial_id = 1:3,
    score = NA_real_,
    parameters = list(list(a = 1), list(a = 2), list(a = 3))
  )

  expect_warning(
    summary <- module_trials(mod),
    "trial.*failed"
  )
  expect_equal(summary$n_trials, 3L)
  expect_true(is.na(summary$best_score))
  expect_true(is.na(summary$best_trial))
})
