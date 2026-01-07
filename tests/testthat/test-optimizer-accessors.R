# Tests for optimizer accessor functions

# Create a mock module for testing
create_test_module <- function(compiled = FALSE, with_demos = FALSE) {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test instructions"
  )

  mod <- module(signature = sig, type = "predict")

  if (compiled) {
    # Set up a compiled state manually
    mod$config$temperature <- 0.5
    mod$config$prompt_style <- "concise"
    mod$state$compiled <- TRUE
    mod$state$best_score <- 0.85
    mod$state$best_trial <- 2L
    mod$state$best_params <- list(temperature = 0.5, prompt_style = "concise")
    mod$state$trials <- tibble::tibble(
      trial_id = 1:3,
      parameters = list(
        list(temperature = 0.3),
        list(temperature = 0.5),
        list(temperature = 0.7)
      ),
      score = c(0.6, 0.85, 0.75),
      n_evaluated = c(10L, 10L, 10L),
      n_errors = c(0L, 0L, 0L),
      evaluation = list(list(), list(), list()),
      timestamp = rep(Sys.time(), 3)
    )
  }

  if (with_demos) {
    # PredictModule uses $demos field, not config$demos
    mod$demos <- list(
      list(text = "Great product!", sentiment = "positive"),
      list(text = "Terrible service", sentiment = "negative")
    )
  }

  mod
}


# ---- best_params() tests ----

test_that("best_params returns NULL for non-compiled module", {
  mod <- create_test_module(compiled = FALSE)
  expect_warning(
    result <- best_params(mod),
    "has not been optimized"
  )
  expect_null(result)
})

test_that("best_params returns best parameters for compiled module", {
  mod <- create_test_module(compiled = TRUE)
  params <- best_params(mod)

  expect_type(params, "list")
  expect_equal(params$temperature, 0.5)
  expect_equal(params$prompt_style, "concise")
})

test_that("best_params flattens parameters correctly", {
  mod <- create_test_module(compiled = TRUE)
  # Simulate nested list structure
  mod$state$best_params <- list(temperature = list(0.5), style = "verbose")

  params <- best_params(mod, flatten = TRUE)
  expect_equal(params$temperature, 0.5)

  params_no_flatten <- best_params(mod, flatten = FALSE)
  expect_type(params_no_flatten$temperature, "list")
})

test_that("best_params errors on non-module input", {
  expect_error(best_params("not a module"), "must be a DSPrrr Module object")
})


# ---- best_demos() tests ----

test_that("best_demos returns NULL when no demos", {
  mod <- create_test_module(compiled = TRUE, with_demos = FALSE)
  expect_null(best_demos(mod))
})

test_that("best_demos returns demos as list", {
  mod <- create_test_module(compiled = TRUE, with_demos = TRUE)
  demos <- best_demos(mod)

  expect_type(demos, "list")
  expect_length(demos, 2)
  expect_equal(demos[[1]]$text, "Great product!")
})

test_that("best_demos returns demos as tibble when requested", {
  mod <- create_test_module(compiled = TRUE, with_demos = TRUE)
  demos <- best_demos(mod, as_tibble = TRUE)

  expect_s3_class(demos, "tbl_df")
  expect_true("text" %in% names(demos))
  expect_true("sentiment" %in% names(demos))
})


# ---- apply_best_config() tests ----

test_that("apply_best_config copies params to target module", {
  source <- create_test_module(compiled = TRUE, with_demos = TRUE)
  target <- create_test_module(compiled = FALSE)

  apply_best_config(source, target, include = "params")

  expect_equal(target$config$temperature, 0.5)
  expect_true(target$is_compiled())
})

test_that("apply_best_config copies demos when requested", {
  source <- create_test_module(compiled = TRUE, with_demos = TRUE)
  target <- create_test_module(compiled = FALSE)

  apply_best_config(source, target, include = "demos")

  # PredictModule uses $demos field
  expect_length(target$demos, 2)
  expect_equal(target$demos[[1]]$text, "Great product!")
})

test_that("apply_best_config creates fresh copy when target is NULL", {
  source <- create_test_module(compiled = TRUE)
  result <- apply_best_config(source, target = NULL)

  expect_s3_class(result, "Module")
  expect_false(identical(result, source))
  expect_equal(result$config$temperature, 0.5)
})


# ---- top_trials() tests ----

test_that("top_trials returns top k trials for maximize", {
  mod <- create_test_module(compiled = TRUE)
  top <- top_trials(mod, k = 2, objective = "maximize")

  expect_equal(nrow(top), 2)
  expect_equal(top$trial_id[1], 2) # Highest score first
  expect_equal(top$score[1], 0.85)
})

test_that("top_trials returns top k trials for minimize", {
  mod <- create_test_module(compiled = TRUE)
  top <- top_trials(mod, k = 2, objective = "minimize")

  expect_equal(nrow(top), 2)
  expect_equal(top$trial_id[1], 1) # Lowest score first
  expect_equal(top$score[1], 0.6)
})

test_that("top_trials warns when no trials exist", {
  mod <- create_test_module(compiled = FALSE)
  expect_warning(
    result <- top_trials(mod),
    "no optimization trials"
  )
  expect_equal(nrow(result), 0)
})

test_that("top_trials.TrialLog works correctly", {
  log <- TrialLog$new(optimizer_name = "test")

  trial1 <- Trial(
    trial_id = "t1",
    optimizer_name = "test",
    params = list(k = 1),
    metric_summary = list(mean_score = 0.7),
    status = "completed"
  )
  trial2 <- Trial(
    trial_id = "t2",
    optimizer_name = "test",
    params = list(k = 2),
    metric_summary = list(mean_score = 0.9),
    status = "completed"
  )

  log$add_trial(trial1, persist = FALSE)
  log$add_trial(trial2, persist = FALSE)

  top <- top_trials(log, k = 1, objective = "maximize")
  expect_equal(nrow(top), 1)
  expect_equal(top$trial_id, "t2")
})


# ---- config_diff() tests ----

test_that("config_diff shows changed parameters", {
  mod <- create_test_module(compiled = TRUE)
  diff <- config_diff(mod)

  expect_s3_class(diff, "tbl_df")
  expect_true("parameter" %in% names(diff))
  expect_true("before" %in% names(diff))
  expect_true("after" %in% names(diff))
  expect_true("changed" %in% names(diff))

  # Temperature should be marked as changed (default 1.0 -> 0.5)
  temp_row <- diff[diff$parameter == "temperature", ]
  expect_true(temp_row$changed)
})

test_that("config_diff uses custom baseline", {
  mod <- create_test_module(compiled = TRUE)
  diff <- config_diff(mod, baseline = list(temperature = 0.5))

  temp_row <- diff[diff$parameter == "temperature", ]
  expect_false(temp_row$changed) # Same as baseline
})


# ---- export_module_code() tests ----

test_that("export_module_code generates valid R code", {
  mod <- create_test_module(compiled = TRUE, with_demos = TRUE)
  code <- export_module_code(mod, name = "test_mod")

  expect_type(code, "character")
  expect_true(grepl("test_mod <- module", code, fixed = TRUE))
  expect_true(grepl("temperature <- 0.5", code, fixed = TRUE))
  expect_true(grepl("compiled <- TRUE", code, fixed = TRUE))
})

test_that("export_module_code includes demos when requested", {
  mod <- create_test_module(compiled = TRUE, with_demos = TRUE)
  code <- export_module_code(mod, include_demos = TRUE)

  expect_true(grepl("demos <- list", code, fixed = TRUE))
  expect_true(grepl("Great product!", code, fixed = TRUE))
})

test_that("export_module_code excludes demos when not requested", {
  mod <- create_test_module(compiled = TRUE, with_demos = TRUE)
  code <- export_module_code(mod, include_demos = FALSE)

  expect_false(grepl("Great product!", code, fixed = TRUE))
})

test_that("export_module_code writes to file", {
  mod <- create_test_module(compiled = TRUE)
  temp_file <- tempfile(fileext = ".R")
  on.exit(unlink(temp_file))

  expect_message(
    export_module_code(mod, file = temp_file),
    "written to"
  )

  expect_true(file.exists(temp_file))
  content <- readLines(temp_file)
  expect_true(any(grepl("module", content)))
})


# ---- optimization_summary() tests ----

test_that("optimization_summary returns correct structure", {
  mod <- create_test_module(compiled = TRUE)
  summary <- optimization_summary(mod)

  expect_s3_class(summary, "dsprrr_optimization_summary")
  expect_equal(summary$n_trials, 3)
  expect_equal(summary$best_score, 0.85)
  expect_equal(summary$best_trial, 2)
  expect_true(summary$compiled)
})

test_that("optimization_summary handles empty trials", {
  mod <- create_test_module(compiled = FALSE)
  summary <- optimization_summary(mod)

  expect_equal(summary$n_trials, 0)
  expect_true(is.na(summary$best_score))
  expect_false(summary$compiled)
})

test_that("optimization_summary calculates improvement", {
  mod <- create_test_module(compiled = TRUE)
  summary <- optimization_summary(mod)

  # First score was 0.6, best is 0.85
  expect_equal(summary$improvement, 0.25)
})

test_that("optimization_summary print works", {
  mod <- create_test_module(compiled = TRUE)
  summary <- optimization_summary(mod)

  # cli outputs to stderr, use expect_message or capture both streams
  expect_no_error(print(summary))
  expect_s3_class(summary, "dsprrr_optimization_summary")
  expect_invisible(print(summary))
})
