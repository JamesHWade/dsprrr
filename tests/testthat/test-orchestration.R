# Tests for orchestration helpers

# ---- Helper Functions ----

# Create a minimal test module
create_test_module <- function() {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test instructions"
  )
  module(signature = sig, type = "predict", template = "Test: {text}")
}

# ---- pin_module_config tests ----

test_that("pin_module_config requires pins package", {
  skip_if_not_installed("pins")

  mod <- create_test_module()
  board <- pins::board_temp()

  result <- pin_module_config(board, "test-module", mod)

  expect_equal(result, "test-module")
})

test_that("pin_module_config validates module argument", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  expect_error(
    pin_module_config(board, "test", "not a module"),
    "must be a DSPrrr Module object"
  )
})

test_that("pin_module_config saves correct structure", {
  skip_if_not_installed("pins")

  mod <- create_test_module()
  board <- pins::board_temp()

  pin_module_config(board, "test-module", mod)
  config <- pins::pin_read(board, "test-module")

  expect_identical(config$format, "dsprrr-program")
  expect_identical(config$format_version, 4L)
  expect_named(
    config,
    c(
      "format",
      "format_version",
      "root",
      "graph",
      "metadata",
      "exclusions",
      "integrity"
    )
  )
  root <- config$graph$nodes[[config$root]]

  # Check signature
  expect_equal(length(root$signature$inputs), 1)
  expect_equal(root$signature$inputs[[1]]$name, "text")
  expect_equal(root$signature$instructions, "Test instructions")

  # Check metadata
  expect_equal(root$class, "PredictModule")
  expect_false(is.null(config$metadata$created_at))
  expect_false(is.null(config$metadata$packages$dsprrr))
})

test_that("pin_module_config captures optimization state", {
  skip_if_not_installed("pins")

  mod <- create_test_module()

  # Simulate optimization
  mod$state$compiled <- TRUE
  mod$state$best_score <- 0.85
  mod$state$best_trial <- 2
  mod$state$best_params <- list(temperature = 0.3)
  mod$state$trials <- tibble::tibble(trial_id = 1:3, score = c(0.7, 0.85, 0.75))
  mod$config$compiled <- TRUE

  board <- pins::board_temp()
  pin_module_config(board, "optimized-module", mod)
  config <- pins::pin_read(board, "optimized-module")
  root <- config$graph$nodes[[config$root]]

  expect_true(root$optimization$compiled)
  expect_equal(root$optimization$best_score, 0.85)
  expect_equal(root$optimization$best_trial, 2)
  expect_equal(root$optimization$best_params$temperature, 0.3)
  expect_equal(root$optimization$n_trials, 3)
})

# ---- restore_module_config tests ----

test_that("restore_module_config recreates module", {
  skip_if_not_installed("pins")

  mod <- create_test_module()
  mod$config$temperature <- 0.5
  mod$config$template <- "Custom: {text}"

  board <- pins::board_temp()
  pin_module_config(board, "to-restore", mod)
  config <- pins::pin_read(board, "to-restore")

  restored <- restore_module_config(config)

  expect_s3_class(restored, "Module")
  expect_equal(restored$config$temperature, 0.5)
  expect_equal(restored$config$template, "Custom: {text}")
})

test_that("restore_module_config restores optimization state", {
  skip_if_not_installed("pins")

  mod <- create_test_module()
  mod$state$compiled <- TRUE
  mod$state$best_score <- 0.9
  mod$state$best_trial <- 1
  mod$state$best_params <- list(temperature = 0.1)
  mod$config$compiled <- TRUE

  board <- pins::board_temp()
  pin_module_config(board, "compiled-module", mod)
  config <- pins::pin_read(board, "compiled-module")

  restored <- restore_module_config(config)

  expect_true(restored$is_compiled())
  expect_equal(restored$state$best_score, 0.9)
})

test_that("restore_module_config rejects legacy pinned configs", {
  legacy_config <- list(
    signature = list(
      inputs = list(),
      output_type = "string",
      instructions = ""
    ),
    config = list()
  )

  expect_snapshot(restore_module_config(legacy_config), error = TRUE)
})

test_that("pin_module_config round-trips chain_of_thought kind", {
  skip_if_not_installed("pins")

  mod <- module(signature("question -> answer"), type = "chain_of_thought")
  board <- pins::board_temp()

  pin_module_config(board, "cot", mod)
  config <- pins::pin_read(board, "cot")
  restored <- restore_module_config(config)

  root <- config$graph$nodes[[config$root]]
  expect_equal(root$kind, "chain_of_thought")
  expect_equal(restored$config$.module_kind, "chain_of_thought")
  expect_true("reasoning" %in% names(restored$signature@output_type@properties))
})

test_that("pin_module_config round-trips multichain core state", {
  skip_if_not_installed("pins")

  mod <- multi_chain_comparison("question -> answer", M = 4, temperature = 0.9)
  board <- pins::board_temp()

  pin_module_config(board, "mcc", mod)
  config <- pins::pin_read(board, "mcc")
  restored <- restore_module_config(config)

  root <- config$graph$nodes[[config$root]]
  expect_equal(root$kind, "multichain")
  expect_s3_class(restored, "Module")
  expect_equal(restored$config$.module_kind, "multichain")
  expect_equal(restored$M, 4)
  expect_equal(restored$temperature, 0.9)
  expect_equal(restored$inner_module$config$.module_kind, "chain_of_thought")
})

# ---- pin_trace tests ----

test_that("pin_trace requires pins package", {
  skip_if_not_installed("pins")

  mod <- create_test_module()
  board <- pins::board_temp()

  # Module has no traces - should warn
  expect_warning(
    pin_trace(board, "empty-traces", mod),
    "No traces to pin"
  )
})

test_that("pin_trace validates module argument", {
  skip_if_not_installed("pins")

  board <- pins::board_temp()

  expect_error(
    pin_trace(board, "test", list()),
    "must be a DSPrrr Module object"
  )
})

test_that("pin_trace saves traces with metadata", {
  skip_if_not_installed("pins")

  mod <- create_test_module()

  # Add mock traces
  mod$state$traces <- list(
    list(
      timestamp = Sys.time(),
      latency_ms = 100,
      input_tokens = 50,
      output_tokens = 20,
      total_tokens = 70,
      cost = 0.001,
      model = "test-model",
      prompt_length = 100,
      prompt = "Test prompt",
      output = "Test output"
    ),
    list(
      timestamp = Sys.time(),
      latency_ms = 150,
      input_tokens = 60,
      output_tokens = 25,
      total_tokens = 85,
      cost = 0.0015,
      model = "test-model",
      prompt_length = 120,
      prompt = "Another prompt",
      output = "Another output"
    )
  )

  board <- pins::board_temp()
  pin_trace(board, "test-traces", mod, include_prompts = TRUE)
  trace_data <- pins::pin_read(board, "test-traces")

  expect_true("traces" %in% names(trace_data))
  expect_true("summary" %in% names(trace_data))
  expect_true("metadata" %in% names(trace_data))

  expect_equal(trace_data$metadata$n_traces, 2)
  expect_true(trace_data$metadata$include_prompts)
})

# ---- pin_vitals_log tests ----

test_that("pin_vitals_log requires pins package", {
  skip_if_not_installed("pins")

  eval_result <- structure(
    list(
      mean_score = 0.85,
      scores = list(1, 0.8, 0.75),
      predictions = list("a", "b", "c"),
      n_evaluated = 3,
      n_errors = 0,
      metadata = list()
    ),
    class = "dsprrr_evaluation"
  )

  board <- pins::board_temp()
  result <- pin_vitals_log(board, "test-eval", eval_result)

  expect_equal(result, "test-eval")
})

test_that("pin_vitals_log saves evaluation data", {
  skip_if_not_installed("pins")

  eval_result <- structure(
    list(
      mean_score = 0.9,
      scores = list(1, 0.8, 0.9, 1),
      predictions = list("pos", "neg", "pos", "pos"),
      n_evaluated = 4,
      n_errors = 0,
      metadata = list(test = "value")
    ),
    class = "dsprrr_evaluation"
  )

  mod <- create_test_module()
  board <- pins::board_temp()

  pin_vitals_log(board, "eval-data", eval_result, module = mod)
  log_data <- pins::pin_read(board, "eval-data")

  expect_equal(log_data$type, "dsprrr_evaluation")
  expect_equal(log_data$mean_score, 0.9)
  expect_equal(log_data$n_evaluated, 4)
  expect_true(!is.null(log_data$module_info))
  expect_equal(log_data$module_info$module_type, "PredictModule")
})

test_that("pin_vitals_log handles generic list results", {
  skip_if_not_installed("pins")

  generic_result <- list(
    accuracy = 0.85,
    f1 = 0.82,
    custom_metric = 123
  )

  board <- pins::board_temp()
  pin_vitals_log(board, "generic-eval", generic_result)
  log_data <- pins::pin_read(board, "generic-eval")

  expect_equal(log_data$type, "generic")
  expect_equal(log_data$data$accuracy, 0.85)
})

# ---- validate_workflow tests ----

test_that("validate_workflow checks module type", {
  result <- validate_workflow("not a module")

  expect_false(result$valid)
  expect_false(result$checks$module$passed)
})

test_that("validate_workflow validates module correctly", {
  mod <- create_test_module()

  result <- validate_workflow(mod)

  expect_true(result$checks$module$passed)
  expect_true(result$checks$signature$passed)
})

test_that("validate_workflow checks dataset compatibility", {
  mod <- create_test_module()

  # Compatible dataset
  good_data <- tibble::tibble(text = c("a", "b", "c"))
  result1 <- validate_workflow(mod, data = good_data)
  expect_true(result1$checks$data$passed)

  # Incompatible data
  bad_data <- tibble::tibble(wrong_column = c("a", "b", "c"))
  result2 <- validate_workflow(mod, data = bad_data)
  expect_false(result2$valid)
  expect_false(result2$checks$data$passed)
})

test_that("validate_workflow checks pins board", {
  skip_if_not_installed("pins")

  mod <- create_test_module()
  board <- pins::board_temp()

  result <- validate_workflow(mod, board = board)

  expect_true(result$checks$board$passed)
})

# ---- use_dsprrr_template tests ----

test_that("use_dsprrr_template creates targets template", {
  skip_on_cran()

  temp_dir <- tempdir()
  on.exit(unlink(file.path(temp_dir, "_targets.R")))

  # Check if template exists in installed package
  template_path <- system.file(
    "templates",
    "targets",
    "_targets.R",
    package = "dsprrr"
  )

  if (file.exists(template_path)) {
    result <- use_dsprrr_template("targets", path = temp_dir)
    expect_true(file.exists(file.path(temp_dir, "_targets.R")))
  } else {
    skip("Templates not installed (development mode)")
  }
})

test_that("targets template parses and declares a valid graph", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")

  project_dir <- withr::local_tempdir()
  script <- file.path(project_dir, "_targets.R")
  use_dsprrr_template("targets", path = project_dir)

  parsed <- parse(script)
  expect_gt(length(parsed), 0L)

  withr::local_package("targets")
  withr::local_package("tarchetypes")
  graph <- source(
    script,
    local = new.env(parent = globalenv())
  )$value

  expect_type(graph, "list")
  expect_length(graph, 12L)

  manifest <- targets::tar_manifest(
    script = script,
    fields = c("name", "format"),
    callr_function = NULL
  )
  expect_setequal(
    manifest$name,
    c(
      "train_data",
      "test_data",
      "module_definition",
      "llm_client",
      "optimized_module",
      "evaluation_results",
      "pins_board",
      "pinned_config",
      "pinned_traces",
      "pinned_evaluation",
      "summary_stats",
      "summary_json"
    )
  )
  expect_equal(
    manifest$format[manifest$name == "summary_json"],
    "file"
  )
})

test_that("generated targets workflow runs its core graph end to end", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tarchetypes")
  skip_if_not_installed("jsonlite")

  make_mock_chat <- function() {
    structure(
      list(
        chat_structured = function(prompt, type, ...) {
          sentiment <- if (
            grepl(
              "terrible|not recommend|worst|waste|disappointing|not worth",
              prompt,
              ignore.case = TRUE
            )
          ) {
            "negative"
          } else if (
            grepl(
              "amazing|love|exceeded|great|highly recommend",
              prompt,
              ignore.case = TRUE
            )
          ) {
            "positive"
          } else {
            "neutral"
          }

          list(sentiment = sentiment)
        },
        get_turns = function(...) list(),
        set_turns = function(...) invisible(NULL),
        last_turn = function(...) NULL,
        get_model = function() "deterministic-test",
        clone = function(deep = FALSE) make_mock_chat()
      ),
      class = "Chat"
    )
  }

  project_dir <- withr::local_tempdir()
  use_dsprrr_template("targets", path = project_dir)
  withr::local_dir(project_dir)
  withr::local_envvar(DSPRRR_CACHE_ENABLED = "false")
  old_cache <- configure_cache(enable = FALSE)
  withr::defer(do.call(configure_cache, old_cache))
  withr::local_options(
    dsprrr.targets.llm_factory = function(model) make_mock_chat()
  )

  targets::tar_make(
    names = "summary_json",
    callr_function = NULL,
    reporter = "silent"
  )

  optimized <- targets::tar_read_raw("optimized_module")
  evaluation <- targets::tar_read_raw("evaluation_results")
  summary_path <- targets::tar_read_raw("summary_json")
  metadata <- targets::tar_meta(
    fields = c("name", "error")
  )
  completed <- metadata[
    metadata$name %in%
      c("optimized_module", "evaluation_results", "summary_json"),
    ,
    drop = FALSE
  ]

  expect_true(optimized$is_compiled())
  expect_s3_class(evaluation, "dsprrr_evaluation")
  expect_equal(evaluation$n_evaluated, 3L)
  expect_equal(evaluation$n_errors, 0L)
  expect_equal(evaluation$mean_score, 1)
  expect_true(file.exists(summary_path))
  expect_equal(
    jsonlite::read_json(summary_path, simplifyVector = TRUE)$accuracy,
    1
  )
  expect_setequal(
    completed$name,
    c("optimized_module", "evaluation_results", "summary_json")
  )
  expect_length(stats::na.omit(completed$error), 0L)
})

test_that("use_dsprrr_template respects overwrite argument", {
  skip_on_cran()

  temp_dir <- tempdir()
  test_file <- file.path(temp_dir, "_targets.R")
  on.exit(unlink(test_file))

  # Create existing file
  writeLines("existing content", test_file)

  template_path <- system.file(
    "templates",
    "targets",
    "_targets.R",
    package = "dsprrr"
  )

  if (file.exists(template_path)) {
    # Should warn without overwrite
    expect_warning(
      use_dsprrr_template("targets", path = temp_dir, overwrite = FALSE),
      "File already exists"
    )

    # Original content should be preserved
    expect_equal(readLines(test_file), "existing content")
  } else {
    skip("Templates not installed (development mode)")
  }
})

# ---- Round-trip tests ----

test_that("module config round-trips correctly", {
  skip_if_not_installed("pins")

  # Create a fully configured module
  mod <- create_test_module()
  mod$config$temperature <- 0.7
  mod$config$prompt_style <- "detailed"
  mod$demos <- list(
    list(inputs = list(text = "example"), output = "response")
  )

  # Simulate optimization
  mod$state$compiled <- TRUE
  mod$state$best_score <- 0.92
  mod$state$best_trial <- 3
  mod$state$best_params <- list(temperature = 0.7, prompt_style = "detailed")
  mod$config$compiled <- TRUE

  # Round-trip through pins
  board <- pins::board_temp()
  pin_module_config(board, "roundtrip-test", mod)
  config <- pins::pin_read(board, "roundtrip-test")
  restored <- restore_module_config(config)

  # Verify restoration
  expect_equal(restored$config$temperature, 0.7)
  expect_equal(restored$config$prompt_style, "detailed")
  expect_true(restored$is_compiled())
  expect_equal(restored$state$best_score, 0.92)
})

test_that("pin_module_config preserves complete pipelines (dsprrr-07u)", {
  skip_if_not_installed("pins")

  m1 <- module(signature("question -> thought"), type = "predict")
  m2 <- module(signature("thought -> answer"), type = "predict")
  pipe <- pipeline(m1, m2)
  board <- pins::board_temp()

  pin_module_config(board, "pipe", pipe)
  artifact <- pins::pin_read(board, "pipe")
  restored <- restore_module_config(artifact)

  expect_s3_class(restored, "PipelineModule")
  expect_identical(module_graph(restored)$path, module_graph(pipe)$path)
})
