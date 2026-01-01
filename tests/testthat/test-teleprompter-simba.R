# Tests for SIMBA teleprompter

test_that("SIMBA can be created with defaults", {
  tp <- SIMBA()
  expect_s3_class(tp, "dsprrr::SIMBA")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(tp@bsize, 32L)
  expect_equal(tp@num_candidates, 6L)
  expect_equal(tp@max_steps, 8L)
  expect_equal(tp@max_demos, 4L)
  expect_null(tp@prompt_model)
  expect_equal(tp@seed, 0L)
  expect_null(tp@log_dir)
  expect_null(tp@metric)
})

test_that("SIMBA can be created with custom parameters", {
  metric_fn <- function(pred, exp) as.numeric(pred == exp)
  prompt_model <- list(chat_structured = function(...) "rule")

  tp <- SIMBA(
    metric = metric_fn,
    metric_threshold = 0.5,
    max_errors = 10L,
    bsize = 8L,
    num_candidates = 3L,
    max_steps = 2L,
    max_demos = 1L,
    prompt_model = prompt_model,
    seed = 42L,
    log_dir = tempdir()
  )

  expect_identical(tp@metric, metric_fn)
  expect_equal(tp@metric_threshold, 0.5)
  expect_equal(tp@max_errors, 10L)
  expect_equal(tp@bsize, 8L)
  expect_equal(tp@num_candidates, 3L)
  expect_equal(tp@max_steps, 2L)
  expect_equal(tp@max_demos, 1L)
  expect_identical(tp@prompt_model, prompt_model)
  expect_equal(tp@seed, 42L)
  expect_equal(tp@log_dir, tempdir())
})

test_that("SIMBA validates properties", {
  expect_error(
    SIMBA(bsize = 0L),
    "at least 1"
  )

  expect_error(
    SIMBA(num_candidates = 0L),
    "at least 1"
  )

  expect_error(
    SIMBA(max_steps = 0L),
    "at least 1"
  )

  expect_error(
    SIMBA(max_demos = -1L),
    "non-negative"
  )

  expect_error(
    SIMBA(seed = c(1, 2)),
    "single numeric"
  )

  expect_error(
    SIMBA(log_dir = 123),
    "character string"
  )
})

test_that("SIMBA requires metric for compilation", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = c("What is 2+2?", "What is 3+3?"),
    answer = c("4", "6")
  )

  tp <- SIMBA()
  expect_error(
    compile(tp, mod, trainset),
    "requires a metric"
  )
})

test_that("SIMBA compile returns unmodified program for empty trainset", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig, type = "predict")

  empty_trainset <- data.frame(question = character(), answer = character())
  tp <- SIMBA(metric = function(pred, exp) 1.0)

  expect_warning(
    result <- compile(tp, mod, empty_trainset),
    "Empty trainset"
  )
  expect_identical(result, mod)
})

test_that("SIMBA compile applies rules and demos when improved", {
  MockSIMBAModule <- R6::R6Class(
    "MockSIMBAModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      initialize = function(
        signature,
        template = "",
        demos = list(),
        config = list()
      ) {
        super$initialize(
          signature,
          template = template,
          demos = demos,
          config = config
        )
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        input_val <- batch[[1]][[1]]
        expected_map <- c("What is 2+2?" = "4", "What is 3+3?" = "6")
        output_val <- if (grepl("SIMBA_RULE", self$signature@instructions)) {
          expected_map[[input_val]]
        } else {
          "wrong"
        }
        tibble::tibble(
          output = list(output_val),
          chat = list(NULL),
          metadata = list(list())
        )
      }
    )
  )

  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- MockSIMBAModule$new(signature = sig, template = "{question}")

  trainset <- data.frame(
    question = c("What is 2+2?", "What is 3+3?"),
    answer = c("4", "6")
  )

  metric_fn <- function(pred, row) {
    if (is.null(pred) || is.null(row$answer)) {
      return(0)
    }
    as.numeric(pred == row$answer)
  }

  prompt_model <- list(
    chat_structured = function(prompt, type, ...) {
      list(rule = "SIMBA_RULE")
    }
  )

  tp <- SIMBA(
    metric = metric_fn,
    bsize = 2L,
    num_candidates = 2L,
    max_steps = 2L,
    max_demos = 2L,
    prompt_model = prompt_model,
    seed = 1L
  )

  result <- compile(tp, mod, trainset)

  expect_true(result$config$compiled)
  expect_equal(result$config$teleprompter, "SIMBA")
  expect_true(grepl("SIMBA_RULE", result$signature@instructions))
  expect_true(length(result$demos) > 0)
  expect_true("SIMBA_RULE" %in% result$config$optimizer$rules)
})
