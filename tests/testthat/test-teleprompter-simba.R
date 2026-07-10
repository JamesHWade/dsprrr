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

test_that("generate_simba_rule handles Chat objects", {
  # Create a mock Chat object
  MockChat <- R6::R6Class(
    "MockChat",
    inherit = NULL,
    public = list(
      chat = function(prompt) {
        "Generated rule from Chat object"
      }
    )
  )
  # Set class to include "Chat" so inherits() works
  mock_chat <- MockChat$new()
  class(mock_chat) <- c("Chat", class(mock_chat))

  hard_examples <- data.frame(
    question = "What is 2+2?",
    answer = "4"
  )

  result <- dsprrr:::generate_simba_rule(
    mock_chat,
    hard_examples,
    input_names = "question",
    output_col = "answer"
  )

  expect_equal(result, "Generated rule from Chat object")
})

test_that("generate_simba_rule handles plain functions", {
  prompt_fn <- function(prompt) {
    "Generated rule from function"
  }

  hard_examples <- data.frame(
    question = "What is 2+2?",
    answer = "4"
  )

  result <- dsprrr:::generate_simba_rule(
    prompt_fn,
    hard_examples,
    input_names = "question",
    output_col = "answer"
  )

  expect_equal(result, "Generated rule from function")
})

test_that("generate_simba_rule falls back when prompt_model is NULL", {
  hard_examples <- data.frame(
    question = "What is 2+2?",
    answer = "4"
  )

  result <- dsprrr:::generate_simba_rule(
    NULL,
    hard_examples,
    input_names = "question",
    output_col = "answer"
  )

  expect_match(result, "SIMBA rule:")
})

test_that("SIMBA compile applies rules and demos when improved", {
  # Track call count to simulate improvement after rule is applied
  call_count <- 0L

  # Mock LLM that returns wrong answers initially, correct after SIMBA_RULE
  mock_llm <- local({
    self <- structure(
      list(
        chat_structured = function(prompt, type, ...) {
          call_count <<- call_count + 1L
          # Check if SIMBA_RULE has been applied by looking at the prompt
          if (grepl("SIMBA_RULE", prompt, fixed = TRUE)) {
            # After rule is applied, return correct answers
            if (grepl("2\\+2", prompt)) {
              return("4")
            } else if (grepl("3\\+3", prompt)) {
              return("6")
            }
          }
          # Before rule, return wrong answer
          "wrong"
        },
        clone = function(...) self,
        set_turns = function(turns) invisible(NULL)
      ),
      class = "Chat"
    )
    self
  })

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

  result <- compile(tp, mod, trainset, .llm = mock_llm)

  expect_true(result$config$compiled)
  expect_equal(result$config$teleprompter, "SIMBA")
  expect_true(grepl("SIMBA_RULE", result$signature@instructions, fixed = TRUE))
  expect_true(length(result$demos) > 0)
  expect_true("SIMBA_RULE" %in% result$config$optimizer$rules)
})
