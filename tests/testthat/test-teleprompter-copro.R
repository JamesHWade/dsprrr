# Tests for COPRO teleprompter

test_that("COPRO can be created with defaults", {
  tp <- COPRO()
  expect_s3_class(tp, "dsprrr::COPRO")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(tp@breadth, 10L)
  expect_equal(tp@depth, 3L)
  expect_equal(tp@init_temperature, 1.4)
  expect_true(tp@track_stats)
  expect_null(tp@prompt_model)
  expect_equal(tp@seed, 0L)
  expect_null(tp@log_dir)
  expect_null(tp@metric)
})

test_that("COPRO can be created with custom parameters", {
  metric_fn <- function(pred, exp) as.numeric(pred == exp)
  prompt_model <- list(chat = function(...) "new instruction")

  tp <- COPRO(
    metric = metric_fn,
    metric_threshold = 0.5,
    max_errors = 10L,
    prompt_model = prompt_model,
    breadth = 5L,
    depth = 2L,
    init_temperature = 1.0,
    track_stats = FALSE,
    seed = 42L,
    log_dir = tempdir()
  )

  expect_identical(tp@metric, metric_fn)
  expect_equal(tp@metric_threshold, 0.5)
  expect_equal(tp@max_errors, 10L)
  expect_identical(tp@prompt_model, prompt_model)
  expect_equal(tp@breadth, 5L)
  expect_equal(tp@depth, 2L)
  expect_equal(tp@init_temperature, 1.0)
  expect_false(tp@track_stats)
  expect_equal(tp@seed, 42L)
  expect_equal(tp@log_dir, tempdir())
})

test_that("COPRO validates properties", {
  expect_error(
    COPRO(breadth = 0L),
    "at least 1"
  )

  expect_error(
    COPRO(depth = 0L),
    "at least 1"
  )

  expect_error(
    COPRO(init_temperature = -1),
    "positive numeric"
  )

  expect_error(
    COPRO(init_temperature = c(1, 2)),
    "single positive numeric"
  )

  expect_error(
    COPRO(track_stats = c(TRUE, FALSE)),
    "single logical"
  )

  expect_error(
    COPRO(seed = c(1, 2)),
    "single numeric"
  )

  expect_error(
    COPRO(log_dir = 123),
    "character string"
  )
})

test_that("COPRO requires metric for compilation", {
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

  tp <- COPRO()
  expect_error(
    compile(tp, mod, trainset),
    "requires a metric"
  )
})

test_that("COPRO compile returns unmodified program for empty trainset", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig, type = "predict")

  empty_trainset <- data.frame(question = character(), answer = character())
  tp <- COPRO(metric = function(pred, exp) 1.0)

  expect_warning(
    result <- compile(tp, mod, empty_trainset),
    "Empty trainset"
  )
  expect_identical(result, mod)
})

test_that("generate_single_copro_candidate handles Chat objects", {
  # Create a mock Chat object
  MockChat <- R6::R6Class(
    "MockChat",
    inherit = NULL,
    public = list(
      chat = function(prompt) {
        "New improved instruction from Chat object"
      }
    )
  )
  # Set class to include "Chat" so inherits() works
  mock_chat <- MockChat$new()
  class(mock_chat) <- c("Chat", class(mock_chat))

  result <- dsprrr:::generate_single_copro_candidate(
    "Generate new instruction",
    prompt_model = mock_chat
  )

  expect_equal(result, "New improved instruction from Chat object")
})

test_that("generate_single_copro_candidate handles plain functions", {
  prompt_fn <- function(prompt) {
    "New instruction from function"
  }

  result <- dsprrr:::generate_single_copro_candidate(
    "Generate new instruction",
    prompt_model = prompt_fn
  )

  expect_equal(result, "New instruction from function")
})

test_that("generate_single_copro_candidate handles list with chat method", {
  prompt_model <- list(
    chat = function(prompt) {
      "New instruction from list$chat"
    }
  )

  result <- dsprrr:::generate_single_copro_candidate(
    "Generate new instruction",
    prompt_model = prompt_model
  )

  expect_equal(result, "New instruction from list$chat")
})

test_that("generate_single_copro_candidate handles list with chat_structured", {
  prompt_model <- list(
    chat_structured = function(prompt, type) {
      "New instruction from chat_structured"
    }
  )

  result <- dsprrr:::generate_single_copro_candidate(
    "Generate new instruction",
    prompt_model = prompt_model
  )

  expect_equal(result, "New instruction from chat_structured")
})

test_that("generate_single_copro_candidate returns NULL when no model", {
  expect_warning(
    result <- dsprrr:::generate_single_copro_candidate(
      "Generate new instruction",
      prompt_model = NULL,
      .llm = NULL
    ),
    "No prompt_model or .llm"
  )

  expect_null(result)
})

test_that("format_copro_failed_examples handles empty list", {
  result <- dsprrr:::format_copro_failed_examples(
    list(),
    c("question"),
    "answer"
  )

  expect_equal(result, "")
})

test_that("format_copro_failed_examples formats examples correctly", {
  failed_examples <- list(
    list(
      inputs = list(question = "What is 2+2?"),
      expected = "4",
      predicted = "5",
      score = 0
    ),
    list(
      inputs = list(question = "What is 3+3?"),
      expected = "6",
      predicted = "7",
      score = 0
    )
  )

  result <- dsprrr:::format_copro_failed_examples(
    failed_examples,
    c("question"),
    "answer",
    max_examples = 2L
  )

  expect_match(result, "Example 1")
  expect_match(result, "Example 2")
  expect_match(result, "2\\+2")
  expect_match(result, "Expected: 4")
  expect_match(result, "Got: 5")
})

test_that("format_copro_failed_examples respects max_examples", {
  failed_examples <- list(
    list(inputs = list(q = "1"), expected = "a", predicted = "b", score = 0),
    list(inputs = list(q = "2"), expected = "c", predicted = "d", score = 0),
    list(inputs = list(q = "3"), expected = "e", predicted = "f", score = 0)
  )

  result <- dsprrr:::format_copro_failed_examples(
    failed_examples,
    c("q"),
    "answer",
    max_examples = 1L
  )

  expect_match(result, "Example 1")
  expect_false(grepl("Example 2", result))
})

test_that("COPRO compile optimizes instructions", {
  # Track call count for different instructions
  call_count <- 0L
  best_instruction_applied <- FALSE

  # Mock LLM that improves with better instructions
  mock_llm <- list(
    chat_structured = function(prompt, type, ...) {
      call_count <<- call_count + 1L
      # Check if improved instruction is in the prompt
      if (grepl("IMPROVED", prompt)) {
        best_instruction_applied <<- TRUE
        return("4")
      }
      # Default behavior - sometimes wrong
      if (grepl("2\\+2", prompt)) {
        return(if (runif(1) > 0.3) "4" else "wrong")
      }
      "wrong"
    },
    chat = function(prompt) {
      # This is used for instruction generation
      "IMPROVED instruction for math problems"
    }
  )

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

  tp <- COPRO(
    metric = metric_fn,
    breadth = 2L,
    depth = 1L,
    track_stats = TRUE,
    seed = 1L
  )

  result <- compile(tp, mod, trainset, .llm = mock_llm)

  expect_true(result$config$compiled)
  expect_equal(result$config$teleprompter, "COPRO")
  expect_true(!is.null(result$config$optimizer$history))
  expect_true(length(result$config$optimizer$history) > 0)
})

test_that("COPRO tracks instruction history when track_stats is TRUE", {
  mock_llm <- list(
    chat_structured = function(prompt, type, ...) {
      "4"
    },
    chat = function(prompt) {
      paste("Instruction version", sample(1:100, 1))
    }
  )

  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = "What is 2+2?",
    answer = "4"
  )

  metric_fn <- function(pred, row) 1.0

  tp <- COPRO(
    metric = metric_fn,
    breadth = 2L,
    depth = 1L,
    track_stats = TRUE,
    seed = 42L
  )

  result <- compile(tp, mod, trainset, .llm = mock_llm)

  # Should have history recorded
  expect_true(length(result$config$optimizer$history) > 0)

  # First entry should be the baseline
  baseline <- result$config$optimizer$history[[1]]
  expect_equal(baseline$iteration, 0L)
  expect_equal(baseline$instructions, "Answer the question")
})

test_that("COPRO does not track history when track_stats is FALSE", {
  mock_llm <- list(
    chat_structured = function(prompt, type, ...) {
      "4"
    },
    chat = function(prompt) {
      "New instruction"
    }
  )

  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = "What is 2+2?",
    answer = "4"
  )

  metric_fn <- function(pred, row) 1.0

  tp <- COPRO(
    metric = metric_fn,
    breadth = 2L,
    depth = 1L,
    track_stats = FALSE,
    seed = 42L
  )

  result <- compile(tp, mod, trainset, .llm = mock_llm)

  expect_null(result$config$optimizer$history)
})

test_that("COPRO print method works", {
  tp <- COPRO(
    metric = function(x, y) 1,
    breadth = 5L,
    depth = 2L,
    init_temperature = 1.2,
    seed = 42L
  )

  # Print method should return invisibly
  expect_invisible(print(tp))
  expect_identical(print(tp), tp)
})

test_that("COPRO accepts Chat object as prompt_model", {
  MockChat <- R6::R6Class(
    "MockChat",
    inherit = NULL,
    public = list(
      chat = function(prompt) "instruction"
    )
  )
  mock_chat <- MockChat$new()
  class(mock_chat) <- c("Chat", class(mock_chat))

  tp <- COPRO(prompt_model = mock_chat)
  expect_s3_class(tp, "dsprrr::COPRO")
})

test_that("COPRO accepts function as prompt_model", {
  prompt_fn <- function(prompt) "instruction"

  tp <- COPRO(prompt_model = prompt_fn)
  expect_s3_class(tp, "dsprrr::COPRO")
})

test_that("COPRO accepts list with chat method as prompt_model", {
  prompt_model <- list(chat = function(prompt) "instruction")

  tp <- COPRO(prompt_model = prompt_model)
  expect_s3_class(tp, "dsprrr::COPRO")
})

test_that("COPRO accepts list with chat_structured method as prompt_model", {
  prompt_model <- list(chat_structured = function(prompt, type) "instruction")

  tp <- COPRO(prompt_model = prompt_model)
  expect_s3_class(tp, "dsprrr::COPRO")
})

test_that("COPRO rejects invalid prompt_model", {
  expect_error(
    COPRO(prompt_model = 123),
    "prompt_model must be"
  )

  expect_error(
    COPRO(prompt_model = list(invalid = "method")),
    "prompt_model must be"
  )
})

test_that("generate_copro_candidates deduplicates results", {
  # Create a prompt model that returns duplicates
  call_count <- 0L
  prompt_model <- list(
    chat = function(prompt) {
      call_count <<- call_count + 1L
      # Return same instruction every time
      "Same instruction"
    }
  )

  candidates <- dsprrr:::generate_copro_candidates(
    current_instructions = "Original",
    failed_examples = list(),
    input_names = "question",
    output_col = "answer",
    breadth = 5L,
    prompt_model = prompt_model
  )

  # Should have deduplicated to just 1 unique instruction
  expect_equal(length(candidates), 1)
  expect_equal(candidates[[1]], "Same instruction")
})
