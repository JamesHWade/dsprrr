# Tests for COPRO teleprompter

new_copro_prompt_chat <- function(chat) {
  new_test_chat(chat = chat)
}

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
  prompt_model <- new_copro_prompt_chat(function(...) "new instruction")

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
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig)

  trainset <- data.frame(
    question = c("What is 2+2?", "What is 3+3?"),
    answer = c("4", "6")
  )

  tp <- COPRO()
  expect_error(
    compile(mod, tp, trainset),
    "requires a metric"
  )
})

test_that("COPRO compile returns unmodified program for empty trainset", {
  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig)

  empty_trainset <- data.frame(question = character(), answer = character())
  tp <- COPRO(metric = function(pred, exp) 1.0)

  expect_warning(
    result <- compile(mod, tp, empty_trainset),
    "Empty trainset"
  )
  expect_identical(result, mod)
})

test_that("generate_single_copro_candidate handles Chat objects", {
  prompt_model <- new_copro_prompt_chat(
    function(prompt) "New improved instruction from Chat object"
  )

  result <- dsprrr:::generate_single_copro_candidate(
    "Generate new instruction",
    prompt_model = prompt_model
  )

  expect_equal(result, "New improved instruction from Chat object")
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
  expect_false(grepl("Example 2", result, fixed = TRUE))
})

test_that("COPRO compile optimizes instructions", {
  # Track call count for different instructions
  call_count <- 0L
  best_instruction_applied <- FALSE

  # Mock LLM that improves with better instructions
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      call_count <<- call_count + 1L
      # Check if improved instruction is in the prompt
      if (grepl("IMPROVED", prompt, fixed = TRUE)) {
        best_instruction_applied <<- TRUE
        return("4")
      }
      # Deterministic baseline behavior; batch branches must not share RNG state.
      if (grepl("2\\+2", prompt)) {
        return("wrong")
      }
      "wrong"
    },
    chat = function(prompt) {
      # This is used for instruction generation
      "IMPROVED instruction for math problems"
    }
  )

  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(signature = sig)

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

  result <- compile(mod, tp, trainset, .llm = mock_llm)

  expect_true(result$config$compiled)
  expect_equal(result$config$teleprompter, "COPRO")
  expect_true(!is.null(result$config$optimizer$history))
  expect_true(length(result$config$optimizer$history) > 0)
})

test_that("COPRO tracks instruction history when track_stats is TRUE", {
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      "4"
    },
    chat = function(prompt) {
      paste("Instruction version", sample.int(100, 1))
    }
  )

  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(signature = sig)

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

  result <- compile(mod, tp, trainset, .llm = mock_llm)

  # Should have history recorded
  expect_true(length(result$config$optimizer$history) > 0)

  # First entry should be the baseline
  baseline <- result$config$optimizer$history[[1]]
  expect_equal(baseline$iteration, 0L)
  expect_equal(baseline$instructions, "Answer the question")
})

test_that("COPRO does not track history when track_stats is FALSE", {
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      "4"
    },
    chat = function(prompt) {
      "New instruction"
    }
  )

  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(signature = sig)

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

  result <- compile(mod, tp, trainset, .llm = mock_llm)

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
  mock_chat <- new_copro_prompt_chat(function(prompt) "instruction")

  tp <- COPRO(prompt_model = mock_chat)
  expect_s3_class(tp, "dsprrr::COPRO")
})

test_that("COPRO rejects non-Chat prompt models", {
  expect_error(
    COPRO(prompt_model = function(prompt) "instruction"),
    "NULL or an ellmer Chat R6 object"
  )
  expect_error(
    COPRO(prompt_model = list(chat = function(prompt) "instruction")),
    "NULL or an ellmer Chat R6 object"
  )
  expect_error(
    COPRO(
      prompt_model = list(
        chat_structured = function(prompt, type) "instruction"
      )
    ),
    "NULL or an ellmer Chat R6 object"
  )
})

test_that("generate_copro_candidates deduplicates results", {
  # Create a prompt model that returns duplicates
  call_count <- 0L
  prompt_model <- new_copro_prompt_chat(
    function(prompt) {
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

test_that("COPRO generation budget follows requested candidate order", {
  calls <- 0L
  prompt_model <- new_copro_prompt_chat(
    function(prompt) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(NULL)
      }
      if (calls == 2L) {
        return("usable instruction")
      }
      stop("generation failed")
    }
  )
  budget <- dsprrr:::new_optimizer_budget(
    dsprrr:::optimizer_control(max_errors = 2L)
  )

  candidates <- expect_test_warnings(
    dsprrr:::generate_copro_candidates(
      current_instructions = "Original",
      failed_examples = list(),
      input_names = "question",
      output_col = "answer",
      breadth = 3L,
      prompt_model = prompt_model,
      budget = budget
    ),
    "COPRO instruction generation failed"
  )
  summary <- dsprrr:::optimizer_budget_summary(budget)

  expect_equal(calls, 3L)
  expect_identical(candidates, list("usable instruction"))
  expect_equal(summary$attempts, 3L)
  expect_equal(summary$successes, 1L)
  expect_equal(summary$total_errors, 2L)
  expect_equal(summary$consecutive_errors, 1L)
  expect_false(summary$stopped)
})

test_that("COPRO generation max_errors zero stops after its first request", {
  calls <- 0L
  budget <- dsprrr:::new_optimizer_budget(
    dsprrr:::optimizer_control(max_errors = 0L)
  )

  candidates <- dsprrr:::generate_copro_candidates(
    current_instructions = "Original",
    failed_examples = list(),
    input_names = "question",
    output_col = "answer",
    breadth = 3L,
    prompt_model = new_copro_prompt_chat(
      function(prompt) {
        calls <<- calls + 1L
        NULL
      }
    ),
    budget = budget
  )
  summary <- dsprrr:::optimizer_budget_summary(budget)

  expect_equal(calls, 1L)
  expect_length(candidates, 0L)
  expect_equal(summary$attempts, 1L)
  expect_equal(summary$total_errors, 1L)
  expect_true(summary$stopped)
  expect_equal(summary$stop_reason$limit, 0L)
})

test_that("COPRO preserves the best candidate when evaluation exhausts budget", {
  eval_calls <- 0L
  generation_calls <- 0L
  eval_result <- function(mean_score, scores, errors) {
    dsprrr:::EvalResult(
      examples = data.frame(score = scores, error = errors),
      mean_score = mean_score,
      n_evaluated = as.integer(sum(!is.na(scores))),
      n_errors = as.integer(sum(is.na(scores)))
    )
  }

  testthat::local_mocked_bindings(
    identify_failed_examples = function(...) list(),
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      if (eval_calls == 1L) {
        return(eval_result(0.5, 0.5, NA_character_))
      }
      if (eval_calls == 2L) {
        return(eval_result(0.7, 0.7, NA_character_))
      }
      eval_result(
        0.9,
        c(NA_real_, NA_real_),
        c("first failure", "second failure")
      )
    },
    .package = "dsprrr"
  )

  prompt_model <- new_copro_prompt_chat(
    function(prompt) {
      generation_calls <<- generation_calls + 1L
      paste("Instruction", generation_calls)
    }
  )
  teleprompter <- COPRO(
    metric = function(...) 1,
    breadth = 2L,
    depth = 1L,
    max_errors = 2L,
    prompt_model = prompt_model,
    track_stats = TRUE
  )
  result <- dsprrr:::compile_copro(
    teleprompter,
    module(signature("question -> answer")),
    data.frame(question = "q", answer = "a")
  )
  optimizer <- result$config$optimizer

  expect_equal(eval_calls, 3L)
  expect_equal(generation_calls, 2L)
  expect_equal(optimizer$budget_summary$attempts, 6L)
  expect_equal(optimizer$budget_summary$successes, 4L)
  expect_equal(optimizer$error_count, 2L)
  expect_true(optimizer$budget_summary$stopped)
  expect_equal(optimizer$stop_reason$attempts, 6L)
  expect_identical(result$signature@instructions, "Instruction 2")
})

test_that("COPRO retains partial evidence without selecting or logging it", {
  eval_calls <- 0L
  log_dir <- withr::local_tempdir()
  if (.Platform$OS.type == "unix") {
    Sys.chmod(log_dir, mode = "0700", use_umask = FALSE)
  }

  testthat::local_mocked_bindings(
    identify_failed_examples = function(...) list(),
    eval_program = function(program, dataset, ...) {
      eval_calls <<- eval_calls + 1L
      score <- if (
        identical(
          program$signature@instructions,
          "Biased partial candidate"
        )
      ) {
        1
      } else {
        0.5
      }
      dsprrr:::EvalResult(
        examples = data.frame(
          score = score,
          error = NA_character_,
          predicted = "answer",
          feedback = NA_character_
        ),
        mean_score = score,
        n_evaluated = 1L,
        n_errors = 0L,
        metric_calls = 1L
      )
    },
    .package = "dsprrr"
  )

  result <- dsprrr:::compile_copro(
    COPRO(
      metric = function(...) 1,
      prompt_model = new_copro_prompt_chat(
        function(...) "Biased partial candidate"
      ),
      breadth = 1L,
      depth = 1L,
      track_stats = TRUE
    ),
    module(
      signature("question -> answer", instructions = "Baseline")
    ),
    data.frame(
      question = c("q1", "q2"),
      answer = c("a1", "a2")
    ),
    control = dsprrr:::optimizer_control(
      max_metric_calls = 3L,
      log_dir = log_dir
    )
  )
  optimizer <- result$config$optimizer

  expect_equal(eval_calls, 3L)
  expect_identical(result$signature@instructions, "Baseline")
  expect_equal(optimizer$final_score, 0.5)
  expect_true(optimizer$best_complete)
  expect_true(optimizer$baseline_complete)
  expect_true(optimizer$partial)
  expect_identical(optimizer$stop_reason$code, "max_metric_calls")
  expect_true("copro:baseline" %in% optimizer$budget_summary$completed_units)
  expect_false(
    "copro:iteration:1:candidate:1" %in%
      optimizer$budget_summary$completed_units
  )
  expect_length(optimizer$history, 2L)
  expect_equal(optimizer$history[[2]]$score, 1)
  expect_false(optimizer$history[[2]]$complete)
  expect_false(optimizer$history[[2]]$is_best)
  trials_path <- file.path(log_dir, "trials.jsonl")
  expect_length(dsprrr:::read_trials_jsonl(trials_path), 0L)
})
