# Tests for SIMBA teleprompter

new_simba_prompt_chat <- function(chat) {
  new_test_chat(chat = chat)
}

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
  prompt_model <- new_simba_prompt_chat(function(...) "rule")

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
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig)

  trainset <- data.frame(
    question = c("What is 2+2?", "What is 3+3?"),
    answer = c("4", "6")
  )

  tp <- SIMBA()
  expect_error(
    compile(mod, tp, trainset),
    "requires a metric"
  )
})

test_that("SIMBA compile returns unmodified program for empty trainset", {
  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig)

  empty_trainset <- data.frame(question = character(), answer = character())
  tp <- SIMBA(metric = function(pred, exp) 1.0)

  expect_warning(
    result <- compile(mod, tp, empty_trainset),
    "Empty trainset"
  )
  expect_identical(result, mod)
})

test_that("generate_simba_rule handles Chat objects", {
  prompt_model <- new_simba_prompt_chat(
    function(prompt) "Generated rule from Chat object"
  )

  hard_examples <- data.frame(
    question = "What is 2+2?",
    answer = "4"
  )

  result <- dsprrr:::generate_simba_rule(
    prompt_model,
    hard_examples,
    input_names = "question",
    output_col = "answer"
  )

  expect_equal(result, "Generated rule from Chat object")
})

test_that("SIMBA rejects non-Chat prompt models", {
  expect_error(
    SIMBA(prompt_model = function(prompt) "rule"),
    "NULL or an ellmer Chat R6 object"
  )
  expect_error(
    SIMBA(prompt_model = list(chat = function(prompt) "rule")),
    "NULL or an ellmer Chat R6 object"
  )
  expect_error(
    SIMBA(
      prompt_model = list(
        chat_structured = function(prompt, type) "rule"
      )
    ),
    "NULL or an ellmer Chat R6 object"
  )
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

test_that("generate_simba_rule falls back when its Chat fails", {
  prompt_model <- new_simba_prompt_chat(
    function(prompt) stop("provider unavailable")
  )
  hard_examples <- data.frame(
    question = "What is 2+2?",
    answer = "4"
  )

  expect_warning(
    result <- dsprrr:::generate_simba_rule(
      prompt_model,
      hard_examples,
      input_names = "question",
      output_col = "answer"
    ),
    class = "dsprrr_simba_rule_warning"
  )

  expect_match(result, "SIMBA rule:")
})

test_that("SIMBA compile applies rules and demos when improved", {
  # Track call count to simulate improvement after rule is applied
  call_count <- 0L

  # Mock LLM that returns wrong answers initially, correct after SIMBA_RULE
  mock_llm <- new_test_chat(
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

  prompt_model <- new_simba_prompt_chat(
    function(prompt, ...) "SIMBA_RULE"
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

  result <- compile(mod, tp, trainset, .llm = mock_llm)

  expect_true(result$config$compiled)
  expect_equal(result$config$teleprompter, "SIMBA")
  expect_true(grepl("SIMBA_RULE", result$signature@instructions, fixed = TRUE))
  expect_true(length(result$demos) > 0)
  expect_true("SIMBA_RULE" %in% result$config$optimizer$rules)
})

test_that("SIMBA returns empty variability diagnostics when its budget stops", {
  program <- module(
    signature("question -> answer", instructions = "Baseline")
  )
  minibatch <- data.frame(
    question = c("q1", "q2"),
    answer = c("a1", "a2")
  )
  budget <- dsprrr:::new_optimizer_budget(
    dsprrr:::optimizer_control(max_metric_calls = 0L)
  )

  variability <- dsprrr:::simba_variability(
    program,
    minibatch,
    output_col = "answer",
    metric = function(...) 1,
    num_candidates = 1L,
    control = dsprrr:::optimizer_control(max_metric_calls = 0L),
    budget = budget
  )

  expect_s3_class(variability, "tbl_df")
  expect_identical(
    names(variability),
    c("row_id", "variability", "mean_score", "difficulty")
  )
  expect_equal(nrow(variability), 0L)
  expect_identical(
    dsprrr:::optimizer_budget_summary(budget)$stop_reason$code,
    "max_metric_calls"
  )

  testthat::local_mocked_bindings(
    optimizer_eval_candidate = function(...) dsprrr:::EvalResult(),
    .package = "dsprrr"
  )
  expect_error(
    dsprrr:::simba_variability(
      program,
      minibatch,
      output_col = "answer",
      metric = function(...) 1,
      num_candidates = 1L
    ),
    class = "dsprrr_simba_all_candidates_failed"
  )
})

test_that("SIMBA preserves and reports a complete baseline over a biased partial", {
  eval_calls <- 0L
  log_dir <- withr::local_tempdir()
  if (.Platform$OS.type == "unix") {
    Sys.chmod(log_dir, mode = "0700", use_umask = FALSE)
  }

  testthat::local_mocked_bindings(
    eval_program = function(program, dataset, ...) {
      eval_calls <<- eval_calls + 1L
      score <- if (
        grepl(
          "SIMBA rule:",
          program$signature@instructions,
          fixed = TRUE
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

  result <- dsprrr:::compile_simba(
    SIMBA(
      metric = function(...) 1,
      bsize = 2L,
      num_candidates = 1L,
      max_steps = 1L,
      max_demos = 0L,
      seed = 1L
    ),
    module(
      signature("question -> answer", instructions = "Baseline")
    ),
    data.frame(
      question = c("q1", "q2"),
      answer = c("a1", "a2")
    ),
    control = dsprrr:::optimizer_control(
      max_metric_calls = 5L,
      log_dir = log_dir
    )
  )
  optimizer <- result$config$optimizer

  expect_equal(eval_calls, 5L)
  expect_identical(result$signature@instructions, "Baseline")
  expect_equal(optimizer$best_score, 0.5)
  expect_true(optimizer$best_complete)
  expect_true(optimizer$partial)
  expect_identical(optimizer$stop_reason$code, "max_metric_calls")
  expect_true("simba:baseline" %in% optimizer$budget_summary$completed_units)
  expect_false(
    "simba:step:1:candidate" %in% optimizer$budget_summary$completed_units
  )
  trials_path <- file.path(log_dir, "trials.jsonl")
  expect_length(dsprrr:::read_trials_jsonl(trials_path), 0L)
})
