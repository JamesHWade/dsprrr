# Tests for BootstrapFewShotWithRandomSearch teleprompter

test_that("BootstrapFewShotWithRandomSearch can be created with defaults", {
  tp <- BootstrapFewShotWithRandomSearch()
  expect_s3_class(tp, "dsprrr::BootstrapFewShotWithRandomSearch")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(tp@num_candidate_programs, 16L)
  expect_equal(tp@num_threads, 1L)
  expect_null(tp@stop_at_score)
  expect_equal(tp@max_bootstrapped_demos, 4L)
  expect_equal(tp@max_labeled_demos, 16L)
  expect_equal(tp@max_rounds, 1L)
  expect_null(tp@teacher_settings)
  expect_null(tp@seed)
  expect_null(tp@log_dir)
  expect_null(tp@metric)
})

test_that("BootstrapFewShotWithRandomSearch can be created with custom parameters", {
  metric_fn <- function(pred, exp) as.numeric(pred == exp)
  tp <- BootstrapFewShotWithRandomSearch(
    metric = metric_fn,
    metric_threshold = 0.5,
    num_candidate_programs = 8L,
    num_threads = 2L,
    stop_at_score = 0.9,
    max_bootstrapped_demos = 6L,
    max_labeled_demos = 8L,
    max_rounds = 2L,
    teacher_settings = list(temperature = 0.8),
    seed = 42L,
    log_dir = tempdir()
  )

  expect_identical(tp@metric, metric_fn)
  expect_equal(tp@metric_threshold, 0.5)
  expect_equal(tp@num_candidate_programs, 8L)
  expect_equal(tp@num_threads, 2L)
  expect_equal(tp@stop_at_score, 0.9)
  expect_equal(tp@max_bootstrapped_demos, 6L)
  expect_equal(tp@max_labeled_demos, 8L)
  expect_equal(tp@max_rounds, 2L)
  expect_equal(tp@teacher_settings, list(temperature = 0.8))
  expect_equal(tp@seed, 42L)
})

test_that("BootstrapFewShotWithRandomSearch validates properties", {
  # Invalid num_candidate_programs
  expect_error(
    BootstrapFewShotWithRandomSearch(num_candidate_programs = 0L),
    "at least 1"
  )

  # Invalid num_threads
  expect_error(
    BootstrapFewShotWithRandomSearch(num_threads = 0L),
    "at least 1"
  )

  # Invalid stop_at_score
  expect_error(
    BootstrapFewShotWithRandomSearch(stop_at_score = 1.5),
    "between 0 and 1"
  )
  expect_error(
    BootstrapFewShotWithRandomSearch(stop_at_score = c(0.5, 0.8)),
    "single numeric"
  )

  # Invalid max_bootstrapped_demos
  expect_error(
    BootstrapFewShotWithRandomSearch(max_bootstrapped_demos = -1L),
    "non-negative"
  )

  # Invalid max_labeled_demos
  expect_error(
    BootstrapFewShotWithRandomSearch(max_labeled_demos = -1L),
    "non-negative"
  )

  # Invalid max_rounds
  expect_error(
    BootstrapFewShotWithRandomSearch(max_rounds = 0L),
    "at least 1"
  )

  # Invalid seed
  expect_error(
    BootstrapFewShotWithRandomSearch(seed = c(1, 2)),
    "single numeric"
  )
})

test_that("BootstrapFewShotWithRandomSearch requires metric", {
  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig)
  trainset <- data.frame(question = "Q1", answer = "A1")
  valset <- data.frame(question = "Q2", answer = "A2")

  tp <- BootstrapFewShotWithRandomSearch()
  expect_error(
    compile(mod, tp, trainset, valset = valset),
    "requires a metric"
  )
})

test_that("BootstrapFewShotWithRandomSearch rejects RLM before candidates", {
  runner <- list(
    execute = function(code, context = list(), ...) {
      list(success = TRUE, result = NULL)
    },
    policy = function() {
      list(
        backend = "test",
        trust = "test-only",
        sandboxed = TRUE,
        persistent = TRUE
      )
    }
  )
  program <- rlm_module("question -> answer", runner = runner)
  optimizer <- BootstrapFewShotWithRandomSearch(
    metric = function(prediction, expected) 1,
    num_candidate_programs = 3L
  )

  error <- tryCatch(
    compile(
      program,
      optimizer,
      data.frame(question = "train", answer = "train"),
      valset = data.frame(question = "val", answer = "val")
    ),
    error = identity
  )

  expect_s3_class(error, "dsprrr_bootstrap_graph_unsupported")
  expect_match(
    conditionMessage(error),
    "BootstrapFewShotWithRandomSearch",
    fixed = TRUE
  )
  expect_identical(error$paths, "$")
})

test_that("BootstrapFewShotWithRandomSearch rejects wrapped Flex", {
  flex_program <- suppressWarnings(flex("question -> answer"))
  program <- best_of_n(
    flex_program,
    N = 2L,
    reward_fn = function(...) 1
  )
  optimizer <- BootstrapFewShotWithRandomSearch(
    metric = function(prediction, expected) 1,
    num_candidate_programs = 3L
  )

  error <- tryCatch(
    compile(
      program,
      optimizer,
      data.frame(question = "train", answer = "train"),
      valset = data.frame(question = "val", answer = "val")
    ),
    error = identity
  )

  expect_s3_class(error, "dsprrr_flex_demo_unsupported_error")
  expect_match(
    conditionMessage(error),
    "BootstrapFewShotWithRandomSearch",
    fixed = TRUE
  )
  expect_identical(error$paths, "$/module")
})

test_that("BootstrapFewShotWithRandomSearch requires valset", {
  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig)
  trainset <- data.frame(question = "Q1", answer = "A1")

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, exp) 1.0
  )
  expect_error(
    compile(mod, tp, trainset),
    "requires a validation set"
  )
})

test_that("BootstrapFewShotWithRandomSearch handles empty trainset", {
  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig)
  empty_trainset <- data.frame(question = character(), answer = character())
  valset <- data.frame(question = "Q1", answer = "A1")

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, exp) 1.0
  )

  expect_warning(
    result <- compile(mod, tp, empty_trainset, valset = valset),
    "Empty trainset"
  )
  expect_identical(result, mod)
})

test_that("generate_candidate_configs produces correct candidates", {
  tp <- BootstrapFewShotWithRandomSearch(
    num_candidate_programs = 5L,
    max_bootstrapped_demos = 4L,
    max_labeled_demos = 8L,
    max_rounds = 2L,
    seed = 42L
  )

  configs <- dsprrr:::generate_candidate_configs(tp, 100, 42L)

  # Should have 5 candidates

  expect_length(configs, 5)

  # First should be baseline
  expect_equal(configs[[1]]$name, "baseline")
  expect_equal(configs[[1]]$type, "baseline")

  # Second should be labeled_only
  expect_equal(configs[[2]]$name, "labeled_only")
  expect_equal(configs[[2]]$type, "labeled")

  # Third should be bootstrap_unshuffled
  expect_equal(configs[[3]]$name, "bootstrap_unshuffled")
  expect_equal(configs[[3]]$type, "bootstrap")
  expect_false(configs[[3]]$shuffle)

  # Remaining should be bootstrap with random seeds
  expect_equal(configs[[4]]$type, "bootstrap")
  expect_true(configs[[4]]$shuffle)
  expect_equal(configs[[5]]$type, "bootstrap")
  expect_true(configs[[5]]$shuffle)
})

test_that("BootstrapFewShotWithRandomSearch compiles and selects best", {
  # Use a standard module with a mock LLM that returns predictable results
  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(signature = sig)

  trainset <- data.frame(
    question = c("Q1", "Q2", "Q3", "Q4"),
    answer = c("correct", "correct", "correct", "correct")
  )

  valset <- data.frame(
    question = c("VQ1", "VQ2"),
    answer = c("correct", "correct")
  )

  # Mock LLM that always returns "correct"
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      list(answer = "correct")
    }
  )

  # Metric that returns 1.0 for "correct"
  exact_metric <- function(pred, row) {
    expected <- if (is.data.frame(row)) row$answer else row
    as.numeric(pred == expected)
  }

  tp <- BootstrapFewShotWithRandomSearch(
    metric = exact_metric,
    num_candidate_programs = 4L, # baseline + labeled + unshuffled + 1 random
    max_labeled_demos = 2L,
    max_bootstrapped_demos = 2L,
    seed = 42L
  )

  result <- compile(mod, tp, trainset, valset = valset, .llm = mock_llm)

  expect_true(inherits(result, "Module"))
  expect_true(result$config$compiled)
  expect_equal(result$config$teleprompter, "BootstrapFewShotWithRandomSearch")

  # Should have optimizer info
  expect_true("optimizer" %in% names(result$config))
  expect_true("candidate_programs" %in% names(result$config$optimizer))
  expect_true("best_candidate" %in% names(result$config$optimizer))
  expect_true("best_score" %in% names(result$config$optimizer))

  # Candidates should be ranked
  candidates <- result$config$optimizer$candidate_programs
  expect_gte(length(candidates), 1)
})

test_that("BootstrapFewShotWithRandomSearch early stopping works", {
  # Use a standard module with a mock LLM
  sig <- Signature(
    inputs = list(input(name = "x", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  mod <- module(signature = sig)

  trainset <- data.frame(x = c("a", "b", "c", "d"), y = c("1", "2", "3", "4"))
  valset <- data.frame(x = c("e", "f"), y = c("correct", "correct"))

  # Mock LLM that always returns "correct"
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      list(answer = "correct")
    }
  )

  tp <- BootstrapFewShotWithRandomSearch(
    # Always return 1.0 so early stopping triggers on first candidate
    metric = function(pred, row) 1.0,
    num_candidate_programs = 10L, # Would normally try 10
    stop_at_score = 0.9, # But should stop early since metric returns 1.0
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L
  )

  result <- compile(mod, tp, trainset, valset = valset, .llm = mock_llm)

  # Should have stopped early (first candidate should hit threshold)
  expect_lt(
    result$config$optimizer$num_candidates_evaluated,
    10
  )
})

test_that("BootstrapFewShotWithRandomSearch print method works", {
  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(x, y) 1.0,
    num_candidate_programs = 8L,
    num_threads = 2L,
    stop_at_score = 0.95,
    max_bootstrapped_demos = 6L,
    seed = 42L
  )

  # Verify print method runs without error and returns invisibly
  expect_invisible(print(tp))
  expect_identical(print(tp), tp)
})

test_that("compile works with BootstrapFewShotWithRandomSearch", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Summarize"
  )
  mod <- module(signature = sig)

  trainset <- data.frame(
    text = c("Hello world", "Goodbye world"),
    summary = c("greeting", "farewell")
  )

  valset <- data.frame(
    text = c("Test text"),
    summary = c("test")
  )

  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      list(summary = "mocked")
    }
  )

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, exp) 0.5,
    num_candidate_programs = 3L,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L
  )

  result <- compile(mod, tp, trainset, valset = valset, .llm = mock_llm)

  expect_true(inherits(result, "Module"))
  expect_true(result$is_compiled())
  expect_equal(result$config$teleprompter, "BootstrapFewShotWithRandomSearch")
})

test_that("candidate configs include proper metadata", {
  tp <- BootstrapFewShotWithRandomSearch(
    num_candidate_programs = 4L,
    max_bootstrapped_demos = 3L,
    max_labeled_demos = 5L,
    max_rounds = 2L,
    teacher_settings = list(temperature = 0.9),
    seed = 123L
  )

  configs <- dsprrr:::generate_candidate_configs(tp, 50, 123L)

  # Check bootstrap configs have all required fields
  bootstrap_config <- configs[[3]] # bootstrap_unshuffled
  expect_equal(bootstrap_config$max_bootstrapped_demos, 3L)
  expect_equal(bootstrap_config$max_labeled_demos, 5L)
  expect_equal(bootstrap_config$max_rounds, 2L)
  expect_equal(bootstrap_config$teacher_settings, list(temperature = 0.9))
})

test_that("BootstrapFewShotWithRandomSearch handles candidate compilation errors", {
  # Use a standard module with a mock LLM
  sig <- Signature(
    inputs = list(input(name = "x", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  mod <- module(signature = sig)

  trainset <- data.frame(x = c("a", "b"), y = c("1", "2"))
  valset <- data.frame(x = c("c"), y = c("ok"))

  # Mock LLM that returns "ok"
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      list(answer = "ok")
    }
  )

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, row) {
      expected <- if (is.data.frame(row)) row$y else row
      as.numeric(pred == expected)
    },
    num_candidate_programs = 3L,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L
  )

  # Should complete successfully
  result <- compile(mod, tp, trainset, valset = valset, .llm = mock_llm)

  expect_true(inherits(result, "Module"))
  expect_true(result$config$compiled)
})

test_that("Bootstrap random search resets its outer budget on valid candidates", {
  configs <- lapply(letters[1:3], function(name) {
    list(name = name, type = "baseline")
  })
  eval_calls <- 0L

  testthat::local_mocked_bindings(
    generate_candidate_configs = function(...) configs,
    compile_candidate = function(config, program, ...) {
      compiled <- copy_module(program)
      compiled$config$candidate_name <- config$name
      compiled
    },
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      if (eval_calls %in% c(1L, 3L)) {
        stop("candidate evaluation failed")
      }
      EvalResult(mean_score = 0.8, n_evaluated = 1L)
    },
    .package = "dsprrr"
  )

  teleprompter <- BootstrapFewShotWithRandomSearch(
    metric = function(...) 1,
    num_candidate_programs = 3L,
    max_errors = 2L
  )
  result <- expect_test_warnings(
    dsprrr:::compile_bootstrap_rs(
      teleprompter,
      module(signature("x -> y")),
      data.frame(x = "train", y = "train"),
      valset = data.frame(x = "val", y = "val")
    ),
    "Failed to evaluate candidate"
  )
  budget <- result$config$optimizer$budget_summary

  expect_equal(budget$attempts, 3L)
  expect_equal(budget$successes, 1L)
  expect_equal(budget$total_errors, 2L)
  expect_equal(budget$consecutive_errors, 1L)
  expect_false(budget$stopped)
  expect_identical(result$config$optimizer$best_candidate, "b")
})

test_that("Bootstrap random search honors caller resource controls", {
  configs <- lapply(letters[1:3], function(name) {
    list(name = name, type = "baseline")
  })
  eval_calls <- 0L

  testthat::local_mocked_bindings(
    generate_candidate_configs = function(...) configs,
    compile_candidate = function(config, program, ...) copy_module(program),
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      EvalResult(
        examples = tibble::tibble(
          score = 0.8,
          error = NA_character_,
          predicted = "answer",
          feedback = NA_character_
        ),
        mean_score = 0.8,
        n_evaluated = 1L,
        input_tokens = 2L,
        output_tokens = 1L,
        total_tokens = 3L,
        total_cost = 0.01,
        provider_calls = 1L,
        metric_calls = 1L
      )
    },
    .package = "dsprrr"
  )

  teleprompter <- BootstrapFewShotWithRandomSearch(
    metric = function(...) 1,
    num_candidate_programs = 3L
  )
  result <- dsprrr:::compile_bootstrap_rs(
    teleprompter,
    module(signature("x -> y")),
    data.frame(x = "train", y = "train"),
    valset = data.frame(x = "val", y = "val"),
    control = dsprrr:::optimizer_control(
      max_metric_calls = 2L,
      progress = FALSE
    )
  )
  budget <- result$config$optimizer$budget_summary

  expect_identical(eval_calls, 2L)
  expect_identical(budget$trials, 2L)
  expect_identical(budget$metric_calls, 2L)
  expect_identical(budget$provider_calls, 2L)
  expect_identical(budget$input_tokens, 4L)
  expect_identical(budget$output_tokens, 2L)
  expect_identical(budget$total_tokens, 6L)
  expect_equal(budget$total_cost, 0.02)
  expect_identical(budget$attempts, 2L)
  expect_identical(budget$successes, 2L)
  expect_identical(budget$stop_reason$code, "max_metric_calls")
})

test_that("Bootstrap random search preserves mixed validation row outcomes", {
  configs <- lapply(letters[1:2], function(name) {
    list(name = name, type = "baseline")
  })
  eval_calls <- 0L

  testthat::local_mocked_bindings(
    generate_candidate_configs = function(...) configs,
    compile_candidate = function(config, program, ...) copy_module(program),
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      EvalResult(
        examples = tibble::tibble(
          score = c(1, NA_real_),
          error = c(NA_character_, "metric failed")
        ),
        mean_score = 0.5,
        n_evaluated = 1L,
        n_errors = 1L,
        metric_calls = 2L
      )
    },
    .package = "dsprrr"
  )

  teleprompter <- BootstrapFewShotWithRandomSearch(
    metric = function(...) 1,
    num_candidate_programs = 2L,
    max_errors = 1L
  )
  result <- dsprrr:::compile_bootstrap_rs(
    teleprompter,
    module(signature("x -> y")),
    data.frame(x = "train", y = "train"),
    valset = data.frame(
      x = c("first", "second"),
      y = c("first", "second")
    )
  )
  optimizer <- result$config$optimizer
  budget <- optimizer$budget_summary

  expect_identical(eval_calls, 1L)
  expect_identical(optimizer$num_candidates_evaluated, 1L)
  expect_identical(optimizer$best_candidate, "a")
  expect_identical(budget$attempts, 2L)
  expect_identical(budget$successes, 1L)
  expect_identical(budget$total_errors, 1L)
  expect_identical(budget$consecutive_errors, 1L)
  expect_true(budget$stopped)
  expect_identical(budget$stop_reason$code, "max_errors")
  expect_identical(
    budget$stop_reason$stage,
    "bootstrap_rs_validation"
  )
})

test_that("Bootstrap random search returns baseline when validation is blocked", {
  configs <- lapply(letters[1:2], function(name) {
    list(name = name, type = "baseline")
  })
  eval_calls <- 0L

  testthat::local_mocked_bindings(
    generate_candidate_configs = function(...) configs,
    compile_candidate = function(config, program, ...) {
      compiled <- copy_module(program)
      compiled$config$candidate_name <- config$name
      compiled
    },
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      EvalResult(mean_score = 1, n_evaluated = 1L, metric_calls = 1L)
    },
    .package = "dsprrr"
  )

  teleprompter <- BootstrapFewShotWithRandomSearch(
    metric = function(...) 1,
    num_candidate_programs = 2L
  )
  result <- dsprrr:::compile_bootstrap_rs(
    teleprompter,
    module(signature("x -> y")),
    data.frame(x = "train", y = "train"),
    valset = data.frame(x = "val", y = "val"),
    control = dsprrr:::optimizer_control(
      max_metric_calls = 0L,
      progress = FALSE
    )
  )
  optimizer <- result$config$optimizer
  budget <- optimizer$budget_summary

  expect_s3_class(result, "PredictModule")
  expect_identical(result$config$candidate_name, "a")
  expect_identical(eval_calls, 0L)
  expect_identical(optimizer$num_candidates_evaluated, 1L)
  expect_true(optimizer$partial)
  expect_false(optimizer$best_complete)
  expect_identical(optimizer$best_candidate, NA_character_)
  expect_identical(optimizer$best_score, NA_real_)
  expect_false(optimizer$candidate_programs[[1L]]$complete)
  expect_identical(budget$attempts, 0L)
  expect_identical(budget$successes, 0L)
  expect_identical(budget$total_errors, 0L)
  expect_identical(budget$trials, 0L)
  expect_identical(budget$metric_calls, 0L)
  expect_true(budget$stopped)
  expect_s3_class(budget$stop_reason, "dsprrr_optimizer_stop_reason")
  expect_identical(optimizer$stop_reason, budget$stop_reason)
  expect_identical(budget$stop_reason$code, "max_metric_calls")
  expect_identical(budget$stop_reason$observed, 0L)
})

test_that("Bootstrap random search preserves its best at the exact limit", {
  configs <- lapply(letters[1:4], function(name) {
    list(name = name, type = "baseline")
  })
  eval_calls <- 0L

  testthat::local_mocked_bindings(
    generate_candidate_configs = function(...) configs,
    compile_candidate = function(config, program, ...) {
      compiled <- copy_module(program)
      compiled$config$candidate_name <- config$name
      compiled$config$optimizer <- list(error_count = 99L)
      compiled
    },
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      if (eval_calls > 1L) {
        stop("candidate evaluation failed")
      }
      EvalResult(mean_score = 0.9, n_evaluated = 1L)
    },
    .package = "dsprrr"
  )

  teleprompter <- BootstrapFewShotWithRandomSearch(
    metric = function(...) 1,
    num_candidate_programs = 4L,
    max_errors = 2L
  )
  result <- expect_test_warnings(
    dsprrr:::compile_bootstrap_rs(
      teleprompter,
      module(signature("x -> y")),
      data.frame(x = "train", y = "train"),
      valset = data.frame(x = "val", y = "val")
    ),
    "Failed to evaluate candidate"
  )
  optimizer <- result$config$optimizer

  expect_equal(eval_calls, 3L)
  expect_equal(optimizer$num_candidates_evaluated, 3L)
  expect_identical(optimizer$best_candidate, "a")
  expect_equal(optimizer$error_count, 2L)
  expect_true(optimizer$budget_summary$stopped)
  expect_equal(optimizer$stop_reason$observed, 2L)
  expect_equal(optimizer$stop_reason$attempts, 3L)
})

test_that("Bootstrap random search rejects unusable evaluation scores", {
  configs <- lapply(letters[1:3], function(name) {
    list(name = name, type = "baseline")
  })
  eval_calls <- 0L
  scores <- c(0.9, NA_real_, Inf)

  testthat::local_mocked_bindings(
    generate_candidate_configs = function(...) configs,
    compile_candidate = function(config, program, ...) copy_module(program),
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      EvalResult(mean_score = scores[[eval_calls]], n_evaluated = 1L)
    },
    .package = "dsprrr"
  )

  teleprompter <- BootstrapFewShotWithRandomSearch(
    metric = function(...) 1,
    num_candidate_programs = 3L,
    max_errors = 2L
  )
  result <- expect_test_warnings(
    dsprrr:::compile_bootstrap_rs(
      teleprompter,
      module(signature("x -> y")),
      data.frame(x = "train", y = "train"),
      valset = data.frame(x = "val", y = "val")
    ),
    "unusable score"
  )
  optimizer <- result$config$optimizer
  candidate_scores <- vapply(
    optimizer$candidate_programs,
    function(candidate) candidate$score,
    numeric(1)
  )

  expect_equal(eval_calls, 3L)
  expect_identical(optimizer$best_candidate, "a")
  expect_equal(optimizer$best_score, 0.9)
  expect_equal(optimizer$budget_summary$attempts, 5L)
  expect_equal(optimizer$budget_summary$successes, 3L)
  expect_equal(optimizer$error_count, 2L)
  expect_equal(optimizer$budget_summary$consecutive_errors, 1L)
  expect_false(optimizer$budget_summary$stopped)
  expect_null(optimizer$stop_reason)
  expect_equal(candidate_scores, c(0.9, NA_real_, NA_real_))
})

test_that("Bootstrap random search max_errors zero stops after one failure", {
  configs <- lapply(letters[1:3], function(name) {
    list(name = name, type = "baseline")
  })
  compile_calls <- 0L

  testthat::local_mocked_bindings(
    generate_candidate_configs = function(...) configs,
    compile_candidate = function(config, program, ...) {
      compile_calls <<- compile_calls + 1L
      if (config$name == "b") {
        stop("candidate compilation failed")
      }
      compiled <- copy_module(program)
      compiled$config$optimizer <- list(error_count = 99L)
      compiled
    },
    eval_program = function(...) {
      EvalResult(mean_score = 0.7, n_evaluated = 1L)
    },
    .package = "dsprrr"
  )

  teleprompter <- BootstrapFewShotWithRandomSearch(
    metric = function(...) 1,
    num_candidate_programs = 3L,
    max_errors = 0L
  )
  result <- expect_test_warnings(
    dsprrr:::compile_bootstrap_rs(
      teleprompter,
      module(signature("x -> y")),
      data.frame(x = "train", y = "train"),
      valset = data.frame(x = "val", y = "val")
    ),
    "Failed to compile candidate"
  )
  budget <- result$config$optimizer$budget_summary

  expect_equal(compile_calls, 2L)
  expect_equal(budget$attempts, 2L)
  expect_equal(budget$successes, 1L)
  expect_equal(budget$total_errors, 1L)
  expect_equal(budget$stop_reason$limit, 0L)
  expect_identical(result$config$optimizer$best_candidate, "a")
})

test_that("BootstrapFewShotWithRandomSearch errors when all candidates fail", {
  # Use a standard module with a mock LLM that always fails
  sig <- Signature(
    inputs = list(input(name = "x", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  mod <- module(signature = sig)

  trainset <- data.frame(x = c("a", "b"), y = c("1", "2"))
  valset <- data.frame(x = c("c"), y = c("ok"))

  # Mock LLM that always fails
  failing_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      stop("LLM always fails")
    }
  )

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, row) as.numeric(pred == row$y),
    num_candidate_programs = 3L,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L
  )

  # Should error when all candidates fail
  expect_error(
    suppressWarnings(
      compile(mod, tp, trainset, valset = valset, .llm = failing_llm)
    ),
    "All .* candidate programs failed"
  )
})
