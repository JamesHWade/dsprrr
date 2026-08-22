# Tests for GEPA teleprompter

test_that("GEPA can be created with defaults", {
  tp <- GEPA()
  expect_s3_class(tp, "dsprrr::GEPA")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(tp@population_size, 20L)
  expect_equal(tp@generations, 10L)
  expect_equal(tp@mutation_rate, 0.1)
  expect_equal(tp@crossover_rate, 0.7)
  expect_equal(tp@selection, "pareto")
  expect_identical(tp@component_selector, "round_robin")
  expect_identical(tp@use_merge, TRUE)
  expect_identical(tp@max_merge_invocations, 5L)
  expect_true(tp@verbose)
  expect_true(tp@track_stats)
  expect_identical(tp@track_best_outputs, FALSE)
  expect_null(tp@metrics)
})

test_that("GEPA validates properties", {
  expect_error(GEPA(population_size = 1L), "at least 2")
  expect_error(GEPA(generations = 0L), "at least 1")
  expect_error(GEPA(mutation_rate = -0.1), "between 0 and 1")
  expect_error(GEPA(crossover_rate = 1.2), "between 0 and 1")
  expect_error(GEPA(selection = "unknown"), "selection must")
  expect_error(GEPA(metrics = list("not a function")), "functions")
  expect_no_error(GEPA(seed = 42))
  for (seed in list(NA_real_, Inf, 1.5, -1, .Machine$integer.max + 1)) {
    expect_error(GEPA(seed = seed), "whole number")
  }
  expect_snapshot(
    error = TRUE,
    GEPA(component_selector = "random")
  )
  expect_snapshot(
    error = TRUE,
    GEPA(max_merge_invocations = -1L)
  )
})

test_that("Pareto utilities identify frontier", {
  # Scores where rows 1, 2, 3 are non-dominated (Pareto frontier)
  # Row 1: (1.0, 0.2) - best on metric 1
  # Row 2: (0.5, 0.8) - balanced
  # Row 3: (0.2, 1.0) - best on metric 2
  # Row 4: (0.2, 0.2) - dominated by all others
  scores <- matrix(
    c(
      1.0,
      0.2,
      0.5,
      0.8,
      0.2,
      1.0,
      0.2,
      0.2
    ),
    ncol = 2,
    byrow = TRUE
  )

  frontier <- dsprrr:::pareto_frontier(scores)
  expect_setequal(frontier, c(1, 2, 3))

  ranks <- dsprrr:::pareto_ranks(scores)
  expect_equal(ranks[1], 1L)
  expect_equal(ranks[2], 1L)
  expect_equal(ranks[3], 1L)
  expect_equal(ranks[4], 2L)
})

test_that("GEPA compiles with reflection-based mutation", {
  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Be concise."
  )
  mod <- module(signature = sig)

  trainset <- data.frame(
    question = c("q1", "q2"),
    answer = c("yes", "yes"),
    stringsAsFactors = FALSE
  )

  # Metric that checks if prediction matches expected
  metric_fn <- function(pred, expected) {
    as.numeric(pred == expected$answer)
  }

  # Track mutation calls to verify LLM is being used
  mutation_calls <- 0L

  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      # Check if this is a mutation call (reflection prompt) or regular inference
      if (grepl("improving system instructions", prompt, ignore.case = TRUE)) {
        mutation_calls <<- mutation_calls + 1L
        list(instructions = "Be accurate and explicit.")
      } else if (grepl("accurate", prompt, ignore.case = TRUE)) {
        # Mutated instructions yield the correct answer.
        list(answer = "yes")
      } else {
        list(answer = "no")
      }
    }
  )

  tp <- GEPA(
    metrics = list(quality = metric_fn),
    population_size = 4L,
    generations = 1L,
    mutation_rate = 1,
    selection = "current_best",
    seed = 1L,
    verbose = FALSE
  )

  compiled <- compile(mod, tp, trainset, .llm = mock_llm)

  expect_true(compiled$config$compiled)
  expect_equal(compiled$config$teleprompter, "GEPA")

  # Verify mutations were called (at least for initial population generation)
  expect_true(mutation_calls >= 3) # population_size - 1 for initial population

  # The compiled module should have mutated instructions (members with "accurate"
  # scored higher because the mock LLM only returns correct answers for them)
  expect_true(grepl(
    "accurate",
    compiled$signature@instructions,
    ignore.case = TRUE
  ))
  expect_true(length(compiled$config$optimizer$objective_pareto_front) >= 1)
})

test_that("GEPA accounts every metric-row outcome in execution order", {
  eval_calls <- 0L
  mean_scores <- c(0.3, 0.4, 0.8, 0.7)
  make_result <- function(mean_score) {
    dsprrr:::EvalResult(
      examples = data.frame(
        score = c(NA_real_, 0.1),
        error = c("row failed", NA_character_),
        predicted = c(NA_character_, "low score"),
        feedback = c(NA_character_, "try again")
      ),
      mean_score = mean_score,
      n_evaluated = 1L,
      n_errors = 1L
    )
  }

  testthat::local_mocked_bindings(
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      make_result(mean_scores[[eval_calls]])
    },
    gepa_mutate_instruction = function(...) "Mutated instructions",
    .package = "dsprrr"
  )

  teleprompter <- GEPA(
    metrics = list(
      quality = function(...) 1,
      safety = function(...) 1
    ),
    population_size = 2L,
    generations = 1L,
    selection = "pareto",
    max_errors = 2L,
    verbose = FALSE
  )
  result <- dsprrr:::compile_gepa(
    teleprompter,
    module(signature("question -> answer")),
    data.frame(
      question = c("q1", "q2"),
      answer = c("a1", "a2")
    )
  )
  budget <- result$config$optimizer$budget_summary

  expect_equal(eval_calls, 4L)
  expect_equal(budget$attempts, 8L)
  expect_equal(budget$successes, 4L)
  expect_equal(budget$total_errors, 4L)
  expect_equal(budget$consecutive_errors, 0L)
  expect_false(budget$stopped)
  expect_length(result$config$optimizer$all_generations[[1]]$population, 2L)
})

test_that("GEPA preserves a complete partial generation at max_errors zero", {
  eval_calls <- 0L
  make_result <- function(mean_score, error = NA_character_) {
    failed <- !is.na(error)
    dsprrr:::EvalResult(
      examples = data.frame(
        score = if (failed) NA_real_ else mean_score,
        error = error,
        predicted = if (failed) NA_character_ else "answer",
        feedback = NA_character_
      ),
      mean_score = mean_score,
      n_evaluated = if (failed) 0L else 1L,
      n_errors = if (failed) 1L else 0L
    )
  }

  testthat::local_mocked_bindings(
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      if (eval_calls == 1L) {
        return(make_result(0.6))
      }
      if (eval_calls == 2L) {
        return(make_result(0.7))
      }
      make_result(NA_real_, "secondary candidate failed")
    },
    gepa_mutate_instruction = function(...) "Mutated instructions",
    .package = "dsprrr"
  )

  teleprompter <- GEPA(
    metrics = list(
      quality = function(...) 1,
      safety = function(...) 1
    ),
    population_size = 2L,
    generations = 1L,
    selection = "pareto",
    max_errors = 0L,
    verbose = FALSE
  )
  result <- dsprrr:::compile_gepa(
    teleprompter,
    module(
      signature(
        "question -> answer",
        instructions = "Original instructions"
      )
    ),
    data.frame(question = "q", answer = "a")
  )
  optimizer <- result$config$optimizer

  expect_equal(eval_calls, 3L)
  expect_equal(optimizer$budget_summary$attempts, 3L)
  expect_equal(optimizer$budget_summary$successes, 2L)
  expect_equal(optimizer$error_count, 1L)
  expect_true(optimizer$budget_summary$stopped)
  expect_identical(optimizer$stop_reason$stage, "gepa_metric_1")
  expect_equal(optimizer$stop_reason$limit, 0L)
  expect_length(optimizer$all_generations[[1]]$population, 1L)
  expect_equal(optimizer$best_scores, c(quality = 0.6, safety = 0.7))
  expect_identical(result$signature@instructions, "Original instructions")
})

test_that("GEPA keeps the scalar best across regressing generations", {
  eval_calls <- 0L
  mean_scores <- c(0.9, 0.8, 0.2, 0.1)
  make_result <- function(score) {
    dsprrr:::EvalResult(
      examples = data.frame(
        score = score,
        error = NA_character_,
        predicted = "answer",
        feedback = NA_character_
      ),
      mean_score = score,
      n_evaluated = 1L,
      n_errors = 0L
    )
  }

  testthat::local_mocked_bindings(
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      make_result(mean_scores[[eval_calls]])
    },
    gepa_mutate_instruction = function(...) "Initial alternative",
    gepa_next_generation = function(...) {
      list("Worse generation one", "Worse generation two")
    },
    .package = "dsprrr"
  )

  teleprompter <- GEPA(
    metrics = list(quality = function(...) 1),
    population_size = 2L,
    generations = 2L,
    selection = "current_best",
    max_errors = 2L,
    verbose = FALSE
  )
  result <- dsprrr:::compile_gepa(
    teleprompter,
    module(
      signature("question -> answer", instructions = "Global best")
    ),
    data.frame(question = "q", answer = "a")
  )

  expect_equal(eval_calls, 4L)
  expect_equal(result$config$optimizer$best_scores, c(quality = 0.9))
  expect_identical(result$signature@instructions, "Global best")
  expect_length(result$config$optimizer$all_generations, 2L)
})

test_that("GEPA Pareto selection survives an interrupted worse generation", {
  eval_calls <- 0L
  mean_scores <- c(0.9, 0.9, 0.8, 0.8, 0.2, 0.2, NA_real_)
  make_result <- function(score) {
    failed <- is.na(score)
    dsprrr:::EvalResult(
      examples = data.frame(
        score = score,
        error = if (failed) "evaluation failed" else NA_character_,
        predicted = if (failed) NA_character_ else "answer",
        feedback = NA_character_
      ),
      mean_score = score,
      n_evaluated = if (failed) 0L else 1L,
      n_errors = if (failed) 1L else 0L
    )
  }

  testthat::local_mocked_bindings(
    eval_program = function(...) {
      eval_calls <<- eval_calls + 1L
      make_result(mean_scores[[eval_calls]])
    },
    gepa_mutate_instruction = function(...) "Initial alternative",
    gepa_next_generation = function(...) {
      list("Worse complete", "Interrupted candidate")
    },
    .package = "dsprrr"
  )

  teleprompter <- GEPA(
    metrics = list(
      quality = function(...) 1,
      safety = function(...) 1
    ),
    population_size = 2L,
    generations = 2L,
    selection = "pareto",
    max_errors = 0L,
    verbose = FALSE
  )
  result <- dsprrr:::compile_gepa(
    teleprompter,
    module(
      signature("question -> answer", instructions = "Global Pareto best")
    ),
    data.frame(question = "q", answer = "a")
  )
  optimizer <- result$config$optimizer

  expect_equal(eval_calls, 7L)
  expect_equal(optimizer$best_scores, c(quality = 0.9, safety = 0.9))
  expect_identical(result$signature@instructions, "Global Pareto best")
  expect_true(optimizer$budget_summary$stopped)
  expect_length(optimizer$all_generations[[1]]$population, 2L)
  expect_length(optimizer$all_generations[[2]]$population, 1L)
  expect_equal(optimizer$stop_reason$limit, 0L)
})

test_that("GEPA never selects or logs a biased partial metric evaluation", {
  eval_calls <- 0L
  log_dir <- withr::local_tempdir()
  if (.Platform$OS.type == "unix") {
    Sys.chmod(log_dir, mode = "0700", use_umask = FALSE)
  }

  testthat::local_mocked_bindings(
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
    gepa_mutate_instruction = function(...) "Biased partial candidate",
    .package = "dsprrr"
  )

  result <- dsprrr:::compile_gepa(
    GEPA(
      metrics = list(
        quality = function(...) 1,
        safety = function(...) 1
      ),
      population_size = 2L,
      generations = 1L,
      selection = "pareto",
      verbose = FALSE
    ),
    module(
      signature("question -> answer", instructions = "Baseline")
    ),
    data.frame(
      question = c("q1", "q2"),
      answer = c("a1", "a2")
    ),
    control = dsprrr:::optimizer_control(
      max_metric_calls = 7L,
      log_dir = log_dir
    )
  )
  optimizer <- result$config$optimizer

  expect_equal(eval_calls, 7L)
  expect_identical(result$signature@instructions, "Baseline")
  expect_equal(optimizer$best_scores, c(quality = 0.5, safety = 0.5))
  expect_true(optimizer$partial)
  expect_identical(optimizer$stop_reason$code, "max_metric_calls")
  expect_true(all(
    c(
      "gepa:generation:1:candidate:1:metric:1",
      "gepa:generation:1:candidate:1:metric:2",
      "gepa:generation:1:candidate:2:metric:1"
    ) %in%
      optimizer$budget_summary$completed_units
  ))
  expect_false(
    "gepa:generation:1:candidate:2:metric:2" %in%
      optimizer$budget_summary$completed_units
  )
  expect_length(optimizer$all_generations[[1]]$population, 1L)

  trials <- dsprrr:::read_trials_jsonl(file.path(log_dir, "trials.jsonl"))
  expect_length(trials, 1L)
  expect_identical(trials[[1]]@status, "completed")
  expect_identical(trials[[1]]@params$index, 1L)
})

test_that("GEPA reports an early budget stop without a failure warning", {
  testthat::local_mocked_bindings(
    eval_program = function(...) {
      dsprrr:::EvalResult(
        examples = data.frame(
          score = 1,
          error = NA_character_,
          predicted = "answer",
          feedback = NA_character_
        ),
        mean_score = 1,
        n_evaluated = 1L,
        n_errors = 0L,
        metric_calls = 1L
      )
    },
    gepa_mutate_instruction = function(...) "Partial candidate",
    .package = "dsprrr"
  )

  result <- expect_no_warning(dsprrr:::compile_gepa(
    GEPA(
      metrics = list(quality = function(...) 1),
      population_size = 2L,
      generations = 1L,
      verbose = FALSE
    ),
    module(
      signature("question -> answer", instructions = "Baseline")
    ),
    data.frame(
      question = c("q1", "q2"),
      answer = c("a1", "a2")
    ),
    control = dsprrr:::optimizer_control(max_metric_calls = 1L)
  ))
  optimizer <- result$config$optimizer

  expect_identical(result$signature@instructions, "Baseline")
  expect_true(all(is.na(optimizer$best_scores)))
  expect_true(optimizer$partial)
  expect_identical(optimizer$stop_reason$code, "max_metric_calls")
  expect_length(optimizer$all_generations[[1]]$population, 0L)
})

test_that("GEPA restores the caller RNG state", {
  testthat::local_mocked_bindings(
    eval_program = function(...) {
      dsprrr:::EvalResult(
        examples = data.frame(
          row_id = 1L,
          score = 1,
          error = NA_character_,
          predicted = I(list("answer")),
          feedback = NA_character_
        ),
        mean_score = 1,
        n_evaluated = 1L,
        n_errors = 0L
      )
    },
    gepa_mutate_instruction = function(...) "Candidate",
    .package = "dsprrr"
  )
  withr::local_seed(892L)
  before <- get(".Random.seed", envir = globalenv())

  dsprrr:::compile_gepa(
    GEPA(
      metric = function(...) 1,
      population_size = 2L,
      generations = 1L,
      seed = 31L,
      verbose = FALSE
    ),
    module(signature("question -> answer")),
    data.frame(question = "q", answer = "a")
  )

  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("GEPA tunes both graph-visible RLM predictors", {
  skip_if_not_installed("callr")
  local_reset_cache()

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  expect_identical(runner$policy()$persistent, TRUE)
  program <- make_rlm_optimizer_program(runner)
  chat <- make_rlm_optimizer_chat()
  original_action <- program$generate_action$signature@instructions
  original_extract <- program$extract$signature@instructions

  run <- capture_rlm_optimizer_warnings(
    compile(
      program,
      GEPA(
        metric = rlm_optimizer_accuracy,
        population_size = 2L,
        generations = 1L,
        mutation_rate = 1,
        crossover_rate = 0,
        component_selector = "all",
        use_merge = FALSE,
        seed = 17L,
        verbose = FALSE
      ),
      data.frame(question = "inspect", answer = "yes"),
      .llm = chat
    )
  )
  compiled <- expect_only_rlm_fallback_warnings(run)

  expect_identical(
    compiled$config$optimizer$component_ids,
    c(
      "instructions::$/generate_action",
      "instructions::$/extract"
    )
  )
  expect_identical(
    compiled$generate_action$signature@instructions,
    "ACTION-TUNED"
  )
  expect_identical(
    compiled$extract$signature@instructions,
    "EXTRACT-TUNED"
  )
  expect_identical(
    program$generate_action$signature@instructions,
    original_action
  )
  expect_identical(program$extract$signature@instructions, original_extract)
  expect_equal(compiled$config$optimizer$best_scores[["quality"]], 1)
  expect_length(chat$optimizer_state$reflection_prompts, 2L)
})

test_that("GEPA reflection receives feedback from bounded RLM trajectories", {
  skip_if_not_installed("callr")
  local_reset_cache()

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  expect_identical(runner$policy()$persistent, TRUE)
  program <- make_rlm_optimizer_program(runner, max_output_chars = 96L)
  chat <- make_rlm_optimizer_chat()
  observed_outputs <- character()
  trace_metric <- metric_with_trace(
    function(prediction, expected, program_trace) {
      event <- program_trace$events[[length(program_trace$events)]]
      output <- event$history[[1L]]$output
      observed_outputs <<- c(observed_outputs, output)
      list(
        score = 0,
        feedback = paste0("BOUNDED-RLM-TRACE: ", output)
      )
    },
    field = "answer"
  )

  run <- capture_rlm_optimizer_warnings(
    compile(
      program,
      GEPA(
        metric = trace_metric,
        population_size = 2L,
        generations = 2L,
        mutation_rate = 1,
        crossover_rate = 0,
        component_selector = "round_robin",
        use_merge = FALSE,
        seed = 23L,
        verbose = FALSE
      ),
      data.frame(question = "inspect", answer = "yes"),
      .llm = chat
    )
  )
  expect_only_rlm_fallback_warnings(run)

  expect_gt(length(observed_outputs), 0L)
  expect_true(all(nchar(observed_outputs) <= program$max_output_chars))
  expect_true(all(grepl("TRACE_HEAD", observed_outputs, fixed = TRUE)))
  expect_true(all(grepl("TRACE_TAIL", observed_outputs, fixed = TRUE)))
  expect_true(any(grepl(
    "BOUNDED-RLM-TRACE:",
    chat$optimizer_state$reflection_prompts,
    fixed = TRUE
  )))
})
