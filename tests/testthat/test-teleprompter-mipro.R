# Tests for MIPROv2 teleprompter

test_that("MIPROv2 can be created with defaults", {
  tp <- MIPROv2()
  expect_s3_class(tp, "dsprrr::MIPROv2")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(tp@auto, "light")
  expect_equal(tp@max_bootstrapped_demos, 4L)
  expect_equal(tp@max_labeled_demos, 4L)
  expect_equal(tp@seed, 9L)
  expect_true(tp@track_stats)
})

test_that("MIPROv2 validates properties", {
  expect_error(
    MIPROv2(auto = "fast"),
    "auto must be NULL"
  )

  expect_error(
    MIPROv2(num_candidates = 0),
    "positive integer"
  )

  expect_error(
    MIPROv2(init_temperature = 0),
    "positive numeric"
  )
})

test_that("MIPROv2 runs end-to-end with auto=light", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(sig, type = "predict")

  trainset <- data.frame(
    question = c(
      "What is 2+2?",
      "What is 3+3?",
      "What is 4+4?",
      "What is 5+5?"
    ),
    answer = c("4", "6", "8", "10")
  )

  # Create a mock LLM that returns predictable results based on input
  call_count <- 0L
  mock_llm <- local({
    self <- structure(
      list(
        chat_structured = function(prompt, type, ...) {
          call_count <<- call_count + 1L
          # Extract the question from the prompt and return matching answer
          if (grepl("2\\+2|2 \\+ 2", prompt)) {
            list(answer = "4")
          } else if (grepl("3\\+3|3 \\+ 3", prompt)) {
            list(answer = "6")
          } else if (grepl("4\\+4|4 \\+ 4", prompt)) {
            list(answer = "8")
          } else if (grepl("5\\+5|5 \\+ 5", prompt)) {
            list(answer = "10")
          } else {
            list(answer = "unknown")
          }
        },
        clone = function(...) self,
        set_turns = function(turns) invisible(NULL)
      ),
      class = "Chat"
    )
    self
  })

  log_dir <- tempfile("mipro-log-")
  dir.create(log_dir)

  tp <- MIPROv2(
    metric = metric_exact_match(field = "answer"),
    auto = "light",
    max_bootstrapped_demos = 2L,
    max_labeled_demos = 2L,
    seed = 1L,
    log_dir = log_dir
  )

  compiled <- compile(tp, mod, trainset, valset = trainset, .llm = mock_llm)

  expect_true(compiled$config$compiled)
  expect_equal(compiled$config$teleprompter, "MIPROv2")
  expect_true(length(compiled$demos) > 0)

  optimizer <- compiled$config$optimizer
  expect_true(length(optimizer$demo_candidates) > 0)
  expect_true(length(optimizer$instruction_candidates) > 0)
  expect_s3_class(optimizer$trial_history, "tbl_df")
  expect_true(any(optimizer$trial_history$eval_type == "full"))
  expect_false(optimizer$budget_summary$stopped)
  expect_null(optimizer$stop_reason)
  expect_equal(optimizer$error_count, 0L)

  expect_true(file.exists(file.path(log_dir, "trials.jsonl")))
})

test_that("MIPROv2 requires metric for compilation", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(sig, type = "predict")
  trainset <- data.frame(question = "test", answer = "test")

  tp <- MIPROv2(metric = NULL)
  expect_error(
    compile(tp, mod, trainset),
    "requires a metric"
  )
})

test_that("MIPROv2 print method works", {
  tp <- MIPROv2(
    metric = function(x, y) 1.0,
    auto = "medium",
    max_bootstrapped_demos = 6L,
    seed = 42L
  )

  expect_invisible(print(tp))
  expect_identical(print(tp), tp)
})

test_that("MIPROv2 validates additional properties", {
  expect_error(MIPROv2(max_bootstrapped_demos = -1L), "non-negative")
  expect_error(MIPROv2(max_labeled_demos = -1L), "non-negative")
  expect_error(MIPROv2(num_threads = 0L), "at least 1")
  expect_error(MIPROv2(seed = c(1, 2)), "single numeric")
  expect_error(MIPROv2(track_stats = c(TRUE, FALSE)), "single logical")
  expect_error(MIPROv2(log_dir = 123), "character string")
})

# Tests for resolve_mipro_settings
test_that("resolve_mipro_settings returns correct light settings", {
  settings <- dsprrr:::resolve_mipro_settings("light", NULL, 100)
  expect_equal(settings$trials, 20L)
  expect_equal(settings$minibatch_size, 5L)
  expect_equal(settings$full_eval_every, 5L)
  expect_equal(settings$demo_candidates, 3L)
  expect_equal(settings$instruction_candidates, 5L)
})

test_that("resolve_mipro_settings returns correct medium settings", {
  settings <- dsprrr:::resolve_mipro_settings("medium", NULL, 100)
  expect_equal(settings$trials, 50L)
  expect_equal(settings$minibatch_size, 10L)
  expect_equal(settings$full_eval_every, 10L)
  expect_equal(settings$demo_candidates, 5L)
  expect_equal(settings$instruction_candidates, 8L)
})

test_that("resolve_mipro_settings returns correct heavy settings", {
  settings <- dsprrr:::resolve_mipro_settings("heavy", NULL, 100)
  expect_equal(settings$trials, 100L)
  expect_equal(settings$minibatch_size, 20L)
  expect_equal(settings$full_eval_every, 20L)
  expect_equal(settings$demo_candidates, 7L)
  expect_equal(settings$instruction_candidates, 12L)
})

test_that("resolve_mipro_settings respects num_candidates override", {
  settings <- dsprrr:::resolve_mipro_settings(NULL, 50, 100)
  expect_equal(settings$trials, 50)
  expect_equal(settings$instruction_candidates, 50)
})

test_that("resolve_mipro_settings caps minibatch_size to n_train", {
  settings <- dsprrr:::resolve_mipro_settings("light", NULL, 3)
  expect_equal(settings$minibatch_size, 3L)
})

# Tests for run_discrete_bo
test_that("run_discrete_bo validates inputs", {
  mock_control <- dsprrr:::optimizer_control()

  expect_error(
    dsprrr:::run_discrete_bo(
      candidates = list(),
      eval_fn = function(c, t, i) NULL,
      control = mock_control,
      max_trials = 5,
      minibatch_size = 2,
      full_eval_every = 2
    ),
    "non-empty list"
  )

  expect_error(
    dsprrr:::run_discrete_bo(
      candidates = list(list(id = "a")),
      eval_fn = "not_a_function",
      control = mock_control,
      max_trials = 5,
      minibatch_size = 2,
      full_eval_every = 2
    ),
    "must be a function"
  )

  expect_error(
    dsprrr:::run_discrete_bo(
      candidates = list(list(id = "a")),
      eval_fn = function(c, t, i) NULL,
      control = mock_control,
      max_trials = 0,
      minibatch_size = 2,
      full_eval_every = 2
    ),
    "positive integer"
  )
})

test_that("run_discrete_bo selects best candidate", {
  # Create mock candidates with different scores
  candidates <- list(
    list(id = "good", demos = list("demo1")),
    list(id = "bad", demos = list("demo2"))
  )

  # Mock eval function that returns higher score for "good" candidate
  mock_eval_fn <- function(candidate, eval_type, trial_idx) {
    score <- if (candidate$id == "good") 0.9 else 0.3
    dsprrr:::EvalResult(
      mean_score = score,
      std_error = 0.05,
      n_evaluated = 1L,
      n_errors = 0L
    )
  }

  mock_control <- dsprrr:::optimizer_control()

  result <- dsprrr:::run_discrete_bo(
    candidates = candidates,
    eval_fn = mock_eval_fn,
    control = mock_control,
    max_trials = 10,
    minibatch_size = 1,
    full_eval_every = 5,
    seed = 42,
    track_stats = TRUE
  )

  expect_equal(result$best_candidate$id, "good")
  expect_s3_class(result$trial_history, "tbl_df")
  expect_true(nrow(result$trial_history) == 10)
})

test_that("run_discrete_bo does not descend past a completed resume cursor", {
  candidates <- list(list(id = "candidate"))
  resume_state <- dsprrr:::discrete_bo_restore_state(NULL, candidates)
  resume_state$next_trial <- 3L
  calls <- integer()

  result <- dsprrr:::run_discrete_bo(
    candidates = candidates,
    eval_fn = function(candidate, eval_type, trial_idx) {
      calls <<- c(calls, trial_idx)
      dsprrr:::EvalResult(mean_score = 1, n_evaluated = 1L)
    },
    control = dsprrr:::optimizer_control(),
    max_trials = 2L,
    minibatch_size = 1L,
    full_eval_every = 2L,
    resume_state = resume_state
  )

  expect_identical(calls, integer())
  expect_identical(result$resume_state$next_trial, 3L)
  expect_identical(result$complete, TRUE)
  expect_identical(result$budget_summary$trials, 0L)
})

test_that("run_discrete_bo respects error limit", {
  candidates <- list(list(id = "test"))

  # Mock eval function that always fails
  failing_eval_fn <- function(candidate, eval_type, trial_idx) {
    stop("Simulated failure")
  }

  mock_control <- dsprrr:::optimizer_control(max_errors = 3L)

  result <- expect_test_warnings(
    dsprrr:::run_discrete_bo(
      candidates = candidates,
      eval_fn = failing_eval_fn,
      control = mock_control,
      max_trials = 10,
      minibatch_size = 1,
      full_eval_every = 5
    ),
    "Evaluation failed for trial"
  )

  expect_null(result$best_candidate)
  expect_equal(nrow(result$trial_history), 3L)
  expect_equal(result$error_count, 3L)
  expect_true(result$budget_summary$stopped)
  expect_s3_class(result$stop_reason, "dsprrr_optimizer_stop_reason")
  expect_identical(result$stop_reason$stage, "discrete_bo_minibatch")
  expect_equal(result$stop_reason$observed, 3L)
})

test_that("run_discrete_bo keeps the best partial candidate at exhaustion", {
  candidates <- list(list(id = "best"))
  eval_fn <- function(candidate, eval_type, trial_idx) {
    if (trial_idx > 1L) {
      stop("later evaluation failed")
    }

    dsprrr:::EvalResult(
      mean_score = 0.8,
      n_evaluated = 1L,
      n_errors = 0L
    )
  }

  result <- expect_test_warnings(
    dsprrr:::run_discrete_bo(
      candidates = candidates,
      eval_fn = eval_fn,
      control = dsprrr:::optimizer_control(max_errors = 2L),
      max_trials = 10L,
      minibatch_size = 1L,
      full_eval_every = 1L
    ),
    "Evaluation failed for trial"
  )

  expect_identical(result$best_candidate$id, "best")
  expect_equal(nrow(result$trial_history), 3L)
  expect_equal(result$error_count, 2L)
  expect_equal(result$budget_summary$attempts, 3L)
  expect_equal(result$budget_summary$successes, 1L)
  expect_equal(result$stop_reason$observed, 2L)
})

test_that("run_discrete_bo counts ordered EvalResult row errors", {
  candidates <- list(list(id = "partial"))
  eval_fn <- function(candidate, eval_type, trial_idx) {
    dsprrr:::EvalResult(
      examples = data.frame(error = c("first", "second")),
      mean_score = 0.6,
      n_evaluated = 0L,
      n_errors = 2L
    )
  }

  result <- dsprrr:::run_discrete_bo(
    candidates = candidates,
    eval_fn = eval_fn,
    control = dsprrr:::optimizer_control(max_errors = 2L),
    max_trials = 10L,
    minibatch_size = 1L,
    full_eval_every = 1L
  )

  expect_identical(result$best_candidate$id, "partial")
  expect_equal(nrow(result$trial_history), 1L)
  expect_equal(result$error_count, 2L)
  expect_true(result$budget_summary$stopped)
  expect_identical(result$stop_reason$condition_class, "simpleError")
})

test_that("run_discrete_bo lets max_errors zero attempt once", {
  calls <- 0L
  result <- expect_test_warnings(
    dsprrr:::run_discrete_bo(
      candidates = list(list(id = "candidate")),
      eval_fn = function(candidate, eval_type, trial_idx) {
        calls <<- calls + 1L
        stop("first failure")
      },
      control = dsprrr:::optimizer_control(max_errors = 0L),
      max_trials = 10L,
      minibatch_size = 1L,
      full_eval_every = 10L
    ),
    "Evaluation failed for trial"
  )

  expect_equal(calls, 1L)
  expect_equal(result$error_count, 1L)
  expect_equal(result$stop_reason$limit, 0L)
  expect_equal(result$stop_reason$observed, 1L)
})

test_that("run_discrete_bo keeps authentication and configuration fatal", {
  run_failure <- function(eval_fn) {
    dsprrr:::run_discrete_bo(
      candidates = list(list(id = "candidate")),
      eval_fn = eval_fn,
      control = dsprrr:::optimizer_control(max_errors = 2L),
      max_trials = 2L,
      minibatch_size = 1L,
      full_eval_every = 1L
    )
  }

  expect_error(
    run_failure(function(...) stop("Unauthorized API key")),
    class = "dsprrr_mipro_auth_error"
  )
  expect_error(
    run_failure(function(...) {
      cli::cli_abort(
        "Invalid optimizer configuration",
        class = "dsprrr_test_config_error"
      )
    }),
    class = "dsprrr_test_config_error"
  )
})

test_that("MIPROv2 propagates a typed budget stop into metadata", {
  budget <- dsprrr:::new_optimizer_budget(
    dsprrr:::optimizer_control(max_errors = 1L)
  )
  dsprrr:::record_optimizer_outcome(
    budget,
    FALSE,
    "discrete_bo_minibatch",
    simpleError("evaluation failed")
  )
  budget_summary <- dsprrr:::optimizer_budget_summary(budget)

  testthat::local_mocked_bindings(
    generate_mipro_demo_candidates = function(...) {
      list(list(id = "demo", demos = list(), params = list(id = "demo")))
    },
    generate_mipro_instruction_candidates = function(...) {
      list(list(
        id = "instruction",
        instructions = "Use the input.",
        params = list(id = "instruction")
      ))
    },
    run_discrete_bo = function(candidates, ...) {
      list(
        best_candidate = candidates[[1L]],
        trial_history = tibble::tibble(),
        candidate_stats = list(),
        budget_summary = budget_summary,
        stop_reason = budget_summary$stop_reason,
        error_count = budget_summary$total_errors
      )
    },
    .package = "dsprrr"
  )

  program <- module(signature("question -> answer"), type = "predict")
  teleprompter <- MIPROv2(metric = function(...) 1)
  compiled <- dsprrr:::compile_mipro(
    teleprompter,
    program,
    data.frame(question = "test", answer = "test")
  )

  metadata <- compiled$config$optimizer
  expect_identical(metadata$budget_summary, budget_summary)
  expect_identical(metadata$stop_reason, budget_summary$stop_reason)
  expect_equal(metadata$error_count, 1L)
})

test_that("run_discrete_bo UCB explores untried candidates first", {
  candidates <- list(
    list(id = "a"),
    list(id = "b"),
    list(id = "c")
  )

  visited <- character()

  # Track which candidates are visited
  tracking_eval_fn <- function(candidate, eval_type, trial_idx) {
    visited <<- c(visited, candidate$id)
    dsprrr:::EvalResult(
      mean_score = 0.5,
      std_error = 0.1,
      n_evaluated = 1L,
      n_errors = 0L
    )
  }

  mock_control <- dsprrr:::optimizer_control()

  result <- expect_test_warnings(
    dsprrr:::run_discrete_bo(
      candidates = candidates,
      eval_fn = tracking_eval_fn,
      control = mock_control,
      max_trials = 3,
      minibatch_size = 1,
      full_eval_every = 10,
      seed = 123
    ),
    "No candidate received full evaluation"
  )

  # First 3 trials should visit all candidates (exploration)
  expect_true(all(c("a", "b", "c") %in% visited[1:3]))
})
