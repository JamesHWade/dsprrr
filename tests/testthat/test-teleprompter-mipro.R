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

test_that("run_discrete_bo respects error limit", {
  candidates <- list(list(id = "test"))

  # Mock eval function that always fails
  failing_eval_fn <- function(candidate, eval_type, trial_idx) {
    stop("Simulated failure")
  }

  mock_control <- dsprrr:::optimizer_control(max_errors = 3L)

  expect_error_with_warnings(
    dsprrr:::run_discrete_bo(
      candidates = candidates,
      eval_fn = failing_eval_fn,
      control = mock_control,
      max_trials = 10,
      minibatch_size = 1,
      full_eval_every = 5
    ),
    warning_regexp = "Evaluation failed for trial",
    error_regexp = "Exceeded maximum errors"
  )
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
