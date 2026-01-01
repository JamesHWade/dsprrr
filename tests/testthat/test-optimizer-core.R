# Tests for optimizer core infrastructure

test_that("OptimizerControl creates with defaults", {
  ctrl <- OptimizerControl()

  expect_null(ctrl@seed)
  expect_null(ctrl@max_trials)
  expect_equal(ctrl@max_errors, 5L)
  expect_equal(ctrl@num_threads, 1L)
  expect_true(is.na(ctrl@progress))
  expect_null(ctrl@log_dir)
  expect_false(ctrl@verbose)
})

test_that("optimizer_control convenience function works", {
  ctrl <- optimizer_control(
    seed = 42L,
    max_trials = 100L,
    max_errors = 10L,
    num_threads = 4L,
    log_dir = "logs/"
  )

  expect_equal(ctrl@seed, 42)
  expect_equal(ctrl@max_trials, 100L)
  expect_equal(ctrl@max_errors, 10L)
  expect_equal(ctrl@num_threads, 4L)
  expect_equal(ctrl@log_dir, "logs/")
})

test_that("OptimizerControl validates inputs", {
  expect_error(
    OptimizerControl(seed = c(1, 2)),
    "single numeric"
  )

  expect_error(
    OptimizerControl(max_trials = -1),
    "positive integer"
  )

  expect_error(
    OptimizerControl(max_errors = -1L),
    "non-negative"
  )

  expect_error(
    OptimizerControl(num_threads = 0L),
    "at least 1"
  )
})

test_that("sample_dataset returns deterministic results with seed", {
  df <- tibble::tibble(x = 1:100, y = letters[1:100])

  sample1 <- sample_dataset(df, n = 10, seed = 42)
  sample2 <- sample_dataset(df, n = 10, seed = 42)

  expect_equal(sample1, sample2)
  expect_equal(nrow(sample1), 10)
})

test_that("sample_dataset returns different results without seed", {
  df <- tibble::tibble(x = 1:1000)

  sample1 <- sample_dataset(df, n = 10)
  sample2 <- sample_dataset(df, n = 10)

  # Very unlikely to be equal with random sampling
  expect_false(identical(sample1$x, sample2$x))
})

test_that("sample_dataset handles edge cases", {
  df <- tibble::tibble(x = 1:10)

  # n > nrow returns full dataset
  result <- sample_dataset(df, n = 20)
  expect_equal(nrow(result), 10)

  # n = NULL returns full dataset
  result <- sample_dataset(df, n = NULL)
  expect_equal(nrow(result), 10)

  # Empty dataset
  empty <- tibble::tibble(x = integer())
  result <- sample_dataset(empty, n = 5)
  expect_equal(nrow(result), 0)
})

test_that("split_dataset creates valid train/val split", {
  df <- tibble::tibble(x = 1:100)

  split <- split_dataset(df, prop = 0.8, seed = 42)

  expect_named(split, c("train", "val"))
  expect_equal(nrow(split$train), 80)
  expect_equal(nrow(split$val), 20)

  # No overlap
  expect_equal(
    length(intersect(split$train$x, split$val$x)),
    0
  )

  # Deterministic with seed
  split2 <- split_dataset(df, prop = 0.8, seed = 42)
  expect_equal(split$train, split2$train)
})

test_that("split_dataset validates prop", {
  df <- tibble::tibble(x = 1:10)

  expect_error(split_dataset(df, prop = 0), "between 0 and 1")
  expect_error(split_dataset(df, prop = 1), "between 0 and 1")
  expect_error(split_dataset(df, prop = -0.5), "between 0 and 1")
})

test_that("check_budget detects max_trials", {
  ctrl <- optimizer_control(max_trials = 10L)

  result <- check_budget(5, 0, ctrl)
  expect_false(result$should_stop)

  result <- check_budget(10, 0, ctrl)
  expect_true(result$should_stop)
  expect_match(result$reason, "max_trials")
})

test_that("check_budget detects max_errors", {
  ctrl <- optimizer_control(max_errors = 3L)

  result <- check_budget(0, 2, ctrl)
  expect_false(result$should_stop)

  result <- check_budget(0, 3, ctrl)
  expect_true(result$should_stop)
  expect_match(result$reason, "max_errors")
})

test_that("generate_trial_id creates unique IDs", {
  ids <- replicate(100, generate_trial_id())
  expect_equal(length(unique(ids)), 100)

  # Check format
  expect_match(ids[1], "^trial_\\d{8}_\\d{6}_[a-z0-9]{6}$")

  # Custom prefix
  id <- generate_trial_id(prefix = "my_opt")
  expect_match(id, "^my_opt_")
})

test_that("EvalResult creates correctly", {
  result <- EvalResult(
    mean_score = 0.85,
    std_error = 0.05,
    n_evaluated = 10L,
    n_errors = 2L,
    total_tokens = 1000L,
    total_cost = 0.05
  )

  expect_equal(result@mean_score, 0.85)
  expect_equal(result@n_evaluated, 10L)
  expect_equal(result@total_tokens, 1000L)
})

test_that("CostSummary accumulates correctly", {
  summary <- CostSummary()
  expect_equal(summary@total_tokens, 0L)

  cost1 <- list(tokens_in = 100L, tokens_out = 50L, total_tokens = 150L, total_cost = 0.01)
  summary <- update_cost_summary(summary, cost1)

  expect_equal(summary@total_tokens, 150L)
  expect_equal(summary@tokens_in, 100L)
  expect_equal(summary@n_calls, 1L)

  cost2 <- list(tokens_in = 200L, tokens_out = 100L, total_tokens = 300L, total_cost = 0.02)
  summary <- update_cost_summary(summary, cost2)

  expect_equal(summary@total_tokens, 450L)
  expect_equal(summary@total_cost, 0.03)
  expect_equal(summary@n_calls, 2L)
})

# Tests for Trial and TrialLog

test_that("Trial creates correctly", {
  trial <- Trial(
    trial_id = "test_123",
    optimizer_name = "BootstrapFewShot",
    params = list(k = 4, temp = 0.7),
    status = "pending"
  )

  expect_equal(trial@trial_id, "test_123")
  expect_equal(trial@optimizer_name, "BootstrapFewShot")
  expect_equal(trial@params$k, 4)
  expect_equal(trial@status, "pending")
})

test_that("Trial validates status", {
  expect_error(
    Trial(status = "invalid"),
    "must be one of"
  )
})

test_that("create_trial generates ID", {
  trial <- create_trial(
    optimizer_name = "TestOpt",
    params = list(a = 1)
  )

  expect_true(nzchar(trial@trial_id))
  expect_equal(trial@optimizer_name, "TestOpt")
  expect_equal(trial@status, "pending")
  expect_false(is.null(trial@start_time))
})

test_that("start_trial updates status", {
  trial <- create_trial("TestOpt")
  started <- start_trial(trial)

  expect_equal(started@status, "running")
  expect_false(is.null(started@start_time))
})

test_that("complete_trial updates with results", {
  trial <- create_trial("TestOpt")

  eval_result <- EvalResult(
    mean_score = 0.9,
    std_error = 0.02,
    n_evaluated = 50L,
    n_errors = 0L,
    total_tokens = 5000L,
    total_cost = 0.25,
    total_latency_ms = 10000
  )

  completed <- complete_trial(trial, eval_result)

  expect_equal(completed@status, "completed")
  expect_equal(completed@metric_summary$mean_score, 0.9)
  expect_equal(completed@cost_summary$total_tokens, 5000L)
  expect_false(is.null(completed@end_time))
})

test_that("fail_trial updates with error", {
  trial <- create_trial("TestOpt")
  failed <- fail_trial(trial, "Connection timeout")

  expect_equal(failed@status, "failed")
  expect_match(failed@notes, "Connection timeout")
})

test_that("TrialLog tracks trials", {
  log <- TrialLog$new("TestOptimizer")

  expect_equal(log$n_trials(), 0)

  trial1 <- create_trial("TestOptimizer", list(k = 1))
  trial1 <- complete_trial(trial1, EvalResult(mean_score = 0.7, n_evaluated = 10L))
  log$add_trial(trial1, persist = FALSE)

  trial2 <- create_trial("TestOptimizer", list(k = 2))
  trial2 <- complete_trial(trial2, EvalResult(mean_score = 0.9, n_evaluated = 10L))
  log$add_trial(trial2, persist = FALSE)

  expect_equal(log$n_trials(), 2)

  # Best trial
  best <- log$best_trial()
  expect_equal(best@metric_summary$mean_score, 0.9)

  # As tibble
  tbl <- log$as_tibble()
  expect_equal(nrow(tbl), 2)
  expect_true("mean_score" %in% names(tbl))
})

test_that("TrialLog summary works", {
  log <- TrialLog$new("TestOptimizer")

  trial1 <- create_trial("TestOptimizer")
  trial1 <- complete_trial(
    trial1,
    EvalResult(mean_score = 0.8, n_evaluated = 10L, total_tokens = 100L, total_cost = 0.01)
  )
  log$add_trial(trial1, persist = FALSE)

  trial2 <- create_trial("TestOptimizer")
  trial2 <- fail_trial(trial2, "Error")
  log$add_trial(trial2, persist = FALSE)

  summary <- log$summary()

  expect_equal(summary$n_trials, 2)
  expect_equal(summary$n_completed, 1)
  expect_equal(summary$n_failed, 1)
  expect_equal(summary$best_score, 0.8)
})

test_that("write_trials_jsonl and read_trials_jsonl roundtrip", {
  trials <- list(
    create_trial("Opt1", list(a = 1, b = "x")),
    create_trial("Opt2", list(c = 2))
  )

  # Complete first trial
  trials[[1]] <- complete_trial(
    trials[[1]],
    EvalResult(mean_score = 0.85, n_evaluated = 10L)
  )

  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)

  write_trials_jsonl(trials, tmp)
  expect_true(file.exists(tmp))

  # Read back
  read_trials <- read_trials_jsonl(tmp)

  expect_equal(length(read_trials), 2)
  expect_equal(read_trials[[1]]@optimizer_name, "Opt1")
  expect_equal(read_trials[[1]]@params$a, 1)
  expect_equal(read_trials[[1]]@metric_summary$mean_score, 0.85)
})

test_that("TrialLog persists to directory", {
  tmp_dir <- tempfile()
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  log <- TrialLog$new("TestOpt", log_dir = tmp_dir)

  trial <- create_trial("TestOpt", list(k = 4))
  trial <- complete_trial(trial, EvalResult(mean_score = 0.9, n_evaluated = 10L))
  log$add_trial(trial)

  # Check files created

  expect_true(file.exists(file.path(tmp_dir, "trials.jsonl")))
  expect_true(file.exists(file.path(tmp_dir, "metadata.json")))
  expect_true(file.exists(file.path(tmp_dir, "README.md")))

  # Load back
  loaded <- load_trial_log(tmp_dir)
  expect_equal(loaded$n_trials(), 1)
  expect_equal(loaded$best_trial()@metric_summary$mean_score, 0.9)
})

test_that("eval_program validates inputs", {
  expect_error(
    eval_program("not a module", tibble::tibble(), identity),
    "must be a DSPrrr Module"
  )

  sig <- signature("q -> a")
  mod <- module(sig, type = "predict")

  expect_error(
    eval_program(mod, "not a dataframe", identity),
    "must be a data frame"
  )

  expect_error(
    eval_program(mod, tibble::tibble(), "not a function"),
    "must be a function"
  )
})

test_that("eval_program handles empty dataset", {
  sig <- signature("q -> a")
  mod <- module(sig, type = "predict")

  result <- eval_program(
    mod,
    tibble::tibble(q = character(), a = character()),
    metric_exact_match()
  )

  expect_s3_class(result, "dsprrr::EvalResult")
  expect_true(is.na(result@mean_score))
  expect_equal(result@n_evaluated, 0L)
})

test_that("eval_program works with mock LLM", {
  # Create a mock LLM that returns predictable responses
  mock_llm <- list(
    chat_structured = function(prompt, type, ...) {
      # Return a simple answer
      list(a = "mocked")
    }
  )

  # Make it look like a Chat object
  class(mock_llm) <- c("Chat", class(mock_llm))

  sig <- signature("q -> a")
  mod <- module(sig, type = "predict")

  dataset <- tibble::tibble(
    q = c("Q1", "Q2"),
    a = c("mocked", "different")
  )

  # Skip if module internals don't work with mock
  skip_if_not(
    tryCatch(
      {
        mod$forward(list(q = "test"), .llm = mock_llm)
        TRUE
      },
      error = function(e) FALSE
    ),
    "Mock LLM not compatible with module"
  )

  result <- eval_program(
    mod,
    dataset,
    metric_exact_match(field = "a"),
    .llm = mock_llm
  )

  expect_s3_class(result, "dsprrr::EvalResult")
  expect_equal(result@n_evaluated + result@n_errors, 2L)
})

# Tests for RNG state restoration
test_that("sample_dataset restores RNG state", {
  df <- tibble::tibble(x = 1:100)

  # Set a known RNG state
  set.seed(123)
  before <- runif(1)

  # Sample with seed (should not affect outer state)
  set.seed(123)
  sample_dataset(df, n = 5, seed = 42)
  after <- runif(1)

  expect_equal(before, after)
})

test_that("split_dataset restores RNG state", {
  df <- tibble::tibble(x = 1:100)

  # Set a known RNG state
  set.seed(456)
  before <- runif(1)

  # Split with seed (should not affect outer state)
  set.seed(456)
  split_dataset(df, prop = 0.8, seed = 42)
  after <- runif(1)

  expect_equal(before, after)
})

# Tests for NA handling in cost accumulation
test_that("update_cost_summary handles NA in total_cost", {
  summary <- CostSummary()

  # Cost with NA total_cost
  cost_with_na <- list(
    tokens_in = 100L,
    tokens_out = 50L,
    total_tokens = 150L,
    total_cost = NA_real_
  )

  updated <- update_cost_summary(summary, cost_with_na)

  expect_equal(updated@total_tokens, 150L)
  expect_equal(updated@total_cost, 0)  # NA treated as 0
  expect_equal(updated@n_calls, 1L)
})

test_that("update_cost_summary handles NULL values in cost list", {
  summary <- CostSummary(total_tokens = 100L, total_cost = 0.01)

  # Cost with NULL values
  cost_with_nulls <- list(
    tokens_in = NULL,
    tokens_out = NULL,
    total_tokens = NULL,
    total_cost = NULL
  )

  updated <- update_cost_summary(summary, cost_with_nulls)

  # Should preserve existing values, add 0 from NULL
  expect_equal(updated@total_tokens, 100L)
  expect_equal(updated@total_cost, 0.01)
  expect_equal(updated@n_calls, 1L)
})

# Tests for JSONL parse error handling
test_that("read_trials_jsonl handles malformed JSON gracefully", {
  tmp <- tempfile(fileext = ".jsonl")
  on.exit(unlink(tmp), add = TRUE)

  # Write some valid and invalid lines
  lines <- c(
    '{"trial_id": "valid_1", "optimizer_name": "Test", "status": "completed"}',
    'not valid json at all',
    '{"trial_id": "valid_2", "optimizer_name": "Test", "status": "pending"}'
  )
  writeLines(lines, tmp)

  # Should warn about the invalid line but still return valid trials
  expect_warning(
    trials <- read_trials_jsonl(tmp),
    "Failed to parse trial on line 2"
  )

  expect_equal(length(trials), 2)
  expect_equal(trials[[1]]@trial_id, "valid_1")
  expect_equal(trials[[2]]@trial_id, "valid_2")
})
