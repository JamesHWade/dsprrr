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
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig, type = "predict")
  trainset <- data.frame(question = "Q1", answer = "A1")
  valset <- data.frame(question = "Q2", answer = "A2")

  tp <- BootstrapFewShotWithRandomSearch()
  expect_error(
    compile(tp, mod, trainset, valset = valset),
    "requires a metric"
  )
})

test_that("BootstrapFewShotWithRandomSearch requires valset", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig, type = "predict")
  trainset <- data.frame(question = "Q1", answer = "A1")

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, exp) 1.0
  )
  expect_error(
    compile(tp, mod, trainset),
    "requires a validation set"
  )
})

test_that("BootstrapFewShotWithRandomSearch handles empty trainset", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig, type = "predict")
  empty_trainset <- data.frame(question = character(), answer = character())
  valset <- data.frame(question = "Q1", answer = "A1")

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, exp) 1.0
  )

  expect_warning(
    result <- compile(tp, mod, empty_trainset, valset = valset),
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
  # Create a mock module that returns predictable outputs
  MockRSModule <- R6::R6Class(
    "MockRSModule",
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
        # Return better predictions when we have demos
        n_demos <- length(self$demos)
        output_val <- if (n_demos > 0) "correct" else "wrong"
        tibble::tibble(
          output = list(output_val),
          chat = list(NULL),
          metadata = list(list())
        )
      }
      # No need to override deepcopy - parent's clone-based impl preserves subclass
    )
  )

  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- MockRSModule$new(signature = sig, template = "{question}")

  trainset <- data.frame(
    question = c("Q1", "Q2", "Q3", "Q4"),
    answer = c("correct", "correct", "correct", "correct")
  )

  valset <- data.frame(
    question = c("VQ1", "VQ2"),
    answer = c("correct", "correct")
  )

  # Metric that returns 1.0 for "correct"
  exact_metric <- function(pred, row) {
    as.numeric(pred == row$answer)
  }

  tp <- BootstrapFewShotWithRandomSearch(
    metric = exact_metric,
    num_candidate_programs = 4L, # baseline + labeled + unshuffled + 1 random
    max_labeled_demos = 2L,
    max_bootstrapped_demos = 2L,
    seed = 42L
  )

  result <- compile(tp, mod, trainset, valset = valset)

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
  MockEarlyStopModule <- R6::R6Class(
    "MockEarlyStopModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      call_count = 0,
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
        self$call_count <- self$call_count + 1
        # Always return correct
        tibble::tibble(
          output = list("correct"),
          chat = list(NULL),
          metadata = list(list())
        )
      }
      # No need to override deepcopy - parent's clone-based impl preserves subclass
    )
  )

  sig <- Signature(
    inputs = list(input(name = "x", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  mod <- MockEarlyStopModule$new(signature = sig)

  trainset <- data.frame(x = c("a", "b", "c", "d"), y = c("1", "2", "3", "4"))
  valset <- data.frame(x = c("e", "f"), y = c("correct", "correct"))

  tp <- BootstrapFewShotWithRandomSearch(
    # Always return 1.0 so early stopping triggers on first candidate
    metric = function(pred, row) 1.0,
    num_candidate_programs = 10L, # Would normally try 10
    stop_at_score = 0.9, # But should stop early since metric returns 1.0
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L
  )

  result <- compile(tp, mod, trainset, valset = valset)

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

test_that("compile_module works with BootstrapFewShotWithRandomSearch", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Summarize"
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    text = c("Hello world", "Goodbye world"),
    summary = c("greeting", "farewell")
  )

  valset <- data.frame(
    text = c("Test text"),
    summary = c("test")
  )

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(summary = "mocked")
      }
    ),
    class = "Chat"
  )

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, exp) 0.5,
    num_candidate_programs = 3L,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L
  )

  result <- compile_module(mod, tp, trainset, valset = valset, .llm = mock_llm)

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
  # Create a module that fails on certain operations
  FailingCandidateModule <- R6::R6Class(
    "FailingCandidateModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      should_fail = FALSE,
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
        if (self$should_fail) {
          stop("Intentional forward failure")
        }
        tibble::tibble(
          output = list("ok"),
          chat = list(NULL),
          metadata = list(list())
        )
      }
      # No need to override deepcopy - parent's clone-based impl preserves
      # subclass and all public fields (including should_fail)
    )
  )

  sig <- Signature(
    inputs = list(input(name = "x", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  mod <- FailingCandidateModule$new(signature = sig)

  trainset <- data.frame(x = c("a", "b"), y = c("1", "2"))
  valset <- data.frame(x = c("c"), y = c("ok"))

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, row) as.numeric(pred == row$y),
    num_candidate_programs = 3L,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L
  )

  # Should complete even if some candidates fail
  result <- compile(tp, mod, trainset, valset = valset)

  expect_true(inherits(result, "Module"))
  expect_true(result$config$compiled)
})

test_that("BootstrapFewShotWithRandomSearch errors when all candidates fail", {
  # Create a module that always fails
  AlwaysFailModule <- R6::R6Class(
    "AlwaysFailModule",
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
        stop("Module always fails")
      }
      # No need to override deepcopy - parent's clone-based impl preserves subclass
    )
  )

  sig <- Signature(
    inputs = list(input(name = "x", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  mod <- AlwaysFailModule$new(signature = sig)

  trainset <- data.frame(x = c("a", "b"), y = c("1", "2"))
  valset <- data.frame(x = c("c"), y = c("ok"))

  tp <- BootstrapFewShotWithRandomSearch(
    metric = function(pred, row) as.numeric(pred == row$y),
    num_candidate_programs = 3L,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L
  )

  # Should error when all candidates fail
  expect_error(
    compile(tp, mod, trainset, valset = valset),
    "All .* candidate programs failed"
  )
})
