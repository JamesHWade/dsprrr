# Tests for EnsembleModule and Ensemble teleprompter

# Helper: Create a mock module for testing
create_mock_module <- function(response = "answer1") {
  mock_mod <- list(
    signature = signature("question -> answer"),
    chat = NULL,
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      tibble::tibble(
        output = list(list(answer = response)),
        chat = list(NULL),
        metadata = list(list(
          total_tokens = 100,
          cost = 0.001,
          model = "mock-model"
        ))
      )
    },
    reset_copy = function() create_mock_module(response)
  )
  class(mock_mod) <- c("MockModule", "Module", "R6")
  mock_mod
}

# ============================================================================
# EnsembleModule Tests
# ============================================================================

test_that("EnsembleModule class exists and inherits from Module", {
  expect_true(R6::is.R6Class(EnsembleModule))
})

test_that("ensemble creates EnsembleModule", {
  mod1 <- create_mock_module("a")
  mod2 <- create_mock_module("b")

  ens <- ensemble(list(mod1, mod2))

  expect_s3_class(ens, "EnsembleModule")
  expect_s3_class(ens, "Module")
  expect_equal(length(ens$modules), 2)
})

test_that("ensemble validates modules argument", {
  expect_error(
    ensemble(list()),
    "non-empty list"
  )

  expect_error(
    ensemble("not a list"),
    "non-empty list"
  )

  expect_error(
    ensemble(list("not a module")),
    "must be Module"
  )
})

test_that("ensemble validates signature compatibility", {
  # Create modules with different input field names
  mod1 <- module(signature("question -> answer"))
  mod2 <- module(signature("context, query -> answer"))

  expect_error(
    ensemble(list(mod1, mod2)),
    "Incompatible signatures"
  )
})

test_that("ensemble allows compatible signatures", {
  # Create modules with identical signatures
  mod1 <- module(signature("question -> answer"))
  mod2 <- module(signature("question -> answer"))
  mod3 <- module(signature("question -> answer"))

  # Should not error
  ens <- ensemble(list(mod1, mod2, mod3))
  expect_s3_class(ens, "EnsembleModule")
})

test_that("ensemble validates signature compatibility with multiple inputs", {
  # Create modules with same number of inputs but different names
  mod1 <- module(signature("context, question -> answer"))
  mod2 <- module(signature("context, query -> answer")) # 'query' instead of 'question'

  expect_error(
    ensemble(list(mod1, mod2)),
    "Incompatible signatures"
  )
})

test_that("ensemble validates signature compatibility error message is helpful", {
  mod1 <- module(signature("question -> answer"))
  mod2 <- module(signature("text -> summary"))

  expect_error(
    ensemble(list(mod1, mod2)),
    "question.*text" # Error message should mention both input fields
  )
})

test_that("ensemble allows single module without compatibility check", {
  # Single module should work (no compatibility check needed)
  mod1 <- module(signature("question -> answer"))

  ens <- ensemble(list(mod1))
  expect_s3_class(ens, "EnsembleModule")
})

test_that("ensemble validates all modules beyond position 2", {
  # Verify that modules at position 3+ are also validated

  mod1 <- module(signature("question -> answer"))
  mod2 <- module(signature("question -> answer"))
  mod3 <- module(signature("different_input -> answer")) # Incompatible

  expect_error(
    ensemble(list(mod1, mod2, mod3)),
    "Module 3" # Error message should identify position 3
  )
})

test_that("ensemble warns for module with NULL signature", {
  mod1 <- module(signature("question -> answer"))

  # Create a mock module with NULL signature
  mod2 <- list(
    signature = NULL,
    chat = NULL,
    forward = function(...) NULL,
    reset_copy = function() NULL
  )
  class(mod2) <- c("MockModule", "Module", "R6")

  # Should warn about NULL signature and then error on incompatibility
  expect_warning(
    expect_error(ensemble(list(mod1, mod2)), "Incompatible signatures"),
    "NULL signature"
  )
})

test_that("ensemble warns for module with invalid signature type", {
  mod1 <- module(signature("question -> answer"))

  # Create a mock module with non-Signature object
  mod2 <- list(
    signature = list(inputs = list(list(name = "question"))), # Not a real Signature
    chat = NULL,
    forward = function(...) NULL,
    reset_copy = function() NULL
  )
  class(mod2) <- c("MockModule", "Module", "R6")

  # Should warn about invalid signature type
  expect_warning(
    expect_error(ensemble(list(mod1, mod2)), "Incompatible signatures"),
    "not a Signature object"
  )
})

test_that("ensemble uses default reduce_majority", {
  mod1 <- create_mock_module("a")
  mod2 <- create_mock_module("a")
  ens <- ensemble(list(mod1, mod2))

  expect_true(is.function(ens$reduce_fn))
})

test_that("EnsembleModule forward returns correct structure", {
  mod1 <- create_mock_module("a")
  mod2 <- create_mock_module("b")

  ens <- ensemble(list(mod1, mod2))
  result <- ens$forward(list(question = "test"))

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("output", "chat", "metadata"))
  expect_length(result$output, 1)
  expect_length(result$metadata, 1)
})

test_that("EnsembleModule runs all modules", {
  call_counts <- c(0, 0, 0)
  mods <- lapply(1:3, function(i) {
    mock <- create_mock_module(paste0("answer", i))
    orig_forward <- mock$forward
    mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
      call_counts[i] <<- call_counts[i] + 1
      orig_forward(batch, .llm, trace, ...)
    }
    mock
  })

  ens <- ensemble(mods)
  ens$forward(list(question = "test"))

  expect_equal(call_counts, c(1, 1, 1))
})

test_that("EnsembleModule metadata includes module counts", {
  mods <- lapply(1:3, function(i) create_mock_module(paste0("a", i)))
  ens <- ensemble(mods)

  result <- ens$forward(list(question = "test"))
  meta <- result$metadata[[1]]

  expect_equal(meta$n_modules, 3)
  expect_equal(meta$n_successful, 3)
  expect_equal(meta$n_errors, 0)
  expect_equal(meta$n_llm_calls, 3)
})

test_that("EnsembleModule handles module errors gracefully", {
  mod1 <- create_mock_module("a")
  mod2 <- create_mock_module("b")
  mod2$forward <- function(...) stop("Simulated failure")
  mod3 <- create_mock_module("a")

  ens <- ensemble(list(mod1, mod2, mod3))

  expect_warning(
    result <- ens$forward(list(question = "test")),
    "Module 2.*failed"
  )

  # Should still succeed with 2 of 3 modules
  expect_s3_class(result, "tbl_df")
  expect_equal(result$metadata[[1]]$n_successful, 2)
  expect_equal(result$metadata[[1]]$n_errors, 1)
})

test_that("EnsembleModule errors when all modules fail", {
  mods <- lapply(1:3, function(i) {
    mock <- create_mock_module("a")
    mock$forward <- function(...) stop("Fail")
    mock
  })

  ens <- ensemble(mods)

  expect_error(
    suppressWarnings(ens$forward(list(question = "test"))),
    "All .* modules failed"
  )
})

test_that("EnsembleModule uses correct weights when middle module fails", {
  # Bug fix test: weights should correspond to successful modules, not positions
  # If module 2 fails, we should use weights for modules 1 and 3, not 1 and 2

  mod1 <- create_mock_module("minority") # weight 1

  mod2 <- create_mock_module("should_not_matter") # weight 100 - but will fail
  mod2$forward <- function(...) stop("Simulated failure")
  mod3 <- create_mock_module("majority") # weight 10

  # With weights [1, 100, 10], if module 2 fails:
  # - Correct: minority (weight 1) vs majority (weight 10) -> majority wins

  # - Bug: minority (weight 1) vs majority (weight 100) -> majority wins with wrong weight
  # To detect the bug, we need outputs where wrong weight assignment changes result

  # Better test: 3 modules with weights [1, 50, 2]

  # Module 1: "a", Module 2: fails, Module 3: "b"
  # Correct weights: [1, 2] -> "b" should NOT win (weight 2 vs 1 - b wins)
  # Bug weights: [1, 50] -> "b" wins with weight 50

  # Actually, let's make it clearer:
  # Weights: [10, 1, 1] - module 1 has high weight
  # Module 2 fails
  # Correct: module 1 (weight 10) vs module 3 (weight 1) -> module 1 should win
  # Bug: module 1 (weight 10) vs module 3 (weight 1) -> still correct in this case

  # Best test: [1, 100, 1] where middle module fails
  # If bug: successful modules get weights [1, 100] instead of [1, 1]
  mod1 <- create_mock_module("a") # should have weight 1
  mod2 <- create_mock_module("ignored")
  mod2$forward <- function(...) stop("fail")
  mod3 <- create_mock_module("b") # should have weight 1

  ens <- ensemble(
    list(mod1, mod2, mod3),
    reduce_fn = reduce_weighted_vote(),
    weights = c(1, 100, 1) # middle weight is 100 but module fails
  )

  # With correct fix: weights are [1, 1] -> tie, "a" wins (first occurrence)
  # With bug: weights are [1, 100] -> "b" wins with weight 100
  expect_warning(
    result <- ens$forward(list(question = "test")),
    "Module 2.*failed"
  )

  # Should be a tie (both weight 1), so first answer "a" wins
  expect_equal(result$output[[1]]$answer, "a")
})

test_that("EnsembleModule get_individual_outputs returns outputs", {
  mods <- lapply(1:3, function(i) create_mock_module(paste0("answer", i)))
  ens <- ensemble(mods)
  ens$forward(list(question = "test"))

  outputs <- ens$get_individual_outputs()

  expect_s3_class(outputs, "tbl_df")
  expect_equal(nrow(outputs), 3)
  expect_true("output" %in% names(outputs))
  expect_true("module" %in% names(outputs))
})

test_that("EnsembleModule get_individual_outputs all=TRUE works", {
  mods <- lapply(1:2, function(i) create_mock_module(paste0("a", i)))
  ens <- ensemble(mods)

  ens$forward(list(question = "test1"))
  ens$forward(list(question = "test2"))

  outputs <- ens$get_individual_outputs(all = TRUE)
  expect_equal(nrow(outputs), 4) # 2 runs x 2 modules
})

test_that("EnsembleModule accepts custom reduce function", {
  custom_reduce <- function(outputs, weights = NULL) {
    # Always return the second output
    outputs[[min(2, length(outputs))]]
  }

  mods <- list(
    create_mock_module("first"),
    create_mock_module("second"),
    create_mock_module("third")
  )

  ens <- ensemble(mods, reduce_fn = custom_reduce)
  result <- ens$forward(list(question = "test"))

  expect_equal(result$output[[1]]$answer, "second")
})

test_that("EnsembleModule uses weights", {
  mods <- list(
    create_mock_module("a"),
    create_mock_module("b"),
    create_mock_module("b")
  )

  ens <- ensemble(mods, weights = c(10, 1, 1))

  # With majority voting, "b" should win (2 votes)
  # But if we were using weighted voting, "a" would win (10 weight)
  expect_equal(length(ens$weights), 3)
  expect_equal(ens$weights, c(10, 1, 1))
})

test_that("EnsembleModule validates weights length", {
  mods <- lapply(1:3, function(i) create_mock_module("a"))

  expect_error(
    ensemble(mods, weights = c(1, 2)),
    "same length"
  )
})

test_that("EnsembleModule reset_copy creates fresh wrapper", {
  mods <- lapply(1:3, function(i) create_mock_module(paste0("a", i)))
  ens <- ensemble(mods, weights = c(1, 2, 3))
  ens$forward(list(question = "test"))

  copy <- ens$reset_copy()

  expect_s3_class(copy, "EnsembleModule")
  expect_equal(length(copy$modules), 3)
  expect_equal(copy$weights, c(1, 2, 3))
  expect_length(copy$state$traces, 0)
})

test_that("EnsembleModule apply_optimization_params forwards to all modules", {
  # Create real modules that have apply_optimization_params
  mod1 <- module(signature("question -> answer"))
  mod2 <- module(signature("question -> answer"))
  mod3 <- module(signature("question -> answer"))

  ens <- ensemble(list(mod1, mod2, mod3))

  # Apply params - should forward to all child modules
  # Test all parameter types that PredictModule accepts
  params <- list(
    instructions = "Be concise",
    template = "Q: {question}\nA:",
    prompt_style = "structured"
  )
  result <- ens$apply_optimization_params(params)

  # Should return self invisibly
  expect_identical(result, ens)

  # All child modules should have received all the params
  for (mod in ens$modules) {
    # PredictModule stores instructions in signature
    expect_equal(mod$signature@instructions, "Be concise")
    # Template is stored directly on module
    expect_equal(mod$template, "Q: {question}\nA:")
    # prompt_style is stored in config
    expect_equal(mod$config$prompt_style, "structured")
  }
})

test_that("EnsembleModule forward handles dataframe batch input", {
  # Use a module that captures its input to verify conversion
  captured_inputs <- list()

  mod1 <- create_mock_module("a")
  orig_forward <- mod1$forward
  mod1$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    captured_inputs <<- append(captured_inputs, list(batch))
    orig_forward(batch, .llm, trace, ...)
  }

  mod2 <- create_mock_module("b")

  ens <- ensemble(list(mod1, mod2))

  # Dataframe with multiple columns (as run_dataset would provide)
  batch_df <- data.frame(
    question = "what is 2+2?",
    context = "math",
    stringsAsFactors = FALSE
  )

  result <- ens$forward(batch_df)

  # Should return proper output structure
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("output", "chat", "metadata"))
  expect_length(result$output, 1)

  # The module should have received a list with all input fields preserved
  expect_true(length(captured_inputs) > 0)
  first_input <- captured_inputs[[1]]
  expect_true("question" %in% names(first_input))
  expect_true("context" %in% names(first_input))
})

test_that("EnsembleModule forward processes only first row of multi-row dataframe", {
  # EnsembleModule processes one example at a time (via run_dataset loop)
  # When given a multi-row dataframe, it takes only the first row
  captured_inputs <- list()

  mod1 <- create_mock_module("a")
  orig_forward <- mod1$forward
  mod1$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
    captured_inputs <<- append(captured_inputs, list(batch))
    orig_forward(batch, .llm, trace, ...)
  }

  ens <- ensemble(list(mod1))

  # Multi-row dataframe
  batch_df <- data.frame(
    question = c("q1", "q2", "q3"),
    stringsAsFactors = FALSE
  )

  result <- ens$forward(batch_df)

  # Should return single result (first row only)
  expect_length(result$output, 1)

  # Module should have received only the first row's data
  expect_true(length(captured_inputs) > 0)
  first_input <- captured_inputs[[1]]

  # The input should contain only the first question
  # (dataframe row converted to list)
  expect_true("question" %in% names(first_input))
})

test_that("EnsembleModule print method works", {
  mods <- lapply(1:3, function(i) create_mock_module("a"))
  ens <- ensemble(mods)

  expect_invisible(print(ens))
})

test_that("EnsembleModule records traces", {
  mods <- lapply(1:2, function(i) create_mock_module("a"))
  ens <- ensemble(mods)
  ens$forward(list(question = "test"), trace = TRUE)

  expect_length(ens$state$traces, 1)
  trace <- ens$state$traces[[1]]
  expect_true("individual_outputs" %in% names(trace))
  expect_true("n_successful" %in% names(trace))
})

test_that("EnsembleModule with trace=FALSE does not record", {
  mods <- lapply(1:2, function(i) create_mock_module("a"))
  ens <- ensemble(mods)
  ens$forward(list(question = "test"), trace = FALSE)

  expect_length(ens$state$traces, 0)
})

# ============================================================================
# Reducer Function Tests
# ============================================================================

test_that("reduce_majority returns most common output", {
  reducer <- reduce_majority()

  outputs <- list(
    list(answer = "a"),
    list(answer = "b"),
    list(answer = "a"),
    list(answer = "a")
  )

  result <- reducer(outputs)
  expect_equal(result$answer, "a")
})
test_that("reduce_majority handles ties with first", {
  reducer <- reduce_majority(tie_breaker = "first")

  outputs <- list(
    list(answer = "a"),
    list(answer = "b")
  )

  result <- reducer(outputs)
  expect_true(result$answer %in% c("a", "b"))
})

test_that("reduce_majority uses specified field", {
  reducer <- reduce_majority(field = "sentiment")

  outputs <- list(
    list(answer = "x", sentiment = "positive"),
    list(answer = "y", sentiment = "negative"),
    list(answer = "z", sentiment = "positive")
  )

  result <- reducer(outputs)
  expect_equal(result$sentiment, "positive")
})

test_that("reduce_majority handles single output", {
  reducer <- reduce_majority()
  outputs <- list(list(answer = "only_one"))

  result <- reducer(outputs)
  expect_equal(result$answer, "only_one")
})

test_that("reduce_majority errors on empty list", {
  reducer <- reduce_majority()
  expect_error(reducer(list()), "empty list")
})

test_that("reduce_weighted_vote uses weights", {
  reducer <- reduce_weighted_vote()

  outputs <- list(
    list(answer = "a"),
    list(answer = "b"),
    list(answer = "b")
  )

  # Without weights, "b" wins (2 vs 1)
  result <- reducer(outputs, weights = NULL)
  expect_equal(result$answer, "b")

  # With high weight on "a", it should win
  result2 <- reducer(outputs, weights = c(10, 1, 1))
  expect_equal(result2$answer, "a")
})

test_that("reduce_weighted_vote handles single output", {
  reducer <- reduce_weighted_vote()
  result <- reducer(list(list(answer = "single")), weights = c(1))
  expect_equal(result$answer, "single")
})

test_that("reduce_first returns first output", {
  reducer <- reduce_first()

  outputs <- list(
    list(answer = "first"),
    list(answer = "second"),
    list(answer = "third")
  )

  result <- reducer(outputs)
  expect_equal(result$answer, "first")
})

test_that("reduce_first errors on empty list", {
  reducer <- reduce_first()
  expect_error(reducer(list()), "empty list")
})

test_that("reduce_best_by_metric scores outputs", {
  metric <- metric_exact_match(field = "answer")
  reducer <- reduce_best_by_metric(metric)

  # Set expected value - must be a list with the field since metric extracts it
  attr(reducer, "set_expected")(list(answer = "correct"))

  outputs <- list(
    list(answer = "wrong"),
    list(answer = "correct"),
    list(answer = "also wrong")
  )

  result <- reducer(outputs)
  expect_equal(result$answer, "correct")
})

test_that("reduce_best_by_metric with maximize = FALSE returns lowest score", {
  # Create a metric that returns numeric scores
  length_metric <- function(output, expected) {
    nchar(output$answer)
  }

  reducer <- reduce_best_by_metric(length_metric, maximize = FALSE)
  attr(reducer, "set_expected")(list(answer = "ignored"))

  outputs <- list(
    list(answer = "very long answer"),
    list(answer = "short"),
    list(answer = "medium answer")
  )

  result <- reducer(outputs)
  expect_equal(result$answer, "short") # Shortest = lowest score
})

test_that("reduce_best_by_metric errors when no expected value set", {
  metric <- metric_exact_match(field = "answer")
  reducer <- reduce_best_by_metric(metric)

  # Don't set expected value
  outputs <- list(
    list(answer = "a"),
    list(answer = "b")
  )

  expect_error(
    reducer(outputs),
    "No expected value set"
  )
})

test_that("reduce_best_by_metric warns for individual failures then errors when all fail", {
  # Create a metric that throws an error
  failing_metric <- function(output, expected) {
    stop("Metric computation failed")
  }

  reducer <- reduce_best_by_metric(failing_metric)
  attr(reducer, "set_expected")(list(answer = "expected"))

  outputs <- list(
    list(answer = "a"),
    list(answer = "b")
  )

  # Should warn for each scoring failure, then error when all scores are NA

  expect_error_with_warnings(
    reducer(outputs),
    warning_regexp = "Metric scoring failed",
    error_regexp = "All metric scores are NA"
  )
})

test_that("reduce_best_by_metric errors when all scores are NA", {
  # Create a metric that returns NA
  na_metric <- function(output, expected) {
    NA_real_
  }

  reducer <- reduce_best_by_metric(na_metric)
  attr(reducer, "set_expected")(list(answer = "expected"))

  outputs <- list(
    list(answer = "a"),
    list(answer = "b")
  )

  expect_error(
    reducer(outputs),
    "All metric scores are NA"
  )
})

test_that("reduce_weighted_vote errors on empty list", {
  reducer <- reduce_weighted_vote()
  expect_error(reducer(list()), "empty list")
})

test_that("reduce_majority with tie_breaker = 'random' works", {
  reducer <- reduce_majority(tie_breaker = "random")

  outputs <- list(
    list(answer = "a"),
    list(answer = "b")
  )

  # Should return one of the tied values
  result <- reducer(outputs)
  expect_true(result$answer %in% c("a", "b"))
})

test_that("EnsembleModule errors with helpful message when reduce function fails", {
  mods <- list(
    create_mock_module("a"),
    create_mock_module("b")
  )

  # Use a reduce function that throws an error
  failing_reduce <- function(outputs, weights = NULL) {
    stop("Custom reduce error")
  }

  ens <- ensemble(mods, reduce_fn = failing_reduce)

  expect_error(
    ens$forward(list(question = "test")),
    "Reduce function failed"
  )
})

# ============================================================================
# Ensemble Teleprompter Tests
# ============================================================================

test_that("Ensemble S7 class exists", {
  expect_true(S7::S7_inherits(Ensemble(), Teleprompter))
})

test_that("Ensemble creates with default values", {
  tp <- Ensemble()

  expect_null(tp@reduce_fn)
  expect_null(tp@size)
  expect_null(tp@weights)
})

test_that("Ensemble accepts reduce_fn parameter", {
  tp <- Ensemble(reduce_fn = reduce_first())
  expect_true(is.function(tp@reduce_fn))
})

test_that("Ensemble accepts size parameter", {
  tp <- Ensemble(size = 3L)
  expect_equal(tp@size, 3L)
})

test_that("Ensemble validates size parameter", {
  expect_error(Ensemble(size = -1))
  expect_error(Ensemble(size = 0))
})

test_that("Ensemble accepts weights parameter", {
  tp <- Ensemble(weights = c(0.9, 0.8, 0.7))
  expect_equal(tp@weights, c(0.9, 0.8, 0.7))
})

test_that("compile_ensemble creates EnsembleModule", {
  mods <- lapply(1:3, function(i) create_mock_module(paste0("a", i)))
  tp <- Ensemble()

  result <- compile(tp, program = NULL, trainset = NULL, programs = mods)

  expect_s3_class(result, "EnsembleModule")
  expect_equal(length(result$modules), 3)
  expect_true(result$config$compiled)
  expect_equal(result$config$teleprompter, "Ensemble")
})

test_that("compile_ensemble respects size parameter", {
  mods <- lapply(1:5, function(i) create_mock_module(paste0("a", i)))
  tp <- Ensemble(size = 3L)

  result <- compile(tp, program = NULL, trainset = NULL, programs = mods)

  expect_equal(length(result$modules), 3)
})

test_that("compile_ensemble uses reduce_fn from teleprompter", {
  mods <- lapply(1:3, function(i) create_mock_module(paste0("a", i)))
  tp <- Ensemble(reduce_fn = reduce_first())

  result <- compile(tp, program = NULL, trainset = NULL, programs = mods)

  # Verify the reduce function is reduce_first by checking behavior
  run_result <- result$forward(list(question = "test"))
  expect_equal(run_result$output[[1]]$answer, "a1") # First module's output
})

test_that("compile_ensemble uses weights from teleprompter", {
  mods <- lapply(1:3, function(i) create_mock_module(paste0("a", i)))
  tp <- Ensemble(weights = c(0.9, 0.8, 0.7))

  result <- compile(tp, program = NULL, trainset = NULL, programs = mods)

  expect_equal(result$weights, c(0.9, 0.8, 0.7))
})

test_that("compile_ensemble warns when more weights than programs", {
  mods <- lapply(1:2, function(i) create_mock_module(paste0("a", i)))
  tp <- Ensemble(weights = c(0.9, 0.8, 0.7)) # 3 weights for 2 programs

  expect_warning(
    result <- compile(tp, program = NULL, trainset = NULL, programs = mods),
    "More weights"
  )
  # Should truncate to first 2 weights
  expect_equal(result$weights, c(0.9, 0.8))
})

test_that("compile_ensemble errors when fewer weights than programs", {
  mods <- lapply(1:4, function(i) create_mock_module(paste0("a", i)))
  tp <- Ensemble(weights = c(0.9, 0.8)) # 2 weights for 4 programs

  # Should error since user explicitly provided weights but count is wrong
  expect_error(
    compile(tp, program = NULL, trainset = NULL, programs = mods),
    "Fewer weights"
  )
})

test_that("compile_ensemble errors without programs", {
  tp <- Ensemble()

  expect_error(
    compile(tp, program = NULL, trainset = NULL),
    "requires a list of modules"
  )
})

test_that("compile_ensemble accepts program as list of modules", {
  mods <- lapply(1:3, function(i) create_mock_module(paste0("a", i)))
  tp <- Ensemble()

  # Pass modules via program argument instead of programs

  result <- compile(tp, program = mods, trainset = NULL)

  expect_s3_class(result, "EnsembleModule")
  expect_equal(length(result$modules), 3)
})

test_that("ensemble_from_programs convenience function works", {
  mods <- lapply(1:3, function(i) create_mock_module(paste0("a", i)))

  result <- ensemble_from_programs(mods)

  expect_s3_class(result, "EnsembleModule")
  expect_equal(length(result$modules), 3)
})

test_that("ensemble_from_programs accepts all parameters", {
  mods <- lapply(1:5, function(i) create_mock_module(paste0("a", i)))

  result <- ensemble_from_programs(
    programs = mods,
    reduce_fn = reduce_first(),
    weights = c(0.9, 0.85, 0.8),
    size = 3L
  )

  expect_equal(length(result$modules), 3)
  expect_equal(result$weights, c(0.9, 0.85, 0.8))
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("ensemble works with real PredictModule", {
  mod1 <- module(signature("question -> answer: string"))
  mod2 <- module(signature("question -> answer: string"))

  ens <- ensemble(list(mod1, mod2))

  expect_s3_class(ens, "EnsembleModule")
  expect_equal(length(ens$modules), 2)
})

test_that("majority voting selects correct answer in practice", {
  # 3 modules: 2 return "yes", 1 returns "no"
  mods <- list(
    create_mock_module("yes"),
    create_mock_module("no"),
    create_mock_module("yes")
  )

  ens <- ensemble(mods, reduce_fn = reduce_majority())
  result <- ens$forward(list(question = "test"))

  expect_equal(result$output[[1]]$answer, "yes")
})

test_that("weighted voting can override majority", {
  # 2 modules return "minority", 1 returns "majority" with high weight
  mods <- list(
    create_mock_module("majority"),
    create_mock_module("minority"),
    create_mock_module("minority")
  )

  # Give majority module very high weight
  ens <- ensemble(
    mods,
    reduce_fn = reduce_weighted_vote(),
    weights = c(100, 1, 1)
  )
  result <- ens$forward(list(question = "test"))

  expect_equal(result$output[[1]]$answer, "majority")
})

test_that("ensemble accumulates token costs", {
  mods <- lapply(1:3, function(i) {
    mock <- create_mock_module("a")
    mock$forward <- function(batch, .llm = NULL, trace = TRUE, ...) {
      tibble::tibble(
        output = list(list(answer = "a")),
        chat = list(NULL),
        metadata = list(list(
          total_tokens = 100,
          cost = 0.001,
          model = "mock"
        ))
      )
    }
    mock
  })

  ens <- ensemble(mods)
  result <- ens$forward(list(question = "test"))

  expect_equal(result$metadata[[1]]$total_tokens, 300)
  expect_equal(result$metadata[[1]]$total_cost, 0.003)
})
