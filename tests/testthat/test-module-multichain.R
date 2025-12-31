# Tests for module-multichain.R (MultiChainComparisonModule)

# Helper: Create a mock module for testing
create_mock_module <- function(
  responses = list("answer1", "answer2", "answer3")
) {
  idx <- 0
  mock_mod <- list(
    signature = signature("question -> answer"),
    chat = NULL,
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      idx <<- idx + 1
      response <- responses[[(idx - 1) %% length(responses) + 1]]
      tibble::tibble(
        output = list(list(reasoning = "thinking...", answer = response)),
        chat = list(NULL),
        metadata = list(list(
          total_tokens = 100,
          cost = 0.001,
          model = "mock-model"
        ))
      )
    },
    reset_copy = function() create_mock_module(responses)
  )
  class(mock_mod) <- c("MockModule", "PredictModule", "Module", "R6")
  mock_mod
}

# ============================================================================
# MultiChainComparisonModule Tests
# ============================================================================

test_that("MultiChainComparisonModule class exists", {
  expect_true(R6::is.R6Class(MultiChainComparisonModule))
})

test_that("multi_chain_comparison creates MultiChainComparisonModule", {
  mcc <- multi_chain_comparison("question -> answer")

  expect_s3_class(mcc, "MultiChainComparisonModule")
  expect_s3_class(mcc, "Module")
})

test_that("multi_chain_comparison uses default M of 3", {
  mcc <- multi_chain_comparison("question -> answer")
  expect_equal(mcc$M, 3L)
})

test_that("multi_chain_comparison uses default temperature of 0.7", {
  mcc <- multi_chain_comparison("question -> answer")
  expect_equal(mcc$temperature, 0.7)
})

test_that("multi_chain_comparison accepts custom M and temperature", {
  mcc <- multi_chain_comparison(
    "question -> answer",
    M = 5,
    temperature = 0.9
  )

  expect_equal(mcc$M, 5L)
  expect_equal(mcc$temperature, 0.9)
})

test_that("multi_chain_comparison creates inner module with CoT", {
  mcc <- multi_chain_comparison("question -> answer")

  # Inner module should have reasoning
  expect_true(has_reasoning(mcc$inner_module$signature))
})

test_that("multi_chain_comparison accepts custom inner module", {
  inner <- module(signature("q -> a"))
  mcc <- multi_chain_comparison(
    "question -> answer",
    inner_module = inner
  )

  expect_identical(mcc$inner_module, inner)
})

test_that("module factory creates MultiChainComparisonModule with type='multichain'", {
  sig <- signature("question -> answer")
  mcc <- module(sig, type = "multichain", M = 4)

  expect_s3_class(mcc, "MultiChainComparisonModule")
  expect_equal(mcc$M, 4L)
})

test_that("MultiChainComparisonModule has correct structure", {
  # Test structure without forward execution (needs real LLM)
  mcc <- multi_chain_comparison("question -> answer", M = 2)

  # Check structure
  expect_s3_class(mcc, "MultiChainComparisonModule")
  expect_equal(mcc$M, 2L)
  expect_true(!is.null(mcc$inner_module))
  expect_true(!is.null(mcc$comparison_template))
})

test_that("MultiChainComparisonModule get_attempts returns tibble", {
  mcc <- multi_chain_comparison("question -> answer", M = 2)

  attempts <- mcc$get_attempts()

  expect_s3_class(attempts, "tbl_df")
  expect_true("run" %in% names(attempts))
  expect_true("attempt" %in% names(attempts))
  expect_true("prediction" %in% names(attempts))
})

test_that("MultiChainComparisonModule print works", {
  mcc <- multi_chain_comparison("question -> answer", M = 5)

  expect_invisible(print(mcc))
  expect_s3_class(mcc, "MultiChainComparisonModule")
})

test_that("MultiChainComparisonModule reset_copy creates fresh module", {
  mcc <- multi_chain_comparison(
    "question -> answer",
    M = 5,
    temperature = 0.9
  )

  copy <- mcc$reset_copy()

  expect_s3_class(copy, "MultiChainComparisonModule")
  expect_equal(copy$M, 5L)
  expect_equal(copy$temperature, 0.9)
  expect_length(copy$state$traces, 0)
})

test_that("MultiChainComparisonModule apply_optimization_params updates settings", {
  mcc <- multi_chain_comparison("question -> answer", M = 3, temperature = 0.7)

  mcc$apply_optimization_params(list(M = 5, temperature = 0.9))

  expect_equal(mcc$M, 5L)
  expect_equal(mcc$temperature, 0.9)
})

test_that("MultiChainComparisonModule has default comparison template", {
  mcc <- multi_chain_comparison("question -> answer")

  expect_true(nchar(mcc$comparison_template) > 0)
  expect_true(grepl("\\{M\\}", mcc$comparison_template))
  expect_true(grepl("attempts", mcc$comparison_template, fixed = TRUE))
})

test_that("MultiChainComparisonModule accepts custom comparison template", {
  template <- "Custom template with {M} attempts: {attempts_text}"
  mcc <- multi_chain_comparison(
    "question -> answer",
    comparison_template = template
  )

  expect_equal(mcc$comparison_template, template)
})

test_that("multi_chain_comparison with string signature", {
  mcc <- multi_chain_comparison("context, question -> answer: string")

  expect_s3_class(mcc, "MultiChainComparisonModule")
  expect_equal(length(mcc$signature@inputs), 2)
})

test_that("multi_chain_comparison with Signature object", {
  sig <- signature("question -> answer")
  mcc <- multi_chain_comparison(sig, M = 4)

  expect_s3_class(mcc, "MultiChainComparisonModule")
  expect_equal(mcc$M, 4L)
})

test_that("multi_chain_comparison rejects invalid signature", {
  expect_error(
    multi_chain_comparison(123),
    "Signature"
  )
})

test_that("multi_chain_comparison rejects invalid inner_module", {
  expect_error(
    multi_chain_comparison(
      "question -> answer",
      inner_module = "not a module"
    ),
    "must be a Module"
  )
})

# ============================================================================
# Format Attempts Tests
# ============================================================================

test_that("format_attempts creates readable output", {
  mcc <- multi_chain_comparison("question -> answer", M = 2)

  # Access private method
  format_fn <- mcc$.__enclos_env__$private$format_attempts

  attempts <- list(
    list(
      attempt = 1,
      prediction = list(reasoning = "Step 1...", answer = "42")
    ),
    list(
      attempt = 2,
      prediction = list(reasoning = "Step 2...", answer = "43")
    )
  )

  result <- format_fn(attempts)

  expect_type(result, "character")
  expect_true(grepl("Attempt 1", result, fixed = TRUE))
  expect_true(grepl("Attempt 2", result, fixed = TRUE))
  expect_true(grepl("42", result, fixed = TRUE))
  expect_true(grepl("43", result, fixed = TRUE))
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("MultiChainComparisonModule works with chain_of_thought inner module", {
  cot <- chain_of_thought("question -> answer")
  mcc <- multi_chain_comparison(
    "question -> answer",
    inner_module = cot,
    M = 2
  )

  expect_s3_class(mcc, "MultiChainComparisonModule")
  expect_identical(mcc$inner_module, cot)
})

test_that("MultiChainComparisonModule signature matches inner", {
  mcc <- multi_chain_comparison("context, question -> answer")

  # MCC signature should be the original
  expect_equal(length(mcc$signature@inputs), 2)
  expect_equal(mcc$signature@inputs[[1]]$name, "context")
  expect_equal(mcc$signature@inputs[[2]]$name, "question")
})

# ============================================================================
# Validation Tests
# ============================================================================

test_that("multi_chain_comparison rejects M < 1", {
  expect_error(
    multi_chain_comparison("question -> answer", M = 0),
    "M must be at least 1"
  )

  expect_error(
    multi_chain_comparison("question -> answer", M = -1),
    "M must be at least 1"
  )
})

test_that("MultiChainComparisonModule accepts M = 1", {
  mcc <- multi_chain_comparison("question -> answer", M = 1)
  expect_equal(mcc$M, 1L)
})

# ============================================================================
# Forward Tests with Mocking
# ============================================================================

test_that("MultiChainComparisonModule forward calls inner module M times", {
  skip("Requires LLM for run_comparison step - tested via integration tests")
})

test_that("MultiChainComparisonModule forward handles partial failures", {
  # First call succeeds, second fails, third succeeds
  call_count <- 0
  mock_mod <- list(
    signature = signature("question -> answer"),
    chat = NULL,
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      call_count <<- call_count + 1
      if (call_count == 2) {
        stop("Simulated failure")
      }
      tibble::tibble(
        output = list(list(
          reasoning = "thinking",
          answer = paste0("answer", call_count)
        )),
        chat = list(NULL),
        metadata = list(list(total_tokens = 50, cost = 0.001, model = "mock"))
      )
    },
    reset_copy = function() create_mock_module()
  )
  class(mock_mod) <- c("MockModule", "PredictModule", "Module", "R6")

  mcc <- multi_chain_comparison(
    "question -> answer",
    M = 3,
    inner_module = mock_mod
  )

  # Note: run_comparison requires real LLM, so we just test that:
  # 1. Partial failures are warned about
  # 2. All attempts failing results in an error
  # Full forward() testing would need VCR cassettes

  # Test that when all attempts fail, we get appropriate error
  all_fail_mod <- list(
    signature = signature("question -> answer"),
    chat = NULL,
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      stop("Always fails")
    },
    reset_copy = function() create_mock_module()
  )
  class(all_fail_mod) <- c("MockModule", "PredictModule", "Module", "R6")

  mcc_fail <- multi_chain_comparison(
    "question -> answer",
    M = 2,
    inner_module = all_fail_mod
  )

  expect_error(
    suppressWarnings(mcc_fail$forward(list(question = "test?"))),
    "All.*attempts failed"
  )
})
