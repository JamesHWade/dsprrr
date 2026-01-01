# Tests for ProgramOfThoughtModule

# Helper: Create a mock LLM for testing
create_mock_pot_llm <- function(code_responses = list()) {
  call_count <- 0
  list(
    chat_structured = function(prompt, type, ...) {
      call_count <<- call_count + 1
      if (call_count <= length(code_responses)) {
        code_responses[[call_count]]
      } else {
        # Default: simple arithmetic
        list(
          code = "1 + 1",
          explanation = "Simple addition"
        )
      }
    },
    chat = function(prompt, ...) {
      "42"
    }
  )
}

# ============================================================================
# Factory Function Tests
# ============================================================================

test_that("program_of_thought requires runner", {
  expect_error(
    program_of_thought("question -> answer"),
    "Code execution requires an explicit runner"
  )
})

test_that("program_of_thought validates runner type", {
  expect_error(
    program_of_thought("question -> answer", runner = "not a runner"),
    "runner must be an RCodeRunner"
  )
})

test_that("program_of_thought creates ProgramOfThoughtModule", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  pot <- program_of_thought("question -> answer", runner = runner)

  expect_s3_class(pot, "ProgramOfThoughtModule")
  expect_s3_class(pot, "Module")
})

test_that("program_of_thought accepts string signature", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  pot <- program_of_thought("context, question -> answer", runner = runner)

  expect_equal(length(pot$signature@inputs), 2)
})

test_that("program_of_thought accepts Signature object", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  sig <- signature("question -> answer")
  pot <- program_of_thought(sig, runner = runner)

  expect_s3_class(pot, "ProgramOfThoughtModule")
})

test_that("program_of_thought respects max_iters", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  pot <- program_of_thought(
    "question -> answer",
    runner = runner,
    max_iters = 5
  )

  expect_equal(pot$max_iters, 5L)
})

test_that("program_of_thought respects extract_answer", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  pot <- program_of_thought(
    "question -> answer",
    runner = runner,
    extract_answer = FALSE
  )

  expect_false(pot$extract_answer)
})

# ============================================================================
# Module Structure Tests
# ============================================================================

test_that("ProgramOfThoughtModule has correct fields", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  pot <- program_of_thought("question -> answer", runner = runner)

  expect_true(!is.null(pot$runner))
  expect_true(!is.null(pot$max_iters))
  expect_true(!is.null(pot$extract_answer))
  expect_true(!is.null(pot$signature))
})

test_that("ProgramOfThoughtModule get_executions returns list", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  pot <- program_of_thought("question -> answer", runner = runner)

  executions <- pot$get_executions()

  expect_type(executions, "list")
  expect_length(executions, 0) # No executions yet
})

test_that("ProgramOfThoughtModule reset_copy creates fresh module", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  pot <- program_of_thought(
    "question -> answer",
    runner = runner,
    max_iters = 7
  )

  copy <- pot$reset_copy()

  expect_s3_class(copy, "ProgramOfThoughtModule")
  expect_equal(copy$max_iters, 7L)
  expect_length(copy$state$executions, 0)
})

test_that("ProgramOfThoughtModule print works", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  pot <- program_of_thought("question -> answer", runner = runner)

  expect_invisible(print(pot))
})

# ============================================================================
# Forward Execution Tests
# ============================================================================

test_that("ProgramOfThoughtModule forward executes generated code", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  pot <- program_of_thought(
    "question -> answer",
    runner = runner,
    extract_answer = FALSE
  )

  # Mock LLM that generates simple code
  mock_llm <- create_mock_pot_llm(list(
    list(code = "2 + 2", explanation = "Adding numbers")
  ))

  result <- pot$forward(
    list(question = "What is 2+2?"),
    .llm = mock_llm
  )

  expect_s3_class(result, "tbl_df")
  expect_true("output" %in% names(result))
  expect_true("metadata" %in% names(result))
})

test_that("ProgramOfThoughtModule uses context correctly", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  pot <- program_of_thought(
    "numbers -> sum",
    runner = runner,
    extract_answer = FALSE
  )

  # Code that uses context
  mock_llm <- create_mock_pot_llm(list(
    list(
      code = "sum(.context$numbers)",
      explanation = "Sum the numbers from context"
    )
  ))

  result <- pot$forward(
    list(numbers = c(1, 2, 3, 4, 5)),
    .llm = mock_llm
  )

  expect_true(result$metadata[[1]]$success)
  expect_equal(result$metadata[[1]]$execution_result, 15)
})

test_that("ProgramOfThoughtModule retries on error", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  pot <- program_of_thought(
    "question -> answer",
    runner = runner,
    max_iters = 3,
    extract_answer = FALSE
  )

  # First attempt fails, second succeeds
  mock_llm <- create_mock_pot_llm(list(
    list(code = "stop('intentional error')", explanation = "Will fail"),
    list(code = "42", explanation = "Fixed version")
  ))

  result <- suppressWarnings(
    pot$forward(
      list(question = "What is the answer?"),
      .llm = mock_llm
    )
  )

  expect_true(result$metadata[[1]]$success)
  expect_equal(result$metadata[[1]]$iterations, 2)
  expect_equal(result$metadata[[1]]$execution_result, 42)
})

test_that("ProgramOfThoughtModule fails after max_iters", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  pot <- program_of_thought(
    "question -> answer",
    runner = runner,
    max_iters = 2
  )

  # All attempts fail
  mock_llm <- create_mock_pot_llm(list(
    list(code = "stop('error 1')", explanation = "Will fail"),
    list(code = "stop('error 2')", explanation = "Still fails")
  ))

  expect_error(
    suppressWarnings(
      pot$forward(
        list(question = "What is the answer?"),
        .llm = mock_llm
      )
    ),
    "failed after 2 iterations"
  )
})

test_that("ProgramOfThoughtModule stores execution history", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  pot <- program_of_thought(
    "question -> answer",
    runner = runner,
    extract_answer = FALSE
  )

  mock_llm <- create_mock_pot_llm(list(
    list(code = "100", explanation = "Return 100")
  ))

  pot$forward(
    list(question = "Give me 100"),
    .llm = mock_llm
  )

  executions <- pot$get_executions()

  expect_length(executions, 1)
  expect_true("iterations" %in% names(executions[[1]]))
  expect_true(executions[[1]]$success)
})

test_that("ProgramOfThoughtModule handles data frame input", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  pot <- program_of_thought(
    "x, y -> result",
    runner = runner,
    extract_answer = FALSE
  )

  mock_llm <- create_mock_pot_llm(list(
    list(code = ".context$x * .context$y", explanation = "Multiply")
  ))

  batch <- data.frame(x = 6, y = 7)
  result <- pot$forward(batch, .llm = mock_llm)

  expect_true(result$metadata[[1]]$success)
  expect_equal(result$metadata[[1]]$execution_result, 42)
})

# ============================================================================
# Metadata Tests
# ============================================================================

test_that("ProgramOfThoughtModule returns correct metadata", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  pot <- program_of_thought(
    "question -> answer",
    runner = runner,
    extract_answer = FALSE
  )

  mock_llm <- create_mock_pot_llm(list(
    list(code = "pi", explanation = "Return pi")
  ))

  result <- pot$forward(
    list(question = "What is pi?"),
    .llm = mock_llm
  )

  metadata <- result$metadata[[1]]

  expect_equal(metadata$model, "program_of_thought")
  expect_true(metadata$success)
  expect_true("iterations" %in% names(metadata))
  expect_true("final_code" %in% names(metadata))
  expect_true("duration_ms" %in% names(metadata))
  expect_true("execution_result" %in% names(metadata))
})

# ============================================================================
# Integration with module() factory
# ============================================================================

test_that("module() factory works with type='program_of_thought'", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  pot <- module(
    signature("question -> answer"),
    type = "program_of_thought",
    runner = runner
  )

  expect_s3_class(pot, "ProgramOfThoughtModule")
})

test_that("module() factory requires runner for program_of_thought", {
  expect_error(
    module(
      signature("question -> answer"),
      type = "program_of_thought"
    ),
    "program_of_thought requires a runner"
  )
})
