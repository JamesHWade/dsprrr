# Tests for RLMModule (Recursive Language Model)

# Helper: Create a mock LLM for RLM testing
create_mock_rlm_llm <- function(
  code_responses = list(),
  fallback_chat = "fallback answer",
  fallback_structured = list(answer = fallback_chat)
) {
  call_count <- 0
  mock <- list(
    clone = function() {
      create_mock_rlm_llm(code_responses, fallback_chat, fallback_structured)
    },
    chat_structured = function(prompt, type, ...) {
      type_fields <- if (
        inherits(type, "ellmer::TypeObject") &&
          methods::.hasSlot(type, "properties")
      ) {
        names(type@properties)
      } else {
        character()
      }

      is_code_gen <- all(c("reasoning", "code") %in% type_fields)

      if (is_code_gen) {
        call_count <<- call_count + 1
        if (call_count <= length(code_responses)) {
          return(code_responses[[call_count]])
        }

        # Default: simple SUBMIT
        return(list(
          reasoning = "Returning default answer",
          code = "SUBMIT('default answer')"
        ))
      }

      if (!is.null(fallback_structured)) {
        fallback_structured
      } else {
        stop("structured fallback unavailable")
      }
    },
    chat = function(prompt, ...) {
      fallback_chat
    }
  )
  mock
}

# ============================================================================
# Factory Function Tests
# ============================================================================

test_that("rlm_module requires runner", {
  expect_error(
    rlm_module("question -> answer"),
    "RLM requires an explicit runner"
  )
})

test_that("rlm_module validates runner type", {
  expect_error(
    rlm_module("question -> answer", runner = "not a runner"),
    "runner must implement the dsprrr code-runner protocol"
  )
})

test_that("rlm_module creates RLMModule", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  rlm <- rlm_module("question -> answer", runner = runner)

  expect_s3_class(rlm, "RLMModule")
  expect_s3_class(rlm, "Module")
})

test_that("rlm_module accepts string signature", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  rlm <- rlm_module("document, question -> answer", runner = runner)

  expect_equal(length(rlm$signature@inputs), 2)
})

test_that("rlm_module accepts Signature object", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  sig <- signature("question -> answer")
  rlm <- rlm_module(sig, runner = runner)

  expect_s3_class(rlm, "RLMModule")
})

test_that("rlm_module respects max_iterations", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 10
  )

  expect_equal(rlm$max_iterations, 10L)
})

test_that("rlm_module respects max_llm_calls", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_llm_calls = 25
  )

  expect_equal(rlm$max_llm_calls, 25L)
})

test_that("rlm_module validates tools parameter", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)

  # Must be a list
  expect_error(
    rlm_module("question -> answer", runner = runner, tools = "not a list"),
    "tools must be a named list"
  )

  # If not empty, must be named
  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(function() {})
    ),
    "tools must be a named list"
  )
})

test_that("rlm_module validates all tools are functions", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)

  # Non-function in tools list
  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(good = function() {}, bad = "not a function")
    ),
    "All tools must be functions"
  )

  # Multiple non-functions
  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(a = 1, b = "string", c = function() {})
    ),
    "All tools must be functions"
  )
})

test_that("rlm_module validates tool names", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)

  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(`bad-name` = function() 1)
    ),
    "valid R identifiers"
  )

  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(SUBMIT = function() 1)
    ),
    "conflict with built-in RLM tools"
  )

  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(llm_query = function() "x")
    ),
    "conflict with built-in RLM tools"
  )

  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(print = function(...) NULL)
    ),
    "conflict with built-in RLM tools"
  )
})

test_that("rlm_module validates max_iterations bounds", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)

  expect_error(
    rlm_module("question -> answer", runner = runner, max_iterations = 0),
    "max_iterations must be at least 1"
  )

  expect_error(
    rlm_module("question -> answer", runner = runner, max_iterations = -5),
    "max_iterations must be at least 1"
  )
})

test_that("rlm_module validates max_llm_calls bounds", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)

  expect_error(
    rlm_module("question -> answer", runner = runner, max_llm_calls = -1),
    "max_llm_calls must be non-negative"
  )

  # Zero is allowed (disables recursive calls)
  expect_no_error(
    rlm_module("question -> answer", runner = runner, max_llm_calls = 0)
  )
})

# ============================================================================
# Module Structure Tests
# ============================================================================

test_that("RLMModule has correct fields", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  rlm <- rlm_module("question -> answer", runner = runner)

  expect_true(!is.null(rlm$runner))
  expect_true(!is.null(rlm$max_iterations))
  expect_true(!is.null(rlm$max_llm_calls))
  expect_true(!is.null(rlm$max_output_chars))
  expect_true(!is.null(rlm$signature))
  expect_false(rlm$verbose)
  expect_null(rlm$sub_lm)
})

test_that("RLMModule get_repl_history returns list", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  rlm <- rlm_module("question -> answer", runner = runner)

  history <- rlm$get_repl_history()

  expect_type(history, "list")
  expect_length(history, 0) # No executions yet
})

test_that("RLMModule reset_copy creates fresh module", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 15
  )

  copy <- rlm$reset_copy()

  expect_s3_class(copy, "RLMModule")
  expect_equal(copy$max_iterations, 15L)
  expect_length(copy$state$repl_history, 0)
})

test_that("RLMModule print works", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  rlm <- rlm_module("question -> answer", runner = runner)

  expect_invisible(print(rlm))
})

# ============================================================================
# SUBMIT Termination Tests
# ============================================================================

test_that("RLMModule terminates on SUBMIT call", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  # Mock LLM that immediately calls SUBMIT
  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Computing the answer",
      code = "SUBMIT(42)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is 6 * 7?"),
    .llm = mock_llm
  )

  expect_s3_class(result, "tbl_df")
  expect_true("output" %in% names(result))
  expect_true("metadata" %in% names(result))
  expect_equal(result$metadata[[1]]$iterations, 1)
})

test_that("RLMModule SUBMIT returns correct value", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Simple string answer",
      code = "SUBMIT('the answer is 42')"
    )
  ))

  result <- rlm$forward(
    list(question = "What is the meaning of life?"),
    .llm = mock_llm
  )

  # The output should contain the submitted answer
  expect_equal(result$output[[1]]$answer, "the answer is 42")
})

test_that("RLMModule SUBMIT works with complex values", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Returning a list",
      code = "SUBMIT(list(value = 42, unit = 'answer'))"
    )
  ))

  result <- rlm$forward(
    list(question = "Give me structured data"),
    .llm = mock_llm
  )

  expect_type(result$output[[1]]$answer, "list")
  expect_equal(result$output[[1]]$answer$value, 42)
})

test_that("RLMModule strips markdown code fences before execution", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module("question -> answer", runner = runner)

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Return fenced code block",
      code = "```r\nx <- 40 + 2\nSUBMIT(x)\n```"
    )
  ))

  result <- rlm$forward(
    list(question = "What is 40 + 2?"),
    .llm = mock_llm
  )

  expect_equal(result$output[[1]]$answer, 42)
})

test_that("RLMModule supports multi-output SUBMIT with named arguments", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer, confidence: number",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Return named outputs",
      code = "SUBMIT(answer = 'Paris', confidence = '0.95')"
    )
  ))

  result <- rlm$forward(
    list(question = "What is the capital of France?"),
    .llm = mock_llm
  )

  expect_equal(result$output[[1]]$answer, "Paris")
  expect_equal(result$output[[1]]$confidence, 0.95)
})

test_that("RLMModule supports multi-output SUBMIT with positional arguments", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer, confidence: number",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Return positional outputs",
      code = "SUBMIT('Paris', 0.8)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is the capital of France?"),
    .llm = mock_llm
  )

  expect_equal(result$output[[1]]$answer, "Paris")
  expect_equal(result$output[[1]]$confidence, 0.8)
})

test_that("RLMModule retries after invalid multi-output SUBMIT payload", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer, confidence: number",
    runner = runner,
    max_iterations = 3
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Missing one required output field",
      code = "SUBMIT(answer = 'Paris')"
    ),
    list(
      reasoning = "Now provide both fields",
      code = "SUBMIT(answer = 'Paris', confidence = 0.9)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is the capital of France?"),
    .llm = mock_llm
  )

  expect_equal(result$metadata[[1]]$iterations, 2)
  expect_equal(result$output[[1]]$answer, "Paris")
  expect_equal(result$output[[1]]$confidence, 0.9)
})

# ============================================================================
# REPL Tools Tests
# ============================================================================

test_that("RLM peek function works in REPL", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "document -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Peek at document",
      code = "first_100 <- peek(.context$document, 1, 100)\nSUBMIT(first_100)"
    )
  ))

  long_doc <- paste(rep("Hello world. ", 100), collapse = "")

  result <- rlm$forward(
    list(document = long_doc),
    .llm = mock_llm
  )

  # Should get first 100 characters
  expect_equal(nchar(result$output[[1]]$answer), 100)
  expect_true(startsWith(result$output[[1]]$answer, "Hello world"))
})

test_that("RLM search function works in REPL", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "document -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Search for emails",
      code = "matches <- search(.context$document, '[a-z]+@[a-z]+\\\\.[a-z]+')\nSUBMIT(paste(matches, collapse=', '))"
    )
  ))

  doc <- "Contact us at test@example.com or info@example.org for more info."

  result <- rlm$forward(
    list(document = doc),
    .llm = mock_llm
  )

  expect_true(grepl("test@example.com", result$output[[1]]$answer))
  expect_true(grepl("info@example.org", result$output[[1]]$answer))
})

# ============================================================================
# Multiple Iterations Tests
# ============================================================================

test_that("RLMModule supports multiple iterations", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  # First iteration explores, second submits
  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "First, let me compute something",
      code = "x <- 10 + 32"
    ),
    list(
      reasoning = "Now submit the answer",
      code = "SUBMIT(42)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is 10 + 32?"),
    .llm = mock_llm
  )

  expect_equal(result$metadata[[1]]$iterations, 2)
})

test_that("RLMModule respects max_iterations", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 2
  )

  # LLM never calls SUBMIT
  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Step 1", code = "x <- 1"),
    list(reasoning = "Step 2", code = "y <- 2"),
    list(reasoning = "Step 3", code = "z <- 3") # Won't be reached
  ))

  result <- expect_test_warnings(
    rlm$forward(
      list(question = "test"),
      .llm = mock_llm
    ),
    "reached max_iterations"
  )

  # Should use fallback after max_iterations
  expect_equal(result$metadata[[1]]$iterations, 2)
  expect_equal(result$metadata[[1]]$max_iterations, 2)
})

test_that("RLMModule uses fallback when no SUBMIT", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 1
  )

  # LLM never calls SUBMIT
  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Just computing", code = "1 + 1")
  ))

  result <- expect_test_warnings(
    rlm$forward(
      list(question = "test"),
      .llm = mock_llm
    ),
    "reached max_iterations"
  )

  # Should have used fallback extraction
  expect_equal(result$output[[1]]$answer, "fallback answer")
})

test_that("RLMModule normalizes structured fallback to signature fields and types", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> score: number, label: enum('positive', 'negative'), tags: array(string)",
    runner = runner,
    max_iterations = 1
  )

  mock_llm <- create_mock_rlm_llm(
    code_responses = list(
      list(reasoning = "No final output yet", code = "x <- 1")
    ),
    fallback_structured = list(
      score = "0.75",
      label = "POSITIVE",
      tags = c(1, 2, "ok")
    )
  )

  result <- expect_test_warnings(
    rlm$forward(
      list(question = "Classify this"),
      .llm = mock_llm
    ),
    "reached max_iterations"
  )

  expect_equal(result$output[[1]]$score, 0.75)
  expect_equal(result$output[[1]]$label, "positive")
  expect_equal(result$output[[1]]$tags, c("1", "2", "ok"))
})

# ============================================================================
# Error Handling Tests
# ============================================================================

test_that("RLMModule handles code execution errors", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 3
  )

  # First iteration fails, second succeeds
  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Try something", code = "stop('intentional error')"),
    list(reasoning = "Fixed it", code = "SUBMIT('success')")
  ))

  result <- rlm$forward(
    list(question = "test"),
    .llm = mock_llm
  )

  expect_equal(result$metadata[[1]]$iterations, 2)
  expect_equal(result$output[[1]]$answer, "success")
})

# ============================================================================
# Custom Tools Tests
# ============================================================================

test_that("RLMModule supports custom tools", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)

  # Define a custom tool
  custom_tools <- list(
    double_it = function(x) x * 2
  )

  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    tools = custom_tools
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Using custom tool",
      code = "result <- double_it(21)\nSUBMIT(result)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is double 21?"),
    .llm = mock_llm
  )

  expect_equal(result$output[[1]]$answer, 42)
})

# ============================================================================
# REPL History Tests
# ============================================================================

test_that("RLMModule stores REPL history", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Computing", code = "SUBMIT('done')")
  ))

  rlm$forward(
    list(question = "test"),
    .llm = mock_llm
  )

  history <- rlm$get_repl_history()

  expect_length(history, 1)
  expect_true("inputs" %in% names(history[[1]]))
  expect_true("history" %in% names(history[[1]]))
  expect_true("final_answer" %in% names(history[[1]]))
})

test_that("RLMModule respects trace=FALSE", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Computing", code = "SUBMIT('done')")
  ))

  rlm$forward(list(question = "test"), .llm = mock_llm, trace = FALSE)

  # No history should be stored when trace=FALSE
  expect_length(rlm$get_repl_history(), 0)
})

# ============================================================================
# Metadata Tests
# ============================================================================

test_that("RLMModule returns correct metadata", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 20,
    max_llm_calls = 50
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Direct answer", code = "SUBMIT('pi')")
  ))

  result <- rlm$forward(
    list(question = "What is pi?"),
    .llm = mock_llm
  )

  metadata <- result$metadata[[1]]

  expect_equal(metadata$model, "rlm")
  expect_true("iterations" %in% names(metadata))
  expect_true("max_iterations" %in% names(metadata))
  expect_true("llm_calls" %in% names(metadata))
  expect_true("max_llm_calls" %in% names(metadata))
  expect_true("duration_ms" %in% names(metadata))
  expect_true("repl_history" %in% names(metadata))

  expect_equal(metadata$max_iterations, 20)
  expect_equal(metadata$max_llm_calls, 50)
})

# ============================================================================
# Integration with module() factory
# ============================================================================

test_that("module() factory works with type='rlm'", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5)
  rlm <- module(
    signature("question -> answer"),
    type = "rlm",
    runner = runner
  )

  expect_s3_class(rlm, "RLMModule")
})

test_that("module() factory requires runner for rlm", {
  expect_error(
    module(
      signature("question -> answer"),
      type = "rlm"
    ),
    "rlm requires a runner"
  )
})

# ============================================================================
# Context Description Tests
# ============================================================================

test_that("RLMModule handles various context types", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "text, numbers, df -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Process inputs",
      code = "SUBMIT(paste('text:', nchar(.context$text), 'numbers:', length(.context$numbers), 'df rows:', nrow(.context$df)))"
    )
  ))

  result <- rlm$forward(
    list(
      text = "Hello world",
      numbers = c(1, 2, 3, 4, 5),
      df = data.frame(a = 1:3, b = 4:6)
    ),
    .llm = mock_llm
  )

  expect_true(grepl("text: 11", result$output[[1]]$answer, fixed = TRUE))
  expect_true(grepl("numbers: 5", result$output[[1]]$answer, fixed = TRUE))
  expect_true(grepl("df rows: 3", result$output[[1]]$answer, fixed = TRUE))
})

# ============================================================================
# RLM Tools Unit Tests
# ============================================================================

test_that("create_rlm_prelude generates valid R code", {
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  expect_type(prelude, "character")
  # Parse should succeed
  expect_no_error(parse(text = prelude))
})

test_that("create_rlm_prelude includes SUBMIT function", {
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  expect_true(grepl("SUBMIT <- function", prelude, fixed = TRUE))
})

test_that("create_rlm_prelude includes peek function", {
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  expect_true(grepl("peek <- function", prelude, fixed = TRUE))
})

test_that("create_rlm_prelude includes search function", {
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  expect_true(grepl("search <- function", prelude, fixed = TRUE))
})

test_that("create_rlm_prelude includes rlm_query when sub_lm enabled", {
  prelude_with <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list()
  )

  prelude_without <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  # With sub_lm: should have working rlm_query
  expect_true(grepl("rlm_query_request", prelude_with, fixed = TRUE))

  # Without sub_lm: should have disabled rlm_query
  expect_true(grepl(
    "Recursive LLM queries are disabled",
    prelude_without,
    fixed = TRUE
  ))
})

test_that("create_rlm_prelude includes custom tools", {
  custom_tools <- list(
    my_tool = function(x) x * 2,
    another_tool = function(a, b) a + b
  )

  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = custom_tools
  )

  expect_true(grepl("my_tool <-", prelude, fixed = TRUE))
  expect_true(grepl("another_tool <-", prelude, fixed = TRUE))
})

test_that("create_rlm_prelude enforces multi-output SUBMIT shape", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list(),
    output_fields = c("answer", "confidence")
  )

  ok <- runner$execute(
    paste0(prelude, "\nSUBMIT('ok', 0.9)"),
    context = list()
  )
  expect_true(ok$success)
  expect_true(dsprrr:::is_rlm_final(ok$result))
  expect_equal(
    names(dsprrr:::extract_rlm_final(ok$result)),
    c("answer", "confidence")
  )

  bad <- runner$execute(
    paste0(prelude, "\nSUBMIT(answer = 'ok')"),
    context = list()
  )
  expect_false(bad$success)
  expect_true(grepl("missing outputs", bad$error, fixed = TRUE))
})

test_that("strip_rlm_code_fences removes markdown fences", {
  expect_equal(
    dsprrr:::strip_rlm_code_fences("```r\nx <- 1\nSUBMIT(x)\n```"),
    "x <- 1\nSUBMIT(x)"
  )

  expect_equal(
    dsprrr:::strip_rlm_code_fences("```\n1 + 1\n```"),
    "1 + 1"
  )

  expect_equal(
    dsprrr:::strip_rlm_code_fences("x <- 1\nx"),
    "x <- 1\nx"
  )
})

test_that("is_rlm_final detects rlm_final class", {
  # Regular value
  expect_false(dsprrr:::is_rlm_final(42))
  expect_false(dsprrr:::is_rlm_final("hello"))

  # rlm_final value
  final <- structure("answer", class = c("rlm_final", "character"))
  expect_true(dsprrr:::is_rlm_final(final))

  # Via attribute
  final2 <- "answer"
  attr(final2, "rlm_final") <- TRUE
  expect_true(dsprrr:::is_rlm_final(final2))
})

test_that("extract_rlm_final removes rlm_final class", {
  final <- structure("answer", class = c("rlm_final", "character"))
  attr(final, "rlm_final") <- TRUE

  extracted <- dsprrr:::extract_rlm_final(final)

  expect_false(dsprrr:::is_rlm_final(extracted))
  expect_equal(extracted, "answer")
  expect_null(attr(extracted, "rlm_final"))
})

# ============================================================================
# Error Handling and Edge Case Tests
# ============================================================================

test_that("RLMModule warns when max_iterations reached without SUBMIT", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 2
  )

  # LLM never calls SUBMIT
  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Step 1", code = "x <- 1"),
    list(reasoning = "Step 2", code = "y <- 2")
  ))

  expect_warning(
    rlm$forward(list(question = "test"), .llm = mock_llm),
    "reached max_iterations"
  )
})

test_that("RLMModule handles LLM response with missing code", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  # Mock LLM that returns invalid response (missing code)
  mock_llm <- list(
    clone = function() mock_llm,
    chat_structured = function(prompt, type, ...) {
      list(reasoning = "Thinking")
      # Missing 'code' field
    },
    chat = function(prompt, ...) "fallback"
  )

  expect_error(
    rlm$forward(list(question = "test"), .llm = mock_llm),
    "invalid"
  )
})

test_that("RLMModule handles LLM response with non-string code", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  # Mock LLM that returns code as number
  mock_llm <- list(
    clone = function() mock_llm,
    chat_structured = function(prompt, type, ...) {
      list(reasoning = "Thinking", code = 123)
    },
    chat = function(prompt, ...) "fallback"
  )

  expect_error(
    rlm$forward(list(question = "test"), .llm = mock_llm),
    "invalid"
  )
})

# ============================================================================
# rlm_query Batch Tests
# ============================================================================

with_mock_parallel_chat <- function(mock_fn, code) {
  ns <- asNamespace("ellmer")
  if (!exists("parallel_chat", envir = ns, inherits = FALSE)) {
    skip("ellmer::parallel_chat not available in this ellmer version")
  }
  old_fn <- get("parallel_chat", envir = ns, inherits = FALSE)

  unlockBinding("parallel_chat", ns)
  assign("parallel_chat", mock_fn, envir = ns)
  lockBinding("parallel_chat", ns)

  on.exit(
    {
      unlockBinding("parallel_chat", ns)
      assign("parallel_chat", old_fn, envir = ns)
      lockBinding("parallel_chat", ns)
    },
    add = TRUE
  )

  force(code)
}

test_that("is_rlm_query_request detects query requests", {
  regular <- 42
  expect_false(dsprrr:::is_rlm_query_request(regular))

  request <- structure(
    list(query = "test", context = NULL, batch = FALSE),
    class = "rlm_query_request"
  )
  expect_true(dsprrr:::is_rlm_query_request(request))
})

test_that("rlm_query_batch generates batch request marker", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)

  # Execute prelude in subprocess to test rlm_query_batch
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list()
  )

  result <- runner$execute(
    paste0(prelude, "\nrlm_query_batch(c('q1', 'q2'))"),
    context = list()
  )

  expect_true(result$success)
  expect_s3_class(result$result, "rlm_query_request")
  expect_true(result$result$batch)
  expect_equal(result$result$queries, c("q1", "q2"))
})

test_that("llm_query_batched works and rlm_query_batch alias is preserved", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list()
  )

  result_primary <- runner$execute(
    paste0(prelude, "\nllm_query_batched(c('q1', 'q2'))"),
    context = list()
  )
  expect_true(result_primary$success)
  expect_s3_class(result_primary$result, "rlm_query_request")
  expect_true(result_primary$result$batch)

  result_alias <- runner$execute(
    paste0(prelude, "\nrlm_query_batch(c('q1', 'q2'))"),
    context = list()
  )
  expect_true(result_alias$success)
  expect_s3_class(result_alias$result, "rlm_query_request")
  expect_true(result_alias$result$batch)
})

test_that("rlm_query_batch validates queries is character", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)

  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list()
  )

  result <- runner$execute(
    paste0(prelude, "\nrlm_query_batch(123)"),
    context = list()
  )

  expect_false(result$success)
  expect_true(grepl("character vector", result$error, fixed = TRUE))
})

test_that("rlm_query_batch validates slices length", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)

  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list()
  )

  result <- runner$execute(
    paste0(prelude, "\nrlm_query_batch(c('q1', 'q2'), slices = c('s1'))"),
    context = list()
  )

  expect_false(result$success)
  expect_true(grepl("same length", result$error, fixed = TRUE))
})

test_that("process_rlm_query_batch uses bounded parallelism and preserves order", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    sub_lm = list()
  )

  call_counter <- new.env(parent = emptyenv())
  call_counter$count <- 0L
  request <- list(queries = c("q1", "q2", "q3"), slices = NULL, batch = TRUE)
  captured <- new.env(parent = emptyenv())

  make_fake_chat <- function(text) {
    list(last_turn = function() list(text = text))
  }

  withr::local_options(list(dsprrr.rlm_batch_max_active = 2L))
  warning_message <- NULL
  result <- withCallingHandlers(
    with_mock_parallel_chat(
      function(
        chat,
        prompts,
        max_active = 10,
        rpm = 500,
        on_error = c("return", "continue", "stop")
      ) {
        captured$prompts <- prompts
        captured$max_active <- max_active
        captured$on_error <- on_error
        list(
          make_fake_chat("first"),
          simpleError("boom"),
          make_fake_chat("third")
        )
      },
      rlm$.__enclos_env__$private$process_rlm_query_batch(request, call_counter)
    ),
    warning = function(w) {
      warning_message <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(call_counter$count, 3L)
  expect_equal(captured$max_active, 2L)
  expect_equal(captured$prompts, as.list(c("q1", "q2", "q3")))
  expect_equal(captured$on_error, "return")
  expect_match(warning_message, "Some batch queries failed")

  expect_false(result$success)
  expect_match(result$error, "Query 2: boom")
  expect_match(result$formatted_output, "Query 1 result: first")
  expect_match(result$formatted_output, "Query 2 result: \\[Error: boom\\]")
  expect_match(result$formatted_output, "Query 3 result: third")

  pos1 <- regexpr("Query 1 result: first", result$formatted_output)[1]
  pos2 <- regexpr("Query 2 result: \\[Error: boom\\]", result$formatted_output)[
    1
  ]
  pos3 <- regexpr("Query 3 result: third", result$formatted_output)[1]
  expect_true(pos1 < pos2 && pos2 < pos3)
})

test_that("process_rlm_query_batch does not retry sequentially after parallel infrastructure failure", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10)
  chat_calls <- 0L
  sub_lm <- list(
    chat = function(prompt, ...) {
      chat_calls <<- chat_calls + 1L
      paste0("ok: ", prompt)
    }
  )

  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    sub_lm = sub_lm
  )

  call_counter <- new.env(parent = emptyenv())
  call_counter$count <- 0L
  request <- list(
    queries = c("good", "better", "fine"),
    slices = NULL,
    batch = TRUE
  )

  expect_warning(
    result <- with_mock_parallel_chat(
      function(
        chat,
        prompts,
        max_active = 10,
        rpm = 500,
        on_error = c("return", "continue", "stop")
      ) {
        stop("parallel unavailable")
      },
      rlm$.__enclos_env__$private$process_rlm_query_batch(request, call_counter)
    ),
    "Some batch queries failed"
  )

  expect_equal(call_counter$count, 3L)
  expect_equal(chat_calls, 0L)
  expect_false(result$success)
  expect_match(result$error, "queries not retried")
  expect_match(
    result$formatted_output,
    "Query 1 result: \\[Error: Parallel batch infrastructure error"
  )
  expect_match(
    result$formatted_output,
    "Query 2 result: \\[Error: Parallel batch infrastructure error"
  )
  expect_match(
    result$formatted_output,
    "Query 3 result: \\[Error: Parallel batch infrastructure error"
  )
})
