test_that("mcp_repl_runner implements a sandboxed persistent runner", {
  requests <- list()
  repl <- function(input, timeout_ms) {
    requests[[length(requests) + 1L]] <<- list(
      input = input,
      timeout_ms = timeout_ms
    )
    list(
      result = list(
        content = list(list(type = "text", text = "[1] 42"))
      )
    )
  }

  runner <- mcp_repl_runner(repl = repl, timeout = 2)
  result <- runner$execute(
    "sum(.context$values)",
    context = list(values = list(20, 22))
  )

  expect_s3_class(runner, "McpReplRunner")
  expect_identical(result$success, TRUE)
  expect_identical(result$result, "[1] 42")
  expect_match(requests[[1L]]$input, "\\.context <-")
  expect_match(requests[[1L]]$input, "sum\\(\\.context\\$values\\)")
  expect_identical(requests[[1L]]$timeout_ms, 2000L)

  policy <- runner$policy()
  expect_identical(policy$backend, "posit-mcp-repl")
  expect_identical(policy$trust, "untrusted-code")
  expect_identical(policy$sandboxed, TRUE)
  expect_identical(policy$network_access, "disabled")
  expect_identical(policy$persistent, TRUE)
})

test_that("mcp_repl_runner normalizes MCP errors and resets the session", {
  inputs <- character()
  repl <- function(input, timeout_ms) {
    inputs <<- c(inputs, input)
    if (identical(input, "\u0004")) {
      return(list(result = list(content = list())))
    }
    list(
      result = list(
        isError = TRUE,
        content = list(list(type = "text", text = "object not found"))
      )
    )
  }

  runner <- mcp_repl_runner(repl = repl)
  result <- runner$execute("missing_object")
  runner$reset()

  expect_identical(result$success, FALSE)
  expect_match(result$error, "execution error")
  expect_identical(result$stdout, "object not found")
  expect_identical(utils::tail(inputs, 1L), "\u0004")
})

test_that("mcp_repl_runner rounds timeout milliseconds up safely", {
  timeout_values <- integer()
  repl <- function(input, timeout_ms) {
    timeout_values <<- c(timeout_values, timeout_ms)
    list(result = list(content = list()))
  }
  runner <- mcp_repl_runner(repl = repl, timeout = 0.0001)

  runner$execute("1 + 1")
  runner$reset()

  expect_identical(timeout_values, c(1L, 1L))
})

test_that("mcp_repl_runner requires integer-valued output limits", {
  invalid_limits <- c(Inf, 10.5, .Machine$integer.max + 1)

  for (limit in invalid_limits) {
    expect_error(
      mcp_repl_runner(
        repl = function(input, timeout_ms) input,
        max_output_chars = limit
      ),
      "must be a positive integer",
      info = paste("max_output_chars =", limit)
    )
  }
})

test_that("mcp_repl_runner refuses an unsandboxed policy", {
  expect_snapshot(
    error = TRUE,
    mcp_repl_runner(
      repl = function(input, timeout_ms) input,
      sandbox = "off"
    )
  )
})
