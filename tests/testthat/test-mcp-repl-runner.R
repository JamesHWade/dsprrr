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

test_that("mcp_repl_runner refuses an unsandboxed policy", {
  expect_snapshot(
    error = TRUE,
    mcp_repl_runner(
      repl = function(input, timeout_ms) input,
      sandbox = "off"
    )
  )
})
