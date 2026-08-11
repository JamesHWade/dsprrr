test_that("injected mcp_repl_runner is persistent but unverified", {
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
  expect_identical(policy$backend, "external-mcp-repl")
  expect_identical(policy$trust, "caller-managed")
  expect_identical(policy$sandboxed, FALSE)
  expect_identical(policy$network_access, "unknown")
  expect_identical(policy$sandbox_enforcement, "unverified")
  expect_identical(policy$persistent, TRUE)
  output <- capture.output(print(runner), type = "message")
  expect_match(paste(output, collapse = "\n"), "sandbox unverified")
  expect_false(grepl(
    "OS-sandboxed for untrusted code",
    paste(output, collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("mcp_repl_runner realizes arguments for mcptools wrappers", {
  requests <- list()
  call_tool <- function(input, timeout_ms) {
    requests[[length(requests) + 1L]] <<- list(
      input = input,
      timeout_ms = timeout_ms
    )
    list(result = list(content = list(list(type = "text", text = "ok"))))
  }
  repl <- function(input, timeout_ms) {
    call_info <- match.call()
    tool_args <- lapply(call_info[-1L], eval)
    do.call(call_tool, tool_args)
  }

  runner <- mcp_repl_runner(repl = repl, timeout = 2)
  result <- runner$execute("1 + 1")

  expect_true(result$success)
  expect_identical(result$result, "ok")
  expect_identical(requests[[1L]]$timeout_ms, 2000L)

  runner$reset()

  expect_identical(requests[[2L]]$input, "\u0004")
  expect_identical(requests[[2L]]$timeout_ms, 2000L)
})

test_that("mcp_repl_runner overwrites persistent context even when empty", {
  inputs <- character()
  repl <- function(input, timeout_ms) {
    inputs <<- c(inputs, input)
    list(result = list(content = list(list(type = "text", text = "ok"))))
  }
  runner <- mcp_repl_runner(repl = repl)

  runner$execute("invisible(NULL)", context = list(secret = "sensitive"))
  runner$execute("invisible(NULL)", context = list())

  expect_length(inputs, 2L)
  expect_match(inputs[[1L]], "sensitive", fixed = TRUE)
  expect_match(inputs[[2L]], "\\.context <-")
  expect_match(inputs[[2L]], 'fromJSON("[]",', fixed = TRUE)
  expect_false(grepl("sensitive", inputs[[2L]], fixed = TRUE))
})

test_that("mcp_repl_input compresses large requests without changing them", {
  context <- list(
    payload = strrep("replay payload ", 700L),
    label = "na\u00efve caf\u00e9"
  )
  code <- paste(
    "local({",
    "  value <- .context",
    "  base::rm(.context, envir = base::globalenv())",
    "  value",
    "})",
    sep = "\n"
  )

  input <- dsprrr:::mcp_repl_input(code, context)

  expect_match(input, "base::memDecompress", fixed = TRUE)
  expect_lte(
    dsprrr:::mcp_repl_request_bytes(input),
    dsprrr:::.mcp_repl_request_limit_bytes
  )
  value <- eval(parse(text = input), envir = .GlobalEnv)
  expect_identical(value, context)
  expect_false(exists(".context", envir = .GlobalEnv, inherits = FALSE))
})

test_that("mcp_repl_input preserves fitting high-entropy requests", {
  withr::local_seed(125L)
  alphabet <- strsplit(
    paste0(
      "!#$%&()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      "[]^_abcdefghijklmnopqrstuvwxyz{|}~"
    ),
    "",
    fixed = TRUE
  )[[1L]]
  payload <- paste(sample(alphabet, 6040L, replace = TRUE), collapse = "")
  code <- paste0("#", payload, "\n1 + 1")
  expected <- paste0(
    '.context <- jsonlite::fromJSON("[]", simplifyVector = FALSE)\n',
    code,
    "\n"
  )

  expect_gt(nchar(expected, type = "bytes"), 6000L)
  expect_lte(
    dsprrr:::mcp_repl_request_bytes(expected),
    dsprrr:::.mcp_repl_request_limit_bytes
  )
  expect_identical(dsprrr:::mcp_repl_input(code, list()), expected)
})

test_that("mcp_repl_input rejects oversized encoded requests before transport", {
  calls <- 0L
  repl <- function(input, timeout_ms) {
    calls <<- calls + 1L
    "ok"
  }
  runner <- mcp_repl_runner(repl = repl)
  result <- local({
    testthat::local_mocked_bindings(
      mcp_repl_request_bytes = function(input) {
        dsprrr:::.mcp_repl_request_limit_bytes + 1L
      },
      .package = "dsprrr"
    )
    dsprrr:::execute_code_runner(runner, "1 + 1")
  })

  expect_s3_class(result, "dsprrr_mcp_repl_input_size_error")
  expect_false(result$success)
  expect_identical(result$error_type, "execution")
  expect_true(result$retryable)
  expect_identical(calls, 0L)
  expect_false(runner$terminal)

  recovered <- runner$execute("1 + 1")
  expect_true(recovered$success)
  expect_identical(recovered$result, "ok")
  expect_identical(calls, 1L)
})

test_that("dsprrr-managed mcp_repl_runner advertises its derived policy", {
  repl <- function(input, timeout_ms) list(result = list(content = list()))
  testthat::local_mocked_bindings(
    mcp_repl_tool = function(...) {
      list(repl = repl, close = function() invisible(NULL))
    },
    .package = "dsprrr"
  )

  runner <- mcp_repl_runner()
  policy <- runner$policy()

  expect_identical(policy$backend, "posit-mcp-repl")
  expect_identical(policy$trust, "untrusted-code")
  expect_identical(policy$sandboxed, TRUE)
  expect_identical(policy$process_isolation, TRUE)
  expect_identical(policy$network_access, "disabled")
  expect_identical(policy$sandbox_enforcement, "operating-system")
})

test_that("managed setup closes servers with missing repl tools", {
  skip_if_not_installed("mcptools")
  closes <- 0L
  testthat::local_mocked_bindings(
    mcp_tools = function(config) list(),
    .package = "mcptools"
  )
  testthat::local_mocked_bindings(
    mcp_repl_managed_closer = function(server_name) {
      function() {
        closes <<- closes + 1L
        invisible(NULL)
      }
    },
    .package = "dsprrr"
  )

  expect_error(
    dsprrr:::mcp_repl_tool(
      command = unname(Sys.which("true")),
      interpreter = "r",
      sandbox = "workspace-write",
      oversized_output = "files",
      extra_args = character()
    ),
    "did not expose exactly one"
  )
  expect_identical(closes, 1L)
})

test_that("managed setup closes servers with duplicate repl tools", {
  skip_if_not_installed("mcptools")
  closes <- 0L
  repl <- ellmer::tool(
    function(input, timeout_ms) "ok",
    description = "test repl",
    arguments = list(
      input = ellmer::type_string(),
      timeout_ms = ellmer::type_integer()
    ),
    name = "repl"
  )
  testthat::local_mocked_bindings(
    mcp_tools = function(config) list(repl, repl),
    .package = "mcptools"
  )
  testthat::local_mocked_bindings(
    mcp_repl_managed_closer = function(server_name) {
      function() {
        closes <<- closes + 1L
        invisible(NULL)
      }
    },
    .package = "dsprrr"
  )

  expect_error(
    dsprrr:::mcp_repl_tool(
      command = unname(Sys.which("true")),
      interpreter = "r",
      sandbox = "workspace-write",
      oversized_output = "files",
      extra_args = character()
    ),
    "did not expose exactly one"
  )
  expect_identical(closes, 1L)
})

test_that("managed setup best-effort closes after lifecycle capture failure", {
  skip_if_not_installed("mcptools")
  cleanup_calls <- 0L
  repl <- ellmer::tool(
    function(input, timeout_ms) "ok",
    description = "test repl",
    arguments = list(
      input = ellmer::type_string(),
      timeout_ms = ellmer::type_integer()
    ),
    name = "repl"
  )
  testthat::local_mocked_bindings(
    mcp_tools = function(config) list(repl),
    .package = "mcptools"
  )
  testthat::local_mocked_bindings(
    mcp_repl_managed_closer = function(server_name) {
      cli::cli_abort(
        "lifecycle unavailable",
        class = "dsprrr_mcp_repl_lifecycle_error"
      )
    },
    mcp_repl_best_effort_close = function(server_name, ...) {
      cleanup_calls <<- cleanup_calls + 1L
      NULL
    },
    .package = "dsprrr"
  )

  expect_error(
    dsprrr:::mcp_repl_tool(
      command = unname(Sys.which("true")),
      interpreter = "r",
      sandbox = "workspace-write",
      oversized_output = "files",
      extra_args = character()
    ),
    class = "dsprrr_mcp_repl_lifecycle_error"
  )
  expect_identical(cleanup_calls, 1L)
})

test_that("managed setup cleans partial registration when startup errors", {
  skip_if_not_installed("mcptools")
  cleanup_calls <- 0L
  testthat::local_mocked_bindings(
    mcp_tools = function(config) stop("startup failed after registration"),
    .package = "mcptools"
  )
  testthat::local_mocked_bindings(
    mcp_repl_best_effort_close = function(server_name, ...) {
      cleanup_calls <<- cleanup_calls + 1L
      NULL
    },
    .package = "dsprrr"
  )

  expect_error(
    dsprrr:::mcp_repl_tool(
      command = unname(Sys.which("true")),
      interpreter = "r",
      sandbox = "workspace-write",
      oversized_output = "files",
      extra_args = character()
    ),
    "startup failed after registration",
    fixed = TRUE
  )
  expect_identical(cleanup_calls, 1L)
})

test_that("managed setup kills only its process after partial startup", {
  skip_if_not_installed("mcptools")
  kills <- 0L
  preexisting_kills <- 0L
  preexisting <- new.env(parent = emptyenv())
  preexisting$is_alive <- function() TRUE
  preexisting$kill <- function() {
    preexisting_kills <<- preexisting_kills + 1L
  }
  spawned <- new.env(parent = emptyenv())
  spawned$is_alive <- function() TRUE
  spawned$kill <- function() {
    kills <<- kills + 1L
  }
  registry <- new.env(parent = emptyenv())
  registry$mcp_servers <- list()
  registry$server_processes <- list(preexisting = preexisting)

  testthat::local_mocked_bindings(
    mcp_repl_registry = function() registry,
    .package = "dsprrr"
  )
  testthat::local_mocked_bindings(
    mcp_tools = function(config) {
      parsed <- jsonlite::read_json(config, simplifyVector = TRUE)
      server <- parsed$mcpServers[[1L]]
      process_key <- paste(c(server$command, server$args), collapse = " ")
      registry$server_processes <- c(
        registry$server_processes,
        stats::setNames(list(spawned), process_key)
      )
      stop("startup failed after process creation")
    },
    .package = "mcptools"
  )

  expect_error(
    dsprrr:::mcp_repl_tool(
      command = unname(Sys.which("true")),
      interpreter = "r",
      sandbox = "workspace-write",
      oversized_output = "files",
      extra_args = character()
    ),
    "startup failed after process creation",
    fixed = TRUE
  )
  expect_identical(kills, 1L)
  expect_identical(preexisting_kills, 0L)
  expect_length(registry$server_processes, 1L)
  expect_identical(registry$server_processes[[1L]], preexisting)
})

test_that("best-effort cleanup refuses ambiguous new processes", {
  kills <- 0L
  process <- function() {
    value <- new.env(parent = emptyenv())
    value$is_alive <- function() TRUE
    value$kill <- function() {
      kills <<- kills + 1L
    }
    value
  }
  registry <- new.env(parent = emptyenv())
  registry$mcp_servers <- list()
  registry$server_processes <- stats::setNames(
    list(process(), process()),
    c("expected command", "expected command")
  )
  testthat::local_mocked_bindings(
    mcp_repl_registry = function() registry,
    .package = "dsprrr"
  )

  result <- dsprrr:::mcp_repl_best_effort_close(
    "not-registered",
    process_snapshot = list(),
    process_key = "expected command"
  )

  expect_s3_class(result, "condition")
  expect_match(
    conditionMessage(result),
    "ambiguous process state",
    fixed = TRUE
  )
  expect_identical(kills, 0L)
  expect_length(registry$server_processes, 2L)
})

test_that("managed closer kills and prunes after graceful close fails", {
  kills <- 0L
  process <- new.env(parent = emptyenv())
  process$is_alive <- function() TRUE
  process$kill <- function() {
    kills <<- kills + 1L
  }
  transport <- list(process = process)
  registry <- new.env(parent = emptyenv())
  registry$mcp_servers <- list(
    managed = list(transport = transport, process = process)
  )
  registry$server_processes <- list(managed = process)

  expect_invisible(dsprrr:::mcp_repl_close_captured(
    registry = registry,
    server_name = "managed",
    transport = transport,
    process = process,
    close_transport = function(transport) stop("graceful close failed")
  ))
  expect_identical(kills, 1L)
  expect_null(registry$mcp_servers$managed)
  expect_length(registry$server_processes, 0L)
})

test_that("managed runner closures are process-specific and idempotent", {
  first_closes <- 0L
  second_closes <- 0L
  first <- dsprrr:::McpReplRunner$new(
    repl = function(input, timeout_ms) "first",
    timeout = 1,
    max_output_chars = 100L,
    sandbox = "workspace-write",
    sandbox_verified = TRUE,
    oversized_output = "files",
    close_connection = function() first_closes <<- first_closes + 1L,
    connection_owned = TRUE
  )
  second <- dsprrr:::McpReplRunner$new(
    repl = function(input, timeout_ms) "second",
    timeout = 1,
    max_output_chars = 100L,
    sandbox = "workspace-write",
    sandbox_verified = TRUE,
    oversized_output = "files",
    close_connection = function() second_closes <<- second_closes + 1L,
    connection_owned = TRUE
  )

  first$close()
  first$close()
  expect_identical(first_closes, 1L)
  expect_identical(second_closes, 0L)
  expect_identical(second$execute("1 + 1")$result, "second")
  second$close()
  expect_identical(second_closes, 1L)
})

test_that("a failed managed close still makes the runner terminal", {
  close_calls <- 0L
  runner <- dsprrr:::McpReplRunner$new(
    repl = function(input, timeout_ms) "result",
    timeout = 1,
    max_output_chars = 100L,
    sandbox = "workspace-write",
    sandbox_verified = TRUE,
    oversized_output = "files",
    close_connection = function() {
      close_calls <<- close_calls + 1L
      stop("transport close failed")
    },
    connection_owned = TRUE
  )

  expect_error(runner$close(), "transport close failed")
  expect_true(runner$closed)
  expect_invisible(runner$close())
  expect_identical(close_calls, 1L)
  expect_error(
    runner$execute("1 + 1"),
    class = "dsprrr_interpreter_closed_error"
  )
})

test_that("managed mcp-repl policy flags cannot be overridden", {
  repl <- function(input, timeout_ms) list(result = list(content = list()))
  reserved <- c(
    "--sandbox",
    "--sandbox=danger-full-access",
    "--add-writable-root=/tmp",
    "--add-writeable-root",
    "--add-allowed-domain=example.com",
    "--config=sandbox_workspace_write.network_access=true",
    "--config",
    "--worker-spec=unsafe.json",
    "--debug-repl"
  )
  for (argument in reserved) {
    expect_error(
      mcp_repl_runner(repl = repl, extra_args = argument),
      class = "dsprrr_mcp_extra_args_error",
      info = argument
    )
  }

  args <- dsprrr:::mcp_repl_server_args(
    extra_args = character(),
    sandbox = "workspace-write",
    oversized_output = "files",
    interpreter = "r"
  )
  expect_identical(
    utils::tail(args, 6L),
    c(
      "--sandbox",
      "workspace-write",
      "--oversized-output",
      "files",
      "--interpreter",
      "r"
    )
  )
})

test_that("sandbox-required consumers reject injected repl functions", {
  runner <- mcp_repl_runner(
    repl = function(input, timeout_ms) list(result = list(content = list()))
  )

  expect_error(
    dsprrr:::harness_validate_runner(runner, required = TRUE),
    class = "dsprrr_runner_sandbox_required"
  )
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
  expect_identical(result$error_type, "execution")
  expect_true(result$retryable)
  expect_false(runner$terminal)
  expect_identical(result$stdout, "object not found")
  expect_identical(utils::tail(inputs, 1L), "\u0004")
})

test_that("mcp_repl_runner rejects protocol-level reset failures", {
  repl <- function(input, timeout_ms) {
    list(result = list(isError = TRUE, content = list()))
  }
  runner <- mcp_repl_runner(repl = repl)

  expect_error(
    runner$reset(),
    class = "dsprrr_mcp_repl_reset_error"
  )
  expect_true(runner$terminal)
  expect_error(
    runner$execute("1 + 1"),
    class = "dsprrr_interpreter_terminal_error"
  )
})

test_that("mcp transport failures terminalize the interpreter session", {
  calls <- 0L
  runner <- mcp_repl_runner(repl = function(input, timeout_ms) {
    calls <<- calls + 1L
    stop("transport disconnected")
  })

  result <- runner$execute("1 + 1")

  expect_false(result$success)
  expect_identical(result$error_type, "interpreter")
  expect_false(result$retryable)
  expect_true(runner$terminal)
  expect_error(
    runner$execute("2 + 2"),
    class = "dsprrr_interpreter_terminal_error"
  )
  expect_identical(calls, 1L)
  expect_invisible(runner$shutdown())
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

test_that("mcp_repl_runner validates sandbox as one non-missing string", {
  repl <- function(input, timeout_ms) input
  for (sandbox in list(
    NA_character_,
    c("workspace-write", "workspace-write")
  )) {
    expect_error(
      mcp_repl_runner(repl = repl, sandbox = sandbox),
      class = "dsprrr_mcp_sandbox_error"
    )
  }
})

test_that("mcp_repl_runner refuses an unverifiable inherited sandbox", {
  expect_snapshot(
    error = TRUE,
    mcp_repl_runner(
      repl = function(input, timeout_ms) input,
      sandbox = "inherit-codex"
    )
  )
})

test_that("authenticated RLM traffic fails closed on file previews", {
  previews <- c(
    paste0(
      "head\n",
      "...[middle truncated; shown lines 1-2 and 99-100 of 100 total; ",
      "full output: /sandbox/output.txt]...\n",
      "tail"
    ),
    "...[middle truncated; ordered output bundle index: /sandbox/index.md]...",
    "...[middle truncated; output bundle images: /sandbox/images]..."
  )
  for (preview in previews) {
    repl <- function(input, timeout_ms) {
      list(result = list(content = list(list(type = "text", text = preview))))
    }
    runner <- mcp_repl_runner(repl = repl, oversized_output = "files")

    result <- runner$execute("SUBMIT('ok')", .control_nonce = "current")
    expect_s3_class(result, "dsprrr_mcp_repl_transport_error")
    expect_false(result$success)
    expect_match(result$error, "file preview", fixed = TRUE)
  }

  preview <- previews[[1L]]
  repl <- function(input, timeout_ms) {
    list(result = list(content = list(list(type = "text", text = preview))))
  }
  runner <- mcp_repl_runner(repl = repl, oversized_output = "files")
  ordinary <- runner$execute("cat('ordinary output')")

  expect_identical(ordinary$success, TRUE)
  expect_identical(ordinary$result, preview)
})

test_that("Flex normal inline control is decoded before display truncation", {
  encode_frame <- function(
    nonce,
    kind = "final",
    payload = list(output = list(answer = "ok"))
  ) {
    envelope <- list(
      .dsprrr_flex_control = TRUE,
      version = 1L,
      nonce = nonce,
      kind = kind,
      payload = payload
    )
    json <- jsonlite::toJSON(
      envelope,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      dataframe = "rows",
      digits = NA
    )
    paste0(
      dsprrr:::.flex_code_control_prefix,
      gsub(
        "[[:space:]]",
        "",
        jsonlite::base64_enc(charToRaw(as.character(json)))
      )
    )
  }

  nonce <- "current-step"
  frame <- encode_frame(nonce)
  raw_text <- paste(strrep("noise", 20L), frame, sep = "\n")
  repl <- function(input, timeout_ms) {
    list(result = list(content = list(list(type = "text", text = raw_text))))
  }
  runner <- mcp_repl_runner(repl = repl, max_output_chars = 20L)

  result <- runner$execute(
    "invisible(NULL)",
    .control_nonce = nonce,
    .control_protocol = "flex",
    .control_max_bytes = runner$control_frame_limit
  )

  expect_true(result$success)
  expect_identical(result$result$.dsprrr_flex_control, TRUE)
  expect_identical(result$result$nonce, nonce)
  expect_identical(result$result$payload$output$answer, "ok")
  expect_match(result$stdout, "output truncated by dsprrr", fixed = TRUE)
  expect_identical(
    dsprrr:::flex_code_decode_control(list(result$result), nonce),
    result$result
  )

  invalid <- list(
    missing = "ordinary output",
    stale = encode_frame("previous-step"),
    truncated = substr(frame, 1L, nchar(frame) - 8L),
    malformed = paste0(dsprrr:::.flex_code_control_prefix, "not-base64!"),
    duplicate = paste(frame, frame, sep = "\n"),
    unknown_kind = encode_frame(nonce, kind = "unknown"),
    non_list_payload = encode_frame(nonce, payload = "invalid"),
    oversized = encode_frame(
      nonce,
      payload = list(output = list(answer = strrep("x", 2000L)))
    )
  )
  for (name in names(invalid)) {
    raw_text <- invalid[[name]]
    repl <- function(input, timeout_ms) {
      list(result = list(content = list(list(type = "text", text = raw_text))))
    }
    runner <- mcp_repl_runner(repl = repl, max_output_chars = 100000L)
    max_bytes <- if (identical(name, "oversized")) {
      200L
    } else {
      runner$control_frame_limit
    }

    result <- runner$execute(
      "invisible(NULL)",
      .control_nonce = nonce,
      .control_protocol = "flex",
      .control_max_bytes = max_bytes
    )

    expect_s3_class(result, "dsprrr_mcp_repl_transport_error")
    expect_false(result$success, info = name)
    expect_null(result$result, info = name)
  }
})

test_that("Flex recovers one bounded current-step frame from file previews", {
  encode_frame <- function(nonce, value = "ok", kind = "final") {
    payload <- if (identical(kind, "overflow")) {
      list(stage = "final output", encoded_bytes = 4001L, frame_limit = 3000L)
    } else {
      list(output = list(answer = value))
    }
    envelope <- list(
      .dsprrr_flex_control = TRUE,
      version = 1L,
      nonce = nonce,
      kind = kind,
      payload = payload
    )
    json <- jsonlite::toJSON(
      envelope,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      dataframe = "rows",
      digits = NA
    )
    paste0(
      dsprrr:::.flex_code_control_prefix,
      gsub(
        "[[:space:]]",
        "",
        jsonlite::base64_enc(charToRaw(as.character(json)))
      )
    )
  }
  preview <- function(frame) {
    paste(
      "stderr: compiler warning",
      "...[full output: /sandbox/output-0001/transcript.txt]...",
      paste0(frame, " > "),
      "...[full output: /sandbox/output-0002/transcript.txt]...",
      sep = "\n"
    )
  }
  nonce <- "current-step"
  raw_text <- preview(encode_frame(nonce))
  repl <- function(input, timeout_ms) {
    list(result = list(content = list(list(type = "text", text = raw_text))))
  }
  runner <- mcp_repl_runner(
    repl = repl,
    max_output_chars = 20L,
    oversized_output = "files"
  )

  result <- runner$execute(
    "invisible(NULL)",
    .control_nonce = nonce,
    .control_protocol = "flex",
    .control_max_bytes = runner$control_frame_limit
  )

  expect_identical(result$success, TRUE)
  expect_identical(result$result$.dsprrr_flex_control, TRUE)
  expect_identical(result$result$nonce, nonce)
  expect_identical(result$result$payload$output$answer, "ok")
  expect_match(result$stdout, "output truncated by dsprrr", fixed = TRUE)

  rlm_result <- runner$execute(
    "invisible(NULL)",
    .control_nonce = nonce
  )
  expect_s3_class(rlm_result, "dsprrr_mcp_repl_transport_error")
  expect_identical(rlm_result$success, FALSE)

  raw_text <- preview(encode_frame(nonce, kind = "overflow"))
  overflow_result <- runner$execute(
    "invisible(NULL)",
    .control_nonce = nonce,
    .control_protocol = "flex",
    .control_max_bytes = runner$control_frame_limit
  )
  expect_identical(overflow_result$success, TRUE)
  expect_identical(overflow_result$result$kind, "overflow")
  expect_error(
    dsprrr:::flex_code_decode_control(list(overflow_result$result), nonce),
    class = "dsprrr_flex_bridge_frame_size_error"
  )
})

test_that("Flex file-preview recovery rejects ambiguous control output", {
  encode_frame <- function(nonce, value = "ok") {
    envelope <- list(
      .dsprrr_flex_control = TRUE,
      version = 1L,
      nonce = nonce,
      kind = "final",
      payload = list(output = list(answer = value))
    )
    json <- jsonlite::toJSON(
      envelope,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      dataframe = "rows",
      digits = NA
    )
    paste0(
      dsprrr:::.flex_code_control_prefix,
      gsub(
        "[[:space:]]",
        "",
        jsonlite::base64_enc(charToRaw(as.character(json)))
      )
    )
  }
  preview <- function(body) {
    paste(
      "...[full output: /sandbox/output-0001/transcript.txt]...",
      body,
      "...[full output: /sandbox/output-0002/transcript.txt]...",
      sep = "\n"
    )
  }
  nonce <- "current-step"
  current <- encode_frame(nonce)
  cases <- list(
    stale = encode_frame("previous-step"),
    missing = "ordinary output",
    truncated = substr(current, 1L, nchar(current) - 8L),
    invalid = paste0(dsprrr:::.flex_code_control_prefix, "not-base64!"),
    duplicate = paste(current, current, sep = "\n"),
    oversized = encode_frame(nonce, strrep("x", 4000L))
  )

  for (name in names(cases)) {
    raw_text <- preview(cases[[name]])
    repl <- function(input, timeout_ms) {
      list(result = list(content = list(list(type = "text", text = raw_text))))
    }
    runner <- mcp_repl_runner(repl = repl, oversized_output = "files")

    result <- runner$execute(
      "invisible(NULL)",
      .control_nonce = nonce,
      .control_protocol = "flex",
      .control_max_bytes = runner$control_frame_limit
    )

    expect_s3_class(result, "dsprrr_mcp_repl_transport_error")
    expect_identical(result$success, FALSE)
  }

  bundle_markers <- c(
    "...[middle truncated; ordered output bundle index: /sandbox/index.md]...",
    "...[middle truncated; output bundle images: /sandbox/images]...",
    "...[middle truncated; output bundle unavailable]..."
  )
  for (marker in bundle_markers) {
    raw_text <- paste(marker, current, sep = "\n")
    repl <- function(input, timeout_ms) {
      list(result = list(content = list(list(type = "text", text = raw_text))))
    }
    runner <- mcp_repl_runner(repl = repl, oversized_output = "files")

    result <- runner$execute(
      "invisible(NULL)",
      .control_nonce = nonce,
      .control_protocol = "flex"
    )

    expect_s3_class(result, "dsprrr_mcp_repl_transport_error")
    expect_identical(result$success, FALSE)
  }
})

test_that("Flex preview recovery does not bypass MCP errors or pagers", {
  nonce <- "current-step"
  envelope <- list(
    .dsprrr_flex_control = TRUE,
    version = 1L,
    nonce = nonce,
    kind = "final",
    payload = list(output = list(answer = "ok"))
  )
  json <- jsonlite::toJSON(envelope, auto_unbox = TRUE)
  frame <- paste0(
    dsprrr:::.flex_code_control_prefix,
    gsub(
      "[[:space:]]",
      "",
      jsonlite::base64_enc(charToRaw(as.character(json)))
    )
  )
  file_text <- paste(
    "...[full output: /sandbox/output.txt]...",
    frame,
    sep = "\n"
  )
  error_repl <- function(input, timeout_ms) {
    list(
      result = list(
        isError = TRUE,
        content = list(list(type = "text", text = file_text))
      )
    )
  }
  error_runner <- mcp_repl_runner(
    repl = error_repl,
    oversized_output = "files"
  )

  error_result <- error_runner$execute(
    "invisible(NULL)",
    .control_nonce = nonce,
    .control_protocol = "flex"
  )

  expect_identical(error_result$success, FALSE)
  expect_identical(error_result$error_type, "execution")
  expect_match(error_result$error, "execution error", fixed = TRUE)
  expect_null(error_result$result)

  inputs <- character()
  pager_repl <- function(input, timeout_ms) {
    inputs <<- c(inputs, input)
    if (identical(input, "\u0004")) {
      return(list(result = list(content = list())))
    }
    text <- paste(frame, "--More-- (2p)", sep = "\n")
    list(result = list(content = list(list(type = "text", text = text))))
  }
  pager_runner <- mcp_repl_runner(
    repl = pager_repl,
    oversized_output = "pager"
  )

  pager_result <- pager_runner$execute(
    "invisible(NULL)",
    .control_nonce = nonce,
    .control_protocol = "flex"
  )

  expect_s3_class(pager_result, "dsprrr_mcp_repl_transport_error")
  expect_identical(pager_result$success, FALSE)
  expect_identical(utils::tail(inputs, 1L), "\u0004")
})

test_that("authenticated RLM traffic resets an active mcp-repl pager", {
  inputs <- character()
  repl <- function(input, timeout_ms) {
    inputs <<- c(inputs, input)
    if (identical(input, "\u0004")) {
      return(list(result = list(content = list())))
    }
    text <- paste(
      "partial output",
      "--More-- (2p, 10.0%, @1..20/200)",
      sep = "\n"
    )
    list(result = list(content = list(list(type = "text", text = text))))
  }
  runner <- mcp_repl_runner(repl = repl, oversized_output = "pager")

  result <- runner$execute("SUBMIT('ok')", .control_nonce = "current")

  expect_s3_class(result, "dsprrr_mcp_repl_transport_error")
  expect_false(result$success)
  expect_match(result$error, "session was reset", fixed = TRUE)
  expect_identical(utils::tail(inputs, 1L), "\u0004")
})

test_that("authenticated RLM pager failures report reset failures accurately", {
  repl <- function(input, timeout_ms) {
    if (identical(input, "\u0004")) {
      stop("restart transport unavailable")
    }
    text <- paste("partial output", "--More-- (2p)", sep = "\n")
    list(result = list(content = list(list(type = "text", text = text))))
  }
  runner <- mcp_repl_runner(repl = repl, oversized_output = "pager")

  result <- runner$execute("SUBMIT('ok')", .control_nonce = "current")

  expect_s3_class(result, "dsprrr_mcp_repl_transport_error")
  expect_false(result$success)
  expect_match(result$error, "session could not be reset", fixed = TRUE)
  expect_identical(result$error_type, "interpreter")
  expect_true(runner$terminal)
  expect_false(grepl("session was reset", result$error, fixed = TRUE))
})

test_that("mcp-repl rejects control frames above its safe inline limit", {
  eval_repl <- function(input, timeout_ms) {
    text <- tryCatch(
      {
        env <- new.env(parent = baseenv())
        value <- eval(parse(text = input), envir = env)
        paste(capture.output(print(value)), collapse = "\n")
      },
      error = function(e) paste("Error:", conditionMessage(e))
    )
    list(result = list(content = list(list(type = "text", text = text))))
  }
  runner <- mcp_repl_runner(repl = eval_repl)
  nonce <- "bounded-control-frame"
  prelude <- dsprrr:::create_rlm_prelude(
    control_nonce = nonce,
    control_frame_limit = runner$control_frame_limit
  )

  result <- runner$execute(
    paste0(prelude, "\nSUBMIT(base::strrep('x', 4000L))"),
    .control_nonce = nonce
  )

  expect_s3_class(result, "dsprrr_mcp_repl_transport_error")
  expect_false(result$success)
  expect_match(result$error, "safe inline transport limit", fixed = TRUE)
})
