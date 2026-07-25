#' Posit mcp-repl Code Runner
#'
#' @description
#' Creates a dsprrr code runner backed by Posit's
#' [`mcp-repl`](https://github.com/posit-dev/mcp-repl) MCP server.
#' `mcp-repl` keeps a long-lived R session and enforces its sandbox with
#' operating-system primitives. This makes it suitable for code proposed by an
#' optimizer or language model.
#'
#' @details
#' By default, `mcp_repl_runner()` starts `mcp-repl` through
#' [mcptools::mcp_tools()] with:
#'
#' - the R interpreter;
#' - the `workspace-write` sandbox;
#' - network access disabled by the sandbox; and
#' - oversized output written to sandbox-visible files.
#'
#' The sandbox is deliberately on by default. Setting `sandbox = "off"` is
#' rejected because this runner advertises itself as safe for untrusted code.
#' Use [r_code_runner()] explicitly for trusted-input-only subprocess
#' isolation.
#'
#' Supplying `repl` is useful for an already-managed MCP connection and for
#' deterministic tests. It must be a function with the mcp-repl tool contract:
#' `repl(input, timeout_ms)`.
#'
#' @param repl Optional function implementing the mcp-repl `repl` tool.
#' @param command Path or command name for the `mcp-repl` executable.
#' @param interpreter Interpreter passed to mcp-repl. Currently only `"r"` is
#'   supported by this runner.
#' @param sandbox mcp-repl sandbox policy. Defaults to `"workspace-write"`.
#'   `"inherit-codex"` may be used only when the MCP client propagates Codex
#'   sandbox metadata; [mcptools::mcp_tools()] does not currently do so.
#' @param timeout Maximum execution time in seconds.
#' @param max_output_chars Maximum number of output characters returned to the
#'   optimizer.
#' @param oversized_output mcp-repl oversized-output mode.
#' @param extra_args Additional command-line arguments passed to mcp-repl.
#'
#' @return An `McpReplRunner` implementing the dsprrr code-runner protocol.
#'
#' @export
#' @examples
#' \dontrun{
#' runner <- mcp_repl_runner()
#' runner$execute("mean(1:10)")
#' runner$reset()
#' }
mcp_repl_runner <- function(
  repl = NULL,
  command = "mcp-repl",
  interpreter = "r",
  sandbox = "workspace-write",
  timeout = 30,
  max_output_chars = 100000L,
  oversized_output = "files",
  extra_args = character()
) {
  if (
    !is.character(command) ||
      length(command) != 1L ||
      is.na(command) ||
      !nzchar(command)
  ) {
    cli::cli_abort("{.arg command} must be one non-empty string")
  }
  if (!identical(interpreter, "r")) {
    cli::cli_abort(
      "{.arg interpreter} must be {.val r} for a dsprrr R code runner"
    )
  }
  if (!sandbox %in% c("workspace-write", "inherit-codex")) {
    cli::cli_abort(c(
      "{.arg sandbox} must be {.val workspace-write} or {.val inherit-codex}",
      "i" = "Use {.fn r_code_runner} explicitly for trusted, unsandboxed code."
    ))
  }
  if (
    !is.numeric(timeout) ||
      length(timeout) != 1L ||
      is.na(timeout) ||
      !is.finite(timeout) ||
      timeout <= 0
  ) {
    cli::cli_abort("{.arg timeout} must be a positive number")
  }
  if (
    !is.numeric(max_output_chars) ||
      length(max_output_chars) != 1L ||
      is.na(max_output_chars) ||
      max_output_chars < 1
  ) {
    cli::cli_abort("{.arg max_output_chars} must be a positive integer")
  }
  if (!is.character(extra_args) || anyNA(extra_args)) {
    cli::cli_abort("{.arg extra_args} must be a character vector")
  }
  if (
    !is.character(oversized_output) ||
      length(oversized_output) != 1L ||
      is.na(oversized_output) ||
      !oversized_output %in% c("files", "pager")
  ) {
    cli::cli_abort(
      "{.arg oversized_output} must be {.val files} or {.val pager}"
    )
  }

  if (is.null(repl)) {
    repl <- mcp_repl_tool(
      command = command,
      interpreter = interpreter,
      sandbox = sandbox,
      oversized_output = oversized_output,
      extra_args = extra_args
    )
  }
  if (!is.function(repl)) {
    cli::cli_abort(
      "{.arg repl} must be a function with arguments {.arg input} and {.arg timeout_ms}"
    )
  }

  McpReplRunner$new(
    repl = repl,
    timeout = timeout,
    max_output_chars = as.integer(max_output_chars),
    sandbox = sandbox
  )
}

mcp_repl_tool <- function(
  command,
  interpreter,
  sandbox,
  oversized_output,
  extra_args
) {
  rlang::check_installed(
    "mcptools",
    reason = "to connect R to the Posit mcp-repl MCP server"
  )
  command_path <- Sys.which(command)
  if (!nzchar(command_path)) {
    cli::cli_abort(c(
      "Could not find the {.code mcp-repl} executable",
      "i" = "Install {.pkg posit-mcp-repl}, or supply an existing {.arg repl} tool function.",
      "i" = "See {.url https://github.com/posit-dev/mcp-repl}."
    ))
  }

  server_name <- paste0(
    "dsprrr_mcp_repl_",
    digest::digest(
      list(Sys.getpid(), unclass(Sys.time()), tempfile()),
      algo = "xxhash64"
    )
  )
  config_path <- tempfile("dsprrr-mcp-repl-", fileext = ".json")
  on.exit(unlink(config_path), add = TRUE)

  args <- c(
    "--sandbox",
    sandbox,
    "--oversized-output",
    oversized_output,
    "--interpreter",
    interpreter,
    extra_args
  )
  config <- list(mcpServers = list())
  config$mcpServers[[server_name]] <- list(
    command = unname(command_path),
    args = args
  )
  jsonlite::write_json(
    config,
    path = config_path,
    auto_unbox = TRUE,
    pretty = TRUE
  )

  tools <- mcptools::mcp_tools(config = config_path)
  names <- vapply(
    tools,
    function(tool) {
      tryCatch(as.character(tool@name)[[1L]], error = function(e) "")
    },
    character(1)
  )
  repl_index <- which(names == "repl")
  if (length(repl_index) != 1L) {
    cli::cli_abort(c(
      "The configured mcp-repl server did not expose exactly one {.code repl} tool",
      "i" = "Exposed tools: {.val {names[nzchar(names)]}}"
    ))
  }
  tools[[repl_index]]
}

#' mcp-repl Runner
#'
#' @description
#' R6 implementation of the dsprrr code-runner protocol for Posit mcp-repl.
#'
#' @noRd
McpReplRunner <- R6::R6Class(
  "McpReplRunner",
  public = list(
    repl = NULL,
    timeout = NULL,
    max_output_chars = NULL,
    sandbox = NULL,

    initialize = function(
      repl,
      timeout,
      max_output_chars,
      sandbox
    ) {
      self$repl <- repl
      self$timeout <- timeout
      self$max_output_chars <- max_output_chars
      self$sandbox <- sandbox
    },

    execute = function(code, context = list()) {
      if (!is.character(code) || length(code) != 1L || is.na(code)) {
        cli::cli_abort("{.arg code} must be a single non-missing string")
      }
      if (!is.list(context)) {
        cli::cli_abort("{.arg context} must be a list")
      }

      input <- mcp_repl_input(code, context)
      started_at <- Sys.time()
      response <- tryCatch(
        self$repl(
          input = input,
          timeout_ms = as.integer(self$timeout * 1000)
        ),
        error = function(e) e
      )
      duration_ms <- as.numeric(
        difftime(Sys.time(), started_at, units = "secs")
      ) *
        1000

      if (inherits(response, "error")) {
        return(mcp_repl_error_result(
          conditionMessage(response),
          duration_ms = duration_ms
        ))
      }

      normalized <- mcp_repl_normalize_response(response)
      text <- mcp_repl_truncate(
        normalized$text,
        self$max_output_chars
      )
      if (!is.null(normalized$error)) {
        return(mcp_repl_error_result(
          normalized$error,
          stdout = text,
          duration_ms = duration_ms
        ))
      }

      list(
        success = TRUE,
        result = text,
        stdout = text,
        stderr = "",
        messages = "",
        warnings = "",
        error = NULL,
        duration_ms = round(duration_ms, 2)
      )
    },

    reset = function() {
      response <- tryCatch(
        self$repl(
          input = "\u0004",
          timeout_ms = as.integer(self$timeout * 1000)
        ),
        error = function(e) e
      )
      if (inherits(response, "error")) {
        cli::cli_abort(c(
          "Could not reset the mcp-repl session",
          "x" = conditionMessage(response)
        ))
      }
      invisible(self)
    },

    policy = function() {
      list(
        backend = "posit-mcp-repl",
        trust = "untrusted-code",
        sandboxed = TRUE,
        process_isolation = TRUE,
        persistent = TRUE,
        filesystem_access = self$sandbox,
        network_access = "disabled",
        sandbox_enforcement = "operating-system"
      )
    },

    print = function() {
      cli::cli_h3("Posit mcp-repl Runner")
      cli::cli_bullets(c(
        "*" = "Sandbox: {.val {self$sandbox}}",
        "*" = "Timeout: {.val {self$timeout}} seconds",
        "*" = "Persistent R session: {.val TRUE}",
        "v" = "Trust: OS-sandboxed for untrusted code"
      ))
      invisible(self)
    }
  )
)

mcp_repl_input <- function(code, context) {
  if (length(context) == 0L) {
    return(paste0(code, "\n"))
  }
  context_json <- jsonlite::toJSON(
    context,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA
  )
  context_literal <- encodeString(as.character(context_json), quote = "\"")
  paste0(
    ".context <- jsonlite::fromJSON(",
    context_literal,
    ", simplifyVector = FALSE)\n",
    code,
    "\n"
  )
}

mcp_repl_normalize_response <- function(response) {
  if (is.character(response)) {
    return(list(text = paste(response, collapse = "\n"), error = NULL))
  }
  if (!is.list(response)) {
    return(list(
      text = "",
      error = paste0(
        "mcp-repl returned unsupported response type: ",
        paste(class(response), collapse = "/")
      )
    ))
  }

  error <- response$error %||% NULL
  result <- response$result %||% response
  if (is.list(result) && isTRUE(result$isError %||% FALSE)) {
    error <- "mcp-repl reported an execution error"
  }

  content <- if (is.list(result)) result$content %||% NULL else NULL
  text <- if (is.character(result)) {
    paste(result, collapse = "\n")
  } else if (is.character(response$text %||% NULL)) {
    paste(response$text, collapse = "\n")
  } else if (is.list(content)) {
    pieces <- vapply(
      content,
      function(item) {
        if (is.character(item)) {
          return(paste(item, collapse = "\n"))
        }
        if (is.list(item) && is.character(item$text %||% NULL)) {
          return(paste(item$text, collapse = "\n"))
        }
        ""
      },
      character(1)
    )
    paste(pieces[nzchar(pieces)], collapse = "\n")
  } else {
    ""
  }

  if (is.list(error)) {
    error <- error$message %||% jsonlite::toJSON(error, auto_unbox = TRUE)
  }
  if (!is.null(error)) {
    error <- as.character(error)[[1L]]
  }
  list(text = text, error = error)
}

mcp_repl_truncate <- function(text, max_chars) {
  text <- paste(text %||% "", collapse = "\n")
  if (nchar(text, type = "chars") <= max_chars) {
    return(text)
  }
  paste0(
    substr(text, 1L, max_chars),
    "\n... [mcp-repl output truncated by dsprrr]"
  )
}

mcp_repl_error_result <- function(
  error,
  stdout = "",
  duration_ms = 0
) {
  list(
    success = FALSE,
    result = NULL,
    stdout = stdout,
    stderr = "",
    messages = "",
    warnings = "",
    error = as.character(error)[[1L]],
    duration_ms = round(duration_ms, 2)
  )
}
