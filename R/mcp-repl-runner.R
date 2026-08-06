#' Posit mcp-repl Code Runner
#'
#' @description
#' Creates a dsprrr code runner backed by Posit's
#' [`mcp-repl`](https://github.com/posit-dev/mcp-repl) MCP server.
#' `mcp-repl` keeps a long-lived R session and enforces its sandbox with
#' operating-system primitives. This makes it suitable for code proposed by an
#' optimizer or language model.
#'
#' For authenticated RLM submit/query traffic, dsprrr caps each encoded control
#' frame at 3,000 bytes so it stays below mcp-repl's inline-output threshold.
#' If mcp-repl nevertheless returns a file-preview or active-pager marker (for
#' example because user code printed a large value first), the runner fails the
#' iteration instead of accepting an unverifiable partial control frame. These
#' markers are plain text in the upstream protocol, so detection is necessarily
#' conservative and can only fail closed; dsprrr never follows a disclosed
#' sandbox file path from the host process.
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
#' Supplying `repl` is useful for an externally managed MCP connection and for
#' deterministic tests. It must be a function with the mcp-repl tool contract:
#' `repl(input, timeout_ms)`. Because dsprrr did not launch that function's
#' server, its runner policy is deliberately marked unverified and it is
#' rejected by optimizers that require an OS sandbox.
#'
#' @param repl Optional function implementing the mcp-repl `repl` tool.
#' @param command Path or command name for the `mcp-repl` executable.
#' @param interpreter Interpreter passed to mcp-repl. Currently only `"r"` is
#'   supported by this runner.
#' @param sandbox mcp-repl sandbox policy. Defaults to `"workspace-write"`.
#'   `"inherit-codex"` is rejected because [mcptools::mcp_tools()] does not
#'   currently propagate the required Codex sandbox metadata.
#' @param timeout Maximum execution time in seconds.
#' @param max_output_chars Maximum number of output characters returned to the
#'   optimizer.
#' @param oversized_output mcp-repl oversized-output mode. During authenticated
#'   RLM traffic, a detected oversized-output preview fails the iteration;
#'   dsprrr attempts to reset active pager state before returning the failure.
#' @param extra_args Reserved for future vetted mcp-repl options. It must be
#'   empty because arbitrary server flags can weaken the managed sandbox policy.
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
  if (
    !is.character(sandbox) ||
      length(sandbox) != 1L ||
      is.na(sandbox) ||
      !sandbox %in% c("workspace-write", "inherit-codex")
  ) {
    cli::cli_abort(
      c(
        "{.arg sandbox} must be {.val workspace-write} or {.val inherit-codex}",
        "i" = "Use {.fn r_code_runner} explicitly for trusted, unsandboxed code."
      ),
      class = "dsprrr_mcp_sandbox_error"
    )
  }
  if (identical(sandbox, "inherit-codex")) {
    cli::cli_abort(
      c(
        "{.val inherit-codex} cannot be verified by this MCP client",
        "i" = "Use {.code sandbox = \"workspace-write\"}."
      ),
      class = "dsprrr_mcp_sandbox_unverified"
    )
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
      !is.finite(max_output_chars) ||
      max_output_chars < 1 ||
      max_output_chars != floor(max_output_chars) ||
      max_output_chars > .Machine$integer.max
  ) {
    cli::cli_abort("{.arg max_output_chars} must be a positive integer")
  }
  if (!is.character(extra_args) || anyNA(extra_args)) {
    cli::cli_abort("{.arg extra_args} must be a character vector")
  }
  validate_mcp_repl_extra_args(extra_args)
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

  managed_connection <- is.null(repl)
  if (managed_connection) {
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
    sandbox = sandbox,
    sandbox_verified = managed_connection,
    oversized_output = oversized_output
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

  args <- mcp_repl_server_args(
    extra_args = extra_args,
    sandbox = sandbox,
    oversized_output = oversized_output,
    interpreter = interpreter
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
  tool_names <- vapply(
    tools,
    function(tool) {
      tryCatch(as.character(tool@name)[[1L]], error = function(e) "")
    },
    character(1)
  )
  repl_index <- which(tool_names == "repl")
  if (length(repl_index) != 1L) {
    cli::cli_abort(c(
      "The configured mcp-repl server did not expose exactly one {.code repl} tool",
      "i" = "Exposed tools: {.val {tool_names[nzchar(tool_names)]}}"
    ))
  }
  tools[[repl_index]]
}

validate_mcp_repl_extra_args <- function(extra_args) {
  if (length(extra_args) > 0L) {
    cli::cli_abort(
      c(
        "{.arg extra_args} cannot be used with the managed mcp-repl runner",
        "x" = "Arbitrary server flags can weaken filesystem or network isolation.",
        "i" = "Configure the vetted {.arg sandbox}, {.arg oversized_output}, and {.arg interpreter} arguments directly."
      ),
      class = "dsprrr_mcp_extra_args_error"
    )
  }
  invisible(extra_args)
}

mcp_repl_server_args <- function(
  extra_args,
  sandbox,
  oversized_output,
  interpreter
) {
  validate_mcp_repl_extra_args(extra_args)
  # Keep dsprrr-owned policy flags authoritative. Arbitrary extra flags are
  # rejected above because mcp-repl also supports configuration, domain, and
  # writable-root options that could invalidate the advertised policy.
  c(
    extra_args,
    "--sandbox",
    sandbox,
    "--oversized-output",
    oversized_output,
    "--interpreter",
    interpreter
  )
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
    sandbox_verified = NULL,
    oversized_output = NULL,
    control_frame_limit = 3000L,

    initialize = function(
      repl,
      timeout,
      max_output_chars,
      sandbox,
      sandbox_verified,
      oversized_output
    ) {
      self$repl <- repl
      self$timeout <- timeout
      self$max_output_chars <- max_output_chars
      self$sandbox <- sandbox
      self$sandbox_verified <- isTRUE(sandbox_verified)
      self$oversized_output <- oversized_output
    },

    execute = function(code, context = list(), .control_nonce = NULL) {
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
          timeout_ms = mcp_repl_timeout_ms(self$timeout)
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
      raw_text <- normalized$text
      text <- mcp_repl_truncate(raw_text, self$max_output_chars)
      if (!is.null(normalized$error)) {
        return(mcp_repl_error_result(
          normalized$error,
          stdout = text,
          duration_ms = duration_ms
        ))
      }

      transport_issue <- if (!is.null(.control_nonce)) {
        mcp_repl_rlm_transport_issue(raw_text, self$oversized_output)
      } else {
        NULL
      }
      if (!is.null(transport_issue)) {
        if (identical(transport_issue, "pager")) {
          # A pager is modal. Best-effort reset prevents the next invocation
          # from being interpreted as pager input rather than R code.
          reset_ok <- tryCatch(
            {
              self$reset()
              TRUE
            },
            error = function(e) FALSE
          )
          if (!reset_ok) {
            transport_issue <- "pager-reset-failed"
          }
        }
        return(mcp_repl_transport_error_result(
          transport_issue,
          stdout = text,
          duration_ms = duration_ms
        ))
      }

      control_value <- decode_rlm_control(raw_text, .control_nonce)
      list(
        success = TRUE,
        # Authenticated RLM frames are decoded before ordinary truncation. A
        # caller that does not supply the internal nonce receives normal text.
        result = control_value %||% text,
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
          timeout_ms = mcp_repl_timeout_ms(self$timeout)
        ),
        error = function(e) e
      )
      if (inherits(response, "error")) {
        cli::cli_abort(
          c(
            "Could not reset the mcp-repl session",
            "x" = conditionMessage(response)
          ),
          class = "dsprrr_mcp_repl_reset_error"
        )
      }
      normalized <- mcp_repl_normalize_response(response)
      if (!is.null(normalized$error)) {
        cli::cli_abort(
          c(
            "Could not reset the mcp-repl session",
            "x" = normalized$error
          ),
          class = "dsprrr_mcp_repl_reset_error"
        )
      }
      invisible(self)
    },

    policy = function() {
      if (!self$sandbox_verified) {
        return(list(
          backend = "external-mcp-repl",
          trust = "caller-managed",
          sandboxed = FALSE,
          process_isolation = NA,
          persistent = TRUE,
          filesystem_access = "unknown",
          network_access = "unknown",
          sandbox_enforcement = "unverified",
          oversized_output = self$oversized_output,
          rlm_control_frame_limit = self$control_frame_limit
        ))
      }
      list(
        backend = "posit-mcp-repl",
        trust = "untrusted-code",
        sandboxed = TRUE,
        process_isolation = TRUE,
        persistent = TRUE,
        filesystem_access = self$sandbox,
        network_access = "disabled",
        sandbox_enforcement = "operating-system",
        oversized_output = self$oversized_output,
        rlm_control_frame_limit = self$control_frame_limit
      )
    },

    print = function() {
      cli::cli_h3("Posit mcp-repl Runner")
      trust_text <- if (self$sandbox_verified) {
        "OS-sandboxed for untrusted code"
      } else {
        "Externally managed; sandbox unverified"
      }
      trust_bullet <- stats::setNames(
        paste0("Trust: ", trust_text),
        if (self$sandbox_verified) "v" else "!"
      )
      cli::cli_bullets(c(
        "*" = "Sandbox: {.val {self$sandbox}}",
        "*" = "Timeout: {.val {self$timeout}} seconds",
        "*" = "Persistent R session: {.val TRUE}",
        trust_bullet
      ))
      invisible(self)
    }
  )
)

mcp_repl_timeout_ms <- function(timeout) {
  as.integer(min(
    .Machine$integer.max,
    max(1, ceiling(timeout * 1000))
  ))
}

mcp_repl_input <- function(code, context) {
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

mcp_repl_rlm_transport_issue <- function(text, oversized_output) {
  text <- paste(text %||% "", collapse = "\n")
  if (!nzchar(text)) {
    return(NULL)
  }

  if (
    grepl(
      "Authenticated RLM control frame exceeds the runner transport limit",
      text,
      fixed = TRUE
    )
  ) {
    return("control-frame-limit")
  }

  if (identical(oversized_output, "files")) {
    # Upstream uses this same outer marker for text previews, mixed ordered
    # output bundles, image bundles, and bundle-write failures.
    preview_pattern <- paste0(
      "\\.\\.\\.\\[middle truncated;",
      "[^\\r\\n\\]]*\\]\\.\\.\\."
    )
    short_pattern <- "\\.\\.\\.\\[full output: [^\\r\\n\\]]+\\]\\.\\.\\."
    if (
      grepl(preview_pattern, text, perl = TRUE) ||
        grepl(short_pattern, text, perl = TRUE)
    ) {
      return("files")
    }
  }

  if (identical(oversized_output, "pager")) {
    pager_pattern <- paste0(
      "(?m)^--More-- \\([0-9]+p",
      "(?:, [0-9]+\\.[0-9]+%, @[0-9]+(?:\\.\\.[0-9]+)?/[0-9]+)?",
      "\\)\\r?$"
    )
    if (grepl(pager_pattern, text, perl = TRUE)) {
      return("pager")
    }
  }

  NULL
}

mcp_repl_transport_error_result <- function(
  issue,
  stdout = "",
  duration_ms = 0
) {
  detail <- switch(
    issue,
    files = paste(
      "mcp-repl compacted authenticated RLM output into a file preview;",
      "the control frame could not be verified"
    ),
    pager = paste(
      "mcp-repl entered its pager while returning authenticated RLM output;",
      "the session was reset because the control frame could not be verified"
    ),
    `pager-reset-failed` = paste(
      "mcp-repl entered its pager while returning authenticated RLM output;",
      "the control frame could not be verified and the session could not be reset"
    ),
    `control-frame-limit` = paste(
      "the authenticated RLM control frame exceeded mcp-repl's safe inline",
      "transport limit"
    ),
    "authenticated RLM output could not be verified"
  )
  result <- mcp_repl_error_result(
    detail,
    stdout = stdout,
    duration_ms = duration_ms
  )
  class(result) <- c("dsprrr_mcp_repl_transport_error", class(result))
  result
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
