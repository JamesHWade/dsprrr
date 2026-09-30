#' Run R code in an operating-system sandbox with mcp-repl
#'
#' @description
#' `mcp_repl_runner()` creates a code runner backed by Posit's
#' [`mcp-repl`](https://github.com/posit-dev/mcp-repl) MCP server. `mcp-repl`
#' keeps a long-lived R session and enforces a sandbox with operating-system
#' primitives, so it is suitable for code written by a model or an optimizer.
#' Use it with [rlm_module()], [code_act()], [program_of_thought()] and the
#' agentic optimizers [AutoResearch()] and [MetaHarness()].
#'
#' It needs the suggested mcptools package (1.0.1 or later) and the external
#' `mcp-repl` executable.
#'
#' @details
#' ## Sandbox
#'
#' By default, the runner starts `mcp-repl` through `mcptools::mcp_tools()`
#' with the R interpreter and the `workspace-write` sandbox: network access is
#' disabled, writes are allowed inside the workspace, and oversized output is
#' written to files the sandbox can see. `sandbox = "off"` is not accepted,
#' because this runner promises an enforced sandbox; use [r_code_runner()] for
#' trusted input without a sandbox.
#'
#' ## Your own connection
#'
#' `repl` accepts a function with the mcp-repl tool contract,
#' `repl(input, timeout_ms)`, for an MCP connection you manage or for tests.
#' Because dsprrr did not start that server, it cannot vouch for the sandbox:
#' the runner is marked unverified and optimizers that require a sandbox
#' reject it. `$shutdown()` then ends the runner but leaves your connection
#' open. A runner that dsprrr starts shuts down only the transport it started.
#'
#' ## Long-running code
#'
#' mcptools waits only about 4 seconds for each reply from mcp-repl, so dsprrr
#' asks mcp-repl to wait at most 3 seconds per request. Code that runs longer
#' keeps running: mcp-repl reports it as busy, and dsprrr collects the rest of
#' its output with further requests until it finishes, then returns all of it
#' together, without mcp-repl's busy status lines. Code still running after
#' `timeout` seconds is interrupted, as with Ctrl-C, and `$execute()` returns
#' a timeout error; the session and its variables are kept. If the interrupt
#' does not stop the code, or mcp-repl does not reply in time, the runner
#' cannot be used again, so that a late reply can never be read as the answer
#' to a later request.
#'
#' ## Output limits
#'
#' RLM exchanges control messages with the guest session, capped at 3,000
#' bytes each so they stay below mcp-repl's inline-output limit. If mcp-repl
#' still replies with a file preview or a pager (for example because the code
#' printed a large value first), the iteration fails rather than accepting a
#' partial message; dsprrr never follows a file path reported by the sandbox.
#' Executable Flex programs read one bounded message per step and can recover
#' it from a plain file preview; anything ambiguous fails. Host requests that
#' are too large are compressed and, if still too large, rejected before
#' sending.
#'
#' @param repl Optional function implementing the mcp-repl `repl` tool; see
#'   Details.
#' @param command Path or name of the `mcp-repl` executable.
#' @param interpreter Interpreter passed to mcp-repl. Only `"r"` is
#'   supported.
#' @param sandbox Sandbox policy, `"workspace-write"` (the default).
#'   `"inherit-codex"` is rejected because `mcptools::mcp_tools()` does not
#'   pass on the Codex sandbox metadata it needs.
#' @param timeout Maximum time for one `$execute()` or `$reset()` call, in
#'   seconds (default 30). Code still running after it is interrupted and
#'   reported as a timeout error; see Details.
#' @param max_output_chars Maximum number of output characters returned per
#'   call (default `100000L`).
#' @param oversized_output mcp-repl's mode for oversized output (default
#'   `"files"`). RLM rejects file previews; executable Flex accepts one bounded
#'   message in a plain file preview. dsprrr tries to reset an active pager
#'   before reporting a failure.
#' @param extra_args Reserved for future vetted mcp-repl options and must be
#'   empty, because arbitrary server flags could weaken the sandbox.
#'
#' @return An `McpReplRunner` object implementing the runner interface
#'   described in [r_code_runner()]: `$execute()`, `$policy()`, `$reset()`
#'   and `$shutdown()`.
#'
#' @export
#' @family code execution
#' @examples
#' \dontrun{
#' runner <- mcp_repl_runner()
#' runner$policy()$sandboxed
#' runner$execute("mean(1:10)")$result
#' runner$reset()
#' runner$shutdown()
#'
#' # Give each RLM call a fresh sandboxed session
#' analyst <- rlm_module(
#'   "document, question -> answer",
#'   interpreter_factory = function() mcp_repl_runner(timeout = 30)
#' )
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
  close_connection <- NULL
  if (managed_connection) {
    managed <- mcp_repl_tool(
      command = command,
      interpreter = interpreter,
      sandbox = sandbox,
      oversized_output = oversized_output,
      extra_args = extra_args
    )
    repl <- managed$repl
    close_connection <- managed$close
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
    oversized_output = oversized_output,
    close_connection = close_connection,
    connection_owned = managed_connection
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
  process_key <- paste(c(unname(command_path), args), collapse = " ")
  process_snapshot <- tryCatch(
    mcp_repl_registry_processes(mcp_repl_registry()),
    error = function(condition) {
      cli::cli_abort(
        c(
          "Could not capture the managed mcp-repl lifecycle before startup",
          "i" = "The installed {.pkg mcptools} version does not expose the process state dsprrr needs for deterministic teardown."
        ),
        class = "dsprrr_mcp_repl_lifecycle_error",
        parent = condition
      )
    }
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

  close <- NULL
  handed_off <- FALSE
  cleanup_done <- FALSE
  cleanup_setup <- function() {
    if (cleanup_done) {
      return(NULL)
    }
    cleanup_done <<- TRUE
    if (is.function(close)) {
      return(tryCatch(
        {
          close()
          NULL
        },
        interrupt = function(condition) condition,
        error = function(condition) condition
      ))
    }
    mcp_repl_best_effort_close(
      server_name,
      process_snapshot = process_snapshot,
      process_key = process_key
    )
  }
  # Register cleanup before asking mcptools to start the configured server:
  # startup can fail after partially populating its internal registry.
  on.exit(
    if (!handed_off) {
      try(cleanup_setup(), silent = TRUE)
    },
    add = TRUE
  )

  tools <- mcptools::mcp_tools(config = config_path)
  close <- tryCatch(
    mcp_repl_managed_closer(server_name),
    error = function(condition) {
      cleanup_error <- cleanup_setup()
      if (inherits(cleanup_error, "condition")) {
        attr(condition, "dsprrr_interpreter_close_error") <- cleanup_error
      }
      stop(condition)
    }
  )
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
  bundle <- list(
    repl = tools[[repl_index]],
    close = close
  )
  handed_off <- TRUE
  bundle
}

# Some supported mcptools versions do not expose a public connection-close API.
# Keep access to the internal registry behind guarded compatibility shims.
mcp_repl_registry <- function() {
  namespace <- asNamespace("mcptools")
  registry <- get("the", namespace)
  if (!is.environment(registry)) {
    stop("mcptools registry is unavailable")
  }
  registry
}

mcp_repl_registry_processes <- function(registry) {
  processes <- registry$server_processes %||% list()
  if (!is.list(processes)) {
    stop("mcptools process registry is unavailable")
  }
  processes
}

mcp_repl_process_is_in <- function(process, processes) {
  length(processes) > 0L &&
    any(vapply(processes, identical, logical(1), y = process))
}

mcp_repl_stop_process <- function(process) {
  if (is.null(process)) {
    return(FALSE)
  }
  alive <- tryCatch(
    isTRUE(process$is_alive()),
    interrupt = function(condition) NA,
    error = function(condition) NA
  )
  if (identical(alive, FALSE)) {
    return(TRUE)
  }
  tryCatch(
    {
      process$kill()
      TRUE
    },
    interrupt = function(condition) FALSE,
    error = function(condition) FALSE
  )
}

mcp_repl_prune_process <- function(registry, process) {
  if (is.null(process)) {
    return(invisible(NULL))
  }
  processes <- mcp_repl_registry_processes(registry)
  keep <- !vapply(processes, identical, logical(1), y = process)
  registry$server_processes <- processes[keep]
  invisible(NULL)
}

mcp_repl_close_captured <- function(
  registry,
  server_name,
  transport,
  process,
  close_transport
) {
  transport_error <- NULL
  transport_closed <- FALSE
  if (!is.null(transport) && is.function(close_transport)) {
    transport_closed <- tryCatch(
      {
        close_transport(transport)
        TRUE
      },
      interrupt = function(condition) {
        transport_error <<- condition
        FALSE
      },
      error = function(condition) {
        transport_error <<- condition
        FALSE
      }
    )
  }

  process_closed <- if (is.null(process)) {
    FALSE
  } else {
    mcp_repl_stop_process(process)
  }
  closed <- if (is.null(process)) transport_closed else process_closed
  if (!closed) {
    cli::cli_abort(
      "Managed mcp-repl transport could not be closed",
      class = "dsprrr_mcp_repl_lifecycle_error",
      parent = transport_error
    )
  }

  current <- registry$mcp_servers[[server_name]]
  if (
    !is.null(current) &&
      (identical(current$transport, transport) ||
        (!is.null(process) && identical(current$process, process)))
  ) {
    registry$mcp_servers[[server_name]] <- NULL
  }
  mcp_repl_prune_process(registry, process)
  invisible(NULL)
}

mcp_repl_best_effort_close <- function(
  server_name,
  process_snapshot = list(),
  process_key = NULL
) {
  tryCatch(
    {
      namespace <- asNamespace("mcptools")
      registry <- mcp_repl_registry()
      server <- registry$mcp_servers[[server_name]]
      close_transport <- tryCatch(
        get("mcp_transport_close", namespace),
        error = function(condition) NULL
      )

      if (!is.null(server)) {
        transport <- server$transport
        process <- transport$process %||% server$process
        mcp_repl_close_captured(
          registry,
          server_name,
          transport,
          process,
          close_transport
        )
        return(NULL)
      }

      processes <- mcp_repl_registry_processes(registry)
      is_new <- !vapply(
        processes,
        mcp_repl_process_is_in,
        logical(1),
        processes = process_snapshot
      )
      if (!is.null(process_key) && !is.null(names(processes))) {
        is_new <- is_new & names(processes) == process_key
      }
      candidates <- processes[is_new]
      if (length(candidates) == 0L) {
        return(NULL)
      }
      if (length(candidates) != 1L) {
        stop("managed mcp-repl startup left ambiguous process state")
      }
      process <- candidates[[1L]]
      mcp_repl_close_captured(
        registry,
        server_name,
        transport = NULL,
        process = process,
        close_transport = NULL
      )
      NULL
    },
    interrupt = function(condition) condition,
    error = function(condition) condition
  )
}

mcp_repl_managed_closer <- function(server_name) {
  namespace <- asNamespace("mcptools")
  registry <- tryCatch(mcp_repl_registry(), error = function(e) NULL)
  close_transport <- tryCatch(
    get("mcp_transport_close", namespace),
    error = function(e) NULL
  )
  server <- if (is.environment(registry)) {
    registry$mcp_servers[[server_name]]
  } else {
    NULL
  }
  if (
    is.null(server) ||
      is.null(server$transport) ||
      !is.function(close_transport)
  ) {
    cli::cli_abort(
      c(
        "Could not capture the managed mcp-repl lifecycle",
        "i" = "The installed {.pkg mcptools} version does not expose the transport state dsprrr needs for deterministic teardown."
      ),
      class = "dsprrr_mcp_repl_lifecycle_error"
    )
  }

  transport <- server$transport
  process <- transport$process %||% server$process
  closed <- FALSE
  function() {
    if (closed) {
      return(invisible(NULL))
    }
    mcp_repl_close_captured(
      registry,
      server_name,
      transport,
      process,
      close_transport
    )
    closed <<- TRUE
    invisible(NULL)
  }
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
    close_connection = NULL,
    connection_owned = FALSE,
    closed = FALSE,
    started = FALSE,
    terminal = FALSE,
    terminal_reason = NULL,

    initialize = function(
      repl,
      timeout,
      max_output_chars,
      sandbox,
      sandbox_verified,
      oversized_output,
      close_connection = NULL,
      connection_owned = FALSE
    ) {
      self$repl <- repl
      self$timeout <- timeout
      self$max_output_chars <- max_output_chars
      self$sandbox <- sandbox
      self$sandbox_verified <- isTRUE(sandbox_verified)
      self$oversized_output <- oversized_output
      self$close_connection <- close_connection
      self$connection_owned <- isTRUE(connection_owned)
    },

    execute = function(
      code,
      context = list(),
      .control_nonce = NULL,
      .control_protocol = NULL,
      .control_max_bytes = NULL
    ) {
      private$assert_usable()
      if (!self$started) {
        self$start()
      }
      if (!is.character(code) || length(code) != 1L || is.na(code)) {
        cli::cli_abort("{.arg code} must be a single non-missing string")
      }
      if (!is.list(context)) {
        cli::cli_abort("{.arg context} must be a list")
      }
      control_protocol <- mcp_repl_control_protocol(
        .control_protocol,
        .control_nonce
      )
      control_max_bytes <- mcp_repl_control_max_bytes(
        .control_max_bytes,
        self$control_frame_limit
      )

      input <- tryCatch(
        mcp_repl_input(code, context),
        dsprrr_mcp_repl_input_size_error = identity
      )
      if (inherits(input, "dsprrr_mcp_repl_input_size_error")) {
        result <- mcp_repl_error_result(
          conditionMessage(input),
          error_type = "execution"
        )
        result$input_bytes <- input$input_bytes
        result$request_bytes <- input$request_bytes
        result$request_limit <- input$request_limit
        class(result) <- c(
          "dsprrr_mcp_repl_input_size_error",
          class(result)
        )
        return(result)
      }
      started_at <- Sys.time()
      reply <- private$exchange(input, interrupt = TRUE)
      duration_ms <- as.numeric(
        difftime(Sys.time(), started_at, units = "secs")
      ) *
        1000

      raw_text <- reply$text
      text <- mcp_repl_truncate(raw_text, self$max_output_chars)
      if (!is.null(reply$failure)) {
        # exchange() has already made the session terminal.
        return(mcp_repl_error_result(
          reply$failure,
          stdout = text,
          duration_ms = duration_ms,
          error_type = "interpreter"
        ))
      }
      if (reply$timed_out) {
        return(mcp_repl_error_result(
          paste0(
            "Execution timed out after ",
            self$timeout,
            " seconds and was interrupted"
          ),
          stdout = text,
          duration_ms = duration_ms,
          error_type = "execution"
        ))
      }
      if (reply$execution_error) {
        return(mcp_repl_error_result(
          mcp_repl_execution_error(raw_text),
          stdout = text,
          duration_ms = duration_ms,
          error_type = "execution"
        ))
      }

      transport_issue <- if (!is.null(control_protocol)) {
        mcp_repl_rlm_transport_issue(raw_text, self$oversized_output)
      } else {
        NULL
      }
      flex_control <- NULL
      if (identical(control_protocol, "flex")) {
        flex_control <- if (identical(transport_issue, "files")) {
          mcp_repl_inline_flex_preview(
            raw_text,
            nonce = .control_nonce,
            max_bytes = control_max_bytes
          )
        } else if (is.null(transport_issue)) {
          mcp_repl_inline_flex_control(
            raw_text,
            nonce = .control_nonce,
            max_bytes = control_max_bytes
          )
        } else {
          NULL
        }
        if (
          !is.null(flex_control) &&
            identical(transport_issue, "files")
        ) {
          transport_issue <- NULL
        }
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
        if (identical(transport_issue, "pager-reset-failed")) {
          private$mark_terminal(
            "mcp-repl pager state could not be reset"
          )
        }
        return(mcp_repl_transport_error_result(
          transport_issue,
          stdout = text,
          duration_ms = duration_ms
        ))
      }

      control_value <- if (identical(control_protocol, "flex")) {
        flex_control
      } else if (identical(control_protocol, "rlm")) {
        # The module decodes this value again after the runner boundary. Mark
        # it with a process-local identity so arbitrary classed R
        # objects returned by generated code cannot impersonate control frames.
        decode_rlm_control(raw_text, .control_nonce, .attest = TRUE)
      } else {
        NULL
      }
      if (identical(control_protocol, "flex") && is.null(control_value)) {
        return(mcp_repl_transport_error_result(
          "flex-control",
          stdout = text,
          duration_ms = duration_ms
        ))
      }
      list(
        success = TRUE,
        # Control frames are decoded from raw output before display truncation.
        # A caller without an internal control protocol receives normal text.
        result = control_value %||% text,
        stdout = text,
        stderr = "",
        messages = "",
        warnings = "",
        error = NULL,
        error_type = NULL,
        retryable = FALSE,
        duration_ms = round(duration_ms, 2)
      )
    },

    start = function() {
      private$assert_usable()
      self$started <- TRUE
      invisible(self)
    },

    reset = function() {
      private$assert_usable()
      # A session that is still busy after `timeout` is not known to be fresh,
      # so the reset fails and the runner becomes terminal.
      reply <- private$exchange("\u0004", interrupt = FALSE)
      reason <- reply$failure
      if (is.null(reason) && reply$execution_error) {
        reason <- mcp_repl_execution_error(reply$text)
      }
      if (!is.null(reason)) {
        private$mark_terminal(reason)
        cli::cli_abort(
          c(
            "Could not reset the mcp-repl session",
            "x" = reason
          ),
          class = "dsprrr_mcp_repl_reset_error"
        )
      }
      invisible(self)
    },

    shutdown = function() {
      if (self$closed) {
        return(invisible(self))
      }
      # Terminal state is committed before teardown so a failing shutdown
      # cannot make later calls reuse a partially closed interpreter or trigger
      # an implicit lifecycle retry from the finalizer.
      self$closed <- TRUE
      if (is.function(self$close_connection)) {
        self$close_connection()
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
          connection_owned = FALSE,
          filesystem_access = "unknown",
          network_access = "unknown",
          sandbox_enforcement = "unverified",
          oversized_output = self$oversized_output,
          rlm_control_frame_limit = self$control_frame_limit,
          flex_control_frame_limit = self$control_frame_limit,
          host_tools = "unsupported",
          lifecycle = "start-execute-shutdown"
        ))
      }
      list(
        backend = "posit-mcp-repl",
        trust = "untrusted-code",
        sandboxed = TRUE,
        process_isolation = TRUE,
        persistent = TRUE,
        connection_owned = self$connection_owned,
        filesystem_access = self$sandbox,
        network_access = "disabled",
        sandbox_enforcement = "operating-system",
        oversized_output = self$oversized_output,
        rlm_control_frame_limit = self$control_frame_limit,
        flex_control_frame_limit = self$control_frame_limit,
        host_tools = "unsupported",
        lifecycle = "start-execute-shutdown"
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
  ),

  private = list(
    assert_usable = function() {
      if (self$closed) {
        cli::cli_abort(
          "The mcp-repl runner is closed",
          class = c(
            "dsprrr_mcp_repl_closed_error",
            "dsprrr_interpreter_closed_error"
          )
        )
      }
      if (self$terminal) {
        cli::cli_abort(
          c(
            "The mcp-repl interpreter session is terminal",
            "x" = self$terminal_reason %||%
              "A process or protocol failure occurred."
          ),
          class = c(
            "dsprrr_mcp_repl_terminal_error",
            "dsprrr_interpreter_terminal_error"
          )
        )
      }
      invisible(NULL)
    },

    mark_terminal = function(reason) {
      self$terminal <- TRUE
      self$terminal_reason <- as.character(reason)[[1L]]
      invisible(NULL)
    },

    call_repl = function(input, timeout_ms) {
      tryCatch(
        # mcptools-generated wrappers reconstruct their call from
        # match.call(). Pass realized values so expressions that mention the
        # R6 `self` binding are not re-evaluated outside this method frame.
        do.call(
          self$repl,
          list(input = input, timeout_ms = timeout_ms)
        ),
        interrupt = function(condition) {
          # The abandoned reply could still arrive and answer a later request.
          private$mark_terminal("An mcp-repl request was interrupted")
          stop(condition)
        },
        error = function(e) e
      )
    },

    # Sends `input`, then polls with empty input while mcp-repl reports the
    # cell busy, until it is idle or `self$timeout` has elapsed. No request
    # waits longer than `.mcp_repl_call_timeout_ms`. Returns the output of all
    # replies without busy status lines. A cell still busy at the deadline is
    # interrupted when `interrupt` is TRUE. Any outcome that could leave a
    # reply or a running cell behind marks the session terminal and is
    # returned as `failure`, so it can never answer a later request.
    exchange = function(input, interrupt = TRUE) {
      deadline <- mcp_repl_clock() + self$timeout
      timeout_ms <- mcp_repl_call_timeout_ms(self$timeout)
      pieces <- character()
      execution_error <- FALSE
      outcome <- function(failure = NULL, timed_out = FALSE) {
        if (!is.null(failure)) {
          private$mark_terminal(failure)
        }
        list(
          text = mcp_repl_join_output(pieces),
          failure = failure,
          timed_out = timed_out,
          execution_error = execution_error
        )
      }

      repeat {
        response <- private$call_repl(input, timeout_ms)
        if (inherits(response, "error")) {
          return(outcome(failure = conditionMessage(response)))
        }
        normalized <- mcp_repl_normalize_response(response)
        status <- mcp_repl_busy_status(normalized$text)
        pieces <- c(pieces, status$output)
        if (identical(normalized$error_type, "interpreter")) {
          return(outcome(failure = normalized$error))
        }
        # An error reported by any reply is an error of the whole cell, as it
        # would be in a single reply.
        execution_error <- execution_error ||
          identical(normalized$error_type, "execution")
        if (!status$busy) {
          return(outcome())
        }
        remaining <- deadline - mcp_repl_clock()
        if (remaining <= 0) {
          break
        }
        input <- ""
        timeout_ms <- mcp_repl_call_timeout_ms(remaining)
      }

      if (!interrupt) {
        return(outcome(
          failure = paste0(
            "mcp-repl was still busy after ",
            self$timeout,
            " seconds"
          )
        ))
      }
      not_stopped <- paste0(
        "Execution exceeded the ",
        self$timeout,
        "-second timeout and could not be interrupted: "
      )
      response <- private$call_repl("\u0003", .mcp_repl_call_timeout_ms)
      if (inherits(response, "error")) {
        return(outcome(
          failure = paste0(not_stopped, conditionMessage(response))
        ))
      }
      normalized <- mcp_repl_normalize_response(response)
      status <- mcp_repl_busy_status(normalized$text)
      pieces <- c(pieces, status$output)
      if (identical(normalized$error_type, "interpreter")) {
        return(outcome(failure = paste0(not_stopped, normalized$error)))
      }
      if (status$busy) {
        return(outcome(
          failure = paste0(not_stopped, "mcp-repl still reports it busy")
        ))
      }
      # mcp-repl reports the interpreter idle again; an interrupted cell may
      # report that as an error, which the timeout result already covers.
      outcome(timed_out = TRUE)
    },

    finalize = function() {
      try(self$shutdown(), silent = TRUE)
    }
  )
)

# mcptools 1.0.2 and 1.0.3 read a stdio reply for about 4 seconds (20 polls of
# 0.2 s), then give up and return NULL; they do not check the JSON-RPC id, so a
# reply that arrives later is read as the answer to the next request. mcp-repl
# answers shortly before `timeout_ms` with a busy status while the code keeps
# running, so capping every request at 3 seconds keeps each reply inside that
# window with room for transport and scheduling delays.
.mcp_repl_call_timeout_ms <- 3000L

# mcp-repl's reply carries a status line with this text while the code is
# still running; a later request with empty input collects the rest of the
# output. Matched as a fixed string, so the delimiters around it do not matter.
.mcp_repl_busy_marker <- "repl status: busy"

# `seconds` is the time left for the whole call; one request waits at most
# `.mcp_repl_call_timeout_ms`.
mcp_repl_call_timeout_ms <- function(seconds) {
  as.integer(min(
    .mcp_repl_call_timeout_ms,
    max(1, ceiling(seconds * 1000))
  ))
}

mcp_repl_clock <- function() {
  proc.time()[["elapsed"]]
}

# Whether a reply reports a busy cell, and its output without the status
# line(s). Where mcp-repl splits the output between replies is not specified,
# so trailing newlines before the status are dropped and replies are joined
# with one newline (see mcp_repl_join_output()).
mcp_repl_busy_status <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1L]]
  busy_lines <- grepl(.mcp_repl_busy_marker, lines, fixed = TRUE)
  if (!any(busy_lines)) {
    return(list(busy = FALSE, output = text))
  }
  list(
    busy = TRUE,
    output = sub("\n+$", "", paste(lines[!busy_lines], collapse = "\n"))
  )
}

mcp_repl_join_output <- function(pieces) {
  paste(pieces[nzchar(pieces)], collapse = "\n")
}

.mcp_repl_request_limit_bytes <- 7000L

mcp_repl_input <- function(code, context) {
  # `serializeJSON()` preserves the distinction between data frames, atomic
  # vectors, and nested replay records. `fromJSON(..., simplifyVector = TRUE)`
  # cannot preserve all three at once: it repairs data frames by also
  # simplifying bridge replay ledgers into data frames.
  context_json <- jsonlite::serializeJSON(context, digits = NA)
  context_literal <- encodeString(as.character(context_json), quote = "\"")
  input <- paste0(
    ".context <- jsonlite::unserializeJSON(",
    context_literal,
    ")\n",
    code,
    "\n"
  )
  request_bytes <- mcp_repl_request_bytes(input)
  if (request_bytes <= .mcp_repl_request_limit_bytes) {
    return(input)
  }
  input_bytes <- nchar(input, type = "bytes")

  compressed <- memCompress(charToRaw(enc2utf8(input)), type = "gzip")
  token <- gsub(
    "[[:space:]]",
    "",
    jsonlite::base64_enc(compressed)
  )
  wrapped <- paste0(
    "base::eval(base::parse(text = base::rawToChar(",
    "base::memDecompress(jsonlite::base64_dec(\"",
    token,
    "\"), type = \"gzip\"))), envir = base::globalenv())\n"
  )
  wrapped_request_bytes <- mcp_repl_request_bytes(wrapped)
  if (wrapped_request_bytes > .mcp_repl_request_limit_bytes) {
    request_limit <- .mcp_repl_request_limit_bytes
    cli::cli_abort(
      c(
        "mcp-repl input exceeds the managed transport limit",
        "x" = "The compressed request needs {wrapped_request_bytes} bytes; the limit is {request_limit} bytes.",
        "i" = "Reduce the source or serialized execution context."
      ),
      class = "dsprrr_mcp_repl_input_size_error",
      input_bytes = input_bytes,
      request_bytes = wrapped_request_bytes,
      request_limit = .mcp_repl_request_limit_bytes
    )
  }
  wrapped
}

mcp_repl_request_bytes <- function(input) {
  # Sizes the request that would carry `input`; nothing is sent. The largest
  # integers stand in for the id and `timeout_ms` so the bound holds for any
  # value of either.
  request <- list(
    jsonrpc = "2.0",
    id = .Machine$integer.max,
    method = "tools/call",
    params = list(
      name = "repl",
      arguments = list(
        input = input,
        timeout_ms = .Machine$integer.max
      )
    )
  )
  wire <- jsonlite::toJSON(request, auto_unbox = TRUE)
  nchar(as.character(wire), type = "bytes") + 1L
}

mcp_repl_control_protocol <- function(protocol, nonce) {
  if (is.null(protocol)) {
    if (is.null(nonce)) {
      return(NULL)
    }
    return("rlm")
  }
  if (
    !is.character(protocol) ||
      length(protocol) != 1L ||
      is.na(protocol) ||
      !protocol %in% c("rlm", "flex")
  ) {
    cli::cli_abort(
      "Internal control protocol must be {.val rlm}, {.val flex}, or NULL",
      class = "dsprrr_code_runner_protocol_error"
    )
  }
  if (
    !is.character(nonce) ||
      length(nonce) != 1L ||
      is.na(nonce) ||
      !nzchar(nonce)
  ) {
    cli::cli_abort(
      "Internal control nonce must be one non-empty string",
      class = "dsprrr_code_runner_protocol_error"
    )
  }
  protocol
}

mcp_repl_control_max_bytes <- function(max_bytes, runner_limit) {
  if (is.null(max_bytes)) {
    return(as.integer(runner_limit))
  }
  if (
    !is.numeric(max_bytes) ||
      length(max_bytes) != 1L ||
      is.na(max_bytes) ||
      !is.finite(max_bytes) ||
      max_bytes < 1 ||
      max_bytes != floor(max_bytes)
  ) {
    cli::cli_abort(
      "Internal control byte limit must be one positive integer",
      class = "dsprrr_code_runner_protocol_error"
    )
  }
  as.integer(min(max_bytes, runner_limit))
}

mcp_repl_inline_flex_control <- function(text, nonce, max_bytes) {
  text <- paste(text %||% "", collapse = "\n")
  if (!nzchar(text)) {
    return(NULL)
  }

  prefix_locations <- gregexpr(
    .flex_code_control_prefix,
    text,
    fixed = TRUE
  )[[1L]]
  if (
    identical(prefix_locations[[1L]], -1L) ||
      length(prefix_locations) != 1L
  ) {
    return(NULL)
  }
  frame_pattern <- paste0(
    .flex_code_control_prefix,
    "[A-Za-z0-9+/=]+"
  )
  frames <- regmatches(text, gregexpr(frame_pattern, text, perl = TRUE))[[1L]]
  if (
    length(frames) != 1L ||
      !nzchar(frames[[1L]]) ||
      nchar(frames[[1L]], type = "bytes") > max_bytes
  ) {
    return(NULL)
  }

  token <- sub(
    .flex_code_control_prefix,
    "",
    frames[[1L]],
    fixed = TRUE
  )
  envelope <- tryCatch(
    jsonlite::fromJSON(
      rawToChar(jsonlite::base64_dec(token)),
      simplifyVector = FALSE
    ),
    error = function(error) NULL
  )

  # The nonce is a current-step correlation value, not a secret credential.
  # Exact framing, schema validation, and the byte bound are all required.
  valid <- is.list(envelope) &&
    identical(envelope$.dsprrr_flex_control, TRUE) &&
    identical(envelope$version, 1L) &&
    identical(envelope$nonce, nonce) &&
    is.character(envelope$kind) &&
    length(envelope$kind) == 1L &&
    !is.na(envelope$kind) &&
    envelope$kind %in% c("request", "final", "overflow") &&
    is.list(envelope$payload)
  if (!valid) {
    return(NULL)
  }
  envelope
}

mcp_repl_inline_flex_preview <- function(text, nonce, max_bytes) {
  text <- paste(text %||% "", collapse = "\n")

  # Only mcp-repl's plain transcript-file marker is recoverable. Ordered
  # bundles, image bundles, write failures, and middle-truncated previews may
  # omit or reorder output and therefore stay fail-closed.
  if (
    grepl("...[middle truncated;", text, fixed = TRUE) ||
      grepl("bundle", tolower(text), fixed = TRUE)
  ) {
    return(NULL)
  }
  file_pattern <- "\\.\\.\\.\\[full output: [^\\r\\n\\]]+\\]\\.\\.\\."
  file_markers <- regmatches(
    text,
    gregexpr(file_pattern, text, perl = TRUE)
  )[[1L]]
  if (length(file_markers) < 1L) {
    return(NULL)
  }

  mcp_repl_inline_flex_control(text, nonce, max_bytes)
}

mcp_repl_normalize_response <- function(response) {
  if (is.null(response)) {
    # mcptools returns NULL when no reply arrived within its wait. The reply
    # may still arrive and would then answer the next request.
    return(list(
      text = "",
      error = "mcp-repl did not reply within the MCP client's wait",
      error_type = "interpreter"
    ))
  }
  if (is.character(response)) {
    return(list(
      text = paste(response, collapse = "\n"),
      error = NULL,
      error_type = NULL
    ))
  }
  if (!is.list(response)) {
    return(list(
      text = "",
      error = paste0(
        "mcp-repl returned unsupported response type: ",
        paste(class(response), collapse = "/")
      ),
      error_type = "interpreter"
    ))
  }

  protocol_error <- response$error %||% NULL
  error <- protocol_error
  error_type <- if (is.null(protocol_error)) NULL else "interpreter"
  result <- response$result %||% response
  execution_error <- is.null(protocol_error) &&
    is.list(result) &&
    isTRUE(result$isError %||% FALSE)

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

  if (execution_error) {
    error <- mcp_repl_execution_error(text)
    error_type <- "execution"
  }

  if (is.list(error)) {
    error <- error$message %||% jsonlite::toJSON(error, auto_unbox = TRUE)
  }
  if (!is.null(error)) {
    error <- as.character(error)[[1L]]
  }
  list(text = text, error = error, error_type = error_type)
}

mcp_repl_execution_error <- function(text) {
  if (nzchar(text)) {
    paste("mcp-repl reported an execution error:", text)
  } else {
    "mcp-repl reported an execution error"
  }
}

mcp_repl_truncate <- function(text, max_chars) {
  text <- paste(text %||% "", collapse = "\n")
  total <- nchar(text, type = "chars")
  if (total <= max_chars) {
    return(text)
  }
  marker <- paste0(
    "\n... [mcp-repl output truncated by dsprrr; ",
    total,
    " total characters] ...\n"
  )
  if (nchar(marker, type = "chars") + 2L > max_chars) {
    return(substr(text, 1L, max_chars))
  }
  visible <- max_chars - nchar(marker, type = "chars")
  head_chars <- ceiling(visible / 2)
  tail_chars <- floor(visible / 2)
  paste0(
    substr(text, 1L, head_chars),
    marker,
    substr(text, total - tail_chars + 1L, total)
  )
}

mcp_repl_rlm_transport_issue <- function(text, oversized_output) {
  text <- paste(text %||% "", collapse = "\n")
  if (!nzchar(text)) {
    return(NULL)
  }

  if (
    grepl(
      "RLM control frame exceeds the runner transport limit",
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
      "mcp-repl compacted nonce-bound RLM output into a file preview;",
      "the control frame could not be verified"
    ),
    pager = paste(
      "mcp-repl entered its pager while returning nonce-bound RLM output;",
      "the session was reset because the control frame could not be verified"
    ),
    `pager-reset-failed` = paste(
      "mcp-repl entered its pager while returning nonce-bound RLM output;",
      "the control frame could not be verified and the session could not be reset"
    ),
    `control-frame-limit` = paste(
      "the RLM control frame exceeded mcp-repl's safe inline",
      "transport limit"
    ),
    `flex-control` = paste(
      "mcp-repl returned executable Flex output whose control frame",
      "could not be verified"
    ),
    "nonce-bound RLM output could not be verified"
  )
  result <- mcp_repl_error_result(
    detail,
    stdout = stdout,
    duration_ms = duration_ms,
    error_type = if (identical(issue, "pager-reset-failed")) {
      "interpreter"
    } else {
      "execution"
    }
  )
  class(result) <- c("dsprrr_mcp_repl_transport_error", class(result))
  result
}

mcp_repl_error_result <- function(
  error,
  stdout = "",
  duration_ms = 0,
  error_type = "execution"
) {
  list(
    success = FALSE,
    result = NULL,
    stdout = stdout,
    stderr = "",
    messages = "",
    warnings = "",
    error = as.character(error)[[1L]],
    error_type = error_type,
    retryable = identical(error_type, "execution"),
    duration_ms = round(duration_ms, 2)
  )
}
