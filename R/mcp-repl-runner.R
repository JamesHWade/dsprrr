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
#' Executable Flex may recover a plain file preview only when its raw response
#' contains exactly one bounded control frame for the current replay step. The
#' frame remains untrusted and is still subject to Flex's host-side request,
#' budget, and output validation. Ambiguous previews, pagers, bundles, and MCP
#' errors fail closed. Large host-generated requests are compressed before
#' transport and rejected before sending if they still exceed the safe bound.
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
#' rejected by optimizers that require an OS sandbox. Calling `$close()` makes
#' the wrapper terminal but does not close that caller-managed connection.
#'
#' A managed runner captures and closes only the mcp-repl transport it starts.
#' Some supported mcptools versions do not expose public per-server teardown,
#' so dsprrr uses a guarded compatibility shim and fails setup if deterministic
#' ownership cannot be captured. This path is tested against mcptools 1.0.1.
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
#' @param oversized_output mcp-repl oversized-output mode. RLM previews fail
#'   closed. Executable Flex accepts only one bounded current-step frame in a
#'   plain file preview. dsprrr attempts to reset active pager state before
#'   returning a failure.
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
      timeout_ms <- mcp_repl_timeout_ms(self$timeout)
      started_at <- Sys.time()
      response <- tryCatch(
        # mcptools-generated wrappers reconstruct their call from
        # match.call(). Pass realized values so expressions that mention the
        # R6 `self` binding are not re-evaluated outside this method frame.
        do.call(
          self$repl,
          list(input = input, timeout_ms = timeout_ms)
        ),
        error = function(e) e
      )
      duration_ms <- as.numeric(
        difftime(Sys.time(), started_at, units = "secs")
      ) *
        1000

      if (inherits(response, "error")) {
        private$mark_terminal(conditionMessage(response))
        return(mcp_repl_error_result(
          conditionMessage(response),
          duration_ms = duration_ms,
          error_type = "interpreter"
        ))
      }

      normalized <- mcp_repl_normalize_response(response)
      raw_text <- normalized$text
      text <- mcp_repl_truncate(raw_text, self$max_output_chars)
      if (!is.null(normalized$error)) {
        if (identical(normalized$error_type, "interpreter")) {
          private$mark_terminal(normalized$error)
        }
        return(mcp_repl_error_result(
          normalized$error,
          stdout = text,
          duration_ms = duration_ms,
          error_type = normalized$error_type
        ))
      }

      transport_issue <- if (!is.null(control_protocol)) {
        mcp_repl_rlm_transport_issue(raw_text, self$oversized_output)
      } else {
        NULL
      }
      preview_control <- NULL
      if (
        identical(control_protocol, "flex") &&
          identical(transport_issue, "files")
      ) {
        preview_control <- mcp_repl_inline_flex_preview(
          raw_text,
          nonce = .control_nonce,
          max_bytes = control_max_bytes
        )
        if (!is.null(preview_control)) {
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

      control_value <- if (!is.null(preview_control)) {
        preview_control
      } else if (identical(control_protocol, "rlm")) {
        decode_rlm_control(raw_text, .control_nonce)
      } else {
        NULL
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
      timeout_ms <- mcp_repl_timeout_ms(self$timeout)
      response <- tryCatch(
        do.call(
          self$repl,
          list(input = "\u0004", timeout_ms = timeout_ms)
        ),
        error = function(e) e
      )
      if (inherits(response, "error")) {
        private$mark_terminal(conditionMessage(response))
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
        private$mark_terminal(normalized$error)
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

    close = function() {
      if (self$closed) {
        return(invisible(self))
      }
      # Terminal state is committed before teardown so a failing closer cannot
      # make later calls reuse a partially closed interpreter or trigger an
      # implicit lifecycle retry from the finalizer.
      self$closed <- TRUE
      if (is.function(self$close_connection)) {
        self$close_connection()
      }
      invisible(self)
    },

    shutdown = function() {
      self$close()
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

    finalize = function() {
      try(self$close(), silent = TRUE)
    }
  )
)

mcp_repl_timeout_ms <- function(timeout) {
  as.integer(min(
    .Machine$integer.max,
    max(1, ceiling(timeout * 1000))
  ))
}

.mcp_repl_compress_input_bytes <- 6000L
.mcp_repl_request_limit_bytes <- 7000L

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
  input <- paste0(
    ".context <- jsonlite::fromJSON(",
    context_literal,
    ", simplifyVector = FALSE)\n",
    code,
    "\n"
  )
  input_bytes <- nchar(input, type = "bytes")
  request_bytes <- mcp_repl_request_bytes(input)
  if (
    input_bytes <= .mcp_repl_compress_input_bytes &&
      request_bytes <= .mcp_repl_request_limit_bytes
  ) {
    return(input)
  }

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

mcp_repl_inline_flex_preview <- function(text, nonce, max_bytes) {
  text <- paste(text %||% "", collapse = "\n")
  if (!nzchar(text)) {
    return(NULL)
  }

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

mcp_repl_normalize_response <- function(response) {
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
    error <- if (nzchar(text)) {
      paste("mcp-repl reported an execution error:", text)
    } else {
      "mcp-repl reported an execution error"
    }
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
