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

    execute = function(code, context = list(), .control_nonce = NULL) {
      private$assert_open()
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
      private$assert_open()
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
          rlm_control_frame_limit = self$control_frame_limit
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
  ),

  private = list(
    assert_open = function() {
      if (self$closed) {
        cli::cli_abort(
          "The mcp-repl runner is closed",
          class = c(
            "dsprrr_mcp_repl_closed_error",
            "dsprrr_interpreter_closed_error"
          )
        )
      }
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
