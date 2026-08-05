#' Recursive Language Model (RLM) Module
#'
#' @description
#' A module that transforms context from "input" to "environment", enabling LLMs
#' to programmatically explore large contexts through a REPL interface rather
#' than embedding them in prompts.
#'
#' @details
#' Instead of `llm(prompt, context=huge_document)`, RLM stores context as R
#' variables that the LLM can peek, slice, search, and recursively query.
#'
#' The execution flow is:
#' 1. Context is made available as variables in an R execution environment
#' 2. LLM generates R code to explore and analyze the context
#' 3. Code is executed by the configured code runner
#' 4. Results are fed back to the LLM for the next iteration
#' 5. Process continues until SUBMIT() is called or max_iterations reached
#' 6. If max_iterations reached without SUBMIT(), fallback extraction is used
#'
#' Available REPL tools:
#' - `SUBMIT(...)`: Terminate and return final output values
#' - `peek(var, start, end)`: View a slice of a variable (default: first 1000 chars)
#' - `search(var, pattern)`: Regex search in variable
#' - `llm_query(query, context_slice)`: Recursive LLM call (requires sub_lm)
#' - `llm_query_batched(queries, slices)`: Batched recursive calls
#'
#' Security: Code execution requires explicit opt-in via `runner` or
#' `interpreter_factory`.
#' The built-in runner uses a separate process but is NOT a security sandbox.
#' Inspect `runner$policy()` before execution. For untrusted inputs, provide a
#' runner backed by OS-level sandboxing, such as [mcp_repl_runner()].
#' Authenticated RLM control frames sent through mcp-repl are limited to 3,000
#' encoded bytes. If aggregate output is compacted into a file preview or pager,
#' the iteration fails closed because mcp-repl does not expose structured
#' compaction metadata; dsprrr does not read a sandbox-disclosed path from the
#' host process.
#'
#' Runner lifecycle: supply exactly one runtime source. `runner` is
#' caller-owned, reused across calls, and never closed by dsprrr. The backend determines
#' whether execution state persists and whether `reset()` is available;
#' serialize access to stateful backends. `interpreter_factory` is a
#' zero-argument function that returns a fresh runner implementing `execute()`,
#' `policy()`, optional `start()`, and terminal `shutdown()` or `close()`. The
#' module owns that runner for one invocation and shuts it down exactly once on
#' success, error, or interrupt.
#'
#' [run_async()] supports factory-backed RLM in an isolated mirai process and
#' rejects caller-owned runners. [stream_async()] and a module's `$stream()`
#' method remain unavailable because streaming would bypass execution. The
#' [run_stream()] one-shot `forward()` fallback remains available.
#'
#' @examples
#' \dontrun{
#' # Create a runner (required for code execution)
#' runner <- r_code_runner(timeout = 30)
#'
#' # Create an RLM module for exploring large documents
#' rlm <- rlm_module(
#'   signature = "document, question -> answer",
#'   runner = runner
#' )
#'
#' # Use it for context exploration
#' long_doc <- paste(readLines("large_file.txt"), collapse = "\n")
#' result <- run(rlm, document = long_doc, question = "What are the main themes?", .llm = llm)
#'
#' # Enable recursive LLM calls for complex reasoning
#' rlm_recursive <- rlm_module(
#'   signature = "document -> summary",
#'   runner = runner,
#'   sub_lm = ellmer::chat_openai(model = "gpt-4o-mini"),
#'   max_llm_calls = 10
#' )
#' }
#'
#' @name module-rlm
NULL


#' Create a Recursive Language Model (RLM) Module
#'
#' @description
#' Factory function to create an RLMModule that enables LLMs to programmatically
#' explore large contexts through a REPL interface.
#' Use [run()] to execute it. [run_async()] supports factory-backed modules;
#' async streaming and module `$stream()` reject RLM. [run_stream()] preserves
#' the synchronous `forward()` fallback unless a matching token-stream request
#' is active; that request is rejected first.
#'
#' @param signature A Signature object or string notation defining inputs/outputs
#' @param runner Optional caller-owned code runner implementing `execute()` and
#'   `policy()`. It is retained, never automatically closed, and must not be
#'   shared concurrently when persistent.
#' @param max_iterations Maximum REPL iterations before fallback (default 20)
#' @param max_iters DSPy 3.3-compatible alias for `max_iterations`. Supply only
#'   one of these arguments.
#' @param interpreter_factory Optional zero-argument function returning a fresh
#'   runner with `execute()`, `policy()`, optional `start()`, and idempotent
#'   terminal `shutdown()` or `close()`.
#'   Supply exactly one of `runner` and `interpreter_factory`.
#' @param max_llm_calls Maximum recursive LLM calls allowed (default 50)
#' @param max_output_chars Maximum characters per execution output (default 100000)
#' @param sub_lm Optional ellmer Chat for recursive queries. NULL = disabled.
#' @param verbose Logical. Print execution progress (default FALSE)
#' @param tools Named list of user-defined host functions. Guest code emits an
#'   authenticated request, dsprrr invokes the original function in the host,
#'   and the guest is replayed with the response. Closures are never deparsed or
#'   serialized into generated code.
#' @param ... Additional arguments passed to the module
#'
#' @return An RLMModule object
#'
#' @export
#' @examples
#' \dontrun{
#' runner <- r_code_runner(timeout = 30)
#' rlm <- rlm_module("question -> answer", runner = runner)
#' result <- run(rlm, question = "What is the 10th Fibonacci number?", .llm = llm)
#' }
rlm_module <- function(
  signature,
  runner = NULL,
  max_iterations = 20L,
  max_llm_calls = 50L,
  max_output_chars = 100000L,
  sub_lm = NULL,
  verbose = FALSE,
  tools = list(),
  max_iters = NULL,
  ...,
  interpreter_factory = NULL
) {
  binding <- normalize_code_runner_binding(
    runner = runner,
    interpreter_factory = interpreter_factory,
    module_name = "RLM"
  )

  # Parse signature if string
  if (is.character(signature)) {
    signature <- signature(signature)
  }

  if (!S7::S7_inherits(signature, Signature)) {
    cli::cli_abort(c(
      "signature must be a Signature object or string notation",
      "x" = "You provided: {.cls {class(signature)[1]}}"
    ))
  }

  if (!is.null(max_iters)) {
    if (!missing(max_iterations)) {
      cli::cli_abort(
        "Supply only one of {.arg max_iterations} and {.arg max_iters}",
        class = "dsprrr_rlm_argument_conflict"
      )
    }
    max_iterations <- max_iters
  }

  # Validate before coercion so fractional, vector, infinite, and out-of-range
  # values cannot be silently truncated or converted to NA.
  max_iterations <- normalize_rlm_bound(
    max_iterations,
    "max_iterations",
    minimum = 1L
  )
  max_llm_calls <- normalize_rlm_bound(
    max_llm_calls,
    "max_llm_calls",
    minimum = 0L
  )
  max_output_chars <- normalize_rlm_bound(
    max_output_chars,
    "max_output_chars",
    minimum = 1L
  )

  validate_rlm_tools(tools)

  RLMModule$new(
    signature = signature,
    runner = binding$runner,
    interpreter_factory = binding$interpreter_factory,
    max_iterations = max_iterations,
    max_llm_calls = max_llm_calls,
    max_output_chars = max_output_chars,
    sub_lm = sub_lm,
    verbose = verbose,
    tools = tools,
    ...
  )
}

rlm_reserved_tool_names <- function() {
  c(
    ".context",
    "SUBMIT",
    "print",
    "peek",
    "search",
    "llm_query",
    "llm_query_batched",
    "rlm_query",
    "rlm_query_batch"
  )
}

normalize_rlm_bound <- function(value, name, minimum) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == floor(value) &&
    value >= minimum &&
    value <= .Machine$integer.max
  if (!valid) {
    message <- switch(
      name,
      max_iterations = "max_iterations must be at least 1",
      max_llm_calls = "max_llm_calls must be non-negative",
      max_output_chars = "max_output_chars must be a positive integer",
      paste0(name, " is outside its supported integer range")
    )
    cli::cli_abort(message, class = "dsprrr_rlm_bounds_error")
  }
  as.integer(value)
}

validate_rlm_tools <- function(tools) {
  if (!is.list(tools)) {
    cli::cli_abort(
      c(
        "tools must be a named list of functions",
        "x" = "You provided: {.cls {class(tools)[1]}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }
  if (length(tools) == 0L) {
    return(invisible(tools))
  }

  tool_names <- names(tools)
  if (is.null(tool_names)) {
    cli::cli_abort(
      c(
        "tools must be a named list",
        "i" = "Example: {.code tools = list(my_tool = function(...) ...)}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }
  if (
    anyNA(tool_names) ||
      any(!nzchar(tool_names))
  ) {
    cli::cli_abort(
      c(
        "tools must have non-empty, non-missing names",
        "i" = "Example: {.code tools = list(my_tool = function(...) ...)}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }
  if (anyDuplicated(tool_names)) {
    duplicates <- unique(tool_names[duplicated(tool_names)])
    cli::cli_abort(
      c(
        "Tool names must be unique",
        "x" = "Duplicate name{?s}: {.val {duplicates}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  non_functions <- vapply(tools, Negate(is.function), logical(1))
  if (any(non_functions)) {
    bad_names <- tool_names[non_functions]
    cli::cli_abort(
      c(
        "All tools must be functions",
        "x" = "Non-function tool{?s}: {.val {bad_names}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  invalid_names <- tool_names[
    make.names(tool_names) != tool_names |
      tool_names == "..." |
      grepl("^\\.\\.[0-9]+$", tool_names)
  ]
  if (length(invalid_names) > 0L) {
    cli::cli_abort(
      c(
        "Tool names must be valid R identifiers",
        "x" = "Invalid name{?s}: {.val {invalid_names}}",
        "i" = "Names cannot use ellipsis pronouns such as ... or ..1."
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  collisions <- intersect(tool_names, rlm_reserved_tool_names())
  if (length(collisions) > 0L) {
    cli::cli_abort(
      c(
        "Tool names conflict with built-in RLM tools",
        "x" = "Reserved name{?s}: {.val {collisions}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  internal_collisions <- tool_names[grepl("^\\.rlm_", tool_names)]
  if (length(internal_collisions) > 0L) {
    cli::cli_abort(
      c(
        "Tool names conflict with internal RLM bindings",
        "x" = "Internal name{?s}: {.val {internal_collisions}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  base_collisions <- intersect(tool_names, ls(baseenv(), all.names = TRUE))
  if (length(base_collisions) > 0L) {
    cli::cli_abort(
      c(
        "Tool names must not mask base R functions",
        "x" = "Base name{?s}: {.val {base_collisions}}",
        "i" = "Use a domain-specific tool name instead."
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  invisible(tools)
}

normalize_rlm_sub_lm_text <- function(response) {
  text <- if (is.character(response)) {
    response
  } else if (inherits(response, "S7_object")) {
    tryCatch(response@text, error = function(e) NULL)
  } else if (is.list(response) && !is.null(response$text)) {
    response$text
  } else {
    NULL
  }
  if (
    !is.character(text) ||
      length(text) != 1L ||
      is.na(text) ||
      !nzchar(text)
  ) {
    cli::cli_abort(
      c(
        "Recursive sub-LM returned an invalid response",
        "i" = "Expected one non-empty text response, got {.cls {class(response)[1]}}."
      ),
      class = "dsprrr_rlm_sub_lm_response_error"
    )
  }
  text
}


#' RLM Module R6 Class
#'
#' @description
#' R6 class implementing the Recursive Language Model pattern: LLM-driven
#' REPL exploration of context with programmatic tools.
#'
#' @keywords internal
#' @noRd
RLMModule <- R6::R6Class(
  "RLMModule",
  inherit = Module,
  public = list(
    #' @field runner Code runner for code execution
    runner = NULL,

    #' @field interpreter_factory Factory for an invocation-owned code runner
    interpreter_factory = NULL,

    #' @field max_iterations Maximum REPL iterations before fallback
    max_iterations = NULL,

    #' @field max_llm_calls Maximum recursive LLM calls
    max_llm_calls = NULL,

    #' @field max_output_chars Maximum output size per execution
    max_output_chars = NULL,

    #' @field sub_lm Optional LLM for recursive queries
    sub_lm = NULL,

    #' @field verbose Whether to print execution progress
    verbose = NULL,

    #' @field tools User-defined REPL tools
    tools = NULL,

    #' @description
    #' Initialize an RLMModule
    #'
    #' @param signature Signature object defining inputs/outputs
    #' @param runner Code runner for code execution
    #' @param max_iterations Maximum REPL iterations
    #' @param max_llm_calls Maximum recursive LLM calls
    #' @param max_output_chars Maximum output size
    #' @param sub_lm Optional LLM for recursive queries
    #' @param verbose Whether to print progress
    #' @param tools User-defined tools
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    #' @param interpreter_factory Optional zero-argument invocation-owned runner
    #'   factory. Supply exactly one of this and `runner`.
    initialize = function(
      signature,
      runner = NULL,
      max_iterations = 20L,
      max_llm_calls = 50L,
      max_output_chars = 100000L,
      sub_lm = NULL,
      verbose = FALSE,
      tools = list(),
      config = list(),
      chat = NULL,
      interpreter_factory = NULL
    ) {
      binding <- normalize_code_runner_binding(
        runner = runner,
        interpreter_factory = interpreter_factory,
        module_name = "RLM"
      )
      if (!S7::S7_inherits(signature, Signature)) {
        cli::cli_abort(
          "{.arg signature} must be a Signature object",
          class = "dsprrr_rlm_signature_error"
        )
      }
      signature_inputs <- vapply(
        signature@inputs,
        function(input) input$name,
        character(1)
      )
      if (anyDuplicated(signature_inputs)) {
        duplicates <- unique(signature_inputs[duplicated(signature_inputs)])
        cli::cli_abort(
          c(
            "RLM signature input names must be unique",
            "x" = "Duplicate name{?s}: {.field {duplicates}}"
          ),
          class = "dsprrr_rlm_signature_error"
        )
      }
      if (rlm_host_tool_replay_field() %in% signature_inputs) {
        cli::cli_abort(
          "RLM signature input {.field {rlm_host_tool_replay_field()}} is reserved for the authenticated host-tool bridge",
          class = "dsprrr_rlm_signature_error"
        )
      }
      validate_rlm_tools(tools)
      max_iterations <- normalize_rlm_bound(
        max_iterations,
        "max_iterations",
        minimum = 1L
      )
      max_llm_calls <- normalize_rlm_bound(
        max_llm_calls,
        "max_llm_calls",
        minimum = 0L
      )
      max_output_chars <- normalize_rlm_bound(
        max_output_chars,
        "max_output_chars",
        minimum = 1L
      )
      super$initialize(
        signature = signature,
        config = config,
        chat = chat
      )

      self$runner <- binding$runner
      self$interpreter_factory <- binding$interpreter_factory
      self$max_iterations <- max_iterations
      self$max_llm_calls <- max_llm_calls
      self$max_output_chars <- max_output_chars
      self$sub_lm <- sub_lm
      self$verbose <- verbose
      self$tools <- tools

      # Store REPL history per execution
      self$state$repl_history <- list()
    },

    #' @description
    #' Execute the RLM workflow
    #'
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param ... Additional arguments
    #' @return Tibble with output, chat, metadata columns
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      # Handle inputs
      if (is.data.frame(batch)) {
        inputs <- as.list(batch[1, , drop = FALSE])
      } else {
        inputs <- batch
      }
      expected_inputs <- vapply(
        self$signature@inputs,
        function(input) input$name,
        character(1)
      )
      valid_empty_inputs <- is.list(inputs) &&
        length(inputs) == 0L &&
        length(expected_inputs) == 0L
      if (
        !is.list(inputs) ||
          (!valid_empty_inputs && is.null(names(inputs))) ||
          anyNA(names(inputs)) ||
          any(!nzchar(names(inputs))) ||
          anyDuplicated(names(inputs))
      ) {
        cli::cli_abort(
          "RLM inputs must be supplied as a named list or data frame",
          class = "dsprrr_rlm_input_error"
        )
      }
      missing_inputs <- setdiff(expected_inputs, names(inputs))
      unexpected_inputs <- setdiff(names(inputs), expected_inputs)
      if (length(missing_inputs) > 0L || length(unexpected_inputs) > 0L) {
        cli::cli_abort(
          c(
            "RLM inputs must exactly match the signature",
            if (length(missing_inputs) > 0L) {
              c("x" = "Missing: {.field {missing_inputs}}")
            },
            if (length(unexpected_inputs) > 0L) {
              c("x" = "Unexpected: {.field {unexpected_inputs}}")
            }
          ),
          class = "dsprrr_rlm_input_error"
        )
      }

      # Get LLM - clone for fresh conversation
      base_llm <- .llm %||% self$chat %||% get_default_chat()
      if (is.null(base_llm)) {
        cli::cli_abort("No LLM provided. Pass .llm or set a default chat.")
      }

      llm <- base_llm$clone()

      with_code_runner_lease(
        self$runner,
        self$interpreter_factory,
        "RLM",
        function(runner, lease) {
          start_time <- Sys.time()

          # Initialize call counter (shared across recursions)
          call_counter <- new.env()
          call_counter$count <- 0L

          # Build context description for system prompt
          context_desc <- private$describe_context(inputs)

          # Build system prompt
          system_prompt <- private$build_system_prompt(context_desc)

          # REPL iteration loop
          history <- list()
          final_answer <- NULL

          for (iter in seq_len(self$max_iterations)) {
            if (self$verbose) {
              cli::cli_alert_info("RLM Iteration {iter}/{self$max_iterations}")
            }

            # Build prompt for this iteration
            prompt <- private$build_iteration_prompt(
              system_prompt,
              history,
              iter
            )

            # Get LLM response (code generation)
            response <- private$get_code_response(llm, prompt)

            if (self$verbose) {
              cli::cli_alert(
                "Code generated: {substr(response$code, 1, 100)}..."
              )
            }

            # Execute code with RLM tools injected
            exec_result <- private$execute_with_rlm_tools(
              response$code,
              inputs,
              call_counter,
              runner,
              lease$policy
            )

            # Record in history
            history[[iter]] <- list(
              iteration = iter,
              reasoning = response$reasoning,
              code = response$code,
              output = exec_result$formatted_output,
              success = exec_result$success,
              is_final = exec_result$is_final
            )

            # Check for SUBMIT() termination
            if (isTRUE(exec_result$success) && isTRUE(exec_result$is_final)) {
              final_answer <- exec_result$final_value
              if (self$verbose) {
                cli::cli_alert_success("SUBMIT called with answer")
              }
              break
            }

            # Check for errors - feed back to LLM for retry
            if (!exec_result$success) {
              if (self$verbose) {
                cli::cli_alert_warning(
                  "Iteration {iter}: Code execution failed - {exec_result$error}"
                )
              }
              # Error will be in history, LLM can see and fix
              next
            }

            if (self$verbose) {
              cli::cli_alert(
                "Output: {substr(exec_result$formatted_output, 1, 200)}..."
              )
            }
          }

          final_source <- "submit"

          # Fallback extract if no SUBMIT()
          if (is.null(final_answer)) {
            final_source <- "fallback"
            cli::cli_warn(c(
              "RLM reached max_iterations ({self$max_iterations}) without SUBMIT()",
              "i" = "Using fallback extraction from trajectory",
              "i" = "Consider increasing max_iterations or simplifying the query"
            ))
            final_answer <- private$extract_fallback(inputs, history, llm)
          }

          # Store REPL history
          if (trace) {
            self$state$repl_history <- c(
              self$state$repl_history,
              list(list(
                timestamp = start_time,
                inputs = inputs,
                history = history,
                final_answer = private$build_output(
                  final_answer,
                  source = final_source
                ),
                iterations_used = length(history),
                llm_calls_used = call_counter$count
              ))
            )
          }

          # Build output matching signature
          output <- private$build_output(final_answer, source = final_source)

          duration_secs <- as.numeric(difftime(
            Sys.time(),
            start_time,
            units = "secs"
          ))
          duration_ms <- duration_secs * 1000

          # Build metadata
          metadata <- list(
            model = "rlm",
            iterations = length(history),
            max_iterations = self$max_iterations,
            llm_calls = call_counter$count,
            max_llm_calls = self$max_llm_calls,
            duration_ms = round(duration_ms, 2),
            repl_history = history,
            runner_policy = lease$policy_summary,
            runner_lifecycle = if (lease$owned) {
              "invocation-owned"
            } else {
              "caller-owned"
            }
          )

          tibble::tibble(
            output = list(output),
            chat = list(llm),
            metadata = list(metadata)
          )
        }
      )
    },

    #' @description
    #' Get REPL history for inspection
    #' @return List of REPL execution records
    get_repl_history = function() {
      self$state$repl_history
    },

    #' @description
    #' Create a fresh copy of this module
    #' @return New RLMModule with same settings
    reset_copy = function() {
      RLMModule$new(
        signature = self$signature,
        runner = self$runner,
        interpreter_factory = self$interpreter_factory,
        max_iterations = self$max_iterations,
        max_llm_calls = self$max_llm_calls,
        max_output_chars = self$max_output_chars,
        sub_lm = self$sub_lm,
        verbose = self$verbose,
        tools = self$tools,
        config = self$config,
        chat = self$chat
      )
    },

    #' @description
    #' Create a deep copy while preserving the configured runtime source
    #' @return New RLMModule with copied state
    deepcopy = function() {
      copied <- RLMModule$new(
        signature = self$signature,
        runner = self$runner,
        max_iterations = self$max_iterations,
        max_llm_calls = self$max_llm_calls,
        max_output_chars = self$max_output_chars,
        sub_lm = self$sub_lm,
        verbose = self$verbose,
        tools = self$tools,
        config = lapply(self$config, identity),
        chat = self$chat,
        interpreter_factory = self$interpreter_factory
      )
      copied$state <- lapply(self$state, identity)
      copied
    },

    #' @description
    #' Print method for RLMModule
    print = function() {
      # Format signature
      input_names <- vapply(
        self$signature@inputs,
        function(x) x$name,
        character(1)
      )
      sig_str <- paste0(
        paste(input_names, collapse = ", "),
        " -> ",
        private$get_output_names()
      )

      cli::cli_h3("RLMModule")
      cli::cli_bullets(c(
        "*" = "Signature: {sig_str}",
        "*" = "Max iterations: {.val {self$max_iterations}}",
        "*" = "Max LLM calls: {.val {self$max_llm_calls}}",
        "*" = "Runner: {code_runner_binding_label(self$runner, self$interpreter_factory)}",
        "*" = "Recursive queries: {.val {if (is.null(self$sub_lm)) 'disabled' else 'enabled'}}",
        "*" = "Custom tools: {.val {length(self$tools)}}"
      ))
      invisible(self)
    }
  ),

  private = list(
    #' Get output field names from signature for display
    get_output_names = function() {
      paste(private$get_output_field_names(), collapse = ", ")
    },

    #' Get output field specs from signature
    get_output_specs = function() {
      output_type <- self$signature@output_type
      if (methods::.hasSlot(output_type, "properties")) {
        props <- output_type@properties
        if (length(props) > 0) {
          return(props)
        }
      }
      list(answer = output_type)
    },

    #' Get output field names from signature
    get_output_field_names = function() {
      names(private$get_output_specs())
    },

    #' Describe context variables for the system prompt
    describe_context = function(inputs) {
      if (length(inputs) == 0) {
        return("No context variables available.")
      }

      descriptions <- vapply(
        names(inputs),
        function(name) {
          val <- inputs[[name]]
          type_str <- class(val)[1]
          size_str <- if (is.character(val)) {
            total_chars <- sum(nchar(val))
            paste0(total_chars, " characters")
          } else if (is.data.frame(val)) {
            paste0(nrow(val), " rows x ", ncol(val), " cols")
          } else if (is.list(val)) {
            paste0(length(val), " elements")
          } else if (is.vector(val)) {
            paste0(length(val), " elements")
          } else {
            "1 object"
          }

          preview <- if (is.character(val) && length(val) == 1) {
            if (nchar(val) > 100) {
              paste0(substr(val, 1, 100), "...")
            } else {
              val
            }
          } else {
            deparse(val, width.cutoff = 60)[1]
          }

          paste0(
            "- `.context$",
            name,
            "` (",
            type_str,
            ", ",
            size_str,
            ")\n",
            "  Preview: ",
            preview
          )
        },
        character(1)
      )

      paste(descriptions, collapse = "\n\n")
    },

    #' Build the system prompt for RLM
    build_system_prompt = function(context_desc) {
      has_sub_lm <- !is.null(self$sub_lm)
      output_fields <- private$get_output_field_names()
      submit_usage <- if (length(output_fields) == 1) {
        paste0("SUBMIT(", output_fields[[1]], ")")
      } else {
        paste0(
          "SUBMIT(",
          paste(output_fields, collapse = ", "),
          ") or SUBMIT(",
          paste0(output_fields, " = ...", collapse = ", "),
          ")"
        )
      }

      # Build tool descriptions
      tool_desc <- c(
        paste0(
          "- `",
          submit_usage,
          "`: Submit final output field values and terminate"
        ),
        "- `peek(var, start = 1, end = 1000)`: View a character slice of a variable",
        "- `search(var, pattern)`: Regex search in variable, returns matches"
      )

      if (has_sub_lm) {
        tool_desc <- c(
          tool_desc,
          "- `llm_query(query, context_slice = NULL)`: Ask a sub-question to another LLM",
          "- `llm_query_batched(queries, slices = NULL)`: Batch multiple sub-questions"
        )
      }

      if (length(self$tools) > 0) {
        custom_tool_names <- names(self$tools)
        tool_desc <- c(
          tool_desc,
          paste0("- `", custom_tool_names, "()`: User-defined tool")
        )
      }

      glue::glue(
        "
You are working in an R REPL environment. Your goal is to answer the query by
writing R code to explore and analyze the provided context.

## Available Variables
{context_desc}

## Available Functions
{paste(tool_desc, collapse = '
')}

## Rules
1. Explore the context programmatically - don't ask to see all content at once
2. Use peek() to examine slices of large text
3. Use search() to find specific patterns
4. Break complex queries into smaller sub-questions{if (has_sub_lm) ' using llm_query()' else ''}
5. Call {submit_usage} when you have the final answer
6. You have {self$max_iterations} iterations{if (has_sub_lm) paste0(' and ', self$max_llm_calls, ' LLM calls') else ''}

## Response Format
Return JSON with:
- \"reasoning\": Your thought process for this step
- \"code\": R code to execute (single string)

The code's output will be shown to you. Continue until you call SUBMIT().
"
      )
    },

    #' Build prompt for a specific iteration
    build_iteration_prompt = function(system_prompt, history, iter) {
      if (iter == 1) {
        # First iteration - just system prompt
        return(system_prompt)
      }

      # Build history context
      history_parts <- vapply(
        history,
        function(h) {
          glue::glue(
            "
## Iteration {h$iteration}
Reasoning: {h$reasoning}

Code:
```r
{h$code}
```

{if (h$success) 'Output:' else 'Error:'}
{h$output}
{if (h$is_final) '(SUBMIT was called)' else ''}
"
          )
        },
        character(1)
      )

      paste0(
        system_prompt,
        "\n\n## Previous Iterations\n",
        paste(history_parts, collapse = "\n"),
        "\n\n## Next Step\nContinue exploring or call SUBMIT() with your answer."
      )
    },

    #' Get code response from LLM
    get_code_response = function(llm, prompt) {
      output_type <- ellmer::type_object(
        reasoning = ellmer::type_string(
          description = "Your thought process for this step"
        ),
        code = ellmer::type_string(
          description = "R code to execute"
        )
      )

      result <- tryCatch(
        llm$chat_structured(prompt, type = output_type),
        error = function(e) {
          cli::cli_abort(c(
            "Failed to get code from LLM",
            "x" = "Error: {e$message}"
          ))
        }
      )

      if (is.null(result$code) || !is.character(result$code)) {
        cli::cli_abort(c(
          "LLM returned invalid response",
          "i" = "Missing or invalid 'code' field"
        ))
      }

      list(
        reasoning = result$reasoning %||% "",
        code = strip_rlm_code_fences(result$code)
      )
    },

    #' Execute code with RLM tools injected
    execute_with_rlm_tools = function(
      code,
      inputs,
      call_counter,
      runner,
      runner_policy
    ) {
      code <- strip_rlm_code_fences(code)
      control_nonce <- rlm_control_nonce()

      # Build RLM prelude that defines tools
      rlm_prelude <- create_rlm_prelude(
        max_llm_calls = self$max_llm_calls,
        has_sub_lm = !is.null(self$sub_lm),
        custom_tools = self$tools,
        output_fields = private$get_output_field_names(),
        control_nonce = control_nonce,
        control_frame_limit = runner_policy$rlm_control_frame_limit %||% Inf
      )

      # Build combined code: prelude + user code
      combined_code <- paste0(
        "# RLM Prelude\n",
        rlm_prelude,
        "\n\n# User Code\n",
        code
      )

      # Execute with inputs as context
      tool_replay <- list()
      repeat {
        execution_context <- inputs
        execution_context[[rlm_host_tool_replay_field()]] <- tool_replay
        result <- execute_code_runner(
          runner,
          combined_code,
          context = execution_context,
          .control_nonce = control_nonce
        )

        control_value <- NULL
        for (candidate in list(
          result$result,
          result$stdout,
          result$stderr,
          result$error
        )) {
          decoded <- decode_rlm_control(candidate, control_nonce)
          if (!is.null(decoded)) {
            control_value <- decoded
            break
          }
        }
        if (!is_rlm_host_tool_request(control_value)) {
          break
        }
        if (length(tool_replay) >= 1000L) {
          cli::cli_abort(
            "RLM exceeded the per-iteration host-tool bridge limit",
            class = "dsprrr_rlm_host_tool_limit_error"
          )
        }
        request <- control_value
        if (!identical(request$index, length(tool_replay) + 1L)) {
          cli::cli_abort(
            "RLM host-tool requests were returned out of order",
            class = "dsprrr_rlm_host_tool_protocol_error"
          )
        }
        if (!request$name %in% names(self$tools)) {
          cli::cli_abort(
            "RLM requested unknown host tool {.val {request$name}}",
            class = "dsprrr_rlm_host_tool_protocol_error"
          )
        }
        tool_outcome <- tryCatch(
          list(
            success = TRUE,
            value = do.call(self$tools[[request$name]], request$arguments),
            error = NULL
          ),
          interrupt = function(condition) stop(condition),
          error = function(condition) {
            list(
              success = FALSE,
              value = NULL,
              error = conditionMessage(condition)
            )
          }
        )
        tool_replay[[length(tool_replay) + 1L]] <- c(
          list(request = unclass(request)),
          tool_outcome
        )
      }

      if (!isTRUE(result$success)) {
        control_value <- NULL
      }

      # Detect SUBMIT termination using the versioned runner-neutral envelope.
      is_final <- is_rlm_final(control_value)
      final_value <- if (is_final) {
        extract_rlm_final(control_value)
      } else {
        NULL
      }

      # Handle rlm_query requests (if sub_lm is available)
      if (is_rlm_query_request(control_value) && !is.null(self$sub_lm)) {
        # Process the recursive query (single or batch)
        query_result <- private$process_rlm_query(
          control_value,
          call_counter
        )

        # Return the query result as the output
        return(list(
          success = query_result$success,
          is_final = FALSE,
          final_value = NULL,
          formatted_output = query_result$formatted_output,
          error = query_result$error,
          raw_result = result
        ))
      }

      # Format output for history
      formatted_output <- if (result$success) {
        private$format_execution_output(result)
      } else {
        paste("Error:", result$error %||% "Unknown error")
      }

      # Truncate if too long
      if (nchar(formatted_output) > self$max_output_chars) {
        formatted_output <- paste0(
          substr(formatted_output, 1, self$max_output_chars),
          "\n... [TRUNCATED]"
        )
      }

      list(
        success = result$success,
        is_final = is_final,
        final_value = final_value,
        formatted_output = formatted_output,
        error = result$error,
        raw_result = result
      )
    },

    #' Process an rlm_query request (single or batch)
    #'
    #' @return List with success, formatted_output, error fields
    process_rlm_query = function(request, call_counter) {
      # Handle batch queries
      if (isTRUE(request$batch)) {
        return(private$process_rlm_query_batch(request, call_counter))
      }

      # Single query processing
      # Check call limit
      if (call_counter$count >= self$max_llm_calls) {
        error_msg <- paste0(
          "Maximum LLM calls (",
          self$max_llm_calls,
          ") exceeded"
        )
        cli::cli_warn(error_msg)
        return(list(
          success = FALSE,
          formatted_output = paste0("Error: ", error_msg),
          error = error_msg
        ))
      }

      call_counter$count <- call_counter$count + 1L

      query <- request$query
      context_slice <- request$context

      prompt <- if (!is.null(context_slice)) {
        paste0("Context:\n", context_slice, "\n\nQuestion: ", query)
      } else {
        query
      }

      result <- tryCatch(
        {
          response <- self$sub_lm$chat(prompt)
          text <- normalize_rlm_sub_lm_text(response)
          list(
            success = TRUE,
            formatted_output = paste0("Query result: ", text),
            error = NULL
          )
        },
        error = function(e) {
          if (inherits(e, "dsprrr_rlm_sub_lm_response_error")) {
            stop(e)
          }
          cli::cli_warn(c(
            "Recursive LLM query failed",
            "x" = "Error: {e$message}",
            "i" = "Query: {substr(query, 1, 100)}..."
          ))
          list(
            success = FALSE,
            formatted_output = paste0("Query error: ", e$message),
            error = e$message
          )
        }
      )

      result
    },

    #' Process batch rlm_query requests
    #'
    #' @return List with success, formatted_output, error fields
    process_rlm_query_batch = function(request, call_counter) {
      queries <- request$queries
      slices <- request$slices
      n_queries <- length(queries)

      # Check if we have enough calls remaining
      remaining_calls <- self$max_llm_calls - call_counter$count
      if (n_queries > remaining_calls) {
        error_msg <- paste0(
          "Batch of ",
          n_queries,
          " queries would exceed limit. ",
          "Remaining calls: ",
          remaining_calls
        )
        cli::cli_warn(error_msg)
        return(list(
          success = FALSE,
          formatted_output = paste0("Error: ", error_msg),
          error = error_msg
        ))
      }

      if (n_queries == 0L) {
        return(list(
          success = TRUE,
          formatted_output = "",
          error = NULL
        ))
      }

      # Reserve call budget up-front to ensure consistent accounting
      call_counter$count <- call_counter$count + n_queries

      prompts <- vapply(
        seq_len(n_queries),
        function(i) {
          query <- queries[[i]]
          context_slice <- if (!is.null(slices)) slices[[i]] else NULL
          if (!is.null(context_slice)) {
            paste0("Context:\n", context_slice, "\n\nQuestion: ", query)
          } else {
            query
          }
        },
        character(1)
      )

      batch_result <- private$run_batched_sub_lm_queries(prompts)
      results <- batch_result$results
      errors <- batch_result$errors

      # Format output
      formatted_parts <- vapply(
        seq_len(n_queries),
        function(i) {
          result_i <- results[[i]]
          result_text <- if (is.null(result_i)) {
            ""
          } else {
            paste(as.character(result_i), collapse = "\n")
          }
          paste0("Query ", i, " result: ", result_text)
        },
        character(1)
      )
      formatted_output <- paste(formatted_parts, collapse = "\n\n")

      if (length(errors) > 0) {
        cli::cli_warn(c(
          "Some batch queries failed",
          "x" = errors
        ))
      }

      list(
        success = length(errors) == 0,
        formatted_output = formatted_output,
        error = if (length(errors) > 0) paste(errors, collapse = "; ") else NULL
      )
    },

    #' Run a batch of sub-LM prompts with bounded parallelism
    run_batched_sub_lm_queries = function(prompts) {
      n_queries <- length(prompts)

      # For a single query, avoid parallel overhead
      if (n_queries <= 1L) {
        return(private$run_batched_sub_lm_queries_sequential(prompts))
      }

      has_parallel_chat <- exists(
        "parallel_chat",
        envir = asNamespace("ellmer"),
        inherits = FALSE
      )

      if (!has_parallel_chat) {
        cli::cli_inform(c(
          "i" = "Falling back to sequential batch queries.",
          "i" = "{.fn ellmer::parallel_chat} not available; upgrade ellmer for parallel execution."
        ))
        return(private$run_batched_sub_lm_queries_sequential(prompts))
      }

      max_active <- private$get_rlm_batch_max_active(n_queries)

      parallel_turns <- tryCatch(
        {
          ellmer::parallel_chat(
            chat = self$sub_lm,
            prompts = as.list(prompts),
            max_active = max_active,
            on_error = "return"
          )
        },
        interrupt = function(i) stop(i),
        error = function(e) {
          e
        }
      )

      if (inherits(parallel_turns, "error")) {
        return(private$build_parallel_batch_failure(
          n_queries = n_queries,
          message = conditionMessage(parallel_turns)
        ))
      }

      if (
        is.null(parallel_turns) ||
          !is.list(parallel_turns) ||
          length(parallel_turns) != n_queries
      ) {
        return(private$build_parallel_batch_failure(
          n_queries = n_queries,
          message = "parallel_chat() returned an invalid response shape"
        ))
      }

      results <- vector("list", n_queries)
      errors <- character()

      for (i in seq_len(n_queries)) {
        parsed <- private$extract_parallel_query_result(parallel_turns[[i]], i)
        results[[i]] <- parsed$result
        if (!is.null(parsed$error)) {
          errors <- c(errors, parsed$error)
        }
      }

      list(results = results, errors = errors)
    },

    #' Build per-query failures for infrastructure-level parallel errors
    build_parallel_batch_failure = function(n_queries, message) {
      error_msg <- paste0(
        "Parallel batch infrastructure error (queries not retried): ",
        message
      )

      results <- rep(list(paste0("[Error: ", error_msg, "]")), n_queries)
      errors <- vapply(
        seq_len(n_queries),
        function(i) paste0("Query ", i, ": ", error_msg),
        character(1)
      )

      list(results = results, errors = errors)
    },

    #' Sequential fallback for batched sub-LM queries
    run_batched_sub_lm_queries_sequential = function(prompts) {
      n_queries <- length(prompts)
      results <- vector("list", n_queries)
      errors <- character()

      for (i in seq_len(n_queries)) {
        results[[i]] <- tryCatch(
          {
            normalize_rlm_sub_lm_text(self$sub_lm$chat(prompts[[i]]))
          },
          error = function(e) {
            if (inherits(e, "dsprrr_rlm_sub_lm_response_error")) {
              stop(e)
            }
            errors <<- c(errors, paste0("Query ", i, ": ", e$message))
            paste0("[Error: ", e$message, "]")
          }
        )
      }

      list(results = results, errors = errors)
    },

    #' Extract a text result from ellmer::parallel_chat() output
    extract_parallel_query_result = function(turn_or_error, index) {
      if (is.null(turn_or_error) || inherits(turn_or_error, "error")) {
        msg <- if (inherits(turn_or_error, "error")) {
          conditionMessage(turn_or_error)
        } else {
          "Unknown error"
        }
        return(list(
          result = paste0("[Error: ", msg, "]"),
          error = paste0("Query ", index, ": ", msg)
        ))
      }

      text <- tryCatch(
        {
          turn <- turn_or_error$last_turn()
          if (inherits(turn, "S7_object")) {
            turn@text
          } else if (is.list(turn) && !is.null(turn$text)) {
            turn$text
          } else if (is.character(turn)) {
            turn[[1]]
          } else {
            NULL
          }
        },
        error = function(e) {
          cli::cli_warn(c(
            "Failed to extract text from parallel query result {index}.",
            "x" = "{e$message}"
          ))
          NULL
        }
      )

      list(result = normalize_rlm_sub_lm_text(text), error = NULL)
    },

    #' Determine bounded parallelism for RLM batch calls
    get_rlm_batch_max_active = function(n_queries) {
      max_active <- getOption("dsprrr.rlm_batch_max_active", 10L)
      if (
        !is.numeric(max_active) ||
          length(max_active) != 1L ||
          is.na(max_active) ||
          max_active < 1
      ) {
        max_active <- 10L
      }
      as.integer(min(n_queries, floor(max_active)))
    },

    #' Format execution output for history
    format_execution_output = function(result) {
      parts <- character()

      # Safely check stdout (may be NULL or missing)
      stdout_val <- result$stdout
      if (
        !is.null(stdout_val) &&
          is.character(stdout_val) &&
          nchar(stdout_val) > 0
      ) {
        parts <- c(parts, paste0("stdout:\n", stdout_val))
      }

      # Safely check messages (may be NULL or missing)
      messages_val <- result$messages
      if (
        !is.null(messages_val) &&
          is.character(messages_val) &&
          nchar(messages_val) > 0
      ) {
        parts <- c(parts, paste0("messages:\n", messages_val))
      }

      # Safely check warnings (may be NULL or missing)
      warnings_val <- result$warnings
      if (
        !is.null(warnings_val) &&
          is.character(warnings_val) &&
          nchar(warnings_val) > 0
      ) {
        parts <- c(parts, paste0("warnings:\n", warnings_val))
      }

      if (!is.null(result$result)) {
        result_str <- tryCatch(
          {
            if (is.data.frame(result$result)) {
              paste(
                utils::capture.output(print(result$result)),
                collapse = "\n"
              )
            } else if (
              is.atomic(result$result) && length(result$result) <= 10
            ) {
              paste(result$result, collapse = ", ")
            } else {
              paste(utils::capture.output(str(result$result)), collapse = "\n")
            }
          },
          error = function(e) deparse(result$result)[1]
        )
        parts <- c(parts, paste0("result:\n", result_str))
      }

      if (length(parts) == 0) {
        return("[No output]")
      }

      paste(parts, collapse = "\n\n")
    },

    #' Extract answer via fallback when max_iterations reached
    extract_fallback = function(inputs, history, llm) {
      # Build trajectory summary
      trajectory <- vapply(
        history,
        function(h) {
          glue::glue(
            "Iteration {h$iteration}:
Reasoning: {h$reasoning}
Code: {h$code}
Output: {substr(h$output, 1, 500)}"
          )
        },
        character(1)
      )

      input_context <- private$format_inputs_for_prompt(inputs)

      prompt <- glue::glue(
        "
The RLM agent ran out of iterations before calling SUBMIT().
Based on the exploration trajectory below, extract the best possible answer.

## Original Query
{input_context}

## Exploration Trajectory
{paste(trajectory, collapse = '
---
')}

## Task
Based on the above exploration, provide the final answer to the original query.
Be concise and direct. If the exploration was incomplete, provide the best
answer possible with what was discovered.
"
      )

      structured_result <- tryCatch(
        llm$chat_structured(prompt, type = self$signature@output_type),
        error = function(e) {
          cli::cli_warn(c(
            "Structured fallback extraction failed",
            "x" = "Error: {e$message}",
            "i" = "Falling back to unstructured extraction"
          ))
          NULL
        }
      )

      if (!is.null(structured_result)) {
        return(structured_result)
      }

      tryCatch(
        llm$chat(prompt),
        error = function(e) {
          cli::cli_warn(c(
            "Fallback extraction failed",
            "x" = "Error: {e$message}",
            "i" = "Returning error message as answer"
          ))
          paste0("[Fallback extraction failed: ", e$message, "]")
        }
      )
    },

    #' Format inputs for prompt display
    format_inputs_for_prompt = function(inputs) {
      parts <- vapply(
        names(inputs),
        function(name) {
          val <- inputs[[name]]
          if (is.character(val) && length(val) == 1) {
            if (nchar(val) > 500) {
              val <- paste0(substr(val, 1, 500), "... [truncated]")
            }
            paste0(name, ": ", val)
          } else {
            paste0(name, ": ", deparse(val, width.cutoff = 500)[1])
          }
        },
        character(1)
      )
      paste(parts, collapse = "\n")
    },

    #' Coerce final answer payload into named signature fields
    normalize_final_answer = function(
      answer,
      source = c("submit", "fallback")
    ) {
      source <- match.arg(source)
      output_specs <- private$get_output_specs()
      output_fields <- names(output_specs)

      # Convert arbitrary value into named list of output fields
      normalized <- NULL

      if (is.list(answer)) {
        answer_names <- names(answer)

        # Named list: align by field names
        if (!is.null(answer_names) && all(nzchar(answer_names))) {
          # Special compatibility case for legacy single-field answer payloads
          if (
            length(output_fields) == 1 &&
              !output_fields[[1]] %in% answer_names &&
              "answer" %in% answer_names
          ) {
            normalized <- setNames(list(answer[["answer"]]), output_fields)
          } else {
            missing <- setdiff(output_fields, answer_names)
            extra <- setdiff(answer_names, output_fields)

            if (length(missing) == 0 && length(extra) == 0) {
              normalized <- answer[output_fields]
            } else if (source == "fallback") {
              normalized <- setNames(
                vector("list", length(output_fields)),
                output_fields
              )
              for (field in output_fields) {
                if (field %in% answer_names) {
                  normalized[[field]] <- answer[[field]]
                } else {
                  normalized[[field]] <- NA
                }
              }
            } else {
              cli::cli_abort(c(
                "SUBMIT output does not match signature fields",
                "x" = "Expected fields: {.val {output_fields}}",
                "x" = "Received fields: {.val {answer_names}}"
              ))
            }
          }
        } else if (length(answer) == length(output_fields)) {
          # Positional list
          normalized <- answer
          names(normalized) <- output_fields
        } else if (length(output_fields) == 1 && length(answer) >= 1) {
          normalized <- setNames(list(answer[[1]]), output_fields)
        } else if (source == "fallback") {
          normalized <- setNames(
            vector("list", length(output_fields)),
            output_fields
          )
          for (i in seq_along(output_fields)) {
            if (i <= length(answer)) {
              normalized[[output_fields[[i]]]] <- answer[[i]]
            } else {
              normalized[[output_fields[[i]]]] <- NA
            }
          }
        } else {
          cli::cli_abort(c(
            "SUBMIT output has invalid length",
            "x" = "Expected {length(output_fields)} value(s), got {length(answer)}"
          ))
        }
      } else if (is.atomic(answer) && length(answer) == length(output_fields)) {
        # Positional atomic vector for multi-output
        normalized <- as.list(answer)
        names(normalized) <- output_fields
      } else if (length(output_fields) == 1) {
        normalized <- setNames(list(answer), output_fields)
      } else if (source == "fallback") {
        normalized <- setNames(
          vector("list", length(output_fields)),
          output_fields
        )
        normalized[[output_fields[[1]]]] <- answer
        if (length(output_fields) > 1) {
          for (i in 2:length(output_fields)) {
            normalized[[output_fields[[i]]]] <- NA
          }
        }
      } else {
        cli::cli_abort(c(
          "SUBMIT output could not be aligned to signature fields",
          "x" = "Expected fields: {.val {output_fields}}"
        ))
      }

      # Coerce to declared output types where practical
      for (field in output_fields) {
        normalized[[field]] <- private$coerce_value_to_type(
          normalized[[field]],
          output_specs[[field]]
        )
      }

      normalized
    },

    #' Coerce a value to an ellmer type when practical
    coerce_value_to_type = function(value, type_spec) {
      if (is.null(type_spec)) {
        return(value)
      }

      if (inherits(type_spec, "ellmer::TypeBasic")) {
        type_name <- type_spec@type
        if (identical(type_name, "string")) {
          return(value)
        }
        if (identical(type_name, "number")) {
          return(suppressWarnings(as.numeric(value)[1]))
        }
        if (identical(type_name, "integer")) {
          return(suppressWarnings(as.integer(value)[1]))
        }
        if (identical(type_name, "boolean")) {
          return(suppressWarnings(as.logical(value)[1]))
        }
        return(value)
      }

      if (inherits(type_spec, "ellmer::TypeEnum")) {
        allowed <- as.character(type_spec@values)
        candidate <- if (length(value) == 0 || is.null(value)) {
          ""
        } else {
          as.character(value)[1]
        }

        if (candidate %in% allowed || length(allowed) == 0) {
          return(candidate)
        }

        idx <- match(tolower(candidate), tolower(allowed))
        if (!is.na(idx)) {
          return(allowed[idx])
        }

        candidate
      } else if (inherits(type_spec, "ellmer::TypeArray")) {
        items <- if (is.list(value)) value else as.list(value)
        if (length(items) == 0) {
          return(items)
        }

        coerced <- lapply(
          items,
          function(item) private$coerce_value_to_type(item, type_spec@items)
        )

        if (
          all(vapply(
            coerced,
            function(x) is.atomic(x) && length(x) == 1,
            logical(1)
          ))
        ) {
          return(unlist(coerced, use.names = FALSE))
        }
        coerced
      } else {
        value
      }
    },

    #' Build output matching signature
    build_output = function(answer, source = c("submit", "fallback")) {
      source <- match.arg(source)
      private$normalize_final_answer(answer, source = source)
    }
  )
)


#' Run a Recursive Language Model in one call
#'
#' @description
#' Convenience wrapper that creates a runner, module, and executes an RLM
#' in a single call. Equivalent to:
#'
#' ```r
#' runner <- r_code_runner(timeout = .timeout)
#' mod <- rlm_module(
#'   signature,
#'   runner = runner,
#'   max_iterations = .max_iterations,
#'   max_llm_calls = .max_llm_calls,
#'   sub_lm = .sub_lm,
#'   verbose = .verbose,
#'   tools = .tools
#' )
#' run(mod, ..., .llm = .llm)
#' ```
#'
#' For repeated use or optimization, prefer creating a module with
#' [rlm_module()] and calling [run()] separately.
#'
#' @param signature A Signature object or string notation defining inputs/outputs
#'   (e.g., `"question -> answer"`)
#' @param ... Named arguments matching the signature's inputs. These are passed
#'   to [run()].
#' @param .llm An ellmer Chat object. If `NULL`, uses the default Chat from
#'   [get_default_chat()].
#' @param .timeout Numeric. Maximum execution time in seconds per code
#'   evaluation. Default 30.
#' @param .max_iterations Integer. Maximum REPL iterations before fallback.
#'   Default 20.
#' @param .max_llm_calls Integer. Maximum recursive LLM calls allowed.
#'   Default 50.
#' @param .sub_lm Optional ellmer Chat for recursive `llm_query()` calls.
#'   `NULL` disables recursive queries.
#' @param .tools Named list of user-defined R functions available in the REPL.
#' @param .verbose Logical. Print execution progress. Default `FALSE`.
#'
#' @return The module output according to the signature.
#'
#' @export
#' @examples
#' \dontrun{
#' # One-liner RLM call
#' result <- rlm(
#'   "document, question -> answer",
#'   document = readLines("big_file.txt") |> paste(collapse = "\n"),
#'   question = "What are the main themes?",
#'   .llm = ellmer::chat_openai()
#' )
#'
#' # With recursive sub-queries
#' result <- rlm(
#'   "codebase, question -> answer",
#'   codebase = source_code,
#'   question = "How does auth work?",
#'   .llm = ellmer::chat_openai(),
#'   .sub_lm = ellmer::chat_openai(model = "gpt-4o-mini")
#' )
#' }
#'
#' @seealso
#' * [rlm_module()] for creating reusable RLM modules
#' * [r_code_runner()] for configuring the code execution backend
#' * [run()] for executing modules
#' * [dsp()] for simple one-shot LLM calls (no code execution)
rlm <- function(
  signature,
  ...,
  .llm = NULL,
  .timeout = 30,
  .max_iterations = 20L,
  .max_llm_calls = 50L,
  .sub_lm = NULL,
  .tools = list(),
  .verbose = FALSE
) {
  runner <- r_code_runner(timeout = .timeout)

  mod <- rlm_module(
    signature = signature,
    runner = runner,
    max_iterations = .max_iterations,
    max_llm_calls = .max_llm_calls,
    sub_lm = .sub_lm,
    verbose = .verbose,
    tools = .tools
  )

  run(mod, ..., .llm = .llm)
}
