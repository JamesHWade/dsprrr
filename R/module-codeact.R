#' CodeAct Module
#'
#' @description
#' A hybrid agent module that combines tool calling with R code execution.
#' The model can choose between calling registered tools or generating R code
#' to solve problems. This enables flexible agentic workflows that leverage
#' both external tools and computational capabilities.
#'
#' @details
#' CodeAct extends the ReAct pattern by adding an `execute_r_code` tool that
#' allows the agent to write and run R code. The execution flow is:
#'
#' 1. Agent receives the task and available tools (including code execution)
#' 2. Agent iteratively calls tools or executes code until it has enough info
#' 3. Agent produces final structured answer
#'
#' Security: Code execution requires explicit opt-in via `runner` or
#' `interpreter_factory`.
#' The built-in runner uses a separate process but is NOT a security sandbox.
#' Inspect `runner$policy()` before execution. For untrusted inputs, provide a
#' runner backed by OS-level sandboxing.
#'
#' Runner lifecycle: supply exactly one runtime source. `runner` is
#' caller-owned, reused across calls, and never closed by dsprrr. The backend determines
#' whether execution state persists and whether `reset()` is available;
#' serialize access to stateful backends. `interpreter_factory` is a
#' zero-argument function that returns a fresh runner implementing `execute()`,
#' `policy()`, optional `start()`, and terminal `shutdown()` or `close()`. The
#' module owns that runner for one invocation and shuts it down exactly once on
#' success, error, or interrupt. Any retained code tool becomes terminal after
#' shutdown.
#'
#' [run_async()] supports factory-backed CodeAct in an isolated mirai process.
#' It rejects caller-owned runners. [stream_async()] and a module's `$stream()`
#' method remain unavailable because streaming would bypass execution. The
#' [run_stream()] one-shot `forward()` fallback remains available.
#'
#' @examples
#' \dontrun{
#' # Create a runner for code execution
#' runner <- r_code_runner(timeout = 30)
#'
#' # Create a CodeAct agent with custom tools
#' search_tool <- ellmer::tool(
#'   function(query) "Search results...",
#'   description = "Search for information"
#' )
#'
#' agent <- code_act(
#'   signature = "question -> answer",
#'   tools = list(search = search_tool),
#'   runner = runner
#' )
#'
#' # The agent can now search AND compute
#' result <- run(agent,
#'   question = "What is 10% of France's population?",
#'   .llm = llm
#' )
#' }
#'
#' @name module-codeact
NULL


#' Create a CodeAct Module
#'
#' @description
#' Factory function to create a CodeActModule that can use both tools and
#' R code execution to solve problems.
#' Use [run()] to execute it. [run_async()] supports factory-backed modules;
#' async streaming and module `$stream()` reject CodeAct. [run_stream()]
#' preserves the synchronous `forward()` fallback unless a matching
#' token-stream request is active; that request is rejected first.
#'
#' @param signature A Signature object or string notation defining inputs/outputs
#' @param tools List of ellmer ToolDef objects for the agent to use. Non-empty
#'   list element names become the registered tool names; unnamed elements keep
#'   their ToolDef name. Effective names may contain only letters, numbers,
#'   hyphens, and underscores.
#' @param runner Optional caller-owned code runner implementing `execute()` and
#'   `policy()`. It is retained, never automatically closed, and must not be
#'   shared concurrently when persistent.
#' @param max_iterations Maximum outer agent iterations and maximum tool calls
#'   within one invocation (default 10). Exceeding the inner tool-call budget
#'   raises a `dsprrr_codeact_iteration_limit` error.
#' @param interpreter_factory Optional zero-argument function returning a fresh
#'   runner with `execute()`, `policy()`, optional `start()`, and idempotent
#'   terminal `shutdown()` or `close()`.
#'   Supply exactly one of `runner` and `interpreter_factory`.
#' @param ... Additional arguments passed to the module
#'
#' @return A CodeActModule object
#'
#' @export
#' @examples
#' \dontrun{
#' runner <- r_code_runner(timeout = 30)
#' agent <- code_act(
#'   "question -> answer",
#'   tools = list(),
#'   runner = runner
#' )
#' result <- run(agent, question = "Calculate 2^10", .llm = llm)
#' }
code_act <- function(
  signature,
  tools = list(),
  runner = NULL,
  max_iterations = 10L,
  ...,
  interpreter_factory = NULL
) {
  binding <- normalize_code_runner_binding(
    runner = runner,
    interpreter_factory = interpreter_factory,
    module_name = "CodeAct"
  )
  tools <- validate_codeact_tools(tools)

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

  max_iterations <- normalize_codeact_iterations(max_iterations)

  CodeActModule$new(
    signature = signature,
    tools = tools,
    runner = binding$runner,
    interpreter_factory = binding$interpreter_factory,
    max_iterations = max_iterations,
    ...
  )
}

normalize_codeact_iterations <- function(value) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == floor(value) &&
    value >= 1L &&
    value <= .Machine$integer.max
  if (!valid) {
    cli::cli_abort(
      "{.arg max_iterations} must be one positive integer",
      class = "dsprrr_codeact_bounds_error"
    )
  }
  as.integer(value)
}

validate_codeact_tools <- function(tools) {
  if (!is.list(tools)) {
    cli::cli_abort(
      "{.arg tools} must be a list of ellmer ToolDef objects",
      class = "dsprrr_codeact_tools_error"
    )
  }
  if (length(tools) == 0L) {
    return(tools)
  }

  valid_tools <- vapply(
    tools,
    function(tool) {
      inherits(tool, "ellmer::ToolDef") || inherits(tool, "ToolDef")
    },
    logical(1)
  )
  if (any(!valid_tools)) {
    invalid <- which(!valid_tools)
    cli::cli_abort(
      c(
        "All CodeAct tools must be ellmer ToolDef objects",
        "x" = "Invalid position{?s}: {.val {invalid}}"
      ),
      class = "dsprrr_codeact_tools_error"
    )
  }

  declared_names <- names(tools)
  if (is.null(declared_names)) {
    declared_names <- rep("", length(tools))
  }
  if (anyNA(declared_names)) {
    cli::cli_abort(
      "CodeAct tool list names cannot be missing",
      class = "dsprrr_codeact_tools_error"
    )
  }
  intrinsic_names <- codeact_tool_names(tools)
  tool_names <- ifelse(nzchar(declared_names), declared_names, intrinsic_names)
  valid_names <- grepl("^[A-Za-z0-9_-]+$", tool_names)
  if (any(!valid_names)) {
    cli::cli_abort(
      c(
        "CodeAct tool names may contain only letters, numbers, - and _",
        "x" = "Invalid name{?s}: {.val {unique(tool_names[!valid_names])}}"
      ),
      class = "dsprrr_codeact_tools_error"
    )
  }
  if (anyDuplicated(tool_names)) {
    duplicates <- unique(tool_names[duplicated(tool_names)])
    cli::cli_abort(
      c(
        "CodeAct tool names must be unique",
        "x" = "Duplicate name{?s}: {.val {duplicates}}"
      ),
      class = "dsprrr_codeact_tools_error"
    )
  }
  if ("execute_r_code" %in% tool_names) {
    cli::cli_abort(
      "{.val execute_r_code} is reserved for CodeAct's runner tool",
      class = "dsprrr_codeact_tools_error"
    )
  }
  normalized <- lapply(seq_along(tools), function(index) {
    tool <- tools[[index]]
    tool@name <- tool_names[[index]]
    tool
  })
  stats::setNames(normalized, tool_names)
}

codeact_tool_names <- function(tools) {
  if (length(tools) == 0L) {
    return(character())
  }
  vapply(
    tools,
    function(tool) {
      name <- tryCatch(tool@name, error = function(e) NA_character_)
      if (
        !is.character(name) ||
          length(name) != 1L ||
          is.na(name) ||
          !nzchar(name)
      ) {
        cli::cli_abort(
          "Every CodeAct ToolDef must have one non-empty name",
          class = "dsprrr_codeact_tools_error"
        )
      }
      name
    },
    character(1)
  )
}


#' CodeAct Module R6 Class
#'
#' @description
#' R6 class implementing the CodeAct pattern: an agent that can use both
#' tools and code execution to solve problems.
#'
#' @keywords internal
#' @noRd
CodeActModule <- R6::R6Class(
  "CodeActModule",
  inherit = Module,
  public = list(
    #' @field tools List of ellmer tools
    tools = NULL,

    #' @field runner Code runner for code execution
    runner = NULL,

    #' @field interpreter_factory Factory for an invocation-owned code runner
    interpreter_factory = NULL,

    #' @field max_iterations Maximum iterations
    max_iterations = NULL,

    #' @description
    #' Initialize a CodeActModule
    #'
    #' @param signature Signature object
    #' @param tools List of ellmer tools
    #' @param runner Code runner for code execution
    #' @param max_iterations Maximum iterations
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    #' @param interpreter_factory Optional zero-argument invocation-owned runner
    #'   factory. Supply exactly one of this and `runner`.
    initialize = function(
      signature,
      tools = list(),
      runner = NULL,
      max_iterations = 10L,
      config = list(),
      chat = NULL,
      interpreter_factory = NULL
    ) {
      binding <- normalize_code_runner_binding(
        runner = runner,
        interpreter_factory = interpreter_factory,
        module_name = "CodeAct"
      )
      tools <- validate_codeact_tools(tools)
      max_iterations <- normalize_codeact_iterations(max_iterations)
      super$initialize(
        signature = signature,
        config = config,
        chat = chat
      )

      self$tools <- tools
      self$runner <- binding$runner
      self$interpreter_factory <- binding$interpreter_factory
      self$max_iterations <- max_iterations

      # Store trajectory history
      self$state$trajectories <- list()
    },

    #' @description
    #' Execute the CodeAct agent workflow
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

      # Get LLM - need to clone for fresh conversation
      base_llm <- .llm %||% self$chat %||% get_default_chat()
      if (is.null(base_llm)) {
        cli::cli_abort("No LLM provided. Pass .llm or set a default chat.")
      }

      # Clone the chat for a fresh conversation
      llm <- base_llm$clone()

      with_code_runner_lease(
        self$runner,
        self$interpreter_factory,
        "CodeAct",
        function(runner, lease) {
          get_turns_safe <- function() {
            tryCatch(
              {
                if (!is.function(llm$get_turns)) {
                  return(list())
                }
                llm$get_turns()
              },
              error = function(e) list()
            )
          }
          start_turn_count <- length(get_turns_safe())

          start_time <- Sys.time()
          trajectory <- list()

          # Build all tools including code execution
          all_tools <- private$build_tools(inputs, runner)

          # Register tools with the chat
          for (tool in all_tools) {
            llm$register_tool(tool)
          }

          # Build the initial prompt
          task_prompt <- private$build_task_prompt(inputs)

          # ellmer can execute several tools inside one chat() call. Guard the
          # callback itself so max_iterations constrains that hidden inner loop.
          tool_calls <- 0L
          if (is.function(llm$on_tool_request)) {
            remove_tool_guard <- llm$on_tool_request(function(request) {
              tool_calls <<- tool_calls + 1L
              if (tool_calls > self$max_iterations) {
                cli::cli_abort(
                  "CodeAct exceeded max_iterations ({self$max_iterations})",
                  class = "dsprrr_codeact_iteration_limit"
                )
              }
              invisible(NULL)
            })
            if (is.function(remove_tool_guard)) {
              on.exit(remove_tool_guard(), add = TRUE)
            }
          }

          # Run the agent loop - ellmer handles tool calling automatically
          # We use chat() which processes tool calls internally
          iterations <- 0
          last_response <- NULL
          consecutive_failures <- 0
          max_consecutive_failures <- 3

          while (iterations < self$max_iterations) {
            iterations <- iterations + 1

            # Send message and let ellmer handle tool calls
            response <- tryCatch(
              {
                if (iterations == 1) {
                  llm$chat(task_prompt)
                } else {
                  # Continue the conversation if we're iterating
                  llm$chat(
                    "Continue working on the task. If you have enough information, provide your final answer."
                  )
                }
              },
              error = function(e) {
                if (
                  inherits(e, "dsprrr_codeact_iteration_limit") ||
                    is_terminal_interpreter_condition(e)
                ) {
                  stop(e)
                }
                consecutive_failures <<- consecutive_failures + 1
                cli::cli_warn(c(
                  "Agent iteration {iterations} failed ({consecutive_failures}/{max_consecutive_failures})",
                  "x" = "Error: {e$message}"
                ))

                if (consecutive_failures >= max_consecutive_failures) {
                  cli::cli_abort(c(
                    "Agent aborted after {max_consecutive_failures} consecutive LLM failures",
                    "x" = "Last error: {e$message}",
                    "i" = "Check your API key, network connection, and rate limits"
                  ))
                }
                NULL
              }
            )

            if (!is.null(response)) {
              consecutive_failures <- 0 # Reset on success
              last_response <- response

              # Check if the agent seems done (no pending tool calls)
              turns <- get_turns_safe()
              last_turn <- if (length(turns) > 0L) {
                turns[[length(turns)]]
              } else {
                list()
              }
              current_turns <- if (length(turns) > start_turn_count) {
                turns[seq.int(start_turn_count + 1L, length(turns))]
              } else {
                list()
              }
              observed_tool_calls <- sum(vapply(
                current_turns,
                private$count_tool_requests,
                integer(1)
              ))
              tool_calls <- max(tool_calls, observed_tool_calls)
              if (tool_calls > self$max_iterations) {
                cli::cli_abort(
                  "CodeAct exceeded max_iterations ({self$max_iterations})",
                  class = "dsprrr_codeact_iteration_limit"
                )
              }

              # Record in trajectory
              trajectory[[iterations]] <- list(
                iteration = iterations,
                response = response,
                has_tool_calls = private$has_pending_tools(last_turn)
              )

              # If no tool calls in the response, we're done
              if (!private$has_pending_tools(last_turn)) {
                break
              }
            }
          }

          # Check for complete failure
          if (is.null(last_response)) {
            cli::cli_abort(c(
              "Agent failed to produce any response in {iterations} iterations",
              "i" = "All LLM calls failed. Check API connectivity and error messages above."
            ))
          }

          # Extract final answer using structured output
          answer <- private$extract_final_answer(inputs, last_response, llm)

          # Build output matching signature
          output <- private$build_output(answer)

          duration_ms <- as.numeric(
            difftime(Sys.time(), start_time, units = "secs")
          ) *
            1000

          # Store trajectory
          if (trace) {
            self$state$trajectories <- c(
              self$state$trajectories,
              list(list(
                timestamp = start_time,
                inputs = inputs,
                trajectory = trajectory,
                iterations = iterations
              ))
            )
          }

          # Build metadata
          metadata <- list(
            model = "codeact",
            iterations = iterations,
            trajectory_length = length(trajectory),
            tool_calls = tool_calls,
            duration_ms = round(duration_ms, 2),
            tools_available = codeact_tool_names(all_tools),
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
    #' Get trajectory history
    #' @return List of trajectory records
    get_trajectories = function() {
      self$state$trajectories
    },

    #' @description
    #' Create a fresh copy of this module
    #' @return New CodeActModule with same settings
    reset_copy = function() {
      artifact_copy_runtime(
        self,
        CodeActModule$new(
          signature = self$signature,
          tools = self$tools,
          runner = self$runner,
          interpreter_factory = self$interpreter_factory,
          max_iterations = self$max_iterations,
          config = self$config,
          chat = self$chat
        )
      )
    },

    #' @description
    #' Create a deep copy while preserving the configured runtime source
    #' @return New CodeActModule with copied state
    deepcopy = function() {
      copied <- CodeActModule$new(
        signature = self$signature,
        tools = self$tools,
        runner = self$runner,
        max_iterations = self$max_iterations,
        config = lapply(self$config, identity),
        chat = self$chat,
        interpreter_factory = self$interpreter_factory
      )
      copied$state <- lapply(self$state, identity)
      artifact_copy_runtime(self, copied)
    },

    #' @description
    #' Print method for CodeActModule
    print = function() {
      # Format signature manually
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

      tool_names <- if (length(self$tools) > 0) {
        names(self$tools)
      } else {
        "(none)"
      }

      cli::cli_h3("CodeActModule")
      cli::cli_bullets(c(
        "*" = "Signature: {sig_str}",
        "*" = "Max iterations: {.val {self$max_iterations}}",
        "*" = "User tools: {.val {tool_names}}",
        "*" = "Runner: {code_runner_binding_label(self$runner, self$interpreter_factory)}"
      ))
      invisible(self)
    }
  ),

  private = list(
    #' Get output field names
    get_output_names = function() {
      output_type <- self$signature@output_type
      if (methods::.hasSlot(output_type, "properties")) {
        props <- output_type@properties
        if (length(props) > 0) {
          return(paste(names(props), collapse = ", "))
        }
      }
      "answer"
    },

    #' Build all tools including code execution
    build_tools = function(inputs, runner) {
      # Start with user-provided tools
      all_tools <- stats::setNames(
        self$tools,
        codeact_tool_names(self$tools)
      )

      # Add code execution tool
      context <- inputs

      execute_r_code <- ellmer::tool(
        fun = function(code) {
          result <- execute_code_runner(runner, code, context = context)
          if (result$success) {
            # Format successful result
            output <- paste0(
              "Execution successful.\n",
              "Result: ",
              deparse(result$result, width.cutoff = 500)[1],
              if (nchar(result$stdout) > 0) {
                paste0("\nStdout: ", result$stdout)
              } else {
                ""
              },
              if (nchar(result$messages) > 0) {
                paste0("\nMessages: ", result$messages)
              } else {
                ""
              }
            )
            output
          } else {
            # Format error
            paste0(
              "Execution failed.\n",
              "Error: ",
              result$error,
              if (nchar(result$stderr) > 0) {
                paste0("\nStderr: ", result$stderr)
              } else {
                ""
              }
            )
          }
        },
        description = paste0(
          "Execute R code in an isolated environment. ",
          "The input data is available in the `.context` list. ",
          "Use this for calculations, data manipulation, or any R computation. ",
          "Returns the execution result or error message."
        ),
        arguments = list(
          code = ellmer::type_string(
            description = "R code to execute. Must be valid R syntax."
          )
        ),
        name = "execute_r_code"
      )

      all_tools$execute_r_code <- execute_r_code
      all_tools
    },

    #' Build the task prompt
    build_task_prompt = function(inputs) {
      # Format inputs
      input_parts <- vapply(
        names(inputs),
        function(name) {
          val <- inputs[[name]]
          if (is.character(val) && length(val) == 1) {
            paste0(name, ": ", val)
          } else {
            paste0(name, ": ", deparse(val, width.cutoff = 500)[1])
          }
        },
        character(1)
      )

      input_text <- paste(input_parts, collapse = "\n")

      glue::glue(
        "
You are a helpful assistant that can use tools and execute R code to solve problems.

## Task
{input_text}

## Instructions
1. Think about what information or computation you need
2. Use available tools or execute R code as needed
3. The input data is available in `.context` when executing R code
4. When you have enough information, provide your final answer

Work step by step to solve this task.
"
      )
    },

    #' Check if turn has pending tool calls
    has_pending_tools = function(turn) {
      private$count_tool_requests(turn) > 0L
    },

    #' Count tool requests in one assistant turn
    count_tool_requests = function(turn) {
      if (is.null(turn)) {
        return(0L)
      }
      contents <- if (inherits(turn, "S7_object")) {
        tryCatch(turn@contents, error = function(e) list())
      } else if (is.list(turn)) {
        turn$contents %||% turn$content %||% list()
      } else {
        list()
      }
      if (!is.list(contents)) {
        contents <- list(contents)
      }
      as.integer(sum(vapply(
        contents,
        function(content) {
          inherits(content, "ellmer::ContentToolRequest") ||
            inherits(content, "ContentToolRequest") ||
            (is.list(content) &&
              identical(content$type %||% NULL, "tool_request"))
        },
        logical(1)
      )))
    },

    #' Extract final answer
    extract_final_answer = function(inputs, last_response, llm) {
      # If we have a simple string response, use it
      if (is.character(last_response) && length(last_response) == 1) {
        return(last_response)
      }

      # Otherwise, ask for structured output
      input_parts <- vapply(
        names(inputs),
        function(name) {
          paste0(name, ": ", inputs[[name]])
        },
        character(1)
      )

      prompt <- paste0(
        "Based on your work, provide the final answer to the task:\n",
        paste(input_parts, collapse = "\n"),
        "\n\nProvide a clear, concise answer."
      )

      llm$chat(prompt)
    },

    #' Build output matching signature
    build_output = function(answer) {
      output_type <- self$signature@output_type

      if (methods::.hasSlot(output_type, "properties")) {
        props <- output_type@properties
        if (length(props) == 1) {
          output <- list()
          output[[names(props)[1]]] <- answer
          return(output)
        }
      }

      list(answer = answer)
    }
  )
)
