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
#' Security: Code execution requires explicit opt-in via a runner parameter.
#' The runner provides subprocess isolation but is NOT a security sandbox.
#' For untrusted inputs, use OS-level sandboxing.
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
#'
#' @param signature A Signature object or string notation defining inputs/outputs
#' @param tools List of ellmer ToolDef objects for the agent to use
#' @param runner An RCodeRunner object for code execution. Required.
#' @param max_iterations Maximum tool/code iterations before forcing answer
#'   (default 10)
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
  runner,
  max_iterations = 10L,
  ...
) {
  # Validate runner
  if (missing(runner) || is.null(runner)) {
    cli::cli_abort(c(
      "CodeAct requires an explicit runner for code execution",
      "i" = "Create one with: {.code runner <- r_code_runner()}",
      "i" = "Then pass it: {.code code_act(..., runner = runner)}"
    ))
  }

  if (!inherits(runner, "RCodeRunner")) {
    cli::cli_abort(c(
      "runner must be an RCodeRunner object",
      "x" = "You provided: {.cls {class(runner)[1]}}",
      "i" = "Create one with: {.code r_code_runner()}"
    ))
  }

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

  CodeActModule$new(
    signature = signature,
    tools = tools,
    runner = runner,
    max_iterations = as.integer(max_iterations),
    ...
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

    #' @field runner RCodeRunner for code execution
    runner = NULL,

    #' @field max_iterations Maximum iterations
    max_iterations = NULL,

    #' @description
    #' Initialize a CodeActModule
    #'
    #' @param signature Signature object
    #' @param tools List of ellmer tools
    #' @param runner RCodeRunner for code execution
    #' @param max_iterations Maximum iterations
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
      signature,
      tools = list(),
      runner,
      max_iterations = 10L,
      config = list(),
      chat = NULL
    ) {
      super$initialize(
        signature = signature,
        config = config,
        chat = chat
      )

      self$tools <- tools
      self$runner <- runner
      self$max_iterations <- as.integer(max_iterations)

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

      start_time <- Sys.time()
      trajectory <- list()

      # Build all tools including code execution
      all_tools <- private$build_tools(inputs)

      # Register tools with the chat
      for (tool in all_tools) {
        llm$register_tool(tool)
      }

      # Build the initial prompt
      task_prompt <- private$build_task_prompt(inputs)

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
          turns <- llm$get_turns()
          last_turn <- turns[[length(turns)]]

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
        duration_ms = round(duration_ms, 2),
        tools_available = names(all_tools)
      )

      tibble::tibble(
        output = list(output),
        chat = list(llm),
        metadata = list(metadata)
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
      CodeActModule$new(
        signature = self$signature,
        tools = self$tools,
        runner = self$runner,
        max_iterations = self$max_iterations,
        config = self$config,
        chat = self$chat
      )
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
        "*" = "Code execution: enabled (timeout: {self$runner$timeout}s)"
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
    build_tools = function(inputs) {
      # Start with user-provided tools
      all_tools <- self$tools

      # Add code execution tool
      runner <- self$runner
      context <- inputs

      execute_r_code <- ellmer::tool(
        fun = function(code) {
          result <- runner$execute(code, context = context)
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
        )
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
      if (is.null(turn)) {
        return(FALSE)
      }

      # Check if the turn contains tool requests
      # This is a simplified check - ellmer handles the actual tool execution
      if (inherits(turn, "list") && "role" %in% names(turn)) {
        # If the assistant made tool calls, ellmer will have processed them
        # We're "done" when the assistant gives a text response without tools
        return(FALSE)
      }

      FALSE
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
