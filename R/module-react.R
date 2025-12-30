#' R6 ReactModule Class
#'
#' @description
#' ReAct-style module that can use tools during execution.
#' Extends PredictModule with tool registration and multi-turn reasoning.
#'
#' The ReAct (Reasoning and Acting) pattern allows the LLM to:
#' 1. Reason about the problem
#' 2. Take actions via tool calls
#' 3. Observe results
#' 4. Continue until a final answer is reached
#'
#' @keywords internal
#' @noRd
ReactModule <- R6::R6Class(
  "ReactModule",
  inherit = PredictModule,

  public = list(
    #' @field tools List of ToolDef objects for the module
    tools = NULL,

    #' @field max_iterations Maximum number of ReAct iterations
    max_iterations = 10L,

    #' @description
    #' Initialize a new ReactModule
    #' @param signature S7 Signature object
    #' @param tools List of ellmer ToolDef objects
    #' @param max_iterations Maximum iterations for ReAct loop (default: 10)
    #' @param template Optional glue template string
    #' @param demos Optional list of demonstrations
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
      signature,
      tools = list(),
      max_iterations = 10L,
      template = "",
      demos = list(),
      config = list(),
      chat = NULL
    ) {
      super$initialize(signature, template, demos, config, chat)

      if (!is.list(tools)) {
        cli::cli_abort("tools must be a list of ToolDef objects")
      }

      # Validate each tool is a ToolDef
      for (i in seq_along(tools)) {
        if (
          !inherits(tools[[i]], "ellmer::ToolDef") &&
            !inherits(tools[[i]], "ToolDef")
        ) {
          cli::cli_abort(c(
            "All tools must be ellmer ToolDef objects",
            "x" = "tools[[{i}]] is a {.cls {class(tools[[i]])[1]}}"
          ))
        }
      }

      self$tools <- tools
      self$max_iterations <- as.integer(max_iterations)
    },

    #' @description
    #' Add a tool to the module
    #' @param tool An ellmer ToolDef object
    #' @return The module (invisibly), for chaining
    add_tool = function(tool) {
      if (!inherits(tool, "ellmer::ToolDef") && !inherits(tool, "ToolDef")) {
        cli::cli_abort("tool must be an ellmer ToolDef object")
      }

      self$tools <- c(self$tools, list(tool))

      # Also register on stored Chat if present
      if (!is.null(self$chat)) {
        self$chat$register_tool(tool)
      }

      invisible(self)
    },

    #' @description
    #' Remove a tool from the module
    #' @param name The name of the tool to remove
    #' @return The module (invisibly), for chaining
    remove_tool = function(name) {
      tool_names <- vapply(self$tools, function(t) t@name, character(1))
      idx <- match(name, tool_names)

      if (is.na(idx)) {
        cli::cli_warn("Tool {.val {name}} not found in module")
      } else {
        self$tools <- self$tools[-idx]
      }

      invisible(self)
    },

    #' @description
    #' List registered tools
    #' @return Character vector of tool names
    list_tools = function() {
      if (length(self$tools) == 0) {
        return(character(0))
      }
      vapply(self$tools, function(t) t@name, character(1))
    },

    #' @description
    #' Execute the module with given inputs
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param ... Additional arguments
    #' @return Tibble with result, .chat, .metadata columns
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      # Handle both list and data frame inputs
      if (is.data.frame(batch)) {
        inputs <- as.list(batch[1, , drop = FALSE])
      } else {
        inputs <- batch
      }

      # Get LLM client
      llm <- .llm %||% self$chat %||% private$get_default_llm()

      # Register all tools on the Chat
      for (tool in self$tools) {
        llm$register_tool(tool)
      }

      # Build initial prompt
      prompt <- private$build_prompt(inputs)

      # Add instructions if present
      if (nchar(self$signature@instructions) > 0) {
        prompt <- paste(self$signature@instructions, prompt, sep = "\n\n")
      }

      # Record start time
      start_time <- Sys.time()

      # Track iterations and tool calls
      iterations <- 0L
      tool_calls <- list()
      all_turns <- list()

      # ReAct loop
      repeat {
        iterations <- iterations + 1L

        if (iterations > self$max_iterations) {
          cli::cli_warn(c(
            "Maximum iterations ({self$max_iterations}) reached",
            "i" = "Increase max_iterations if more reasoning steps are needed"
          ))
          break
        }

        # Make LLM call (non-structured to allow tool use)
        tryCatch(
          {
            llm$chat(prompt, echo = "none")
          },
          error = function(e) {
            cli::cli_abort(
              "LLM call failed in ReAct loop: {e$message}",
              parent = e
            )
          }
        )

        # Get the last assistant turn
        last_turn <- llm$last_turn(role = "assistant")
        all_turns <- c(all_turns, list(last_turn))

        # Check if there's a tool request in the response
        has_tool_request <- any(vapply(
          last_turn@contents,
          function(c) {
            inherits(c, "ContentToolRequest")
          },
          logical(1)
        ))

        if (!has_tool_request) {
          # No more tool calls - we're done with reasoning
          break
        }

        # Extract tool calls for tracing
        for (content in last_turn@contents) {
          if (inherits(content, "ContentToolRequest")) {
            tool_calls <- c(
              tool_calls,
              list(list(
                iteration = iterations,
                tool_name = content@name,
                tool_id = content@id,
                arguments = content@arguments
              ))
            )
          }
        }

        # Continue the conversation (empty prompt continues with tool results)
        # ellmer automatically handles tool execution and result injection
        prompt <- ""
      }

      # Get final structured output
      result <- tryCatch(
        {
          llm$chat_structured(
            "Based on the conversation above, provide your final answer.",
            type = self$signature@output_type,
            echo = "none"
          )
        },
        error = function(e) {
          cli::cli_abort(
            "Failed to get structured output: {e$message}",
            parent = e
          )
        }
      )

      # Calculate metrics
      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # Get the final assistant turn
      final_turn <- tryCatch(
        llm$last_turn(role = "assistant"),
        error = function(e) NULL
      )

      # Get the initial user turn
      user_turn <- tryCatch(
        llm$last_turn(role = "user"),
        error = function(e) NULL
      )

      # Aggregate token info from all turns
      total_input <- 0
      total_output <- 0
      total_cached <- 0
      total_cost <- 0
      total_duration <- 0

      for (turn in all_turns) {
        if (!is.null(turn@tokens)) {
          total_input <- total_input + (turn@tokens[1] %||% 0)
          total_output <- total_output + (turn@tokens[2] %||% 0)
          total_cached <- total_cached + (turn@tokens[3] %||% 0)
        }
        total_cost <- total_cost + (turn@cost %||% 0)
        total_duration <- total_duration + (turn@duration %||% 0)
      }

      # Add final turn metrics
      if (!is.null(final_turn) && !is.null(final_turn@tokens)) {
        total_input <- total_input + (final_turn@tokens[1] %||% 0)
        total_output <- total_output + (final_turn@tokens[2] %||% 0)
        total_cached <- total_cached + (final_turn@tokens[3] %||% 0)
        total_cost <- total_cost + (final_turn@cost %||% 0)
        total_duration <- total_duration + (final_turn@duration %||% 0)
      }

      model <- tryCatch(llm$get_model(), error = function(e) NA_character_)

      # Create metadata
      metadata <- list(
        timestamp = end_time,
        model = model,
        prompt = prompt,
        instructions = self$signature@instructions,
        input_tokens = total_input,
        output_tokens = total_output,
        cached_input_tokens = total_cached,
        total_tokens = total_input + total_output,
        cost = total_cost,
        duration_s = total_duration,
        latency_ms = latency_ms,
        iterations = iterations,
        tool_calls = tool_calls,
        tools_used = unique(vapply(
          tool_calls,
          function(x) x$tool_name,
          character(1)
        ))
      )

      # Record trace if requested
      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = inputs,
          output = result,
          user_turn = user_turn,
          assistant_turn = final_turn,
          all_turns = all_turns,
          tool_calls = tool_calls,
          iterations = iterations,
          model = model
        )

        if (isTRUE(self$config$store_chat_in_traces)) {
          trace_entry$chat <- llm
        }

        self$state$traces <- append(self$state$traces, list(trace_entry))
      }

      # Return tibble format for consistency
      tibble::tibble(
        output = list(result),
        chat = list(llm),
        metadata = list(metadata)
      )
    },

    #' @description
    #' Print the module
    print = function() {
      cli::cli_h2("ReactModule")

      cli::cli_h3("Signature")
      print(self$signature)

      if (length(self$tools) > 0) {
        cli::cli_h3("Tools")
        tool_names <- self$list_tools()
        for (name in tool_names) {
          cli::cli_text("  {cli::symbol$bullet} {name}")
        }
        cli::cli_text("  Max iterations: {self$max_iterations}")
      }

      if (nchar(self$template) > 0) {
        cli::cli_h3("Template")
        cli::cli_code(self$template)
      }

      if (length(self$demos) > 0) {
        cli::cli_h3("Demos")
        cli::cli_text("{length(self$demos)} demonstration(s) loaded")
      }

      if (self$is_compiled()) {
        cli::cli_h3("Compilation Status")
        cli::cli_text("{cli::symbol$tick} Compiled")
        if (!is.null(self$config$teleprompter)) {
          cli::cli_text("  Teleprompter: {self$config$teleprompter}")
        }
        if (!is.null(self$state$best_score)) {
          cli::cli_text("  Best score: {round(self$state$best_score, 3)}")
        }
      }

      trace_summary <- self$trace_summary()
      if (trace_summary$n_traces > 0) {
        cli::cli_h3("Traces")
        cli::cli_text("  {trace_summary$n_traces} trace(s) recorded")
        cli::cli_text(
          "  Total tokens: {trace_summary$total_tokens} (in: {trace_summary$total_input_tokens}, out: {trace_summary$total_output_tokens})"
        )
        if (!is.na(trace_summary$total_cost) && trace_summary$total_cost > 0) {
          cli::cli_text(
            "  Total cost: ${format(trace_summary$total_cost, digits = 4)}"
          )
        }
        cli::cli_text(
          "  Total latency: {round(trace_summary$total_latency_ms / 1000, 2)}s"
        )
      }

      invisible(self)
    },

    #' @description
    #' Create a reset copy of the module
    #' @return New ReactModule with reset state
    reset_copy = function() {
      ReactModule$new(
        signature = self$signature,
        tools = self$tools,
        max_iterations = self$max_iterations,
        template = self$template,
        demos = list(),
        config = list(),
        chat = self$chat
      )
    },

    #' @description
    #' Create a deep copy of the module
    #' @return New ReactModule with copied state
    deepcopy = function() {
      new_demos <- if (length(self$demos) > 0) {
        lapply(self$demos, function(x) x)
      } else {
        list()
      }

      new_config <- if (length(self$config) > 0) {
        lapply(self$config, function(x) x)
      } else {
        list()
      }

      new_signature <- Signature(
        inputs = self$signature@inputs,
        output_type = self$signature@output_type,
        instructions = self$signature@instructions
      )

      new_module <- ReactModule$new(
        signature = new_signature,
        tools = self$tools, # Tools are immutable ToolDef objects
        max_iterations = self$max_iterations,
        template = self$template,
        demos = new_demos,
        config = new_config,
        chat = self$chat
      )

      new_module$state <- lapply(self$state, function(x) x)

      new_module
    }
  )
)
