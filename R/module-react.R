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

    #' @field max_iterations Maximum number of ReAct tool iterations
    max_iterations = 10L,

    #' @description
    #' Initialize a new ReactModule
    #' @param signature S7 Signature object
    #' @param tools List of ellmer ToolDef objects
    #' @param max_iterations Maximum tool-call iterations. dsprrr installs an
    #'   ellmer tool-request guard when callbacks are available and validates the
    #'   native turn history before finalization.
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

      max_iterations <- as.integer(max_iterations)
      if (
        length(max_iterations) != 1L ||
          is.na(max_iterations) ||
          max_iterations < 1L
      ) {
        cli::cli_abort("max_iterations must be a positive integer")
      }

      self$tools <- tools
      self$max_iterations <- max_iterations
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

      request <- build_module_request(self, inputs)
      llm <- resolve_module_llm(self, .llm = .llm)
      start_turn_count <- tryCatch(
        length(llm$get_turns()),
        error = function(e) 0L
      )

      # Register all tools on the Chat
      for (tool in self$tools) {
        llm$register_tool(tool)
      }

      prompt <- request$full_prompt

      start_time <- Sys.time()

      get_turns_safe <- function(chat) {
        tryCatch(
          {
            fn <- chat$get_turns
            if (!is.function(fn)) {
              return(list())
            }
            fn()
          },
          error = function(e) list()
        )
      }

      count_tool_iterations <- function(turns) {
        sum(vapply(
          turns,
          function(turn) {
            inherits(turn, "ellmer::AssistantTurn") &&
              any(vapply(
                turn@contents,
                inherits,
                logical(1),
                "ellmer::ContentToolRequest"
              ))
          },
          logical(1)
        ))
      }

      # ellmer's callback fires before executing a tool. The returned remover
      # keeps this per-run guard from leaking into subsequent conversations.
      if (is.function(llm$on_tool_request)) {
        remove_iteration_guard <- llm$on_tool_request(function(request) {
          iteration_count <- count_tool_iterations(get_turns_safe(llm))
          if (iteration_count > self$max_iterations) {
            cli::cli_abort(
              "ReAct exceeded max_iterations ({self$max_iterations})",
              class = "dsprrr_react_iteration_limit"
            )
          }
          invisible(NULL)
        })
        if (is.function(remove_iteration_guard)) {
          on.exit(remove_iteration_guard(), add = TRUE)
        }
      }

      n_turns_before <- length(get_turns_safe(llm))

      tryCatch(
        {
          llm$chat(prompt, echo = "none")
        },
        error = function(e) {
          if (inherits(e, "dsprrr_react_iteration_limit")) {
            stop(e)
          }
          cli::cli_abort(
            "LLM call failed in ReAct loop: {e$message}",
            parent = e
          )
        }
      )

      all_conversation_turns <- get_turns_safe(llm)
      n_turns_after <- length(all_conversation_turns)

      new_turns <- if (n_turns_after > n_turns_before) {
        all_conversation_turns[seq(n_turns_before + 1L, n_turns_after)]
      } else {
        list()
      }

      tool_calls <- list()
      iterations <- 0L
      all_turns <- list()

      for (turn in new_turns) {
        if (turn@role != "assistant") {
          next
        }
        all_turns <- c(all_turns, list(turn))

        turn_tool_calls <- list()
        for (content in turn@contents) {
          if (inherits(content, "ellmer::ContentToolRequest")) {
            turn_tool_calls <- c(
              turn_tool_calls,
              list(list(
                iteration = iterations + 1L,
                tool_name = content@name,
                tool_id = content@id,
                arguments = content@arguments
              ))
            )
          }
        }

        if (length(turn_tool_calls) > 0) {
          iterations <- iterations + 1L
          tool_calls <- c(tool_calls, turn_tool_calls)
        }
      }

      if (iterations == 0L) {
        iterations <- 1L
      }

      if (iterations > self$max_iterations) {
        cli::cli_abort(
          c(
            "ReAct exceeded max_iterations ({self$max_iterations})",
            "x" = "The model produced {iterations} tool-call iterations.",
            "i" = "Increase max_iterations if more reasoning steps are needed."
          ),
          class = "dsprrr_react_iteration_limit"
        )
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
      turns <- tryCatch(
        {
          current_turns <- llm$get_turns()
          new_start <- start_turn_count + 1L
          if (new_start <= length(current_turns)) {
            current_turns[seq.int(new_start, length(current_turns))]
          } else {
            list()
          }
        },
        error = function(e) c(list(user_turn), all_turns, list(final_turn))
      )

      # Aggregate token info from all turns
      total_input <- 0
      total_output <- 0
      total_cached <- 0
      turn_costs <- numeric()
      total_duration <- 0

      for (turn in all_turns) {
        if (!is.null(turn@tokens)) {
          total_input <- total_input + (turn@tokens[1] %||% 0)
          total_output <- total_output + (turn@tokens[2] %||% 0)
          total_cached <- total_cached + (turn@tokens[3] %||% 0)
        }
        turn_costs <- c(turn_costs, turn@cost %||% NA_real_)
        total_duration <- total_duration + (turn@duration %||% 0)
      }

      # Add final turn metrics
      if (!is.null(final_turn) && !is.null(final_turn@tokens)) {
        total_input <- total_input + (final_turn@tokens[1] %||% 0)
        total_output <- total_output + (final_turn@tokens[2] %||% 0)
        total_cached <- total_cached + (final_turn@tokens[3] %||% 0)
        turn_costs <- c(turn_costs, final_turn@cost %||% NA_real_)
        total_duration <- total_duration + (final_turn@duration %||% 0)
      }
      total_cost <- sum_cost_values(turn_costs)

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
        history = turns,
        finalization = "structured-followup",
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
          prompt = request$full_prompt,
          instructions = self$signature@instructions,
          user_turn = user_turn,
          assistant_turn = final_turn,
          turns = turns,
          all_turns = all_turns,
          tool_calls = tool_calls,
          iterations = iterations,
          latency_ms = latency_ms,
          tokens = list(
            input_tokens = total_input,
            output_tokens = total_output,
            cached_input_tokens = total_cached,
            total_tokens = total_input + total_output
          ),
          cost = total_cost,
          model = model
        )

        if (isTRUE(self$config$store_chat_in_traces)) {
          trace_entry$chat <- llm
        }

        self$state$traces <- append(self$state$traces, list(trace_entry))

        # Also add to global prompt history for inspect_history()
        add_to_global_history(trace_entry, source = "ReactModule")
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
