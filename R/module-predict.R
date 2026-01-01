#' R6 PredictModule Class
#'
#' @description
#' Prediction module for template-based LLM prompting with demonstrations.
#' Extends the base Module class with specific implementation for predict-type modules.
#'
#' @keywords internal
#' @noRd
PredictModule <- R6::R6Class(
  "PredictModule",
  inherit = Module,
  public = list(
    #' @field template Glue template for prompt generation
    template = NULL,

    #' @field demos List of demonstration examples
    demos = NULL,

    #' @description
    #' Initialize a new PredictModule
    #' @param signature S7 Signature object
    #' @param template Optional glue template string
    #' @param demos Optional list of demonstrations
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
      signature,
      template = "",
      demos = list(),
      config = list(),
      chat = NULL
    ) {
      super$initialize(signature, config, chat)

      if (!is.character(template) || length(template) != 1) {
        cli::cli_abort("template must be a single character string")
      }

      if (!is.list(demos)) {
        cli::cli_abort("demos must be a list")
      }

      self$template <- template
      self$demos <- demos
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

      # Build prompt
      prompt <- private$build_prompt(inputs)

      # Get LLM client: prefer passed .llm, then stored chat, then auto-detect
      llm <- .llm %||% self$chat %||% private$get_default_llm()

      # Record start time
      start_time <- Sys.time()

      # Make LLM call (pass inputs for multimodal support)
      result <- tryCatch(
        {
          private$call_llm(
            llm = llm,
            prompt = prompt,
            output_type = self$signature@output_type,
            instructions = self$signature@instructions,
            inputs = inputs
          )
        },
        error = function(e) {
          cli::cli_abort("LLM call failed: {e$message}", parent = e)
        }
      )

      # Calculate metrics
      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # Get the last assistant turn - it has tokens, cost, duration built in
      assistant_turn <- tryCatch(
        llm$last_turn(role = "assistant"),
        error = function(e) NULL
      )

      # Get the last user turn for the prompt
      user_turn <- tryCatch(
        llm$last_turn(role = "user"),
        error = function(e) NULL
      )

      # Extract token info from ellmer's AssistantTurn (has @tokens vector)
      token_info <- if (
        !is.null(assistant_turn) && !is.null(assistant_turn@tokens)
      ) {
        tokens <- assistant_turn@tokens
        list(
          input_tokens = tokens[1],
          output_tokens = tokens[2],
          cached_input_tokens = tokens[3],
          total_tokens = sum(tokens[1:2], na.rm = TRUE)
        )
      } else {
        list(
          input_tokens = NA,
          output_tokens = NA,
          cached_input_tokens = NA,
          total_tokens = NA
        )
      }

      # Get cost and duration from AssistantTurn
      cost <- if (!is.null(assistant_turn)) assistant_turn@cost else NA_real_
      duration_s <- if (!is.null(assistant_turn)) {
        assistant_turn@duration
      } else {
        NA_real_
      }

      model <- tryCatch(llm$get_model(), error = function(e) NA_character_)

      # Create metadata - leverage ellmer's tracking
      metadata <- list(
        timestamp = end_time,
        model = model,
        prompt = prompt, # Keep for convenience
        instructions = self$signature@instructions,
        prompt_length = nchar(prompt),
        input_tokens = token_info$input_tokens,
        output_tokens = token_info$output_tokens,
        cached_input_tokens = token_info$cached_input_tokens,
        total_tokens = token_info$total_tokens,
        cost = cost,
        duration_s = duration_s,
        latency_ms = latency_ms # Our measured latency (includes R overhead)
      )

      # Record trace if requested - store ellmer turns directly
      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = inputs,
          output = result,
          user_turn = user_turn,
          assistant_turn = assistant_turn,
          model = model
        )

        # Optionally include full chat object for advanced replay
        if (isTRUE(self$config$store_chat_in_traces)) {
          trace_entry$chat <- llm
        }

        self$state$traces <- append(self$state$traces, list(trace_entry))

        # Also add to global prompt history for inspect_history()
        # Include the prompt text in the trace for history extraction
        trace_entry$prompt <- prompt
        add_to_global_history(trace_entry, source = "PredictModule")
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
      cli::cli_h2("PredictModule")

      cli::cli_h3("Signature")
      print(self$signature)

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
    #' @return New PredictModule with reset state
    reset_copy = function() {
      PredictModule$new(
        signature = self$signature,
        template = self$template,
        demos = list(),
        config = list(),
        chat = self$chat
      )
    },

    #' @description
    #' Create a deep copy of the module
    #' @return New PredictModule with copied state
    deepcopy = function() {
      # Use R6's clone mechanism to preserve subclass identity
      # This is critical for test mocks that inherit from PredictModule
      new_module <- self$clone(deep = FALSE)

      # Deep copy list fields manually
      new_module$demos <- if (length(self$demos) > 0) {
        lapply(self$demos, function(x) x)
      } else {
        list()
      }

      new_module$config <- if (length(self$config) > 0) {
        lapply(self$config, function(x) x)
      } else {
        list()
      }

      new_module$state <- lapply(self$state, function(x) x)

      # Create new signature (S7 objects need special handling)
      new_module$signature <- Signature(
        inputs = self$signature@inputs,
        output_type = self$signature@output_type,
        instructions = self$signature@instructions
      )

      new_module
    },

    #' @description
    #' Apply optimisation parameters (instructions/template) to the module
    apply_optimization_params = function(params) {
      if (!is.null(params$id)) {
        self$config$current_variant <- params$id
      }

      if (!is.null(params$instructions) && !is.na(params$instructions)) {
        self$signature@instructions <- params$instructions
      }

      if (!is.null(params$template) && !is.na(params$template)) {
        self$template <- params$template
      }

      if (!is.null(params$prompt_style) && !is.na(params$prompt_style)) {
        self$config$prompt_style <- params$prompt_style
      }

      invisible(self)
    }
  ),

  private = list(
    # Build prompt from inputs
    # Supports both glue-style { } and ellmer-style {{ }} delimiters
    build_prompt = function(inputs) {
      prompt_parts <- character()

      # Add demonstrations if present
      if (length(self$demos) > 0) {
        demo_text <- private$format_demos()
        prompt_parts <- c(prompt_parts, demo_text, "")
      }

      # Add the main template with inputs
      if (nchar(self$template) > 0) {
        filled_template <- private$interpolate_template(self$template, inputs)
        prompt_parts <- c(prompt_parts, filled_template)
      } else {
        # Auto-generate template from inputs
        input_text <- private$format_inputs(inputs)
        prompt_parts <- c(prompt_parts, input_text)
      }

      paste(prompt_parts, collapse = "\n")
    },

    # Interpolate template with inputs, supporting both { } and {{ }} syntax
    # Uses ellmer::interpolate() for {{ }} (ellmer-style), glue for { } (glue-style)
    interpolate_template = function(template, inputs) {
      # Check for ellmer-style {{ }} syntax
      if (grepl("\\{\\{[^}]+\\}\\}", template)) {
        # ellmer-style - use ellmer::interpolate with !!!inputs
        # ellmer::interpolate uses {{ }} delimiters
        rlang::inject(ellmer::interpolate(template, !!!inputs))
      } else {
        # glue-style { } - current behavior for backward compatibility
        glue::glue_data(
          .x = inputs,
          template,
          .open = "{",
          .close = "}",
          .envir = parent.frame()
        )
      }
    },

    # Format demonstrations for prompt
    format_demos = function() {
      demo_lines <- character()

      for (i in seq_along(self$demos)) {
        demo <- self$demos[[i]]
        demo_lines <- c(demo_lines, paste0("Example ", i, ":"))

        # Format inputs
        if (!is.null(demo$inputs)) {
          for (name in names(demo$inputs)) {
            demo_lines <- c(demo_lines, paste0(name, ": ", demo$inputs[[name]]))
          }
        }

        # Format output
        if (!is.null(demo$output)) {
          demo_lines <- c(
            demo_lines,
            paste0("Output: ", private$format_output(demo$output))
          )
        }

        demo_lines <- c(demo_lines, "")
      }

      paste(demo_lines, collapse = "\n")
    },

    # Format inputs when no template provided
    format_inputs = function(inputs) {
      if (length(self$signature@inputs) == 0) {
        return("")
      }

      input_lines <- character()
      for (input_spec in self$signature@inputs) {
        name <- input_spec$name
        if (name %in% names(inputs)) {
          value <- inputs[[name]]
          input_lines <- c(input_lines, paste0(name, ": ", value))
        }
      }

      if (length(input_lines) > 0) {
        paste(c("Input:", input_lines), collapse = "\n")
      } else {
        ""
      }
    },

    # Format output for display
    format_output = function(output) {
      if (is.list(output)) {
        if (length(output) == 1 && !is.null(names(output))) {
          # Single named field
          paste0(names(output)[1], ": ", output[[1]])
        } else {
          # Multiple fields
          paste(
            vapply(
              names(output),
              function(name) {
                paste0(name, ": ", output[[name]])
              },
              character(1)
            ),
            collapse = ", "
          )
        }
      } else {
        as.character(output)
      }
    },

    # Get default LLM client
    get_default_llm = function() {
      # Check for configured provider
      provider <- self$config$provider %||%
        Sys.getenv("DSPRRR_PROVIDER", "openai")
      provider <- switch(provider, anthropic = "claude", provider)

      # Get model name for reasoning model detection
      model_name <- self$config$model

      build_api_args <- function(model) {
        args <- self$config$api_args
        if (is.null(args)) {
          args <- list()
        }

        # Check if this is a reasoning model
        is_reasoning <- is_reasoning_model(model)

        append_arg <- function(name, value, allow_zero = TRUE) {
          if (is.null(value)) {
            return()
          }
          if (!allow_zero && identical(value, 0)) {
            return()
          }
          if (is.null(args[[name]])) {
            args[[name]] <<- value
          }
        }

        if (is_reasoning) {
          # Reasoning models use reasoning_effort instead of temperature/top_p
          # ellmer passes this via params(reasoning_effort = ...)
          append_arg("reasoning_effort", self$config$reasoning_effort)
        } else {
          # Traditional models use temperature and top_p
          append_arg("temperature", self$config$temperature, allow_zero = FALSE)
          append_arg("top_p", self$config$top_p)
        }

        # These parameters work for all models
        append_arg("frequency_penalty", self$config$frequency_penalty)
        append_arg("presence_penalty", self$config$presence_penalty)
        append_arg("max_output_tokens", self$config$max_output_tokens)

        args
      }

      llm <- switch(
        provider,
        openai = ellmer::chat_openai(
          model = model_name %||% "gpt-4o-mini",
          api_args = build_api_args(model_name %||% "gpt-4o-mini")
        ),
        claude = ellmer::chat_claude(
          model = model_name %||% "claude-sonnet-4-20250514",
          max_tokens = self$config$max_tokens %||% 4096,
          api_args = build_api_args(model_name %||% "claude-sonnet-4-20250514")
        ),
        gemini = ellmer::chat_google_gemini(
          model = model_name %||% "gemini-2.0-flash",
          api_args = build_api_args(model_name %||% "gemini-2.0-flash")
        ),
        ollama = ellmer::chat_ollama(
          model = model_name %||% "llama3.2:3b",
          api_args = build_api_args(model_name %||% "llama3.2:3b")
        ),
        cli::cli_abort("Unknown provider: {provider}")
      )

      llm
    },

    # Call LLM with structured output
    # Supports multimodal inputs (images, PDFs) via ellmer Content objects
    call_llm = function(
      llm,
      prompt,
      output_type,
      instructions = "",
      inputs = list()
    ) {
      # Check for Content objects in inputs (images, PDFs)
      content_inputs <- Filter(
        function(x) {
          inherits(x, "Content") ||
            inherits(x, "ContentImageRemote") ||
            inherits(x, "ContentImageInline") ||
            inherits(x, "ContentPDF")
        },
        inputs
      )

      if (length(content_inputs) > 0) {
        # Multimodal request - build content list
        # Instructions go in system turn, prompt and content in user turn
        full_prompt <- if (nchar(instructions) > 0) {
          paste(instructions, prompt, sep = "\n\n")
        } else {
          prompt
        }

        # Build content list: text prompt followed by multimodal content
        contents <- c(
          list(ellmer::ContentText(full_prompt)),
          unname(content_inputs)
        )

        # Make multimodal API call
        result <- llm$chat_structured(
          contents,

          type = output_type,
          echo = "none"
        )
      } else {
        # Text-only request (current path)
        full_prompt <- if (nchar(instructions) > 0) {
          paste(instructions, prompt, sep = "\n\n")
        } else {
          prompt
        }

        # Make the API call through ellmer's chat_structured method
        result <- llm$chat_structured(
          full_prompt,
          type = output_type,
          echo = "none"
        )
      }

      result
    }
  )
)
