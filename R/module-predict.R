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
    initialize = function(signature, template = "", demos = list(), config = list()) {
      super$initialize(signature, config)

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

      # Get LLM client
      llm <- .llm %||% private$get_default_llm()

      # Record start time
      start_time <- Sys.time()

      # Make LLM call
      result <- tryCatch({
        private$call_llm(
          llm = llm,
          prompt = prompt,
          output_type = self$signature@output_type,
          instructions = self$signature@instructions
        )
      }, error = function(e) {
        cli::cli_abort("LLM call failed: {e$message}", parent = e)
      })

      # Calculate metrics
      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) * 1000

      # Extract token usage from ellmer
      token_info <- tryCatch({
        tokens_df <- llm$get_tokens()
        list(
          input_tokens = sum(tokens_df$tokens[tokens_df$role == "user"], na.rm = TRUE),
          output_tokens = sum(tokens_df$tokens[tokens_df$role == "assistant"], na.rm = TRUE),
          total_tokens = sum(tokens_df$tokens, na.rm = TRUE)
        )
      }, error = function(e) {
        list(input_tokens = NA, output_tokens = NA, total_tokens = NA)
      })

      # Get cost if available
      cost <- tryCatch({
        llm$get_cost()
      }, error = function(e) {
        NA_real_
      })

      # Create metadata
      metadata <- list(
        latency_ms = latency_ms,
        prompt_length = nchar(prompt),
        prompt = prompt,
        instructions = self$signature@instructions,
        timestamp = end_time,
        input_tokens = token_info$input_tokens,
        output_tokens = token_info$output_tokens,
        total_tokens = token_info$total_tokens,
        cost = cost,
        model = tryCatch(llm$get_model(), error = function(e) NA_character_)
      )

      # Record trace if requested
      if (trace) {
        # Get the full turn history from ellmer
        turns <- tryCatch({
          llm$get_turns(include_system_prompt = FALSE)
        }, error = function(e) {
          list()
        })

        self$state$traces <- append(self$state$traces, list(list(
          timestamp = end_time,
          inputs = inputs,
          prompt = prompt,
          output = result,
          latency_ms = latency_ms,
          tokens = token_info,
          cost = cost,
          model = metadata$model,
          turns = turns,  # Full ellmer turn history
          chat = llm  # Store the chat object itself for full access
        )))
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
        config = list()
      )
    },

    #' @description
    #' Create a deep copy of the module
    #' @return New PredictModule with copied state
    deepcopy = function() {
      # Copy demos
      new_demos <- if (length(self$demos) > 0) {
        lapply(self$demos, function(x) x)
      } else {
        list()
      }

      # Copy config
      new_config <- if (length(self$config) > 0) {
        lapply(self$config, function(x) x)
      } else {
        list()
      }

      # Create new signature (S7 objects need special handling)
      new_signature <- Signature(
        inputs = self$signature@inputs,
        output_type = self$signature@output_type,
        instructions = self$signature@instructions
      )

      new_module <- PredictModule$new(
        signature = new_signature,
        template = self$template,
        demos = new_demos,
        config = new_config
      )

      # Copy state
      new_module$state <- lapply(self$state, function(x) x)

      new_module
    }
  ),

  private = list(
    # Build prompt from inputs
    build_prompt = function(inputs) {
      prompt_parts <- character()

      # Add demonstrations if present
      if (length(self$demos) > 0) {
        demo_text <- private$format_demos()
        prompt_parts <- c(prompt_parts, demo_text, "")
      }

      # Add the main template with inputs
      if (nchar(self$template) > 0) {
        filled_template <- glue::glue_data(
          .x = inputs,
          self$template,
          .open = "{",
          .close = "}",
          .envir = parent.frame()
        )
        prompt_parts <- c(prompt_parts, filled_template)
      } else {
        # Auto-generate template from inputs
        input_text <- private$format_inputs(inputs)
        prompt_parts <- c(prompt_parts, input_text)
      }

      paste(prompt_parts, collapse = "\n")
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
            vapply(names(output), function(name) {
              paste0(name, ": ", output[[name]])
            }, character(1)),
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
      provider <- self$config$provider %||% Sys.getenv("DSPRRR_PROVIDER", "openai")

      # Create appropriate client
      llm <- switch(provider,
        openai = ellmer::chat_openai(
          model = self$config$model %||% "gpt-4o-mini",
          temperature = self$config$temperature %||% 0.7
        ),
        anthropic = ellmer::chat_anthropic(
          model = self$config$model %||% "claude-3-haiku-20240307",
          temperature = self$config$temperature %||% 0.7
        ),
        ollama = ellmer::chat_ollama(
          model = self$config$model %||% "llama3.2:3b",
          temperature = self$config$temperature %||% 0.7
        ),
        cli::cli_abort("Unknown provider: {provider}")
      )

      llm
    },

    # Call LLM with structured output
    call_llm = function(llm, prompt, output_type, instructions = "") {
      # Build the full prompt with instructions
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

      result
    }
  )
)