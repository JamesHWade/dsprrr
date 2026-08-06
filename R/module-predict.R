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
    #' @param .cache Logical or NULL. Per-call cache control. If NULL (default),
    #'   uses global cache configuration. If TRUE, attempts to use cache (no effect
    #'   if caching globally disabled). If FALSE, bypasses cache for this call.
    #' @param rollout_id Optional value for cache partitioning. When set, the
    #'   structured-output cache key includes it, so retries/attempts (e.g. from
    #'   BestOfN, Refine, Assert) get distinct keys and explore fresh responses.
    #' @param ... Additional arguments
    #' @return Tibble with result, .chat, .metadata columns
    forward = function(
      batch,
      .llm = NULL,
      trace = TRUE,
      .cache = NULL,
      rollout_id = NULL,
      ...
    ) {
      # Handle both list and data frame inputs
      if (is.data.frame(batch)) {
        inputs <- as.list(batch[1, , drop = FALSE])
      } else {
        inputs <- batch
      }

      request <- build_module_request(self, inputs)
      prompt <- request$prompt
      llm <- resolve_module_llm(self, .llm = .llm)
      start_turn_count <- tryCatch(
        length(llm$get_turns()),
        error = function(e) 0L
      )

      # Record start time
      start_time <- Sys.time()

      # Make LLM call (pass inputs for multimodal support)
      result <- tryCatch(
        {
          private$call_llm(
            llm = llm,
            request = request,
            output_type = self$signature@output_type,
            .cache = .cache,
            rollout_id = rollout_id
          )
        },
        error = function(e) {
          cli::cli_abort(
            "LLM call failed: {e$message}",
            class = "dsprrr_provider_error",
            parent = e
          )
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
        error = function(e) list(user_turn, assistant_turn)
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
          prompt = request$full_prompt,
          instructions = self$signature@instructions,
          user_turn = user_turn,
          assistant_turn = assistant_turn,
          turns = turns,
          latency_ms = latency_ms,
          tokens = token_info,
          cost = cost,
          model = model
        )

        # Optionally include full chat object for advanced replay
        if (isTRUE(self$config$store_chat_in_traces)) {
          trace_entry$chat <- llm
        }

        self$state$traces <- append(self$state$traces, list(trace_entry))
        # Also add to global prompt history for inspect_history()
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

      # Cache status
      cache_config <- get_cache_config()
      if (cache_config$enable) {
        stats <- cache_stats()
        if (stats$hits > 0 || stats$misses > 0) {
          cli::cli_h3("Cache")
          hit_pct <- format(stats$hit_rate * 100, digits = 1)
          cli::cli_text(
            "  Hit rate: {hit_pct}% ({stats$hits} hits, {stats$misses} misses)"
          )
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
        config = new_config,
        chat = self$chat
      )

      # Copy state
      new_module$state <- lapply(self$state, function(x) x)

      new_module
    },

    #' @description
    #' Apply optimisation parameters (instructions/template) to the module
    apply_optimization_params = function(params) {
      super$apply_optimization_params(params)

      if (!is.null(params$id)) {
        self$config$current_variant <- params$id
      }

      if (!is.null(params$instructions) && !is.na(params$instructions)) {
        self$signature <- with_instructions(
          self$signature,
          params$instructions
        )
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
      build_prompt(self, inputs)
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
      if (!is.list(output)) {
        return(as.character(output))
      }

      # List output: format as name: value pairs
      if (is.null(names(output)) || length(output) == 0) {
        return(as.character(output))
      }

      # Format each field
      formatted_pairs <- vapply(
        names(output),
        function(name) paste0(name, ": ", output[[name]]),
        character(1)
      )

      paste(formatted_pairs, collapse = ", ")
    },

    # Get default LLM client
    get_default_llm = function() {
      resolve_module_llm(self, create = TRUE)
    },

    # Call LLM with structured output
    # Supports multimodal inputs (images, PDFs) via ellmer Content objects
    call_llm = function(
      llm,
      request,
      output_type,
      .cache = NULL,
      rollout_id = NULL
    ) {
      call_llm_request(
        llm = llm,
        request = request,
        output_type = output_type,
        .cache = .cache,
        rollout_id = rollout_id
      )
    }
  ),

  active = list(
    #' @field demo_table Active binding that returns demos as a tibble
    demo_table = function() {
      module_demos_as_tibble(self)
    }
  )
)

#' Convert module demos to a tibble
#'
#' @description
#' Converts the demos list from a compiled module into a tidy tibble format.
#' Handles both flat outputs (single values) and nested outputs (named lists).
#'
#' @param module A Module object (typically a PredictModule with demos)
#' @return A tibble with input columns and output column(s). For nested outputs,
#'   output fields are flattened into separate columns.
#'
#' @examples
#' \dontrun{
#' # After compiling a module with LabeledFewShot
#' compiled <- compile(LabeledFewShot(k = 3), module, trainset)
#'
#' # View demos as tibble
#' module_demos_as_tibble(compiled)
#'
#' # Or use the active binding
#' compiled$demo_table
#' }
#'
#' @export
module_demos_as_tibble <- function(module) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a Module object")
  }

  demos <- module$demos
  if (is.null(demos) || length(demos) == 0) {
    return(tibble::tibble())
  }

  # Convert each demo to a flat row
  rows <- lapply(demos, function(demo) {
    row <- list()

    # Add inputs
    if (!is.null(demo$inputs) && is.list(demo$inputs)) {
      for (name in names(demo$inputs)) {
        row[[name]] <- demo$inputs[[name]]
      }
    }

    # Add output(s)
    output <- demo$output
    if (is.null(output)) {
      row[["output"]] <- NA
    } else if (is.list(output) && !is.null(names(output))) {
      # Nested output - flatten into separate columns
      for (name in names(output)) {
        row[[name]] <- output[[name]]
      }
    } else {
      # Simple output
      row[["output"]] <- output
    }

    row
  })

  # Combine into tibble
  # Use do.call(rbind, ...) to avoid dplyr dependency
  tbl_rows <- lapply(rows, tibble::as_tibble_row)
  if (length(tbl_rows) == 0) {
    return(tibble::tibble())
  }
  do.call(rbind, tbl_rows)
}
