#' MultiChainComparison Module
#'
#' @description
#' Implements the MultiChainComparison (MCC) pattern where multiple reasoning
#' chains are generated and then synthesized to produce the best answer.
#'
#' @name module-multichain
NULL

#' MultiChainComparison Module Class
#'
#' @description
#' An R6 class that runs an inner module M times to generate multiple
#' reasoning attempts, then uses a comparison step to synthesize the
#' best final answer.
#'
#' @details
#' MultiChainComparison provides ensemble-style reasoning by:
#' 1. Running the inner module M times (with temperature for diversity)
#' 2. Collecting all reasoning attempts and answers
#' 3. Using a comparison prompt to synthesize the best answer
#'
#' This is particularly useful for:
#' - Complex reasoning tasks where different approaches may be valid
#' - Reducing variance by synthesizing multiple attempts
#' - Improving reliability through ensemble consensus
#'
#' The execution flow is:
#' 1. For each of M attempts:
#'    - Run the inner module with configured temperature
#'    - Collect reasoning and answer
#' 2. Build comparison prompt with all attempts
#' 3. Run comparison step to synthesize best answer
#' 4. Return synthesized result
#'
#' @keywords internal
#' @noRd
MultiChainComparisonModule <- R6::R6Class(
  "MultiChainComparisonModule",
  inherit = Module,
  public = list(
    #' @field inner_module The module to run M times
    inner_module = NULL,

    #' @field M Number of reasoning chains to generate
    M = NULL,

    #' @field temperature Temperature for attempt diversity
    temperature = NULL,

    #' @field comparison_template Template for the comparison prompt
    comparison_template = NULL,

    #' @description
    #' Initialize a MultiChainComparison module
    #'
    #' @param signature Signature for the task (will be passed to inner module)
    #' @param inner_module Optional pre-created inner module. If NULL, creates
    #'   a PredictModule from the signature.
    #' @param M Number of reasoning chains (default 3)
    #' @param temperature Temperature for generating diverse attempts (default 0.7)
    #' @param comparison_template Template for comparison prompt
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
      signature,
      inner_module = NULL,
      M = 3L,
      temperature = 0.7,
      comparison_template = NULL,
      config = list(),
      chat = NULL
    ) {
      # Coerce signature if string
      sig <- if (is.character(signature)) {
        signature(signature)
      } else if (S7::S7_inherits(signature, Signature)) {
        signature
      } else {
        cli::cli_abort(
          "signature must be a Signature object or string notation"
        )
      }

      super$initialize(signature = sig, config = config, chat = chat)

      # Create or use inner module
      if (!is.null(inner_module)) {
        if (!inherits(inner_module, "Module")) {
          cli::cli_abort("inner_module must be a Module object")
        }
        self$inner_module <- inner_module
      } else {
        # Default: use ChainOfThought for better reasoning
        cot_sig <- with_reasoning(sig)
        self$inner_module <- module(cot_sig, type = "predict", chat = chat)
      }

      self$M <- as.integer(M)
      if (self$M < 1L) {
        cli::cli_abort(c(
          "M must be at least 1",
          "x" = "Got M = {.val {M}}"
        ))
      }
      self$temperature <- temperature
      self$comparison_template <- comparison_template %||%
        private$default_comparison_template()

      # Store attempt details
      self$state$attempts <- list()
    },

    #' @description
    #' Execute the module with M reasoning chains
    #'
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param ... Additional arguments
    #' @return Tibble with output, chat, metadata columns
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      # Handle both list and data frame inputs
      if (is.data.frame(batch)) {
        inputs <- as.list(batch[1, , drop = FALSE])
      } else {
        inputs <- batch
      }

      start_time <- Sys.time()
      attempts <- list()
      total_tokens <- 0
      total_cost <- 0

      # Phase 1: Generate M reasoning chains
      for (i in seq_len(self$M)) {
        # Run inner module
        result <- tryCatch(
          {
            self$inner_module$forward(batch, .llm = .llm, trace = FALSE, ...)
          },
          error = function(e) {
            cli::cli_warn(c(
              "Attempt {i} failed in MultiChainComparison",
              "i" = e$message
            ))
            NULL
          }
        )

        if (!is.null(result)) {
          prediction <- result$output[[1]]
          metadata <- result$metadata[[1]]

          # Accumulate costs
          if (!is.null(metadata$total_tokens)) {
            total_tokens <- total_tokens + metadata$total_tokens
          }
          if (!is.null(metadata$cost) && !is.na(metadata$cost)) {
            total_cost <- total_cost + metadata$cost
          }

          attempts <- append(
            attempts,
            list(list(
              attempt = i,
              prediction = prediction,
              metadata = metadata
            ))
          )
        }
      }

      # Check we have at least one attempt
      if (length(attempts) == 0) {
        cli::cli_abort("All {self$M} attempts failed in MultiChainComparison")
      }

      # Phase 2: Synthesize best answer
      comparison_result <- private$run_comparison(
        inputs = inputs,
        attempts = attempts,
        .llm = .llm
      )

      # Accumulate comparison step costs
      comparison_metadata <- comparison_result$metadata[[1]]
      if (!is.null(comparison_metadata$total_tokens)) {
        total_tokens <- total_tokens + comparison_metadata$total_tokens
      }
      if (
        !is.null(comparison_metadata$cost) && !is.na(comparison_metadata$cost)
      ) {
        total_cost <- total_cost + comparison_metadata$cost
      }

      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # Build final result
      final_output <- comparison_result$output[[1]]

      # Create aggregated metadata
      final_metadata <- list(
        timestamp = end_time,
        model = comparison_metadata$model,
        M = self$M,
        n_successful_attempts = length(attempts),
        n_failed_attempts = self$M - length(attempts),
        n_llm_calls = length(attempts) + 1, # M attempts + 1 comparison
        total_tokens = total_tokens,
        total_cost = total_cost,
        latency_ms = latency_ms
      )

      # Record trace
      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = inputs,
          output = final_output,
          attempts = attempts,
          comparison_output = final_output,
          M = self$M,
          n_successful_attempts = length(attempts),
          n_failed_attempts = self$M - length(attempts),
          model = comparison_metadata$model
        )
        self$state$traces <- append(self$state$traces, list(trace_entry))
        self$state$attempts <- append(self$state$attempts, list(attempts))
      }

      tibble::tibble(
        output = list(final_output),
        chat = comparison_result$chat,
        metadata = list(final_metadata)
      )
    },

    #' @description
    #' Get attempts from last run or all runs
    #' @param all Logical. If TRUE, return all attempts across all runs
    #' @return A tibble of attempts
    get_attempts = function(all = FALSE) {
      if (length(self$state$attempts) == 0) {
        return(tibble::tibble(
          run = integer(),
          attempt = integer(),
          prediction = list()
        ))
      }

      if (all) {
        rows <- list()
        for (run_idx in seq_along(self$state$attempts)) {
          run_attempts <- self$state$attempts[[run_idx]]
          for (a in run_attempts) {
            rows <- append(
              rows,
              list(list(
                run = run_idx,
                attempt = a$attempt,
                prediction = list(a$prediction)
              ))
            )
          }
        }
      } else {
        last_run <- self$state$attempts[[length(self$state$attempts)]]
        rows <- lapply(last_run, function(a) {
          list(
            run = length(self$state$attempts),
            attempt = a$attempt,
            prediction = list(a$prediction)
          )
        })
      }

      if (length(rows) == 0) {
        return(tibble::tibble(
          run = integer(),
          attempt = integer(),
          prediction = list()
        ))
      }

      tibble::tibble(
        run = vapply(rows, function(r) r$run, integer(1)),
        attempt = vapply(rows, function(r) r$attempt, integer(1)),
        prediction = lapply(rows, function(r) r$prediction[[1]])
      )
    },

    #' @description
    #' Print the module
    print = function() {
      cli::cli_h2("MultiChainComparisonModule")
      cli::cli_text("M: {self$M} reasoning chains")
      cli::cli_text("Temperature: {self$temperature}")
      cli::cli_text("Inner module: {.cls {class(self$inner_module)[1]}}")

      if (length(self$state$traces) > 0) {
        last_trace <- self$state$traces[[length(self$state$traces)]]
        cli::cli_h3("Last Run")
        cli::cli_text(
          "  Successful attempts: {last_trace$n_successful_attempts}"
        )
      }

      invisible(self)
    },

    #' @description
    #' Create a reset copy of the module
    reset_copy = function() {
      MultiChainComparisonModule$new(
        signature = self$signature,
        inner_module = self$inner_module$reset_copy(),
        M = self$M,
        temperature = self$temperature,
        comparison_template = self$comparison_template,
        config = list(),
        chat = self$chat
      )
    },

    #' @description
    #' Apply optimization parameters
    apply_optimization_params = function(params) {
      if (!is.null(params$M)) {
        self$M <- as.integer(params$M)
      }
      if (!is.null(params$temperature)) {
        self$temperature <- params$temperature
      }
      # Forward other params to inner module
      inner_params <- params[!names(params) %in% c("M", "temperature")]
      if (length(inner_params) > 0) {
        self$inner_module$apply_optimization_params(inner_params)
      }
      invisible(self)
    }
  ),

  private = list(
    #' Generate default comparison template
    default_comparison_template = function() {
      paste0(
        "You will evaluate {M} reasoning attempts for the same problem.\n\n",
        "{attempts_text}\n\n",
        "Based on all the reasoning attempts above, provide:\n",
        "1. Your refined reasoning that synthesizes the best insights\n",
        "2. The best final answer\n\n",
        "Consider which attempts have the most sound logic and arrive at ",
        "consistent conclusions."
      )
    },

    #' Format attempts for comparison prompt
    format_attempts = function(attempts) {
      attempt_texts <- vapply(
        seq_along(attempts),
        function(i) {
          a <- attempts[[i]]
          pred <- a$prediction

          # Format prediction
          if (is.list(pred)) {
            pred_lines <- vapply(
              names(pred),
              function(n) paste0("  ", n, ": ", pred[[n]]),
              character(1)
            )
            pred_text <- paste(pred_lines, collapse = "\n")
          } else {
            pred_text <- paste0("  ", as.character(pred))
          }

          paste0("=== Attempt ", i, " ===\n", pred_text)
        },
        character(1)
      )

      paste(attempt_texts, collapse = "\n\n")
    },

    #' Build comparison signature
    build_comparison_signature = function() {
      # Build a signature for the comparison step
      # Output should match the original signature's output

      # Add reasoning field if not already present
      if (has_reasoning(self$signature)) {
        # Already has reasoning, use as-is
        comparison_sig <- self$signature
      } else {
        # Add reasoning for synthesis
        comparison_sig <- with_reasoning(self$signature)
      }

      comparison_sig
    },

    #' Run the comparison step
    run_comparison = function(inputs, attempts, .llm = NULL) {
      # Format attempts for prompt
      attempts_text <- private$format_attempts(attempts)

      # Build comparison prompt using template
      template_data <- c(
        inputs,
        list(
          M = length(attempts),
          attempts_text = attempts_text
        )
      )

      comparison_prompt <- tryCatch(
        {
          glue::glue_data(
            template_data,
            self$comparison_template,
            .open = "{",
            .close = "}"
          )
        },
        error = function(e) {
          cli::cli_warn("Failed to build comparison prompt: {e$message}")
          paste0(
            "Evaluate these ",
            length(attempts),
            " attempts and provide the best answer:\n\n",
            attempts_text
          )
        }
      )

      # Get LLM
      llm <- .llm %||% self$chat %||% private$get_default_llm()

      # Build comparison signature
      comparison_sig <- private$build_comparison_signature()

      # Make comparison call
      start_time <- Sys.time()

      result <- llm$chat_structured(
        paste(comparison_sig@instructions, comparison_prompt, sep = "\n\n"),
        type = comparison_sig@output_type,
        echo = "none"
      )

      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # Get token info
      assistant_turn <- tryCatch(
        llm$last_turn(role = "assistant"),
        error = function(e) NULL
      )

      token_info <- if (
        !is.null(assistant_turn) && !is.null(assistant_turn@tokens)
      ) {
        tokens <- assistant_turn@tokens
        list(
          input_tokens = tokens[1],
          output_tokens = tokens[2],
          total_tokens = sum(tokens[1:2], na.rm = TRUE)
        )
      } else {
        list(
          input_tokens = NA,
          output_tokens = NA,
          total_tokens = NA
        )
      }

      cost <- if (!is.null(assistant_turn)) assistant_turn@cost else NA_real_
      model <- tryCatch(llm$get_model(), error = function(e) NA_character_)

      metadata <- list(
        timestamp = end_time,
        model = model,
        prompt = as.character(comparison_prompt),
        total_tokens = token_info$total_tokens,
        cost = cost,
        latency_ms = latency_ms
      )

      tibble::tibble(
        output = list(result),
        chat = list(llm),
        metadata = list(metadata)
      )
    },

    #' Get default LLM client
    get_default_llm = function() {
      # Check for configured provider
      provider <- self$config$provider %||%
        Sys.getenv("DSPRRR_PROVIDER", "openai")
      provider <- switch(provider, anthropic = "claude", provider)

      model_name <- self$config$model

      llm <- switch(
        provider,
        openai = ellmer::chat_openai(
          model = model_name %||% "gpt-4o-mini",
          api_args = list(temperature = self$temperature)
        ),
        claude = ellmer::chat_claude(
          model = model_name %||% "claude-sonnet-4-20250514",
          max_tokens = self$config$max_tokens %||% 4096,
          api_args = list(temperature = self$temperature)
        ),
        gemini = ellmer::chat_google_gemini(
          model = model_name %||% "gemini-2.0-flash",
          api_args = list(temperature = self$temperature)
        ),
        ollama = ellmer::chat_ollama(
          model = model_name %||% "llama3.2:3b",
          api_args = list(temperature = self$temperature)
        ),
        cli::cli_abort("Unknown provider: {provider}")
      )

      llm
    }
  )
)

#' Create a MultiChainComparison Module
#'
#' @description
#' Factory function to create a MultiChainComparison module that generates
#' M reasoning chains and synthesizes the best answer.
#'
#' @param signature Signature for the task, either string notation or Signature object
#' @param inner_module Optional pre-created inner module. If NULL, creates
#'   a ChainOfThought module from the signature.
#' @param M Number of reasoning chains to generate (default 3)
#' @param temperature Temperature for attempt diversity (default 0.7)
#' @param comparison_template Optional custom template for comparison prompt
#' @param ... Additional arguments passed to module constructor
#'
#' @return A MultiChainComparisonModule object
#'
#' @export
#' @examples
#' # Basic usage
#' mcc <- multi_chain_comparison("question -> answer", M = 3)
#'
#' # With custom inner module
#' cot <- chain_of_thought("question -> answer")
#' mcc <- multi_chain_comparison(
#'   "question -> answer",
#'   inner_module = cot,
#'   M = 5,
#'   temperature = 0.8
#' )
multi_chain_comparison <- function(
  signature,
  inner_module = NULL,
  M = 3L,
  temperature = 0.7,
  comparison_template = NULL,
  ...
) {
  MultiChainComparisonModule$new(
    signature = signature,
    inner_module = inner_module,
    M = M,
    temperature = temperature,
    comparison_template = comparison_template,
    ...
  )
}
