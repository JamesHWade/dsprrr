#' R6 RAGModule Class
#'
#' @description
#' Retrieval-Augmented Generation module that combines document retrieval
#' with LLM generation. Integrates with ragnar for vector store operations.
#'
#' @details
#' RAGModule extends the base Module class to automatically retrieve relevant
#' context from a document store before generating responses. This enables
#' grounded generation based on a knowledge base.
#'
#' The module supports:
#' - ragnar stores for vector-based retrieval
#' - Custom retriever functions for flexible retrieval strategies
#' - Configurable number of retrieved documents (k)
#' - Context formatting for prompt construction
#'
#' @keywords internal
#' @noRd
RAGModule <- R6::R6Class(
  "RAGModule",
  inherit = Module,
  public = list(
    #' @field store ragnar store for document retrieval
    store = NULL,

    #' @field retriever Custom retriever function (optional)
    retriever = NULL,

    #' @field k Number of documents to retrieve
    k = 5L,

    #' @field context_format Format string for retrieved context
    context_format = "relevant_context",

    #' @description
    #' Initialize a new RAGModule
    #' @param signature S7 Signature object
    #' @param store Optional ragnar store
    #' @param retriever Optional custom retriever function
    #' @param k Number of documents to retrieve (default 5)
    #' @param context_format Field name for context in prompt (default "relevant_context")
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
      signature,
      store = NULL,
      retriever = NULL,
      k = 5L,
      context_format = "relevant_context",
      config = list(),
      chat = NULL
    ) {
      super$initialize(signature, config, chat)

      if (!is.null(store) && !is.null(retriever)) {
        cli::cli_warn(c(
          "Both {.arg store} and {.arg retriever} provided",
          "i" = "{.arg retriever} will be used, {.arg store} will be ignored"
        ))
      }

      self$store <- store
      self$retriever <- retriever
      self$k <- as.integer(k)
      self$context_format <- context_format
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

      # Extract query from inputs for retrieval
      query <- private$extract_query(inputs)

      # Retrieve relevant context
      context <- private$retrieve_context(query)

      # Add context to inputs
      inputs[[self$context_format]] <- context

      # Build prompt with context
      prompt <- private$build_prompt(inputs)

      # Get LLM client
      llm <- .llm %||% self$chat %||% private$get_default_llm()

      # Record start time
      start_time <- Sys.time()

      # Make LLM call
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

      # Get turn info from ellmer
      assistant_turn <- tryCatch(
        llm$last_turn(role = "assistant"),
        error = function(e) NULL
      )
      user_turn <- tryCatch(
        llm$last_turn(role = "user"),
        error = function(e) NULL
      )

      # Extract token info
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

      cost <- if (!is.null(assistant_turn)) assistant_turn@cost else NA_real_
      duration_s <- if (!is.null(assistant_turn)) {
        assistant_turn@duration
      } else {
        NA_real_
      }

      model <- tryCatch(llm$get_model(), error = function(e) NA_character_)

      # Create metadata with RAG-specific info
      metadata <- list(
        timestamp = end_time,
        model = model,
        prompt = prompt,
        instructions = self$signature@instructions,
        prompt_length = nchar(prompt),
        input_tokens = token_info$input_tokens,
        output_tokens = token_info$output_tokens,
        cached_input_tokens = token_info$cached_input_tokens,
        total_tokens = token_info$total_tokens,
        cost = cost,
        duration_s = duration_s,
        latency_ms = latency_ms,
        # RAG-specific metadata
        retrieved_context = context,
        k = self$k,
        query = query
      )

      # Record trace if requested
      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = inputs,
          output = result,
          user_turn = user_turn,
          assistant_turn = assistant_turn,
          model = model,
          retrieved_context = context,
          query = query
        )
        self$state$traces <- append(self$state$traces, list(trace_entry))
        trace_entry$prompt <- prompt
        add_to_global_history(trace_entry, source = "RAGModule")
      }

      # Return tibble format
      tibble::tibble(
        output = list(result),
        chat = list(llm),
        metadata = list(metadata)
      )
    },

    #' @description
    #' Print the module
    print = function() {
      cli::cli_h2("RAGModule")

      cli::cli_h3("Signature")
      print(self$signature)

      cli::cli_h3("Retrieval Configuration")
      cli::cli_text("  k: {self$k} documents")
      cli::cli_text("  Context field: {.field {self$context_format}}")

      if (!is.null(self$store)) {
        cli::cli_text("  Store: {.cls ragnar store}")
      } else if (!is.null(self$retriever)) {
        cli::cli_text("  Retriever: {.cls custom function}")
      } else {
        cli::cli_alert_warning("No store or retriever configured")
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
    reset_copy = function() {
      RAGModule$new(
        signature = self$signature,
        store = self$store,
        retriever = self$retriever,
        k = self$k,
        context_format = self$context_format,
        config = list(),
        chat = self$chat
      )
    }
  ),

  private = list(
    # Extract query from inputs for retrieval
    extract_query = function(inputs) {
      # Look for common query field names
      query_fields <- c("query", "question", "text", "input", "prompt")

      for (field in query_fields) {
        if (field %in% names(inputs) && !is.null(inputs[[field]])) {
          return(as.character(inputs[[field]]))
        }
      }

      # Fall back to first string input
      for (name in names(inputs)) {
        value <- inputs[[name]]
        if (is.character(value) && length(value) == 1 && nzchar(value)) {
          return(value)
        }
      }

      cli::cli_abort(c(
        "Could not extract query from inputs",
        "i" = "Inputs should contain a field like 'query', 'question', or 'text'"
      ))
    },

    # Retrieve context from store or retriever
    retrieve_context = function(query) {
      fail_on_error <- self$config$fail_on_retrieval_error %||% FALSE
      retrieval_error <- NULL

      if (!is.null(self$retriever)) {
        # Use custom retriever
        docs <- tryCatch(
          self$retriever(query, k = self$k),
          error = function(e) {
            retrieval_error <<- e
            character(0)
          }
        )

        if (!is.null(retrieval_error)) {
          if (fail_on_error) {
            cli::cli_abort(c(
              "Retriever failed",
              "x" = retrieval_error$message,
              "i" = "Set {.code config$fail_on_retrieval_error = FALSE} to continue with empty context"
            ), parent = retrieval_error)
          } else {
            cli::cli_warn(c(
              "Retriever failed, continuing with empty context",
              "x" = retrieval_error$message,
              "i" = "Set {.code config$fail_on_retrieval_error = TRUE} to fail on errors"
            ))
          }
        }
      } else if (!is.null(self$store)) {
        # Check if ragnar is available
        if (!requireNamespace("ragnar", quietly = TRUE)) {
          cli::cli_abort(c(
            "ragnar package required for store-based retrieval",
            "i" = "Install with: {.code pak::pak('tidyverse/ragnar')}"
          ))
        }

        # Use ragnar store retrieval
        docs <- tryCatch(
          {
            results <- ragnar::ragnar_retrieve(self$store, query, k = self$k)
            # Extract text content from results
            if (is.data.frame(results) && "text" %in% names(results)) {
              results$text
            } else if (is.list(results)) {
              vapply(results, function(x) x$text %||% as.character(x), character(1))
            } else {
              as.character(results)
            }
          },
          error = function(e) {
            retrieval_error <<- e
            character(0)
          }
        )

        if (!is.null(retrieval_error)) {
          if (fail_on_error) {
            cli::cli_abort(c(
              "ragnar retrieval failed",
              "x" = retrieval_error$message,
              "i" = "Set {.code config$fail_on_retrieval_error = FALSE} to continue with empty context"
            ), parent = retrieval_error)
          } else {
            cli::cli_warn(c(
              "ragnar retrieval failed, continuing with empty context",
              "x" = retrieval_error$message,
              "i" = "Set {.code config$fail_on_retrieval_error = TRUE} to fail on errors"
            ))
          }
        }
      } else {
        cli::cli_warn(c(
          "No store or retriever configured",
          "i" = "RAG module requires a {.arg store} or {.arg retriever}"
        ))
        docs <- character(0)
      }

      # Format context for prompt
      if (length(docs) == 0) {
        cli::cli_inform(c(
          "i" = "No documents matched the retrieval query",
          "i" = "Query: {.val {substr(query, 1, 80)}}"
        ))
        return("No relevant context found.")
      }

      # Number and format documents
      formatted <- vapply(seq_along(docs), function(i) {
        paste0("[", i, "] ", docs[i])
      }, character(1))

      paste(formatted, collapse = "\n\n")
    },

    # Build prompt including context
    build_prompt = function(inputs) {
      prompt_parts <- character()

      # Format inputs
      for (input_spec in self$signature@inputs) {
        name <- input_spec$name
        if (name %in% names(inputs)) {
          value <- inputs[[name]]

          # Add description as comment if present
          if (!is.null(input_spec$description) && nzchar(input_spec$description)) {
            prompt_parts <- c(prompt_parts, paste0("# ", input_spec$description))
          }

          if (is.character(value) && length(value) == 1) {
            prompt_parts <- c(prompt_parts, paste0(name, ": ", value))
          } else {
            prompt_parts <- c(
              prompt_parts,
              paste0(name, ": ", jsonlite::toJSON(value, auto_unbox = TRUE))
            )
          }
        }
      }

      # Add context field
      if (self$context_format %in% names(inputs)) {
        prompt_parts <- c(
          prompt_parts,
          "",
          paste0("# Retrieved context"),
          paste0(self$context_format, ":"),
          inputs[[self$context_format]]
        )
      }

      paste(prompt_parts, collapse = "\n")
    },

    # Get default LLM (inherited pattern from PredictModule)
    get_default_llm = function() {
      provider <- self$config$provider %||%
        Sys.getenv("DSPRRR_PROVIDER", "openai")
      provider <- switch(provider, anthropic = "claude", provider)

      model_name <- self$config$model

      switch(
        provider,
        openai = ellmer::chat_openai(model = model_name %||% "gpt-4o-mini"),
        claude = ellmer::chat_claude(
          model = model_name %||% "claude-sonnet-4-20250514",
          max_tokens = self$config$max_tokens %||% 4096
        ),
        gemini = ellmer::chat_google_gemini(
          model = model_name %||% "gemini-2.0-flash"
        ),
        ollama = ellmer::chat_ollama(model = model_name %||% "llama3.2:3b"),
        cli::cli_abort("Unknown provider: {provider}")
      )
    },

    # Call LLM with structured output
    call_llm = function(
      llm,
      prompt,
      output_type,
      instructions = "",
      inputs = list()
    ) {
      full_prompt <- if (nchar(instructions) > 0) {
        paste(instructions, prompt, sep = "\n\n")
      } else {
        prompt
      }

      llm$chat_structured(
        full_prompt,
        type = output_type,
        echo = "none"
      )
    }
  )
)


#' Create a RAG Module
#'
#' @description
#' Factory function to create a Retrieval-Augmented Generation module.
#' RAG modules automatically retrieve relevant context from a document store
#' before generating responses.
#'
#' @param signature A signature string or Signature object defining inputs/outputs.
#' @param store Optional ragnar store for document retrieval.
#' @param retriever Optional custom retriever function. Should accept `(query, k)`
#'   and return a character vector of documents.
#' @param k Number of documents to retrieve (default 5).
#' @param context_format Field name for the context in the prompt (default
#'   "relevant_context"). This field is automatically added to inputs.
#' @param config Optional configuration list.
#' @param chat Optional ellmer Chat object.
#'
#' @return A RAGModule object.
#'
#' @export
#' @examples
#' \dontrun{
#' # With ragnar store
#' store <- ragnar::ragnar_store_create(documents)
#' mod <- rag_module(
#'   "question, relevant_context -> answer",
#'   store = store,
#'   k = 3
#' )
#'
#' result <- run(mod, question = "What is the capital of France?")
#'
#' # With custom retriever
#' my_retriever <- function(query, k) {
#'   # Custom retrieval logic
#'   c("Document 1 content", "Document 2 content")
#' }
#'
#' mod <- rag_module(
#'   "query, relevant_context -> response",
#'   retriever = my_retriever
#' )
#' }
rag_module <- function(
  signature,
  store = NULL,
  retriever = NULL,
  k = 5L,
  context_format = "relevant_context",
  config = list(),
  chat = NULL
) {
  # Parse signature if string
  sig <- if (is.character(signature)) {
    signature(signature)
  } else if (inherits(signature, "dsprrr::Signature")) {
    signature
  } else {
    cli::cli_abort(c(
      "Invalid signature",
      "x" = "{.arg signature} must be a string or Signature object"
    ))
  }

  # Validate that either store or retriever is provided
  if (is.null(store) && is.null(retriever)) {
    cli::cli_warn(c(
      "No {.arg store} or {.arg retriever} provided",
      "i" = "RAG module will not retrieve context",
      "i" = "Provide a ragnar store or custom retriever function"
    ))
  }

  RAGModule$new(
    signature = sig,
    store = store,
    retriever = retriever,
    k = as.integer(k),
    context_format = context_format,
    config = config,
    chat = chat
  )
}
