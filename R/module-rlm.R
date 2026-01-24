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
#' 3. Code is executed in an isolated subprocess via RCodeRunner
#' 4. Results are fed back to the LLM for the next iteration
#' 5. Process continues until SUBMIT() is called or max_iterations reached
#' 6. If max_iterations reached without SUBMIT(), fallback extraction is used
#'
#' Available REPL tools:
#' - `SUBMIT(answer)`: Terminate and return final answer
#' - `peek(var, start, end)`: View a slice of a variable (default: first 1000 chars)
#' - `search(var, pattern)`: Regex search in variable
#' - `rlm_query(query, context_slice)`: Recursive LLM call (requires sub_lm)
#' - `rlm_query_batch(queries, slices)`: Batched recursive calls
#'
#' Security: Code execution requires explicit opt-in via a runner parameter.
#' The runner provides subprocess isolation but is NOT a security sandbox.
#' For untrusted inputs, use OS-level sandboxing (containers, AppArmor).
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
#'
#' @param signature A Signature object or string notation defining inputs/outputs
#' @param runner An RCodeRunner object for code execution. Required.
#' @param max_iterations Maximum REPL iterations before fallback (default 20)
#' @param max_llm_calls Maximum recursive LLM calls allowed (default 50)
#' @param max_output_chars Maximum characters per execution output (default 100000)
#' @param sub_lm Optional ellmer Chat for recursive queries. NULL = disabled.
#' @param verbose Logical. Print execution progress (default FALSE)
#' @param tools Named list of user-defined R functions to inject into REPL.
#'   Each tool becomes available as a function in the code execution environment.
#'   Non-function values in the list will cause an error.
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
  runner,
  max_iterations = 20L,
  max_llm_calls = 50L,
  max_output_chars = 100000L,
  sub_lm = NULL,
  verbose = FALSE,
  tools = list(),
  ...
) {
  # Validate runner
  if (missing(runner) || is.null(runner)) {
    cli::cli_abort(c(
      "RLM requires an explicit runner for code execution",
      "i" = "Create one with: {.code runner <- r_code_runner()}",
      "i" = "Then pass it: {.code rlm_module(..., runner = runner)}"
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

  # Validate bounds for iterations and calls
  max_iterations <- as.integer(max_iterations)
  max_llm_calls <- as.integer(max_llm_calls)

  if (max_iterations < 1L) {
    cli::cli_abort(c(
      "max_iterations must be at least 1",
      "x" = "You provided: {.val {max_iterations}}"
    ))
  }

  if (max_llm_calls < 0L) {
    cli::cli_abort(c(
      "max_llm_calls must be non-negative",
      "x" = "You provided: {.val {max_llm_calls}}"
    ))
  }

  # Validate tools
  if (!is.list(tools)) {
    cli::cli_abort(c(
      "tools must be a named list of functions",
      "x" = "You provided: {.cls {class(tools)[1]}}"
    ))
  }

  if (length(tools) > 0 && is.null(names(tools))) {
    cli::cli_abort(c(
      "tools must be a named list",
      "i" = "Example: {.code tools = list(my_func = function(...) ...)}"
    ))
  }

  # Validate all tools are functions
  if (length(tools) > 0) {
    non_functions <- vapply(tools, Negate(is.function), logical(1))
    if (any(non_functions)) {
      bad_names <- names(tools)[non_functions]
      cli::cli_abort(c(
        "All tools must be functions",
        "x" = "Non-function tool{?s}: {.val {bad_names}}"
      ))
    }
  }

  RLMModule$new(
    signature = signature,
    runner = runner,
    max_iterations = max_iterations,
    max_llm_calls = max_llm_calls,
    max_output_chars = as.integer(max_output_chars),
    sub_lm = sub_lm,
    verbose = verbose,
    tools = tools,
    ...
  )
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
    #' @field runner RCodeRunner for code execution
    runner = NULL,

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
    #' @param runner RCodeRunner for code execution
    #' @param max_iterations Maximum REPL iterations
    #' @param max_llm_calls Maximum recursive LLM calls
    #' @param max_output_chars Maximum output size
    #' @param sub_lm Optional LLM for recursive queries
    #' @param verbose Whether to print progress
    #' @param tools User-defined tools
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
      signature,
      runner,
      max_iterations = 20L,
      max_llm_calls = 50L,
      max_output_chars = 100000L,
      sub_lm = NULL,
      verbose = FALSE,
      tools = list(),
      config = list(),
      chat = NULL
    ) {
      super$initialize(
        signature = signature,
        config = config,
        chat = chat
      )

      self$runner <- runner
      self$max_iterations <- as.integer(max_iterations)
      self$max_llm_calls <- as.integer(max_llm_calls)
      self$max_output_chars <- as.integer(max_output_chars)
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

      # Get LLM - clone for fresh conversation
      base_llm <- .llm %||% self$chat %||% get_default_chat()
      if (is.null(base_llm)) {
        cli::cli_abort("No LLM provided. Pass .llm or set a default chat.")
      }

      llm <- base_llm$clone()

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
        prompt <- private$build_iteration_prompt(system_prompt, history, iter)

        # Get LLM response (code generation)
        response <- private$get_code_response(llm, prompt)

        if (self$verbose) {
          cli::cli_alert("Code generated: {substr(response$code, 1, 100)}...")
        }

        # Execute code with RLM tools injected
        exec_result <- private$execute_with_rlm_tools(
          response$code,
          inputs,
          call_counter
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
        if (exec_result$is_final) {
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

      # Fallback extract if no SUBMIT()
      if (is.null(final_answer)) {
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
            final_answer = final_answer,
            iterations_used = length(history),
            llm_calls_used = call_counter$count
          ))
        )
      }

      # Build output matching signature
      output <- private$build_output(final_answer)

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
        repl_history = history
      )

      tibble::tibble(
        output = list(output),
        chat = list(llm),
        metadata = list(metadata)
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
        "*" = "Runner timeout: {.val {self$runner$timeout}}s",
        "*" = "Recursive queries: {.val {if (is.null(self$sub_lm)) 'disabled' else 'enabled'}}",
        "*" = "Custom tools: {.val {length(self$tools)}}"
      ))
      invisible(self)
    }
  ),

  private = list(
    #' Get output field names from signature
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

      # Build tool descriptions
      tool_desc <- c(
        "- `SUBMIT(answer)`: Submit your final answer and terminate",
        "- `peek(var, start = 1, end = 1000)`: View a character slice of a variable",
        "- `search(var, pattern)`: Regex search in variable, returns matches"
      )

      if (has_sub_lm) {
        tool_desc <- c(
          tool_desc,
          "- `rlm_query(query, context_slice = NULL)`: Ask a sub-question to another LLM",
          "- `rlm_query_batch(queries, slices = NULL)`: Batch multiple sub-questions"
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
4. Break complex queries into smaller sub-questions{if (has_sub_lm) ' using rlm_query()' else ''}
5. Call SUBMIT(answer) when you have the final answer
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
        code = result$code
      )
    },

    #' Execute code with RLM tools injected
    execute_with_rlm_tools = function(code, inputs, call_counter) {
      # Build RLM prelude that defines tools
      rlm_prelude <- create_rlm_prelude(
        max_llm_calls = self$max_llm_calls,
        has_sub_lm = !is.null(self$sub_lm),
        custom_tools = self$tools
      )

      # Build combined code: prelude + user code
      combined_code <- paste0(
        "# RLM Prelude\n",
        rlm_prelude,
        "\n\n# User Code\n",
        code
      )

      # Execute with inputs as context
      result <- self$runner$execute(combined_code, context = inputs)

      # Validate runner result structure
      if (!is.list(result)) {
        cli::cli_abort(c(
          "Runner returned invalid result",
          "x" = "Expected list, got {.cls {class(result)[1]}}"
        ))
      }

      required_fields <- c("success", "result")
      missing_fields <- setdiff(required_fields, names(result))
      if (length(missing_fields) > 0) {
        cli::cli_abort(c(
          "Runner result missing required fields",
          "x" = "Missing: {.val {missing_fields}}"
        ))
      }

      # Detect SUBMIT termination using helper function
      is_final <- is_rlm_final(result$result)
      final_value <- if (is_final) {
        extract_rlm_final(result$result)
      } else {
        NULL
      }

      # Handle rlm_query requests (if sub_lm is available)
      if (is_rlm_query_request(result$result) && !is.null(self$sub_lm)) {
        # Process the recursive query (single or batch)
        query_result <- private$process_rlm_query(
          result$result,
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
          list(
            success = TRUE,
            formatted_output = paste0("Query result: ", response),
            error = NULL
          )
        },
        error = function(e) {
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

      # Process each query
      results <- vector("list", n_queries)
      errors <- character()

      for (i in seq_len(n_queries)) {
        call_counter$count <- call_counter$count + 1L

        query <- queries[[i]]
        context_slice <- if (!is.null(slices)) slices[[i]] else NULL

        prompt <- if (!is.null(context_slice)) {
          paste0("Context:\n", context_slice, "\n\nQuestion: ", query)
        } else {
          query
        }

        results[[i]] <- tryCatch(
          {
            self$sub_lm$chat(prompt)
          },
          error = function(e) {
            errors <<- c(errors, paste0("Query ", i, ": ", e$message))
            paste0("[Error: ", e$message, "]")
          }
        )
      }

      # Format output
      formatted_parts <- vapply(
        seq_len(n_queries),
        function(i) paste0("Query ", i, " result: ", results[[i]]),
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

    #' Build output matching signature
    build_output = function(answer) {
      output_type <- self$signature@output_type

      if (methods::.hasSlot(output_type, "properties")) {
        props <- output_type@properties
        if (length(props) == 1) {
          return(setNames(list(answer), names(props)[1]))
        }
      }

      list(answer = answer)
    }
  )
)
