#' Program of Thought Module
#'
#' @description
#' A module that generates R code to solve problems, executes it through an
#' explicitly configured runner,
#' and uses the execution results to produce answers. This is particularly
#' effective for tasks requiring exact computation (arithmetic, statistics,
#' data manipulation) where LLMs alone are unreliable.
#'
#' @details
#' The execution flow is:
#' 1. LLM generates R code based on the inputs
#' 2. Code is executed by the configured code runner
#' 3. If execution fails, the error is fed back to the LLM for repair
#' 4. Steps 2-3 repeat until success or max_iters is reached
#' 5. Final answer is extracted from the execution result
#'
#' Security: Code execution requires explicit opt-in via `runner` or
#' `interpreter_factory`.
#' The built-in runner uses a separate process but is NOT a security sandbox.
#' Inspect `runner$policy()` before execution. For untrusted inputs, provide a
#' runner backed by OS-level sandboxing (such as a container or AppArmor).
#'
#' Runner lifecycle: supply exactly one runtime source. `runner` is
#' caller-owned, reused across calls, and never closed by dsprrr. The backend determines
#' whether execution state persists and whether `reset()` is available;
#' serialize access to stateful backends. `interpreter_factory` is a
#' zero-argument function that returns a fresh runner implementing `execute()`,
#' `policy()`, optional `start()`, and terminal `shutdown()` or `close()`. The
#' module owns that runner for one invocation and shuts it down exactly once on
#' success, error, or interrupt.
#'
#' [run_async()] supports factory-backed ProgramOfThought in an isolated mirai
#' process. It rejects caller-owned runners. [stream_async()] and a module's
#' `$stream()` method remain unavailable because streaming would bypass code
#' execution. [run_stream()] may use its one-shot `forward()` fallback, but
#' rejects an actual token-stream request before provider or factory work.
#'
#' @examples
#' \dontrun{
#' # Create a runner (required for code execution)
#' runner <- r_code_runner(timeout = 30)
#'
#' # Create a Program of Thought module
#' pot <- program_of_thought(
#'   signature = "question -> answer",
#'   runner = runner
#' )
#'
#' # Use it for computation tasks
#' result <- run(pot, question = "What is the sum of primes under 100?", .llm = llm)
#' }
#'
#' @name module-program-of-thought
NULL


#' Create a Program of Thought Module
#'
#' @description
#' Factory function to create a ProgramOfThoughtModule that generates and
#' executes R code to solve problems.
#' Use [run()] to execute it. [run_async()] supports factory-backed modules;
#' async streaming and module `$stream()` entry points reject ProgramOfThought.
#' [run_stream()] preserves the synchronous `forward()` fallback unless a
#' matching token-stream request is active; that request is rejected first.
#'
#' @param signature A Signature object or string notation defining inputs/outputs
#' @param runner Optional caller-owned code runner implementing `execute()` and
#'   `policy()`. It is retained, never automatically closed, and must not be
#'   shared concurrently when persistent.
#' @param max_iters Maximum code generation/repair iterations (default 3)
#' @param extract_answer Logical. If TRUE (default), use LLM to extract final
#'   answer from execution result. If FALSE, return execution result directly.
#' @param interpreter_factory Optional zero-argument function returning a fresh
#'   runner with `execute()`, `policy()`, optional `start()`, and idempotent
#'   terminal `shutdown()` or `close()`.
#'   Supply exactly one of `runner` and `interpreter_factory`.
#' @param ... Additional arguments passed to the module
#'
#' @return A ProgramOfThoughtModule object
#'
#' @export
#' @examples
#' \dontrun{
#' runner <- r_code_runner(timeout = 30)
#' pot <- program_of_thought("question -> answer", runner = runner)
#' result <- run(pot, question = "Calculate 847 * 293", .llm = llm)
#' }
program_of_thought <- function(
  signature,
  runner = NULL,
  max_iters = 3L,
  extract_answer = TRUE,
  ...,
  interpreter_factory = NULL
) {
  binding <- normalize_code_runner_binding(
    runner = runner,
    interpreter_factory = interpreter_factory,
    module_name = "Code execution"
  )

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
  max_iters <- normalize_pot_max_iters(max_iters)

  ProgramOfThoughtModule$new(
    signature = signature,
    runner = binding$runner,
    interpreter_factory = binding$interpreter_factory,
    max_iters = max_iters,
    extract_answer = extract_answer,
    ...
  )
}

normalize_pot_max_iters <- function(value) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == floor(value) &&
    value >= 1L &&
    value <= .Machine$integer.max
  if (!valid) {
    cli::cli_abort(
      "{.arg max_iters} must be one positive integer",
      class = "dsprrr_pot_bounds_error"
    )
  }
  as.integer(value)
}


#' ProgramOfThought Module R6 Class
#'
#' @description
#' R6 class implementing the Program of Thought pattern: generate code,
#' execute it, repair on error, and extract answers.
#'
#' @keywords internal
#' @noRd
ProgramOfThoughtModule <- R6::R6Class(
  "ProgramOfThoughtModule",
  inherit = Module,
  public = list(
    #' @field runner Code runner for code execution
    runner = NULL,

    #' @field interpreter_factory Factory for an invocation-owned code runner
    interpreter_factory = NULL,

    #' @field max_iters Maximum iterations for code repair
    max_iters = NULL,

    #' @field extract_answer Whether to use LLM to extract final answer
    extract_answer = NULL,

    #' @description
    #' Initialize a ProgramOfThoughtModule
    #'
    #' @param signature Signature object defining inputs/outputs
    #' @param runner Code runner for code execution
    #' @param max_iters Maximum code repair iterations
    #' @param extract_answer Whether to extract answer via LLM
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    #' @param interpreter_factory Optional zero-argument invocation-owned runner
    #'   factory. Supply exactly one of this and `runner`.
    initialize = function(
      signature,
      runner = NULL,
      max_iters = 3L,
      extract_answer = TRUE,
      config = list(),
      chat = NULL,
      interpreter_factory = NULL
    ) {
      binding <- normalize_code_runner_binding(
        runner = runner,
        interpreter_factory = interpreter_factory,
        module_name = "ProgramOfThought"
      )
      max_iters <- normalize_pot_max_iters(max_iters)
      super$initialize(
        signature = signature,
        config = config,
        chat = chat
      )

      self$runner <- binding$runner
      self$interpreter_factory <- binding$interpreter_factory
      self$max_iters <- max_iters
      self$extract_answer <- extract_answer

      # Store execution history
      self$state$executions <- list()
    },

    #' @description
    #' Execute the Program of Thought workflow
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
        "ProgramOfThought",
        function(runner, lease) {
          start_time <- Sys.time()
          iterations <- list()
          final_result <- NULL
          final_code <- NULL
          success <- FALSE

          # Build context for code generation
          input_context <- private$format_inputs(inputs)

          for (iter in seq_len(self$max_iters)) {
            # Generate or repair code
            if (iter == 1) {
              code_result <- private$generate_code(input_context, llm)
            } else {
              # Repair with error context
              last_iter <- iterations[[iter - 1]]
              code_result <- private$repair_code(
                input_context,
                last_iter$code,
                last_iter$execution,
                llm
              )
            }

            code <- code_result$code
            explanation <- code_result$explanation

            # Execute the code
            exec_result <- execute_code_runner(runner, code, context = inputs)

            # Record iteration
            iterations[[iter]] <- list(
              iteration = iter,
              code = code,
              explanation = explanation,
              execution = exec_result
            )

            if (exec_result$success) {
              success <- TRUE
              final_result <- exec_result$result
              final_code <- code
              break
            }

            # Log the error for debugging
            if (trace) {
              cli::cli_alert_warning(
                "Iteration {iter}: Code execution failed - {exec_result$error}"
              )
            }
          }

          # Store execution history
          if (trace) {
            self$state$executions <- c(
              self$state$executions,
              list(list(
                timestamp = start_time,
                inputs = inputs,
                iterations = iterations,
                success = success
              ))
            )
          }

          # If all iterations failed, return error
          if (!success) {
            last_error <- iterations[[length(iterations)]]$execution$error
            cli::cli_abort(c(
              "Program of Thought failed after {self$max_iters} iterations",
              "x" = "Last error: {last_error}",
              "i" = "Try increasing max_iters or simplifying the task"
            ))
          }

          # Extract or format final answer
          if (self$extract_answer && !is.null(final_result)) {
            answer <- private$extract_final_answer(
              inputs,
              final_code,
              final_result,
              llm
            )
          } else {
            answer <- private$format_result(final_result)
          }

          # Build output matching signature
          output <- private$build_output(answer)

          duration_ms <- as.numeric(
            difftime(Sys.time(), start_time, units = "secs")
          ) *
            1000

          # Build metadata
          metadata <- list(
            model = "program_of_thought",
            iterations = length(iterations),
            success = success,
            final_code = final_code,
            duration_ms = round(duration_ms, 2),
            execution_result = final_result,
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
    #' Get execution history
    #' @return List of execution records
    get_executions = function() {
      self$state$executions
    },

    #' @description
    #' Create a fresh copy of this module
    #' @return New ProgramOfThoughtModule with same settings
    reset_copy = function() {
      ProgramOfThoughtModule$new(
        signature = self$signature,
        runner = self$runner,
        interpreter_factory = self$interpreter_factory,
        max_iters = self$max_iters,
        extract_answer = self$extract_answer,
        config = self$config,
        chat = self$chat
      )
    },

    #' @description
    #' Create a deep copy while preserving the configured runtime source
    #' @return New ProgramOfThoughtModule with copied state
    deepcopy = function() {
      copied <- ProgramOfThoughtModule$new(
        signature = self$signature,
        runner = self$runner,
        max_iters = self$max_iters,
        extract_answer = self$extract_answer,
        config = lapply(self$config, identity),
        chat = self$chat,
        interpreter_factory = self$interpreter_factory
      )
      copied$state <- lapply(self$state, identity)
      copied
    },

    #' @description
    #' Print method for ProgramOfThoughtModule
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

      cli::cli_h3("ProgramOfThoughtModule")
      cli::cli_bullets(c(
        "*" = "Signature: {sig_str}",
        "*" = "Max iterations: {.val {self$max_iters}}",
        "*" = "Extract answer: {.val {self$extract_answer}}",
        "*" = "Runner: {code_runner_binding_label(self$runner, self$interpreter_factory)}"
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

    #' Format inputs for code generation prompt
    format_inputs = function(inputs) {
      parts <- vapply(
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
      paste(parts, collapse = "\n")
    },

    #' Generate initial code
    generate_code = function(input_context, llm) {
      prompt <- glue::glue(
        "
You are an expert R programmer. Generate R code to solve the following problem.

## Problem
{input_context}

## Instructions
1. Write clear, correct R code that solves the problem
2. The code should compute and return the final answer as the last expression
3. You can use base R and common packages (stats, utils)
4. Include brief comments explaining your approach
5. The inputs are available in the `.context` list (e.g., .context$question)

Return your response as JSON with two fields:
- \"code\": The R code to execute (as a string)
- \"explanation\": Brief explanation of your approach
"
      )

      # Use structured output
      output_type <- ellmer::type_object(
        code = ellmer::type_string(
          description = "R code to execute"
        ),
        explanation = ellmer::type_string(
          description = "Brief explanation of the approach"
        )
      )

      result <- tryCatch(
        llm$chat_structured(prompt, type = output_type),
        error = function(e) {
          cli::cli_abort(c(
            "Failed to generate code from LLM",
            "x" = "Error: {e$message}",
            "i" = "The LLM may be unavailable or returned an invalid response"
          ))
        }
      )

      # Validate result structure
      if (is.null(result$code) || !is.character(result$code)) {
        cli::cli_abort(c(
          "LLM returned invalid code generation response",
          "i" = "Missing or invalid 'code' field in response"
        ))
      }

      list(
        code = result$code,
        explanation = result$explanation %||% ""
      )
    },

    #' Repair code after execution error
    repair_code = function(input_context, previous_code, execution, llm) {
      error_context <- paste0(
        "Error: ",
        execution$error,
        "\n",
        if (nchar(execution$stdout) > 0) {
          paste0("Stdout: ", execution$stdout, "\n")
        } else {
          ""
        },
        if (nchar(execution$stderr) > 0) {
          paste0("Stderr: ", execution$stderr, "\n")
        } else {
          ""
        }
      )

      prompt <- glue::glue(
        "
You are an expert R programmer. Your previous code failed. Fix it.

## Original Problem
{input_context}

## Previous Code
```r
{previous_code}
```

## Execution Error
{error_context}

## Instructions
1. Analyze the error and fix the code
2. The code should compute and return the final answer as the last expression
3. The inputs are available in the `.context` list
4. Make sure to handle edge cases

Return your response as JSON with two fields:
- \"code\": The fixed R code to execute
- \"explanation\": What you changed and why
"
      )

      output_type <- ellmer::type_object(
        code = ellmer::type_string(
          description = "Fixed R code to execute"
        ),
        explanation = ellmer::type_string(
          description = "What was changed and why"
        )
      )

      result <- tryCatch(
        llm$chat_structured(prompt, type = output_type),
        error = function(e) {
          cli::cli_abort(c(
            "Failed to repair code via LLM",
            "x" = "Error: {e$message}",
            "i" = "The LLM may be unavailable or returned an invalid response"
          ))
        }
      )

      # Validate result structure
      if (is.null(result$code) || !is.character(result$code)) {
        cli::cli_abort(c(
          "LLM returned invalid code repair response",
          "i" = "Missing or invalid 'code' field in response"
        ))
      }

      list(
        code = result$code,
        explanation = result$explanation %||% ""
      )
    },

    #' Extract final answer from execution result
    extract_final_answer = function(inputs, code, result, llm) {
      # If result is simple (number, string), return directly
      if (is.atomic(result) && length(result) == 1) {
        return(as.character(result))
      }

      # For complex results, ask LLM to format
      result_str <- tryCatch(
        {
          if (is.data.frame(result)) {
            paste(utils::capture.output(print(result)), collapse = "\n")
          } else {
            paste(utils::capture.output(str(result)), collapse = "\n")
          }
        },
        error = function(e) deparse(result)
      )

      input_context <- private$format_inputs(inputs)

      prompt <- glue::glue(
        "
Given the following problem and code execution result, provide the final answer.

## Problem
{input_context}

## Code Executed
```r
{code}
```

## Execution Result
{result_str}

Provide a clear, concise answer to the original question.
"
      )

      llm$chat(prompt)
    },

    #' Format result for output
    format_result = function(result) {
      if (is.null(result)) {
        return("")
      }
      if (is.atomic(result) && length(result) == 1) {
        return(as.character(result))
      }
      # For complex objects, return as-is for structured handling
      result
    },

    #' Build output matching signature
    build_output = function(answer) {
      # Get output field names from signature
      output_type <- self$signature@output_type

      if (methods::.hasSlot(output_type, "properties")) {
        props <- output_type@properties
        if (length(props) == 1) {
          # Single output field
          output <- list()
          output[[names(props)[1]]] <- answer
          return(output)
        }
      }

      # Default: use "answer" field
      list(answer = answer)
    }
  )
)
