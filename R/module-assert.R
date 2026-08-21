#' Assert Wrapper Module
#'
#' @description
#' An R6 class that wraps any Module to validate outputs with assertions
#' and backtrack (retry) when hard assertions fail.
#'
#' @details
#' AssertModule implements DSPy-style assertions for output validation:
#' - **Hard assertions** (`assert_output`): Must be satisfied. Failures trigger
#'   retries with feedback about what went wrong.
#' - **Soft suggestions** (`suggest_output`): Log warnings but don't block execution.
#'
#' The execution flow is:
#' 1. Run the wrapped module
#' 2. Evaluate all assertions against the output
#' 3. Log any soft suggestion failures as warnings
#' 4. If hard assertions pass, return the result
#' 5. If hard assertions fail and retries remain, inject feedback and retry
#' 6. If max retries exceeded, either error or warn (based on configuration)
#'
#' @noRd
AssertModule <- R6::R6Class(
  "AssertModule",
  inherit = Module,
  public = list(
    #' @field module The wrapped module
    module = NULL,

    #' @field assertion_set Validation rules created by `assertion_set()`
    assertion_set = NULL,

    #' @field max_retries Maximum retry attempts
    max_retries = NULL,

    #' @field on_failure What to do when max retries exceeded: "error" or "warn"
    on_failure = NULL,

    #' @field feedback_template Template for feedback on assertion failure
    feedback_template = NULL,

    #' @description
    #' Initialize an Assert wrapper module
    #'
    #' @param module The module to wrap (must inherit from Module)
    #' @param assertions A list of assertion rules or the result of
    #'   `assertion_set()`
    #' @param max_retries Maximum number of retry attempts (default 3)
    #' @param on_failure What to do when max retries exceeded: "error" (default)
    #'   or "warn" (return best attempt with warning)
    #' @param feedback_template Template for feedback injection on retry.
    #'   Uses glue syntax with {failures} for failure messages.
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
      module,
      assertions,
      max_retries = 3L,
      on_failure = c("error", "warn"),
      feedback_template = NULL,
      config = list(),
      chat = NULL
    ) {
      # Validate module
      if (!inherits(module, "Module")) {
        cli::cli_abort(c(
          "module must be a Module object",
          "x" = "You provided: {.cls {class(module)[1]}}"
        ))
      }

      # Convert assertions to AssertionSet if needed
      if (S7::S7_inherits(assertions, AssertionSet)) {
        assertion_set <- assertions
      } else if (is.list(assertions)) {
        assertion_set <- assertion_set(assertions)
      } else {
        cli::cli_abort(c(
          "assertions must be a list of assertion rules or the result of {.fn assertion_set}",
          "x" = "You provided: {.cls {class(assertions)[1]}}"
        ))
      }

      on_failure <- match.arg(on_failure)

      # Validate module's signature exists
      if (is.null(module$signature)) {
        cli::cli_abort(c(
          "Wrapped module must have a valid signature",
          "x" = "module$signature is NULL"
        ))
      }

      # Use module's signature
      super$initialize(
        signature = module$signature,
        config = config,
        chat = chat %||% module$chat
      )

      self$module <- module
      self$assertion_set <- assertion_set
      self$max_retries <- as.integer(max_retries)
      self$on_failure <- on_failure
      self$feedback_template <- feedback_template %||%
        paste0(
          "Your previous response did not satisfy the following requirements:\n",
          "{failures}\n\n",
          "Please generate a new response that satisfies ALL requirements."
        )

      # Store attempt history
      self$state$attempts <- list()
      self$state$assertion_results <- list()
    },

    #' @description
    #' Execute the module with assertion validation and backtracking
    #'
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param rollout_id Optional id inherited from an enclosing wrapper. Taken
    #'   as a formal (not via `...`) so nested wrappers don't pass it twice.
    #' @param ... Additional arguments passed to wrapped module
    #' @return Tibble with output, chat, metadata columns
    forward = function(
      batch,
      .llm = NULL,
      trace = TRUE,
      rollout_id = NULL,
      ...
    ) {
      # Handle both list and data frame inputs
      if (is.data.frame(batch)) {
        inputs <- as.list(batch[1, , drop = FALSE])
      } else {
        inputs <- batch
      }

      start_time <- Sys.time()
      attempts <- list()
      assertion_results_history <- list()
      module_errors <- list() # Track errors for debugging
      best_result <- NULL
      best_metadata <- NULL
      best_chat <- NULL
      best_assertion_result <- NULL
      current_batch <- batch
      previous_feedback <- NULL

      for (i in seq_len(self$max_retries + 1)) {
        # +1 because first attempt isn't a "retry"
        # Inject feedback from previous failed assertion (if any)
        if (i > 1 && !is.null(previous_feedback)) {
          current_batch <- private$inject_feedback(
            current_batch,
            previous_feedback
          )
        }

        # Run the wrapped module. rollout_id partitions the cache per attempt so
        # retries are not served identical cached responses. compose_* folds in
        # any inherited id so nesting stays unique.
        result <- tryCatch(
          {
            self$module$forward(
              current_batch,
              .llm = .llm,
              trace = FALSE,
              rollout_id = compose_rollout_id(rollout_id, i),
              ...
            )
          },
          error = function(e) {
            cli::cli_warn(c(
              "Attempt {i} failed in AssertModule",
              "x" = e$message
            ))
            # Track error for debugging
            module_errors[[length(module_errors) + 1]] <<- list(
              attempt = i,
              error = e$message,
              timestamp = Sys.time()
            )
            NULL
          }
        )

        if (is.null(result)) {
          next
        }

        # Extract prediction and metadata
        prediction <- result$output[[1]]
        metadata <- result$metadata[[1]]
        chat_obj <- result$chat[[1]]

        # Evaluate assertions
        assertion_result <- evaluate_assertion_set(
          self$assertion_set,
          prediction
        )
        assertion_results_history <- append(
          assertion_results_history,
          list(assertion_result)
        )

        # Log soft suggestion failures
        if (assertion_result$n_soft_failed > 0) {
          for (failure in assertion_result$soft_failures) {
            cli::cli_warn(c(
              "!" = "Suggestion not met: {failure$message}"
            ))
          }
        }

        # Record attempt
        attempt_entry <- list(
          attempt = i,
          prediction = prediction,
          metadata = metadata,
          hard_passed = assertion_result$all_passed,
          n_hard_failed = assertion_result$n_hard_failed,
          n_soft_failed = assertion_result$n_soft_failed,
          feedback = previous_feedback
        )
        attempts <- append(attempts, list(attempt_entry))

        # Track best result (prefer one with fewest hard failures)
        if (
          is.null(best_result) ||
            assertion_result$n_hard_failed < best_assertion_result$n_hard_failed
        ) {
          best_result <- prediction
          best_metadata <- metadata
          best_chat <- chat_obj
          best_assertion_result <- assertion_result
        }

        # If all hard assertions passed, we're done
        if (assertion_result$all_passed) {
          break
        }

        # Generate feedback for next attempt (only if there will be one)
        if (i <= self$max_retries) {
          previous_feedback <- private$generate_feedback(assertion_result)
        }
      }

      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # Handle case where we never got a valid result
      if (is.null(best_result)) {
        error_msgs <- if (length(module_errors) > 0) {
          vapply(module_errors, function(e) e$error, character(1))
        } else {
          "No attempts completed successfully"
        }
        cli::cli_abort(c(
          "All attempts failed in AssertModule",
          "x" = error_msgs
        ))
      }

      # Handle case where assertions never passed after all retries
      if (!best_assertion_result$all_passed) {
        failure_msgs <- vapply(
          best_assertion_result$hard_failures,
          function(f) f$message,
          character(1)
        )
        if (self$on_failure == "error") {
          cli::cli_abort(c(
            "Assertions failed after {length(attempts)} attempt{?s}",
            "x" = failure_msgs
          ))
        } else {
          cli::cli_warn(c(
            "!" = "Assertions failed after {length(attempts)} attempt{?s}, returning best result",
            "x" = failure_msgs
          ))
        }
      }

      usage <- aggregate_module_usage_metadata(
        lapply(attempts, function(attempt) attempt$metadata),
        unknown_attempt = length(module_errors) > 0L
      )

      # Create aggregated metadata
      final_metadata <- list(
        timestamp = end_time,
        model = best_metadata$model,
        n_attempts = length(attempts),
        assertions_passed = best_assertion_result$all_passed,
        n_hard_failed = best_assertion_result$n_hard_failed,
        n_soft_failed = best_assertion_result$n_soft_failed,
        total_tokens = usage$total_tokens,
        cost = usage$cost,
        provider_calls = usage$provider_calls,
        latency_ms = latency_ms,
        module_errors = if (length(module_errors) > 0) module_errors else NULL
      )

      # Record trace with all attempts
      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = inputs,
          output = best_result,
          attempts = attempts,
          assertion_results = assertion_results_history,
          module_errors = module_errors,
          n_attempts = length(attempts),
          assertions_passed = best_assertion_result$all_passed,
          model = best_metadata$model
        )
        self$state$traces <- append(self$state$traces, list(trace_entry))
        self$state$attempts <- append(self$state$attempts, list(attempts))
        self$state$assertion_results <- append(
          self$state$assertion_results,
          list(assertion_results_history)
        )
      }

      tibble::tibble(
        output = list(best_result),
        chat = list(best_chat),
        metadata = list(final_metadata)
      )
    },

    #' @description
    #' Get all attempts from the last run or all runs
    #' @param all Logical. If TRUE, return all attempts across all runs
    #' @return A tibble of attempts
    get_attempts = function(all = FALSE) {
      if (length(self$state$attempts) == 0) {
        return(tibble::tibble(
          run = integer(),
          attempt = integer(),
          prediction = list(),
          hard_passed = logical(),
          n_hard_failed = integer(),
          n_soft_failed = integer()
        ))
      }

      if (all) {
        # All runs
        rows <- list()
        for (run_idx in seq_along(self$state$attempts)) {
          run_attempts <- self$state$attempts[[run_idx]]
          for (a in run_attempts) {
            rows <- append(
              rows,
              list(list(
                run = run_idx,
                attempt = a$attempt,
                prediction = list(a$prediction),
                hard_passed = a$hard_passed,
                n_hard_failed = a$n_hard_failed,
                n_soft_failed = a$n_soft_failed
              ))
            )
          }
        }
      } else {
        # Just last run
        last_run <- self$state$attempts[[length(self$state$attempts)]]
        rows <- lapply(last_run, function(a) {
          list(
            run = length(self$state$attempts),
            attempt = a$attempt,
            prediction = list(a$prediction),
            hard_passed = a$hard_passed,
            n_hard_failed = a$n_hard_failed,
            n_soft_failed = a$n_soft_failed
          )
        })
      }

      if (length(rows) == 0) {
        return(tibble::tibble(
          run = integer(),
          attempt = integer(),
          prediction = list(),
          hard_passed = logical(),
          n_hard_failed = integer(),
          n_soft_failed = integer()
        ))
      }

      tibble::tibble(
        run = vapply(rows, function(r) r$run, integer(1)),
        attempt = vapply(rows, function(r) r$attempt, integer(1)),
        prediction = lapply(rows, function(r) r$prediction[[1]]),
        hard_passed = vapply(rows, function(r) r$hard_passed, logical(1)),
        n_hard_failed = vapply(rows, function(r) r$n_hard_failed, integer(1)),
        n_soft_failed = vapply(rows, function(r) r$n_soft_failed, integer(1))
      )
    },

    #' @description
    #' Print the module
    print = function() {
      n_assert <- sum(vapply(
        self$assertion_set@assertions,
        function(a) a@type == "assert",
        logical(1)
      ))
      n_suggest <- sum(vapply(
        self$assertion_set@assertions,
        function(a) a@type == "suggest",
        logical(1)
      ))

      cli::cli_h2("AssertModule")
      cli::cli_text("Wrapped module: {.cls {class(self$module)[1]}}")
      cli::cli_text("Hard assertions: {n_assert}")
      cli::cli_text("Soft suggestions: {n_suggest}")
      cli::cli_text("Max retries: {self$max_retries}")
      cli::cli_text("On failure: {self$on_failure}")

      if (length(self$state$traces) > 0) {
        last_trace <- self$state$traces[[length(self$state$traces)]]
        cli::cli_h3("Last Run")
        cli::cli_text("  Attempts: {last_trace$n_attempts}")
        cli::cli_text("  Assertions passed: {last_trace$assertions_passed}")
      }

      invisible(self)
    },

    #' @description
    #' Create a reset copy of the module
    reset_copy = function() {
      artifact_copy_runtime(
        self,
        AssertModule$new(
          module = self$module$reset_copy(),
          assertions = self$assertion_set,
          max_retries = self$max_retries,
          on_failure = self$on_failure,
          feedback_template = self$feedback_template,
          config = list(),
          chat = self$chat
        )
      )
    },

    #' @description
    #' Apply optimization parameters
    apply_optimization_params = function(params) {
      # Pass through to wrapped module
      if (!is.null(params$max_retries)) {
        self$max_retries <- as.integer(params$max_retries)
      }
      # Forward other params to wrapped module
      wrapped_params <- params[!names(params) %in% c("max_retries")]
      if (length(wrapped_params) > 0) {
        self$module$apply_optimization_params(wrapped_params)
      }
      invisible(self)
    }
  ),

  private = list(
    #' Generate feedback from assertion failures
    generate_feedback = function(assertion_result) {
      failure_msgs <- vapply(
        assertion_result$hard_failures,
        function(f) paste0("- ", f$message),
        character(1)
      )
      failures_str <- paste(failure_msgs, collapse = "\n")

      glue::glue(
        self$feedback_template,
        failures = failures_str,
        .open = "{",
        .close = "}"
      )
    },

    #' Inject feedback into the batch
    inject_feedback = function(batch, feedback) {
      # Add or update assertion_feedback field
      # Works for both data frames and lists via $ assignment
      batch$assertion_feedback <- feedback
      batch
    }
  )
)

#' Wrap a Module with Assertions
#'
#' @description
#' Factory function to create an assertion wrapper around any module.
#' Validates outputs against assertions and retries with backtracking
#' when hard assertions fail.
#'
#' @param module A Module object to wrap
#' @param assertions A list of assertion rules (from `assert_output()` or
#'   `suggest_output()`) or the result of `assertion_set()`
#' @param max_retries Maximum number of retry attempts (default 3)
#' @param on_failure What to do when max retries exceeded: "error" (default)
#'   or "warn" (return best attempt with warning)
#' @param feedback_template Template for feedback injection on retry.
#'   Uses glue syntax. Available variables:
#'   \itemize{
#'     \item `{failures}`: Bulleted list of hard assertion failure messages
#'   }
#' @param ... Additional arguments passed to the module constructor
#'
#' @return An AssertModule object
#'
#' @details
#' ## Assertion Types
#'
#' - **`assert_output()`**: Hard assertion. Must pass or execution retries.
#'   Use for critical constraints like length limits, required patterns, etc.
#'
#' - **`suggest_output()`**: Soft suggestion. Logs warning but doesn't retry.
#'   Use for style preferences, optional improvements, etc.
#'
#' ## Backtracking Behavior
#'
#' When hard assertions fail:
#' 1. Feedback is generated from the failure messages
#' 2. The module is re-run with feedback injected as `assertion_feedback`
#' 3. This continues until assertions pass or max_retries is exceeded
#' 4. If max_retries exceeded, behavior depends on `on_failure` parameter
#'
#' ## Performance Considerations
#'
#' Each retry makes a new LLM call. Use assertions judiciously and consider:
#' - Starting with max_retries = 2-3 for most use cases
#' - Using "warn" for non-critical assertions to avoid blocking
#' - Combining with caching to reduce costs during development
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a QA module
#' qa <- module(signature("question -> answer"))
#'
#' # Wrap with assertions
#' validated <- with_assertions(
#'   qa,
#'   assertions = list(
#'     assert_output(~ nchar(.x$answer) <= 100, "Answer must be 100 chars or less"),
#'     assert_output(~ nchar(.x$answer) >= 10, "Answer must be at least 10 chars"),
#'     suggest_output(~ grepl("^[A-Z]", .x$answer), "Should start with capital")
#'   ),
#'   max_retries = 3
#' )
#'
#' # Run - will retry if assertions fail
#' result <- run(validated, question = "What is the capital of France?", .llm = llm)
#'
#' # Check attempt history
#' validated$get_attempts()
#' }
with_assertions <- function(
  module,
  assertions,
  max_retries = 3L,
  on_failure = c("error", "warn"),
  feedback_template = NULL,
  ...
) {
  on_failure <- match.arg(on_failure)

  AssertModule$new(
    module = module,
    assertions = assertions,
    max_retries = max_retries,
    on_failure = on_failure,
    feedback_template = feedback_template,
    ...
  )
}
