#' Wrapper Modules for Advanced Reasoning Patterns
#'
#' @description
#' R6 classes that wrap existing modules to provide advanced execution patterns
#' like retry with best-of-N selection and iterative refinement.
#'
#' @name module-wrapper
NULL

#' BestOfN Wrapper Module
#'
#' @description
#' An R6 class that wraps any Module to run it N times and return the best
#' result according to a reward function. Implements early stopping when
#' a result exceeds a threshold.
#'
#' @details
#' BestOfN provides a simple but effective way to improve reliability by
#' making multiple attempts and selecting the best result. This is particularly
#' useful when:
#' - The task has high variance in output quality
#' - A reward function can distinguish good from bad outputs
#' - You want to increase the probability of getting a correct answer
#'
#' The execution flow is:
#' 1. For each attempt (up to N):
#'    - Run the wrapped module
#'    - Score the result using the reward function
#'    - If score >= threshold, return immediately (early stopping)
#' 2. If no result meets threshold, return the best-scoring result
#'
#' @keywords internal
#' @noRd
BestOfNModule <- R6::R6Class(
  "BestOfNModule",
  inherit = Module,
  public = list(
    #' @field module The wrapped module
    module = NULL,

    #' @field N Maximum number of attempts
    N = NULL,

    #' @field reward_fn Reward function: function(prediction, inputs) -> [0, 1]
    reward_fn = NULL,

    #' @field threshold Score threshold for early stopping
    threshold = NULL,

    #' @field fail_count Maximum allowed consecutive failures
    fail_count = NULL,

    #' @description
    #' Initialize a BestOfN wrapper module
    #'
    #' @param module The module to wrap (must inherit from Module)
    #' @param N Maximum number of attempts (default 3)
    #' @param reward_fn Reward function: function(prediction, inputs) -> [0, 1]
    #' @param threshold Score threshold for early stopping (default 1.0)
    #' @param fail_count Maximum consecutive failures before giving up (default N)
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
        module,
        N = 3L,
        reward_fn = NULL,
        threshold = 1.0,
        fail_count = NULL,
        config = list(),
        chat = NULL) {
      # Validate module
      if (!inherits(module, "Module")) {
        cli::cli_abort(c(
          "module must be a Module object",
          "x" = "You provided: {.cls {class(module)[1]}}"
        ))
      }

      # Use module's signature
      super$initialize(
        signature = module$signature,
        config = config,
        chat = chat %||% module$chat
      )

      self$module <- module
      self$N <- as.integer(N)
      self$reward_fn <- reward_fn %||% default_reward_fn()
      self$threshold <- threshold
      self$fail_count <- fail_count %||% self$N

      # Store attempt history
      self$state$attempts <- list()
    },

    #' @description
    #' Execute the module N times and return best result
    #'
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param ... Additional arguments passed to wrapped module
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
      best_score <- -Inf
      best_result <- NULL
      best_metadata <- NULL
      best_chat <- NULL
      consecutive_failures <- 0
      total_tokens <- 0
      total_cost <- 0

      for (i in seq_len(self$N)) {
        # Run the wrapped module
        result <- tryCatch(
          {
            self$module$forward(batch, .llm = .llm, trace = FALSE, ...)
          },
          error = function(e) {
            consecutive_failures <<- consecutive_failures + 1
            if (consecutive_failures >= self$fail_count) {
              cli::cli_abort(c(
                "Too many consecutive failures in BestOfN",
                "x" = "Failed {consecutive_failures} times",
                "i" = "Last error: {e$message}"
              ))
            }
            NULL
          }
        )

        if (is.null(result)) {
          next
        }

        # Reset failure counter on success
        consecutive_failures <- 0

        # Extract prediction and metadata
        prediction <- result$output[[1]]
        metadata <- result$metadata[[1]]
        chat_obj <- result$chat[[1]]

        # Accumulate token counts and costs
        if (!is.null(metadata$total_tokens)) {
          total_tokens <- total_tokens + metadata$total_tokens
        }
        if (!is.null(metadata$cost) && !is.na(metadata$cost)) {
          total_cost <- total_cost + metadata$cost
        }

        # Score the result
        score <- tryCatch(
          {
            s <- self$reward_fn(prediction, inputs)
            if (is.logical(s)) as.numeric(s) else s
          },
          error = function(e) {
            cli::cli_warn(c(
              "Reward function failed for attempt {i}",
              "i" = e$message
            ))
            0.0
          }
        )

        # Record attempt
        attempt_entry <- list(
          attempt = i,
          prediction = prediction,
          score = score,
          metadata = metadata
        )
        attempts <- append(attempts, list(attempt_entry))

        # Track best result
        if (score > best_score) {
          best_score <- score
          best_result <- prediction
          best_metadata <- metadata
          best_chat <- chat_obj
        }

        # Early stopping if threshold met
        if (score >= self$threshold) {
          break
        }
      }

      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # If no successful attempts, error
      if (is.null(best_result)) {
        cli::cli_abort("All {self$N} attempts failed in BestOfN")
      }

      # Create aggregated metadata
      final_metadata <- list(
        timestamp = end_time,
        model = best_metadata$model,
        n_attempts = length(attempts),
        best_score = best_score,
        all_scores = vapply(attempts, function(a) a$score, numeric(1)),
        early_stopped = best_score >= self$threshold,
        total_tokens = total_tokens,
        total_cost = total_cost,
        latency_ms = latency_ms
      )

      # Record trace with all attempts
      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = inputs,
          output = best_result,
          attempts = attempts,
          n_attempts = length(attempts),
          best_score = best_score,
          threshold = self$threshold,
          model = best_metadata$model
        )
        self$state$traces <- append(self$state$traces, list(trace_entry))
        self$state$attempts <- append(self$state$attempts, list(attempts))
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
    #' @return A tibble of attempts with columns: run, attempt, prediction, score
    get_attempts = function(all = FALSE) {
      if (length(self$state$attempts) == 0) {
        return(tibble::tibble(
          run = integer(),
          attempt = integer(),
          prediction = list(),
          score = numeric()
        ))
      }

      if (all) {
        # All runs
        rows <- list()
        for (run_idx in seq_along(self$state$attempts)) {
          run_attempts <- self$state$attempts[[run_idx]]
          for (a in run_attempts) {
            rows <- append(rows, list(list(
              run = run_idx,
              attempt = a$attempt,
              prediction = list(a$prediction),
              score = a$score
            )))
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
            score = a$score
          )
        })
      }

      if (length(rows) == 0) {
        return(tibble::tibble(
          run = integer(),
          attempt = integer(),
          prediction = list(),
          score = numeric()
        ))
      }

      tibble::tibble(
        run = vapply(rows, function(r) r$run, integer(1)),
        attempt = vapply(rows, function(r) r$attempt, integer(1)),
        prediction = lapply(rows, function(r) r$prediction[[1]]),
        score = vapply(rows, function(r) r$score, numeric(1))
      )
    },

    #' @description
    #' Print the module
    print = function() {
      cli::cli_h2("BestOfNModule")
      cli::cli_text("Wrapped module: {.cls {class(self$module)[1]}}")
      cli::cli_text("N: {self$N}")
      cli::cli_text("Threshold: {self$threshold}")

      if (length(self$state$traces) > 0) {
        last_trace <- self$state$traces[[length(self$state$traces)]]
        cli::cli_h3("Last Run")
        cli::cli_text("  Attempts: {last_trace$n_attempts}")
        cli::cli_text("  Best score: {round(last_trace$best_score, 3)}")
      }

      invisible(self)
    },

    #' @description
    #' Create a reset copy of the module
    reset_copy = function() {
      BestOfNModule$new(
        module = self$module$reset_copy(),
        N = self$N,
        reward_fn = self$reward_fn,
        threshold = self$threshold,
        fail_count = self$fail_count,
        config = list(),
        chat = self$chat
      )
    },

    #' @description
    #' Apply optimization parameters
    apply_optimization_params = function(params) {
      # Pass through to wrapped module
      if (!is.null(params$N)) {
        self$N <- as.integer(params$N)
      }
      if (!is.null(params$threshold)) {
        self$threshold <- params$threshold
      }
      # Forward other params to wrapped module
      wrapped_params <- params[!names(params) %in% c("N", "threshold")]
      if (length(wrapped_params) > 0) {
        self$module$apply_optimization_params(wrapped_params)
      }
      invisible(self)
    }
  )
)

#' Create a BestOfN Wrapper Module
#'
#' @description
#' Factory function to create a BestOfN wrapper around any module.
#' Runs the module N times and returns the best result according to
#' a reward function.
#'
#' @param module A Module object to wrap
#' @param N Maximum number of attempts (default 3)
#' @param reward_fn Reward function with signature `function(prediction, inputs)`,
#'   returning a score between 0 and 1. Use `as_reward_fn()` to convert a metric.
#'   If NULL, uses a default that returns 1.0 for all valid predictions.
#' @param threshold Score threshold for early stopping (default 1.0)
#' @param fail_count Maximum consecutive failures before erroring (default N)
#' @param ... Additional arguments passed to the module constructor
#'
#' @return A BestOfNModule object
#'
#' @export
#' @examples
#' # Create a basic QA module
#' qa <- module(signature("question -> answer"))
#'
#' # Wrap it with best-of-3 selection
#' wrapper <- best_of_n(qa, N = 3)
#'
#' # With a custom reward function
#' one_word_reward <- function(pred, inputs) {
#'   words <- strsplit(as.character(pred$answer), "\\s+")[[1]]
#'   if (length(words) == 1) 1.0 else 0.0
#' }
#' wrapper <- best_of_n(qa, N = 5, reward_fn = one_word_reward)
#'
#' # Using a metric-based reward function
#' wrapper <- best_of_n(
#'   qa,
#'   N = 3,
#'   reward_fn = as_reward_fn(metric_exact_match(field = "answer")),
#'   threshold = 1.0
#' )
best_of_n <- function(
    module,
    N = 3L,
    reward_fn = NULL,
    threshold = 1.0,
    fail_count = NULL,
    ...) {
  BestOfNModule$new(
    module = module,
    N = N,
    reward_fn = reward_fn,
    threshold = threshold,
    fail_count = fail_count,
    ...
  )
}

#' Convert a Metric to a Reward Function
#'
#' @description
#' Adapts a metric function (with signature `function(prediction, expected)`)
#' to a reward function (with signature `function(prediction, inputs)`).
#'
#' @param metric A metric function created with `metric_*()` functions
#' @param expected_field Character. Name of the field in inputs that contains
#'   the expected value. Default is "expected".
#' @param prediction_field Character. If the prediction is a list, extract
#'   this field before comparing. If NULL, uses the whole prediction.
#'
#' @return A reward function suitable for `best_of_n()` and `refine()`
#'
#' @export
#' @examples
#' # Convert exact match metric to reward function
#' reward <- as_reward_fn(metric_exact_match(), expected_field = "answer")
#'
#' # With field extraction
#' reward <- as_reward_fn(
#'   metric_exact_match(field = "sentiment"),
#'   expected_field = "expected_sentiment"
#' )
as_reward_fn <- function(
    metric,
    expected_field = "expected",
    prediction_field = NULL) {
  if (!is.function(metric)) {
    cli::cli_abort("metric must be a function")
  }

  function(prediction, inputs) {
    # Get expected value from inputs
    expected <- inputs[[expected_field]]

    if (is.null(expected)) {
      cli::cli_warn(c(
        "No expected value found in inputs",
        "i" = "Looking for field: {.field {expected_field}}",
        "i" = "Available fields: {.field {names(inputs)}}"
      ))
      return(0.0)
    }

    # Extract prediction field if specified
    pred_value <- if (!is.null(prediction_field) && is.list(prediction)) {
      prediction[[prediction_field]]
    } else {
      prediction
    }

    # Call metric
    score <- metric(pred_value, expected)

    # Convert logical to numeric
    if (is.logical(score)) {
      as.numeric(score)
    } else {
      score
    }
  }
}

#' Default Reward Function
#'
#' @description
#' Creates a reward function that returns 1.0 for all valid (non-NULL)
#' predictions. Used as fallback when no reward function is specified.
#'
#' @return A reward function
#' @noRd
default_reward_fn <- function() {
  function(prediction, inputs) {
    if (is.null(prediction)) 0.0 else 1.0
  }
}

# ============================================================================
# RefineModule
# ============================================================================

#' Refine Wrapper Module
#'
#' @description
#' An R6 class that extends BestOfNModule with iterative refinement.
#' After each failed attempt, generates feedback that is injected into
#' subsequent attempts to guide improvement.
#'
#' @details
#' Refine builds on BestOfN by adding a feedback loop. When an attempt
#' doesn't meet the threshold, feedback is generated explaining what was
#' wrong, and this feedback is provided to the next attempt.
#'
#' This is particularly useful for:
#' - Tasks where the model can learn from mistakes
#' - Iterative improvement of complex outputs
#' - Cases where explicit feedback improves subsequent attempts
#'
#' The execution flow is:
#' 1. For each attempt (up to N):
#'    - If not first attempt, inject previous feedback into inputs
#'    - Run the wrapped module
#'    - Score the result
#'    - If score >= threshold, return immediately
#'    - Otherwise, generate feedback for next attempt
#' 2. If no result meets threshold, return the best-scoring result
#'
#' @keywords internal
#' @noRd
RefineModule <- R6::R6Class(
  "RefineModule",
  inherit = BestOfNModule,
  public = list(
    #' @field feedback_template Template for generating feedback
    feedback_template = NULL,

    #' @field feedback_field Name of the field to inject feedback into
    feedback_field = NULL,

    #' @description
    #' Initialize a Refine wrapper module
    #'
    #' @param module The module to wrap
    #' @param N Maximum number of attempts
    #' @param reward_fn Reward function
    #' @param threshold Score threshold for early stopping
    #' @param fail_count Maximum consecutive failures
    #' @param feedback_template Template for feedback generation. Uses glue
    #'   syntax with {score}, {prediction}, and any input field names available.
    #' @param feedback_field Name of the input field to inject feedback into.
    #'   If NULL (default), creates a new field called "feedback".
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
        module,
        N = 3L,
        reward_fn = NULL,
        threshold = 1.0,
        fail_count = NULL,
        feedback_template = NULL,
        feedback_field = "feedback",
        config = list(),
        chat = NULL) {
      super$initialize(
        module = module,
        N = N,
        reward_fn = reward_fn,
        threshold = threshold,
        fail_count = fail_count,
        config = config,
        chat = chat
      )

      self$feedback_template <- feedback_template %||%
        "Previous attempt scored {score}. The answer was: {prediction}. Please try again with improvements."
      self$feedback_field <- feedback_field

      # Store feedback history
      self$state$feedback_history <- list()
    },

    #' @description
    #' Execute the module with iterative refinement
    #'
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param ... Additional arguments passed to wrapped module
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
      feedback_history <- list()
      best_score <- -Inf
      best_result <- NULL
      best_metadata <- NULL
      best_chat <- NULL
      consecutive_failures <- 0
      total_tokens <- 0
      total_cost <- 0
      current_batch <- batch
      previous_feedback <- NULL

      for (i in seq_len(self$N)) {
        # Inject feedback from previous attempt (if any)
        if (i > 1 && !is.null(previous_feedback)) {
          current_batch <- private$inject_feedback(current_batch, previous_feedback)
        }

        # Run the wrapped module
        result <- tryCatch(
          {
            self$module$forward(current_batch, .llm = .llm, trace = FALSE, ...)
          },
          error = function(e) {
            consecutive_failures <<- consecutive_failures + 1
            if (consecutive_failures >= self$fail_count) {
              cli::cli_abort(c(
                "Too many consecutive failures in Refine",
                "x" = "Failed {consecutive_failures} times",
                "i" = "Last error: {e$message}"
              ))
            }
            NULL
          }
        )

        if (is.null(result)) {
          next
        }

        # Reset failure counter on success
        consecutive_failures <- 0

        # Extract prediction and metadata
        prediction <- result$output[[1]]
        metadata <- result$metadata[[1]]
        chat_obj <- result$chat[[1]]

        # Accumulate token counts and costs
        if (!is.null(metadata$total_tokens)) {
          total_tokens <- total_tokens + metadata$total_tokens
        }
        if (!is.null(metadata$cost) && !is.na(metadata$cost)) {
          total_cost <- total_cost + metadata$cost
        }

        # Score the result
        score <- tryCatch(
          {
            s <- self$reward_fn(prediction, inputs)
            if (is.logical(s)) as.numeric(s) else s
          },
          error = function(e) {
            cli::cli_warn(c(
              "Reward function failed for attempt {i}",
              "i" = e$message
            ))
            0.0
          }
        )

        # Record attempt
        attempt_entry <- list(
          attempt = i,
          prediction = prediction,
          score = score,
          metadata = metadata,
          feedback = previous_feedback
        )
        attempts <- append(attempts, list(attempt_entry))

        # Track best result
        if (score > best_score) {
          best_score <- score
          best_result <- prediction
          best_metadata <- metadata
          best_chat <- chat_obj
        }

        # Early stopping if threshold met
        if (score >= self$threshold) {
          break
        }

        # Generate feedback for next attempt (only if there will be one)
        if (i < self$N) {
          previous_feedback <- private$generate_feedback(inputs, prediction, score)
          feedback_history <- append(feedback_history, list(previous_feedback))
        }
      }

      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # If no successful attempts, error
      if (is.null(best_result)) {
        cli::cli_abort("All {self$N} attempts failed in Refine")
      }

      # Create aggregated metadata
      final_metadata <- list(
        timestamp = end_time,
        model = best_metadata$model,
        n_attempts = length(attempts),
        best_score = best_score,
        all_scores = vapply(attempts, function(a) a$score, numeric(1)),
        early_stopped = best_score >= self$threshold,
        feedback_count = length(feedback_history),
        total_tokens = total_tokens,
        total_cost = total_cost,
        latency_ms = latency_ms
      )

      # Record trace with all attempts and feedback
      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = inputs,
          output = best_result,
          attempts = attempts,
          feedback_history = feedback_history,
          n_attempts = length(attempts),
          best_score = best_score,
          threshold = self$threshold,
          model = best_metadata$model
        )
        self$state$traces <- append(self$state$traces, list(trace_entry))
        self$state$attempts <- append(self$state$attempts, list(attempts))
        self$state$feedback_history <- append(
          self$state$feedback_history,
          list(feedback_history)
        )
      }

      tibble::tibble(
        output = list(best_result),
        chat = list(best_chat),
        metadata = list(final_metadata)
      )
    },

    #' @description
    #' Get feedback history from last run or all runs
    #' @param all Logical. If TRUE, return all feedback across all runs
    #' @return Character vector of feedback messages
    get_feedback_history = function(all = FALSE) {
      if (length(self$state$feedback_history) == 0) {
        return(character())
      }

      if (all) {
        unlist(self$state$feedback_history)
      } else {
        unlist(self$state$feedback_history[[length(self$state$feedback_history)]])
      }
    },

    #' @description
    #' Print the module
    print = function() {
      cli::cli_h2("RefineModule")
      cli::cli_text("Wrapped module: {.cls {class(self$module)[1]}}")
      cli::cli_text("N: {self$N}")
      cli::cli_text("Threshold: {self$threshold}")
      cli::cli_text("Feedback field: {self$feedback_field}")

      if (length(self$state$traces) > 0) {
        last_trace <- self$state$traces[[length(self$state$traces)]]
        cli::cli_h3("Last Run")
        cli::cli_text("  Attempts: {last_trace$n_attempts}")
        cli::cli_text("  Best score: {round(last_trace$best_score, 3)}")
        cli::cli_text("  Feedback rounds: {length(last_trace$feedback_history)}")
      }

      invisible(self)
    },

    #' @description
    #' Create a reset copy of the module
    reset_copy = function() {
      RefineModule$new(
        module = self$module$reset_copy(),
        N = self$N,
        reward_fn = self$reward_fn,
        threshold = self$threshold,
        fail_count = self$fail_count,
        feedback_template = self$feedback_template,
        feedback_field = self$feedback_field,
        config = list(),
        chat = self$chat
      )
    }
  ),

  private = list(
    #' Generate feedback for the next attempt
    generate_feedback = function(inputs, prediction, score) {
      # Format prediction for template
      pred_str <- if (is.list(prediction)) {
        paste(
          vapply(
            names(prediction),
            function(n) paste0(n, ": ", prediction[[n]]),
            character(1)
          ),
          collapse = "; "
        )
      } else {
        as.character(prediction)
      }

      # Build template data
      template_data <- c(
        inputs,
        list(
          score = round(score, 3),
          prediction = pred_str
        )
      )

      # Generate feedback using glue
      tryCatch(
        {
          glue::glue_data(
            template_data,
            self$feedback_template,
            .open = "{",
            .close = "}"
          )
        },
        error = function(e) {
          cli::cli_warn(c(
            "Failed to generate feedback from template",
            "i" = e$message
          ))
          paste0("Previous attempt scored ", round(score, 3), ". Please try again.")
        }
      )
    },

    #' Inject feedback into the batch
    inject_feedback = function(batch, feedback) {
      if (is.data.frame(batch)) {
        batch[[self$feedback_field]] <- feedback
      } else {
        batch[[self$feedback_field]] <- feedback
      }
      batch
    }
  )
)

#' Create a Refine Wrapper Module
#'
#' @description
#' Factory function to create a Refine wrapper around any module.
#' Extends BestOfN with iterative refinement using feedback.
#'
#' @param module A Module object to wrap
#' @param N Maximum number of attempts (default 3)
#' @param reward_fn Reward function with signature `function(prediction, inputs)`,
#'   returning a score between 0 and 1
#' @param threshold Score threshold for early stopping (default 1.0)
#' @param fail_count Maximum consecutive failures before erroring (default N)
#' @param feedback_template Template for generating feedback. Uses glue syntax
#'   with available variables: `\{score\}`, `\{prediction\}`, and input field names.
#' @param feedback_field Name of the input field to inject feedback into
#' @param ... Additional arguments passed to the module constructor
#'
#' @return A RefineModule object
#'
#' @export
#' @examples
#' # Create a QA module
#' qa <- module(signature("question, feedback -> answer"))
#'
#' # Wrap with refinement
#' one_word_reward <- function(pred, inputs) {
#'   words <- strsplit(as.character(pred$answer), "\\s+")[[1]]
#'   if (length(words) == 1) 1.0 else 0.0
#' }
#'
#' refined <- refine(
#'   qa,
#'   N = 3,
#'   reward_fn = one_word_reward,
#'   feedback_template = "Score: {score}. Your answer '{prediction}' was too long. Give a single word."
#' )
refine <- function(
    module,
    N = 3L,
    reward_fn = NULL,
    threshold = 1.0,
    fail_count = NULL,
    feedback_template = NULL,
    feedback_field = "feedback",
    ...) {
  RefineModule$new(
    module = module,
    N = N,
    reward_fn = reward_fn,
    threshold = threshold,
    fail_count = fail_count,
    feedback_template = feedback_template,
    feedback_field = feedback_field,
    ...
  )
}
