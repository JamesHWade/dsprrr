#' Wrapper modules: best-of-N selection and iterative refinement
#'
#' The user-facing constructors are [best_of_n()] and [refine()].
#'
#' @name module-wrapper
#' @noRd
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

    #' @field reward_fn Reward function returning a score between 0 and 1
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
    #' @param reward_fn Reward function returning a score between 0 and 1
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
      chat = NULL
    ) {
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
    #' Inputs the wrapped module fills in itself.
    #' @return A character vector.
    supplied_inputs = function() {
      self$module$supplied_inputs()
    },

    #' @description
    #' Execute the module N times and return best result
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
      best_score <- -Inf
      best_result <- NULL
      best_metadata <- NULL
      best_chat <- NULL
      first_result <- NULL # Fallback if all scores are NA
      first_metadata <- NULL
      first_chat <- NULL
      consecutive_failures <- 0
      n_errors <- 0L

      for (i in seq_len(self$N)) {
        # Run the wrapped module. rollout_id partitions the cache per attempt so
        # attempts 2..N are not served identical cached responses (which would
        # make BestOfN explore nothing when caching is enabled). compose_*
        # folds in any inherited id so nesting stays unique and consistent.
        result <- tryCatch(
          {
            self$module$forward(
              batch,
              .llm = .llm,
              trace = FALSE,
              rollout_id = compose_rollout_id(rollout_id, i),
              ...
            )
          },
          error = function(e) {
            consecutive_failures <<- consecutive_failures + 1
            n_errors <<- n_errors + 1L
            cli::cli_warn(c(
              "Attempt {i} of {self$N} failed in BestOfN",
              "x" = e$message,
              "i" = "Consecutive failures: {consecutive_failures}/{self$fail_count}"
            ))
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

        # Keep first successful result as fallback (in case all scores are NA)
        if (is.null(first_result)) {
          first_result <- prediction
          first_metadata <- metadata
          first_chat <- chat_obj
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
              "x" = "Treating as unscored (will not be selected as best)",
              "i" = e$message
            ))
            NA_real_
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

        # Track best result (NA scores are not eligible for best)
        if (!is.na(score) && score > best_score) {
          best_score <- score
          best_result <- prediction
          best_metadata <- metadata
          best_chat <- chat_obj
        }

        # Early stopping if threshold met (NA scores don't trigger early stop)
        if (!is.na(score) && score >= self$threshold) {
          break
        }
      }

      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # If no successful attempts at all, error
      if (is.null(first_result)) {
        cli::cli_abort("All {self$N} attempts failed in BestOfN")
      }

      # Use first_result as fallback if all scores were NA
      if (is.null(best_result)) {
        best_result <- first_result
        best_metadata <- first_metadata
        best_chat <- first_chat
        # best_score stays -Inf since no valid scores
      }

      usage <- aggregate_module_usage_metadata(
        lapply(attempts, function(attempt) attempt$metadata),
        unknown_attempt = n_errors > 0L
      )

      # Create aggregated metadata
      final_metadata <- list(
        timestamp = end_time,
        model = best_metadata$model,
        n_attempts = length(attempts),
        best_score = best_score,
        all_scores = vapply(attempts, function(a) a$score, numeric(1)),
        early_stopped = !is.infinite(best_score) &&
          best_score >= self$threshold,
        total_tokens = usage$total_tokens,
        cost = usage$cost,
        provider_calls = usage$provider_calls,
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
            rows <- append(
              rows,
              list(list(
                run = run_idx,
                attempt = a$attempt,
                prediction = list(a$prediction),
                score = a$score
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
      artifact_copy_runtime(
        self,
        BestOfNModule$new(
          module = self$module$reset_copy(),
          N = self$N,
          reward_fn = self$reward_fn,
          threshold = self$threshold,
          fail_count = self$fail_count,
          config = list(),
          chat = self$chat
        )
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

#' Run a module up to N times and keep the best result
#'
#' @description
#' `best_of_n()` wraps a module so that each [run()] calls it up to `N` times,
#' scores every prediction with `reward_fn`, and returns the best one. It
#' stops early as soon as a prediction scores at least `threshold`.
#'
#' @param module The module to wrap.
#' @param N Maximum number of attempts.
#' @param reward_fn A function called as `reward_fn(prediction, inputs)`,
#'   where `prediction` is the attempt's output (a named list) and `inputs`
#'   are the inputs given to [run()]. It returns a score, usually between 0
#'   and 1; logical values become 0 or 1. The default gives 1 to every
#'   prediction, so with the default `threshold` the first attempt that
#'   succeeds is returned and `N` only limits retries after errors.
#'   [as_reward_fn()] turns a metric into a reward function.
#' @param threshold Score at which to stop early. Default 1.
#' @param fail_count Number of consecutive failed attempts after which to
#'   give up with an error. Defaults to `N`.
#' @param ... Passed to the wrapper: `chat` (an ellmer Chat, which defaults
#'   to the wrapped module's chat) or `config`.
#'
#' @details
#' Each attempt uses its own partition of the response cache, so attempts
#' get fresh responses even when caching is on. A failed attempt gives a
#' warning and is skipped. If a reward function errors or returns `NA`, that
#' attempt cannot be chosen; if no attempt has a score, the first successful
#' prediction is returned.
#'
#' The returned module has a `get_attempts()` method that lists the attempts
#' of the last run (or of all runs, with `all = TRUE`) with their scores.
#' With `.return_format = "structured"`, the metadata also records
#' `n_attempts`, `best_score`, `all_scores` and `early_stopped`.
#'
#' @return A module (an R6 object of class `BestOfNModule`) with the same
#'   signature as `module`.
#'
#' @export
#' @family composition
#' @examples
#' # An offline stand-in for a model that phrases its answer differently
#' # each time
#' answerer <- module_fn(
#'   "question -> answer",
#'   function(question) sample(c("Paris", "It is Paris", "The capital is Paris"), 1)
#' )
#' one_word <- function(prediction, inputs) {
#'   length(strsplit(prediction$answer, " ")[[1]]) == 1
#' }
#'
#' set.seed(5)
#' best <- best_of_n(answerer, N = 5L, reward_fn = one_word)
#' run(best, question = "What is the capital of France?")
#' best$get_attempts()
#'
#' \dontrun{
#' qa <- module(signature("question -> answer"))
#' concise <- best_of_n(qa, N = 3L, reward_fn = one_word)
#' run(
#'   concise,
#'   question = "What is the capital of France?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' }
best_of_n <- function(
  module,
  N = 3L,
  reward_fn = NULL,
  threshold = 1.0,
  fail_count = NULL,
  ...
) {
  BestOfNModule$new(
    module = module,
    N = N,
    reward_fn = reward_fn,
    threshold = threshold,
    fail_count = fail_count,
    ...
  )
}

#' Turn a metric into a reward function
#'
#' @description
#' `as_reward_fn()` adapts a metric, called as `metric(prediction, expected)`,
#' into a reward function for [best_of_n()] and [refine()], called as
#' `reward(prediction, inputs)`. The expected value is read from the inputs
#' under `expected_field`.
#'
#' @param metric A metric, such as `metric_exact_match()`. The metric
#'   receives `prediction_field` of the prediction and the bare expected
#'   value, so do not give it a `field` of its own.
#' @param expected_field Name of the input that holds the expected value.
#' @param prediction_field Name of the prediction field to compare. With
#'   `NULL`, the whole prediction is passed to the metric.
#'
#' @details
#' Because the expected value travels with the inputs of [run()], the
#' wrapped module receives it too, and [run()] warns that it is not declared
#' in the signature. A module without a custom `template` writes every input
#' into its prompt, so it would show the expected answer to the model. Give
#' the wrapped module a `template` that leaves that field out, as in the
#' example. If the input is missing, the reward is 0 with a warning.
#'
#' @return A function `function(prediction, inputs)` returning a numeric
#'   score.
#'
#' @export
#' @family composition
#' @examples
#' reward <- as_reward_fn(
#'   metric_exact_match(ignore_case = TRUE),
#'   expected_field = "expected",
#'   prediction_field = "answer"
#' )
#' reward(list(answer = "paris"), list(question = "Capital?", expected = "Paris"))
#' reward(list(answer = "Lyon"), list(question = "Capital?", expected = "Paris"))
#'
#' \dontrun{
#' qa <- module(
#'   signature("question -> answer"),
#'   template = "Question: {question}\nReply with a single word."
#' )
#' checked <- best_of_n(qa, N = 3L, reward_fn = reward)
#' run(
#'   checked,
#'   question = "What is the capital of France?",
#'   expected = "Paris",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' }
as_reward_fn <- function(
  metric,
  expected_field = "expected",
  prediction_field = NULL
) {
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
      chat = NULL
    ) {
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
    #' Inputs filled in by the wrapper or the wrapped module. The feedback
    #' field is supplied by Refine, so callers never pass it.
    #' @return A character vector.
    supplied_inputs = function() {
      union(super$supplied_inputs(), self$feedback_field)
    },

    #' @description
    #' Execute the module with iterative refinement
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
      feedback_history <- list()
      best_score <- -Inf
      best_result <- NULL
      best_metadata <- NULL
      best_chat <- NULL
      first_result <- NULL # Fallback if all scores are NA
      first_metadata <- NULL
      first_chat <- NULL
      consecutive_failures <- 0
      n_errors <- 0L
      current_batch <- batch
      previous_feedback <- NULL

      # A wrapped module that declares the feedback field needs a value on the
      # first attempt too, before any feedback exists.
      wrapped_inputs <- vapply(
        self$module$signature@inputs,
        function(x) x$name,
        character(1)
      )
      if (
        self$feedback_field %in%
          wrapped_inputs &&
          is.null(current_batch[[self$feedback_field]])
      ) {
        current_batch <- private$inject_feedback(
          current_batch,
          "No feedback yet."
        )
      }

      for (i in seq_len(self$N)) {
        # Inject feedback from previous attempt (if any)
        if (i > 1 && !is.null(previous_feedback)) {
          current_batch <- private$inject_feedback(
            current_batch,
            previous_feedback
          )
        }

        # Run the wrapped module. rollout_id partitions the cache per attempt so
        # retries are not served identical cached responses even before feedback
        # changes the prompt. compose_* folds in any inherited id for nesting.
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
            consecutive_failures <<- consecutive_failures + 1
            n_errors <<- n_errors + 1L
            cli::cli_warn(c(
              "Attempt {i} of {self$N} failed in Refine",
              "x" = e$message,
              "i" = "Consecutive failures: {consecutive_failures}/{self$fail_count}"
            ))
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

        # Keep first successful result as fallback (in case all scores are NA)
        if (is.null(first_result)) {
          first_result <- prediction
          first_metadata <- metadata
          first_chat <- chat_obj
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
              "x" = "Treating as unscored (will not be selected as best)",
              "i" = e$message
            ))
            NA_real_
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

        # Track best result (NA scores are not eligible for best)
        if (!is.na(score) && score > best_score) {
          best_score <- score
          best_result <- prediction
          best_metadata <- metadata
          best_chat <- chat_obj
        }

        # Early stopping if threshold met (NA scores don't trigger early stop)
        if (!is.na(score) && score >= self$threshold) {
          break
        }

        # Generate feedback for next attempt (only if there will be one)
        if (i < self$N) {
          previous_feedback <- private$generate_feedback(
            inputs,
            prediction,
            score
          )
          feedback_history <- append(feedback_history, list(previous_feedback))
        }
      }

      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # If no successful attempts at all, error
      if (is.null(first_result)) {
        cli::cli_abort("All {self$N} attempts failed in Refine")
      }

      # Use first_result as fallback if all scores were NA
      if (is.null(best_result)) {
        best_result <- first_result
        best_metadata <- first_metadata
        best_chat <- first_chat
        # best_score stays -Inf since no valid scores
      }

      usage <- aggregate_module_usage_metadata(
        lapply(attempts, function(attempt) attempt$metadata),
        unknown_attempt = n_errors > 0L
      )

      # Create aggregated metadata
      final_metadata <- list(
        timestamp = end_time,
        model = best_metadata$model,
        n_attempts = length(attempts),
        best_score = best_score,
        all_scores = vapply(attempts, function(a) a$score, numeric(1)),
        early_stopped = !is.infinite(best_score) &&
          best_score >= self$threshold,
        feedback_count = length(feedback_history),
        total_tokens = usage$total_tokens,
        cost = usage$cost,
        provider_calls = usage$provider_calls,
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
        unlist(self$state$feedback_history[[length(
          self$state$feedback_history
        )]])
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
        cli::cli_text(
          "  Feedback rounds: {length(last_trace$feedback_history)}"
        )
      }

      invisible(self)
    },

    #' @description
    #' Create a reset copy of the module
    reset_copy = function() {
      artifact_copy_runtime(
        self,
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
          paste0(
            "Previous attempt scored ",
            round(score, 3),
            ". Please try again."
          )
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

#' Retry a module with feedback until it scores well
#'
#' @description
#' `refine()` works like [best_of_n()], but after an attempt scores below
#' `threshold` it passes feedback about that attempt into the next one. The
#' feedback is the `feedback_template` filled in with the attempt's score,
#' its prediction and the inputs; it says only what the template says.
#'
#' @inheritParams best_of_n
#' @param reward_fn A function called as `reward_fn(prediction, inputs)`,
#'   returning a score, usually between 0 and 1. The default gives 1 to every
#'   prediction, so pass a real reward function.
#' @param feedback_template A glue template for the feedback. It can use
#'   `{score}` (rounded to 3 digits), `{prediction}` (the output fields
#'   written as `field: value`, separated by `; `) and any input field. The
#'   default is "Previous attempt scored \{score\}. The answer was:
#'   \{prediction\}. Please try again with improvements."
#' @param feedback_field Name of the input that carries the feedback.
#'
#' @details
#' If the wrapped module's signature declares `feedback_field` as an input,
#' the first attempt receives "No feedback yet." and later attempts receive
#' the filled-in template; callers never pass it to [run()]. If the signature
#' does not declare it, the feedback is still passed to the wrapped module: a
#' prediction module without a custom template then adds it to the prompt as
#' an extra `feedback:` line on retries.
#'
#' The returned module has `get_attempts()` and `get_feedback_history()`
#' methods for the last run (or all runs, with `all = TRUE`).
#'
#' @return A module (an R6 object of class `RefineModule`) with the same
#'   signature as `module`.
#'
#' @export
#' @family composition
#' @examples
#' one_word <- function(prediction, inputs) {
#'   length(strsplit(prediction$answer, " ")[[1]]) == 1
#' }
#'
#' # An offline stand-in for a model that shortens its answer once it gets
#' # feedback
#' drafter <- module_fn(
#'   "question, feedback -> answer",
#'   function(question, feedback) {
#'     if (feedback == "No feedback yet.") "The capital of France is Paris." else "Paris"
#'   }
#' )
#' refined <- refine(
#'   drafter,
#'   N = 3L,
#'   reward_fn = one_word,
#'   feedback_template = "Your answer '{prediction}' scored {score}. Reply with one word."
#' )
#' run(refined, question = "What is the capital of France?")
#' refined$get_feedback_history()
#'
#' \dontrun{
#' qa <- module(signature("question, feedback -> answer"))
#' refined <- refine(
#'   qa,
#'   N = 3L,
#'   reward_fn = one_word,
#'   feedback_template = "Your answer '{prediction}' was too long. Give a single word."
#' )
#' run(
#'   refined,
#'   question = "What is the capital of France?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' }
refine <- function(
  module,
  N = 3L,
  reward_fn = NULL,
  threshold = 1.0,
  fail_count = NULL,
  feedback_template = NULL,
  feedback_field = "feedback",
  ...
) {
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
