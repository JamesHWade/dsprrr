#' Ensemble Module for Combining Multiple Modules
#'
#' @description
#' R6 class that wraps multiple modules and combines their outputs using
#' a reduce function. Useful for ensemble methods where multiple optimized
#' modules vote on the final answer.
#'
#' @name module-ensemble
NULL

#' Ensemble Module Class
#'
#' @description
#' An R6 class that runs multiple modules on the same input and combines
#' their outputs using a configurable reduce function.
#'
#' @details
#' EnsembleModule provides ensemble-style prediction by:
#' 1. Running all wrapped modules on the same input
#' 2. Collecting all outputs
#' 3. Applying a reduce function to combine outputs into a single result
#'
#' This is particularly useful for:
#' - Combining top candidates from optimization runs
#' - Majority voting across differently-optimized modules
#' - Weighted voting based on validation performance
#'
#' @keywords internal
#' @noRd
EnsembleModule <- R6::R6Class(
  "EnsembleModule",
  inherit = Module,
  public = list(
    #' @field modules List of modules to run
    modules = NULL,

    #' @field reduce_fn Function to combine outputs
    reduce_fn = NULL,

    #' @field weights Optional weights for weighted voting
    weights = NULL,

    #' @description
    #' Initialize an Ensemble module
    #'
    #' @param modules List of Module objects to combine
    #' @param reduce_fn Function to combine outputs. Should accept a list of
    #'   outputs and optional weights, returning a single combined output.
    #' @param weights Optional numeric vector of weights for each module
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(
      modules,
      reduce_fn = NULL,
      weights = NULL,
      config = list(),
      chat = NULL
    ) {
      # Validate modules
      if (!is.list(modules) || length(modules) == 0) {
        cli::cli_abort("modules must be a non-empty list")
      }

      for (i in seq_along(modules)) {
        if (!inherits(modules[[i]], "Module")) {
          cli::cli_abort(c(
            "All modules must be Module objects",
            "x" = "Element {i} is: {.cls {class(modules[[i]])[1]}}"
          ))
        }
      }

      # Validate signature compatibility across all modules
      validate_signature_compatibility(modules)

      # Use the first module's signature as the ensemble signature
      sig <- modules[[1]]$signature

      super$initialize(
        signature = sig,
        config = config,
        chat = chat
      )

      self$modules <- modules
      self$reduce_fn <- reduce_fn %||% reduce_majority()

      # Validate and store weights
      if (!is.null(weights)) {
        if (length(weights) != length(modules)) {
          cli::cli_abort(c(
            "weights must have same length as modules",
            "x" = "Got {length(weights)} weights for {length(modules)} modules"
          ))
        }
        self$weights <- as.numeric(weights)
      } else {
        self$weights <- rep(1, length(modules))
      }

      # Store individual results for inspection
      self$state$individual_results <- list()
    },

    #' @description
    #' Execute all modules and combine their outputs
    #'
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param ... Additional arguments passed to wrapped modules
    #' @return Tibble with output, chat, metadata columns
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      # Handle both list and data frame inputs
      if (is.data.frame(batch)) {
        inputs <- as.list(batch[1, , drop = FALSE])
      } else {
        inputs <- batch
      }

      start_time <- Sys.time()
      outputs <- list()
      chats <- list()
      individual_metadata <- list()
      successful_indices <- integer(0) # Track which modules succeeded
      total_tokens <- 0
      total_cost <- 0
      n_errors <- 0

      # Run all modules
      for (i in seq_along(self$modules)) {
        result <- tryCatch(
          {
            self$modules[[i]]$forward(batch, .llm = .llm, trace = FALSE, ...)
          },
          error = function(e) {
            n_errors <<- n_errors + 1
            cli::cli_warn(c(
              "Module {i} failed in Ensemble",
              "x" = e$message
            ))
            NULL
          }
        )

        if (!is.null(result)) {
          outputs[[length(outputs) + 1]] <- result$output[[1]]
          chats[[length(chats) + 1]] <- result$chat[[1]]
          successful_indices <- c(successful_indices, i) # Track this module's index

          metadata <- result$metadata[[1]]
          individual_metadata[[length(individual_metadata) + 1]] <- metadata

          # Accumulate costs
          if (!is.null(metadata$total_tokens)) {
            total_tokens <- total_tokens + metadata$total_tokens
          }
          if (!is.null(metadata$cost) && !is.na(metadata$cost)) {
            total_cost <- total_cost + metadata$cost
          }
        }
      }

      # Check we have at least one output
      if (length(outputs) == 0) {
        cli::cli_abort(
          "All {length(self$modules)} modules failed in Ensemble"
        )
      }

      # Get corresponding weights for successful modules using tracked indices
      successful_weights <- self$weights[successful_indices]

      # Apply reduce function
      combined_output <- tryCatch(
        {
          self$reduce_fn(outputs, weights = successful_weights)
        },
        error = function(e) {
          cli::cli_abort(c(
            "Reduce function failed in Ensemble",
            "x" = e$message,
            "i" = "Received {length(outputs)} outputs to combine"
          ))
        }
      )

      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # Get model from first successful result
      model <- if (length(individual_metadata) > 0) {
        individual_metadata[[1]]$model
      } else {
        NA_character_
      }

      # Create aggregated metadata
      final_metadata <- list(
        timestamp = end_time,
        model = model,
        n_modules = length(self$modules),
        n_successful = length(outputs),
        n_errors = n_errors,
        n_llm_calls = length(outputs),
        total_tokens = total_tokens,
        total_cost = total_cost,
        latency_ms = latency_ms
      )

      # Record trace
      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = inputs,
          output = combined_output,
          individual_outputs = outputs,
          n_modules = length(self$modules),
          n_successful = length(outputs),
          n_errors = n_errors,
          model = model
        )
        self$state$traces <- append(self$state$traces, list(trace_entry))
        self$state$individual_results <- append(
          self$state$individual_results,
          list(list(
            outputs = outputs,
            metadata = individual_metadata,
            chats = chats
          ))
        )
      }

      # Get first chat if available, otherwise NULL
      first_chat <- if (length(chats) > 0) chats[[1]] else NULL

      tibble::tibble(
        output = list(combined_output),
        chat = list(first_chat),
        metadata = list(final_metadata)
      )
    },

    #' @description
    #' Get individual outputs from last run or all runs
    #' @param all Logical. If TRUE, return all outputs across all runs
    #' @return A tibble of individual module outputs
    get_individual_outputs = function(all = FALSE) {
      if (length(self$state$individual_results) == 0) {
        return(tibble::tibble(
          run = integer(),
          module = integer(),
          output = list()
        ))
      }

      if (all) {
        rows <- list()
        for (run_idx in seq_along(self$state$individual_results)) {
          run_results <- self$state$individual_results[[run_idx]]
          for (mod_idx in seq_along(run_results$outputs)) {
            rows <- append(
              rows,
              list(list(
                run = run_idx,
                module = mod_idx,
                output = list(run_results$outputs[[mod_idx]])
              ))
            )
          }
        }
      } else {
        last_run <- self$state$individual_results[[length(
          self$state$individual_results
        )]]
        rows <- lapply(seq_along(last_run$outputs), function(mod_idx) {
          list(
            run = length(self$state$individual_results),
            module = mod_idx,
            output = list(last_run$outputs[[mod_idx]])
          )
        })
      }

      if (length(rows) == 0) {
        return(tibble::tibble(
          run = integer(),
          module = integer(),
          output = list()
        ))
      }

      tibble::tibble(
        run = vapply(rows, function(r) r$run, integer(1)),
        module = vapply(rows, function(r) r$module, integer(1)),
        output = lapply(rows, function(r) r$output[[1]])
      )
    },

    #' @description
    #' Print the module
    print = function() {
      cli::cli_h2("EnsembleModule")
      cli::cli_text("Modules: {length(self$modules)}")
      cli::cli_text("Reduce function: {.fn {deparse(self$reduce_fn)[1]}}")

      if (!is.null(self$weights) && !all(self$weights == 1)) {
        cli::cli_text(
          "Weights: {paste(round(self$weights, 3), collapse = ', ')}"
        )
      }

      if (length(self$state$traces) > 0) {
        last_trace <- self$state$traces[[length(self$state$traces)]]
        cli::cli_h3("Last Run")
        cli::cli_text("  Successful modules: {last_trace$n_successful}")
        cli::cli_text("  Errors: {last_trace$n_errors}")
      }

      invisible(self)
    },

    #' @description
    #' Create a reset copy of the module
    reset_copy = function() {
      EnsembleModule$new(
        modules = lapply(self$modules, function(m) m$reset_copy()),
        reduce_fn = self$reduce_fn,
        weights = self$weights,
        config = list(),
        chat = self$chat
      )
    },

    #' @description
    #' Apply optimization parameters (forwards to all wrapped modules)
    apply_optimization_params = function(params) {
      # Forward params to all modules
      for (mod in self$modules) {
        mod$apply_optimization_params(params)
      }
      invisible(self)
    }
  )
)

#' Create an Ensemble Module
#'
#' @description
#' Factory function to create an Ensemble module that combines multiple
#' modules using a reduce function.
#'
#' @param modules A list of Module objects to combine
#' @param reduce_fn Function to combine outputs. Default is `reduce_majority()`.
#'   Should accept a list of outputs and optional weights parameter.
#' @param weights Optional numeric vector of weights for each module.
#'   Useful for weighted voting based on validation performance.
#' @param ... Additional arguments passed to module constructor
#'
#' @return An EnsembleModule object
#'
#' @name ensemble_module
#' @export
#' @examples
#' # Create multiple compiled modules
#' sig <- signature("question -> answer")
#' mod1 <- module(sig, type = "predict")
#' mod2 <- module(sig, type = "predict")
#' mod3 <- module(sig, type = "predict")
#'
#' # Combine with majority voting
#' ens <- ensemble(list(mod1, mod2, mod3))
#'
#' # With weighted voting based on validation scores
#' ens <- ensemble(
#'   list(mod1, mod2, mod3),
#'   reduce_fn = reduce_weighted_vote(),
#'   weights = c(0.9, 0.85, 0.8)
#' )
#'
#' # With custom reduce function
#' custom_reduce <- function(outputs, weights = NULL) {
#'   # Take the longest answer
#'   lengths <- vapply(outputs, function(o) nchar(o$answer %||% ""), integer(1))
#'   outputs[[which.max(lengths)]]
#' }
#' ens <- ensemble(list(mod1, mod2, mod3), reduce_fn = custom_reduce)
ensemble <- function(
  modules,
  reduce_fn = NULL,
  weights = NULL,
  ...
) {
  EnsembleModule$new(
    modules = modules,
    reduce_fn = reduce_fn,
    weights = weights,
    ...
  )
}

# ============================================================================
# Helper Functions
# ============================================================================

#' Validate signature compatibility across modules
#'
#' @param modules List of modules to validate
#' @return NULL invisibly (throws error if incompatible)
#' @noRd
validate_signature_compatibility <- function(modules) {
  if (length(modules) < 2) {
    return(invisible(NULL))
  }

  # Extract input field names from first module's signature
  reference_sig <- modules[[1]]$signature
  reference_inputs <- get_signature_input_names(reference_sig)

  # Compare all other modules to the reference
  for (i in seq_along(modules)[-1]) {
    current_sig <- modules[[i]]$signature
    current_inputs <- get_signature_input_names(current_sig)

    # Check input field names match exactly
    if (!identical(reference_inputs, current_inputs)) {
      cli::cli_abort(c(
        "Incompatible signatures in EnsembleModule",
        "x" = "Module 1 expects inputs: {.field {reference_inputs}}",
        "x" = "Module {i} expects inputs: {.field {current_inputs}}",
        "i" = "All modules must have the same input field names"
      ))
    }
  }

  invisible(NULL)
}

#' Extract input field names from a signature
#'
#' @param sig A Signature object, or NULL. Non-Signature values return
#'   an empty character vector with a warning.
#' @return Character vector of input field names. Returns `character(0)` if
#'   `sig` is NULL or not a Signature object.
#' @noRd
get_signature_input_names <- function(sig) {
  if (is.null(sig)) {
    cli::cli_warn(c(
      "Module has NULL signature",
      "i" = "This may indicate a corrupted module state"
    ))
    return(character(0))
  }

  if (!S7::S7_inherits(sig, Signature)) {
    cli::cli_warn(c(
      "Module signature is not a Signature object",
      "x" = "Got: {.cls {class(sig)[1]}}",
      "i" = "This may indicate a corrupted module state"
    ))
    return(character(0))
  }

  vapply(
    sig@inputs,
    function(inp) inp$name %||% "",
    character(1)
  )
}

# ============================================================================
# Reducer Functions
# ============================================================================

#' Majority Vote Reducer
#'
#' @description
#' Creates a reduce function that returns the most common output among
#' the ensemble members. For structured outputs (lists), compares by
#' the first field or a specified field.
#'
#' @param field Optional field name to use for voting when outputs are lists.
#'   If NULL, uses the first field of the output.
#' @param tie_breaker How to handle ties: "first" (default) returns the first
#'   occurrence, "random" picks randomly among tied values.
#'
#' @return A reduce function for use with `ensemble()`
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic majority voting
#' ens <- ensemble(modules, reduce_fn = reduce_majority())
#'
#' # Vote based on specific field
#' ens <- ensemble(modules, reduce_fn = reduce_majority(field = "sentiment"))
#' }
reduce_majority <- function(field = NULL, tie_breaker = "first") {
  tie_breaker <- match.arg(tie_breaker, c("first", "random"))

  function(outputs, weights = NULL) {
    if (length(outputs) == 0) {
      cli::cli_abort("Cannot reduce empty list of outputs")
    }

    if (length(outputs) == 1) {
      return(outputs[[1]])
    }

    # Extract values to vote on
    values <- vapply(
      outputs,
      function(o) {
        if (is.list(o) && !is.null(names(o))) {
          # Structured output - use specified field or first field
          if (!is.null(field)) {
            as.character(o[[field]] %||% NA_character_)
          } else {
            as.character(o[[1]] %||% NA_character_)
          }
        } else {
          as.character(o)
        }
      },
      character(1)
    )

    # Apply weights if provided (round to get vote counts)
    if (!is.null(weights)) {
      # Repeat values according to weights (normalized to sum to n)
      weights <- weights / sum(weights) * length(weights)
      vote_counts <- tapply(weights, values, sum)
    } else {
      vote_counts <- table(values)
    }

    # Find winner
    max_votes <- max(vote_counts)
    winners <- names(vote_counts)[vote_counts == max_votes]

    if (length(winners) == 1) {
      winner <- winners
    } else {
      # Tie - use tie breaker
      winner <- if (tie_breaker == "random") {
        sample(winners, 1)
      } else {
        # Return first occurrence
        winners[which.min(match(winners, values))]
      }
    }

    # Return the full output that has this winning value
    winner_idx <- which(values == winner)[1]
    outputs[[winner_idx]]
  }
}

#' Weighted Vote Reducer
#'
#' @description
#' Creates a reduce function that uses weighted voting, where each module's
#' vote is weighted by its weight (typically validation score).
#'
#' @param field Optional field name to use for voting when outputs are lists.
#'   If NULL, uses the first field of the output.
#'
#' @return A reduce function for use with `ensemble()`
#'
#' @export
#' @examples
#' \dontrun{
#' # Weighted voting with validation scores
#' ens <- ensemble(
#'   modules,
#'   reduce_fn = reduce_weighted_vote(),
#'   weights = c(0.9, 0.85, 0.75)
#' )
#' }
reduce_weighted_vote <- function(field = NULL) {
  function(outputs, weights = NULL) {
    if (length(outputs) == 0) {
      cli::cli_abort("Cannot reduce empty list of outputs")
    }

    if (is.null(weights)) {
      weights <- rep(1, length(outputs))
    }

    if (length(outputs) == 1) {
      return(outputs[[1]])
    }

    # Extract values to vote on
    values <- vapply(
      outputs,
      function(o) {
        if (is.list(o) && !is.null(names(o))) {
          if (!is.null(field)) {
            as.character(o[[field]] %||% NA_character_)
          } else {
            as.character(o[[1]] %||% NA_character_)
          }
        } else {
          as.character(o)
        }
      },
      character(1)
    )

    # Calculate weighted votes
    vote_totals <- tapply(weights, values, sum)

    # Return output with highest weighted vote
    winner <- names(which.max(vote_totals))
    winner_idx <- which(values == winner)[1]
    outputs[[winner_idx]]
  }
}

#' First Successful Output Reducer
#'
#' @description
#' Creates a reduce function that simply returns the first successful output.
#' Useful when you want to try multiple modules but just need one answer.
#'
#' @return A reduce function for use with `ensemble()`
#'
#' @export
#' @examples
#' \dontrun{
#' ens <- ensemble(modules, reduce_fn = reduce_first())
#' }
reduce_first <- function() {
  function(outputs, weights = NULL) {
    if (length(outputs) == 0) {
      cli::cli_abort("Cannot reduce empty list of outputs")
    }
    outputs[[1]]
  }
}

#' Best by Metric Reducer
#'
#' @description
#' Creates a reduce function that scores each output using a metric function
#' and returns the best-scoring output. Requires expected value to be set
#' via the `set_expected` attribute before calling.
#'
#' @param metric A metric function created with `metric_*()` functions
#' @param maximize If TRUE (default), return highest-scoring output.
#'   If FALSE, return lowest-scoring output.
#'
#' @return A reduce function for use with `ensemble()`
#'
#' @export
#' @examples
#' \dontrun{
#' # Score each output and return best
#' ens <- ensemble(
#'   modules,
#'   reduce_fn = reduce_best_by_metric(
#'     metric = metric_exact_match(field = "answer")
#'   )
#' )
#' }
reduce_best_by_metric <- function(
  metric,
  maximize = TRUE
) {
  if (!is.function(metric)) {
    cli::cli_abort("metric must be a function")
  }

  # Use an environment to store the expected value
  state <- new.env(parent = emptyenv())
  state$expected_value <- NULL

  reducer <- function(outputs, weights = NULL) {
    if (length(outputs) == 0) {
      cli::cli_abort("Cannot reduce empty list of outputs")
    }

    if (length(outputs) == 1) {
      return(outputs[[1]])
    }

    if (is.null(state$expected_value)) {
      cli::cli_abort(c(
        "No expected value set for reduce_best_by_metric",
        "i" = "This reducer requires an expected value to score outputs",
        "i" = "Set expected value via {.code attr(reducer, 'set_expected')(value)} before calling",
        "i" = "If you don't have expected values, use {.fn reduce_majority} or {.fn reduce_first} instead"
      ))
    }

    # Score each output
    n_scoring_errors <- 0
    scores <- vapply(
      seq_along(outputs),
      function(i) {
        tryCatch(
          {
            as.numeric(metric(outputs[[i]], state$expected_value))
          },
          error = function(e) {
            n_scoring_errors <<- n_scoring_errors + 1
            cli::cli_warn(c(
              "Metric scoring failed for output {i}",
              "x" = e$message
            ))
            NA_real_
          }
        )
      },
      numeric(1)
    )

    # Find best score
    best_fn <- if (maximize) which.max else which.min
    best_idx <- best_fn(scores)

    if (is.na(best_idx) || length(best_idx) == 0) {
      cli::cli_abort(c(
        "All metric scores are NA in reduce_best_by_metric",
        "x" = "The metric function failed to score any of the {length(outputs)} output{?s}",
        "i" = "{n_scoring_errors} scoring error{?s} occurred during evaluation",
        "i" = "Verify that the metric function is compatible with the output format",
        "i" = "Check that the expected value format matches what the metric expects"
      ))
    }

    outputs[[best_idx]]
  }

  # Add a setter for expected value using the shared environment
  attr(reducer, "set_expected") <- function(value) {
    state$expected_value <- value
  }

  class(reducer) <- c("metric_reducer", "function")
  reducer
}
