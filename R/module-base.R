#' R6 Module Base Class
#'
#' @description
#' Base class for all DSPrrr modules. Provides core functionality for
#' stateful LLM modules with signatures, configuration, and mutable state.
#'
#' @keywords internal
#' @noRd
Module <- R6::R6Class(
  "Module",
  public = list(
    #' @field signature Immutable S7 Signature object defining the module interface
    signature = NULL,

    #' @field config Mutable list of configuration parameters
    config = NULL,

    #' @field state Mutable list for module state (traces, cache, etc.)
    state = NULL,

    #' @description
    #' Initialize a new Module
    #' @param signature S7 Signature object
    #' @param config Optional list of configuration parameters
    initialize = function(signature, config = list()) {
      if (!inherits(signature, "dsprrr::Signature")) {
        cli::cli_abort("signature must be a Signature object")
      }

      self$signature <- signature
      self$config <- config
      self$state <- list(
        traces = list(),
        cache = list(),
        compiled = FALSE,
        optimization_history = list()
      )
    },

    #' @description
    #' Execute the module with given inputs
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param ... Additional arguments passed to implementation
    #' @return Tibble with output, trace, metadata columns
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      cli::cli_abort("forward() must be implemented by subclass")
    },

    #' @description
    #' Run the module with inputs (delegates to forward)
    #' @param ... Named inputs matching the signature
    #' @param .llm Optional ellmer chat object
    #' @param .verbose Logical for debug output
    #' @param .parallel Logical for parallel batch processing
    #' @param .progress Logical for progress bar
    #' @param .return_format Either "simple" or "structured"
    #' @return Module outputs
    run = function(..., .llm = NULL, .verbose = FALSE, .parallel = FALSE,
                   .progress = TRUE, .return_format = "simple") {
      inputs <- list(...)

      # Validate inputs against signature
      if (length(self$signature@inputs) > 0) {
        required_names <- vapply(self$signature@inputs, function(x) x$name, character(1))
        missing_inputs <- setdiff(required_names, names(inputs))

        if (length(missing_inputs) > 0) {
          cli::cli_abort("Missing required inputs: {.field {missing_inputs}}")
        }
      }

      # Delegate to forward for now - full batch logic will be migrated later
      result <- self$forward(inputs, .llm = .llm, trace = TRUE)

      if (.return_format == "simple") {
        return(result$output[[1]])
      } else {
        return(result)
      }
    },

    #' @description
    #' Optimize the module using a development set
    #' @param devset Development dataset
    #' @param objective Metric or metric set
    #' @param control Optimization control parameters
    #' @return Updated module (self)
    optimize = function(devset, objective, control = list()) {
      cli::cli_inform("Base optimize() - subclasses should override")
      invisible(self)
    },

    #' @description
    #' Reset module state
    #' @param hard Logical; if TRUE, also reset config
    #' @return Updated module (self)
    reset = function(hard = FALSE) {
      self$state <- list(
        traces = list(),
        cache = list(),
        compiled = FALSE,
        optimization_history = list()
      )

      if (hard) {
        # Reset config to defaults while preserving signature
        self$config <- list()
      }

      invisible(self)
    },

    #' @description
    #' Get the most recent ellmer chat object
    #' @return The ellmer chat object from the last run, or NULL
    get_last_chat = function() {
      if (length(self$state$traces) > 0) {
        last_trace <- self$state$traces[[length(self$state$traces)]]
        # Return the stored chat if we have it
        if (!is.null(last_trace$chat)) {
          return(last_trace$chat)
        }
      }
      return(NULL)
    },

    #' @description
    #' Get traces as a tidy tibble
    #' @return Tibble with one row per trace
    get_traces = function() {
      traces <- self$state$traces

      if (length(traces) == 0) {
        return(tibble::tibble(
          timestamp = .POSIXct(numeric(0)),
          latency_ms = numeric(0),
          input_tokens = integer(0),
          output_tokens = integer(0),
          total_tokens = integer(0),
          cost = numeric(0),
          model = character(0),
          prompt_length = integer(0)
        ))
      }

      # Convert list of traces to tibble
      tibble::tibble(
        timestamp = vapply(traces, function(x) x$timestamp, .POSIXct(1)),
        latency_ms = vapply(traces, function(x) x$latency_ms %||% NA_real_, numeric(1)),
        input_tokens = vapply(traces, function(x) {
          if (is.list(x$tokens)) as.integer(x$tokens$input_tokens %||% NA) else NA_integer_
        }, integer(1)),
        output_tokens = vapply(traces, function(x) {
          if (is.list(x$tokens)) as.integer(x$tokens$output_tokens %||% NA) else NA_integer_
        }, integer(1)),
        total_tokens = vapply(traces, function(x) {
          if (is.list(x$tokens)) as.integer(x$tokens$total_tokens %||% NA) else NA_integer_
        }, integer(1)),
        cost = vapply(traces, function(x) x$cost %||% NA_real_, numeric(1)),
        model = vapply(traces, function(x) x$model %||% NA_character_, character(1)),
        prompt_length = vapply(traces, function(x) {
          if (!is.null(x$prompt)) nchar(x$prompt) else NA_integer_
        }, integer(1))
      )
    },

    #' @description
    #' Get trace summary
    #' @return List with trace statistics
    trace_summary = function() {
      traces <- self$state$traces

      if (length(traces) == 0) {
        return(list(
          n_traces = 0,
          total_tokens = 0,
          total_latency = 0
        ))
      }

      # Extract metrics from traces
      list(
        n_traces = length(traces),
        total_input_tokens = sum(vapply(traces, function(x) {
          if (is.list(x$tokens)) x$tokens$input_tokens %||% 0 else 0
        }, numeric(1))),
        total_output_tokens = sum(vapply(traces, function(x) {
          if (is.list(x$tokens)) x$tokens$output_tokens %||% 0 else 0
        }, numeric(1))),
        total_tokens = sum(vapply(traces, function(x) {
          if (is.list(x$tokens)) x$tokens$total_tokens %||% 0 else 0
        }, numeric(1))),
        total_latency_ms = sum(vapply(traces, function(x) x$latency_ms %||% 0, numeric(1))),
        total_cost = sum(vapply(traces, function(x) x$cost %||% 0, numeric(1)))
      )
    },

    #' @description
    #' Check if module is compiled/optimized
    #' @return Logical
    is_compiled = function() {
      isTRUE(self$state$compiled)
    },

    #' @description
    #' Create a vitals-compatible solver function
    #' @param .llm Optional ellmer chat object
    #' @param .parallel Logical for parallel processing
    #' @param .return_format Either "simple" or "structured"
    #' @param ... Additional arguments
    #' @return Function compatible with vitals Tasks
    as_vitals_solver = function(.llm = NULL, .parallel = FALSE,
                                .return_format = "structured", ...) {
      module <- self

      function(inputs, ...) {
        if (!is.data.frame(inputs)) {
          inputs <- as.data.frame(inputs)
        }

        # This will use the full run_dataset logic once migrated
        results <- tibble::tibble(
          result = list(),
          .chat = list(),
          .metadata = list()
        )

        # Process each row
        for (i in seq_len(nrow(inputs))) {
          row_inputs <- as.list(inputs[i, , drop = FALSE])
          result <- module$forward(row_inputs, .llm = .llm, trace = TRUE)

          results$result[[i]] <- result$output[[1]]
          results$.chat[[i]] <- result$chat[[1]] %||% NULL
          results$.metadata[[i]] <- result$metadata[[1]] %||% list()
        }

        if (.return_format == "simple") {
          list(
            result = results$result,
            solver_chat = replicate(nrow(results), NULL, simplify = FALSE),
            metadata = replicate(nrow(results), list(), simplify = FALSE)
          )
        } else {
          list(
            result = results$result,
            solver_chat = results$.chat,
            metadata = results$.metadata
          )
        }
      }
    },

    #' @description
    #' Print the module
    print = function() {
      cli::cli_h2("Module")

      cli::cli_h3("Signature")
      print(self$signature)

      if (length(self$config) > 0) {
        cli::cli_h3("Configuration")
        config_names <- names(self$config)
        if (length(config_names) > 5) {
          cli::cli_text("  {length(config_names)} settings configured")
        } else {
          for (name in config_names) {
            cli::cli_text("  {name}: {self$config[[name]]}")
          }
        }
      }

      if (self$is_compiled()) {
        cli::cli_h3("Compilation Status")
        cli::cli_text("{cli::symbol$tick} Compiled")
        if (!is.null(self$state$best_score)) {
          cli::cli_text("  Best score: {round(self$state$best_score, 3)}")
        }
      }

      trace_summary <- self$trace_summary()
      if (trace_summary$n_traces > 0) {
        cli::cli_h3("Traces")
        cli::cli_text("  {trace_summary$n_traces} trace(s) recorded")
        cli::cli_text("  Total tokens: {trace_summary$total_tokens} (in: {trace_summary$total_input_tokens}, out: {trace_summary$total_output_tokens})")
        if (!is.na(trace_summary$total_cost) && trace_summary$total_cost > 0) {
          cli::cli_text("  Total cost: ${format(trace_summary$total_cost, digits = 4)}")
        }
        cli::cli_text("  Total latency: {round(trace_summary$total_latency_ms / 1000, 2)}s")
      }

      invisible(self)
    }
  ),

  private = list(
    # Build prompt from inputs (to be overridden by subclasses)
    build_prompt = function(inputs) {
      cli::cli_abort("build_prompt() must be implemented by subclass")
    },

    # Postprocess model output (to be overridden by subclasses)
    postprocess = function(output) {
      output
    }
  )
)

#' Null-coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x