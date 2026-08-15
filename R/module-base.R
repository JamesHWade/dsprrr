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

    #' @field chat Optional ellmer Chat object for LLM operations
    chat = NULL,

    #' @description
    #' Initialize a new Module
    #' @param signature S7 Signature object
    #' @param config Optional list of configuration parameters
    #' @param chat Optional ellmer Chat object
    initialize = function(signature, config = list(), chat = NULL) {
      if (!inherits(signature, "dsprrr::Signature")) {
        cli::cli_abort("signature must be a Signature object")
      }

      if (!is.null(chat) && !inherits(chat, "Chat")) {
        cli::cli_abort("chat must be an ellmer Chat object")
      }

      self$signature <- signature
      self$config <- normalize_module_config(config)
      self$chat <- chat
      self$state <- list(
        traces = list(),
        cache = list(),
        compiled = FALSE,
        optimization_history = list(),
        trials = tibble::tibble(),
        last_grid = tibble::tibble(),
        best_score = NULL,
        best_params = NULL,
        best_trial = NULL
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
    #' Run the module with inputs
    #'
    #' This method provides a convenient interface for executing modules directly.
    #' For batch processing with parallel execution support, use the `run()` generic
    #' function instead: `run(module, ...)`.
    #'
    #' @param ... Named inputs matching the signature
    #' @param .llm Optional ellmer chat object
    #' @param .verbose Logical for debug output (currently unused, for API consistency)
    #' @param .parallel Logical for parallel batch processing (requires using run() generic)
    #' @param .parallel_method Legacy parallel backend passed through by [run()]
    #' @param .concurrency Optional policy created by [concurrency_control()]
    #' @param .progress Logical for progress bar (currently unused, for API consistency)
    #' @param .return_format Either "simple" or "structured"
    #' @param .trace_context A named JSON-compatible correlation context
    #' @return Module outputs. For .return_format="simple", returns the output value directly.
    #'   For "structured", returns a tibble with output, chat, and metadata columns.
    run = function(
      ...,
      .llm = NULL,
      .verbose = FALSE,
      .parallel = FALSE,
      .parallel_method = c("ellmer", "mirai"),
      .concurrency = NULL,
      .concurrency_runtime = NULL,
      .progress = TRUE,
      .return_format = "simple",
      .cache = NULL,
      .predict_compat = FALSE,
      .trace_context = list()
    ) {
      trace_context_supplied <- !missing(.trace_context)
      trace_context <- trace_context_resolve(
        .trace_context,
        supplied = trace_context_supplied
      )
      trace_cursor <- evaluation_trace_cursor(self)
      previous_trace_context <- trace_context_enter(
        trace_context,
        program = self,
        inherit_program_id = !trace_context_supplied
      )
      invocation_trace_fields <- trace_context_fields()
      on.exit(
        {
          trace_context_restore(previous_trace_context)
          trace_context_annotate_module_traces(
            self,
            trace_cursor,
            fields = invocation_trace_fields
          )
        },
        add = TRUE
      )

      parallel_missing <- missing(.parallel)
      parallel_method_missing <- missing(.parallel_method)
      concurrency_missing <- missing(.concurrency)
      if (is.null(.concurrency_runtime)) {
        concurrency <- resolve_concurrency_control(
          .concurrency = .concurrency,
          concurrency_missing = concurrency_missing,
          .parallel = .parallel,
          parallel_missing = parallel_missing,
          .parallel_method = .parallel_method,
          parallel_method_missing = parallel_method_missing
        )
      } else if (
        !inherits(
          .concurrency_runtime,
          "dsprrr_concurrency_runtime"
        )
      ) {
        cli::cli_abort(
          "Internal concurrency runtime is invalid",
          class = "dsprrr_concurrency_config_error"
        )
      }
      # Validate .cache here too: callers can reach $run() directly (not only
      # via the run() generic, which validates separately), so a malformed
      # value must fail loudly instead of being silently forwarded.
      validate_cache_arg(.cache)
      .return_format <- match.arg(.return_format, c("simple", "structured"))
      if (
        !is.logical(.predict_compat) ||
          length(.predict_compat) != 1L ||
          is.na(.predict_compat)
      ) {
        cli::cli_abort(
          "Internal predict compatibility mode is invalid",
          class = "dsprrr_runtime_config_error"
        )
      }

      inputs <- list(...)

      # Validate inputs against signature
      validate_signature_inputs(
        self$signature,
        inputs,
        missing = if (inherits(self, "FlexModule")) "ignore" else "error",
        extra = if (inherits(self, "FlexModule")) "error" else "warn",
        type = if (inherits(self, "FlexModule")) "error" else "warn",
        context = "inputs"
      )

      input_contract <- module_input_contract(self, inputs)

      if (identical(input_contract$kind, "batch")) {
        if (interpreter_workflow_module(self)) {
          assert_factory_interpreter_async_supported(self, "run")
          runtime_chat <- .llm %||%
            self$chat %||%
            get_default_chat(create = FALSE)
          runtime <- .concurrency_runtime %||%
            normalize_concurrency_runtime(
              concurrency,
              .llm = .llm,
              .chat = runtime_chat
            )
          runtime <- normalize_factory_interpreter_batch_runtime(
            runtime,
            explicit_llm = !is.null(.llm)
          )
          if (
            isTRUE(.predict_compat) &&
              !identical(runtime$effective_backend, "sequential")
          ) {
            cli::cli_abort(
              c(
                "Stateful predict batches require sequential execution",
                "i" = "Use {.fn run} for isolated concurrent batch execution."
              ),
              class = c(
                "dsprrr_predict_concurrency_unsupported",
                "dsprrr_concurrency_unsupported_error"
              )
            )
          }
          inputs <- lapply(
            inputs,
            function(value) {
              module_batch_recycle_input(self, value, input_contract$size)
            }
          )
          return(trace_context_annotate_result(
            run_factory_interpreter_batch(
              module = self,
              inputs = inputs,
              n = input_contract$size,
              .llm = .llm,
              .progress = .progress,
              .return_format = .return_format,
              .concurrency = runtime,
              .cache = .cache
            ),
            fields = invocation_trace_fields
          ))
        }
        if (
          inherits(self, "PredictModule") &&
            !identical(class(self)[1], "PredictModule")
        ) {
          cli::cli_abort(
            c(
              "Batch execution is not yet supported for specialized Predict modules",
              "x" = "{.cls {class(self)[1]}} overrides the row execution contract.",
              "i" = "Run scalar inputs so the module's specialized {.fn forward} method is preserved."
            ),
            class = "dsprrr_batch_unsupported_module",
            module_class = class(self)[1]
          )
        }
        if (!identical(class(self)[1], "PredictModule")) {
          cli::cli_abort(
            c(
              "Batch execution is not supported for this module",
              "x" = "{.cls {class(self)[1]}} does not implement the isolated Predict row contract.",
              "i" = "Run scalar inputs or implement a dedicated isolated row adapter."
            ),
            class = "dsprrr_batch_unsupported_module",
            module_class = class(self)[1]
          )
        }
        runtime_chat <- .llm %||%
          self$chat %||%
          get_default_chat(create = FALSE)
        runtime <- .concurrency_runtime %||%
          normalize_concurrency_runtime(
            concurrency,
            .llm = .llm,
            .chat = runtime_chat
          )
        if (
          isTRUE(.predict_compat) &&
            !identical(runtime$effective_backend, "sequential")
        ) {
          cli::cli_abort(
            c(
              "Stateful predict batches require sequential execution",
              "i" = "Use {.fn run} for isolated concurrent batch execution."
            ),
            class = c(
              "dsprrr_predict_concurrency_unsupported",
              "dsprrr_concurrency_unsupported_error"
            )
          )
        }
      }

      if (identical(input_contract$kind, "empty")) {
        return(trace_context_annotate_result(
          empty_batch_result(.return_format),
          fields = invocation_trace_fields
        ))
      }

      # Exact Predict modules share the same scheduler whether callers use the
      # generic or the public R6 method. This avoids mutating one caller Chat
      # across rows and commits canonical traces in deterministic order.
      if (identical(input_contract$kind, "batch")) {
        inputs <- lapply(
          inputs,
          batch_recycle_input,
          size = input_contract$size
        )
        return(trace_context_annotate_result(
          run_batch(
            module = self,
            inputs = inputs,
            n = input_contract$size,
            .llm = .llm,
            .verbose = .verbose,
            .progress = .progress,
            .return_format = .return_format,
            .cache = .cache,
            .concurrency = runtime,
            .isolate_rows = !isTRUE(.predict_compat)
          ),
          fields = invocation_trace_fields
        ))
      }

      # Single input processing
      result <- self$forward(inputs, .llm = .llm, trace = TRUE, .cache = .cache)

      if (.return_format == "simple") {
        return(result$output[[1]])
      } else {
        return(trace_context_annotate_result(
          structure(
            list(
              output = result$output[[1]],
              chat = result$chat[[1]],
              metadata = result$metadata[[1]]
            ),
            class = "dsprrr_result"
          ),
          fields = invocation_trace_fields
        ))
      }
    },

    #' @description
    #' Predict using the module
    #'
    #' Convenience method that delegates to `run()`. Provides a familiar interface
    #' for users coming from tidymodels or other prediction frameworks.
    #'
    #' @param ... Named inputs matching the signature
    #' @param .llm Optional ellmer chat object (uses stored chat if not provided)
    #' @return The declared output record, or a list of output records for a
    #'   batch. Single-field object outputs retain their field name.
    predict = function(..., .llm = NULL) {
      result <- self$run(
        ...,
        .llm = .llm,
        .return_format = "structured",
        .predict_compat = TRUE
      )
      if (inherits(result, "dsprrr_batch_result")) {
        return(lapply(result, `[[`, "output"))
      }
      result$output
    },

    #' @description
    #' Optimize the module using development data
    #' @param data Development data as a data frame or tibble
    #' @param objective Metric or metric set
    #' @param control Optimization control parameters
    #' @return Updated module (self)
    optimize = function(
      data,
      metric = metric_exact_match(),
      grid = NULL,
      parameters = NULL,
      objective = c("maximize", "minimize"),
      .llm = NULL,
      control = list(),
      ...
    ) {
      self$optimize_grid(
        data = data,
        metric = metric,
        grid = grid,
        parameters = parameters,
        objective = objective,
        .llm = .llm,
        control = control,
        ...
      )
    },

    #' @description
    #' Run grid search optimisation for the module
    #' @param data Development data as data frame or tibble
    #' @param metric Metric function applied per example
    #' @param grid Candidate configurations as data frame (optional)
    #' @param parameters Parameter definitions (named list or dials param set)
    #' @param objective Optimisation direction ("maximize" or "minimize")
    #' @param .llm Optional ellmer chat object reused during optimisation
    #' @param control List of control options (progress, grid_type, etc.)
    #' @param ... Additional arguments forwarded to [evaluate()]
    optimize_grid = function(
      data,
      metric = metric_exact_match(),
      grid = NULL,
      parameters = NULL,
      objective = c("maximize", "minimize"),
      .llm = NULL,
      control = list(),
      ...
    ) {
      if (!is.data.frame(data)) {
        cli::cli_abort(c(
          "{.arg data} must be a data frame or tibble",
          "x" = "Got {.cls {class(data)[1]}}",
          "i" = "Provide a data frame with columns matching your signature inputs"
        ))
      }

      if (nrow(data) == 0) {
        cli::cli_abort(c(
          "{.arg data} must contain at least one row",
          "i" = "Optimization requires at least one example to evaluate"
        ))
      }

      if (!is.function(metric)) {
        cli::cli_abort(c(
          "{.arg metric} must be a function",
          "x" = "Got {.cls {class(metric)[1]}}",
          "i" = "Use a built-in metric: {.code metric_exact_match()}, {.code metric_contains()}",
          "i" = "Or wrap yardstick metrics: {.code as_dsprrr_metric(yardstick::accuracy)}"
        ))
      }

      objective <- match.arg(objective)
      control <- merge_optimization_control(control)
      candidate_grid <- prepare_candidate_grid(
        parameters = parameters,
        grid = grid,
        control = control
      )

      n_candidates <- nrow(candidate_grid)
      if (n_candidates == 0) {
        cli::cli_abort(c(
          "The optimization grid is empty",
          "i" = "Provide {.arg parameters}: {.code list(temperature = c(0.3, 0.7, 1.0))}",
          "i" = "Or provide a {.arg grid} data frame with parameter columns"
        ))
      }

      progress_id <- NULL
      if (isTRUE(control$progress) && n_candidates > 1) {
        progress_id <- cli::cli_progress_bar(
          format = "Optimizing {cli::pb_current}/{cli::pb_total} | Score: {msg_score}",
          total = n_candidates
        )
      }

      trial_ids <- seq_len(n_candidates)
      scores <- rep(NA_real_, n_candidates)
      n_evaluated <- integer(n_candidates)
      n_errors <- integer(n_candidates)
      total_costs <- rep(NA_real_, n_candidates)
      parameters_col <- vector("list", n_candidates)
      evaluations <- vector("list", n_candidates)
      timestamps <- rep(
        as.POSIXct(NA_real_, origin = "1970-01-01"),
        n_candidates
      )

      best_idx <- NA_integer_
      best_score <- if (objective == "maximize") -Inf else Inf

      for (i in seq_len(n_candidates)) {
        params <- as.list(candidate_grid[i, , drop = FALSE])
        parameters_col[[i]] <- params

        candidate <- self$copy(deep = TRUE)
        candidate$config <- utils::modifyList(
          candidate$config,
          params,
          keep.null = TRUE
        )
        if (is.function(candidate$apply_optimization_params)) {
          candidate$apply_optimization_params(params)
        }

        eval_result <- evaluate(
          candidate,
          data = data,
          metric = metric,
          .llm = .llm,
          .parallel = control$parallel,
          .progress = control$evaluation_progress,
          ...
        )

        evaluations[[i]] <- eval_result
        scores[i] <- eval_result$mean_score
        n_evaluated[i] <- eval_result$n_evaluated
        n_errors[i] <- eval_result$n_errors
        total_costs[i] <- eval_result$total_cost %||% NA_real_
        timestamps[i] <- Sys.time()

        if (!is.na(scores[i])) {
          if (is.na(best_idx)) {
            best_idx <- i
            best_score <- scores[i]
          } else if (objective == "maximize" && scores[i] > best_score) {
            best_idx <- i
            best_score <- scores[i]
          } else if (objective == "minimize" && scores[i] < best_score) {
            best_idx <- i
            best_score <- scores[i]
          }
        }

        if (!is.null(progress_id)) {
          msg_score <- if (is.na(scores[i])) {
            "NA"
          } else {
            format(round(scores[i], 4), nsmall = 4)
          }
          cli::cli_progress_update(
            id = progress_id,
            set = i
          )
        }
      }

      if (!is.null(progress_id)) {
        cli::cli_progress_done(id = progress_id)
      }

      trials_tbl <- tibble::tibble(
        trial_id = trial_ids,
        parameters = parameters_col,
        score = scores,
        n_evaluated = n_evaluated,
        n_errors = n_errors,
        total_cost = total_costs,
        evaluation = evaluations,
        timestamp = timestamps
      )

      self$state$trials <- trials_tbl
      self$state$optimization_history <- append(
        self$state$optimization_history,
        list(trials_tbl)
      )
      self$state$last_grid <- candidate_grid

      if (!is.na(best_idx)) {
        best_params <- parameters_col[[best_idx]]
        self$config <- utils::modifyList(
          self$config,
          best_params,
          keep.null = TRUE
        )
        self$state$best_score <- scores[best_idx]
        self$state$best_params <- best_params
        self$state$best_trial <- best_idx
        self$state$compiled <- TRUE
        if (is.function(self$apply_optimization_params)) {
          self$apply_optimization_params(best_params)
        }
      } else {
        cli::cli_warn(
          "No valid scores produced during optimisation; configuration left unchanged"
        )
      }

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
        optimization_history = list(),
        trials = tibble::tibble(),
        last_grid = tibble::tibble(),
        best_score = NULL,
        best_params = NULL,
        best_trial = NULL
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
          cached_input_tokens = integer(0),
          output_tokens = integer(0),
          total_tokens = integer(0),
          cost = numeric(0),
          model = character(0),
          prompt_length = integer(0),
          prompt = character(0),
          response = character(0),
          program_artifact_id = character(0),
          trace_context = list()
        ))
      }

      # Convert list of traces to tibble
      tibble::tibble(
        timestamp = vapply(traces, function(x) x$timestamp, .POSIXct(1)),
        latency_ms = vapply(traces, trace_latency_ms, numeric(1)),
        input_tokens = vapply(
          traces,
          function(x) trace_tokens(x)$input_tokens,
          integer(1)
        ),
        cached_input_tokens = vapply(
          traces,
          function(x) trace_tokens(x)$cached_input_tokens,
          integer(1)
        ),
        output_tokens = vapply(
          traces,
          function(x) trace_tokens(x)$output_tokens,
          integer(1)
        ),
        total_tokens = vapply(
          traces,
          function(x) trace_tokens(x)$total_tokens,
          integer(1)
        ),
        cost = vapply(traces, trace_cost, numeric(1)),
        model = vapply(
          traces,
          function(x) x$model %||% NA_character_,
          character(1)
        ),
        prompt_length = vapply(
          traces,
          function(x) {
            prompt <- trace_prompt_text(x)
            if (!is.na(prompt)) nchar(prompt) else NA_integer_
          },
          integer(1)
        ),
        prompt = vapply(traces, trace_prompt_text, character(1)),
        response = vapply(traces, trace_response_text, character(1)),
        program_artifact_id = vapply(
          traces,
          function(trace) {
            id <- trace$program_artifact_id %||% NA_character_
            if (
              !is.character(id) ||
                length(id) != 1L ||
                is.na(id)
            ) {
              return(NA_character_)
            }
            id
          },
          character(1)
        ),
        trace_context = lapply(
          traces,
          function(trace) trace$trace_context %||% list()
        )
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
          total_latency_ms = 0,
          total_cost = 0
        ))
      }

      all_tokens <- vapply(
        traces,
        function(trace) {
          tokens <- trace_tokens(trace)
          c(
            input = tokens$input_tokens %||% 0,
            output = tokens$output_tokens %||% 0,
            cached = tokens$cached_input_tokens %||% 0
          )
        },
        numeric(3)
      )

      list(
        n_traces = length(traces),
        total_input_tokens = sum(all_tokens["input", ], na.rm = TRUE),
        total_output_tokens = sum(all_tokens["output", ], na.rm = TRUE),
        total_cached_tokens = sum(all_tokens["cached", ], na.rm = TRUE),
        total_tokens = sum(all_tokens["input", ], na.rm = TRUE) +
          sum(all_tokens["output", ], na.rm = TRUE),
        total_duration_s = sum(
          vapply(
            traces,
            function(x) {
              duration <- tryCatch(
                x$assistant_turn@duration,
                error = function(e) NULL
              )
              duration %||% ((trace_latency_ms(x) %||% 0) / 1000)
            },
            numeric(1)
          ),
          na.rm = TRUE
        ),
        total_latency_ms = sum(
          vapply(traces, trace_latency_ms, numeric(1)),
          na.rm = TRUE
        ),
        total_cost = sum_cost_values(vapply(traces, trace_cost, numeric(1)))
      )
    },

    #' @description
    #' Get total cost for this module's LLM calls
    #' @return Numeric total cost in USD
    get_total_cost = function() {
      self$trace_summary()$total_cost
    },

    #' @description
    #' Get cost summary with per-call breakdown
    #' @return A tibble with timestamp, model, tokens, and cost per call
    get_cost_summary = function() {
      traces <- self$get_traces()
      if (nrow(traces) == 0) {
        return(tibble::tibble(
          timestamp = .POSIXct(numeric(0)),
          model = character(0),
          input_tokens = integer(0),
          output_tokens = integer(0),
          cost = numeric(0)
        ))
      }
      tibble::tibble(
        timestamp = traces$timestamp,
        model = traces$model,
        input_tokens = traces$input_tokens,
        output_tokens = traces$output_tokens,
        cost = traces$cost
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
    as_vitals_solver = function(
      .llm = NULL,
      .parallel = FALSE,
      .return_format = "structured",
      ...
    ) {
      module <- self

      function(inputs, ...) {
        if (!is.data.frame(inputs)) {
          inputs <- as.data.frame(inputs)
        }

        # This will use the full run_dataset logic once migrated
        results <- tibble::tibble(
          result = vector("list", nrow(inputs)),
          .chat = vector("list", nrow(inputs)),
          .metadata = vector("list", nrow(inputs))
        )

        # Process each row
        for (i in seq_len(nrow(inputs))) {
          row_inputs <- as.list(inputs[i, , drop = FALSE])
          result <- module$forward(row_inputs, .llm = .llm, trace = TRUE)

          results$result[i] <- list(result$output[[1]])
          results$.chat[i] <- list(result$chat[[1]] %||% NULL)
          results$.metadata[i] <- list(result$metadata[[1]] %||% list())
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
        if (!is.null(self$state$trials) && nrow(self$state$trials) > 0) {
          cli::cli_text("  Trials evaluated: {nrow(self$state$trials)}")
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
    #' Inspect module state and recent activity
    #'
    #' Shows detailed information about the module including its signature,

    #' configuration, compilation status, and the last prompt sent to the LLM.
    #' Useful for debugging and understanding module behavior.
    #'
    #' @return Invisibly returns self
    inspect = function() {
      cli::cli_h2("Module Inspection")

      # Show signature with inputs and output
      cli::cli_h3("Signature")
      sig <- self$signature
      input_names <- vapply(sig@inputs, function(x) x$name, character(1))
      if (length(input_names) > 0) {
        cli::cli_text("  Inputs: {.field {input_names}}")
      }
      cli::cli_text("  Output type: {.cls {class(sig@output_type)[1]}}")
      if (nzchar(sig@instructions)) {
        instructions_preview <- if (nchar(sig@instructions) > 80) {
          paste0(substr(sig@instructions, 1, 80), "...")
        } else {
          sig@instructions
        }
        cli::cli_text("  Instructions: {.emph {instructions_preview}}")
      }

      # Show configuration
      if (length(self$config) > 0) {
        cli::cli_h3("Configuration")
        for (name in names(self$config)) {
          val <- self$config[[name]]
          if (is.null(val)) {
            cli::cli_text("  {name}: {.val NULL}")
          } else if (is.atomic(val) && length(val) == 1) {
            cli::cli_text("  {name}: {.val {val}}")
          } else {
            cli::cli_text("  {name}: {.cls {class(val)[1]}}")
          }
        }
      }

      # Show chat/LLM info if available
      if (!is.null(self$chat)) {
        cli::cli_h3("Chat")
        cli::cli_text("  {.cls {class(self$chat)[1]}} attached")
      }

      # Show compilation status
      cli::cli_h3("State")
      if (self$is_compiled()) {
        cli::cli_text("  {cli::symbol$tick} Compiled")
        if (!is.null(self$state$best_score)) {
          cli::cli_text(
            "  Best score: {.val {round(self$state$best_score, 3)}}"
          )
        }
      } else {
        cli::cli_text("  {cli::symbol$cross} Not compiled")
      }

      # Show trace summary
      trace_summary <- self$trace_summary()
      cli::cli_text(
        "  Traces: {trace_summary$n_traces} call(s), {trace_summary$total_tokens} tokens"
      )
      if (
        !is.null(trace_summary$total_cost) &&
          !is.na(trace_summary$total_cost) &&
          trace_summary$total_cost > 0
      ) {
        cli::cli_text(
          "  Cost: ${format(trace_summary$total_cost, digits = 4)}"
        )
      }

      # Show last prompt if available
      if (length(self$state$traces) > 0) {
        cli::cli_h3("Last Prompt")
        last_trace <- self$state$traces[[length(self$state$traces)]]

        prompt_text <- trace_prompt_text(last_trace)

        if (!is.na(prompt_text) && nzchar(prompt_text)) {
          # Truncate if very long
          if (nchar(prompt_text) > 300) {
            prompt_text <- paste0(
              substr(prompt_text, 1, 300),
              "\n... (truncated)"
            )
          }
          cli::cat_line(prompt_text)
        } else {
          cli::cli_text("{.emph Prompt not available}")
        }

        # Show response preview
        cli::cli_h3("Last Response")
        response_text <- trace_response_text(last_trace)

        if (!is.na(response_text) && nzchar(response_text)) {
          if (nchar(response_text) > 200) {
            response_text <- paste0(substr(response_text, 1, 200), "...")
          }
          cli::cat_line(response_text)
        } else {
          cli::cli_text("{.emph Response not available}")
        }
      }

      invisible(self)
    },

    #' @description
    #' Run the module asynchronously
    #'
    #' Returns a promise that resolves to the structured output.
    #' Useful for running multiple modules in parallel.
    #' Ordinary Predict modules use the provider's native async path.
    #' ProgramOfThought, CodeAct, and RLM use an isolated mirai process when
    #' configured with `interpreter_factory`.
    #'
    #' @param ... Named inputs matching the signature
    #' @param .llm Optional ellmer chat object
    #' @param .trace_context A named JSON-compatible correlation context
    #' @return A promise that resolves to the result
    run_async = function(..., .llm = NULL, .trace_context = list()) {
      if (missing(.trace_context)) {
        return(dsprrr::run_async(self, ..., .llm = .llm))
      }
      dsprrr::run_async(
        self,
        ...,
        .llm = .llm,
        .trace_context = .trace_context
      )
    },

    #' @description
    #' Stream module output asynchronously
    #'
    #' Returns a promise that resolves to an async generator.
    #' This direct provider path supports ordinary Predict modules only;
    #' use [run()] for modules with specialized execution workflows.
    #'
    #' @param ... Named inputs matching the signature
    #' @param .llm Optional ellmer chat object
    #' @return A promise that resolves to an async generator
    stream_async = function(..., .llm = NULL) {
      dsprrr::stream_async(self, ..., .llm = .llm)
    },

    #' @description
    #' Stream text output from the module
    #'
    #' Returns a coro generator that yields text chunks as they arrive.
    #' Note: Streaming bypasses structured output; use [run()] for structured
    #' results.
    #' This direct provider path supports ordinary Predict modules only.
    #'
    #' @param ... Named inputs matching the signature
    #' @param .llm Optional ellmer chat object
    #' @param callback Optional function to call with each text chunk
    #' @return If callback is NULL, returns a coro generator. Otherwise, returns
    #'   the full response text invisibly after streaming completes.
    stream = function(..., .llm = NULL, callback = NULL) {
      assert_direct_provider_async_supported(self, "stream")
      inputs <- list(...)
      request <- build_module_request(self, inputs)
      llm <- resolve_module_llm(self, .llm = .llm)

      # Get stream generator from ellmer
      gen <- llm$stream(request$payload)

      if (!is.null(callback)) {
        # Consume stream with callback
        if (!is.function(callback)) {
          cli::cli_abort("{.arg callback} must be a function")
        }

        full_response <- character()
        coro::loop(
          for (chunk in gen) {
            callback(chunk)
            full_response <- c(full_response, chunk)
          }
        )
        invisible(paste(full_response, collapse = ""))
      } else {
        # Return generator for manual consumption
        gen
      }
    },

    #' @description
    #' Copy the module with independent Chat
    #'
    #' Creates an independent copy of the module with the same provider settings
    #' but fresh state. Uses R6's clone to preserve the exact class, then
    #' recreates the Chat to ensure independent state.
    #'
    #' @param deep Logical; if TRUE (default), also copies configuration and demos
    #' @return A new Module instance with independent state
    copy = function(deep = TRUE) {
      # Use R6's clone to preserve exact class (including subclasses)
      new_mod <- self$clone(deep = deep)

      # Create new Chat with fresh state (not shared conversation history)
      if (!is.null(self$chat)) {
        new_mod$chat <- private$clone_chat(self$chat)
      }

      # Reset state (traces, optimization history, etc.)
      new_mod$state <- list(
        traces = list(),
        cache = list(),
        compiled = FALSE,
        optimization_history = list(),
        trials = tibble::tibble(),
        last_grid = tibble::tibble(),
        best_score = NULL,
        best_params = NULL,
        best_trial = NULL
      )

      artifact_copy_runtime(self, new_mod)
    },

    #' @description
    #' Create a reset copy of the module (to be overridden by subclasses)
    #' @return New Module with reset state
    reset_copy = function() {
      cli::cli_abort("reset_copy() must be implemented by subclass")
    }
  ),

  private = list(
    # Build prompt from inputs (to be overridden by subclasses)
    build_prompt = function(inputs) {
      cli::cli_abort("build_prompt() must be implemented by subclass")
    },

    # Clone a Chat object with fresh state but same provider settings
    # Uses ellmer's R6 clone() method which preserves provider configuration
    clone_chat = function(chat) {
      if (is.null(chat)) {
        return(NULL)
      }

      # Use R6's clone method (ellmer Chat is R6)
      # deep = TRUE ensures internal state is also cloned
      tryCatch(
        {
          cloned <- chat$clone(deep = TRUE)
          # Reset the turn history to start fresh
          cloned$set_turns(list())
          cloned
        },
        error = function(e) {
          # If clone fails, try to recreate from provider info
          model <- tryCatch(chat$get_model(), error = function(e) NULL)
          class_names <- class(chat)

          for (cls in class_names) {
            result <- switch(
              cls,
              "Chat" = NULL, # Skip base class
              # OpenAI variants
              "OpenAIChat" = ,
              "chat_openai" = ellmer::chat_openai(model = model),
              # Anthropic variants
              "ClaudeChat" = ,
              "chat_claude" = ellmer::chat_claude(model = model),
              # Google variants
              "ChatGoogleGemini" = ,
              "chat_google_gemini" = ellmer::chat_google_gemini(model = model),
              # Ollama variants
              "OllamaChat" = ,
              "chat_ollama" = ellmer::chat_ollama(model = model),
              NULL
            )

            if (!is.null(result)) {
              return(result)
            }
          }

          # Fallback: return same chat (not ideal but better than failing)
          cli::cli_warn(
            "Could not clone Chat of class {.cls {class_names[1]}}; sharing reference"
          )
          chat
        }
      )
    },

    # Postprocess model output (to be overridden by subclasses)
    postprocess = function(output) {
      output
    }
  )
)

Module$set("public", "apply_optimization_params", function(params) {
  runtime_params <- intersect(names(params), runtime_param_names())
  if (length(runtime_params) > 0) {
    self$config$params <- self$config$params %||% list()
    for (name in runtime_params) {
      self$config$params[[name]] <- params[[name]]
      self$config[[name]] <- params[[name]]
    }
  }

  invisible(self)
})


normalize_factory_interpreter_batch_runtime <- function(
  runtime,
  explicit_llm = FALSE
) {
  if (!inherits(runtime, "dsprrr_concurrency_runtime")) {
    cli::cli_abort(
      "Internal interpreter concurrency runtime is invalid",
      class = "dsprrr_concurrency_config_error"
    )
  }

  if (is.finite(runtime$max_errors) || !isTRUE(runtime$cancel)) {
    cli::cli_abort(
      c(
        "Interpreter batches cannot enforce this concurrency control",
        "x" = "Finite error budgets and non-default cancellation are unsupported.",
        "i" = "Use {.code max_errors = Inf} and {.code cancel = TRUE}."
      ),
      class = "dsprrr_interpreter_concurrency_control_error"
    )
  }

  if (identical(runtime$effective_backend, "ellmer")) {
    if (identical(runtime$requested_backend, "ellmer")) {
      cli::cli_abort(
        c(
          "The ellmer backend cannot execute interpreter workflows",
          "i" = "Use {.code backend = \"mirai\"} so each row owns a fresh interpreter process."
        ),
        class = "dsprrr_interpreter_concurrency_backend_error"
      )
    }
    if (
      !isTRUE(explicit_llm) &&
        concurrency_backend_available("mirai")
    ) {
      runtime$effective_backend <- "mirai"
      runtime$effective_workers <- runtime$requested_workers
      runtime$fallback_reason <- paste(
        "mirai selected because ellmer cannot execute interpreter workflows"
      )
    } else {
      runtime$effective_backend <- "sequential"
      runtime$effective_workers <- 1L
      runtime$fallback_reason <- paste(
        "sequential execution selected because a supplied Chat cannot be",
        "sent to mirai safely"
      )
    }
  }

  if (
    identical(runtime$effective_backend, "mirai") &&
      (is.finite(runtime$task_timeout) ||
        is.finite(runtime$total_timeout) ||
        is.finite(runtime$max_errors))
  ) {
    cli::cli_abort(
      c(
        "Interpreter batch controls cannot be enforced by this mirai adapter",
        "x" = "Finite task/total timeouts and error budgets are not yet supported.",
        "i" = "Use unlimited controls or sequential execution."
      ),
      class = "dsprrr_interpreter_concurrency_control_error"
    )
  }

  runtime
}


factory_interpreter_history_field <- function(module) {
  if (inherits(module, "ProgramOfThoughtModule")) {
    return("executions")
  }
  if (inherits(module, "CodeActModule")) {
    return("trajectories")
  }
  if (inherits(module, "RLMModule")) {
    return("repl_history")
  }
  NULL
}


# Append one RLM observability record while preserving the module's bounded
# retention contract when records are merged back from isolated workers.
append_factory_interpreter_rlm_record <- function(records, record) {
  limit <- getOption("dsprrr.rlm_trace_limit", 100L)
  if (
    !is.numeric(limit) ||
      length(limit) != 1L ||
      is.na(limit) ||
      !is.finite(limit) ||
      limit < 1L
  ) {
    limit <- 100L
  }
  records <- append(records %||% list(), list(record))
  if (length(records) > floor(limit)) {
    records <- utils::tail(records, floor(limit))
  }
  records
}


# Commit a canonical RLM trace produced by an isolated interpreter worker.
# Worker-local state and prompt history disappear with the mirai process, so
# both stores must be reconstructed in the parent in input-row order.
commit_factory_interpreter_trace <- function(module, trace, index, runtime) {
  if (!inherits(module, "RLMModule") || is.null(trace)) {
    return(invisible(module))
  }
  if (!is.list(trace)) {
    cli::cli_abort(
      "Interpreter worker returned an invalid canonical trace",
      class = "dsprrr_trace_contract_error"
    )
  }

  trace_cursor <- evaluation_trace_cursor(module)
  history_generation_before <- prompt_history_generation()
  sequence <- module$state$trace_sequence %||% 0
  valid_sequence <- is.numeric(sequence) &&
    length(sequence) == 1L &&
    !is.na(sequence) &&
    is.finite(sequence) &&
    sequence >= 0
  if (!valid_sequence) {
    cli::cli_abort(
      "RLM trace sequence is invalid",
      class = "dsprrr_trace_contract_error"
    )
  }

  module$state$trace_sequence <- as.numeric(sequence) + 1
  module$state$traces <- append_factory_interpreter_rlm_record(
    module$state$traces,
    trace
  )

  add_to_global_history(trace, source = "RLMModule")
  reconcile_dataset_row_observability(
    module = module,
    trace_count_before = trace_cursor,
    history_generation_before = history_generation_before,
    row_index = index,
    runtime = runtime
  )
  invisible(module)
}


format_factory_interpreter_batch_row <- function(
  result,
  index,
  .return_format,
  runtime,
  chat = NULL
) {
  if (inherits(result, "condition")) {
    row <- create_error_result(
      error = result,
      index = index,
      prompt = "",
      instructions = "",
      llm = chat,
      .return_format = .return_format,
      metadata = if (identical(.return_format, "structured")) {
        list(
          error = conditionMessage(result),
          error_class = class(result)[1L],
          error_stage = "interpreter-workflow",
          batch_index = index
        )
      } else {
        NULL
      }
    )
  } else if (identical(.return_format, "simple")) {
    row <- result$output
  } else {
    metadata <- result$metadata %||% list()
    metadata$batch_index <- index
    row <- list(
      output = result$output,
      chat = result$chat %||% chat,
      metadata = metadata
    )
  }
  annotate_concurrency_result(row, runtime, .return_format)
}


run_factory_interpreter_batch <- function(
  module,
  inputs,
  n,
  .llm,
  .progress,
  .return_format,
  .concurrency,
  .cache = NULL
) {
  validate_cache_arg(.cache)
  input_sets <- lapply(seq_len(n), function(i) lapply(inputs, `[[`, i))
  history_field <- factory_interpreter_history_field(module)
  row_trace_events <- vector("list", n)
  llm <- .llm %||% module$chat %||% get_default_chat()
  if (is.null(llm)) {
    cli::cli_abort("No LLM provided. Pass .llm or set a default chat.")
  }

  if (identical(.concurrency$effective_backend, "sequential")) {
    rows <- vector("list", n)
    for (index in seq_len(n)) {
      trace_count_before <- evaluation_trace_cursor(module)
      history_generation_before <- prompt_history_generation()
      result <- tryCatch(
        {
          forwarded <- module$forward(
            input_sets[[index]],
            .llm = llm,
            trace = TRUE,
            .cache = .cache
          )
          list(
            output = forwarded$output[[1L]],
            chat = forwarded$chat[[1L]],
            metadata = forwarded$metadata[[1L]]
          )
        },
        interrupt = function(condition) stop(condition),
        error = function(condition) condition
      )
      reconcile_dataset_row_observability(
        module = module,
        trace_count_before = trace_count_before,
        history_generation_before = history_generation_before,
        row_index = index,
        runtime = .concurrency
      )
      row_trace_events[[index]] <- new_evaluation_trace_events(
        module,
        trace_count_before
      )
      rows[[index]] <- format_factory_interpreter_batch_row(
        result,
        index,
        .return_format,
        .concurrency,
        chat = llm
      )
    }
  } else {
    profile <- new_dsprrr_mirai_profile()
    mapped <- NULL
    profile_owned <- FALSE
    on.exit(
      {
        if (profile_owned) {
          shutdown_dsprrr_mirai_profile(
            profile,
            tasks = if (is.null(mapped)) list() else unclass(mapped),
            strict = FALSE
          )
        }
      },
      add = TRUE
    )
    mirai::daemons(
      n = .concurrency$effective_workers,
      dispatcher = TRUE,
      .compute = profile
    )
    profile_owned <- TRUE

    namespace_path <- getNamespaceInfo(asNamespace("dsprrr"), "path")
    worker <- function(
      input_set,
      module,
      llm,
      history_field,
      namespace_path,
      cache
    ) {
      if (
        file.exists(file.path(namespace_path, "R", "module-base.R")) &&
          requireNamespace("pkgload", quietly = TRUE)
      ) {
        pkgload::load_all(namespace_path, quiet = TRUE)
      } else {
        loadNamespace("dsprrr")
      }
      tryCatch(
        {
          trace_cursor <- if (inherits(module, "RLMModule")) {
            get(
              "evaluation_trace_cursor",
              envir = asNamespace("dsprrr")
            )(module)
          } else {
            NULL
          }
          forwarded <- module$forward(
            input_set,
            .llm = llm,
            trace = TRUE,
            .cache = cache
          )
          history <- if (is.null(history_field)) {
            list()
          } else {
            module$state[[history_field]]
          }
          trace <- if (is.null(trace_cursor)) {
            NULL
          } else {
            trace_indices <- get(
              "evaluation_trace_indices",
              envir = asNamespace("dsprrr")
            )(module, trace_cursor)
            if (length(trace_indices) == 0L) {
              cli::cli_abort(
                "RLM worker completed without a canonical trace",
                class = "dsprrr_trace_contract_error"
              )
            } else {
              module$state$traces[[trace_indices[[length(trace_indices)]]]]
            }
          }
          list(
            ok = TRUE,
            output = forwarded$output[[1L]],
            metadata = forwarded$metadata[[1L]],
            history = if (length(history) > 0L) {
              history[[length(history)]]
            } else {
              NULL
            },
            trace = trace
          )
        },
        interrupt = function(condition) stop(condition),
        error = function(condition) {
          list(
            ok = FALSE,
            error = conditionMessage(condition),
            error_class = class(condition)[1L]
          )
        }
      )
    }
    mapped <- mirai::mirai_map(
      input_sets,
      worker,
      .args = list(
        module = module,
        llm = llm,
        history_field = history_field,
        namespace_path = namespace_path,
        cache = .cache
      ),
      .compute = profile
    )
    records <- mapped[]
    stopped <- shutdown_dsprrr_mirai_profile(
      profile,
      tasks = unclass(mapped),
      strict = TRUE
    )
    if (isTRUE(stopped)) {
      profile_owned <- FALSE
    }

    rows <- vector("list", n)
    for (index in seq_len(n)) {
      record <- records[[index]]
      if (mirai::is_mirai_error(record)) {
        condition <- simpleError(record$message %||% as.character(record))
        class(condition) <- unique(c("dsprrr_mirai_error", class(condition)))
        result <- condition
      } else if (!is.list(record) || !isTRUE(record$ok)) {
        condition <- simpleError(record$error %||% "Interpreter worker failed")
        class(condition) <- unique(c(
          record$error_class %||% "dsprrr_interpreter_worker_error",
          class(condition)
        ))
        result <- condition
      } else {
        result <- list(
          output = record$output,
          chat = NULL,
          metadata = record$metadata
        )
        if (!is.null(history_field) && !is.null(record$history)) {
          module$state[[history_field]] <- if (inherits(module, "RLMModule")) {
            append_factory_interpreter_rlm_record(
              module$state[[history_field]],
              record$history
            )
          } else {
            append(module$state[[history_field]], list(record$history))
          }
        }
        trace_count_before <- evaluation_trace_cursor(module)
        commit_factory_interpreter_trace(
          module = module,
          trace = record$trace,
          index = index,
          runtime = .concurrency
        )
        row_trace_events[[index]] <- new_evaluation_trace_events(
          module,
          trace_count_before
        )
      }
      rows[[index]] <- format_factory_interpreter_batch_row(
        result,
        index,
        .return_format,
        .concurrency
      )
    }
  }

  attr(rows, "dsprrr_row_trace_events") <- row_trace_events

  if (identical(.return_format, "structured")) {
    structure(rows, class = c("dsprrr_batch_result", "list"))
  } else {
    rows
  }
}

#' Null-coalescing operator
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Predict Method for Modules (tidymodels-style)
#'
#' @description
#' S3 predict method for dsprrr Modules, providing a tidymodels-familiar
#' interface. This is an alternative to `run_dataset()` that matches the
#' pattern used by parsnip and other tidymodels packages.
#'
#' @param object A dsprrr Module object
#' @param new_data A data frame or tibble with columns matching the module's
#'   signature inputs
#' @param ... Additional arguments passed to `run_dataset()`
#'
#' @return A tibble with the input columns plus prediction results.
#'   The output column is named according to the signature's output field.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a module
#' mod <- signature("text -> sentiment") |>
#'   module(type = "predict", chat = chat_openai())
#'
#' # Use predict() like parsnip models
#' new_data <- tibble::tibble(text = c("Great!", "Terrible"))
#' predict(mod, new_data)
#'
#' # Equivalent to run_dataset()
#' run_dataset(mod, new_data, .llm = mod$chat)
#' }
predict.Module <- function(object, new_data, ...) {
  if (!is.data.frame(new_data)) {
    cli::cli_abort("{.arg new_data} must be a data frame or tibble")
  }

  # Get the LLM to use - prefer stored chat, then try default
  llm <- object$chat
  if (is.null(llm)) {
    llm <- tryCatch(
      get_default_chat(create = TRUE),
      error = function(e) {
        cli::cli_abort(c(
          "No Chat available for prediction",
          "i" = "Either set a chat on the module: {.code mod$chat <- chat_openai()}",
          "i" = "Or pass .llm: {.code predict(mod, data, .llm = chat)}"
        ))
      }
    )
  }

  # Use run_dataset for the actual processing
  run_dataset(object, new_data, .llm = llm, ...)
}

#' Predict Method for PredictModule
#'
#' @rdname predict.Module
#' @export
predict.PredictModule <- function(object, new_data, ...) {
  predict.Module(object, new_data, ...)
}
