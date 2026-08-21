#' Pipeline Module for Sequential Module Composition
#'
#' @description
#' A module that chains multiple modules together, passing outputs from one
#' to inputs of the next. Provides DSPy-style piping with the `%>>%` operator.
#'
#' @name pipeline
NULL

#' PipelineStep S7 Class
#'
#' @description
#' Represents a single step in a pipeline with optional input/output mappings.
#'
#' @keywords internal
#' @noRd
PipelineStep <- S7::new_class(
  "PipelineStep",
  properties = list(
    module = S7::new_property(
      S7::class_any,
      validator = function(value) {
        if (!inherits(value, "Module")) {
          return("module must be a Module object")
        }
        NULL
      }
    ),
    input_map = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        if (!is.list(value)) {
          return("input_map must be a named list")
        }
        if (length(value) > 0 && is.null(names(value))) {
          return(
            "input_map must be a named list (output_field = 'input_field')"
          )
        }
        NULL
      }
    ),
    output_select = S7::new_property(
      S7::class_character | S7::class_missing,
      default = character(0)
    ),
    static_inputs = S7::new_property(
      S7::class_list,
      default = list()
    )
  )
)

#' R6 PipelineModule Class
#'
#' @description
#' A module that chains multiple modules together sequentially. Each module's
#' output is passed as input to the next module. Supports automatic field
#' matching and explicit input/output mapping.
#'
#' @details
#' PipelineModule enables composing complex multi-step LLM workflows from
#' simpler building blocks. Key features:
#'
#' * **Automatic field matching**: When module A outputs field `answer` and
#'   module B expects input `answer`, the connection is automatic.
#' * **Explicit mapping**: Use `step()` to rename fields between modules.
#' * **Metadata aggregation**: Tracks total tokens, cost, and latency across
#'   all steps.
#' * **Works like any module**: Can be run, optimized, and compiled.
#'
#' @keywords internal
#' @noRd
PipelineModule <- R6::R6Class(
  "PipelineModule",
  inherit = Module,
  public = list(
    #' @field steps List of PipelineStep objects
    steps = NULL,

    #' @description
    #' Initialize a new PipelineModule
    #' @param steps List of PipelineStep objects or modules
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    initialize = function(steps, config = list(), chat = NULL) {
      # Convert modules to PipelineSteps if needed
      steps <- lapply(steps, function(s) {
        if (inherits(s, "dsprrr::PipelineStep")) {
          s
        } else if (inherits(s, "Module")) {
          PipelineStep(module = s)
        } else {
          cli::cli_abort(c(
            "Invalid step type",
            "x" = "Expected Module or PipelineStep, got {.cls {class(s)[1]}}"
          ))
        }
      })

      if (length(steps) == 0) {
        cli::cli_abort("Pipeline must have at least one step")
      }

      # Build composite signature
      composite_sig <- private$build_composite_signature(steps)

      super$initialize(
        signature = composite_sig,
        config = config,
        chat = chat
      )

      self$steps <- steps
    },

    #' @description
    #' Execute the pipeline with given inputs
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param .cache Logical or NULL for cache control
    #' @param ... Additional arguments passed to modules
    #' @return Tibble with output, chat, metadata columns
    forward = function(batch, .llm = NULL, trace = TRUE, .cache = NULL, ...) {
      # Handle both list and data frame inputs
      if (is.data.frame(batch)) {
        current_data <- as.list(batch[1, , drop = FALSE])
      } else {
        current_data <- batch
      }

      start_time <- Sys.time()
      all_metadata <- list()
      all_chats <- list()
      step_outputs <- list()
      step_inputs <- list()

      # Execute each step in sequence
      for (i in seq_along(self$steps)) {
        step <- self$steps[[i]]

        # Apply input mapping: rename fields from upstream output
        mapped_inputs <- private$apply_input_mapping(
          current_data,
          step@input_map
        )

        # Merge with static inputs (static inputs override mapped ones)
        merged_inputs <- modifyList(mapped_inputs, step@static_inputs)
        step_inputs[[i]] <- merged_inputs

        # Validate inputs against module signature
        required_inputs <- vapply(
          step@module$signature@inputs,
          function(x) x$name,
          character(1)
        )

        missing <- setdiff(required_inputs, names(merged_inputs))
        if (length(missing) > 0) {
          cli::cli_abort(c(
            "Missing inputs for pipeline step {i}",
            "x" = "Required: {.field {missing}}",
            "i" = "Available: {.field {names(merged_inputs)}}"
          ))
        }

        # Run the module
        result <- tryCatch(
          step@module$forward(
            merged_inputs,
            .llm = .llm,
            trace = FALSE,
            .cache = .cache,
            ...
          ),
          error = function(e) {
            cli::cli_abort(
              c(
                "Pipeline step {i} failed",
                "x" = e$message
              ),
              parent = e
            )
          }
        )

        # Extract output for next step
        output <- result$output[[1]]
        metadata <- result$metadata[[1]]
        chat_obj <- result$chat[[1]]

        # Validate output is not NULL
        if (is.null(output)) {
          cli::cli_abort(c(
            "Pipeline step {i} returned NULL output",
            "x" = "Module {.cls {class(step@module)[1]}} did not produce output",
            "i" = "This may indicate an API failure or module error"
          ))
        }

        all_metadata[[i]] <- metadata
        all_chats[[i]] <- chat_obj
        step_outputs[[i]] <- output

        # Prepare data for next step
        if (is.list(output) && !is.data.frame(output)) {
          # Structured output - use fields directly
          current_data <- output

          # Apply output selection if specified
          if (length(step@output_select) > 0) {
            available <- names(current_data)
            missing_selected <- setdiff(step@output_select, available)

            if (length(missing_selected) > 0) {
              cli::cli_warn(c(
                "Output selection requested non-existent fields at step {i}",
                "x" = "Requested: {.field {missing_selected}}",
                "i" = "Available: {.field {available}}",
                "!" = "These fields will be missing from downstream inputs"
              ))
            }

            current_data <- current_data[intersect(
              available,
              step@output_select
            )]
          }
        } else {
          # Simple output - wrap in list with output name from signature
          output_name <- private$get_output_name(step@module$signature)
          current_data <- setNames(list(output), output_name)
        }
      }

      # Calculate latency
      end_time <- Sys.time()
      latency_ms <- as.numeric(difftime(end_time, start_time, units = "secs")) *
        1000

      # Aggregate metadata
      aggregated_metadata <- private$aggregate_metadata(
        all_metadata,
        latency_ms
      )

      # Record trace if requested
      if (trace) {
        trace_entry <- list(
          timestamp = end_time,
          inputs = batch,
          output = current_data,
          step_inputs = step_inputs,
          step_outputs = step_outputs,
          step_metadata = all_metadata,
          aggregated = aggregated_metadata
        )
        self$state$traces <- append(self$state$traces, list(trace_entry))
      }

      # Return final output
      last_chat <- if (length(all_chats) > 0) {
        all_chats[[length(all_chats)]]
      } else {
        NULL
      }
      tibble::tibble(
        output = list(current_data),
        chat = list(last_chat),
        metadata = list(aggregated_metadata)
      )
    },

    #' @description
    #' Execute the pipeline with streaming listeners and status events.
    #' Used by [run_stream()]; see that function for semantics. Streaming
    #' execution does not record traces.
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param listeners List of [stream_listener()] objects
    #' @param on_status Optional function called with status event lists
    #' @return The final output (named list or plain value)
    forward_stream = function(
      batch,
      .llm = NULL,
      listeners = list(),
      on_status = NULL
    ) {
      if (is.data.frame(batch)) {
        current_data <- as.list(batch[1, , drop = FALSE])
      } else {
        current_data <- batch
      }

      n_steps <- length(self$steps)

      for (i in seq_along(self$steps)) {
        step <- self$steps[[i]]

        mapped_inputs <- private$apply_input_mapping(
          current_data,
          step@input_map
        )
        merged_inputs <- modifyList(mapped_inputs, step@static_inputs)

        required_inputs <- vapply(
          step@module$signature@inputs,
          function(x) x$name,
          character(1)
        )
        missing <- setdiff(required_inputs, names(merged_inputs))
        if (length(missing) > 0) {
          cli::cli_abort(c(
            "Missing inputs for pipeline step {i}",
            "x" = "Required: {.field {missing}}",
            "i" = "Available: {.field {names(merged_inputs)}}"
          ))
        }

        output <- stream_module_step(
          step@module,
          merged_inputs,
          .llm = .llm,
          listeners = listeners,
          on_status = on_status,
          step = i,
          n_steps = n_steps
        )

        if (is.null(output)) {
          cli::cli_abort(c(
            "Pipeline step {i} returned NULL output",
            "x" = "Module {.cls {class(step@module)[1]}} did not produce output"
          ))
        }

        if (is.list(output) && !is.data.frame(output)) {
          current_data <- output
          if (length(step@output_select) > 0) {
            current_data <- current_data[intersect(
              names(current_data),
              step@output_select
            )]
          }
        } else {
          output_name <- private$get_output_name(step@module$signature)
          current_data <- stats::setNames(list(output), output_name)
        }
      }

      current_data
    },

    #' @description
    #' Print the pipeline
    print = function() {
      cli::cli_h2("PipelineModule")

      cli::cli_h3("Steps ({length(self$steps)})")
      for (i in seq_along(self$steps)) {
        step <- self$steps[[i]]
        mod_class <- class(step@module)[1]
        sig <- step@module$signature

        # Get input/output summary
        input_names <- vapply(sig@inputs, function(x) x$name, character(1))
        output_name <- private$get_output_name(sig)

        step_desc <- paste0(
          "[",
          i,
          "] ",
          mod_class,
          ": ",
          paste(input_names, collapse = ", "),
          " -> ",
          output_name
        )

        # Show mapping if present
        if (length(step@input_map) > 0) {
          mappings <- vapply(
            names(step@input_map),
            function(from) {
              paste0(from, " -> ", step@input_map[[from]])
            },
            character(1)
          )
          step_desc <- paste0(
            step_desc,
            " (map: ",
            paste(mappings, collapse = ", "),
            ")"
          )
        }

        cli::cli_li(step_desc)
      }

      cli::cli_h3("Composite Signature")
      print(self$signature)

      if (self$is_compiled()) {
        cli::cli_h3("Compilation Status")
        cli::cli_text("{cli::symbol$tick} Compiled")
      }

      trace_summary <- self$trace_summary()
      if (trace_summary$n_traces > 0) {
        cli::cli_h3("Traces")
        cli::cli_text("  {trace_summary$n_traces} trace(s) recorded")
        cli::cli_text("  Total tokens: {trace_summary$total_tokens}")
        if (!is.na(trace_summary$total_cost) && trace_summary$total_cost > 0) {
          cli::cli_text(
            "  Total cost: ${format(trace_summary$total_cost, digits = 4)}"
          )
        }
      }

      invisible(self)
    },

    #' @description
    #' Create a deep copy of the pipeline with independent step modules
    #' @return New PipelineModule with copied steps, config, and state
    deepcopy = function() {
      new_steps <- lapply(self$steps, function(step) {
        mod <- step@module
        new_mod <- if (is.function(mod$deepcopy)) {
          mod$deepcopy()
        } else if (is.function(mod$reset_copy)) {
          mod$reset_copy()
        } else {
          mod$copy(deep = TRUE)
        }
        PipelineStep(
          module = new_mod,
          input_map = step@input_map,
          output_select = step@output_select,
          static_inputs = step@static_inputs
        )
      })

      new_pipeline <- PipelineModule$new(
        steps = new_steps,
        config = lapply(self$config, function(x) x),
        chat = self$chat
      )
      new_pipeline$state <- lapply(self$state, function(x) x)
      artifact_copy_runtime(self, new_pipeline)
    },

    #' @description
    #' Create a reset copy of the pipeline
    #' @return New PipelineModule with reset state
    reset_copy = function() {
      # Deep copy steps with reset modules
      new_steps <- lapply(self$steps, function(step) {
        new_mod <- if (is.function(step@module$reset_copy)) {
          step@module$reset_copy()
        } else {
          step@module$copy(deep = TRUE)
        }
        PipelineStep(
          module = new_mod,
          input_map = step@input_map,
          output_select = step@output_select,
          static_inputs = step@static_inputs
        )
      })

      artifact_copy_runtime(
        self,
        PipelineModule$new(
          steps = new_steps,
          config = self$config,
          chat = NULL
        )
      )
    }
  ),

  private = list(
    # Build composite signature from steps
    build_composite_signature = function(steps) {
      # Collect all inputs needed at pipeline entry
      # (inputs not satisfied by upstream outputs)
      all_required_inputs <- list()
      satisfied_by_upstream <- character(0)

      for (i in seq_along(steps)) {
        step <- steps[[i]]
        module_inputs <- step@module$signature@inputs

        for (inp in module_inputs) {
          input_name <- inp$name

          # Check if this input is mapped from a different upstream field
          upstream_source <- NULL
          for (from in names(step@input_map)) {
            if (step@input_map[[from]] == input_name) {
              upstream_source <- from
              break
            }
          }

          # Check if satisfied by upstream output or mapping
          if (input_name %in% satisfied_by_upstream) {
            # Already provided by previous step
            next
          } else if (
            !is.null(upstream_source) &&
              upstream_source %in% satisfied_by_upstream
          ) {
            # Mapped from upstream field
            next
          } else if (input_name %in% names(step@static_inputs)) {
            # Provided as static input
            next
          } else {
            # Must come from pipeline entry
            all_required_inputs[[input_name]] <- inp
          }
        }

        # Track what this step produces
        output_type <- step@module$signature@output_type
        output_names <- private$get_output_fields(output_type)
        satisfied_by_upstream <- unique(c(satisfied_by_upstream, output_names))
      }

      # Use output type from final step
      final_output_type <- steps[[length(steps)]]@module$signature@output_type

      # Combine instructions (optional - use first non-empty)
      instructions <- ""
      for (step in steps) {
        if (nchar(step@module$signature@instructions) > 0) {
          instructions <- step@module$signature@instructions
          break
        }
      }

      Signature(
        inputs = unname(all_required_inputs),
        output_type = final_output_type,
        instructions = instructions
      )
    },

    # Get field names from an output type
    get_output_fields = function(output_type) {
      if (inherits(output_type, "ellmer::TypeObject")) {
        names(output_type@properties)
      } else {
        # For non-object types, use a generic name
        "output"
      }
    },

    # Get output name from signature
    get_output_name = function(sig) {
      output_type <- sig@output_type
      if (inherits(output_type, "ellmer::TypeObject")) {
        props <- output_type@properties
        if (length(props) == 1) {
          names(props)[1]
        } else {
          paste(names(props), collapse = ", ")
        }
      } else {
        "output"
      }
    },

    # Apply input mapping to data
    apply_input_mapping = function(data, mapping) {
      if (length(mapping) == 0) {
        return(data)
      }

      result <- data
      available_fields <- names(data)

      for (from in names(mapping)) {
        to <- mapping[[from]]
        if (from %in% available_fields) {
          result[[to]] <- data[[from]]
          # Remove old name if different
          if (from != to) {
            result[[from]] <- NULL
          }
        } else {
          cli::cli_warn(c(
            "Input mapping references non-existent field",
            "x" = "Mapping {.field {from}} -> {.field {to}} ignored",
            "i" = "Available fields: {.field {available_fields}}"
          ))
        }
      }

      result
    },

    # Aggregate metadata from all steps
    aggregate_metadata = function(step_metadata, total_latency_ms) {
      models <- character(0)

      for (meta in step_metadata) {
        if (!is.null(meta$model) && !is.na(meta$model)) {
          models <- c(models, meta$model)
        }
      }
      usage <- aggregate_module_usage_metadata(step_metadata)

      list(
        timestamp = Sys.time(),
        n_steps = length(step_metadata),
        total_tokens = usage$total_tokens,
        cost = usage$cost,
        provider_calls = usage$provider_calls,
        latency_ms = total_latency_ms,
        models = unique(models),
        step_metadata = step_metadata
      )
    }
  )
)


# =============================================================================
# Pipe Operator
# =============================================================================

#' Pipe Operator for Module Composition
#'
#' @description
#' Chains modules together into a pipeline. The output of the left module
#' flows into the input of the right module. When field names match between
#' output and input, they are automatically connected.
#'
#' @param lhs A Module or PipelineModule
#' @param rhs A Module or PipelineModule.
#'
#' @return A PipelineModule combining both modules
#'
#' @export
#' @examples
#' \dontrun{
#' # Simple chaining - automatic field matching
#' qa_pipeline <- mod_parse %>>% mod_answer %>>% mod_format
#'
#' # Run the pipeline
#' result <- run(qa_pipeline, text = "What is 2+2?", .llm = llm)
#'
#' }
`%>>%` <- function(lhs, rhs) {
  if (inherits(rhs, "Module")) {
    rhs_step <- PipelineStep(module = rhs)
  } else {
    cli::cli_abort(c(
      "Invalid right-hand side for %>>%",
      "x" = "Expected Module or PipelineModule, got {.cls {class(rhs)[1]}}"
    ))
  }

  # Build steps list
  if (inherits(lhs, "PipelineModule")) {
    # Extend existing pipeline
    steps <- c(lhs$steps, list(rhs_step))
  } else if (inherits(lhs, "Module")) {
    # Start new pipeline
    lhs_step <- PipelineStep(module = lhs)
    steps <- list(lhs_step, rhs_step)
  } else {
    cli::cli_abort(c(
      "Invalid left-hand side for %>>%",
      "x" = "Expected Module or PipelineModule, got {.cls {class(lhs)[1]}}"
    ))
  }

  PipelineModule$new(steps = steps)
}


# =============================================================================
# Pipeline Constructor
# =============================================================================

#' Create a Pipeline from Modules
#'
#' @description
#' Explicitly constructs a pipeline from a sequence of modules. Use this when
#' you need fine-grained control over input/output mapping between steps.
#'
#' @param ... Modules or steps (created with `step()`) to chain together
#'
#' @return A PipelineModule
#'
#' @export
#' @examples
#' \dontrun{
#' # Simple pipeline
#' p <- pipeline(mod_a, mod_b, mod_c)
#'
#' # With explicit mapping
#' p <- pipeline(
#'   mod_retrieve,
#'   step(mod_answer, map = c(documents = "context")),
#'   mod_summarize
#' )
#'
#' # Run it
#' result <- run(p, query = "...", .llm = llm)
#' }
pipeline <- function(...) {
  args <- list(...)

  if (length(args) == 0) {
    cli::cli_abort("pipeline() requires at least one module")
  }

  # Convert to PipelineSteps
  steps <- lapply(args, function(x) {
    if (inherits(x, "dsprrr::PipelineStep")) {
      x
    } else if (inherits(x, "Module")) {
      PipelineStep(module = x)
    } else {
      cli::cli_abort(c(
        "Invalid argument to pipeline()",
        "x" = "Expected Module or step(), got {.cls {class(x)[1]}}"
      ))
    }
  })

  PipelineModule$new(steps = steps)
}


#' Create a Pipeline Step with Mappings
#'
#' @description
#' Wraps a module with input/output mapping configuration for use in `pipeline()`.
#'
#' @param module A Module object
#' @param map Named character vector mapping upstream output fields to this
#'   module's input fields. Format: `c(output_field = "input_field")`.
#' @param select Character vector of output field names to pass forward.
#'   If empty, all fields are passed.
#' @param ... Static inputs to inject (name = value pairs)
#'
#' @return A PipelineStep object for use in `pipeline()`
#'
#' @export
#' @examples
#' \dontrun{
#' # Map 'documents' from upstream to 'context' input
#' pipeline(
#'   mod_retrieve,
#'   step(mod_answer, map = c(documents = "context")),
#'   mod_format
#' )
#'
#' # Select only certain output fields
#' pipeline(
#'   mod_analyze,
#'   step(mod_format, select = c("summary", "score")),
#'   mod_present
#' )
#'
#' # Inject static inputs
#' pipeline(
#'   mod_retrieve,
#'   step(mod_answer, system_prompt = "Be concise")
#' )
#' }
step <- function(module, map = list(), select = character(0), ...) {
  if (!inherits(module, "Module")) {
    cli::cli_abort(c(
      "step() requires a Module",
      "x" = "Got {.cls {class(module)[1]}}"
    ))
  }

  # Convert named vector to list if needed
  if (is.character(map) && !is.null(names(map))) {
    map <- as.list(map)
  }

  static_inputs <- list(...)

  PipelineStep(
    module = module,
    input_map = map,
    output_select = select,
    static_inputs = static_inputs
  )
}
