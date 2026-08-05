#' Experimental Declarative Flex Module
#'
#' @description
#' `flex()` creates an experimental module whose predictor topology is itself an
#' optimization parameter. The topology is supplied as canonical JSON in
#' `module_src`; it is parsed as data and is never evaluated as R code.
#'
#' Version 1 sources contain `schema_version`, an ordered `steps` array, and an
#' `outputs` object. Each step has a safe, unique `name`, a `primitive` of
#' `"predict"` or `"chain_of_thought"`, a `signature` (`"$outer"` or DSPy
#' string notation), an optional `instructions` string, and an `inputs` object.
#' Input references use `"$input.<name>"` or
#' `"$step.<earlier-step>.<field>"`. `outputs` maps every outer output field to
#' one of the same reference forms. Sources are type checked before binding.
#' Flex v1 supports string, number, integer, boolean, enum, array, and non-empty
#' object signature types. Opaque `TypeJsonSchema` values and empty objects are
#' rejected because their interfaces cannot be checked safely by this compiler.
#'
#' When `module_src` is `NULL`, the baseline is one `predict` step over the
#' outer signature. `$bind()` and `$apply_optimization_params()` validate a new
#' source transactionally, so an invalid candidate cannot replace the active
#' plan. The canonical source is available through the read-only
#' `$module_src` active binding.
#'
#' GEPA optimizes Flex programs as complete instruction and `module_src`
#' component snapshots. Invalid source candidates are scored for audit but are
#' not selectable. GEPA-lite ranks whole-program candidates; it does not
#' implement DSPy's independent per-component Pareto frontiers or inference-time
#' search.
#'
#' [run_async()], [stream_async()], and a module's `$stream()` method reject Flex
#' because their direct provider path would bypass its graph. The [run_stream()]
#' one-shot `forward()` fallback remains available, but an actual token-stream
#' request is rejected before provider work.
#'
#' @param signature A [Signature] object or DSPy-style signature string.
#' @param module_src A version 1 Flex source as one JSON string, or `NULL` for
#'   the single-predictor baseline.
#' @param max_predictor_calls Maximum number of predictor steps allowed in one
#'   source.
#' @param config Optional module configuration passed to each fresh predictor.
#' @param chat Optional ellmer `Chat` used unless `.llm` is supplied at run
#'   time.
#'
#' @return An experimental `FlexModule`.
#' @export
#'
#' @examples
#' program <- flex("question -> answer")
#' program$module_src
#'
#' \dontrun{
#' result <- run(program, question = "Why is the sky blue?", .llm = llm)
#' }
flex <- function(
  signature,
  module_src = NULL,
  max_predictor_calls = 100L,
  config = list(),
  chat = NULL
) {
  flex_warn_experimental()

  sig <- if (is.character(signature)) {
    tryCatch(
      parse_signature(signature),
      error = function(error) {
        flex_source_abort(
          "{.arg signature} is not valid DSPy signature notation",
          class = "dsprrr_flex_signature_error",
          parent = error
        )
      }
    )
  } else {
    signature
  }

  if (!inherits(sig, "dsprrr::Signature")) {
    cli::cli_abort(
      "{.arg signature} must be a Signature object or one signature string",
      class = "dsprrr_flex_config_error"
    )
  }

  max_predictor_calls <- flex_positive_integer(
    max_predictor_calls,
    "max_predictor_calls"
  )

  FlexModule$new(
    signature = sig,
    module_src = module_src,
    max_predictor_calls = max_predictor_calls,
    config = config,
    chat = chat
  )
}

.flex_lifecycle <- new.env(parent = emptyenv())
.flex_lifecycle$warned <- FALSE

flex_warn_experimental <- function() {
  if (!isTRUE(.flex_lifecycle$warned)) {
    .flex_lifecycle$warned <- TRUE
    cli::cli_warn(
      c(
        "{.fn flex} is experimental and its module source schema may change",
        "i" = "Flex source is declarative JSON and is never evaluated as R code."
      ),
      class = "dsprrr_flex_experimental_warning"
    )
  }
  invisible(NULL)
}

#' Count predictor calls encoded by a validated Flex source
#' @noRd
flex_predictor_call_count <- function(module) {
  if (!inherits(module, "FlexModule")) {
    cli::cli_abort(
      "Internal Flex predictor counting requires a FlexModule",
      class = "dsprrr_flex_internal_error"
    )
  }
  source <- jsonlite::fromJSON(module$module_src, simplifyVector = FALSE)
  as.integer(length(source$steps))
}

#' Remove runtime Chat parameters after Flex resolves them once
#' @noRd
flex_predictor_config <- function(config) {
  config <- config %||% list()
  config$params <- NULL
  config[runtime_param_names()] <- NULL
  config
}

# One megabyte is ample for the bounded declarative graph while preventing a
# malformed optimizer proposal from consuming unbounded parser memory.
.flex_max_source_bytes <- 1024L * 1024L

#' Experimental Flex Module R6 Class
#'
#' @keywords internal
#' @noRd
FlexModule <- R6::R6Class(
  "FlexModule",
  inherit = PredictModule,
  public = list(
    initialize = function(
      signature,
      module_src = NULL,
      max_predictor_calls = 100L,
      config = list(),
      chat = NULL
    ) {
      super$initialize(
        signature = signature,
        template = "",
        demos = list(),
        config = config,
        chat = chat
      )
      private$.max_predictor_calls <- flex_positive_integer(
        max_predictor_calls,
        "max_predictor_calls"
      )
      compiled <- flex_compile_source(
        module_src = module_src,
        outer_signature = self$signature,
        max_predictor_calls = private$.max_predictor_calls
      )
      private$.plan <- compiled$plan
      private$.module_src <- compiled$module_src
    },

    forward = function(
      batch,
      .llm = NULL,
      trace = TRUE,
      .cache = NULL,
      rollout_id = NULL,
      ...
    ) {
      inputs <- if (is.data.frame(batch)) {
        if (nrow(batch) != 1L) {
          cli::cli_abort(
            c(
              "Flex forward execution requires exactly one data-frame row",
              "x" = "Received {nrow(batch)} rows.",
              "i" = "Use {.fn run_dataset} for zero or multiple rows."
            ),
            class = c(
              "dsprrr_flex_runtime_input_error",
              "dsprrr_flex_runtime_error"
            )
          )
        }
        flex_data_frame_input_row(batch, self$signature)
      } else {
        batch
      }
      if (
        !is.list(inputs) ||
          (length(inputs) > 0L && is.null(names(inputs)))
      ) {
        cli::cli_abort(
          "Flex inputs must be supplied as a named list or data frame",
          class = "dsprrr_flex_runtime_error"
        )
      }
      flex_validate_runtime_input_names(inputs, self$signature)
      validate_signature_inputs(
        self$signature,
        inputs,
        missing = "ignore",
        extra = "ignore",
        type = "ignore",
        context = "Flex inputs"
      )
      input_types <- flex_signature_input_types(self$signature)
      for (name in names(input_types)) {
        flex_validate_runtime_value(
          inputs[[name]],
          input_types[[name]],
          paste0("Flex input $", name),
          class = c(
            "dsprrr_flex_runtime_input_error",
            "dsprrr_type_mismatch_error",
            "dsprrr_input_validation_error"
          ),
          kind = "input"
        )
      }

      llm <- resolve_module_llm(self, .llm = .llm)
      predictor_config <- flex_predictor_config(self$config)
      started_at <- Sys.time()
      values <- list()
      step_metadata <- vector("list", length(private$.plan$steps))
      names(step_metadata) <- vapply(
        private$.plan$steps,
        `[[`,
        character(1),
        "name"
      )

      for (index in seq_along(private$.plan$steps)) {
        step <- private$.plan$steps[[index]]
        step_inputs <- lapply(
          step$bindings,
          flex_resolve_reference,
          inputs = inputs,
          values = values
        )

        # A new module is deliberately constructed for every step and every
        # invocation. No demonstrations, traces, or mutable module state leak
        # between predictor calls.
        predictor <- module(
          signature = step$signature,
          type = step$primitive,
          config = predictor_config
        )
        result <- tryCatch(
          predictor$forward(
            step_inputs,
            .llm = llm,
            trace = FALSE,
            .cache = .cache,
            rollout_id = rollout_id,
            ...
          ),
          error = function(error) {
            cli::cli_abort(
              "Flex step {.field {step$name}} failed",
              class = "dsprrr_flex_step_error",
              parent = error,
              step = step$name
            )
          }
        )

        values[[step$name]] <- flex_output_record(
          result$output[[1L]],
          step$output_types,
          step$name,
          output_type = step$signature@output_type
        )
        step_metadata[[step$name]] <- list(
          name = step$name,
          primitive = step$primitive,
          metadata = result$metadata[[1L]]
        )
      }

      output <- lapply(
        private$.plan$outputs,
        flex_resolve_reference,
        inputs = inputs,
        values = values
      )
      output <- output[names(private$.plan$outputs)]
      output <- flex_validate_output_record(
        output,
        flex_signature_output_types(self$signature),
        context = "Flex outer output",
        class = "dsprrr_flex_output_error",
        scalar = FALSE
      )

      finished_at <- Sys.time()
      latency_ms <- as.numeric(
        difftime(finished_at, started_at, units = "secs")
      ) *
        1000
      usage <- flex_aggregate_step_usage(step_metadata)
      model <- tryCatch(llm$get_model(), error = function(error) NA_character_)
      metadata <- list(
        timestamp = finished_at,
        model = model,
        predictor_calls = length(private$.plan$steps),
        latency_ms = latency_ms,
        input_tokens = usage$input_tokens,
        output_tokens = usage$output_tokens,
        cached_input_tokens = usage$cached_input_tokens,
        total_tokens = usage$total_tokens,
        cost = usage$cost,
        steps = step_metadata,
        module_src = private$.module_src
      )

      if (isTRUE(trace)) {
        self$state$traces <- append(
          self$state$traces,
          list(list(
            timestamp = finished_at,
            inputs = inputs,
            output = output,
            model = model,
            predictor_calls = length(private$.plan$steps),
            steps = step_metadata,
            module_src = private$.module_src,
            latency_ms = latency_ms,
            tokens = usage[c(
              "input_tokens",
              "output_tokens",
              "cached_input_tokens",
              "total_tokens"
            )],
            cost = usage$cost
          ))
        )
      }

      tibble::tibble(
        output = list(output),
        chat = list(llm),
        metadata = list(metadata)
      )
    },

    bind = function(module_src) {
      compiled <- flex_compile_source(
        module_src = module_src,
        outer_signature = self$signature,
        max_predictor_calls = private$.max_predictor_calls
      )
      private$.plan <- compiled$plan
      private$.module_src <- compiled$module_src
      invisible(self)
    },

    reset_copy = function() {
      FlexModule$new(
        signature = self$signature,
        module_src = private$.module_src,
        max_predictor_calls = private$.max_predictor_calls,
        config = self$config,
        chat = self$chat
      )
    },

    deepcopy = function() {
      new_module <- FlexModule$new(
        signature = self$signature,
        module_src = private$.module_src,
        max_predictor_calls = private$.max_predictor_calls,
        config = lapply(self$config, identity),
        chat = self$chat
      )
      new_module$state <- lapply(self$state, identity)
      new_module
    },

    apply_optimization_params = function(params = list(), module_src = NULL) {
      if (is.null(params)) {
        params <- list()
      }
      if (!is.list(params)) {
        cli::cli_abort(
          "{.arg params} must be a list",
          class = "dsprrr_flex_config_error"
        )
      }
      param_names <- names(params) %||% character()
      allowed_params <- unique(c(
        "module_src",
        "instructions",
        "template",
        "prompt_style",
        "id",
        runtime_param_names()
      ))
      if (
        length(param_names) != length(params) ||
          any(!nzchar(param_names)) ||
          anyDuplicated(param_names)
      ) {
        cli::cli_abort(
          "{.arg params} must have unique, non-empty names",
          class = "dsprrr_flex_config_error"
        )
      }
      unknown_params <- setdiff(param_names, allowed_params)
      if (length(unknown_params) > 0L) {
        cli::cli_abort(
          "Unknown Flex optimization parameters: {.field {unknown_params}}",
          class = "dsprrr_flex_config_error"
        )
      }
      listed_source <- params[["module_src"]]
      if (
        !is.null(module_src) &&
          !is.null(listed_source) &&
          !identical(module_src, listed_source)
      ) {
        cli::cli_abort(
          "Supply Flex source through either {.arg params} or {.arg module_src}, not both",
          class = "dsprrr_flex_config_error"
        )
      }
      candidate <- module_src %||% listed_source

      instruction_update <- flex_optimization_string_update(
        params[["instructions"]],
        "instructions"
      )
      flex_optimization_string_update(params[["template"]], "template")
      flex_optimization_string_update(
        params[["prompt_style"]],
        "prompt_style"
      )

      candidate_signature <- self$signature
      if (instruction_update) {
        candidate_signature <- with_instructions(
          candidate_signature,
          params[["instructions"]]
        )
      }

      # Validate first so an invalid source cannot partially change either the
      # active plan or ordinary runtime optimization parameters. Recompile the
      # current source when outer instructions change because `$outer` steps
      # carry that derived signature in the validated execution plan.
      compiled <- if (is.null(candidate) && !instruction_update) {
        NULL
      } else {
        flex_compile_source(
          module_src = candidate %||% private$.module_src,
          outer_signature = candidate_signature,
          max_predictor_calls = private$.max_predictor_calls
        )
      }

      super$apply_optimization_params(params)
      if (!is.null(compiled)) {
        private$.plan <- compiled$plan
        private$.module_src <- compiled$module_src
      }
      invisible(self)
    },

    graph_is_parameter = function() TRUE
  ),
  active = list(
    module_src = function(value) {
      if (!missing(value)) {
        cli::cli_abort(
          "{.field module_src} is read-only; use {.fn bind}",
          class = "dsprrr_flex_read_only_error"
        )
      }
      private$.module_src
    },
    max_predictor_calls = function(value) {
      if (!missing(value)) {
        cli::cli_abort(
          "{.field max_predictor_calls} is read-only",
          class = "dsprrr_flex_read_only_error"
        )
      }
      private$.max_predictor_calls
    }
  ),
  private = list(
    .module_src = NULL,
    .plan = NULL,
    .max_predictor_calls = NULL
  )
)

flex_positive_integer <- function(value, arg) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == floor(value) &&
    value >= 1L &&
    value <= .Machine$integer.max
  if (!valid) {
    cli::cli_abort(
      "{.arg {arg}} must be one positive integer",
      class = "dsprrr_flex_config_error"
    )
  }
  as.integer(value)
}

flex_validate_runtime_input_names <- function(inputs, signature) {
  provided <- names(inputs) %||% character()
  if (any(!nzchar(provided)) || anyDuplicated(provided)) {
    cli::cli_abort(
      "Flex inputs must have unique, non-empty names",
      class = c(
        "dsprrr_flex_runtime_input_error",
        "dsprrr_flex_runtime_error"
      )
    )
  }
  expected <- names(flex_signature_input_types(signature))
  extra <- setdiff(provided, expected)
  if (length(extra) > 0L) {
    cli::cli_abort(
      c(
        "Extra input not declared in the Flex signature: {.field {extra}}",
        "i" = "Flex runtime inputs exactly match its closed declarative interface."
      ),
      class = c(
        "dsprrr_extra_input_error",
        "dsprrr_input_validation_error",
        "dsprrr_flex_runtime_input_error",
        "dsprrr_flex_runtime_error"
      )
    )
  }
  invisible(NULL)
}

flex_data_frame_input_row <- function(batch, signature) {
  input_types <- flex_signature_input_types(signature)
  values <- lapply(names(batch), function(name) {
    column <- batch[[name]]
    type <- input_types[[name]]
    if (
      !is.null(type) &&
        inherits(type, "ellmer::TypeObject") &&
        is.data.frame(column)
    ) {
      return(tryCatch(
        ellmer_parallel_response_row(column, 1L, type),
        error = function(error) {
          cli::cli_abort(
            "Could not reconstruct data-frame input {.field {name}}",
            class = c(
              "dsprrr_flex_runtime_input_error",
              "dsprrr_flex_runtime_error"
            ),
            parent = error
          )
        }
      ))
    }
    column[[1L]]
  })
  names(values) <- names(batch)
  values
}

flex_optimization_string_update <- function(value, arg) {
  if (is.null(value)) {
    return(FALSE)
  }
  if (
    !is.character(value) ||
      length(value) != 1L
  ) {
    cli::cli_abort(
      "{.arg {arg}} must be one string, NA, or NULL",
      class = "dsprrr_flex_config_error"
    )
  }
  !is.na(value)
}

flex_source_abort <- function(
  message,
  class = "dsprrr_flex_source_error",
  parent = NULL,
  ...
) {
  caller_env <- parent.frame()
  cli::cli_abort(
    message,
    class = unique(c(class, "dsprrr_flex_source_error")),
    parent = parent,
    .envir = caller_env,
    ...
  )
}

flex_compile_source <- function(
  module_src,
  outer_signature,
  max_predictor_calls
) {
  for (name in names(flex_signature_input_types(outer_signature))) {
    flex_validate_supported_type(
      flex_signature_input_types(outer_signature)[[name]],
      paste0("outer input ", name)
    )
  }
  flex_validate_supported_type(
    outer_signature@output_type,
    "outer output"
  )
  if (is.null(module_src)) {
    source <- flex_baseline_source(outer_signature)
  } else {
    source <- flex_parse_json_source(module_src)
  }

  flex_validate_object(
    source,
    required = c("schema_version", "steps", "outputs"),
    context = "Flex source"
  )
  if (!flex_is_integerish(source$schema_version, 1L)) {
    flex_source_abort(
      "{.field schema_version} must be the number 1",
      class = "dsprrr_flex_schema_version_error"
    )
  }
  if (
    !is.list(source$steps) ||
      length(source$steps) < 1L ||
      !is.null(names(source$steps))
  ) {
    flex_source_abort(
      "{.field steps} must be a non-empty JSON array",
      class = "dsprrr_flex_schema_error"
    )
  }
  if (length(source$steps) > max_predictor_calls) {
    flex_source_abort(
      c(
        "Flex source exceeds {.arg max_predictor_calls}",
        "x" = "Source has {length(source$steps)} steps; the limit is {max_predictor_calls}."
      ),
      class = "dsprrr_flex_budget_error",
      predictor_calls = length(source$steps),
      max_predictor_calls = max_predictor_calls
    )
  }

  steps <- lapply(
    seq_along(source$steps),
    function(index) {
      flex_compile_step(
        source$steps[[index]],
        outer_signature = outer_signature,
        index = index
      )
    }
  )
  step_names <- vapply(steps, `[[`, character(1), "name")
  if (anyDuplicated(step_names)) {
    duplicated_names <- unique(step_names[duplicated(step_names)])
    flex_source_abort(
      "Flex step names must be unique: {.field {duplicated_names}}",
      class = "dsprrr_flex_duplicate_step_error",
      duplicated_names = duplicated_names
    )
  }

  known_types <- stats::setNames(
    lapply(steps, `[[`, "output_types"),
    step_names
  )
  dependencies <- stats::setNames(vector("list", length(steps)), step_names)
  for (index in seq_along(steps)) {
    step <- steps[[index]]
    input_types <- flex_signature_input_types(step$signature)
    expected_inputs <- names(input_types)
    flex_validate_exact_names(
      step$inputs,
      expected_inputs,
      "inputs for Flex step {.field {step$name}}"
    )
    step$inputs <- step$inputs[expected_inputs]
    step$bindings <- lapply(
      step$inputs,
      flex_parse_reference,
      outer_signature = outer_signature,
      known_types = known_types,
      context = "input to Flex step {.field {step$name}}"
    )
    dependencies[[step$name]] <- unique(vapply(
      Filter(function(ref) identical(ref$kind, "step"), step$bindings),
      `[[`,
      character(1),
      "name"
    ))
    steps[[index]] <- step
  }

  flex_reject_cycles(dependencies)
  step_positions <- stats::setNames(seq_along(steps), step_names)
  for (index in seq_along(steps)) {
    later <- dependencies[[steps[[index]]$name]][
      step_positions[dependencies[[steps[[index]]$name]]] >= index
    ]
    if (length(later) > 0L) {
      flex_source_abort(
        c(
          "Flex step references must point to earlier steps",
          "x" = "Step {.field {steps[[index]]$name}} references {.field {later}}."
        ),
        class = "dsprrr_flex_forward_reference_error"
      )
    }

    input_types <- flex_signature_input_types(steps[[index]]$signature)
    for (name in names(input_types)) {
      source_type <- flex_reference_type(
        steps[[index]]$bindings[[name]],
        outer_signature,
        known_types
      )
      if (!flex_type_assignable(source_type, input_types[[name]])) {
        flex_source_abort(
          c(
            "Incompatible Flex reference types",
            "x" = "{steps[[index]]$inputs[[name]]} cannot supply input {.field {name}} in step {.field {steps[[index]]$name}}."
          ),
          class = "dsprrr_flex_type_error"
        )
      }
    }
  }

  outer_outputs <- flex_signature_output_types(outer_signature)
  flex_validate_exact_names(
    source$outputs,
    names(outer_outputs),
    "Flex outputs"
  )
  output_refs <- lapply(
    source$outputs[names(outer_outputs)],
    flex_parse_reference,
    outer_signature = outer_signature,
    known_types = known_types,
    context = "Flex output"
  )
  for (name in names(outer_outputs)) {
    source_type <- flex_reference_type(
      output_refs[[name]],
      outer_signature,
      known_types
    )
    if (!flex_type_assignable(source_type, outer_outputs[[name]])) {
      flex_source_abort(
        c(
          "Incompatible Flex output type",
          "x" = "{source$outputs[[name]]} cannot supply outer output {.field {name}}."
        ),
        class = "dsprrr_flex_type_error"
      )
    }
  }

  canonical_steps <- lapply(steps, function(step) {
    canonical <- list(
      name = step$name,
      primitive = step$primitive,
      signature = step$signature_src
    )
    if (!is.null(step$instructions)) {
      canonical$instructions <- step$instructions
    }
    canonical$inputs <- step$inputs
    canonical
  })
  canonical <- list(
    schema_version = 1L,
    steps = canonical_steps,
    outputs = source$outputs[names(outer_outputs)]
  )

  list(
    plan = list(steps = steps, outputs = output_refs),
    module_src = flex_canonical_json(canonical)
  )
}

flex_parse_json_source <- function(module_src) {
  if (
    !is.character(module_src) ||
      length(module_src) != 1L ||
      is.na(module_src)
  ) {
    flex_source_abort(
      "{.arg module_src} must be one non-missing JSON string",
      class = "dsprrr_flex_json_error"
    )
  }
  size <- tryCatch(nchar(module_src, type = "bytes"), error = function(error) {
    NA
  })
  if (is.na(size) || size > .flex_max_source_bytes) {
    max_size <- .flex_max_source_bytes
    flex_source_abort(
      c(
        "Flex source is too large or has an invalid encoding",
        "i" = "The maximum encoded size is {.val {max_size}} bytes."
      ),
      class = "dsprrr_flex_source_size_error",
      size = size,
      max_size = max_size
    )
  }
  if (is.na(iconv(module_src, from = "", to = "UTF-8"))) {
    flex_source_abort(
      "Flex source must contain valid UTF-8 text",
      class = "dsprrr_flex_json_error"
    )
  }

  tryCatch(
    jsonlite::fromJSON(module_src, simplifyVector = FALSE),
    error = function(error) {
      flex_source_abort(
        "{.arg module_src} is not valid JSON",
        class = "dsprrr_flex_json_error",
        parent = error
      )
    }
  )
}

flex_baseline_source <- function(outer_signature) {
  input_names <- names(flex_signature_input_types(outer_signature))
  output_names <- names(flex_signature_output_types(outer_signature))
  inputs <- if (length(input_names) == 0L) {
    stats::setNames(list(), character())
  } else {
    as.list(stats::setNames(paste0("$input.", input_names), input_names))
  }
  outputs <- if (length(output_names) == 0L) {
    stats::setNames(list(), character())
  } else {
    as.list(stats::setNames(
      paste0("$step.predict.", output_names),
      output_names
    ))
  }
  list(
    schema_version = 1L,
    steps = list(list(
      name = "predict",
      primitive = "predict",
      signature = "$outer",
      inputs = inputs
    )),
    outputs = outputs
  )
}

flex_compile_step <- function(step, outer_signature, index) {
  flex_validate_object(
    step,
    required = c("name", "primitive", "signature", "inputs"),
    optional = "instructions",
    context = "Flex step {index}"
  )
  flex_one_string(step$name, "step name")
  if (!grepl("^[A-Za-z][A-Za-z0-9_]{0,63}$", step$name)) {
    flex_source_abort(
      c(
        "Invalid Flex step name {.val {step$name}}",
        "i" = "Use 1-64 ASCII letters, digits, or underscores, starting with a letter."
      ),
      class = "dsprrr_flex_step_name_error"
    )
  }
  flex_one_string(step$primitive, "step primitive")
  if (!step$primitive %in% c("predict", "chain_of_thought")) {
    flex_source_abort(
      "Unknown Flex primitive {.val {step$primitive}}",
      class = "dsprrr_flex_primitive_error"
    )
  }
  flex_one_string(step$signature, "step signature")
  signature_src <- trimws(step$signature)
  if (!nzchar(signature_src)) {
    flex_source_abort(
      "Flex step signatures cannot be empty",
      class = "dsprrr_flex_signature_error"
    )
  }
  step_signature <- if (identical(signature_src, "$outer")) {
    outer_signature
  } else {
    tryCatch(
      parse_signature(signature_src),
      error = function(error) {
        flex_source_abort(
          "Invalid signature for Flex step {.field {step$name}}",
          class = "dsprrr_flex_signature_error",
          parent = error
        )
      }
    )
  }

  has_instructions <- "instructions" %in% names(step)
  instructions <- step$instructions
  if (has_instructions) {
    flex_one_string(instructions, "step instructions", allow_empty = TRUE)
    step_signature <- tryCatch(
      with_instructions(step_signature, instructions),
      error = function(error) {
        flex_source_abort(
          "Invalid instructions for Flex step {.field {step$name}}",
          class = "dsprrr_flex_signature_error",
          parent = error
        )
      }
    )
  }
  for (name in names(flex_signature_input_types(step_signature))) {
    flex_validate_supported_type(
      flex_signature_input_types(step_signature)[[name]],
      paste0("input ", name, " in Flex step ", step$name)
    )
  }
  flex_validate_supported_type(
    step_signature@output_type,
    paste0("output in Flex step ", step$name)
  )
  flex_validate_named_map(
    step$inputs,
    "inputs for Flex step {.field {step$name}}"
  )

  runtime_signature <- if (identical(step$primitive, "chain_of_thought")) {
    tryCatch(
      with_reasoning(step_signature),
      error = function(error) {
        flex_source_abort(
          "Cannot add reasoning to Flex step {.field {step$name}}",
          class = "dsprrr_flex_signature_error",
          parent = error
        )
      }
    )
  } else {
    step_signature
  }

  list(
    name = step$name,
    primitive = step$primitive,
    signature_src = signature_src,
    signature = step_signature,
    instructions = instructions,
    inputs = step$inputs,
    output_types = flex_signature_output_types(runtime_signature)
  )
}

flex_validate_object <- function(
  value,
  required,
  optional = character(),
  context
) {
  if (!is.list(value) || (length(value) > 0L && is.null(names(value)))) {
    flex_source_abort(
      "{context} must be a JSON object",
      class = "dsprrr_flex_schema_error"
    )
  }
  fields <- names(value) %||% character()
  if (any(!nzchar(fields)) || anyDuplicated(fields)) {
    flex_source_abort(
      "{context} must have unique, non-empty field names",
      class = "dsprrr_flex_schema_error"
    )
  }
  missing <- setdiff(required, fields)
  unknown <- setdiff(fields, c(required, optional))
  if (length(missing) > 0L || length(unknown) > 0L) {
    messages <- c("{context} does not match the Flex schema")
    if (length(missing) > 0L) {
      messages <- c(messages, "x" = "Missing fields: {.field {missing}}.")
    }
    if (length(unknown) > 0L) {
      messages <- c(messages, "x" = "Unknown fields: {.field {unknown}}.")
    }
    flex_source_abort(messages, class = "dsprrr_flex_schema_error")
  }
  invisible(value)
}

flex_validate_named_map <- function(value, context) {
  if (!is.list(value) || is.null(names(value))) {
    flex_source_abort(
      "{context} must be a JSON object",
      class = "dsprrr_flex_interface_error"
    )
  }
  fields <- names(value) %||% character()
  if (length(fields) != length(value) || any(!nzchar(fields))) {
    flex_source_abort(
      "{context} must be a JSON object with named fields",
      class = "dsprrr_flex_interface_error"
    )
  }
  if (anyDuplicated(fields)) {
    flex_source_abort(
      "{context} field names must be unique",
      class = "dsprrr_flex_interface_error"
    )
  }
  for (index in seq_along(value)) {
    flex_one_string(value[[index]], "reference in {context}")
  }
  invisible(value)
}

flex_validate_exact_names <- function(value, expected, context) {
  flex_validate_named_map(value, context)
  actual <- names(value) %||% character()
  missing <- setdiff(expected, actual)
  extra <- setdiff(actual, expected)
  if (length(missing) > 0L || length(extra) > 0L) {
    messages <- c("{context} must exactly match its signature")
    if (length(missing) > 0L) {
      messages <- c(messages, "x" = "Missing fields: {.field {missing}}.")
    }
    if (length(extra) > 0L) {
      messages <- c(messages, "x" = "Extra fields: {.field {extra}}.")
    }
    flex_source_abort(messages, class = "dsprrr_flex_interface_error")
  }
  invisible(value)
}

flex_one_string <- function(value, context, allow_empty = FALSE) {
  valid <- is.character(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    (allow_empty || nzchar(value))
  if (!valid) {
    flex_source_abort(
      "{context} must be one {if (allow_empty) '' else 'non-empty '}string",
      class = "dsprrr_flex_schema_error"
    )
  }
  invisible(value)
}

flex_is_integerish <- function(value, expected) {
  is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == expected
}

flex_parse_reference <- function(
  reference,
  outer_signature,
  known_types,
  context
) {
  flex_one_string(reference, "reference in {context}")
  if (startsWith(reference, "$input.")) {
    name <- substring(reference, nchar("$input.") + 1L)
    inputs <- flex_signature_input_types(outer_signature)
    if (!nzchar(name) || !name %in% names(inputs)) {
      flex_source_abort(
        "Unknown outer input reference {.val {reference}} in {context}",
        class = "dsprrr_flex_reference_error"
      )
    }
    return(list(kind = "input", name = name, field = NULL, raw = reference))
  }

  match <- regexec(
    "^\\$step\\.([A-Za-z][A-Za-z0-9_]{0,63})\\.(.+)$",
    reference
  )
  pieces <- regmatches(reference, match)[[1L]]
  if (length(pieces) != 3L) {
    flex_source_abort(
      c(
        "Invalid Flex reference {.val {reference}} in {context}",
        "i" = "Use $input.<name> or $step.<name>.<field>."
      ),
      class = "dsprrr_flex_reference_error"
    )
  }
  step_name <- pieces[[2L]]
  field <- pieces[[3L]]
  if (!step_name %in% names(known_types)) {
    flex_source_abort(
      "Unknown Flex step in reference {.val {reference}}",
      class = "dsprrr_flex_reference_error"
    )
  }
  if (!field %in% names(known_types[[step_name]])) {
    flex_source_abort(
      "Unknown output field in Flex reference {.val {reference}}",
      class = "dsprrr_flex_reference_error"
    )
  }
  list(kind = "step", name = step_name, field = field, raw = reference)
}

flex_reject_cycles <- function(dependencies) {
  state <- stats::setNames(
    rep.int(0L, length(dependencies)),
    names(dependencies)
  )
  visit <- function(name, path = character()) {
    if (state[[name]] == 1L) {
      cycle <- c(path[match(name, path):length(path)], name)
      flex_source_abort(
        "Flex source contains a step cycle: {paste(cycle, collapse = ' -> ')}",
        class = "dsprrr_flex_cycle_error",
        cycle = cycle
      )
    }
    if (state[[name]] == 2L) {
      return(invisible(NULL))
    }
    state[[name]] <<- 1L
    for (dependency in dependencies[[name]]) {
      visit(dependency, c(path, name))
    }
    state[[name]] <<- 2L
    invisible(NULL)
  }
  for (name in names(dependencies)) {
    visit(name)
  }
  invisible(NULL)
}

flex_signature_input_types <- function(signature) {
  inputs <- lapply(signature@inputs, `[[`, "type")
  names(inputs) <- vapply(signature@inputs, `[[`, character(1), "name")
  inputs
}

flex_validate_supported_type <- function(type, context) {
  if (inherits(type, "ellmer::TypeIgnore")) {
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeBasic")) {
    if (!type@type %in% c("string", "number", "integer", "boolean")) {
      flex_source_abort(
        "Unsupported primitive type {.val {type@type}} in {context}",
        class = "dsprrr_flex_type_error"
      )
    }
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeEnum")) {
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeArray")) {
    flex_validate_supported_type(type@items, paste0(context, " array item"))
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeObject")) {
    if (length(type@properties) == 0L) {
      flex_source_abort(
        "Empty object type in {context} has no wireable fields",
        class = "dsprrr_flex_type_error"
      )
    }
    for (name in names(type@properties)) {
      flex_validate_supported_type(
        type@properties[[name]],
        paste0(context, " property ", name)
      )
    }
    return(invisible(NULL))
  }
  flex_source_abort(
    c(
      "Unsupported type family in {context}",
      "i" = "Flex v1 supports string, number, integer, boolean, enum, array, and object types."
    ),
    class = "dsprrr_flex_type_error"
  )
}

flex_signature_output_types <- function(signature) {
  output_type <- signature@output_type
  if (
    methods::.hasSlot(output_type, "properties") &&
      length(output_type@properties) > 0L
  ) {
    properties <- as.list(output_type@properties)
    return(properties[
      !vapply(
        properties,
        inherits,
        logical(1),
        "ellmer::TypeIgnore"
      )
    ])
  }
  stats::setNames(list(output_type), signature_output_field_names(output_type))
}

flex_reference_type <- function(reference, outer_signature, known_types) {
  if (identical(reference$kind, "input")) {
    return(flex_signature_input_types(outer_signature)[[reference$name]])
  }
  known_types[[reference$name]][[reference$field]]
}

flex_type_assignable <- function(source, target) {
  source_required <- tryCatch(isTRUE(source@required), error = function(error) {
    TRUE
  })
  target_required <- tryCatch(isTRUE(target@required), error = function(error) {
    TRUE
  })
  if (target_required && !source_required) {
    return(FALSE)
  }
  source_schema <- ellmer_type_to_json_schema(source)
  target_schema <- ellmer_type_to_json_schema(target)
  flex_schema_assignable(source_schema, target_schema)
}

flex_schema_assignable <- function(source, target) {
  if (is.null(source) || is.null(target)) {
    return(identical(source, target))
  }
  source_type <- source$type %||% NA_character_
  target_type <- target$type %||% NA_character_
  if (identical(source_type, "integer") && identical(target_type, "number")) {
    return(TRUE)
  }
  if (!identical(source_type, target_type)) {
    return(FALSE)
  }

  source_enum <- source$enum
  target_enum <- target$enum
  if (!is.null(target_enum)) {
    if (is.null(source_enum) || !all(source_enum %in% target_enum)) {
      return(FALSE)
    }
  }
  if (identical(source_type, "array")) {
    return(flex_schema_assignable(source$items, target$items))
  }
  if (identical(source_type, "object")) {
    source_properties <- source$properties %||% list()
    target_properties <- target$properties %||% list()
    if (!setequal(names(source_properties), names(target_properties))) {
      return(FALSE)
    }
    source_required <- source$required %||% character()
    target_required <- target$required %||% character()
    if (!all(target_required %in% source_required)) {
      return(FALSE)
    }
    if (
      !isTRUE(target$additionalProperties) &&
        isTRUE(source$additionalProperties)
    ) {
      return(FALSE)
    }
    return(all(vapply(
      names(target_properties),
      function(name) {
        flex_schema_assignable(
          source_properties[[name]],
          target_properties[[name]]
        )
      },
      logical(1)
    )))
  }
  TRUE
}

flex_canonical_json <- function(value) {
  as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    digits = NA,
    pretty = FALSE
  ))
}

flex_resolve_reference <- function(reference, inputs, values) {
  if (identical(reference$kind, "input")) {
    return(inputs[[reference$name]])
  }
  values[[reference$name]][[reference$field]]
}

flex_output_abort <- function(message, class, ..., .envir = parent.frame()) {
  cli::cli_abort(
    message,
    class = unique(c(
      class,
      "dsprrr_flex_output_error",
      "dsprrr_flex_runtime_error"
    )),
    .envir = .envir,
    ...
  )
}

flex_runtime_validation_abort <- function(
  message,
  class,
  kind,
  context,
  ...,
  .envir = parent.frame()
) {
  if (identical(kind, "output")) {
    flex_output_abort(
      message,
      class = class,
      context = context,
      ...,
      .envir = .envir
    )
  } else {
    cli::cli_abort(
      message,
      class = unique(c(class, "dsprrr_flex_runtime_error")),
      context = context,
      .envir = .envir,
      ...
    )
  }
}

flex_strip_ignored_runtime_values <- function(
  value,
  type,
  context,
  class,
  kind = "output"
) {
  if (is.null(value) || inherits(type, "ellmer::TypeIgnore")) {
    return(value)
  }
  if (inherits(type, "ellmer::TypeArray")) {
    if (is.data.frame(value) && inherits(type@items, "ellmer::TypeObject")) {
      return(flex_strip_ignored_runtime_values(
        value,
        type@items,
        context,
        class,
        kind
      ))
    }
    if (is.list(value) && is.null(names(value))) {
      return(lapply(seq_along(value), function(index) {
        flex_strip_ignored_runtime_values(
          value[[index]],
          type@items,
          paste0(context, "[[", index, "]]"),
          class,
          kind
        )
      }))
    }
    return(value)
  }
  if (!inherits(type, "ellmer::TypeObject")) {
    return(value)
  }

  properties <- as.list(type@properties)
  ignored <- names(properties)[vapply(
    properties,
    inherits,
    logical(1),
    "ellmer::TypeIgnore"
  )]
  if (is.data.frame(value)) {
    for (field in intersect(ignored, names(value))) {
      column <- value[[field]]
      non_null <- if (is.list(column)) {
        vapply(column, Negate(is.null), logical(1))
      } else {
        rep(TRUE, length(column))
      }
      if (any(non_null)) {
        flex_runtime_validation_abort(
          "{context}${field} is ignored and must be NULL",
          class,
          kind,
          context,
          field = field
        )
      }
    }
    value <- value[setdiff(names(value), ignored)]
    for (field in intersect(names(properties), names(value))) {
      property <- properties[[field]]
      column <- value[[field]]
      if (inherits(property, "ellmer::TypeObject") && is.data.frame(column)) {
        value[[field]] <- flex_strip_ignored_runtime_values(
          column,
          property,
          paste0(context, "$", field),
          class,
          kind
        )
      } else if (inherits(property, "ellmer::TypeArray") && is.list(column)) {
        value[[field]] <- lapply(seq_along(column), function(index) {
          flex_strip_ignored_runtime_values(
            column[[index]],
            property,
            paste0(context, "$", field, "[[", index, "]]"),
            class,
            kind
          )
        })
      }
    }
    return(value)
  }
  if (!is.list(value) || (length(value) > 0L && is.null(names(value)))) {
    return(value)
  }
  for (field in intersect(ignored, names(value))) {
    if (!is.null(value[[field]])) {
      flex_runtime_validation_abort(
        "{context}${field} is ignored and must be NULL",
        class,
        kind,
        context,
        field = field
      )
    }
  }
  value <- value[setdiff(names(value), ignored)]
  for (field in intersect(names(properties), names(value))) {
    value[field] <- list(flex_strip_ignored_runtime_values(
      value[[field]],
      properties[[field]],
      paste0(context, "$", field),
      class,
      kind
    ))
  }
  if (length(value) == 0L) {
    names(value) <- character()
  }
  value
}

flex_data_frame_object_row <- function(
  value,
  index,
  type,
  context,
  class,
  kind
) {
  properties <- as.list(type@properties)
  ignored <- names(properties)[vapply(
    properties,
    inherits,
    logical(1),
    "ellmer::TypeIgnore"
  )]
  visible <- properties[setdiff(names(properties), ignored)]
  required <- names(visible)[vapply(
    visible,
    function(property) isTRUE(property@required),
    logical(1)
  )]
  missing <- setdiff(required, names(value))
  extra <- setdiff(names(value), names(properties))
  if (length(missing) > 0L) {
    flex_runtime_validation_abort(
      "{context} omitted required fields: {.field {missing}}",
      class,
      kind,
      context,
      missing = missing
    )
  }
  if (length(extra) > 0L && !isTRUE(type@additional_properties)) {
    flex_runtime_validation_abort(
      "{context} included unknown fields: {.field {extra}}",
      class,
      kind,
      context,
      extra = extra
    )
  }
  for (field in intersect(names(visible), names(value))) {
    property <- visible[[field]]
    column <- value[[field]]
    if (inherits(property, "ellmer::TypeObject") && is.data.frame(column)) {
      if (
        !isTRUE(property@required) &&
          isTRUE(ellmer_parallel_object_row_missing(column, index, property))
      ) {
        next
      }
      flex_data_frame_object_row(
        column,
        index,
        property,
        paste0(context, "$", field),
        class,
        kind
      )
      next
    }
    if (index > length(column)) {
      flex_runtime_validation_abort(
        "{context}${field} ended before row {index}",
        class,
        kind,
        context,
        field = field,
        index = index
      )
    }
    cell <- column[[index]]
    if (
      !isTRUE(property@required) &&
        length(cell) == 1L &&
        is.atomic(cell) &&
        is.na(cell)
    ) {
      cell <- NULL
    }
    flex_validate_runtime_value(
      cell,
      property,
      paste0(context, "$", field),
      class,
      kind = kind
    )
  }
  visible_type <- type
  visible_type@properties <- visible
  tryCatch(
    ellmer_parallel_response_row(value, index, visible_type),
    error = function(error) {
      flex_runtime_validation_abort(
        "Could not reconstruct {context}",
        class,
        kind,
        context,
        parent = error
      )
    }
  )
}

flex_validate_runtime_value <- function(
  value,
  type,
  context,
  class,
  kind = c("output", "input")
) {
  kind <- match.arg(kind)
  abort_value <- function(message, ...) {
    caller_env <- parent.frame()
    flex_runtime_validation_abort(
      message,
      class = class,
      kind = kind,
      context = context,
      ...,
      .envir = caller_env
    )
  }
  if (inherits(type, "ellmer::TypeIgnore")) {
    return(invisible(NULL))
  }
  required <- tryCatch(isTRUE(type@required), error = function(error) TRUE)
  if (is.null(value)) {
    if (required) {
      abort_value("{context} is required and cannot be NULL")
    }
    return(invisible(NULL))
  }

  if (inherits(type, "ellmer::TypeBasic")) {
    type_name <- type@type
    valid <- switch(
      type_name,
      string = is.character(value) && length(value) == 1L && !is.na(value),
      number = is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value),
      integer = is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value) &&
        value == floor(value),
      boolean = is.logical(value) && length(value) == 1L && !is.na(value),
      FALSE
    )
    if (!valid) {
      abort_value(
        "{context} must be one non-missing {.val {type_name}} value",
        type_name = type_name
      )
    }
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeEnum")) {
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !value %in% type@values
    ) {
      allowed <- type@values
      abort_value(
        "{context} must be one of {.val {allowed}}",
        allowed = allowed
      )
    }
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeArray")) {
    if (is.data.frame(value)) {
      if (!inherits(type@items, "ellmer::TypeObject")) {
        abort_value("{context} must be an array, not a data frame")
      }
      for (index in seq_len(nrow(value))) {
        record <- flex_data_frame_object_row(
          value,
          index,
          type@items,
          paste0(context, "[[", index, "]]"),
          class,
          kind
        )
        flex_validate_runtime_value(
          record,
          type@items,
          paste0(context, "[[", index, "]]"),
          class,
          kind = kind
        )
      }
      return(invisible(NULL))
    }
    if (
      !(is.atomic(value) || is.list(value)) ||
        (!is.null(names(value)) && any(nzchar(names(value))))
    ) {
      abort_value("{context} must be an unnamed array")
    }
    for (index in seq_along(value)) {
      flex_validate_runtime_value(
        value[[index]],
        type@items,
        paste0(context, "[[", index, "]]"),
        class,
        kind = kind
      )
    }
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeObject")) {
    if (!is.list(value) || is.data.frame(value) || is.null(names(value))) {
      abort_value("{context} must be a named object")
    }
    fields <- names(value)
    if (any(!nzchar(fields)) || anyDuplicated(fields)) {
      abort_value("{context} must have unique, non-empty field names")
    }
    properties <- as.list(type@properties)
    properties <- properties[
      !vapply(
        properties,
        inherits,
        logical(1),
        "ellmer::TypeIgnore"
      )
    ]
    required_fields <- names(properties)[vapply(
      properties,
      function(property) isTRUE(property@required),
      logical(1)
    )]
    missing <- setdiff(required_fields, fields)
    if (length(missing) > 0L) {
      abort_value(
        "{context} omitted required fields: {.field {missing}}",
        missing = missing
      )
    }
    extra <- setdiff(fields, names(properties))
    if (length(extra) > 0L && !isTRUE(type@additional_properties)) {
      abort_value(
        "{context} included unknown fields: {.field {extra}}",
        extra = extra
      )
    }
    for (field in intersect(names(properties), fields)) {
      flex_validate_runtime_value(
        value[[field]],
        properties[[field]],
        paste0(context, "$", field),
        class,
        kind = kind
      )
    }
    return(invisible(NULL))
  }

  abort_value("{context} uses an unsupported runtime type")
}

flex_validate_output_record <- function(
  value,
  output_types,
  context,
  class,
  scalar = TRUE
) {
  fields <- names(output_types)
  if (scalar && length(fields) == 1L) {
    value <- stats::setNames(list(value), fields)
  } else if (
    scalar &&
      length(fields) == 0L &&
      (is.null(value) || (is.list(value) && length(value) == 0L))
  ) {
    value <- stats::setNames(list(), character())
  }
  if (!is.list(value) || is.null(names(value))) {
    flex_output_abort(
      "{context} must be a named output record",
      class = class
    )
  }
  actual <- names(value)
  if (any(!nzchar(actual)) || anyDuplicated(actual)) {
    flex_output_abort(
      "{context} must have unique, non-empty output names",
      class = class
    )
  }
  required_fields <- fields[vapply(
    output_types,
    function(type) isTRUE(type@required),
    logical(1)
  )]
  missing <- setdiff(required_fields, actual)
  extra <- setdiff(actual, fields)
  if (length(missing) > 0L || length(extra) > 0L) {
    messages <- c("{context} does not match its declared output fields")
    if (length(missing) > 0L) {
      messages <- c(messages, "x" = "Missing required: {.field {missing}}.")
    }
    if (length(extra) > 0L) {
      messages <- c(messages, "x" = "Unknown: {.field {extra}}.")
    }
    flex_output_abort(messages, class = class)
  }
  present <- intersect(fields, actual)
  value <- value[present]
  for (field in present) {
    flex_validate_runtime_value(
      value[[field]],
      output_types[[field]],
      paste0(context, "$", field),
      class
    )
  }
  value
}

flex_output_record <- function(value, output_types, step_name, output_type) {
  context <- paste0("Flex step ", step_name, " output")
  value <- flex_strip_ignored_runtime_values(
    value,
    output_type,
    context,
    class = "dsprrr_flex_step_output_error"
  )
  flex_validate_output_record(
    value,
    output_types,
    context = context,
    class = "dsprrr_flex_step_output_error",
    scalar = !inherits(output_type, "ellmer::TypeObject")
  )
}

flex_aggregate_step_usage <- function(step_metadata) {
  metadata <- lapply(step_metadata, `[[`, "metadata")
  sum_field <- function(field, integer = FALSE) {
    values <- vapply(
      metadata,
      function(item) as.numeric(item[[field]] %||% NA_real_),
      numeric(1)
    )
    value <- if (anyNA(values)) NA_real_ else sum(values)
    if (integer) as.integer(value) else value
  }
  list(
    input_tokens = sum_field("input_tokens", integer = TRUE),
    output_tokens = sum_field("output_tokens", integer = TRUE),
    cached_input_tokens = sum_field("cached_input_tokens", integer = TRUE),
    total_tokens = sum_field("total_tokens", integer = TRUE),
    cost = sum_field("cost")
  )
}
