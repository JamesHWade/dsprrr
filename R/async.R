#' Asynchronous Module Operations
#'
#' @description
#' Functions for running modules asynchronously using promises.
#' Useful for parallel execution or non-blocking operations.
#' The direct provider paths support ordinary `PredictModule` objects.
#' ProgramOfThought, CodeAct, and RLM modules may also use [run_async()] when
#' configured with `interpreter_factory`: the complete workflow runs in an
#' isolated mirai process with one invocation-owned interpreter. Their
#' constructor-bound caller-owned runners and all specialized streaming paths
#' remain rejected.
#'
#' @name async
NULL

#' Run a module asynchronously
#'
#' @description
#' Executes a module and returns a promise that resolves to the result.
#' Useful for running multiple modules in parallel.
#' Ordinary `PredictModule` objects use the provider's native async path.
#' ProgramOfThought, CodeAct, and RLM modules use an isolated background process
#' when configured with `interpreter_factory`. Caller-owned runners are rejected
#' because they cannot be safely shared across concurrent invocations.
#'
#' @param module A dsprrr Module object
#' @param ... Named inputs matching the module's signature
#' @param .llm Optional ellmer Chat object
#'
#' @return A promise that resolves to the structured output
#'
#' @export
#' @examples
#' \dontrun{
#' # Run multiple modules in parallel
#' promises <- list(
#'   run_async(mod1, question = "Q1"),
#'   run_async(mod2, question = "Q2"),
#'   run_async(mod3, question = "Q3")
#' )
#'
#' # Wait for all to complete
#' promises::promise_all(.list = promises) |>
#'   promises::then(function(results) {
#'     # Process results
#'   })
#' }
run_async <- function(module, ..., .llm = NULL) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr Module object")
  }
  if (interpreter_workflow_module(module)) {
    assert_factory_interpreter_async_supported(module, "run_async")
    inputs <- list(...)
    validate_signature_inputs(
      module$signature,
      inputs,
      missing = "error",
      extra = "warn",
      type = "warn",
      context = "inputs"
    )
    return(run_factory_interpreter_async(module, inputs, .llm = .llm))
  }
  assert_direct_provider_async_supported(module, "run_async")

  inputs <- list(...)
  request <- build_module_request(module, inputs)
  llm <- resolve_module_llm(module, .llm = .llm)

  # Use ellmer's async method
  llm$chat_structured_async(
    request$payload,
    type = module$signature@output_type
  )
}


interpreter_workflow_module <- function(module) {
  inherits(
    module,
    c("ProgramOfThoughtModule", "CodeActModule", "RLMModule")
  )
}


factory_interpreter_module <- function(module) {
  interpreter_workflow_module(module) &&
    is.null(module$runner) &&
    is.function(module$interpreter_factory)
}


assert_factory_interpreter_async_supported <- function(module, operation) {
  if (factory_interpreter_module(module)) {
    return(invisible(module))
  }
  cli::cli_abort(
    c(
      "{.fn {operation}} cannot reuse a caller-owned interpreter",
      "x" = "{.cls {class(module)[1L]}} is bound to a persistent runner object.",
      "i" = "Configure {.arg interpreter_factory} so every async invocation owns a fresh interpreter."
    ),
    class = c(
      "dsprrr_interpreter_concurrency_unsafe",
      "dsprrr_specialized_async_unsupported"
    ),
    operation = operation,
    module_class = class(module)[1L],
    module_path = "$"
  )
}


run_factory_interpreter_async <- function(module, inputs, .llm = NULL) {
  rlang::check_installed("promises", reason = "for asynchronous execution")
  llm <- .llm %||% module$chat %||% get_default_chat()
  if (is.null(llm)) {
    cli::cli_abort("No LLM provided. Pass .llm or set a default chat.")
  }

  namespace_path <- getNamespaceInfo(asNamespace("dsprrr"), "path")
  worker <- function(module, inputs, llm, namespace_path) {
    if (
      file.exists(file.path(namespace_path, "R", "module-base.R")) &&
        requireNamespace("pkgload", quietly = TRUE)
    ) {
      pkgload::load_all(namespace_path, quiet = TRUE)
    } else {
      loadNamespace("dsprrr")
    }
    result <- module$forward(
      inputs,
      .llm = llm,
      trace = FALSE,
      .cache = FALSE
    )
    result$output[[1L]]
  }

  profile <- new_dsprrr_mirai_profile()
  profile_owned <- FALSE
  task <- NULL
  cleanup_profile <- function(strict = TRUE) {
    if (!profile_owned) {
      return(invisible(TRUE))
    }
    stopped <- shutdown_dsprrr_mirai_profile(
      profile = profile,
      tasks = if (is.null(task)) list() else list(task),
      strict = strict
    )
    if (isTRUE(stopped)) {
      profile_owned <<- FALSE
    }
    invisible(stopped)
  }
  cleanup_after_failure <- function() {
    cleaned <- tryCatch(
      cleanup_profile(strict = FALSE),
      error = function(error) FALSE
    )
    if (!isTRUE(cleaned)) {
      warn_mirai_teardown_failure(profile)
    }
    invisible(cleaned)
  }
  abort_launch <- function(error) {
    cleanup_after_failure()
    cli::cli_abort(
      c(
        "Could not launch the isolated interpreter workflow",
        "x" = conditionMessage(error)
      ),
      class = "dsprrr_interpreter_async_launch_error",
      parent = error
    )
  }

  tryCatch(
    {
      # The profile name was allocated for this invocation. Mark ownership
      # before launch so a partially created pool is still torn down if
      # `daemons()` signals after acquiring resources.
      profile_owned <- TRUE
      mirai::daemons(
        n = 1L,
        dispatcher = TRUE,
        .compute = profile
      )
      task <- mirai::mirai(
        worker(module, inputs, llm, namespace_path),
        .args = list(
          worker = worker,
          module = module,
          inputs = inputs,
          llm = llm,
          namespace_path = namespace_path
        ),
        .compute = profile
      )
    },
    interrupt = function(condition) {
      cleanup_after_failure()
      stop(condition)
    },
    error = abort_launch
  )

  promise <- tryCatch(
    promises::as.promise(task),
    interrupt = function(condition) {
      cleanup_after_failure()
      stop(condition)
    },
    error = abort_launch
  )
  promises::then(
    promise,
    onFulfilled = function(value) {
      cleanup_profile(strict = TRUE)
      value
    },
    onRejected = function(error) {
      cleanup_after_failure()
      stop(error)
    }
  )
}

#' Stream module output asynchronously
#'
#' @description
#' Streams text output from a module asynchronously.
#' Returns a promise that resolves to an async generator.
#' Only ordinary `PredictModule` objects are supported. Modules and composites
#' with specialized `forward()` semantics are rejected before provider work;
#' use [run()] for their complete workflows.
#'
#' @param module A dsprrr Module object
#' @param ... Named inputs matching the module's signature
#' @param .llm Optional ellmer Chat object
#'
#' @return A promise that resolves to an async generator
#'
#' @export
#' @examples
#' \dontrun{
#' stream_async(mod, question = "Write a story") |>
#'   promises::then(function(gen) {
#'     # Process async generator
#'   })
#' }
stream_async <- function(module, ..., .llm = NULL) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr Module object")
  }
  assert_direct_provider_async_supported(module, "stream_async")

  inputs <- list(...)
  request <- build_module_request(module, inputs)
  llm <- resolve_module_llm(module, .llm = .llm)

  # Use ellmer's async stream method
  llm$stream_async(request$payload)
}

#' Create a Stream Listener for a Module Output Field
#'
#' @description
#' Creates a listener that receives streamed content for a specific output
#' field during [run_stream()]. This mirrors DSPy's `StreamListener`:
#' attach a callback to the field you care about (e.g., `"answer"`) and it
#' fires as content for that field is produced — token by token when the
#' field can be token-streamed, or once with the complete value otherwise.
#'
#' @details
#' Token-level streaming is available when a module's output is a single
#' string field. In that case dsprrr streams the response as plain text and
#' treats the accumulated text as the field's value. Modules with multiple
#' or non-string output fields run normally and fire each matching listener
#' once with the completed value (chunked streaming of structured output is
#' not supported by the underlying structured-output API).
#'
#' @param field Name of the output field to listen to (a single string).
#' @param callback A function called with each chunk of text (a single
#'   string). For non-streamable fields, called once with the full value.
#'
#' @return A `dsprrr_stream_listener` object for use with [run_stream()].
#' @export
#' @examples
#' \dontrun{
#' listener <- stream_listener("answer", function(chunk) cat(chunk))
#' run_stream(mod, question = "Tell me a story", listeners = list(listener))
#' }
stream_listener <- function(field, callback) {
  if (!is.character(field) || length(field) != 1 || !nzchar(field)) {
    cli::cli_abort("{.arg field} must be a single non-empty string")
  }
  if (!is.function(callback)) {
    cli::cli_abort("{.arg callback} must be a function")
  }

  structure(
    list(field = field, callback = callback),
    class = "dsprrr_stream_listener"
  )
}

#' Run a Module with Streaming Listeners and Status Events
#'
#' @description
#' Executes a module (or pipeline) while streaming output to per-field
#' listeners and emitting status events. This is dsprrr's analogue of
#' DSPy's `streamify()`: use it to surface intermediate progress and
#' incremental output in Shiny apps or console tools.
#'
#' For pipelines, a status event is emitted as each step starts and ends,
#' and listeners fire for matching fields at any step — not just the final
#' one.
#'
#' @details
#' ## Streaming behavior
#'
#' - Modules whose output is a single string field are token-streamed:
#'   matching listeners receive text chunks as they arrive, and the
#'   accumulated text becomes the field's value. Token streaming uses the
#'   provider's text mode and requires the `coro` package.
#' - Modules with multiple or non-string output fields run normally;
#'   matching listeners fire once with the completed value.
#' - Streaming execution does not record traces and bypasses the response
#'   cache.
#'
#' ## Specialized modules
#'
#' One-shot fallback execution uses each module's own `forward()` method, so it
#' remains available to specialized modules when no matching token listener is
#' active or the output is not token-streamable. Actual token streaming uses a
#' direct provider path and is limited to ordinary `PredictModule` steps.
#' Unsupported token-stream requests are rejected before provider work.
#'
#' ## Status events
#'
#' When `on_status` is provided, it is called with a list describing each
#' event:
#' - `type`: one of `"step_start"`, `"field_start"`, `"field_end"`,
#'   `"field_complete"`, `"step_end"`
#' - `step`, `n_steps`: position within the pipeline (both `1` for a
#'   single module)
#' - `module`: class name of the executing module
#' - `field`: the output field name (field events only)
#'
#' @param module A dsprrr Module or pipeline.
#' @param ... Named inputs matching the module's signature.
#' @param .llm Optional ellmer Chat object.
#' @param listeners A [stream_listener()] or list of them.
#' @param on_status Optional function called with status event lists.
#'
#' @return The final output (named list for structured outputs, character
#'   for plain string outputs), invisibly.
#' @export
#' @examples
#' \dontrun{
#' sig <- signature("question -> answer")
#' mod <- module(sig, type = "predict")
#'
#' run_stream(
#'   mod,
#'   question = "Tell me a story",
#'   .llm = ellmer::chat_openai(),
#'   listeners = stream_listener("answer", function(chunk) cat(chunk)),
#'   on_status = function(ev) message("[", ev$type, "] step ", ev$step)
#' )
#' }
run_stream <- function(
  module,
  ...,
  .llm = NULL,
  listeners = list(),
  on_status = NULL
) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr Module object")
  }
  listeners <- normalize_stream_listeners(listeners)

  if (!is.null(on_status) && !is.function(on_status)) {
    cli::cli_abort("{.arg on_status} must be a function or NULL")
  }

  assert_run_stream_token_supported(module, listeners)

  inputs <- list(...)

  if (inherits(module, "PipelineModule")) {
    result <- module$forward_stream(
      inputs,
      .llm = .llm,
      listeners = listeners,
      on_status = on_status
    )
  } else {
    result <- stream_module_step(
      module,
      inputs,
      .llm = .llm,
      listeners = listeners,
      on_status = on_status,
      step = 1L,
      n_steps = 1L
    )
  }

  invisible(result)
}

#' Normalize the listeners argument to a list of stream listeners
#' @noRd
normalize_stream_listeners <- function(listeners) {
  if (inherits(listeners, "dsprrr_stream_listener")) {
    listeners <- list(listeners)
  }
  if (!is.list(listeners)) {
    cli::cli_abort(
      "{.arg listeners} must be a {.fn stream_listener} or a list of them"
    )
  }
  for (l in listeners) {
    if (!inherits(l, "dsprrr_stream_listener")) {
      cli::cli_abort(c(
        "All listeners must be created with {.fn stream_listener}",
        "x" = "Got {.cls {class(l)[1]}}"
      ))
    }
  }
  listeners
}

#' Identify the streamable output field of a signature, if any
#'
#' Token streaming is only possible when the output is a single string
#' field: either a bare string type or an object type with exactly one
#' string property.
#'
#' @return A list with `field` (name) and `wrap` (whether the output should
#'   be wrapped in a named list), or NULL when not token-streamable.
#' @noRd
streamable_output_field <- function(output_type) {
  if (
    inherits(output_type, "ellmer::TypeBasic") &&
      identical(output_type@type, "string")
  ) {
    return(list(field = "output", wrap = FALSE))
  }

  if (inherits(output_type, "ellmer::TypeObject")) {
    props <- output_type@properties
    if (length(props) == 1) {
      prop <- props[[1]]
      if (
        inherits(prop, "ellmer::TypeBasic") && identical(prop@type, "string")
      ) {
        return(list(field = names(props)[1], wrap = TRUE))
      }
    }
  }

  NULL
}

#' Emit a status event if a handler is registered
#' @noRd
emit_stream_status <- function(on_status, ...) {
  if (!is.null(on_status)) {
    on_status(list(...))
  }
  invisible(NULL)
}

#' Execute one module while streaming to listeners
#'
#' Shared by run_stream() (single modules) and PipelineModule$forward_stream()
#' (each step). Token-streams single-string-field outputs when a listener
#' matches; otherwise falls back to a normal forward pass and fires matching
#' listeners once with the completed value.
#'
#' @return The module's output value (named list or plain value).
#' @noRd
stream_module_step <- function(
  module,
  inputs,
  .llm = NULL,
  listeners = list(),
  on_status = NULL,
  step = 1L,
  n_steps = 1L
) {
  module_class <- class(module)[1]
  emit_stream_status(
    on_status,
    type = "step_start",
    step = step,
    n_steps = n_steps,
    module = module_class
  )

  streamable <- streamable_output_field(module$signature@output_type)
  matching <- if (!is.null(streamable)) {
    Filter(function(l) identical(l$field, streamable$field), listeners)
  } else {
    list()
  }

  can_stream <- length(matching) > 0 &&
    is.function(module$stream) &&
    rlang::is_installed("coro")

  if (length(matching) > 0 && !can_stream && !rlang::is_installed("coro")) {
    cli::cli_warn(c(
      "Token streaming requires the {.pkg coro} package",
      "i" = "Falling back to non-streaming execution",
      "i" = "Install with {.code install.packages('coro')}"
    ))
  }

  if (can_stream) {
    assert_direct_provider_async_supported(module, "run_stream")
    field <- streamable$field
    emit_stream_status(
      on_status,
      type = "field_start",
      step = step,
      n_steps = n_steps,
      module = module_class,
      field = field
    )

    chunk_callback <- function(chunk) {
      for (l in matching) {
        l$callback(chunk)
      }
    }

    text <- do.call(
      module$stream,
      c(inputs, list(.llm = .llm, callback = chunk_callback))
    )

    emit_stream_status(
      on_status,
      type = "field_end",
      step = step,
      n_steps = n_steps,
      module = module_class,
      field = field
    )

    output <- if (streamable$wrap) {
      stats::setNames(list(text), field)
    } else {
      text
    }
  } else {
    # Streaming bypasses the response cache even on the fallback path
    result <- module$forward(
      inputs,
      .llm = .llm,
      trace = FALSE,
      .cache = FALSE
    )
    output <- result$output[[1]]

    # Fire matching listeners once with the completed field values
    if (length(listeners) > 0) {
      completed_fields <- character(0)
      for (l in listeners) {
        value <- if (is.list(output) && l$field %in% names(output)) {
          output[[l$field]]
        } else if (!is.list(output) && identical(l$field, "output")) {
          output
        } else {
          NULL
        }
        if (!is.null(value)) {
          l$callback(paste(as.character(value), collapse = ""))
          completed_fields <- union(completed_fields, l$field)
        }
      }
      # One field_complete event per field, regardless of listener count
      for (field in completed_fields) {
        emit_stream_status(
          on_status,
          type = "field_complete",
          step = step,
          n_steps = n_steps,
          module = module_class,
          field = field
        )
      }
    }
  }

  emit_stream_status(
    on_status,
    type = "step_end",
    step = step,
    n_steps = n_steps,
    module = module_class
  )

  output
}

#' Whether a module may use the direct provider async/stream path
#'
#' Exact Predict modules use the same request assembly as their forward method.
#' Every subclass and composite fails closed because it may override forward()
#' with retrieval, tools, code execution, repeated calls, or graph traversal.
#' @noRd
direct_provider_async_supported <- function(module) {
  identical(class(module)[1L], "PredictModule")
}

#' Reject operations that would bypass a module's forward method
#' @noRd
assert_direct_provider_async_supported <- function(
  module,
  operation,
  module_path = "$"
) {
  if (direct_provider_async_supported(module)) {
    return(invisible(module))
  }

  module_class <- class(module)[1L]
  cli::cli_abort(
    c(
      "{.fn {operation}} is not supported for {.cls {module_class}}",
      "x" = "The direct provider path would bypass this module's specialized execution workflow.",
      "i" = "The unsupported module is at graph path {.code {module_path}}.",
      "i" = "Use {.fn run} for the complete workflow."
    ),
    class = c(
      "dsprrr_specialized_async_unsupported",
      "dsprrr_async_unsupported_module"
    ),
    operation = operation,
    module_class = module_class,
    module_path = module_path
  )
}

#' Preflight token-streaming requests across pipeline steps
#'
#' One-shot run_stream() fallback is safe because it calls forward(). Only
#' modules that would actually enter the inherited direct-provider stream path
#' need the exact-Predict restriction. Preflighting pipelines prevents an
#' earlier step from reaching a provider before a later unsafe step is found.
#' @noRd
assert_run_stream_token_supported <- function(module, listeners) {
  if (length(listeners) == 0L || !rlang::is_installed("coro")) {
    return(invisible(module))
  }

  modules <- if (inherits(module, "PipelineModule")) {
    stats::setNames(
      lapply(module$steps, function(step) step@module),
      paste0("$/steps/", seq_along(module$steps))
    )
  } else {
    stats::setNames(list(module), "$")
  }

  for (module_path in names(modules)) {
    candidate <- modules[[module_path]]
    streamable <- streamable_output_field(candidate$signature@output_type)
    matching <- !is.null(streamable) &&
      any(vapply(
        listeners,
        function(listener) identical(listener$field, streamable$field),
        logical(1)
      ))
    if (
      matching &&
        is.function(candidate$stream) &&
        !direct_provider_async_supported(candidate)
    ) {
      assert_direct_provider_async_supported(
        candidate,
        "run_stream",
        module_path = module_path
      )
    }
  }

  invisible(module)
}

#' Build a simple prompt from inputs
#'
#' @description
#' Helper function to build a prompt from inputs without accessing
#' private module methods. Used by async functions.
#'
#' @param inputs Named list of input values
#' @param input_specs List of input specifications from signature
#'
#' @return Character string prompt
#'
#' @keywords internal
#' @noRd
build_simple_prompt <- function(inputs, input_specs) {
  if (length(input_specs) == 0) {
    return("")
  }

  input_lines <- character()
  for (spec in input_specs) {
    name <- spec$name
    if (name %in% names(inputs)) {
      value <- inputs[[name]]
      input_lines <- c(input_lines, paste0(name, ": ", value))
    }
  }

  if (length(input_lines) > 0) {
    paste(c("Input:", input_lines), collapse = "\n")
  } else {
    ""
  }
}
