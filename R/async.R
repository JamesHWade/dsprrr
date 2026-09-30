#' Asynchronous and streaming execution
#'
#' The user-facing functions are [run_async()], [stream_async()],
#' [run_stream()] and [stream_listener()].
#'
#' @name async
#' @noRd
NULL

#' Run a module asynchronously
#'
#' @description
#' `run_async()` starts one call of a module and returns a promise instead of
#' waiting for the result, so a Shiny app or other event loop stays
#' responsive and several calls can be in flight at once. Handle the result
#' with the promises package, for example `promises::then()`.
#'
#' @param module A prediction module from [module()] or [chain_of_thought()],
#'   or a [program_of_thought()], [code_act()] or [rlm_module()] module
#'   configured with `interpreter_factory`.
#' @param ... Inputs named after the signature's input fields (single
#'   values).
#' @param .llm An ellmer Chat; see [run()] for how it is chosen when
#'   omitted. Give concurrent calls separate chats, for example with
#'   `llm$clone()`.
#' @param .trace_context A named, JSON-compatible list, as in [run()]. The
#'   promise carries it in its `dsprrr_trace_context` attribute.
#'
#' @details
#' Prediction modules call ellmer's `chat_structured_async()` directly: the
#' call does not use the response cache and records no trace or prompt
#' history. Code-running modules run their whole workflow in a separate mirai
#' process with a fresh interpreter; modules bound to a caller-owned `runner`
#' are rejected, because one runner cannot serve concurrent calls. Other
#' modules, such as [react()] or pipelines, are rejected; use [run()].
#'
#' @return A promise that resolves to the module's output, the same value
#'   that [run()] returns by default.
#'
#' @export
#' @family execution
#' @examples
#' \dontrun{
#' llm <- ellmer::chat_openai(model = "gpt-6-luna")
#' summarize <- module(signature("text -> summary"))
#'
#' first <- run_async(summarize, text = "First article ...", .llm = llm$clone())
#' second <- run_async(summarize, text = "Second article ...", .llm = llm$clone())
#'
#' promises::promise_all(first, second) |>
#'   promises::then(function(results) {
#'     vapply(results, function(r) r$summary, character(1))
#'   })
#' }
run_async <- function(module, ..., .llm = NULL, .trace_context = list()) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr Module object")
  }
  trace_context_supplied <- !missing(.trace_context)
  trace_context <- trace_context_resolve(
    .trace_context,
    supplied = trace_context_supplied
  )
  previous_trace_context <- trace_context_enter(
    trace_context,
    program = module,
    inherit_program_id = !trace_context_supplied
  )
  on.exit(trace_context_restore(previous_trace_context), add = TRUE)
  invocation_trace_fields <- trace_context_fields()
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
    result <- run_factory_interpreter_async(module, inputs, .llm = .llm)
    attr(result, "dsprrr_trace_context") <- invocation_trace_fields
    return(result)
  }
  assert_direct_provider_async_supported(module, "run_async")

  inputs <- list(...)
  request <- build_module_request(module, inputs)
  llm <- resolve_module_llm(module, .llm = .llm)

  # Use ellmer's async method. Decision fields request evidence and are
  # decoded when the promise resolves.
  decisions <- module_decisions(module)
  args <- c(
    prompt_parts(request$payload),
    list(
      type = decision_request_type(module$signature@output_type, decisions)
    )
  )
  args$run_context <- chat_run_context(llm, "chat_structured_async")
  result <- do.call(llm$chat_structured_async, args)
  if (length(decisions) > 0L) {
    rlang::check_installed("promises", reason = "for asynchronous execution")
    result <- promises::then(result, function(response) {
      decision_decode_response(response, decisions)$output
    })
  }
  attr(result, "dsprrr_trace_context") <- invocation_trace_fields
  result
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
  llm <- assert_ellmer_chat(llm, arg = ".llm")

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

#' Stream a module's text output asynchronously
#'
#' @description
#' `stream_async()` sends a module's prompt to the model and returns ellmer's
#' async generator of text chunks, for use inside an asynchronous function
#' (for example with the coro package). The response is plain streamed text:
#' the signature's output types are not applied. For per-field callbacks and
#' structured results, use [run_stream()].
#'
#' @param module A prediction module from [module()] or [chain_of_thought()].
#'   Other modules are rejected before any request; use [run()] for them.
#' @param ... Inputs named after the signature's input fields.
#' @param .llm An ellmer Chat; see [run()] for how it is chosen when
#'   omitted.
#'
#' @details
#' The generator comes from ellmer's `Chat$stream_async()`. The call does not
#' use the response cache and records no trace.
#'
#' @return An async generator that yields the response text in chunks.
#'
#' @export
#' @family execution
#' @examples
#' \dontrun{
#' storyteller <- module(signature("topic -> story"))
#' chunks <- stream_async(
#'   storyteller,
#'   topic = "a lighthouse keeper",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#'
#' print_chunks <- coro::async(function(generator) {
#'   for (chunk in coro::await_each(generator)) cat(chunk)
#' })
#' print_chunks(chunks)
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

#' Listen to one output field while streaming
#'
#' @description
#' `stream_listener()` attaches a callback to an output field for
#' [run_stream()], like DSPy's `StreamListener`. The callback receives the
#' field's text as it is produced: chunk by chunk when the field can be
#' streamed, otherwise once with the complete value.
#'
#' @details
#' A field can be streamed chunk by chunk when it is the only output field of
#' its module and is a string. Fields of modules with several outputs, or
#' non-string outputs, arrive once, when the module finishes, because
#' structured output cannot be streamed.
#'
#' @param field The name of the output field, such as `"answer"`.
#' @param callback A function called with one string: each chunk of text, or
#'   the complete value of a field that cannot be streamed.
#'
#' @return A listener (class `dsprrr_stream_listener`) for the `listeners`
#'   argument of [run_stream()].
#' @export
#' @family execution
#' @examples
#' show_answer <- stream_listener("answer", function(chunk) cat(chunk))
#'
#' \dontrun{
#' storyteller <- module(signature("question -> answer"))
#' run_stream(
#'   storyteller,
#'   question = "Tell me a short story about a lighthouse.",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   listeners = show_answer
#' )
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

#' Run a module with streaming callbacks
#'
#' @description
#' `run_stream()` runs a module or pipeline like [run()], but sends output
#' text to [stream_listener()] callbacks as it is produced and reports
#' progress through `on_status`. It is dsprrr's counterpart to DSPy's
#' `streamify()`, for showing progress in Shiny apps or at the console. In a
#' pipeline, listeners fire for matching fields at every step, not only the
#' last.
#'
#' @param module A module or a pipeline from [pipeline()].
#' @param ... Inputs named after the signature's input fields.
#' @param .llm An ellmer Chat; see [run()] for how it is chosen when
#'   omitted.
#' @param listeners A [stream_listener()], or a list of them.
#' @param on_status A function called with a list for each progress event
#'   (see below), or `NULL`.
#'
#' @details
#' ## Streaming
#'
#' A prediction module (from [module()] or [chain_of_thought()]) whose only
#' output field is a string is streamed chunk by chunk when a listener asks
#' for that field; the collected text becomes the field's value. This needs
#' the coro package. Any other module runs normally, and matching listeners
#' receive the complete value once. When coro is installed, a listener on the
#' single string field of a module that is not a prediction module (such as
#' [module_fn()] or [react()]) is an error, raised before any request is
#' made.
#'
#' Streaming runs do not use the response cache and record no traces.
#'
#' ## Status events
#'
#' `on_status` receives lists with these elements:
#' - `type`: `"step_start"`, `"field_start"`, `"field_end"` (around streamed
#'   text), `"field_complete"` (a field delivered in one piece) or
#'   `"step_end"`.
#' - `step` and `n_steps`: the step's position in a pipeline; both are 1 for a
#'   single module.
#' - `module`: the class of the module running the step.
#' - `field`: the output field, for field events.
#'
#' @return The output, invisibly, in the same form as [run()] returns it.
#' @export
#' @family execution
#' @examples
#' \dontrun{
#' writer <- module(signature("question -> answer"))
#' run_stream(
#'   writer,
#'   question = "Tell me a short story about a lighthouse.",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   listeners = stream_listener("answer", function(chunk) cat(chunk)),
#'   on_status = function(event) message("[", event$type, "] step ", event$step)
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
    assert_decisions_supported(module, "token streaming")
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
