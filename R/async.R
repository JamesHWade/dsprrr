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
#' @param .return_format Either `"simple"` (the default), which resolves to the
#'   output value, or `"structured"`, which resolves to a `dsprrr_result` with
#'   `output`, `chat`, and `metadata` fields.
#' @param .trace_context A named, JSON-compatible list, as in [run()]. The
#'   promise carries it in its `dsprrr_trace_context` attribute. Direct Predict
#'   calls also record it in the module's traces.
#' @param .control An optional [react_async_control()] for cooperative
#'   cancellation and progress reporting. Supported only for [react()] modules.
#' @param .history Explicit conversation history is not supported by async
#'   execution; use synchronous [run()] for scalar root ReAct or RLM continuation.
#'
#' @details
#' Prediction modules call ellmer's `chat_structured_async()` directly. These
#' calls bypass the response cache and record one trace on either success or
#' failure. Structured results include canonical usage, decisions, and any
#' provider-supplied native reasoning blocks recorded for the current call;
#' unavailable usage remains `NA` and absent native reasoning remains `NULL`.
#' ReAct failures and cancellations preserve the partial native Chat history,
#' observed tool results, and requested-versus-result status in the trace. That
#' partial history is not a portable continuation or an instruction to replay
#' tools.
#' Factory-backed code-running modules keep their simple result behavior;
#' structured results are rejected because that worker path does not yet
#' preserve the same trace contract. Modules bound to a caller-owned `runner`
#' are rejected, because one runner cannot serve concurrent calls. Other
#' specialized modules, such as pipelines, are rejected; use [run()].
#'
#' Each direct provider call reserves its resolved Chat until the promise
#' settles. A concurrent call using that same Chat is rejected before provider
#' work; use separate Chats for overlapping calls.
#'
#' ReAct modules use ellmer's sequential async tool loop and await their
#' structured follow-up when configured. Their optional `.control` reports
#' progress and supports cooperative cancellation between requests and tools;
#' it does not interrupt a request or tool already in progress.
#'
#' @return A promise that resolves to the module's output, matching
#'   [run()]'s default result, or to a `dsprrr_result` when
#'   `.return_format = "structured"`. Provider, decoding, and completion
#'   failures reject the promise.
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
run_async <- function(
  module,
  ...,
  .llm = NULL,
  .return_format = c("simple", "structured"),
  .trace_context = list(),
  .history = NULL,
  .control = NULL
) {
  reject_explicit_history_entrypoint(.history, "run_async()")
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr Module object")
  }
  .return_format <- match.arg(.return_format)
  trace_context_supplied <- !missing(.trace_context)
  invocation_trace_fields <- capture_async_trace_context(
    module,
    .trace_context,
    supplied = trace_context_supplied
  )
  if (inherits(module, "ReactModule")) {
    inputs <- list(...)
    validate_signature_inputs(
      module$signature,
      inputs,
      missing = "error",
      extra = "warn",
      type = "warn",
      context = "inputs",
      supplied = module$supplied_inputs()
    )
    input_contract <- module_input_contract(module, inputs)
    if (!identical(input_contract$kind, "scalar")) {
      cli::cli_abort(
        c(
          "Async ReAct execution requires scalar inputs",
          "x" = "The supplied inputs describe {.val {input_contract$kind}} values.",
          "i" = "Call the module once per scalar input set with {.fn run_async}."
        ),
        class = "dsprrr_react_async_batch_unsupported"
      )
    }
    return(react_run_async(
      module = module,
      inputs = inputs,
      llm = .llm,
      return_format = .return_format,
      trace_fields = invocation_trace_fields,
      control = .control
    ))
  }
  if (!is.null(.control)) {
    cli::cli_abort(
      "{.arg .control} is supported only for ReAct modules",
      class = "dsprrr_async_control_unsupported"
    )
  }
  if (interpreter_workflow_module(module)) {
    assert_factory_interpreter_async_supported(module, "run_async")
    if (identical(.return_format, "structured")) {
      cli::cli_abort(
        c(
          "Structured results are not supported for factory-backed async modules",
          "x" = "The isolated worker does not preserve the canonical async trace contract.",
          "i" = "Use `.return_format = \"simple\"` or call the module with `run()`."
        ),
        class = "dsprrr_factory_async_structured_unsupported"
      )
    }
    assert_native_reasoning_metadata_supported(module, "run_async")
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
  rlang::check_installed("promises", reason = "for asynchronous execution")

  inputs <- list(...)
  request <- build_module_request(module, inputs)
  llm <- resolve_module_llm(module, .llm = .llm)
  assert_async_chat_method_supported(llm)
  reservation <- reserve_async_chat(llm, operation = "run_async")
  released <- FALSE
  release <- function() {
    if (!released) {
      release_async_chat(llm, reservation)
      released <<- TRUE
    }
    invisible(NULL)
  }
  # Until the promise owns the finalizer, release on every synchronous exit.
  handed_to_promise <- FALSE
  on.exit(
    if (!handed_to_promise) {
      release()
    },
    add = TRUE
  )

  decisions <- module_decisions(module)
  reasoning_mode <- module_reasoning_mode(module)
  store_chat <- isTRUE(module$config$store_chat_in_traces)
  turns_before <- batch_chat_turns(llm)
  run_before <- chat_last_run_id(llm)
  started_at <- Sys.time()
  trace_recorded <- new.env(parent = emptyenv())
  trace_recorded$value <- FALSE
  trace_recorded$local_only <- FALSE

  commit_async_trace <- function(trace, keep_local_on_error = FALSE) {
    trace_result <- commit_async_trace_transaction(
      module,
      trace,
      keep_local_on_error = keep_local_on_error
    )
    if (isTRUE(trace_result$committed) || isTRUE(trace_result$local_only)) {
      trace_recorded$value <- TRUE
      trace_recorded$local_only <- isTRUE(trace_result$local_only)
    }
    trace_result
  }

  build_metadata <- function(
    ended_at,
    error = NULL,
    error_stage = NULL,
    decision_records = NULL
  ) {
    metadata <- canonical_run_metadata(
      request = request,
      started_at = started_at,
      ended_at = ended_at,
      usage = chat_usage_metadata(llm, turns_before = turns_before),
      model = tryCatch(llm$get_model(), error = function(e) NA_character_),
      error = error,
      backend = "async",
      cache = "bypass"
    )
    if (!is.null(error_stage)) {
      metadata$error_stage <- error_stage
    }
    if (!is.null(decision_records)) {
      metadata$decisions <- decision_records
    }
    if (!is.null(reasoning_mode) && !identical(reasoning_mode, "rationale")) {
      metadata$reasoning_mode <- reasoning_mode
    }
    if (identical(reasoning_mode, "native")) {
      metadata$native_reasoning <- native_reasoning_from_turns(
        verified_chat_turn_delta(llm, turns_before)
      )
    }
    agent_run <- chat_new_run_receipt(llm, run_before)
    if (!is.null(agent_run)) {
      metadata$agent_run <- agent_run
    }
    metadata
  }

  record_failure <- function(error, error_stage) {
    if (isTRUE(trace_recorded$value)) {
      return(invisible(NULL))
    }
    tryCatch(
      with_async_trace_context(invocation_trace_fields, function() {
        metadata <- build_metadata(
          ended_at = Sys.time(),
          error = error,
          error_stage = error_stage
        )
        trace <- canonical_run_trace(
          inputs = inputs,
          response = NA,
          request = request,
          chat = llm,
          turns_before = turns_before,
          metadata = metadata,
          error = error,
          store_chat = store_chat
        )
        commit_async_trace(trace, keep_local_on_error = TRUE)
      }),
      error = function(trace_error) {
        if (!isTRUE(trace_recorded$local_only)) {
          cli::cli_warn(
            "Could not record async failure trace: {conditionMessage(trace_error)}"
          )
        }
      }
    )
    invisible(NULL)
  }

  type <- decision_request_type(module$signature@output_type, decisions)
  provider_result <- tryCatch(
    with_async_trace_context(invocation_trace_fields, function() {
      args <- c(prompt_parts(request$payload), list(type = type))
      run_context <- chat_run_context(llm, "chat_structured_async")
      if (!is.null(run_context)) {
        args$run_context <- run_context
      }
      do.call(llm$chat_structured_async, args)
    }),
    interrupt = function(error) {
      record_failure(error, "provider")
      release()
      resignal_condition(error)
    },
    error = function(error) error
  )
  if (inherits(provider_result, "condition")) {
    record_failure(provider_result, "provider")
    release()
    resignal_condition(provider_result)
  }

  provider_promise <- tryCatch(
    if (inherits(provider_result, "promise")) {
      provider_result
    } else {
      promises::promise_resolve(provider_result)
    },
    error = function(error) {
      record_failure(error, "provider")
      release()
      resignal_condition(error)
    }
  )
  result <- tryCatch(
    promises::then(
      provider_promise,
      onFulfilled = function(response) {
        tryCatch(
          with_async_trace_context(invocation_trace_fields, function() {
            decoded <- tryCatch(
              decision_decode_response(response, decisions),
              error = function(error) {
                record_failure(error, "decode")
                resignal_condition(error)
              }
            )
            output <- decoded$output
            ended_at <- Sys.time()
            metadata <- build_metadata(
              ended_at = ended_at,
              decision_records = decoded$decisions
            )
            completed_chat <- completed_batch_chat(
              request$payload,
              output,
              llm,
              turns_before = turns_before
            )
            result <- if (identical(.return_format, "simple")) {
              output
            } else {
              structure(
                list(output = output, chat = llm, metadata = metadata),
                class = "dsprrr_result"
              )
            }
            trace <- canonical_run_trace(
              inputs = inputs,
              response = output,
              request = request,
              chat = completed_chat,
              turns_before = turns_before,
              metadata = metadata,
              store_chat = store_chat
            )
            commit_async_trace(trace)
            result
          }),
          error = function(error) {
            record_failure(error, "completion")
            resignal_condition(error)
          }
        )
      },
      onRejected = function(error) {
        record_failure(error, "provider")
        resignal_condition(error)
      }
    ),
    error = function(error) {
      record_failure(error, "completion")
      release()
      resignal_condition(error)
    }
  )
  result <- tryCatch(
    promises::finally(result, release),
    error = function(error) {
      record_failure(error, "completion")
      release()
      resignal_condition(error)
    }
  )
  attr(result, "dsprrr_trace_context") <- invocation_trace_fields
  handed_to_promise <- TRUE
  result
}


capture_async_trace_context <- function(module, value, supplied) {
  context <- trace_context_resolve(value, supplied = supplied)
  previous <- trace_context_enter(
    context,
    program = module,
    inherit_program_id = !supplied
  )
  on.exit(trace_context_restore(previous), add = TRUE)
  trace_context_fields()
}


with_async_trace_context <- function(fields, code) {
  previous <- trace_context_enter(fields$trace_context)
  .dsprrr_env$trace_program_artifact_id <- fields$program_artifact_id
  on.exit(trace_context_restore(previous), add = TRUE)
  code()
}


async_chat_reservations <- function() {
  reservations <- .dsprrr_env$async_chat_reservations
  if (is.null(reservations)) {
    reservations <- new.env(parent = emptyenv())
    .dsprrr_env$async_chat_reservations <- reservations
  }
  reservations
}


async_chat_key <- function(chat) {
  as.character(rlang::obj_address(chat))
}


assert_async_chat_available <- function(chat) {
  reservations <- async_chat_reservations()
  key <- async_chat_key(chat)
  if (exists(key, envir = reservations, inherits = FALSE)) {
    reservation <- get(key, envir = reservations, inherits = FALSE)
    owner_operation <- reservation$operation %||% "an async invocation"
    cli::cli_abort(
      c(
        "This Chat is reserved by {.val {owner_operation}}",
        "x" = "Overlapping calls could interleave conversation history.",
        "i" = "Wait for the existing promise to settle or use a separate Chat."
      ),
      class = "dsprrr_chat_busy",
      owner_operation = owner_operation
    )
  }
  invisible(chat)
}


reserve_async_chat <- function(chat, operation) {
  assert_async_chat_available(chat)
  reservations <- async_chat_reservations()
  token <- new.env(parent = emptyenv())
  assign(
    async_chat_key(chat),
    list(chat = chat, token = token, operation = operation),
    envir = reservations
  )
  token
}


release_async_chat <- function(chat, token) {
  reservations <- async_chat_reservations()
  key <- async_chat_key(chat)
  if (!exists(key, envir = reservations, inherits = FALSE)) {
    return(invisible(FALSE))
  }
  reservation <- get(key, envir = reservations, inherits = FALSE)
  if (!identical(reservation$token, token)) {
    return(invisible(FALSE))
  }
  rm(list = key, envir = reservations)
  invisible(TRUE)
}


assert_async_chat_method_supported <- function(chat) {
  method <- tryCatch(chat[["chat_structured_async"]], error = function(e) NULL)
  if (!is.function(method)) {
    cli::cli_abort(
      "The resolved Chat does not support `chat_structured_async()`",
      class = "dsprrr_async_chat_method_unsupported"
    )
  }
  invisible(chat)
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
        "x" = "{conditionMessage(error)}"
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
      resignal_condition(condition)
    },
    error = abort_launch
  )

  promise <- tryCatch(
    promises::as.promise(task),
    interrupt = function(condition) {
      cleanup_after_failure()
      resignal_condition(condition)
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
      resignal_condition(error)
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
#' @param .history Explicit conversation history is not supported by async
#'   streaming; use synchronous [run()] for scalar root ReAct or RLM continuation.
#'
#' @details
#' The generator comes from ellmer's `Chat$stream_async()`. The call does not
#' use the response cache and records no trace. It returns unstructured text,
#' so `chain_of_thought(mode = "plan")` and `mode = "native"` are rejected
#' rather than dropping the plan field or native metadata. Use [run_async()]
#' for a structured plan, or synchronous structured [run()] for native
#' metadata.
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
stream_async <- function(module, ..., .llm = NULL, .history = NULL) {
  reject_explicit_history_entrypoint(.history, "stream_async()")
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr Module object")
  }
  assert_reasoning_text_stream_supported(module)
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
  if (!rlang::is_string(field) || !nzchar(field)) {
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
#' @param .history Explicit conversation history is not supported by streaming;
#'   use synchronous [run()] for scalar root ReAct or RLM continuation.
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
#' Streaming runs do not use the response cache and record no traces. Native
#' reasoning mode is rejected because this result contract does not return
#' provider turns or native reasoning metadata.
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
  on_status = NULL,
  .history = NULL
) {
  reject_explicit_history_entrypoint(.history, "run_stream()")
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr Module object")
  }
  assert_native_reasoning_metadata_supported(module, "run_stream")
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
