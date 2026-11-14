#' Create cooperative control for asynchronous ReAct execution
#'
#' @description
#' The control can report ordered progress events and request cooperative
#' cancellation. Cancellation is checked between provider requests and tool
#' calls; it does not interrupt a request or tool that is already running.
#' Progress callback errors reject the async call after cleanup. A failed or
#' cancelled call can leave partial native history; requested tool calls and
#' actual tool results are recorded separately, without replaying tools.
#'
#' @param on_progress Optional function called with progress event lists.
#'   Events include a sequence number and trace correlation fields, but never
#'   include prompts, tool arguments, or reasoning content.
#' @return A one-invocation control object. Call `$cancel(reason)` to request
#'   cancellation while a `run_async()` promise is pending. Repeated cancel
#'   requests after the first are ignored.
#' @export
react_async_control <- function(on_progress = NULL) {
  if (!is.null(on_progress) && !is.function(on_progress)) {
    cli::cli_abort(
      "{.arg on_progress} must be a function or NULL",
      class = "dsprrr_react_async_control_error"
    )
  }

  state <- new.env(parent = emptyenv())
  state$cancelled <- FALSE
  state$reason <- NULL
  state$claimed <- FALSE
  state$settled <- FALSE
  state$sequence <- 0L

  control <- list(
    cancel = function(reason = "requested") {
      if (
        !is.character(reason) ||
          length(reason) != 1L ||
          is.na(reason) ||
          !nzchar(trimws(reason))
      ) {
        cli::cli_abort(
          "{.arg reason} must be one non-empty string",
          class = "dsprrr_react_async_control_error"
        )
      }
      if (state$settled || state$cancelled) {
        return(invisible(FALSE))
      }
      state$cancelled <- TRUE
      state$reason <- reason
      invisible(TRUE)
    },
    is_cancelled = function() isTRUE(state$cancelled),
    cancel_reason = function() state$reason,
    .claim = function() {
      if (state$claimed) {
        cli::cli_abort(
          "An async ReAct control can be used for only one invocation",
          class = "dsprrr_react_async_control_reused"
        )
      }
      state$claimed <- TRUE
      invisible(NULL)
    },
    .emit = function(event) {
      state$sequence <- state$sequence + 1L
      event$sequence <- state$sequence
      event$timestamp <- Sys.time()
      if (!is.null(on_progress)) {
        callback_result <- on_progress(event)
        if (inherits(callback_result, "promise")) {
          cli::cli_abort(
            "{.arg on_progress} must complete synchronously",
            class = "dsprrr_react_async_progress_error"
          )
        }
      }
      invisible(event)
    },
    .settle = function() {
      state$settled <- TRUE
      invisible(NULL)
    }
  )
  class(control) <- "dsprrr_react_async_control"
  control
}

#' Run a ReAct module through ellmer's asynchronous tool loop
#' @noRd
react_run_async <- function(
  module,
  inputs,
  llm = NULL,
  return_format = c("simple", "structured"),
  trace_fields,
  control = NULL
) {
  if (!inherits(module, "ReactModule")) {
    cli::cli_abort("{.arg module} must be a ReactModule")
  }
  return_format <- match.arg(return_format)
  if (is.null(control)) {
    control <- react_async_control()
  }
  if (!inherits(control, "dsprrr_react_async_control")) {
    cli::cli_abort(
      "{.arg control} must be made with {.fn react_async_control}",
      class = "dsprrr_react_async_control_error"
    )
  }
  control$.claim()
  rlang::check_installed("promises", reason = "for asynchronous execution")

  request <- build_module_request(module, inputs)
  llm <- resolve_module_llm(module, .llm = llm)
  react_async_assert_chat(llm)
  reservation <- reserve_async_chat(llm, operation = "run_async (ReAct)")
  started_at <- Sys.time()
  turns_before <- batch_chat_turns(llm)
  if (is.null(turns_before)) {
    release_async_chat(llm, reservation)
    cli::cli_abort(
      "The ellmer Chat history cannot be inspected for async ReAct execution",
      class = "dsprrr_react_async_chat_error"
    )
  }
  run_before <- chat_last_run_id(llm)
  trace_recorded <- new.env(parent = emptyenv())
  trace_recorded$value <- FALSE
  runtime <- new.env(parent = emptyenv())
  runtime$phase <- "launch"
  runtime$request_index <- 0L
  runtime$iterations <- 0L
  runtime$tool_calls <- list()
  runtime$observed_tool_results <- list()

  prepared <- NULL
  removers <- list()
  cleanup_state <- new.env(parent = emptyenv())
  cleanup_state$done <- FALSE
  cleanup_state$resources_done <- FALSE
  cleanup_resources <- function() {
    if (isTRUE(cleanup_state$resources_done)) {
      return(invisible(NULL))
    }
    cleanup_state$resources_done <- TRUE
    first_error <- NULL
    for (name in rev(names(removers))) {
      tryCatch(
        removers[[name]](),
        error = function(error) {
          if (is.null(first_error)) {
            first_error <<- error
          }
        }
      )
    }
    if (!is.null(prepared)) {
      tryCatch(
        if (!is.null(prepared$submit_runtime)) {
          react_submit_cleanup(llm, prepared$submit_runtime)
        } else {
          llm$set_tools(prepared$original_tools)
        },
        error = function(error) {
          if (is.null(first_error)) {
            first_error <<- error
          }
        }
      )
    }
    if (!is.null(first_error)) {
      stop(first_error)
    }
    invisible(NULL)
  }
  cleanup <- function() {
    if (isTRUE(cleanup_state$done)) {
      return(invisible(NULL))
    }
    cleanup_state$done <- TRUE
    first_error <- tryCatch(
      {
        cleanup_resources()
        NULL
      },
      error = identity
    )
    release_async_chat(llm, reservation)
    control$.settle()
    if (inherits(first_error, "condition")) {
      stop(first_error)
    }
    invisible(NULL)
  }
  handed_to_promise <- FALSE
  on.exit(
    if (!handed_to_promise) {
      tryCatch(cleanup(), error = function(error) NULL)
    },
    add = TRUE
  )

  emit <- function(type, details = list(), check_stage = NULL) {
    fields <- list(
      type = type,
      trace_context = trace_fields$trace_context,
      program_artifact_id = trace_fields$program_artifact_id
    )
    fields <- c(fields, details)
    fields <- fields[!vapply(fields, is.null, logical(1))]
    control$.emit(fields)
    if (!is.null(check_stage)) {
      react_async_check_cancelled(control, check_stage, trace_fields)
    }
    invisible(NULL)
  }
  with_context <- function(code) {
    with_async_trace_context(trace_fields, code)
  }

  finalization <- validate_react_finalization(
    module$config$.react_finalization %||% "structured"
  )
  submit_invocation_marker <- if (identical(finalization, "submit")) {
    new.env(parent = emptyenv())
  } else {
    NULL
  }
  validate_react_tools(module$tools)
  prepared <- react_async_prepare_tools(
    llm,
    module$tools,
    module$signature@output_type,
    finalization
  )
  if (identical(finalization, "submit")) {
    runtime$submit_runtime <- prepared$submit_runtime
  }

  register_callback <- function(method, name, callback) {
    remover <- llm[[method]](callback)
    if (!is.function(remover)) {
      cli::cli_abort(
        "The ellmer Chat did not return a {.val {name}} callback remover",
        class = "dsprrr_react_async_chat_error"
      )
    }
    removers[[name]] <<- remover
    invisible(NULL)
  }

  current_run_turns <- function() {
    verified_chat_turn_delta(llm, turns_before) %||% list()
  }
  turn_requests <- function(turn) {
    if (!inherits(turn, "ellmer::AssistantTurn")) {
      return(list())
    }
    Filter(
      function(content) inherits(content, "ellmer::ContentToolRequest"),
      turn@contents
    )
  }
  record_tool_request <- function(request, iteration = runtime$iterations) {
    index <- which(vapply(
      runtime$tool_calls,
      function(call) identical(call$tool_id, request@id),
      logical(1)
    ))
    if (length(index) > 0L) {
      return(invisible(index[[1L]]))
    }
    runtime$tool_calls <- c(
      runtime$tool_calls,
      list(list(
        iteration = iteration,
        tool_name = request@name,
        tool_id = request@id,
        arguments = request@arguments,
        status = "requested",
        is_error = NULL
      ))
    )
    invisible(length(runtime$tool_calls))
  }
  record_tool_result <- function(request, result, is_error) {
    index <- which(vapply(
      runtime$tool_calls,
      function(call) identical(call$tool_id, request@id),
      logical(1)
    ))
    if (length(index) == 0L) {
      record_tool_request(request)
      index <- length(runtime$tool_calls)
    } else {
      index <- index[[1L]]
    }
    runtime$tool_calls[[index]]$status <- "result"
    runtime$tool_calls[[index]]$is_error <- isTRUE(is_error)
    runtime$observed_tool_results <- c(
      runtime$observed_tool_results,
      list(list(
        tool_id = request@id,
        tool_name = request@name,
        result = result
      ))
    )
    invisible(index)
  }
  error_stage <- function(error) {
    if (inherits(error, "dsprrr_react_async_cancelled")) {
      return("cancelled")
    }
    if (inherits(error, "dsprrr_react_iteration_limit")) {
      return("tool_loop")
    }
    switch(
      runtime$phase,
      launch = "provider",
      tool_loop = "provider",
      tool = "tool",
      finalization = "finalization",
      structured_followup = "finalization",
      completion = "completion",
      cleanup = "cleanup",
      "provider"
    )
  }
  build_metadata <- function(ended_at, error = NULL, stage = NULL) {
    metadata <- canonical_run_metadata(
      request = request,
      started_at = started_at,
      ended_at = ended_at,
      usage = chat_usage_metadata(llm, turns_before = turns_before),
      model = tryCatch(llm$get_model(), error = function(error) NA_character_),
      error = error,
      backend = "async",
      cache = "bypass"
    )
    if (!is.null(stage)) {
      metadata$error_stage <- stage
    }
    turns <- current_run_turns()
    metadata$history <- turns
    metadata$iterations <- max(1L, runtime$iterations)
    metadata$tool_calls <- runtime$tool_calls
    metadata$observed_tool_results <- runtime$observed_tool_results
    result_calls <- Filter(
      function(call) identical(call$status, "result"),
      metadata$tool_calls
    )
    metadata$tools_used <- unique(vapply(
      result_calls,
      function(call) call$tool_name,
      character(1)
    ))
    metadata$finalization <- if (identical(finalization, "submit")) {
      "submit"
    } else {
      "structured-followup"
    }
    metadata$termination_reason <- if (is.null(error)) {
      if (identical(finalization, "submit")) "submitted" else "structured"
    } else {
      NA_character_
    }
    agent_run <- chat_new_run_receipt(llm, run_before)
    if (!is.null(agent_run)) {
      metadata$agent_run <- agent_run
    }
    metadata
  }
  record_failure <- function(error, stage) {
    if (isTRUE(trace_recorded$value)) {
      return(invisible(NULL))
    }
    trace_result <- tryCatch(
      with_context(function() {
        metadata <- build_metadata(Sys.time(), error = error, stage = stage)
        trace <- canonical_run_trace(
          inputs = inputs,
          response = NA,
          request = request,
          chat = llm,
          turns_before = turns_before,
          metadata = metadata,
          error = error,
          store_chat = isTRUE(module$config$store_chat_in_traces)
        )
        commit_async_trace_transaction(
          module,
          trace,
          keep_local_on_error = TRUE
        )
      }),
      error = function(trace_error) {
        list(
          committed = FALSE,
          local_only = FALSE,
          error = trace_error
        )
      }
    )
    trace_recorded$value <- TRUE
    if (!is.null(trace_result$error)) {
      tryCatch(
        cli::cli_warn(
          "Could not write async ReAct failure history: {conditionMessage(trace_result$error)}"
        ),
        warning = function(warning) NULL,
        error = function(error) NULL
      )
    }
    invisible(NULL)
  }
  complete_success <- function(output, termination) {
    runtime$phase <- "completion"
    react_async_check_cancelled(control, "completion", trace_fields)
    metadata <- with_context(function() build_metadata(Sys.time()))
    metadata$termination_reason <- termination
    completed_chat <- completed_batch_chat(
      request$payload,
      output,
      llm,
      turns_before = turns_before
    )
    result <- if (identical(return_format, "simple")) {
      output
    } else {
      structure(
        list(output = output, chat = llm, metadata = metadata),
        class = "dsprrr_result"
      )
    }
    trace <- with_context(function() {
      canonical_run_trace(
        inputs = inputs,
        response = output,
        request = request,
        chat = completed_chat,
        turns_before = turns_before,
        metadata = metadata,
        store_chat = isTRUE(module$config$store_chat_in_traces)
      )
    })
    runtime$phase <- "cleanup"
    cleanup_resources()
    runtime$phase <- "completion"
    emit("complete", list(termination_reason = termination), "completion")
    trace_result <- commit_async_trace_transaction(module, trace)
    if (!is.null(trace_result$error)) {
      stop(trace_result$error)
    }
    trace_recorded$value <- isTRUE(trace_result$committed)
    result
  }

  # `react_async_prepare_tools()` returns either the submit helper's temporary
  # registry or the original registry captured before user tools were added.
  register_callback(
    "on_request_start",
    "request_start",
    function(turns) {
      with_context(function() {
        react_async_check_cancelled(control, "provider request", trace_fields)
        runtime$request_index <- runtime$request_index + 1L
        emit(
          "provider_request_start",
          list(
            request_index = runtime$request_index,
            phase = if (identical(runtime$phase, "structured_followup")) {
              "structured_followup"
            } else {
              "tool_loop"
            }
          ),
          "provider request"
        )
      })
    }
  )
  register_callback(
    "on_request_end",
    "request_end",
    function(turn) {
      with_context(function() {
        requests <- turn_requests(turn)
        if (
          identical(runtime$phase, "tool_loop") &&
            length(requests) > 0L
        ) {
          request_ids <- vapply(
            requests,
            function(request) request@id,
            character(1)
          )
          known_ids <- vapply(
            runtime$tool_calls,
            function(call) call$tool_id,
            character(1)
          )
          if (
            anyDuplicated(request_ids) > 0L || any(request_ids %in% known_ids)
          ) {
            cli::cli_abort(
              "The provider reused a tool request ID during this ReAct call",
              class = "dsprrr_react_tool_id_error"
            )
          }
          runtime$iterations <- runtime$iterations + 1L
          for (request in requests) {
            record_tool_request(request, runtime$iterations)
          }
        }
        emit(
          "provider_request_end",
          list(
            request_index = runtime$request_index,
            phase = if (identical(runtime$phase, "structured_followup")) {
              "structured_followup"
            } else {
              "tool_loop"
            },
            tool_request_count = length(requests)
          ),
          "provider response"
        )
        if (
          identical(runtime$phase, "tool_loop") &&
            runtime$iterations > module$max_iterations
        ) {
          cli::cli_abort(
            "ReAct exceeded max_iterations ({module$max_iterations})",
            class = "dsprrr_react_iteration_limit"
          )
        }
        if (
          !identical(runtime$phase, "tool_loop") ||
            !identical(finalization, "submit") ||
            length(requests) == 0L
        ) {
          return(invisible(NULL))
        }
        submit_requests <- Filter(
          function(tool_request) identical(tool_request@name, "submit"),
          requests
        )
        if (length(submit_requests) == 0L) {
          return(invisible(NULL))
        }
        if (length(requests) != 1L || length(submit_requests) != 1L) {
          cli::cli_abort(
            c(
              "A ReAct submit request cannot share a turn with other tools",
              "i" = "The request was rejected before any tool was run."
            ),
            class = "dsprrr_react_submit_mixed_tools"
          )
        }
        submit_request <- submit_requests[[1L]]
        submitted_value <- tryCatch(
          react_submit_validate(
            submit_request@arguments,
            module$signature@output_type,
            !inherits(
              module$signature@output_type,
              "ellmer::TypeObject"
            )
          ),
          dsprrr_react_submit_validation_error = function(error) error
        )
        if (inherits(submitted_value, "dsprrr_react_submit_validation_error")) {
          prepared$submit_runtime$validation_state$invalid_submit <- submitted_value
          return(invisible(NULL))
        }
        runtime$phase <- "finalization"
        emit(
          "finalization_start",
          list(
            finalization = "submit",
            request_index = runtime$request_index,
            iteration = runtime$iterations
          ),
          "finalization"
        )
        emit(
          "finalization_end",
          list(finalization = "submit", termination_reason = "submitted"),
          "completion"
        )
        react_submit_completed(
          submit_request,
          submitted_value,
          submit_invocation_marker
        )
      })
    }
  )
  register_callback(
    "on_tool_request",
    "tool_request",
    function(request) {
      with_context(function() {
        runtime$phase <- if (identical(runtime$phase, "structured_followup")) {
          "structured_followup"
        } else {
          "tool"
        }
        if (identical(runtime$phase, "tool")) {
          record_tool_request(request, runtime$iterations)
        }
        react_async_check_cancelled(control, "tool execution", trace_fields)
        emit(
          "tool_start",
          list(
            request_index = runtime$request_index,
            iteration = runtime$iterations,
            tool_name = request@name,
            tool_id = request@id
          ),
          "tool execution"
        )
        if (identical(runtime$phase, "tool")) {
          runtime$phase <- "tool_loop"
        }
        invisible(NULL)
      })
    }
  )
  register_callback(
    "on_tool_result",
    "tool_result",
    function(result) {
      with_context(function() {
        request <- tryCatch(result@request, error = function(error) NULL)
        result_error <- !is.null(tryCatch(
          result@error,
          error = function(error) NULL
        ))
        if (!is.null(request) && identical(runtime$phase, "tool_loop")) {
          record_tool_result(request, result, result_error)
        }
        emit(
          "tool_end",
          list(
            request_index = runtime$request_index,
            iteration = runtime$iterations,
            tool_name = tryCatch(request@name, error = function(error) NULL),
            tool_id = tryCatch(request@id, error = function(error) NULL),
            is_error = result_error
          ),
          "tool result"
        )
        invisible(NULL)
      })
    }
  )

  initial_result <- tryCatch(
    with_context(function() {
      react_async_check_cancelled(control, "provider request", trace_fields)
      emit("run_start", list(module = "ReactModule"), "provider request")
      runtime$phase <- "tool_loop"
      args <- c(
        prompt_parts(request$payload),
        list(tool_mode = "sequential")
      )
      run_context <- chat_run_context(llm, "chat_async")
      if (!is.null(run_context)) {
        args$run_context <- run_context
      }
      do.call(llm$chat_async, args)
    }),
    error = function(error) error
  )
  if (inherits(initial_result, "condition")) {
    initial_promise <- promises::promise_reject(initial_result)
  } else {
    initial_promise <- if (inherits(initial_result, "promise")) {
      initial_result
    } else {
      promises::promise_resolve(initial_result)
    }
  }

  on_submit_rejection <- function(error) {
    if (
      !identical(finalization, "submit") ||
        is.null(submit_invocation_marker) ||
        !inherits(error, "dsprrr_react_submit_completed") ||
        !identical(error$invocation_marker, submit_invocation_marker)
    ) {
      stop(error)
    }
    react_async_check_cancelled(control, "completion", trace_fields)
    submit_result <- react_submit_complete(llm, error$request, error$value)
    record_tool_result(error$request, submit_result, FALSE)
    complete_success(error$value, "submitted")
  }

  finish_structured <- function() {
    react_async_check_cancelled(control, "structured follow-up", trace_fields)
    if (identical(finalization, "submit")) {
      cli::cli_abort(
        c(
          "ReAct submit finalization requires a valid submit tool call",
          "i" = "The model returned without submitting the declared output."
        ),
        class = "dsprrr_react_submit_missing"
      )
    }
    runtime$phase <- "finalization"
    emit(
      "finalization_start",
      list(finalization = "structured"),
      "structured follow-up"
    )
    runtime$phase <- "structured_followup"
    type <- module$signature@output_type
    args <- c(
      list("Based on the conversation above, provide your final answer."),
      list(type = type, echo = "none")
    )
    run_context <- chat_run_context(llm, "chat_structured_async")
    if (!is.null(run_context)) {
      args$run_context <- run_context
    }
    response <- do.call(llm$chat_structured_async, args)
    response_promise <- if (inherits(response, "promise")) {
      response
    } else {
      promises::promise_resolve(response)
    }
    promises::then(response_promise, function(output) {
      with_context(function() {
        emit(
          "finalization_end",
          list(finalization = "structured"),
          "completion"
        )
        complete_success(output, "structured")
      })
    })
  }

  provider_flow <- tryCatch(
    promises::then(
      initial_promise,
      onFulfilled = function(text) {
        with_context(function() {
          react_async_check_cancelled(control, "tool loop", trace_fields)
          finish_structured()
        })
      },
      onRejected = function(error) {
        with_context(function() {
          if (inherits(error, "dsprrr_react_submit_completed")) {
            on_submit_rejection(error)
          } else {
            stop(error)
          }
        })
      }
    ),
    error = function(error) {
      record_failure(error, error_stage(error))
      stop(error)
    }
  )
  result <- promises::then(
    provider_flow,
    onFulfilled = identity,
    onRejected = function(error) {
      with_context(function() {
        record_failure(error, error_stage(error))
        stop(error)
      })
    }
  )
  result <- promises::then(
    result,
    onFulfilled = function(value) {
      cleanup_error <- tryCatch(cleanup(), error = identity)
      if (inherits(cleanup_error, "condition")) {
        record_failure(cleanup_error, "cleanup")
        stop(cleanup_error)
      }
      value
    },
    onRejected = function(error) {
      cleanup_error <- tryCatch(cleanup(), error = identity)
      if (inherits(cleanup_error, "condition")) {
        tryCatch(
          cli::cli_warn(
            "ReAct async cleanup also failed: {conditionMessage(cleanup_error)}"
          ),
          warning = function(warning) NULL,
          error = function(error) NULL
        )
      }
      stop(error)
    }
  )
  attr(result, "dsprrr_trace_context") <- trace_fields
  handed_to_promise <- TRUE
  result
}

#' Check for a cooperative ReAct cancellation request
#' @noRd
react_async_check_cancelled <- function(control, stage, trace_fields) {
  if (!isTRUE(control$is_cancelled())) {
    return(invisible(NULL))
  }
  control$.emit(list(
    type = "cancelled",
    stage = stage,
    reason = control$cancel_reason(),
    trace_context = trace_fields$trace_context,
    program_artifact_id = trace_fields$program_artifact_id
  ))
  cli::cli_abort(
    "Async ReAct execution cancelled before {.val {stage}}",
    class = "dsprrr_react_async_cancelled",
    cancellation_reason = control$cancel_reason(),
    stage = stage
  )
}

#' Validate public Chat APIs needed by async ReAct
#' @noRd
react_async_assert_chat <- function(chat) {
  required <- c(
    "chat_async",
    "chat_structured_async",
    "get_turns",
    "get_tools",
    "set_tools",
    "set_turns",
    "register_tool",
    "on_request_start",
    "on_request_end",
    "on_tool_request",
    "on_tool_result"
  )
  missing <- required[
    !vapply(
      required,
      function(name) {
        is.function(tryCatch(chat[[name]], error = function(e) NULL))
      },
      logical(1)
    )
  ]
  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "The ellmer Chat cannot run ReAct asynchronously",
        "x" = "Missing public Chat methods: {.field {missing}}",
        "i" = "Use a Chat with async tool-loop and callback support."
      ),
      class = "dsprrr_react_async_chat_error"
    )
  }
  invisible(chat)
}

#' Register module tools and snapshot any temporary submit tool
#' @noRd
react_async_prepare_tools <- function(
  chat,
  user_tools,
  output_type,
  finalization
) {
  if (identical(finalization, "submit")) {
    submit_runtime <- react_submit_prepare(chat, user_tools, output_type)
    return(list(
      original_tools = submit_runtime$original_tools,
      submit_runtime = submit_runtime
    ))
  }

  original_tools <- chat$get_tools()
  if (!is.list(original_tools)) {
    cli::cli_abort(
      "The ellmer Chat returned an invalid tool registry",
      class = "dsprrr_react_async_chat_error"
    )
  }
  prepared <- FALSE
  on.exit(
    if (!prepared) {
      chat$set_tools(original_tools)
    },
    add = TRUE
  )
  for (tool in user_tools) {
    chat$register_tool(tool)
  }
  prepared <- TRUE
  list(original_tools = original_tools, submit_runtime = NULL)
}
