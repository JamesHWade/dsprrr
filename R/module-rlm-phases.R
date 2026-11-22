# Prepare inputs and the interpreter, execute the trajectory, and record its result.

rlm_prepare_invocation_phase <- function(
  module,
  batch,
  .llm = NULL,
  .cache = NULL,
  .history = NULL
) {
  validate_cache_arg(.cache)

  supplied_history <- NULL
  prior_context <- ""
  if (!is.null(.history)) {
    supplied_history <- rlm_history_validate(.history)
    if (is.null(module$interpreter_factory) || !is.null(module$runner)) {
      cli::cli_abort(
        c(
          "Explicit RLM history requires an invocation-owned interpreter factory",
          "x" = "This module has a caller-owned runner or no factory.",
          "i" = "Create the module with {.arg interpreter_factory}; each call must start in a fresh interpreter."
        ),
        class = c(
          "dsprrr_rlm_history_factory_error",
          "dsprrr_history_error"
        ),
        call = rlang::caller_env()
      )
    }
    prior_context <- history_render_rlm(supplied_history)
  }

  reasoning_mode <- module_reasoning_mode(module)
  validate_rlm_predictor_children(
    list(
      generate_action = module$generate_action,
      extract = module$extract
    ),
    module$signature@output_type,
    reasoning_mode = reasoning_mode
  )

  inputs <- if (is.data.frame(batch)) {
    if (nrow(batch) != 1L) {
      cli::cli_abort(
        c(
          "RLM forward() accepts exactly one data-frame row",
          "x" = "Received {nrow(batch)} rows.",
          "i" = "Use {.fn run} with an interpreter factory for isolated batch execution."
        ),
        class = "dsprrr_rlm_batch_error",
        call = rlang::caller_env()
      )
    }
    as.list(batch[1, , drop = FALSE])
  } else {
    batch
  }

  expected_inputs <- vapply(
    module$signature@inputs,
    function(input) input$name,
    character(1)
  )
  valid_empty_inputs <- is.list(inputs) &&
    length(inputs) == 0L &&
    length(expected_inputs) == 0L
  if (
    !is.list(inputs) ||
      (!valid_empty_inputs && is.null(names(inputs))) ||
      anyNA(names(inputs)) ||
      !all(nzchar(names(inputs))) ||
      anyDuplicated(names(inputs))
  ) {
    cli::cli_abort(
      "RLM inputs must be supplied as a named list or data frame",
      class = "dsprrr_rlm_input_error",
      call = rlang::caller_env()
    )
  }
  required_inputs <- vapply(
    module$signature@inputs,
    function(input) {
      tryCatch(isTRUE(input$type@required), error = function(e) TRUE)
    },
    logical(1)
  )
  missing_inputs <- setdiff(
    expected_inputs[required_inputs],
    names(inputs)
  )
  unexpected_inputs <- setdiff(names(inputs), expected_inputs)
  if (length(missing_inputs) > 0L || length(unexpected_inputs) > 0L) {
    cli::cli_abort(
      c(
        "RLM inputs must match declared signature fields",
        if (length(missing_inputs) > 0L) {
          c("x" = "Missing: {.field {missing_inputs}}")
        },
        if (length(unexpected_inputs) > 0L) {
          c("x" = "Unexpected: {.field {unexpected_inputs}}")
        }
      ),
      class = "dsprrr_rlm_input_error",
      call = rlang::caller_env()
    )
  }

  base_llm <- resolve_module_llm(module, .llm = .llm)
  list(
    inputs = inputs,
    supplied_history = supplied_history,
    prior_context = prior_context,
    reasoning_mode = reasoning_mode,
    cache = .cache,
    base_llm = base_llm,
    llm = clone_rlm_chat(base_llm),
    sub_lm = module$sub_lm %||% base_llm
  )
}

rlm_prepare_runner <- function(runner, lease, inputs) {
  if (!isTRUE(lease$policy$persistent)) {
    cli::cli_abort(
      c(
        "RLM requires a persistent interpreter",
        "x" = "The selected runner starts a fresh execution state for each turn.",
        "i" = "Use {.code r_code_runner(persistent = TRUE)} for trusted inputs or {.fn mcp_repl_runner} for managed execution."
      ),
      class = "dsprrr_rlm_persistent_runner_error",
      call = rlang::caller_env()
    )
  }

  call_counter <- new.env(parent = rlang::caller_env())
  call_counter$count <- 0L
  call_counter$provider_calls <- 0L
  call_counter$provider_calls_known <- TRUE
  call_counter$usage <- list()

  prepare_context <- tryCatch(
    runner[["prepare_context"]],
    error = function(e) NULL
  )
  context_prepared <- FALSE
  if (isTRUE(lease$policy$persistent) && is.function(prepare_context)) {
    prepare_context(inputs)
    context_prepared <- TRUE
  }

  list(call_counter = call_counter, context_prepared = context_prepared)
}

rlm_execute_invocation_phase <- function(
  module,
  prepared,
  runner,
  lease,
  operations,
  session,
  session_state_id,
  start_time,
  trace
) {
  context_desc <- operations$describe_context(prepared$inputs)
  task_text <- if (!is.null(prepared$supplied_history)) {
    history_rlm_task_text(
      module$signature@instructions,
      context_desc
    )
  } else {
    NULL
  }
  system_prompt <- operations$build_system_prompt(
    context_desc,
    has_sub_lm = module$max_llm_calls > 0L,
    prior_context = prepared$prior_context
  )

  history <- list()
  final_answer <- NULL
  fallback_metadata <- NULL
  result_chat <- prepared$llm

  for (iter in seq_len(module$max_iterations)) {
    if (module$verbose) {
      cli::cli_alert_info("RLM Iteration {iter}/{module$max_iterations}")
    }

    prompt <- operations$build_iteration_prompt(system_prompt, history, iter)
    response <- operations$get_code_response(
      prepared$base_llm,
      prompt,
      trace = trace,
      .cache = prepared$cache
    )
    result_chat <- response$chat

    if (module$verbose) {
      cli::cli_alert(
        "Code generated: {substr(response$code, 1, 100)}..."
      )
    }

    exec_result <- operations$execute_with_rlm_tools(
      response$code,
      prepared$inputs,
      session$call_counter,
      runner,
      lease$policy,
      sub_lm = prepared$sub_lm,
      context_prepared = session$context_prepared,
      session_state_id = session_state_id
    )

    action_history <- list(iteration = iter)
    if (identical(prepared$reasoning_mode, "rationale")) {
      action_history$reasoning <- response$reasoning
    } else if (identical(prepared$reasoning_mode, "plan")) {
      action_history$plan <- response$plan
    }
    action_history$code <- response$code
    action_history$output <- exec_result$formatted_output
    action_history$success <- exec_result$success
    action_history$is_final <- exec_result$is_final
    action_history$action_metadata <- operations$compact_action_metadata(
      response$metadata
    )
    action_history$execution_metadata <- list(
      duration_ms = exec_result$raw_result$duration_ms %||% NA_real_,
      error_type = exec_result$raw_result$error_type %||% NULL
    )
    history[[iter]] <- action_history

    if (isTRUE(exec_result$success) && isTRUE(exec_result$is_final)) {
      final_answer <- exec_result$final_value
      if (module$verbose) {
        cli::cli_alert_success("SUBMIT called with answer")
      }
      break
    }

    if (!exec_result$success) {
      if (module$verbose) {
        cli::cli_alert_warning(
          "Iteration {iter}: Code execution failed - {exec_result$error}"
        )
      }
      next
    }

    if (module$verbose) {
      cli::cli_alert(
        "Output: {substr(exec_result$formatted_output, 1, 200)}..."
      )
    }
  }

  final_source <- "submit"
  if (is.null(final_answer)) {
    final_source <- "fallback"
    cli::cli_warn(
      c(
        "RLM reached max_iterations ({module$max_iterations}) without SUBMIT()",
        "i" = "Using fallback extraction from trajectory",
        "i" = "Consider increasing max_iterations or simplifying the query"
      ),
      call = rlang::caller_env()
    )
    fallback_result <- operations$extract_fallback(
      prepared$inputs,
      history,
      prepared$base_llm,
      trace = trace,
      .cache = prepared$cache,
      prior_context = prepared$prior_context,
      context_desc = if (!is.null(prepared$supplied_history)) {
        context_desc
      } else {
        NULL
      }
    )
    final_answer <- fallback_result$value
    fallback_metadata <- fallback_result$metadata
    result_chat <- fallback_result$chat
  }

  list(
    start_time = start_time,
    history = history,
    final_answer = final_answer,
    final_source = final_source,
    fallback_metadata = fallback_metadata,
    result_chat = result_chat,
    call_counter = session$call_counter,
    task_text = task_text,
    lease = lease
  )
}

rlm_finalize_invocation_phase <- function(
  module,
  prepared,
  execution,
  operations,
  trace
) {
  output <- operations$build_output(
    execution$final_answer,
    source = execution$final_source
  )
  next_history <- if (is.null(prepared$supplied_history)) {
    NULL
  } else {
    portable_trajectory <- lapply(execution$history, function(step) {
      list(
        iteration = as.integer(step$iteration),
        code = step$code,
        output = step$output,
        success = isTRUE(step$success),
        is_final = isTRUE(step$is_final)
      )
    })
    history_append_rlm(
      prepared$supplied_history,
      task_text = execution$task_text,
      trajectory = portable_trajectory,
      output = output,
      output_source = execution$final_source
    )
  }

  call_counter <- execution$call_counter
  usage <- operations$summarize_action_usage(
    execution$history,
    fallback_metadata = execution$fallback_metadata,
    recursive_metadata = call_counter$usage
  )
  action_calls <- length(execution$history)
  extraction_calls <- as.integer(identical(execution$final_source, "fallback"))
  action_provider_calls <- operations$summarize_provider_calls(
    lapply(execution$history, function(entry) entry$action_metadata %||% list())
  )
  extraction_provider_calls <- if (extraction_calls == 1L) {
    operations$metadata_provider_calls(execution$fallback_metadata)
  } else {
    0L
  }
  recursive_provider_calls <- if (isTRUE(call_counter$provider_calls_known)) {
    as.integer(call_counter$provider_calls)
  } else {
    NA_integer_
  }
  provider_calls <- operations$sum_provider_call_counts(c(
    action_provider_calls,
    recursive_provider_calls,
    extraction_provider_calls
  ))

  if (trace) {
    module$state$trace_sequence <- module$state$trace_sequence + 1
    input_summary <- operations$summarize_trace_inputs(prepared$inputs)
    module$state$repl_history <- operations$append_bounded_trace(
      module$state$repl_history,
      list(
        timestamp = execution$start_time,
        inputs = input_summary,
        history = execution$history,
        final_answer = output,
        iterations_used = length(execution$history),
        llm_calls_used = call_counter$count
      )
    )
    trace_entry <- list(
      timestamp = execution$start_time,
      inputs = input_summary,
      output = output,
      history = execution$history,
      latency_ms = as.numeric(difftime(
        Sys.time(),
        execution$start_time,
        units = "secs"
      )) *
        1000,
      tokens = usage,
      input_tokens = usage$input_tokens,
      cached_input_tokens = usage$cached_input_tokens,
      output_tokens = usage$output_tokens,
      total_tokens = usage$total_tokens,
      cost = usage$cost,
      provider_calls = provider_calls,
      action_calls = action_calls,
      action_provider_calls = action_provider_calls,
      recursive_calls = call_counter$count,
      recursive_provider_calls = recursive_provider_calls,
      extraction_calls = extraction_calls,
      extraction_provider_calls = extraction_provider_calls,
      model = operations$rlm_model_name(prepared$base_llm),
      output_source = execution$final_source
    )
    module$state$traces <- operations$append_bounded_trace(
      module$state$traces,
      trace_entry
    )
    add_to_global_history(trace_entry, source = "RLMModule")
  }

  duration_ms <- as.numeric(difftime(
    Sys.time(),
    execution$start_time,
    units = "secs"
  )) *
    1000

  metadata <- list(
    model = "rlm",
    iterations = length(execution$history),
    max_iterations = module$max_iterations,
    llm_calls = call_counter$count,
    max_llm_calls = module$max_llm_calls,
    output_source = execution$final_source,
    duration_ms = round(duration_ms, 2),
    repl_history = execution$history,
    input_tokens = usage$input_tokens,
    cached_input_tokens = usage$cached_input_tokens,
    output_tokens = usage$output_tokens,
    total_tokens = usage$total_tokens,
    cost = usage$cost,
    provider_calls = provider_calls,
    action_calls = action_calls,
    action_provider_calls = action_provider_calls,
    recursive_calls = call_counter$count,
    recursive_provider_calls = recursive_provider_calls,
    extraction_calls = extraction_calls,
    extraction_provider_calls = extraction_provider_calls,
    runner_policy = execution$lease$policy_summary,
    runner_lifecycle = if (execution$lease$owned) {
      "invocation-owned"
    } else {
      "caller-owned"
    }
  )
  if (!is.null(next_history)) {
    metadata$conversation_history <- next_history
  }
  if (!identical(prepared$reasoning_mode, "rationale")) {
    metadata$reasoning_mode <- prepared$reasoning_mode
  }
  if (identical(prepared$reasoning_mode, "native")) {
    native_reasoning <- operations$aggregate_native_reasoning(execution$history)
    metadata["native_reasoning"] <- list(native_reasoning$contents)
    metadata["native_reasoning_action_indices"] <- list(
      native_reasoning$action_indices
    )
  }

  tibble::tibble(
    output = list(output),
    chat = list(execution$result_chat),
    metadata = list(metadata)
  )
}

rlm_format_excerpt <- function(text, max_chars) {
  if (is.null(text) || length(text) == 0L) {
    return("")
  }
  text <- paste(as.character(text), collapse = "\n")
  total <- nchar(text, type = "chars")
  if (total <= max_chars) {
    return(text)
  }
  marker <- paste0("\n... [", total, " total characters] ...\n")
  if (nchar(marker) + 2L > max_chars) {
    return(substr(text, 1L, max_chars))
  }
  omitted <- total
  for (iteration in seq_len(10L)) {
    marker <- paste0(
      "\n... [",
      omitted,
      " characters omitted; total ",
      total,
      "] ...\n"
    )
    if (nchar(marker) + 2L > max_chars) {
      return(substr(text, 1L, max_chars))
    }
    visible <- max_chars - nchar(marker)
    head_chars <- ceiling(visible / 2)
    tail_chars <- floor(visible / 2)
    next_omitted <- total - head_chars - tail_chars
    if (identical(next_omitted, omitted)) {
      break
    }
    omitted <- next_omitted
  }
  paste0(
    substr(text, 1L, head_chars),
    marker,
    substr(text, total - tail_chars + 1L, total)
  )
}

rlm_format_execution_output <- function(result, max_output_chars) {
  parts <- character()

  # Safely check stdout (may be NULL or missing)
  stdout_val <- result$stdout
  if (
    !is.null(stdout_val) &&
      is.character(stdout_val) &&
      nchar(stdout_val) > 0
  ) {
    parts <- c(parts, paste0("stdout:\n", stdout_val))
  }

  # Safely check messages (may be NULL or missing)
  messages_val <- result$messages
  if (
    !is.null(messages_val) &&
      is.character(messages_val) &&
      nchar(messages_val) > 0
  ) {
    parts <- c(parts, paste0("messages:\n", messages_val))
  }

  # Safely check warnings (may be NULL or missing)
  warnings_val <- result$warnings
  if (
    !is.null(warnings_val) &&
      is.character(warnings_val) &&
      nchar(warnings_val) > 0
  ) {
    parts <- c(parts, paste0("warnings:\n", warnings_val))
  }

  if (!is.null(result$result)) {
    result_str <- tryCatch(
      {
        if (is.data.frame(result$result)) {
          rows <- nrow(result$result)
          shown <- if (rows <= 20L) {
            result$result
          } else {
            rbind(
              utils::head(result$result, 10L),
              utils::tail(result$result, 10L)
            )
          }
          rendered <- paste(
            utils::capture.output(print(shown)),
            collapse = "\n"
          )
          if (rows > 20L) {
            paste0(
              rendered,
              "\n... [",
              rows - 20L,
              " data-frame rows omitted; ",
              rows,
              " total]"
            )
          } else {
            rendered
          }
        } else if (is.atomic(result$result) && length(result$result) <= 10) {
          paste(result$result, collapse = ", ")
        } else {
          paste(
            utils::capture.output(utils::str(result$result)),
            collapse = "\n"
          )
        }
      },
      error = function(e) deparse(result$result)[1]
    )
    parts <- c(parts, paste0("result:\n", result_str))
  }

  if (length(parts) == 0) {
    return("[No output]")
  }

  rlm_format_excerpt(paste(parts, collapse = "\n\n"), max_output_chars)
}
