# Dataset evaluation and its accounting, including resumable row execution.

optimizer_evaluate_dataset <- function(
  program,
  dataset,
  metric,
  .llm,
  control,
  epochs,
  ...
) {
  # OptimizerControl@max_errors is enforced by the optimizer outcome ledger.
  # Batch max_errors has different row-cancellation semantics and must not
  # truncate an optimizer evaluation before that ledger observes the result.
  concurrency <- concurrency_control(
    max_active = control@num_threads,
    max_errors = Inf
  )
  tryCatch(
    {
      evaluate(
        program,
        data = dataset,
        metric = metric,
        .llm = .llm,
        .concurrency = concurrency,
        .progress = control@progress,
        .return_format = "structured",
        epochs = epochs,
        ...
      )
    },
    error = function(e) {
      provider_condition <- run_provider_error_condition(e)
      if (!is.null(provider_condition)) {
        stop(provider_condition)
      }
      cli::cli_warn(
        "Evaluation failed: {conditionMessage(e)}",
        class = "dsprrr_eval_error"
      )
      # Return a result structure indicating total failure
      list(
        scores = rep(NA_real_, nrow(dataset)),
        predictions = rep(NA_character_, nrow(dataset)),
        errors = rep(conditionMessage(e), nrow(dataset)),
        mean_score = NA_real_,
        n_evaluated = 0L,
        n_errors = nrow(dataset),
        epoch_scores = NULL,
        score_std = NA_real_,
        ci_95 = c(NA_real_, NA_real_)
      )
    }
  )
}

optimizer_eval_result <- function(
  program,
  dataset,
  eval_result,
  epochs,
  start_time,
  end_time,
  input_signature
) {
  total_latency_ms <- as.numeric(difftime(
    end_time,
    start_time,
    units = "secs"
  )) *
    1000

  # Prefer the canonical per-row execution metadata returned by evaluate().
  # Traces are only a fallback: one program trace is not necessarily one
  # provider call for composite or iterative modules.
  cost_info <- extract_optimizer_usage(
    program,
    eval_result$metadata %||% NULL,
    epochs = epochs
  )

  # Build per-example tibble
  n <- nrow(dataset)
  ordered_errors <- rep(NA_character_, n)
  compact_errors <- eval_result$errors %||% character(0)
  if (length(compact_errors) == n) {
    ordered_errors <- compact_errors
  } else if (length(compact_errors) > 0L) {
    failed_rows <- which(is.na(eval_result$scores))
    assign_count <- min(length(compact_errors), length(failed_rows))
    if (assign_count > 0L) {
      ordered_errors[failed_rows[seq_len(assign_count)]] <-
        compact_errors[seq_len(assign_count)]
    }
  }

  examples <- tibble::tibble(
    row_id = seq_len(n),
    score = eval_result$scores,
    error = ordered_errors,
    predicted = eval_result$predictions,
    feedback = if (length(eval_result$feedbacks %||% character(0)) == n) {
      eval_result$feedbacks
    } else {
      rep(NA_character_, n)
    },
    program_trace = if (length(eval_result$traces %||% list()) == n) {
      eval_result$traces
    } else {
      rep(list(NULL), n)
    }
  )

  # Add input columns from dataset
  input_names <- get_input_names(input_signature)
  for (name in input_names) {
    if (name %in% names(dataset)) {
      examples[[paste0("input_", name)]] <- dataset[[name]]
    }
  }

  # Compute standard error
  valid_scores <- eval_result$scores[!is.na(eval_result$scores)]
  std_error <- if (length(valid_scores) > 1) {
    stats::sd(valid_scores) / sqrt(length(valid_scores))
  } else {
    NA_real_
  }

  # Extract epoch-specific fields if present
  epoch_count <- as.integer(epochs)
  epoch_scores_list <- if (!is.null(eval_result$epoch_scores)) {
    eval_result$epoch_scores
  } else {
    list()
  }
  score_std <- if (!is.null(eval_result$score_std)) {
    eval_result$score_std
  } else {
    NA_real_
  }
  ci_values <- if (!is.null(eval_result$ci_95)) {
    eval_result$ci_95
  } else {
    c(NA_real_, NA_real_)
  }

  EvalResult(
    examples = examples,
    mean_score = eval_result$mean_score,
    std_error = std_error,
    n_evaluated = as.integer(eval_result$n_evaluated),
    n_errors = as.integer(eval_result$n_errors),
    input_tokens = as.integer(cost_info$tokens_in),
    output_tokens = as.integer(cost_info$tokens_out),
    total_tokens = as.integer(cost_info$total_tokens),
    total_cost = cost_info$total_cost,
    provider_calls = as.integer(cost_info$provider_calls),
    metric_calls = as.integer(cost_info$metric_calls),
    provider_usage_unknown = cost_info$provider_usage_unknown,
    token_usage_unknown = cost_info$token_usage_unknown,
    total_latency_ms = total_latency_ms,
    start_time = start_time,
    end_time = end_time,
    epochs = epoch_count,
    epoch_scores = epoch_scores_list,
    score_std = score_std,
    ci_lower = ci_values[1],
    ci_upper = ci_values[2],
    trace_context = eval_result$trace_context %||% current_trace_context()
  )
}

optimizer_usage_scalar <- function(value, integer = FALSE) {
  if (
    is.null(value) ||
      length(value) != 1L ||
      !is.numeric(value) ||
      is.na(value) ||
      !is.finite(value) ||
      value < 0
  ) {
    return(if (integer) NA_integer_ else NA_real_)
  }
  if (integer) as.integer(value) else as.numeric(value)
}

optimizer_usage_sum <- function(values, integer = FALSE) {
  values <- unlist(values, use.names = FALSE)
  if (length(values) == 0L) {
    return(if (integer) 0L else 0)
  }
  if (anyNA(values)) {
    return(if (integer) NA_integer_ else NA_real_)
  }
  total <- sum(values)
  if (integer) as.integer(total) else total
}

optimizer_metadata_provider_calls <- function(program, metadata) {
  if ("provider_calls" %in% names(metadata)) {
    return(optimizer_usage_scalar(
      metadata$provider_calls,
      integer = TRUE
    ))
  }

  if (inherits(program, "FlexModule")) {
    predictor_calls <- optimizer_usage_scalar(
      metadata$predictor_calls,
      integer = TRUE
    )
    step_metadata <- metadata$steps %||% list()
    if (
      is.na(predictor_calls) ||
        length(step_metadata) != predictor_calls
    ) {
      return(predictor_calls)
    }
    calls <- vapply(
      step_metadata,
      function(step) {
        item <- step$metadata %||% list()
        if ("provider_calls" %in% names(item)) {
          return(optimizer_usage_scalar(
            item$provider_calls,
            integer = TRUE
          ))
        }
        if (identical(item$cache %||% "unknown", "hit")) 0L else 1L
      },
      integer(1)
    )
    return(optimizer_usage_sum(calls, integer = TRUE))
  }

  if (identical(class(program)[1L], "PredictModule")) {
    cache_status <- metadata$cache %||% "unknown"
    if (identical(cache_status, "hit")) {
      return(0L)
    }
    # The base Predict batch/scalar path invokes exactly one structured Chat
    # request. Specialized subclasses are handled below as unknown unless they
    # report an explicit count.
    return(1L)
  }

  if (inherits(program, "PipelineModule")) {
    step_metadata <- metadata$step_metadata %||%
      metadata$aggregated$step_metadata %||%
      list()
    if (length(step_metadata) != length(program$steps)) {
      return(NA_integer_)
    }
    calls <- vapply(
      seq_along(program$steps),
      function(i) {
        optimizer_metadata_provider_calls(
          program$steps[[i]]@module,
          step_metadata[[i]]
        )
      },
      integer(1)
    )
    return(optimizer_usage_sum(calls, integer = TRUE))
  }

  if (inherits(program, "FnModule") && is.null(metadata$model)) {
    return(0L)
  }

  NA_integer_
}

optimizer_metadata_usage <- function(program, metadata) {
  if (!is.list(metadata)) {
    return(list(
      tokens_in = NA_integer_,
      tokens_out = NA_integer_,
      total_tokens = NA_integer_,
      total_cost = NA_real_,
      provider_calls = NA_integer_
    ))
  }

  provider_calls <- optimizer_metadata_provider_calls(program, metadata)
  if (identical(metadata$cache %||% NULL, "hit")) {
    return(list(
      tokens_in = 0L,
      tokens_out = 0L,
      total_tokens = 0L,
      total_cost = 0,
      provider_calls = provider_calls
    ))
  }

  tokens_in <- optimizer_usage_scalar(metadata$input_tokens, integer = TRUE)
  tokens_out <- optimizer_usage_scalar(metadata$output_tokens, integer = TRUE)
  total_tokens <- optimizer_usage_scalar(metadata$total_tokens, integer = TRUE)
  if (is.na(total_tokens) && !anyNA(c(tokens_in, tokens_out))) {
    total_tokens <- tokens_in + tokens_out
  }

  list(
    tokens_in = tokens_in,
    tokens_out = tokens_out,
    total_tokens = total_tokens,
    total_cost = optimizer_usage_scalar(
      metadata$cost,
      integer = FALSE
    ),
    provider_calls = provider_calls
  )
}

# Extract only usage that can be proved from canonical current-call metadata.
# Ambiguous provider or token usage is represented explicitly rather than as 0.
extract_optimizer_usage <- function(program, metadata, epochs = 1L) {
  if (is.list(metadata) && length(metadata) > 0L && epochs == 1L) {
    rows <- lapply(metadata, function(item) {
      optimizer_metadata_usage(program, item)
    })
    tokens_in <- optimizer_usage_sum(
      lapply(rows, `[[`, "tokens_in"),
      integer = TRUE
    )
    tokens_out <- optimizer_usage_sum(
      lapply(rows, `[[`, "tokens_out"),
      integer = TRUE
    )
    total_tokens <- optimizer_usage_sum(
      lapply(rows, `[[`, "total_tokens"),
      integer = TRUE
    )
    total_cost <- optimizer_usage_sum(lapply(rows, `[[`, "total_cost"))
    provider_calls <- optimizer_usage_sum(
      lapply(rows, `[[`, "provider_calls"),
      integer = TRUE
    )
    metric_calls <- sum(vapply(
      metadata,
      function(item) {
        !optimizer_error_present(item$error %||% NULL)
      },
      logical(1)
    ))

    return(list(
      tokens_in = tokens_in,
      tokens_out = tokens_out,
      total_tokens = total_tokens,
      total_cost = total_cost,
      provider_calls = provider_calls,
      metric_calls = as.integer(metric_calls),
      provider_usage_unknown = is.na(provider_calls),
      token_usage_unknown = anyNA(c(tokens_in, tokens_out, total_tokens))
    ))
  }

  fallback <- extract_cost_from_module(program)
  list(
    tokens_in = fallback$tokens_in,
    tokens_out = fallback$tokens_out,
    total_tokens = fallback$total_tokens,
    total_cost = fallback$total_cost,
    provider_calls = NA_integer_,
    metric_calls = NA_integer_,
    provider_usage_unknown = TRUE,
    token_usage_unknown = anyNA(c(
      fallback$tokens_in,
      fallback$tokens_out,
      fallback$total_tokens
    ))
  )
}

optimizer_min_provider_calls <- function(program) {
  if (inherits(program, "FnModule")) {
    return(0L)
  }
  if (inherits(program, "FlexModule")) {
    return(flex_predictor_call_count(program))
  }
  if (inherits(program, "PipelineModule")) {
    calls <- vapply(
      program$steps,
      function(step) {
        optimizer_min_provider_calls(step@module)
      },
      integer(1)
    )
    return(sum(calls))
  }
  if (inherits(program, "MultiChainComparisonModule")) {
    return(as.integer(program$M + 1L))
  }
  if (inherits(program, "ReActModule")) {
    return(2L)
  }
  # All other current optimizer-supported Module implementations perform at
  # least one provider operation. Exact postflight counts still come only from
  # canonical metadata; this is a conservative preflight lower bound.
  1L
}

optimizer_eval_usage <- function(eval_result) {
  list(
    metric_calls = if (is.na(eval_result@metric_calls)) {
      NA_integer_
    } else {
      eval_result@metric_calls
    },
    provider_calls = if (isTRUE(eval_result@provider_usage_unknown)) {
      NA_integer_
    } else {
      eval_result@provider_calls
    },
    input_tokens = if (isTRUE(eval_result@token_usage_unknown)) {
      NA_integer_
    } else {
      eval_result@input_tokens
    },
    output_tokens = if (isTRUE(eval_result@token_usage_unknown)) {
      NA_integer_
    } else {
      eval_result@output_tokens
    },
    total_tokens = if (isTRUE(eval_result@token_usage_unknown)) {
      NA_integer_
    } else {
      eval_result@total_tokens
    },
    known_cost = eval_result@total_cost
  )
}

optimizer_forward_output <- function(result) {
  if (
    is.data.frame(result) &&
      "output" %in% names(result) &&
      nrow(result) > 0L
  ) {
    return(result$output[[1L]])
  }
  result
}

optimizer_forward_usage <- function(program, result) {
  metadata <- if (
    is.data.frame(result) &&
      "metadata" %in% names(result) &&
      nrow(result) > 0L
  ) {
    result$metadata[[1L]]
  } else {
    NULL
  }
  usage <- optimizer_metadata_usage(program, metadata)
  list(
    provider_calls = usage$provider_calls,
    input_tokens = usage$tokens_in,
    output_tokens = usage$tokens_out,
    total_tokens = usage$total_tokens,
    known_cost = usage$total_cost
  )
}

optimizer_unknown_provider_usage <- function() {
  list(
    provider_calls = NA_integer_,
    input_tokens = NA_integer_,
    output_tokens = NA_integer_,
    total_tokens = NA_integer_,
    known_cost = NA_real_
  )
}

# Run one direct optimizer-side Chat request as a bounded work unit. A direct
# request is one known provider call, but token/cost usage is only known when a
# verified Chat turn delta exposes it.
optimizer_budgeted_provider_call <- function(
  budget,
  model,
  stage,
  unit_id,
  call,
  success = function(value, condition) is.null(condition),
  work_unit = "optimizer_provider_call"
) {
  optimizer_budget_validate_stage(stage)
  if (!is.function(call) || !is.function(success)) {
    cli::cli_abort(
      "{.arg call} and {.arg success} must be functions",
      class = "dsprrr_optimizer_invariant_error"
    )
  }
  if (is.null(model)) {
    return(list(started = FALSE, value = NULL, condition = NULL))
  }
  model <- assert_ellmer_chat(model, arg = "model")
  if (
    !optimizer_budget_preflight(
      budget,
      stage = stage,
      planned = list(
        provider_calls = 1L,
        input_tokens = 1L,
        output_tokens = 1L,
        total_tokens = 1L
      ),
      unit_id = unit_id,
      work_unit = work_unit,
      max_started = 0L
    )
  ) {
    return(list(started = FALSE, value = NULL, condition = NULL))
  }

  turns_before <- batch_chat_turns(model)
  condition <- NULL
  value <- tryCatch(
    call(),
    error = function(e) {
      condition <<- e
      NULL
    }
  )
  metadata <- chat_usage_metadata(model, turns_before = turns_before)
  record_optimizer_usage(
    budget,
    list(
      provider_calls = 1L,
      input_tokens = metadata$input_tokens,
      output_tokens = metadata$output_tokens,
      total_tokens = metadata$total_tokens,
      known_cost = metadata$cost
    ),
    stage = stage,
    unit_id = unit_id,
    work_unit = work_unit,
    max_started = 1L
  )
  record_optimizer_outcome(
    budget,
    success = isTRUE(success(value, condition)),
    stage = stage,
    condition = condition
  )
  optimizer_budget_complete_unit(budget, unit_id)
  list(started = TRUE, value = value, condition = condition)
}

optimizer_eval_row_record <- function(eval_result, row_index) {
  example <- eval_result@examples[1L, , drop = FALSE]
  record <- list(
    row_index = as.integer(row_index),
    score = as.numeric(example$score[[1L]] %||% NA_real_),
    error = as.character(example$error[[1L]] %||% NA_character_),
    predicted = if ("predicted" %in% names(example)) {
      example$predicted[[1L]]
    } else {
      NA
    },
    feedback = as.character(example$feedback[[1L]] %||% NA_character_),
    program_trace = if ("program_trace" %in% names(example)) {
      example$program_trace[[1L]]
    } else {
      NULL
    },
    input_tokens = eval_result@input_tokens,
    output_tokens = eval_result@output_tokens,
    total_tokens = eval_result@total_tokens,
    total_cost = eval_result@total_cost,
    provider_calls = eval_result@provider_calls,
    metric_calls = eval_result@metric_calls,
    provider_usage_unknown = eval_result@provider_usage_unknown,
    tokens_unknown = eval_result@token_usage_unknown,
    latency_ms = eval_result@total_latency_ms
  )
  if ("error_class" %in% names(example)) {
    record$error_class <- as.character(
      example$error_class[[1L]] %||% NA_character_
    )
  }
  record
}

optimizer_eval_checkpoint_records <- function(records, project = NULL) {
  if (is.function(project)) {
    projected <- project(records)
    if (!is.list(projected) || length(projected) != length(records)) {
      cli::cli_abort(
        "Optimizer row projection must return one record per evaluation row",
        class = "dsprrr_optimizer_checkpoint_unsafe_value"
      )
    }
    return(optimizer_checkpoint_sanitize(
      projected,
      "progress.evaluation_rows"
    ))
  }

  lapply(records, function(record) {
    record$predicted <- NULL
    # Raw trace events can contain prompts and model responses. They are useful
    # during live optimization, but never belong in resumable checkpoints.
    record$program_trace <- NULL
    record
  })
}

optimizer_eval_restore_records <- function(records) {
  if (is.null(records)) {
    return(list())
  }
  if (!is.list(records)) {
    cli::cli_abort(
      "Partial optimizer evaluation state must be a list",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  lapply(records, function(record) {
    if (!is.list(record) || is.null(record$row_index)) {
      cli::cli_abort(
        "Partial optimizer row state is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    if (!"predicted" %in% names(record)) {
      record["predicted"] <- list(NA)
    }
    record$program_trace <- record$program_trace %||% NULL
    record
  })
}

optimizer_eval_known_sum <- function(records, field, integer = FALSE) {
  values <- vapply(
    records,
    function(record) {
      value <- record[[field]]
      if (is.null(value) || length(value) != 1L) NA_real_ else as.numeric(value)
    },
    numeric(1)
  )
  if (length(values) == 0L || anyNA(values)) {
    return(if (integer) NA_integer_ else NA_real_)
  }
  if (integer) as.integer(sum(values)) else sum(values)
}

optimizer_combine_eval_records <- function(records, dataset) {
  if (length(records) == 0L) {
    return(EvalResult(
      examples = tibble::tibble(),
      mean_score = NA_real_,
      n_evaluated = 0L,
      n_errors = 0L,
      trace_context = current_trace_context()
    ))
  }
  order_index <- order(vapply(records, `[[`, integer(1), "row_index"))
  records <- records[order_index]
  row_indices <- vapply(records, `[[`, integer(1), "row_index")
  scores <- vapply(records, function(record) record$score, numeric(1))
  errors <- vapply(
    records,
    function(record) {
      record$error %||% NA_character_
    },
    character(1)
  )
  feedback <- vapply(
    records,
    function(record) {
      record$feedback %||% NA_character_
    },
    character(1)
  )
  predictions <- lapply(records, `[[`, "predicted")
  traces <- lapply(records, `[[`, "program_trace")
  examples <- tibble::tibble(
    row_id = row_indices,
    score = scores,
    error = errors,
    predicted = predictions,
    feedback = feedback,
    program_trace = traces
  )
  if (
    any(vapply(
      records,
      function(record) !is.null(record$error_class),
      logical(1)
    ))
  ) {
    examples$error_class <- vapply(
      records,
      function(record) record$error_class %||% NA_character_,
      character(1)
    )
  }
  input_names <- intersect(names(dataset), names(dataset))
  for (name in input_names) {
    examples[[paste0("input_", name)]] <- dataset[[name]][row_indices]
  }
  valid_scores <- scores[!is.na(scores)]
  std_error <- if (length(valid_scores) > 1L) {
    stats::sd(valid_scores) / sqrt(length(valid_scores))
  } else {
    NA_real_
  }
  provider_unknown <- any(vapply(
    records,
    function(record) {
      isTRUE(record$provider_usage_unknown)
    },
    logical(1)
  ))
  token_unknown <- any(vapply(
    records,
    function(record) {
      isTRUE(record$tokens_unknown)
    },
    logical(1)
  ))
  total_cost <- optimizer_eval_known_sum(records, "total_cost")
  provider_calls <- optimizer_eval_known_sum(
    records,
    "provider_calls",
    integer = TRUE
  )
  metric_calls <- optimizer_eval_known_sum(
    records,
    "metric_calls",
    integer = TRUE
  )

  EvalResult(
    examples = examples,
    mean_score = failure_adjusted_mean(scores),
    std_error = std_error,
    n_evaluated = as.integer(sum(!is.na(scores))),
    n_errors = as.integer(sum(is.na(scores))),
    input_tokens = optimizer_eval_known_sum(
      records,
      "input_tokens",
      integer = TRUE
    ),
    output_tokens = optimizer_eval_known_sum(
      records,
      "output_tokens",
      integer = TRUE
    ),
    total_tokens = optimizer_eval_known_sum(
      records,
      "total_tokens",
      integer = TRUE
    ),
    total_cost = total_cost,
    provider_calls = provider_calls,
    metric_calls = metric_calls,
    provider_usage_unknown = provider_unknown || is.na(provider_calls),
    token_usage_unknown = token_unknown,
    total_latency_ms = optimizer_eval_known_sum(records, "latency_ms"),
    trace_context = current_trace_context()
  )
}

optimizer_budget_requires_row_units <- function(budget) {
  limits <- budget$limits[c(
    "max_metric_calls",
    "max_provider_calls",
    "max_input_tokens",
    "max_output_tokens",
    "max_total_tokens",
    "max_cost",
    "max_elapsed_seconds"
  )]
  !all(vapply(limits, is.null, logical(1)))
}

# Evaluate one candidate with the whole-evaluation path when
# no fine-grained resource cap is active. Any metric/provider/token/cost/time cap
# switches to row units so its only postflight overshoot is one started row.
optimizer_eval_candidate <- function(
  program,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  budget = NULL,
  stage,
  unit_id,
  ...
) {
  control <- control %||% optimizer_control()
  budget <- budget %||% new_optimizer_budget(control)
  if (optimizer_budget_requires_row_units(budget)) {
    return(optimizer_eval_program(
      program,
      dataset,
      metric,
      .llm = .llm,
      control = control,
      budget = budget,
      stage = stage,
      unit_id = unit_id,
      ...
    ))
  }
  if (
    !optimizer_budget_preflight(
      budget,
      stage = stage,
      planned = list(trials = 1L),
      unit_id = unit_id,
      work_unit = "optimizer_trial",
      max_started = 0L,
      planned_outcomes = max(1L, nrow(dataset))
    )
  ) {
    return(EvalResult(trace_context = current_trace_context()))
  }
  result <- eval_program(
    program,
    dataset,
    metric,
    .llm = .llm,
    control = control,
    ...
  )
  record_optimizer_usage(
    budget,
    optimizer_eval_usage(result),
    stage = stage,
    unit_id = unit_id,
    work_unit = "evaluation_dataset",
    max_started = 1L
  )
  record_eval_result_outcomes(budget, result, stage)
  optimizer_budget_count_trial(budget, stage, unit_id)
  optimizer_budget_complete_unit(budget, unit_id)
  result
}

optimizer_clone_program_leaf_chats <- function(program) {
  graph <- module_graph(program, boundaries = "cross", cycles = "error")
  leaf <- vapply(
    graph$path,
    function(path) {
      prefix <- if (identical(path, "$")) "$/" else paste0(path, "/")
      !any(startsWith(graph$path, prefix))
    },
    logical(1)
  )
  for (module in graph$module[leaf]) {
    chat <- tryCatch(module$chat, error = function(error) NULL)
    if (!is.null(chat)) {
      module$chat <- clone_ellmer_chat(
        chat,
        arg = "module$chat",
        reset_turns = TRUE
      )
    }
  }
  program
}

# Evaluate one optimizer candidate in row-sized work units. The maximum
# postflight overshoot is therefore the usage of one already-started evaluation
# row in the current sequential implementation. When the shared concurrency
# contract supplies `effective_workers`, callers may raise `max_started` to that
# exact value without changing ledger semantics.
optimizer_eval_program <- function(
  program,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  budget,
  stage,
  unit_id,
  partial_records = list(),
  on_progress = NULL,
  checkpoint_projection = NULL,
  clone_program_per_row = FALSE,
  ...
) {
  if (is.null(control)) {
    control <- optimizer_control()
  }
  if (!is.data.frame(dataset)) {
    cli::cli_abort("{.arg dataset} must be a data frame")
  }
  if (
    !is.logical(clone_program_per_row) ||
      length(clone_program_per_row) != 1L ||
      is.na(clone_program_per_row)
  ) {
    cli::cli_abort(
      "{.arg clone_program_per_row} must be TRUE or FALSE",
      class = "dsprrr_optimizer_checkpoint_config_error"
    )
  }
  if (!is.null(checkpoint_projection) && !is.function(checkpoint_projection)) {
    cli::cli_abort(
      "{.arg checkpoint_projection} must be a function or NULL",
      class = "dsprrr_optimizer_checkpoint_config_error"
    )
  }
  optimizer_budget_validate_stage(stage)
  records <- optimizer_eval_restore_records(partial_records)
  if (length(records) == 0L) {
    can_start <- optimizer_budget_preflight(
      budget,
      stage,
      planned = list(trials = 1L),
      unit_id = unit_id,
      work_unit = "optimizer_trial",
      max_started = 0L
    )
    if (!can_start) {
      return(optimizer_combine_eval_records(records, dataset))
    }
  }

  evaluate_row <- function(program, row, llm, control, index) {
    eval_program(
      program,
      row,
      metric,
      .llm = llm,
      control = control,
      .trace_row_ids = index,
      ...
    )
  }
  records <- optimizer_eval_rows(
    program,
    dataset,
    .llm,
    control,
    budget,
    stage,
    unit_id,
    records,
    on_progress,
    checkpoint_projection,
    clone_program_per_row,
    evaluate_row
  )
  optimizer_eval_finish(
    records,
    dataset,
    budget,
    stage,
    unit_id,
    on_progress,
    checkpoint_projection
  )
}

optimizer_eval_rows <- function(
  program,
  dataset,
  .llm,
  control,
  budget,
  stage,
  unit_id,
  records,
  on_progress,
  checkpoint_projection,
  clone_program_per_row,
  evaluate_row
) {
  completed_rows <- if (length(records) == 0L) {
    integer()
  } else {
    vapply(records, `[[`, integer(1), "row_index")
  }

  min_provider_calls <- optimizer_min_provider_calls(program)
  planned <- list(metric_calls = 1L)
  if (min_provider_calls > 0L) {
    planned$provider_calls <- min_provider_calls
    planned$input_tokens <- 1L
    planned$output_tokens <- 1L
    planned$total_tokens <- 1L
  }

  for (row_index in seq_len(nrow(dataset))) {
    if (row_index %in% completed_rows) {
      next
    }
    row_unit_id <- paste0(unit_id, ":row:", row_index)
    if (
      !optimizer_budget_preflight(
        budget,
        stage,
        planned = planned,
        unit_id = row_unit_id,
        work_unit = "evaluation_row",
        max_started = 0L
      )
    ) {
      break
    }
    row_result <- optimizer_eval_row(
      program,
      dataset,
      .llm,
      control,
      row_index,
      clone_program_per_row,
      evaluate_row
    )
    record_optimizer_usage(
      budget,
      optimizer_eval_usage(row_result),
      stage,
      unit_id = row_unit_id,
      work_unit = "evaluation_row",
      max_started = 1L
    )
    record_eval_result_outcomes(budget, row_result, stage)
    records[[length(records) + 1L]] <- optimizer_eval_row_record(
      row_result,
      row_index
    )
    optimizer_budget_complete_unit(budget, row_unit_id)
    optimizer_eval_publish_progress(
      on_progress,
      records,
      row_index,
      budget,
      checkpoint_projection
    )
    if (optimizer_budget_stopped(budget)) {
      break
    }
  }
  records
}

optimizer_eval_row <- function(
  program,
  dataset,
  .llm,
  control,
  row_index,
  clone_program_per_row,
  evaluate_row
) {
  row_program <- if (isTRUE(clone_program_per_row)) {
    copy_module(program)
  } else {
    program
  }
  if (isTRUE(clone_program_per_row) && is.null(.llm)) {
    row_program <- optimizer_clone_program_leaf_chats(row_program)
  }
  row_llm <- if (isTRUE(clone_program_per_row) && !is.null(.llm)) {
    clone_ellmer_chat(.llm, arg = ".llm", reset_turns = TRUE)
  } else {
    .llm
  }
  evaluate_row(
    row_program,
    dataset[row_index, , drop = FALSE],
    row_llm,
    optimizer_control(
      seed = control@seed,
      max_errors = control@max_errors,
      num_threads = 1L,
      progress = FALSE,
      log_dir = control@log_dir
    ),
    row_index
  )
}

optimizer_eval_finish <- function(
  records,
  dataset,
  budget,
  stage,
  unit_id,
  on_progress,
  checkpoint_projection
) {
  result <- optimizer_combine_eval_records(records, dataset)
  if (length(records) > 0L) {
    optimizer_budget_count_trial(budget, stage, unit_id)
  }
  if (length(records) == nrow(dataset) && nrow(dataset) > 0L) {
    optimizer_budget_complete_unit(budget, unit_id)
  }
  optimizer_eval_publish_progress(
    on_progress,
    records,
    if (length(records) == 0L) {
      0L
    } else {
      max(vapply(records, `[[`, integer(1), "row_index"))
    },
    budget,
    checkpoint_projection
  )
  result
}

optimizer_eval_publish_progress <- function(
  on_progress,
  records,
  row_index,
  budget,
  checkpoint_projection
) {
  if (is.function(on_progress)) {
    on_progress(
      optimizer_eval_checkpoint_records(
        records,
        project = checkpoint_projection
      ),
      row_index,
      budget
    )
  }
}
