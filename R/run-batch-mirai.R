#' Construct a typed mirai boundary condition
#' @noRd
mirai_boundary_condition <- function(message, classes) {
  rlang::error_cnd(
    class = classes,
    message = as.character(message)[1]
  )
}

#' Whether mirai reported its documented timeout error value
#' @noRd
is_mirai_timeout_record <- function(record) {
  inherits(record, "errorValue") &&
    length(record) == 1L &&
    isTRUE(as.integer(record) == 5L)
}

#' Whether a converted mirai response may legitimately be NULL
#' @noRd
mirai_output_allows_null <- function(output_type) {
  !isTRUE(output_type@required) ||
    inherits(output_type, c("ellmer::TypeJsonSchema", "ellmer::TypeIgnore"))
}

#' Validate an untrusted record returned across the mirai process boundary
#' @noRd
validate_mirai_worker_record <- function(
  record,
  fallback_started_at,
  fallback_ended_at,
  allow_null_response = FALSE
) {
  invalid <- function(message) {
    list(
      record = NULL,
      error = mirai_boundary_condition(
        message,
        c("dsprrr_mirai_record_error", "dsprrr_mirai_worker_error")
      ),
      started_at = fallback_started_at,
      ended_at = fallback_ended_at,
      usage = list(),
      model = NA_character_,
      turns_before = NULL
    )
  }

  if (inherits(record, "miraiError")) {
    message <- attr(record, "message", exact = TRUE)
    if (is.null(message) || !nzchar(as.character(message)[1])) {
      message <- as.character(record)[1]
    }
    return(list(
      record = NULL,
      error = mirai_boundary_condition(
        message,
        c("dsprrr_mirai_worker_error", "dsprrr_mirai_error")
      ),
      started_at = fallback_started_at,
      ended_at = fallback_ended_at,
      usage = list(),
      model = NA_character_,
      turns_before = NULL
    ))
  }
  if (!is.list(record) || length(record) == 0L || is.null(names(record))) {
    return(invalid("mirai worker returned a non-record value"))
  }
  if (anyDuplicated(names(record)) > 0L) {
    return(invalid("mirai worker record contains duplicate fields"))
  }

  required <- c(
    "ok",
    "started_at",
    "ended_at",
    "usage",
    "model",
    "turns_before"
  )
  missing <- setdiff(required, names(record))
  if (length(missing) > 0L) {
    return(invalid(paste0(
      "mirai worker record is missing fields: ",
      paste(missing, collapse = ", ")
    )))
  }
  if (
    !is.logical(record$ok) ||
      length(record$ok) != 1L ||
      is.na(record$ok)
  ) {
    return(invalid("mirai worker record has an invalid ok field"))
  }
  valid_time <- function(value) {
    inherits(value, "POSIXt") && length(value) == 1L && !is.na(value)
  }
  if (!valid_time(record$started_at) || !valid_time(record$ended_at)) {
    return(invalid("mirai worker record has invalid timestamps"))
  }
  if (record$ended_at < record$started_at) {
    return(invalid("mirai worker record ends before it starts"))
  }
  if (!is.list(record$usage)) {
    return(invalid("mirai worker record has invalid usage metadata"))
  }
  usage_names <- names(record$usage)
  known_usage <- c(
    "input_tokens",
    "output_tokens",
    "cached_input_tokens",
    "total_tokens",
    "cost",
    "duration_s"
  )
  if (
    length(record$usage) > 0L &&
      (is.null(usage_names) ||
        anyDuplicated(usage_names) > 0L ||
        length(setdiff(usage_names, known_usage)) > 0L ||
        !all(vapply(
          record$usage,
          function(value) {
            is.numeric(value) && length(value) == 1L
          },
          logical(1)
        )))
  ) {
    return(invalid("mirai worker record has invalid usage fields"))
  }
  if (
    !is.character(record$model) ||
      length(record$model) != 1L
  ) {
    return(invalid("mirai worker record has an invalid model field"))
  }
  if (!is.null(record$turns_before) && !is.list(record$turns_before)) {
    return(invalid("mirai worker record has invalid baseline turns"))
  }

  if (isTRUE(record$ok)) {
    success_required <- c("response", "usage_verified")
    missing <- setdiff(success_required, names(record))
    if (length(missing) > 0L) {
      return(invalid(paste0(
        "successful mirai worker record is missing fields: ",
        paste(missing, collapse = ", ")
      )))
    }
    if (
      !is.logical(record$usage_verified) ||
        length(record$usage_verified) != 1L ||
        is.na(record$usage_verified)
    ) {
      return(invalid(
        "successful mirai worker record has invalid usage verification"
      ))
    }
    if (
      (is.null(record$response) && !isTRUE(allow_null_response)) ||
        inherits(record$response, "condition")
    ) {
      return(invalid("successful mirai worker record has an invalid response"))
    }
    allowed <- c(required, success_required)
    if (length(setdiff(names(record), allowed)) > 0L) {
      return(invalid("successful mirai worker record contains unknown fields"))
    }
    if (!identical(record$usage_verified, TRUE)) {
      record$usage <- list()
    }
    return(list(
      record = record,
      error = NULL,
      started_at = record$started_at,
      ended_at = record$ended_at,
      usage = record$usage,
      model = record$model,
      turns_before = record$turns_before
    ))
  }

  failure_required <- c("error_message", "error_class", "error_kind")
  missing <- setdiff(failure_required, names(record))
  if (length(missing) > 0L) {
    return(invalid(paste0(
      "failed mirai worker record is missing fields: ",
      paste(missing, collapse = ", ")
    )))
  }
  if (
    !is.character(record$error_message) ||
      length(record$error_message) != 1L ||
      is.na(record$error_message) ||
      !nzchar(record$error_message)
  ) {
    return(invalid("failed mirai worker record has no error message"))
  }
  if (
    !is.character(record$error_class) ||
      length(record$error_class) != 1L ||
      is.na(record$error_class) ||
      !nzchar(record$error_class)
  ) {
    return(invalid("failed mirai worker record has no error class"))
  }
  if (
    !is.character(record$error_kind) ||
      length(record$error_kind) != 1L ||
      !record$error_kind %in% c("provider", "serialization")
  ) {
    return(invalid("failed mirai worker record has an invalid error kind"))
  }
  allowed <- c(required, failure_required)
  if (length(setdiff(names(record), allowed)) > 0L) {
    return(invalid("failed mirai worker record contains unknown fields"))
  }
  typed_class <- if (identical(record$error_kind, "serialization")) {
    "dsprrr_mirai_serialization_error"
  } else {
    record$error_class
  }
  error <- mirai_boundary_condition(
    record$error_message,
    c(typed_class, record$error_class, "dsprrr_mirai_worker_error")
  )
  list(
    record = record,
    error = error,
    started_at = record$started_at,
    ended_at = record$ended_at,
    usage = list(),
    model = record$model,
    turns_before = record$turns_before
  )
}

#' Normalize one serializable mirai worker record in the parent process
#' @noRd
mirai_worker_result <- function(
  record,
  module,
  input_set,
  request,
  chat,
  index,
  .return_format,
  fallback_started_at = Sys.time(),
  fallback_ended_at = Sys.time()
) {
  validated <- validate_mirai_worker_record(
    record,
    fallback_started_at = fallback_started_at,
    fallback_ended_at = fallback_ended_at,
    allow_null_response = mirai_output_allows_null(
      module$signature@output_type
    )
  )
  record <- validated$record
  error <- validated$error
  metadata <- canonical_run_metadata(
    request = request,
    started_at = validated$started_at,
    ended_at = validated$ended_at,
    usage = validated$usage,
    model = validated$model,
    error = error,
    backend = "mirai",
    batch_index = index,
    cache = "bypass"
  )

  if (!is.null(error)) {
    return(create_error_result(
      error = error,
      index = index,
      prompt = request$prompt,
      instructions = request$instructions,
      llm = NULL,
      .return_format = .return_format,
      inputs = input_set,
      request = request,
      turns_before = validated$turns_before,
      metadata = metadata,
      module = module,
      .capture_trace = TRUE
    ))
  }

  completed_chat <- mock_batch_chat(
    request$payload,
    record$response,
    chat,
    turns_before = record$turns_before
  )
  trace <- canonical_run_trace(
    inputs = input_set,
    response = record$response,
    request = request,
    chat = completed_chat,
    turns_before = record$turns_before,
    metadata = metadata,
    store_chat = isTRUE(module$config$store_chat_in_traces)
  )
  result <- if (.return_format == "simple") {
    record$response
  } else {
    list(output = record$response, chat = completed_chat, metadata = metadata)
  }
  attach_run_trace(result, trace)
}

#' Execute one mirai row in an isolated worker
#' @noRd
mirai_batch_worker <- function(
  i,
  requests,
  output_type,
  llm_template
) {
  started_at <- Sys.time()
  worker_llm <- tryCatch(
    unserialize(serialize(llm_template, connection = NULL, version = 2)),
    error = function(e) e
  )
  if (inherits(worker_llm, "condition")) {
    return(list(
      ok = FALSE,
      error_message = conditionMessage(worker_llm),
      error_class = class(worker_llm)[1],
      error_kind = "serialization",
      started_at = started_at,
      ended_at = Sys.time(),
      usage = list(),
      model = NA_character_,
      turns_before = NULL
    ))
  }

  get_turns <- tryCatch(worker_llm$get_turns, error = function(e) NULL)
  turns_before <- if (is.function(get_turns)) {
    tryCatch(get_turns(), error = function(e) NULL)
  } else {
    NULL
  }
  response <- tryCatch(
    worker_llm$chat_structured(
      requests[[i]]$payload,
      type = output_type,
      echo = "none"
    ),
    error = function(e) e
  )
  ended_at <- Sys.time()
  if (inherits(response, "condition")) {
    return(list(
      ok = FALSE,
      error_message = conditionMessage(response),
      error_class = class(response)[1],
      error_kind = "provider",
      started_at = started_at,
      ended_at = ended_at,
      usage = list(),
      model = NA_character_,
      turns_before = turns_before
    ))
  }

  turns_after <- if (is.function(get_turns)) {
    tryCatch(get_turns(), error = function(e) NULL)
  } else {
    NULL
  }
  before_n <- length(turns_before)
  usage_verified <- !is.null(turns_before) &&
    !is.null(turns_after) &&
    length(turns_after) > before_n &&
    (before_n == 0L ||
      identical(
        turns_after[seq_len(before_n)],
        turns_before
      ))
  turn_delta <- if (usage_verified) {
    turns_after[seq.int(before_n + 1L, length(turns_after))]
  } else {
    list()
  }
  assistant_turns <- Filter(
    function(turn) inherits(turn, "ellmer::AssistantTurn"),
    turn_delta
  )
  assistant_turn <- if (length(assistant_turns) > 0L) {
    assistant_turns[[length(assistant_turns)]]
  } else {
    NULL
  }
  tokens <- tryCatch(assistant_turn@tokens, error = function(e) NULL)
  input_tokens <- as.integer(
    if (length(tokens) >= 1L) {
      tokens[[1]]
    } else {
      NA_integer_
    }
  )
  output_tokens <- as.integer(
    if (length(tokens) >= 2L) {
      tokens[[2]]
    } else {
      NA_integer_
    }
  )
  cached_input_tokens <- as.integer(
    if (length(tokens) >= 3L) {
      tokens[[3]]
    } else {
      NA_integer_
    }
  )
  usage <- list(
    input_tokens = input_tokens,
    output_tokens = output_tokens,
    cached_input_tokens = cached_input_tokens,
    total_tokens = if (anyNA(c(input_tokens, output_tokens))) {
      NA_integer_
    } else {
      input_tokens + output_tokens
    },
    cost = tryCatch(assistant_turn@cost, error = function(e) NA_real_),
    duration_s = tryCatch(
      assistant_turn@duration,
      error = function(e) NA_real_
    )
  )
  list(
    ok = TRUE,
    response = response,
    started_at = started_at,
    ended_at = ended_at,
    usage = usage,
    usage_verified = usage_verified,
    model = tryCatch(
      worker_llm$get_model(),
      error = function(e) NA_character_
    ),
    turns_before = turns_before
  )
}

#' Schedule one planned batch using mirai
#' @noRd
schedule_mirai_batch <- function(
  plan,
  .return_format,
  .progress
) {
  module <- plan$module
  input_sets <- plan$input_sets
  n <- plan$n
  requests <- plan$requests
  llm_template <- plan$llm_template
  baseline_turns <- plan$baseline_turns
  batch_started_at <- plan$batch_started_at
  deadline <- plan$deadline
  compute_profile <- plan$compute_profile
  .concurrency <- plan$concurrency
  tasks <- vector("list", n)
  profile_owned <- FALSE
  tryCatch(
    {
      mirai::daemons(
        n = .concurrency$requested_workers,
        dispatcher = TRUE,
        .compute = compute_profile
      )
      profile_owned <- TRUE
    },
    error = function(error) {
      cli::cli_abort(
        c(
          "Could not create the dsprrr mirai worker pool",
          "x" = "{conditionMessage(error)}"
        ),
        class = "dsprrr_concurrency_backend_error",
        parent = error
      )
    }
  )
  shutdown_owned <- function(strict = TRUE) {
    if (!profile_owned) {
      return(invisible(TRUE))
    }
    stopped <- shutdown_dsprrr_mirai_profile(
      profile = compute_profile,
      tasks = tasks,
      strict = strict,
      deadline = if (is.finite(deadline)) deadline else NULL
    )
    if (isTRUE(stopped)) {
      profile_owned <<- FALSE
    }
    invisible(stopped)
  }
  on.exit(
    {
      if (profile_owned) {
        cleaned <- tryCatch(
          shutdown_owned(strict = FALSE),
          error = function(error) FALSE
        )
        if (!isTRUE(cleaned)) {
          warn_mirai_teardown_failure(compute_profile)
        }
      }
    },
    add = TRUE
  )

  results <- vector("list", n)
  active <- integer()
  next_index <- 1L
  completed <- 0L
  error_count <- 0L
  stop_scheduling <- FALSE
  warnings_to_emit <- character()
  task_timeout_count <- 0L
  task_started_at <- rep(list(NULL), n)
  task_started_elapsed <- rep(NA_real_, n)

  progress_id <- NULL
  if (.progress && n > 1L) {
    progress_id <- cli::cli_progress_bar(
      format = "Processing {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = n,
      clear = FALSE
    )
    on.exit(
      try(cli::cli_progress_done(id = progress_id), silent = TRUE),
      add = TRUE
    )
  }

  update_progress <- function() {
    if (!is.null(progress_id)) {
      cli::cli_progress_update(id = progress_id)
    }
  }

  task_timeout_ms <- if (is.finite(.concurrency$task_timeout)) {
    max(1, ceiling(.concurrency$task_timeout * 1000))
  } else {
    NULL
  }
  launch_one <- function(index) {
    task_started_at[[index]] <<- Sys.time()
    task_started_elapsed[[index]] <<- concurrency_elapsed()
    output_type <- module$signature@output_type
    worker <- mirai_batch_worker
    row_requests <- list(requests[[index]])
    tasks[[index]] <<- mirai::mirai(
      .expr = worker(1L, requests, output_type, llm_template),
      .args = list(
        worker = worker,
        requests = row_requests,
        output_type = output_type,
        llm_template = llm_template
      ),
      .timeout = task_timeout_ms,
      .compute = compute_profile
    )
    active <<- c(active, index)
  }

  launch_available <- function() {
    while (
      !stop_scheduling &&
        concurrency_elapsed() < deadline &&
        next_index <= n &&
        length(active) < .concurrency$effective_workers
    ) {
      launch_one(next_index)
      next_index <<- next_index + 1L
    }
  }

  fill_concurrency_rows <- function(
    indices,
    message,
    classes,
    reason
  ) {
    for (index in indices) {
      if (!is.null(results[[index]])) {
        next
      }
      results[[index]] <<- create_concurrency_error_result(
        module = module,
        input_set = input_sets[[index]],
        index = index,
        llm = NULL,
        .return_format = .return_format,
        runtime = .concurrency,
        message = message(index),
        classes = classes,
        cancellation_reason = reason,
        started_at = batch_started_at
      )
      completed <<- completed + 1L
      update_progress()
    }
  }

  stop_active <- function(indices) {
    for (index in indices) {
      if (!is.null(tasks[[index]]) && mirai::unresolved(tasks[[index]])) {
        try(mirai::stop_mirai(tasks[[index]]), silent = TRUE)
      }
    }
    # Stopping the owned profile is the hard synchronization boundary: after
    # this returns, no dsprrr worker from this batch can continue executing.
    shutdown_owned(strict = TRUE)
  }

  launch_available()

  while (completed < n) {
    if (concurrency_elapsed() >= deadline) {
      unfinished <- which(vapply(results, is.null, logical(1)))
      active_at_timeout <- active
      stop_scheduling <- TRUE
      stop_active(active_at_timeout)
      active <- integer()
      fill_concurrency_rows(
        unfinished,
        message = function(index) {
          paste0(
            "Item ",
            index,
            " did not finish before the batch total timeout of ",
            .concurrency$total_timeout,
            " seconds"
          )
        },
        classes = c(
          "dsprrr_mirai_timeout_error",
          "dsprrr_concurrency_total_timeout_error",
          "dsprrr_concurrency_error"
        ),
        reason = "total_timeout"
      )
      cli::cli_warn(c(
        "Parallel processing timed out after {(.concurrency$total_timeout)} seconds",
        "x" = "{length(unfinished)} of {n} tasks did not complete."
      ))
      break
    }

    resolved <- active[
      !vapply(
        tasks[active],
        mirai::unresolved,
        logical(1)
      )
    ]
    if (length(resolved) == 0L) {
      Sys.sleep(0.005)
      next
    }

    for (index in resolved) {
      record <- tasks[[index]][["data"]]
      result <- if (is_mirai_timeout_record(record)) {
        # Mirai's timeout record is resolved only after dispatcher
        # cancellation. Issue an explicit stop as a defensive barrier before
        # releasing this worker slot or scheduling replacement work.
        try(mirai::stop_mirai(tasks[[index]]), silent = TRUE)
        stop_deadline <- concurrency_elapsed() + 1
        stopped <- tryCatch(
          !mirai::unresolved(tasks[[index]]),
          error = function(error) FALSE
        )
        while (!stopped && concurrency_elapsed() < stop_deadline) {
          Sys.sleep(0.001)
          stopped <- tryCatch(
            !mirai::unresolved(tasks[[index]]),
            error = function(error) FALSE
          )
        }
        if (!stopped) {
          shutdown_owned(strict = TRUE)
          cli::cli_abort(
            c(
              "Timed-out mirai task did not stop cleanly",
              "x" = "The dsprrr-owned worker pool was shut down before returning."
            ),
            class = c(
              "dsprrr_mirai_task_stop_error",
              "dsprrr_concurrency_error"
            )
          )
        }
        task_timeout_count <- task_timeout_count + 1L
        timeout_result <- create_concurrency_error_result(
          module = module,
          input_set = input_sets[[index]],
          index = index,
          llm = NULL,
          .return_format = .return_format,
          runtime = .concurrency,
          message = paste0(
            "Task timed out after ",
            .concurrency$task_timeout,
            " seconds"
          ),
          classes = c(
            "dsprrr_mirai_timeout_error",
            "dsprrr_concurrency_task_timeout_error",
            "dsprrr_mirai_worker_error"
          ),
          cancellation_reason = "task_timeout",
          started_at = task_started_at[[index]]
        )
        set_concurrency_result_latency(
          timeout_result,
          latency_ms = (concurrency_elapsed() -
            task_started_elapsed[[index]]) *
            1000,
          .return_format = .return_format
        )
      } else {
        annotate_concurrency_result(
          mirai_worker_result(
            record = record,
            module = module,
            input_set = input_sets[[index]],
            request = requests[[index]],
            chat = llm_template,
            index = index,
            .return_format = .return_format,
            fallback_started_at = batch_started_at,
            fallback_ended_at = Sys.time()
          ),
          .concurrency,
          .return_format
        )
      }

      if (
        identical(.return_format, "simple") &&
          length(result) == 1L &&
          is.na(result) &&
          !is.null(attr(result, "error_message", exact = TRUE))
      ) {
        warnings_to_emit <- c(
          warnings_to_emit,
          paste0(
            "Failed to process item ",
            index,
            ": ",
            attr(result, "error_message", exact = TRUE)
          )
        )
      }

      results[[index]] <- result
      completed <- completed + 1L
      if (concurrency_row_failed(result, .return_format)) {
        error_count <- error_count + 1L
      }
      update_progress()
    }
    active <- setdiff(active, resolved)

    if (
      !stop_scheduling &&
        concurrency_error_budget_reached(
          error_count,
          .concurrency$max_errors
        )
    ) {
      stop_scheduling <- TRUE
      queued <- if (next_index <= n) seq.int(next_index, n) else integer()
      if (isTRUE(.concurrency$cancel)) {
        cancelled <- c(active, queued)
        stop_active(active)
        active <- integer()
      } else {
        cancelled <- queued
      }
      next_index <- n + 1L
      fill_concurrency_rows(
        cancelled,
        message = function(index) {
          paste0(
            "Item ",
            index,
            " was cancelled because the batch error budget was reached"
          )
        },
        classes = c(
          "dsprrr_concurrency_cancelled_error",
          "dsprrr_concurrency_error"
        ),
        reason = "max_errors"
      )
      cli::cli_warn(c(
        "Batch error budget reached during mirai execution",
        "x" = "Items cancelled or not started: {length(cancelled)}."
      ))
    }

    launch_available()
  }

  if (task_timeout_count > 0L) {
    cli::cli_warn(
      "{task_timeout_count} mirai task{?s} timed out after {(.concurrency$task_timeout)} seconds"
    )
  }

  if (profile_owned) {
    shutdown_owned(strict = TRUE)
  }

  # Emit accumulated warnings
  for (warning_msg in warnings_to_emit) {
    cli::cli_warn("{warning_msg}")
  }

  results
}

#' Build the execution plan before starting a mirai worker pool
#' @noRd
plan_mirai_batch <- function(module, input_sets, n, .llm, .concurrency) {
  requests <- lapply(input_sets, function(input_set) {
    build_module_request(module, input_set)
  })
  llm_template <- resolve_module_llm(module, .llm = .llm)
  baseline_turns <- batch_chat_turns(llm_template)
  batch_started_at <- Sys.time()
  batch_started_elapsed <- concurrency_elapsed()
  deadline <- if (is.finite(.concurrency$total_timeout)) {
    batch_started_elapsed + .concurrency$total_timeout
  } else {
    Inf
  }
  compute_profile <- new_dsprrr_mirai_profile()
  if (!mirai_profile_is_unoccupied(compute_profile)) {
    cli::cli_abort(
      c(
        "Refusing to replace an occupied mirai compute profile",
        "x" = "Profile {.val {compute_profile}} is already owned by another caller."
      ),
      class = "dsprrr_mirai_profile_collision",
      profile = compute_profile,
      call = rlang::caller_env()
    )
  }
  list(
    module = module,
    input_sets = input_sets,
    n = n,
    requests = requests,
    llm_template = llm_template,
    baseline_turns = baseline_turns,
    batch_started_at = batch_started_at,
    deadline = deadline,
    compute_profile = compute_profile,
    concurrency = .concurrency
  )
}

#' Run a planned batch through mirai and finalize private row traces
#' @noRd
run_batch_parallel <- function(
  module,
  input_sets,
  n,
  .llm,
  .verbose,
  .return_format,
  .progress,
  .cache = NULL,
  .concurrency
) {
  plan <- plan_mirai_batch(
    module = module,
    input_sets = input_sets,
    n = n,
    .llm = .llm,
    .concurrency = .concurrency
  )
  results <- schedule_mirai_batch(
    plan = plan,
    .return_format = .return_format,
    .progress = .progress
  )
  collect_backend_traces(results)
}

# Factory-backed interpreter batch execution

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
  llm <- assert_ellmer_chat(llm, arg = ".llm")

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
        interrupt = function(condition) resignal_condition(condition),
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
        interrupt = function(condition) resignal_condition(condition),
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
