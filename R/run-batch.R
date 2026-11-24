#' Process batch inputs
#' @noRd
run_batch <- function(
  module,
  inputs,
  n,
  .llm,
  .verbose,
  .progress,
  .return_format,
  .cache = NULL,
  .concurrency
) {
  if (n == 0L) {
    return(empty_batch_result(.return_format))
  }

  runtime <- ellmer_parallel_schema_runtime(module, .concurrency)
  if (!identical(runtime$effective_backend, "sequential")) {
    assert_decisions_supported(
      module,
      paste0("the ", runtime$effective_backend, " batch backend")
    )
  }

  plan <- plan_batch_execution(inputs, n)
  results <- execute_batch_plan(
    module = module,
    plan = plan,
    .llm = .llm,
    .verbose = .verbose,
    .progress = .progress,
    .return_format = .return_format,
    .cache = .cache,
    runtime = runtime
  )
  finalize_batch_execution(module, results, .return_format)
}

#' Build the immutable work plan shared by every batch backend
#' @noRd
plan_batch_execution <- function(inputs, n) {
  list(
    input_sets = lapply(seq_len(n), function(i) lapply(inputs, `[[`, i)),
    n = n
  )
}

#' Execute a planned batch using the selected backend
#' @noRd
execute_batch_plan <- function(
  module,
  plan,
  .llm,
  .verbose,
  .progress,
  .return_format,
  .cache,
  runtime
) {
  # The backend is fully normalized before any Chat or topology is resolved.
  switch(
    runtime$effective_backend,
    sequential = run_batch_sequential(
      module,
      plan$input_sets,
      plan$n,
      .llm,
      .verbose,
      .return_format,
      .progress,
      .cache,
      runtime
    ),
    ellmer = run_batch_ellmer_parallel(
      module,
      plan$input_sets,
      plan$n,
      .llm,
      .verbose,
      .return_format,
      .progress,
      .cache,
      runtime
    ),
    mirai = run_batch_parallel(
      module,
      plan$input_sets,
      plan$n,
      .llm,
      .verbose,
      .return_format,
      .progress,
      .cache,
      runtime
    ),
    cli::cli_abort(
      "Unknown batch backend {.val {runtime$effective_backend}}",
      class = "dsprrr_concurrency_backend_error"
    )
  )
}

#' Commit traces and normalize the public batch result
#' @noRd
finalize_batch_execution <- function(module, results, .return_format) {
  traces <- attr(results, "dsprrr_traces", exact = TRUE)
  commit_run_traces(module, traces)
  results <- strip_backend_traces(results)

  if (identical(.return_format, "structured")) {
    structure(results, class = c("dsprrr_batch_result", "list"))
  } else {
    results
  }
}

#' Run batch processing sequentially
#' @noRd
run_batch_sequential <- function(
  module,
  input_sets,
  n,
  .llm,
  .verbose,
  .return_format,
  .progress,
  .cache = NULL,
  .concurrency = NULL
) {
  if (is.null(.concurrency)) {
    .concurrency <- normalize_concurrency_runtime(
      concurrency_control(backend = "sequential")
    )
  }
  plan <- plan_sequential_batch(
    module = module,
    n = n,
    .llm = .llm,
    concurrency = .concurrency
  )
  results <- evaluate_sequential_batch(
    module = module,
    input_sets = input_sets,
    plan = plan,
    .verbose = .verbose,
    .return_format = .return_format,
    .progress = .progress,
    .cache = .cache
  )
  collect_backend_traces(results)
}

#' Resolve row Chats before sequential batch evaluation
#' @noRd
plan_sequential_batch <- function(
  module,
  n,
  .llm,
  concurrency
) {
  baseline_llm <- resolve_module_llm(module, .llm = .llm)
  list(
    row_llms = batch_chat_branches(baseline_llm, n),
    n = n,
    concurrency = concurrency
  )
}

#' Evaluate planned rows while preserving sequential batch contracts
#' @noRd
evaluate_sequential_batch <- function(
  module,
  input_sets,
  plan,
  .verbose,
  .return_format,
  .progress,
  .cache
) {
  n <- plan$n
  .concurrency <- plan$concurrency
  row_llms <- plan$row_llms
  results <- vector("list", n)

  progress_id <- NULL
  if (.progress && n > 1) {
    progress_id <- cli::cli_progress_bar(
      format = "Processing {cli::pb_current}/{cli::pb_total} | {cli::pb_percent} | ETA: {cli::pb_eta}",
      total = n,
      clear = FALSE
    )
  }

  error_count <- 0L
  for (i in seq_len(n)) {
    request <- build_module_request(module, input_sets[[i]])
    row_llm <- row_llms[[i]]
    results[[i]] <- tryCatch(
      {
        process_batch_item(
          input_set = input_sets[[i]],
          module = module,
          llm = row_llm,
          index = i,
          .verbose = .verbose,
          .return_format = .return_format,
          .cache = .cache,
          backend = "sequential",
          .capture_trace = TRUE
        )
      },
      error = function(e) {
        create_error_result(
          error = e,
          index = i,
          prompt = request$prompt,
          instructions = request$instructions,
          llm = row_llm,
          .return_format = .return_format,
          inputs = input_sets[[i]],
          request = request,
          turns_before = batch_chat_turns(row_llm),
          module = module,
          .capture_trace = TRUE
        )
      }
    )
    results[[i]] <- annotate_concurrency_result(
      results[[i]],
      .concurrency,
      .return_format
    )

    error_message <- if (.return_format == "simple") {
      attr(results[[i]], "error_message", exact = TRUE)
    } else {
      value <- results[[i]]$metadata$error
      if (run_error_present(value)) value else NULL
    }
    if (!is.null(error_message)) {
      cli::cli_warn("Failed to process item {i}: {error_message}")
    }
    if (concurrency_row_failed(results[[i]], .return_format)) {
      error_count <- error_count + 1L
    }

    if (!is.null(progress_id)) {
      cli::cli_progress_update(id = progress_id)
    }

    if (
      concurrency_error_budget_reached(
        error_count,
        .concurrency$max_errors
      ) &&
        i < n
    ) {
      queued <- seq.int(i + 1L, n)
      for (j in queued) {
        results[[j]] <- create_concurrency_error_result(
          module = module,
          input_set = input_sets[[j]],
          index = j,
          llm = row_llms[[j]],
          .return_format = .return_format,
          runtime = .concurrency,
          message = paste0(
            "Item ",
            j,
            " was not started because the batch error budget was reached"
          ),
          classes = c(
            "dsprrr_concurrency_cancelled_error",
            "dsprrr_concurrency_error"
          ),
          cancellation_reason = "max_errors"
        )
        if (!is.null(progress_id)) {
          cli::cli_progress_update(id = progress_id)
        }
      }
      cli::cli_warn(c(
        "Batch error budget reached after item {i}",
        "x" = "{length(queued)} queued item{?s} were not started."
      ))
      break
    }
  }

  if (!is.null(progress_id)) {
    cli::cli_progress_done(id = progress_id)
  }
  results
}


#' Read and validate one current Chat history
#' @noRd
batch_chat_history <- function(chat, stage = "batch execution") {
  chat <- assert_ellmer_chat(chat, arg = "chat")
  getter <- tryCatch(chat[["get_turns"]], error = function(e) NULL)
  if (!is.function(getter)) {
    cli::cli_abort(
      "Cannot inspect the Chat history for {stage}",
      class = "dsprrr_chat_isolation_error"
    )
  }

  turns <- tryCatch(
    getter(),
    error = function(e) {
      cli::cli_abort(
        "Cannot inspect the Chat history for {stage}",
        class = "dsprrr_chat_isolation_error",
        parent = e
      )
    }
  )
  if (!is.list(turns)) {
    cli::cli_abort(
      "The Chat history for {stage} must be a list",
      class = "dsprrr_chat_isolation_error"
    )
  }
  turns
}

#' Create one independent current Chat while preserving its history
#' @noRd
batch_chat_copy <- function(chat, stage) {
  starting_history <- batch_chat_history(chat, stage)
  branch <- clone_ellmer_chat(
    chat,
    arg = "chat",
    reset_turns = FALSE
  )
  if (!identical(batch_chat_history(branch, stage), starting_history)) {
    cli::cli_abort(
      "The cloned Chat did not preserve the exact history for {stage}",
      class = c("dsprrr_chat_clone_error", "dsprrr_chat_isolation_error")
    )
  }
  branch
}

#' Create independent current Chat branches for batch rows
#' @noRd
batch_chat_branches <- function(chat, n) {
  if (n == 0L) {
    return(list())
  }

  chat <- assert_ellmer_chat(chat, arg = "chat")
  branches <- lapply(seq_len(n), function(i) {
    batch_chat_copy(chat, paste0("row ", i))
  })
  branch_ids <- vapply(branches, rlang::obj_address, character(1))
  if (anyDuplicated(branch_ids) > 0L) {
    cli::cli_abort(
      "Chat cloning returned the same object for multiple batch rows",
      class = c("dsprrr_chat_clone_error", "dsprrr_chat_isolation_error")
    )
  }
  branches
}
