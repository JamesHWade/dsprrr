#' Reconstruct one scalar value from ellmer's vectorized batch representation
#' @noRd
ellmer_parallel_scalar_value <- function(value, type) {
  if (is.null(value)) {
    return(NULL)
  }

  if (inherits(type, "ellmer::TypeObject")) {
    if (is.data.frame(value)) {
      if (nrow(value) != 1L) {
        cli::cli_abort(
          "Nested ellmer object output did not contain exactly one row",
          class = "dsprrr_parallel_response_error"
        )
      }
      return(ellmer_parallel_response_row(value, 1L, type))
    }
    if (!is.list(value) || is.null(names(value))) {
      cli::cli_abort(
        "Nested ellmer object output is not a named record",
        class = "dsprrr_parallel_response_error"
      )
    }

    properties <- type@properties
    result <- lapply(names(properties), function(name) {
      if (!name %in% names(value)) {
        if (!isTRUE(properties[[name]]@required)) {
          return(NULL)
        }
        cli::cli_abort(
          "ellmer output is missing required field {.field {name}}",
          class = "dsprrr_parallel_response_error"
        )
      }
      ellmer_parallel_scalar_value(value[[name]], properties[[name]])
    })
    names(result) <- names(properties)
    if (isTRUE(type@additional_properties)) {
      extras <- setdiff(names(value), names(properties))
      result <- c(result, value[extras])
    }
    return(result)
  }

  if (inherits(type, "ellmer::TypeBasic")) {
    if (is.list(value) && length(value) == 1L) {
      value <- value[[1L]]
    }
    if (
      !isTRUE(type@required) &&
        length(value) == 1L &&
        is.atomic(value) &&
        is.na(value)
    ) {
      return(NULL)
    }
    return(switch(
      type@type,
      boolean = as.logical(value),
      integer = as.integer(value),
      number = as.numeric(value),
      string = as.character(value),
      value
    ))
  }

  if (inherits(type, "ellmer::TypeEnum")) {
    if (!isTRUE(type@required) && length(value) == 1L && is.na(value)) {
      return(NULL)
    }
    return(as.character(value))
  }

  if (inherits(type, "ellmer::TypeIgnore")) {
    return(NULL)
  }

  # TypeArray and TypeJsonSchema values are already converted per row by
  # ellmer. In particular, arrays of objects are tibbles and must not be
  # collapsed to a one-row list column.
  value
}

#' Probe whether a required nested property carries evidence of presence
#'
#' Ellmer vectorizes nested objects into nested tibbles. When an optional
#' object is absent, required scalar fields in that row become typed `NA`s.
#' This helper returns `TRUE` for that missing sentinel, `FALSE` for evidence
#' that the property is present, and `NA` when the converted representation is
#' inherently ambiguous (for example, a required array may legitimately be
#' empty).
#' @noRd
ellmer_parallel_required_property_missing <- function(column, index, type) {
  if (inherits(type, c("ellmer::TypeBasic", "ellmer::TypeEnum"))) {
    if (index > length(column)) {
      return(NA)
    }
    value <- column[[index]]
    return(
      is.null(value) ||
        (length(value) == 1L &&
          is.atomic(value) &&
          is.na(value))
    )
  }

  if (inherits(type, "ellmer::TypeObject")) {
    if (is.data.frame(column)) {
      return(ellmer_parallel_object_row_missing(column, index, type))
    }
    if (index > length(column)) {
      return(NA)
    }
    return(is.null(column[[index]]))
  }

  # Arrays and arbitrary JSON values do not have an unambiguous converted
  # sentinel: an empty value can be a valid, present value.
  NA
}

#' Detect an absent optional object in ellmer's nested-tibble representation
#'
#' Only required properties with an unambiguous missing sentinel participate.
#' If an object has no such property, preserve it as present. This deliberately
#' avoids collapsing a genuinely present object whose optional fields are all
#' missing.
#' @noRd
ellmer_parallel_object_row_missing <- function(responses, index, type) {
  if (index > nrow(responses)) {
    return(NA)
  }

  properties <- type@properties
  required <- names(properties)[vapply(
    properties,
    function(property) isTRUE(property@required),
    logical(1)
  )]
  if (length(required) == 0L) {
    return(NA)
  }

  probes <- vapply(
    required,
    function(name) {
      if (!name %in% names(responses)) {
        return(TRUE)
      }
      ellmer_parallel_required_property_missing(
        responses[[name]],
        index,
        properties[[name]]
      )
    },
    logical(1)
  )
  probes <- probes[!is.na(probes)]
  if (length(probes) == 0L) {
    return(NA)
  }
  all(probes)
}

#' Extract one row from a vectorized ellmer output column
#' @noRd
ellmer_parallel_column_value <- function(column, index, type) {
  if (inherits(type, "ellmer::TypeObject") && is.data.frame(column)) {
    if (
      !isTRUE(type@required) &&
        isTRUE(ellmer_parallel_object_row_missing(column, index, type))
    ) {
      return(NULL)
    }
    return(ellmer_parallel_response_row(column, index, type))
  }
  if (index > length(column)) {
    cli::cli_abort(
      "ellmer output column ended before row {index}",
      class = "dsprrr_parallel_response_error"
    )
  }
  ellmer_parallel_scalar_value(column[[index]], type)
}

#' Reconstruct one scalar response from ellmer's typed batch output
#' @noRd
ellmer_parallel_response_row <- function(responses, index, output_type) {
  if (!inherits(output_type, "ellmer::TypeObject")) {
    if (index > length(responses)) {
      cli::cli_abort(
        "ellmer output ended before row {index}",
        class = "dsprrr_parallel_response_error"
      )
    }
    return(ellmer_parallel_scalar_value(responses[[index]], output_type))
  }

  if (!is.data.frame(responses)) {
    if (!is.list(responses) || index > length(responses)) {
      cli::cli_abort(
        "ellmer object output is not a row-oriented record",
        class = "dsprrr_parallel_response_error"
      )
    }
    return(ellmer_parallel_scalar_value(responses[[index]], output_type))
  }
  if (index > nrow(responses)) {
    cli::cli_abort(
      "ellmer output ended before row {index}",
      class = "dsprrr_parallel_response_error"
    )
  }

  properties <- output_type@properties
  result <- lapply(names(properties), function(name) {
    if (!name %in% names(responses)) {
      if (!isTRUE(properties[[name]]@required)) {
        return(NULL)
      }
      cli::cli_abort(
        "ellmer output is missing required field {.field {name}}",
        class = "dsprrr_parallel_response_error"
      )
    }
    ellmer_parallel_column_value(
      responses[[name]],
      index,
      properties[[name]]
    )
  })
  names(result) <- names(properties)
  if (isTRUE(output_type@additional_properties)) {
    extras <- setdiff(names(responses), names(properties))
    extra_values <- lapply(extras, function(name) responses[[name]][[index]])
    names(extra_values) <- extras
    result <- c(result, extra_values)
  }
  result
}

#' Wrap non-object outputs so ellmer retains its per-row error column
#'
#' `parallel_chat_structured()` only appends `.error` when conversion produces
#' a data frame. A one-field object wrapper makes every non-object output use
#' that lossless row channel while preserving the public output after unwrapping.
#' @noRd
ellmer_parallel_native_output <- function(output_type) {
  if (inherits(output_type, "ellmer::TypeObject")) {
    return(list(type = output_type, wrapped = FALSE))
  }
  list(
    type = ellmer::type_object(value = output_type),
    wrapped = TRUE
  )
}

#' Reconstruct and unwrap one successful native ellmer response
#' @noRd
ellmer_parallel_native_response <- function(
  responses,
  index,
  native_output
) {
  response <- ellmer_parallel_response_row(
    responses,
    index,
    native_output$type
  )
  if (isTRUE(native_output$wrapped)) response$value else response
}

#' Run batch processing using ellmer's parallel_chat_structured
#'
#' Uses ellmer's native parallel processing for structured outputs.
#' This approach is more efficient as it handles parallel HTTP requests
#' internally without the overhead of spawning R processes.
#'
#' @noRd
run_batch_ellmer_parallel <- function(
  module,
  input_sets,
  n,
  .llm,
  .verbose,
  .return_format,
  .progress,
  .cache = NULL,
  .concurrency = NULL,
  .batch_indices = seq_len(n),
  .single_wave = FALSE
) {
  if (is.null(.concurrency)) {
    .concurrency <- normalize_concurrency_runtime(concurrency_control(
      backend = "ellmer",
      max_active = max(1L, n)
    ))
  }

  if (!.single_wave) {
    return(schedule_ellmer_waves(
      module = module,
      input_sets = input_sets,
      n = n,
      .llm = .llm,
      .verbose = .verbose,
      .return_format = .return_format,
      .progress = .progress,
      .cache = .cache,
      .concurrency = .concurrency
    ))
  }

  plan <- plan_ellmer_wave(
    module = module,
    input_sets = input_sets,
    n = n,
    .llm = .llm,
    .concurrency = .concurrency,
    .batch_indices = .batch_indices
  )
  call <- call_ellmer_wave(plan)
  decoded <- decode_ellmer_wave(plan, call)
  results <- evaluate_ellmer_wave(
    plan = plan,
    call = call,
    decoded = decoded,
    .return_format = .return_format
  )
  collect_backend_traces(results)
}

#' Schedule bounded waves for native ellmer execution
#' @noRd
schedule_ellmer_waves <- function(
  module,
  input_sets,
  n,
  .llm,
  .verbose,
  .return_format,
  .progress,
  .cache,
  .concurrency
) {
  if (!concurrency_backend_available("ellmer")) {
    cli::cli_abort(
      "Requested concurrency backend {.val ellmer} is unavailable",
      class = "dsprrr_concurrency_backend_unavailable"
    )
  }

  progress_started <- isTRUE(.progress) && n > 1L
  if (progress_started) {
    cli::cli_progress_step(
      "Processing {n} items with ellmer (max active: {(.concurrency$requested_workers)})...",
      spinner = TRUE
    )
    on.exit(try(cli::cli_progress_done(), silent = TRUE), add = TRUE)
  }

  results <- vector("list", n)
  traces <- vector("list", n)
  conditions <- vector("list", n)
  error_count <- 0L
  wave_id <- ceiling(seq_len(n) / .concurrency$effective_workers)
  waves <- split(seq_len(n), wave_id)

  for (indices in waves) {
    wave <- run_batch_ellmer_parallel(
      module = module,
      input_sets = input_sets[indices],
      n = length(indices),
      .llm = .llm,
      .verbose = .verbose,
      .return_format = .return_format,
      .progress = FALSE,
      .cache = .cache,
      .concurrency = .concurrency,
      .batch_indices = indices,
      .single_wave = TRUE
    )
    results[indices] <- wave[seq_along(indices)]
    traces[indices] <- attr(wave, "dsprrr_traces", exact = TRUE)
    wave_conditions <- attr(
      wave,
      "dsprrr_error_conditions",
      exact = TRUE
    )
    conditions[indices] <- wave_conditions
    error_count <- error_count +
      sum(
        !vapply(
          wave_conditions,
          is.null,
          logical(1)
        )
      )

    if (
      concurrency_error_budget_reached(
        error_count,
        .concurrency$max_errors
      ) &&
        max(indices) < n
    ) {
      queued <- seq.int(max(indices) + 1L, n)
      for (index in queued) {
        cancelled <- create_concurrency_error_result(
          module = module,
          input_set = input_sets[[index]],
          index = index,
          llm = .llm %||% module$chat,
          .return_format = .return_format,
          runtime = .concurrency,
          message = paste0(
            "Item ",
            index,
            " was not started because the batch error budget was reached"
          ),
          classes = c(
            "dsprrr_concurrency_cancelled_error",
            "dsprrr_concurrency_error"
          ),
          cancellation_reason = "max_errors"
        )
        traces[[index]] <- attr(
          cancelled,
          "dsprrr_trace",
          exact = TRUE
        )
        conditions[[index]] <- attr(
          cancelled,
          "dsprrr_error_condition",
          exact = TRUE
        )
        results[[index]] <- strip_run_trace(cancelled)
      }
      cli::cli_warn(c(
        "Batch error budget reached after an ellmer wave",
        "x" = "Queued items not started: {length(queued)}."
      ))
      break
    }
  }

  attr(results, "dsprrr_traces") <- traces
  attr(results, "dsprrr_error_conditions") <- conditions
  return(results)
}

#' Plan one ellmer wave without invoking the provider
#' @noRd
plan_ellmer_wave <- function(
  module,
  input_sets,
  n,
  .llm,
  .concurrency,
  .batch_indices
) {
  # Build prompts for all inputs (as list, required by ellmer::parallel_chat_structured)
  requests <- lapply(input_sets, function(input_set) {
    build_module_request(module, input_set)
  })
  prompts <- lapply(requests, `[[`, "payload")

  caller_chat <- resolve_module_llm(module, .llm = .llm)

  baseline_turns <- batch_chat_turns(caller_chat)
  chat <- batch_chat_copy(caller_chat, "ellmer parallel execution")

  start_time <- Sys.time()

  output_type <- module$signature@output_type
  native_output <- ellmer_parallel_native_output(output_type)
  output_fields <- output_field_names(output_type)
  token_fields <- c("input_tokens", "output_tokens", "cached_input_tokens")
  include_tokens <- !any(token_fields %in% output_fields)
  include_cost <- !"cost" %in% output_fields
  list(
    module = module,
    input_sets = input_sets,
    n = n,
    concurrency = .concurrency,
    batch_indices = .batch_indices,
    requests = requests,
    prompts = prompts,
    caller_chat = caller_chat,
    baseline_turns = baseline_turns,
    chat = chat,
    start_time = start_time,
    output_type = output_type,
    native_output = native_output,
    output_fields = output_fields,
    token_fields = token_fields,
    include_tokens = include_tokens,
    include_cost = include_cost
  )
}

#' Invoke the native ellmer provider for one planned wave
#' @noRd
call_ellmer_wave <- function(plan) {
  chat <- plan$chat
  prompts <- plan$prompts
  native_output <- plan$native_output
  include_tokens <- plan$include_tokens
  include_cost <- plan$include_cost
  .concurrency <- plan$concurrency
  start_time <- plan$start_time
  # Call ellmer's parallel_chat_structured
  responses <- tryCatch(
    {
      ellmer::parallel_chat_structured(
        chat = chat,
        prompts = prompts,
        type = native_output$type,
        include_tokens = include_tokens,
        include_cost = include_cost,
        max_active = .concurrency$requested_workers,
        on_error = "continue"
      )
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "Parallel LLM call failed",
          "x" = e$message,
          "i" = "Try {.code .concurrency = concurrency_control(backend = \"sequential\")}"
        ),
        parent = e
      )
    }
  )

  end_time <- Sys.time()
  total_latency <- as.numeric(difftime(end_time, start_time, units = "secs")) *
    1000
  list(
    responses = responses,
    end_time = end_time,
    total_latency = total_latency
  )
}

#' Decode and validate native ellmer responses
#' @noRd
decode_ellmer_wave <- function(plan, call) {
  responses <- call$responses
  n <- plan$n
  native_output <- plan$native_output
  include_tokens <- plan$include_tokens
  include_cost <- plan$include_cost
  token_fields <- plan$token_fields
  # Validate response format

  if (is.null(responses)) {
    cli::cli_abort(c(
      "parallel_chat_structured() returned NULL",
      "i" = "This may indicate an API failure or empty response"
    ))
  }

  # Normalize responses to list-of-lists format
  # parallel_chat_structured returns a tibble where each row is a response
  response_errors <- vector("list", n)
  response_usage <- replicate(n, list(), simplify = FALSE)

  if (is.data.frame(responses)) {
    if (nrow(responses) != n) {
      cli::cli_abort(c(
        "Response count mismatch from parallel_chat_structured()",
        "x" = "Expected {n} responses, got {nrow(responses)}",
        "i" = "Some requests may have failed silently"
      ))
    }
    if (".error" %in% names(responses)) {
      response_errors <- lapply(responses$.error, normalize_backend_error)
      responses$.error <- NULL
    }
    telemetry_fields <- c(
      if (include_tokens) token_fields else character(),
      if (include_cost) "cost" else character()
    )
    usage_fields <- intersect(telemetry_fields, names(responses))
    if (length(usage_fields) > 0) {
      response_usage <- lapply(seq_len(n), function(i) {
        usage <- as.list(responses[i, usage_fields, drop = FALSE])
        usage$total_tokens <- if (
          all(c("input_tokens", "output_tokens") %in% names(usage)) &&
            !anyNA(c(usage$input_tokens, usage$output_tokens))
        ) {
          as.integer(usage$input_tokens + usage$output_tokens)
        } else {
          NA_integer_
        }
        usage
      })
      responses[usage_fields] <- NULL
    }
    responses_list <- lapply(seq_len(nrow(responses)), function(i) {
      if (!is.null(response_errors[[i]])) {
        return(NULL)
      }
      ellmer_parallel_native_response(responses, i, native_output)
    })
  } else if (is.list(responses)) {
    if (length(responses) != n) {
      cli::cli_abort(c(
        "Response count mismatch from parallel_chat_structured()",
        "x" = "Expected {n} responses, got {length(responses)}",
        "i" = "Some requests may have failed silently"
      ))
    }
    response_errors <- lapply(responses, function(response) {
      if (inherits(response, "error")) {
        normalize_backend_error(response)
      } else {
        NULL
      }
    })
    responses_list <- lapply(seq_len(n), function(i) {
      if (!is.null(response_errors[[i]])) {
        return(NULL)
      }
      ellmer_parallel_native_response(responses, i, native_output)
    })
  } else if (is.atomic(responses) && length(responses) == n) {
    responses_list <- lapply(seq_len(n), function(i) {
      ellmer_parallel_native_response(responses, i, native_output)
    })
  } else {
    cli::cli_abort(c(
      "Unexpected response format from parallel_chat_structured()",
      "x" = "Got {.cls {class(responses)[1]}} instead of a typed batch value",
      "i" = "This may be a version mismatch with ellmer"
    ))
  }
  list(
    responses_list = responses_list,
    response_errors = response_errors,
    response_usage = response_usage
  )
}

#' Evaluate decoded ellmer rows and build deterministic traces
#' @noRd
evaluate_ellmer_wave <- function(plan, call, decoded, .return_format) {
  module <- plan$module
  input_sets <- plan$input_sets
  requests <- plan$requests
  prompts <- plan$prompts
  caller_chat <- plan$caller_chat
  baseline_turns <- plan$baseline_turns
  output_fields <- plan$output_fields
  token_fields <- plan$token_fields
  responses_list <- decoded$responses_list
  response_errors <- decoded$response_errors
  response_usage <- decoded$response_usage
  end_time <- call$end_time
  total_latency <- call$total_latency
  chat <- plan$chat
  .concurrency <- plan$concurrency
  .batch_indices <- plan$batch_indices
  model <- tryCatch(chat$get_model(), error = function(e) NA_character_)
  results <- purrr::map2(
    responses_list,
    seq_along(responses_list),
    function(response, i) {
      batch_index <- .batch_indices[[i]]
      error <- response_errors[[i]]
      row_started_at <- end_time - total_latency / 1000
      metadata <- canonical_run_metadata(
        request = requests[[i]],
        started_at = row_started_at,
        ended_at = end_time,
        usage = response_usage[[i]],
        model = model,
        error = error,
        backend = "ellmer",
        batch_index = batch_index,
        cache = "bypass"
      )
      metadata[intersect(
        output_fields,
        c(token_fields, "total_tokens", "cost", "duration_s")
      )] <- NULL

      if (!is.null(error)) {
        error_chat <- batch_chat_copy(
          caller_chat,
          "ellmer parallel error result"
        )
        return(annotate_concurrency_result(
          create_error_result(
            error = error,
            index = batch_index,
            prompt = requests[[i]]$prompt,
            instructions = requests[[i]]$instructions,
            llm = error_chat,
            .return_format = .return_format,
            inputs = input_sets[[i]],
            request = requests[[i]],
            turns_before = baseline_turns,
            metadata = metadata,
            module = module,
            .capture_trace = TRUE
          ),
          .concurrency,
          .return_format
        ))
      }

      completed_chat <- mock_batch_chat(
        prompts[[i]],
        response,
        chat,
        turns_before = baseline_turns
      )
      trace <- canonical_run_trace(
        inputs = input_sets[[i]],
        response = response,
        request = requests[[i]],
        chat = completed_chat,
        turns_before = baseline_turns,
        metadata = metadata,
        store_chat = isTRUE(module$config$store_chat_in_traces)
      )
      result <- if (.return_format == "simple") {
        response
      } else {
        list(output = response, chat = completed_chat, metadata = metadata)
      }
      annotate_concurrency_result(
        attach_run_trace(result, trace),
        .concurrency,
        .return_format
      )
    }
  )
  results
}
