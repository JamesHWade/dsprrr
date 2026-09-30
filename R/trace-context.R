# Trace context validation and propagation ---------------------------------

#' Validate a trace context value
#'
#' Trace context is deliberately narrower than arbitrary R data. The root is a
#' named JSON object; nested values may be named-list objects, unnamed-list
#' arrays, finite atomic scalars, or `NULL`. Runtime objects and fields whose
#' names look credential-bearing never cross this boundary.
#' @noRd
trace_context_validate <- function(
  value,
  arg = ".trace_context",
  path = "$",
  root = TRUE,
  depth = 0L,
  state = NULL
) {
  if (is.null(state)) {
    state <- new.env(parent = emptyenv())
    state$nodes <- 0L
    state$bytes <- 0
  }
  state$nodes <- state$nodes + 1L
  if (depth > 64L || state$nodes > 4096L) {
    trace_context_size_abort(arg, path)
  }

  if (is.null(value)) {
    if (isTRUE(root)) {
      trace_context_type_abort(
        arg,
        path,
        "a named list, or an empty list"
      )
    }
    return(NULL)
  }

  if (is.atomic(value)) {
    value_type <- typeof(value)
    valid_type <- value_type %in%
      c(
        "logical",
        "integer",
        "double",
        "character"
      )
    if (
      isTRUE(root) ||
        !valid_type ||
        !is.null(attributes(value)) ||
        length(value) != 1L ||
        anyNA(value) ||
        (identical(value_type, "double") && !is.finite(value)) ||
        (identical(value_type, "character") && !validUTF8(value))
    ) {
      trace_context_type_abort(arg, path, "a plain JSON-compatible scalar")
    }
    if (identical(value_type, "character")) {
      state$bytes <- state$bytes + nchar(value, type = "bytes")
      if (state$bytes > 262144) {
        trace_context_size_abort(arg, path)
      }
    }
    return(value)
  }

  if (!artifact_is_plain_list(value)) {
    trace_context_type_abort(arg, path, "a plain JSON object or array")
  }

  keys <- names(value)
  if (length(value) == 0L) {
    if (isTRUE(root) || is.null(keys)) {
      return(list())
    }
    return(structure(list(), names = character()))
  }
  if (isTRUE(root) && is.null(keys)) {
    trace_context_name_abort(
      arg,
      path,
      "the root must be a named JSON object"
    )
  }
  if (!is.null(keys)) {
    valid_names <- length(keys) == length(value) &&
      !anyNA(keys) &&
      all(nzchar(keys)) &&
      !anyDuplicated(keys) &&
      all(validUTF8(keys))
    if (!valid_names) {
      trace_context_name_abort(
        arg,
        path,
        "object fields must have unique, non-empty UTF-8 names"
      )
    }
    state$bytes <- state$bytes + sum(nchar(keys, type = "bytes"))
    if (state$bytes > 262144) {
      trace_context_size_abort(arg, path)
    }
    for (key in keys) {
      if (isTRUE(root) && identical(key, "program_artifact_id")) {
        trace_context_name_abort(
          arg,
          trace_context_path(path, key),
          "program_artifact_id is reserved for dsprrr's verified identity"
        )
      }
      if (artifact_is_secret_name(key)) {
        cli::cli_abort(
          c(
            "{.arg {arg}} contains a credential-like field",
            "x" = "Field {.field {key}} at {.code {trace_context_path(path, key)}} is not allowed.",
            "i" = "Pass opaque correlation identifiers, never credentials or session secrets."
          ),
          class = c(
            "dsprrr_trace_context_credential_error",
            "dsprrr_trace_context_error"
          ),
          path = trace_context_path(path, key),
          field = key
        )
      }
    }
  }

  output <- vector("list", length(value))
  if (!is.null(keys)) {
    names(output) <- keys
  }
  for (index in seq_along(value)) {
    child_path <- if (is.null(keys)) {
      paste0(path, "[", index, "]")
    } else {
      trace_context_path(path, keys[[index]])
    }
    output[index] <- list(trace_context_validate(
      value[[index]],
      arg = arg,
      path = child_path,
      root = FALSE,
      depth = depth + 1L,
      state = state
    ))
  }
  output
}

trace_context_path <- function(path, key) {
  paste0(path, "$", key)
}

trace_context_type_abort <- function(arg, path, expected) {
  cli::cli_abort(
    c(
      "{.arg {arg}} contains a value that cannot be serialized as JSON",
      "x" = "Value at {.code {path}} must be {expected}."
    ),
    class = c(
      "dsprrr_trace_context_type_error",
      "dsprrr_trace_context_error"
    ),
    path = path
  )
}

trace_context_name_abort <- function(arg, path, detail) {
  cli::cli_abort(
    c(
      "{.arg {arg}} has an invalid object structure",
      "x" = "At {.code {path}}, {detail}."
    ),
    class = c(
      "dsprrr_trace_context_name_error",
      "dsprrr_trace_context_error"
    ),
    path = path
  )
}

trace_context_size_abort <- function(arg, path) {
  cli::cli_abort(
    c(
      "{.arg {arg}} exceeds the trace context size limit",
      "x" = "Context at {.code {path}} is too deep or contains too much data.",
      "i" = "Use compact correlation identifiers instead of payload data."
    ),
    class = c(
      "dsprrr_trace_context_size_error",
      "dsprrr_trace_context_error"
    ),
    path = path
  )
}

current_trace_context <- function() {
  trace_context_validate(
    .dsprrr_env$trace_context %||% list(),
    arg = ".trace_context"
  )
}

current_trace_program_artifact_id <- function() {
  value <- .dsprrr_env$trace_program_artifact_id %||% NA_character_
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    return(NA_character_)
  }
  value
}

trace_context_program_artifact_id <- function(
  program,
  inherit = FALSE
) {
  inherited <- current_trace_program_artifact_id()
  if (
    isTRUE(inherit) &&
      identical(program, .dsprrr_env$trace_program) &&
      !is.na(inherited)
  ) {
    return(inherited)
  }

  tryCatch(
    program_artifact_id(program),
    error = function(error) {
      artifact_error <- any(grepl("^dsprrr_artifact_", class(error)))
      if (artifact_error) {
        return(NA_character_)
      }
      stop(error)
    }
  )
}

trace_context_enter <- function(
  value,
  program = NULL,
  arg = ".trace_context",
  inherit_program_id = FALSE
) {
  context <- trace_context_validate(value, arg = arg)
  previous <- list(
    context = .dsprrr_env$trace_context %||% list(),
    program_artifact_id = .dsprrr_env$trace_program_artifact_id %||%
      NA_character_,
    program = .dsprrr_env$trace_program
  )
  program_artifact_id <- if (is.null(program)) {
    previous$program_artifact_id
  } else {
    trace_context_program_artifact_id(
      program,
      inherit = inherit_program_id
    )
  }
  .dsprrr_env$trace_context <- context
  .dsprrr_env$trace_program_artifact_id <- program_artifact_id
  if (!is.null(program)) {
    .dsprrr_env$trace_program <- program
  }
  previous
}

trace_context_restore <- function(previous) {
  .dsprrr_env$trace_context <- previous$context
  .dsprrr_env$trace_program_artifact_id <- previous$program_artifact_id
  .dsprrr_env$trace_program <- previous$program
  invisible(NULL)
}

trace_context_resolve <- function(value, supplied, arg = ".trace_context") {
  if (isTRUE(supplied)) {
    trace_context_validate(value, arg = arg)
  } else {
    current_trace_context()
  }
}

trace_context_fields <- function(
  context = current_trace_context(),
  program_artifact_id = current_trace_program_artifact_id()
) {
  list(
    program_artifact_id = program_artifact_id,
    trace_context = context
  )
}

trace_context_assign_fields <- function(target, fields, overwrite = TRUE) {
  for (name in names(fields)) {
    if (isTRUE(overwrite) || !name %in% names(target)) {
      target[[name]] <- fields[[name]]
    }
  }
  target
}

trace_context_annotate_metadata <- function(
  metadata,
  fields = trace_context_fields(),
  overwrite = TRUE
) {
  if (is.null(metadata)) {
    metadata <- list()
  } else if (!is.list(metadata)) {
    metadata <- list(value = metadata)
  }
  metadata <- trace_context_assign_fields(
    metadata,
    fields,
    overwrite = overwrite
  )
  if (is.list(metadata$program_trace_events)) {
    metadata$program_trace_events <- trace_context_annotate_events(
      metadata$program_trace_events,
      fields = fields,
      overwrite = overwrite
    )
  }
  metadata
}

trace_context_annotate_event <- function(
  event,
  fields = trace_context_fields(),
  overwrite = TRUE
) {
  if (!is.list(event)) {
    return(event)
  }
  event <- trace_context_assign_fields(event, fields, overwrite = overwrite)
  event$metadata <- trace_context_annotate_metadata(
    event$metadata %||% list(),
    fields = fields,
    overwrite = overwrite
  )
  event
}

trace_context_annotate_result <- function(
  result,
  fields = trace_context_fields(),
  overwrite = TRUE
) {
  if (inherits(result, "dsprrr_result")) {
    result$metadata <- trace_context_annotate_metadata(
      result$metadata,
      fields = fields,
      overwrite = overwrite
    )
    return(result)
  }
  if (inherits(result, "dsprrr_batch_result")) {
    result[] <- lapply(
      result,
      trace_context_annotate_result,
      fields = fields,
      overwrite = overwrite
    )
    row_events <- attr(result, "dsprrr_row_trace_events", exact = TRUE)
    if (!is.null(row_events)) {
      attr(result, "dsprrr_row_trace_events") <- lapply(
        row_events,
        trace_context_annotate_events,
        fields = fields,
        overwrite = overwrite
      )
    }
    return(result)
  }
  if (is.data.frame(result) && ".metadata" %in% names(result)) {
    result$.metadata <- lapply(
      result$.metadata,
      trace_context_annotate_metadata,
      fields = fields,
      overwrite = overwrite
    )
    return(result)
  }
  result
}

trace_context_annotate_events <- function(
  events,
  fields = trace_context_fields(),
  overwrite = TRUE
) {
  if (is.null(events)) {
    return(list())
  }
  lapply(
    events,
    trace_context_annotate_event,
    fields = fields,
    overwrite = overwrite
  )
}

trace_context_annotate_module_traces <- function(
  module,
  cursor,
  fields = trace_context_fields(),
  overwrite = FALSE
) {
  traces <- module$state$traces %||% list()
  indices <- evaluation_trace_indices(module, cursor)
  if (length(indices) == 0L) {
    return(invisible(module))
  }
  for (index in indices) {
    traces[[index]] <- trace_context_annotate_event(
      traces[[index]],
      fields = fields,
      overwrite = overwrite
    )
  }
  module$state$traces <- traces
  invisible(module)
}

# Agent runtimes -----------------------------------------------------------

#' The run context dsprrr passes to a chat that records runs
#'
#' An agent runtime that implements the ellmer Chat protocol, such as a deputy
#' Agent, takes `run_context` in its structured-request methods and records
#' each call as a run. dsprrr passes its trace fields there under one `dsprrr`
#' entry, so the runtime's records name the program and trace context that
#' made the call. A plain ellmer Chat has no `run_context` argument and is
#' called as before.
#' @noRd
chat_run_context <- function(llm, method = "chat_structured") {
  fn <- tryCatch(llm[[method]], error = function(e) NULL)
  if (!is.function(fn) || !"run_context" %in% names(formals(fn))) {
    return(NULL)
  }
  fields <- trace_context_fields()
  context <- list()
  if (!is.na(fields$program_artifact_id)) {
    context$program_artifact_id <- fields$program_artifact_id
  }
  if (length(fields$trace_context) > 0L) {
    context$trace_context <- fields$trace_context
  }
  if (length(context) == 0L) {
    return(NULL)
  }
  list(dsprrr = context)
}

#' Identifiers of the latest run a chat that records runs made
#'
#' A deputy Agent's `last_run()` returns the result of its latest run. The
#' receipt keeps its identifiers, so a dsprrr trace can be matched with the
#' agent's own records. `NULL` for a chat without `last_run()`, or one that has
#' not run.
#' @noRd
chat_run_receipt <- function(llm) {
  last_run <- tryCatch(llm[["last_run"]], error = function(e) NULL)
  if (!is.function(last_run)) {
    return(NULL)
  }
  run <- tryCatch(last_run(), error = function(e) NULL)
  if (is.null(run)) {
    return(NULL)
  }
  field <- function(name) {
    value <- tryCatch(do.call(`$`, list(run, name)), error = function(e) NULL)
    if (is.character(value) && length(value) == 1L && !is.na(value)) value
  }
  receipt <- list(
    run_id = field("run_id"),
    agent_id = field("agent_id"),
    session_id = field("session_id"),
    stop_reason = field("stop_reason")
  )
  receipt <- Filter(Negate(is.null), receipt)
  if (is.null(receipt$run_id)) NULL else receipt
}

#' The receipt of the run a call made, if it made a new one
#'
#' `before` is the run id recorded before the call. A cache hit, or a call that
#' failed before the agent started a run, leaves the latest run unchanged, and
#' then there is no receipt.
#' @noRd
chat_new_run_receipt <- function(llm, before) {
  receipt <- chat_run_receipt(llm)
  if (is.null(receipt) || identical(receipt$run_id, before)) {
    return(NULL)
  }
  receipt
}

chat_last_run_id <- function(llm) {
  chat_run_receipt(llm)$run_id
}
