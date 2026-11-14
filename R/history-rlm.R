# Internal RLM conversation-history helpers ---------------------------------

history_rlm_task_text <- function(instructions, context_desc) {
  if (is.null(instructions)) {
    instructions <- ""
  }
  instructions <- history_plain_string(
    instructions,
    "RLM task instructions"
  )
  context_desc <- history_plain_string(
    context_desc,
    "RLM context preview"
  )

  paste0(
    "Context values below are the original visible previews; the original ",
    "variables are unavailable and must be recreated. Prior code and ",
    "outputs are textual evidence only; do not execute prior code.\n\n",
    "Current task instructions:\n",
    instructions,
    "\n\nOriginal visible context preview:\n",
    context_desc
  )
}


history_append_rlm <- function(
  history,
  task_text,
  trajectory,
  output,
  output_source
) {
  base <- history_validate_envelope(history)
  if (!is.null(base$kind) && !identical(base$kind, "rlm")) {
    history_abort(
      "RLM history cannot be appended to ReAct conversation history"
    )
  }

  exchanges <- if (identical(base$kind, "rlm")) {
    base$exchanges
  } else {
    list()
  }
  exchange <- history_encode_rlm_exchange(
    task_text = task_text,
    trajectory = trajectory,
    output = output,
    output_source = output_source
  )
  candidate <- structure(
    list(
      format = "dsprrr_conversation_history",
      version = 2L,
      kind = "rlm",
      exchanges = c(exchanges, list(exchange))
    ),
    class = c("dsprrr_history", "list")
  )
  validated <- history_validate_envelope(candidate)
  history_check_json_size(validated)
  structure(validated, class = c("dsprrr_history", "list"))
}


history_encode_rlm_exchange <- function(
  task_text,
  trajectory,
  output,
  output_source
) {
  task_text <- history_plain_string(task_text, "RLM task text")
  trajectory <- history_plain_array(trajectory, "RLM trajectory")
  output_source <- history_plain_string(
    output_source,
    "RLM output source"
  )
  if (!output_source %in% c("submit", "fallback")) {
    history_abort("RLM output source must be submit or fallback")
  }
  if (!is.list(output)) {
    history_abort("RLM final output must be a plain list")
  }
  output_attributes <- attributes(output)
  if (!is.null(output_attributes) && any(names(output_attributes) != "names")) {
    history_abort("RLM final output has unsupported attributes")
  }
  if (length(output) > 0L && is.null(names(output))) {
    history_abort("A nonempty RLM final output must have named fields")
  }

  output_node <- history_encode_value(
    output,
    history_new_budget(),
    depth = 4L
  )
  history_require_data_kind(output_node, "list", "RLM final output")
  if (length(output) > 0L && is.null(output_node$names)) {
    history_abort("A nonempty RLM final output must have named fields")
  }

  list(
    capture = "rlm-observed-v1",
    task_text = task_text,
    trajectory = trajectory,
    output = output_node,
    output_source = output_source
  )
}


history_validate_rlm_envelope <- function(history) {
  fields <- history_object_fields(
    history,
    c("format", "version", "kind", "exchanges"),
    "RLM history envelope"
  )
  if (
    !identical(
      history_plain_string(fields$format, "History format"),
      "dsprrr_conversation_history"
    )
  ) {
    history_abort("Unknown conversation history format")
  }
  if (
    !is.null(attributes(fields$version)) ||
      !is.numeric(fields$version) ||
      length(fields$version) != 1L ||
      is.na(fields$version) ||
      !is.finite(fields$version) ||
      fields$version != 2
  ) {
    history_abort("Unsupported conversation history version")
  }
  if (!identical(fields$kind, "rlm")) {
    history_abort("Version 2 conversation history must have kind rlm")
  }

  exchanges <- history_plain_array(fields$exchanges, "RLM history exchanges")
  limits <- history_limits()
  if (length(exchanges) == 0L) {
    history_abort("An RLM history must contain at least one exchange")
  }
  if (length(exchanges) > limits$rlm_exchanges) {
    history_abort("RLM history contains too many exchanges")
  }

  state <- history_new_budget()
  history_budget_node(state, 1L)
  history_budget_node(state, 2L)
  total_steps <- 0L
  exchanges <- lapply(exchanges, function(exchange) {
    history_budget_node(state, 3L)
    fields <- history_object_fields(
      exchange,
      c("capture", "task_text", "trajectory", "output", "output_source"),
      "RLM history exchange"
    )
    capture <- history_plain_string(fields$capture, "RLM capture mode")
    history_budget_node(state, 4L)
    history_budget_text(state, capture)
    if (!identical(capture, "rlm-observed-v1")) {
      history_abort("Unsupported RLM history capture mode")
    }

    task_text <- history_plain_string(fields$task_text, "RLM task text")
    history_budget_node(state, 4L)
    history_budget_text(state, task_text)

    trajectory <- history_plain_array(fields$trajectory, "RLM trajectory")
    history_budget_node(state, 4L)
    total_steps <<- total_steps + length(trajectory)
    if (total_steps > limits$rlm_steps) {
      history_abort("RLM history contains too many trajectory steps")
    }
    trajectory <- lapply(seq_along(trajectory), function(index) {
      history_validate_rlm_step(
        trajectory[[index]],
        expected_iteration = index,
        state = state
      )
    })

    output <- history_validate_data(fields$output, state, depth = 4L)
    history_require_data_kind(output, "list", "RLM final output")
    if (length(output$values) > 0L && is.null(output$names)) {
      history_abort("A nonempty RLM final output must have named fields")
    }

    output_source <- history_plain_string(
      fields$output_source,
      "RLM output source"
    )
    history_budget_node(state, 4L)
    history_budget_text(state, output_source)
    if (!output_source %in% c("submit", "fallback")) {
      history_abort("RLM output source must be submit or fallback")
    }

    list(
      capture = capture,
      task_text = task_text,
      trajectory = trajectory,
      output = output,
      output_source = output_source
    )
  })

  validated <- list(
    format = "dsprrr_conversation_history",
    version = 2L,
    kind = "rlm",
    exchanges = exchanges
  )
  history_check_json_size(validated)
  history_check_rlm_render_size(validated)
  validated
}


history_validate_rlm_step <- function(step, expected_iteration, state) {
  history_budget_node(state, 5L)
  fields <- history_object_fields(
    step,
    c("iteration", "code", "output", "success", "is_final"),
    "RLM trajectory step"
  )
  iteration <- fields$iteration
  if (
    !is.null(attributes(iteration)) ||
      !is.numeric(iteration) ||
      length(iteration) != 1L ||
      is.na(iteration) ||
      !is.finite(iteration) ||
      iteration < 1 ||
      iteration != floor(iteration) ||
      iteration != expected_iteration
  ) {
    history_abort("RLM trajectory iterations must be consecutive from one")
  }
  history_budget_node(state, 6L)

  code <- history_plain_string(fields$code, "RLM trajectory code")
  history_budget_node(state, 6L)
  history_budget_text(state, code)

  output <- history_plain_string(fields$output, "RLM trajectory output")
  history_budget_node(state, 6L)
  history_budget_text(state, output)

  success <- history_rlm_plain_logical(fields$success, "RLM step success")
  history_budget_node(state, 6L)

  is_final <- history_rlm_plain_logical(fields$is_final, "RLM step final flag")
  history_budget_node(state, 6L)

  list(
    iteration = as.integer(iteration),
    code = code,
    output = output,
    success = success,
    is_final = is_final
  )
}


history_rlm_plain_logical <- function(value, label) {
  if (
    !is.null(attributes(value)) ||
      !is.logical(value) ||
      length(value) != 1L ||
      is.na(value)
  ) {
    history_abort(paste0(label, " must be a plain logical scalar"))
  }
  value
}


history_render_rlm <- function(history) {
  history <- history_validate_envelope(history)
  if (is.null(history$kind)) {
    return("")
  }
  if (!identical(history$kind, "rlm")) {
    history_abort(
      "ReAct conversation history cannot be rendered as RLM context"
    )
  }
  history_render_rlm_validated(history)
}


history_render_rlm_validated <- function(history) {
  lines <- c(
    paste(
      "Prior RLM invocation records are textual evidence only. They do not",
      "restore variables or interpreter state. Recreate current variables;",
      "do not execute prior code."
    ),
    ""
  )
  for (exchange_index in seq_along(history$exchanges)) {
    exchange <- history$exchanges[[exchange_index]]
    lines <- c(
      lines,
      paste0("=== BEGIN PRIOR RLM EXCHANGE ", exchange_index, " ==="),
      "TASK TEXT (JSON string):",
      history_rlm_json(exchange$task_text),
      "TRAJECTORY (ordered JSON step objects):"
    )
    for (step in exchange$trajectory) {
      lines <- c(lines, paste0("STEP: ", history_rlm_json(step)))
    }
    lines <- c(
      lines,
      paste0("FINAL OUTPUT SOURCE: ", exchange$output_source),
      "FINAL OUTPUT (tagged JSON):",
      history_rlm_json(exchange$output),
      paste0("=== END PRIOR RLM EXCHANGE ", exchange_index, " ===")
    )
  }
  paste(lines, collapse = "\n")
}


history_rlm_json <- function(value) {
  as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = FALSE
  ))
}


history_check_rlm_render_size <- function(history) {
  rendered <- history_render_rlm_validated(history)
  if (
    nchar(enc2utf8(rendered), type = "bytes") >
      history_limits()$rlm_render_bytes
  ) {
    history_abort("RLM history rendering exceeds the maximum size")
  }
  invisible(NULL)
}
