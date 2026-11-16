#' Construct a portable conversation history
#'
#' Converts native ellmer user and assistant turns into a versioned envelope
#' containing only allowlisted conversation data. Tool requests and results are
#' retained as data; they are never executed. System turns and unsupported or
#' nonportable content fail explicitly.
#'
#' @param turns An unnamed list of ellmer UserTurn and AssistantTurn objects.
#'   The empty list constructs an empty history.
#'
#' @return An object with class dsprrr_history. This constructor creates a
#'   version-1 empty or ReAct history. RLM execution returns version-2 history
#'   containing observed invocation exchanges in result metadata.
#'
#' @details
#' Supported content includes text, JSON, thinking with JSON-safe extra data,
#' inline and remote images, PDFs, tool requests, and tool results. Runtime tool
#' objects are omitted. A tool result may contain JSON-safe data or an ordered
#' list of allowlisted content. Condition errors are retained as their message;
#' runtime condition and call objects are dropped. Uploaded content and
#' arbitrary R objects are rejected. Tool request IDs must be unique and every
#' request must have one matching result before the history is accepted.
#' Histories are limited to 64 data nesting levels, 12 MiB of data, 16 MiB of
#' JSON, 10,000 turns, and 100,000 tagged values.
#'
#' Histories are copied into a tagged data representation. They do not retain
#' ellmer Chat objects, functions, environments, external pointers, credentials
#' attached to runtime tools, or usage, cost, or duration metadata. Conversation
#' text, reasoning, media, and tool data may still be sensitive; protect saved
#' history files accordingly.
#'
#' RLM histories instead store visible task previews, inert code and observed
#' output text, plus complete typed final outputs. They do not restore variables
#' or interpreter state. ReAct turns and RLM exchanges cannot be mixed.
#'
#' Saved JSON is limited to 16 MiB; retained string and media data are limited
#' to 12 MiB. Histories also have limits on turns, nesting depth, and values.
#' RLM histories additionally retain at most 100 exchanges and 1,000 observed
#' steps, with a canonical rendered context limit of 128 KiB. Oversized
#' histories fail explicitly rather than silently truncating evidence.
#'
#' @examples
#' turns <- list(
#'   ellmer::UserTurn(list(ellmer::ContentText("What is 2 + 2?"))),
#'   ellmer::AssistantTurn(list(ellmer::ContentText("4")))
#' )
#' history <- conversation_history(turns)
#' identical(length(history_turns(history)), 2L)
#'
#' @export
conversation_history <- function(turns = list()) {
  state <- history_new_budget()
  turns <- history_plain_array(turns, "turns")
  if (length(turns) > history_limits()$turns) {
    history_abort("Conversation history contains too many turns")
  }

  encoded_turns <- lapply(turns, function(turn) {
    history_encode_turn(turn, state)
  })
  history <- structure(
    list(
      format = "dsprrr_conversation_history",
      version = 1L,
      kind = if (length(encoded_turns) == 0L) NULL else "react",
      turns = encoded_turns
    ),
    class = c("dsprrr_history", "list")
  )
  history <- structure(
    history_validate_envelope(history),
    class = c("dsprrr_history", "list")
  )
  history_check_json_size(history)
  history
}

#' Reconstruct native ellmer turns from conversation history
#'
#' @param history An object created by conversation_history(), loaded by
#'   load_history(), or returned in structured result metadata.
#'
#' @return A fresh unnamed list of ellmer UserTurn and AssistantTurn objects
#'   for empty or ReAct histories. RLM histories contain observed invocation
#'   exchanges and cannot be reconstructed as ellmer turns. Assistant JSON and
#'   finish reason are retained. Usage, cost, and duration metadata use ellmer
#'   defaults.
#'
#' @export
history_turns <- function(history) {
  history <- history_validate_envelope(history)
  if (identical(history$kind, "rlm")) {
    history_abort(
      "RLM histories contain observed invocation exchanges, not ellmer turns"
    )
  }
  lapply(history$turns, history_decode_turn)
}

#' Save conversation history as JSON
#'
#' @param history An object created by conversation_history(), loaded by
#'   load_history(), or returned in structured result metadata.
#' @param path A file path for the JSON document.
#'
#' @return The path, invisibly.
#'
#' @export
save_history <- function(history, path) {
  history <- history_validate_envelope(history)
  json <- history_to_json(history)
  history_check_json_size(json)
  path <- history_path(path)
  tryCatch(
    writeBin(charToRaw(enc2utf8(json)), con = path),
    error = function(error) {
      history_abort(
        paste0(
          "Could not write conversation history: ",
          conditionMessage(error)
        )
      )
    }
  )
  invisible(path)
}

#' Load conversation history from JSON
#'
#' @param path A file path to a JSON document written by save_history().
#'
#' @return A validated dsprrr_history object.
#'
#' @export
load_history <- function(path) {
  path <- history_path(path)
  info <- file.info(path)
  if (is.na(info$size) || isTRUE(info$isdir)) {
    history_abort("Conversation history path is not a readable file")
  }
  if (info$size > history_limits()$json_bytes) {
    history_abort("Conversation history JSON exceeds the maximum file size")
  }

  json <- tryCatch(
    readChar(path, nchars = info$size, useBytes = TRUE),
    error = function(error) {
      history_abort(
        paste0("Could not read conversation history: ", conditionMessage(error))
      )
    }
  )
  history_reject_duplicate_json_fields(json)
  if (!isTRUE(jsonlite::validate(json))) {
    history_abort("Conversation history is not valid JSON")
  }
  decoded <- tryCatch(
    jsonlite::fromJSON(json, simplifyVector = FALSE),
    error = function(error) {
      history_abort(
        paste0(
          "Could not parse conversation history JSON: ",
          conditionMessage(error)
        )
      )
    }
  )
  history <- history_validate_envelope(decoded, require_class = FALSE)
  history_check_json_size(history)
  structure(history, class = c("dsprrr_history", "list"))
}

history_limits <- function() {
  list(
    depth = 64L,
    nodes = 100000L,
    parser_nodes = 1000000L,
    turns = 10000L,
    rlm_exchanges = 100L,
    rlm_steps = 1000L,
    rlm_render_bytes = 128 * 1024,
    data_bytes = 12 * 1024^2,
    json_bytes = 16 * 1024^2
  )
}

history_abort <- function(message) {
  cli::cli_abort(message, class = "dsprrr_history_error")
}

history_new_budget <- function() {
  state <- new.env(parent = emptyenv())
  state$nodes <- 0L
  state$bytes <- 0
  state
}

history_budget_node <- function(state, depth) {
  limits <- history_limits()
  if (depth > limits$depth) {
    history_abort("Conversation history data exceeds the maximum nesting depth")
  }
  state$nodes <- state$nodes + 1L
  if (state$nodes > limits$nodes) {
    history_abort("Conversation history data contains too many values")
  }
}

history_budget_text <- function(state, value) {
  if (is.null(value) || length(value) == 0L) {
    return(invisible(NULL))
  }
  bytes <- sum(nchar(enc2utf8(value), type = "bytes"))
  state$bytes <- state$bytes + bytes
  if (state$bytes > history_limits()$data_bytes) {
    history_abort("Conversation history data exceeds the maximum size")
  }
  invisible(NULL)
}

history_plain_array <- function(x, label) {
  if (!is.list(x) || !is.null(attr(x, "class", exact = TRUE))) {
    history_abort(paste0(label, " must be a plain list"))
  }
  attributes <- attributes(x)
  if (!is.null(attributes) && any(names(attributes) != "names")) {
    history_abort(paste0(label, " must not have unsupported attributes"))
  }
  value_names <- names(x)
  if (!is.null(value_names)) {
    history_abort(paste0(label, " must be an unnamed list"))
  }
  unname(x)
}

history_plain_string <- function(x, label, nonempty = FALSE) {
  if (!is.null(attributes(x))) {
    history_abort(paste0(label, " must be a plain character scalar"))
  }
  if (
    !is.character(x) ||
      length(x) != 1L ||
      is.na(x) ||
      (nonempty && !nzchar(x))
  ) {
    history_abort(paste0(label, " must be a plain character scalar"))
  }
  x <- enc2utf8(x)
  x
}

history_value_names <- function(value, label, state) {
  value_names <- attr(value, "names", exact = TRUE)
  if (is.null(value_names)) {
    return(NULL)
  }
  if (!is.null(attributes(value_names))) {
    history_abort(paste0(label, " names must be plain character values"))
  }
  if (
    !is.character(value_names) ||
      length(value_names) != length(value) ||
      anyNA(value_names) ||
      !all(nzchar(value_names)) ||
      anyDuplicated(value_names)
  ) {
    history_abort(
      paste0(label, " names must be unique, nonempty character values")
    )
  }
  value_names <- enc2utf8(value_names)
  history_budget_text(state, value_names)
  unname(as.list(value_names))
}

history_encode_value <- function(value, state, depth = 1L) {
  history_budget_node(state, depth)
  if (is.null(value)) {
    return(list(tag = "null"))
  }

  attributes <- attributes(value)
  if (!is.null(attributes) && any(names(attributes) != "names")) {
    history_abort(
      paste0(
        "Conversation history cannot retain attributes on ",
        typeof(value),
        " values"
      )
    )
  }

  if (is.list(value)) {
    value_names <- history_value_names(value, "List", state)
    values <- lapply(seq_along(value), function(i) {
      history_encode_value(value[[i]], state, depth + 1L)
    })
    return(list(tag = "list", names = value_names, values = values))
  }

  type <- typeof(value)
  if (!type %in% c("logical", "integer", "double", "character")) {
    history_abort(
      paste0("Conversation history cannot retain values of type ", type)
    )
  }
  if (type == "double" && any(is.nan(value) | is.infinite(value))) {
    history_abort("Conversation history does not accept NaN or infinite values")
  }

  value_names <- history_value_names(value, "Vector", state)
  values <- lapply(seq_along(value), function(i) {
    item <- value[[i]]
    if (is.na(item)) {
      return(list(tag = type, value = NULL))
    }
    if (type == "integer") {
      item <- as.character(item)
    } else if (type == "double") {
      item <- formatC(item, digits = 17L, format = "g", decimal.mark = ".")
    } else if (type == "character") {
      item <- enc2utf8(item)
      history_budget_text(state, item)
    }
    list(tag = type, value = item)
  })
  list(tag = "vector", type = type, names = value_names, values = values)
}

history_encode_turn <- function(turn, state) {
  turn_class <- class(turn)
  if (
    identical(turn_class, c("ellmer::SystemTurn", "ellmer::Turn", "S7_object"))
  ) {
    history_abort("SystemTurn objects are not retained in conversation history")
  }
  if (
    !identical(
      turn_class,
      c("ellmer::UserTurn", "ellmer::Turn", "S7_object")
    ) &&
      !identical(
        turn_class,
        c("ellmer::AssistantTurn", "ellmer::Turn", "S7_object")
      )
  ) {
    history_abort(
      "Turns must be native ellmer UserTurn or AssistantTurn objects"
    )
  }
  contents <- tryCatch(turn@contents, error = function(error) NULL)
  contents <- history_plain_array(contents, "Turn contents")
  record <- list(
    role = if (identical(turn_class[[1L]], "ellmer::UserTurn")) {
      "user"
    } else {
      "assistant"
    },
    contents = lapply(contents, history_encode_content, state = state)
  )
  if (identical(turn_class[[1L]], "ellmer::AssistantTurn")) {
    record$json <- history_encode_value(turn@json, state)
    record$finish_reason <- history_encode_value(turn@finish_reason, state)
  }
  record
}

history_encode_content <- function(content, state) {
  content_class <- class(content)
  type <- history_content_type(content)
  if (is.null(type)) {
    if (identical(content_class[[1L]], "ellmer::ContentUploaded")) {
      history_abort(
        "Uploaded content is not portable and cannot be retained in history"
      )
    }
    history_abort(
      paste0(
        "Unsupported conversation content class: ",
        paste(content_class, collapse = "/")
      )
    )
  }

  if (identical(type, "text")) {
    return(list(
      type = type,
      text = history_encode_value(content@text, state)
    ))
  }
  if (identical(type, "json")) {
    return(list(
      type = type,
      data = history_encode_value(content@data, state),
      string = history_encode_value(content@string, state)
    ))
  }
  if (identical(type, "thinking")) {
    return(list(
      type = type,
      thinking = history_encode_value(content@thinking, state),
      extra = history_encode_value(content@extra, state)
    ))
  }
  if (identical(type, "image_inline")) {
    return(list(
      type = type,
      media_type = history_encode_value(content@type, state),
      data = history_encode_value(content@data, state)
    ))
  }
  if (identical(type, "image_remote")) {
    return(list(
      type = type,
      url = history_encode_value(content@url, state),
      detail = history_encode_value(content@detail, state)
    ))
  }
  if (identical(type, "pdf")) {
    return(list(
      type = type,
      media_type = history_encode_value(content@type, state),
      data = history_encode_value(content@data, state),
      filename = history_encode_value(content@filename, state),
      url = history_encode_value(content@url, state)
    ))
  }
  if (identical(type, "tool_request")) {
    return(history_encode_tool_request(content, state))
  }
  history_encode_tool_result(content, state)
}

history_encode_tool_request <- function(content, state) {
  id <- history_plain_string(content@id, "Tool request ID", nonempty = TRUE)
  name <- history_plain_string(content@name, "Tool name", nonempty = TRUE)
  history_budget_text(state, c(id, name))
  list(
    type = "tool_request",
    id = id,
    name = name,
    arguments = history_encode_value(content@arguments, state),
    extra = history_encode_value(content@extra, state)
  )
}

history_encode_tool_result <- function(content, state) {
  request <- content@request
  if (is.null(request)) {
    request_record <- NULL
  } else {
    if (
      !identical(
        class(request),
        c("ellmer::ContentToolRequest", "ellmer::Content", "S7_object")
      )
    ) {
      history_abort("Tool result request must be a native ContentToolRequest")
    }
    request_record <- history_encode_tool_request(request, state)
  }
  list(
    type = "tool_result",
    value = history_encode_tool_result_value(content@value, state),
    error = history_encode_value(history_safe_tool_error(content@error), state),
    extra = history_encode_value(content@extra, state),
    request = request_record
  )
}

history_safe_tool_error <- function(error) {
  if (inherits(error, "condition")) {
    error <- tryCatch(conditionMessage(error), error = function(e) {
      "Tool failed"
    })
  }
  error
}

history_content_classes <- function() {
  list(
    text = c("ellmer::ContentText", "ellmer::Content", "S7_object"),
    json = c("ellmer::ContentJson", "ellmer::Content", "S7_object"),
    thinking = c("ellmer::ContentThinking", "ellmer::Content", "S7_object"),
    image_inline = c(
      "ellmer::ContentImageInline",
      "ellmer::ContentImage",
      "ellmer::Content",
      "S7_object"
    ),
    image_remote = c(
      "ellmer::ContentImageRemote",
      "ellmer::ContentImage",
      "ellmer::Content",
      "S7_object"
    ),
    pdf = c("ellmer::ContentPDF", "ellmer::Content", "S7_object"),
    tool_request = c(
      "ellmer::ContentToolRequest",
      "ellmer::Content",
      "S7_object"
    ),
    tool_result = c("ellmer::ContentToolResult", "ellmer::Content", "S7_object")
  )
}

history_content_type <- function(content) {
  content_classes <- history_content_classes()
  matches <- names(content_classes)[vapply(
    content_classes,
    identical,
    logical(1),
    y = class(content)
  )]
  if (length(matches) == 1L) matches else NULL
}

history_encode_tool_result_value <- function(value, state) {
  if (inherits(value, "ellmer::Content")) {
    return(list(
      tag = "content",
      content = history_encode_tool_display_content(value, state)
    ))
  }
  value_attributes <- attributes(value)
  if (
    !is.null(value_attributes) &&
      any(names(value_attributes) != "names")
  ) {
    history_abort(
      "Conversation history cannot retain attributes on tool result values"
    )
  }
  if (
    is.list(value) &&
      length(value) > 0L &&
      all(vapply(value, inherits, logical(1), what = "ellmer::Content"))
  ) {
    value <- history_plain_array(value, "Tool result content list")
    return(list(
      tag = "content_list",
      contents = lapply(
        value,
        history_encode_tool_display_content,
        state = state
      )
    ))
  }
  list(tag = "data", data = history_encode_value(value, state))
}

history_encode_tool_display_content <- function(content, state) {
  if (inherits(content, "ellmer::ContentUploaded")) {
    history_abort(
      "Uploaded content is not portable and cannot be retained in history"
    )
  }
  type <- history_content_type(content)
  if (
    is.null(type) ||
      !type %in%
        c("text", "json", "thinking", "image_inline", "image_remote", "pdf")
  ) {
    history_abort("Tool result content must be supported display content")
  }
  record <- history_encode_content(content, state)
  record
}

history_validate_tool_result_value <- function(value, state) {
  value <- history_plain_object(value, "Tool result value")
  tag <- history_plain_string(value[["tag"]], "Tool result value tag")
  if (identical(tag, "data")) {
    fields <- history_object_fields(
      value,
      c("tag", "data"),
      "Tool result data value"
    )
    return(list(tag = tag, data = history_validate_data(fields$data, state)))
  }
  if (identical(tag, "content")) {
    fields <- history_object_fields(
      value,
      c("tag", "content"),
      "Tool result content value"
    )
    return(list(
      tag = tag,
      content = history_validate_tool_display_record(fields$content, state)
    ))
  }
  if (identical(tag, "content_list")) {
    fields <- history_object_fields(
      value,
      c("tag", "contents"),
      "Tool result content list"
    )
    contents <- history_plain_array(fields$contents, "Tool result contents")
    return(list(
      tag = tag,
      contents = lapply(
        contents,
        history_validate_tool_display_record,
        state = state
      )
    ))
  }
  history_abort(paste0("Unknown tool result value tag: ", tag))
}

history_validate_tool_display_record <- function(content, state) {
  content <- history_plain_object(content, "Tool result content")
  type <- history_plain_string(
    content[["type"]],
    "Tool result content type"
  )
  if (
    !type %in%
      c("text", "json", "thinking", "image_inline", "image_remote", "pdf")
  ) {
    history_abort("Tool result content must be supported display content")
  }
  history_validate_content(content, state)
}

history_validate_envelope <- function(history, require_class = TRUE) {
  if (require_class) {
    if (!identical(class(history), c("dsprrr_history", "list"))) {
      history_abort(
        "history must be created by conversation_history() or load_history()"
      )
    }
    history <- unclass(history)
  }
  history <- history_plain_object(history, "Conversation history envelope")
  field_names <- attr(history, "names", exact = TRUE)
  required <- c("format", "version", "kind")
  if (length(setdiff(required, field_names)) > 0L) {
    history_abort("Conversation history envelope is missing required fields")
  }
  if (
    !identical(
      history_plain_string(history[["format"]], "History format"),
      "dsprrr_conversation_history"
    )
  ) {
    history_abort("Unknown conversation history format")
  }
  version <- history[["version"]]
  if (
    !is.null(attributes(version)) ||
      !is.numeric(version) ||
      length(version) != 1L ||
      is.na(version) ||
      !is.finite(version)
  ) {
    history_abort("Unsupported conversation history version")
  }
  kind <- history[["kind"]]
  if (!is.null(kind)) {
    kind <- history_plain_string(kind, "History kind")
  }

  if (identical(as.numeric(version), 1)) {
    return(history_validate_v1_envelope(history))
  }
  if (identical(as.numeric(version), 2) && identical(kind, "rlm")) {
    return(history_validate_rlm_envelope(history))
  }
  if (identical(as.numeric(version), 2)) {
    history_abort("Unsupported conversation history version or kind")
  }
  history_abort("Unsupported conversation history version")
}

history_validate_v1_envelope <- function(history) {
  fields <- history_object_fields(
    history,
    c("format", "version", "kind", "turns"),
    "Conversation history envelope"
  )
  if (!is.null(fields$kind) && !identical(fields$kind, "react")) {
    history_abort("Unsupported conversation history kind")
  }
  turns <- history_plain_array(fields$turns, "History turns")
  if (length(turns) > history_limits()$turns) {
    history_abort("Conversation history contains too many turns")
  }
  state <- history_new_budget()
  turns <- lapply(turns, history_validate_turn, state = state)
  if (is.null(fields$kind) && length(turns) != 0L) {
    history_abort("An empty history kind requires an empty turn list")
  }
  if (identical(fields$kind, "react") && length(turns) == 0L) {
    history_abort("A react history kind requires at least one turn")
  }
  history_validate_tool_pairs(turns)
  list(
    format = "dsprrr_conversation_history",
    version = 1L,
    kind = fields$kind,
    turns = turns
  )
}

history_object_fields <- function(value, expected, label) {
  value <- history_plain_object(value, label)
  field_names <- attr(value, "names", exact = TRUE)
  unknown <- setdiff(field_names, expected)
  missing <- setdiff(expected, field_names)
  if (length(unknown) > 0L || length(missing) > 0L) {
    history_abort(
      paste0(
        label,
        " fields do not match the supported schema",
        if (length(unknown) > 0L) {
          paste0("; unknown: ", paste(unknown, collapse = ", "))
        } else {
          ""
        },
        if (length(missing) > 0L) {
          paste0("; missing: ", paste(missing, collapse = ", "))
        } else {
          ""
        }
      )
    )
  }
  value[expected]
}

history_plain_object <- function(value, label) {
  if (!is.list(value) || !is.null(attr(value, "class", exact = TRUE))) {
    history_abort(paste0(label, " must be a plain JSON object"))
  }
  attributes <- attributes(value)
  if (!is.null(attributes) && any(names(attributes) != "names")) {
    history_abort(paste0(label, " has unsupported attributes"))
  }
  field_names <- attr(value, "names", exact = TRUE)
  if (
    is.null(field_names) ||
      !is.character(field_names) ||
      !is.null(attributes(field_names)) ||
      anyNA(field_names) ||
      !all(nzchar(field_names)) ||
      anyDuplicated(field_names)
  ) {
    history_abort(paste0(label, " has missing or duplicate fields"))
  }
  value
}

history_validate_turn <- function(turn, state) {
  turn <- history_plain_object(turn, "History turn")
  role <- history_plain_string(turn[["role"]], "Turn role")
  if (!role %in% c("user", "assistant")) {
    history_abort("History may contain only user and assistant turns")
  }
  expected <- if (identical(role, "assistant")) {
    c("role", "contents", "json", "finish_reason")
  } else {
    c("role", "contents")
  }
  fields <- history_object_fields(turn, expected, "History turn")
  contents <- history_plain_array(fields$contents, "Turn contents")
  record <- list(
    role = role,
    contents = lapply(contents, history_validate_content, state = state)
  )
  if (identical(role, "assistant")) {
    json <- history_validate_data(fields$json, state)
    history_require_data_kind(json, "list", "Assistant JSON")
    finish_reason <- history_validate_data(fields$finish_reason, state)
    history_require_data_kind(
      finish_reason,
      "character",
      "Assistant finish reason"
    )
    record$json <- json
    record$finish_reason <- finish_reason
  }
  record
}

history_validate_content <- function(content, state) {
  content <- history_plain_object(content, "History content")
  type <- history_plain_string(content[["type"]], "History content type")
  expected <- switch(
    type,
    text = c("type", "text"),
    json = c("type", "data", "string"),
    thinking = c("type", "thinking", "extra"),
    image_inline = c("type", "media_type", "data"),
    image_remote = c("type", "url", "detail"),
    pdf = c("type", "media_type", "data", "filename", "url"),
    tool_request = c("type", "id", "name", "arguments", "extra"),
    tool_result = c("type", "value", "error", "extra", "request"),
    NULL
  )
  if (is.null(expected)) {
    history_abort(paste0("Unknown conversation content tag: ", type))
  }
  fields <- history_object_fields(content, expected, "History content")
  if (type == "text") {
    text <- history_validate_data(fields$text, state)
    history_require_data_string(text, "Text content", nonempty = FALSE)
    return(list(type = type, text = text))
  }
  if (type == "json") {
    data <- history_validate_data(fields$data, state)
    string <- history_validate_data(fields$string, state)
    history_require_data_kind(
      string,
      "character",
      "JSON content string",
      allow_null = TRUE
    )
    return(list(
      type = type,
      data = data,
      string = string
    ))
  }
  if (type == "thinking") {
    thinking <- history_validate_data(fields$thinking, state)
    history_require_data_string(thinking, "Thinking content", nonempty = FALSE)
    extra <- history_validate_data(fields$extra, state)
    history_require_data_kind(extra, "list", "Thinking extra")
    return(list(
      type = type,
      thinking = thinking,
      extra = extra
    ))
  }
  if (type == "image_inline") {
    media_type <- history_data_string(
      fields$media_type,
      "Image media type",
      TRUE,
      state
    )
    data <- history_data_string(fields$data, "Inline image data", FALSE, state)
    return(list(type = type, media_type = media_type, data = data))
  }
  if (type == "image_remote") {
    url <- history_data_string(fields$url, "Remote image URL", TRUE, state)
    detail <- history_data_string(fields$detail, "Image detail", FALSE, state)
    return(list(type = type, url = url, detail = detail))
  }
  if (type == "pdf") {
    media_type <- history_data_string(
      fields$media_type,
      "PDF media type",
      TRUE,
      state
    )
    data <- history_data_string(fields$data, "PDF data", FALSE, state)
    filename <- history_data_string(
      fields$filename,
      "PDF filename",
      FALSE,
      state
    )
    url <- history_validate_data(fields$url, state)
    if (!identical(url$tag, "null")) {
      history_require_data_string(url, "PDF URL", nonempty = FALSE)
    }
    return(list(
      type = type,
      media_type = media_type,
      data = data,
      filename = filename,
      url = url
    ))
  }
  if (type == "tool_request") {
    id <- history_plain_string(fields$id, "Tool request ID", nonempty = TRUE)
    name <- history_plain_string(fields$name, "Tool name", nonempty = TRUE)
    history_budget_text(state, c(id, name))
    arguments <- history_validate_data(fields$arguments, state)
    extra <- history_validate_data(fields$extra, state)
    history_require_data_kind(arguments, "list", "Tool request arguments")
    history_require_data_kind(extra, "list", "Tool request extra")
    return(list(
      type = type,
      id = id,
      name = name,
      arguments = arguments,
      extra = extra
    ))
  }
  if (type == "tool_result") {
    request <- fields$request
    if (!is.null(request)) {
      request <- history_plain_object(request, "Tool result request")
      if (!identical(request[["type"]], "tool_request")) {
        history_abort("Tool result request must contain a tool_request record")
      }
      request <- history_validate_content(request, state)
    }
    value <- history_validate_tool_result_value(fields$value, state)
    error <- history_validate_data(fields$error, state)
    extra <- history_validate_data(fields$extra, state)
    history_require_data_kind(
      error,
      "character",
      "Tool result error",
      allow_null = TRUE
    )
    history_require_data_kind(extra, "list", "Tool result extra")
    return(list(
      type = type,
      value = value,
      error = error,
      extra = extra,
      request = request
    ))
  }
  history_abort("Unsupported conversation content tag")
}

history_validate_data <- function(value, state, depth = 1L) {
  history_budget_node(state, depth)
  value <- history_plain_object(value, "History data value")
  tag <- history_plain_string(value[["tag"]], "History data tag")
  if (identical(tag, "null")) {
    fields <- history_object_fields(value, "tag", "History null value")
    return(list(tag = fields$tag))
  }
  if (identical(tag, "list")) {
    fields <- history_object_fields(
      value,
      c("tag", "names", "values"),
      "History list value"
    )
    values <- history_plain_array(fields$values, "History list values")
    names <- history_validate_names(fields$names, values, state)
    values <- lapply(
      values,
      history_validate_data,
      state = state,
      depth = depth + 1L
    )
    return(list(tag = tag, names = names, values = values))
  }
  if (identical(tag, "vector")) {
    fields <- history_object_fields(
      value,
      c("tag", "type", "names", "values"),
      "History vector value"
    )
    type <- history_plain_string(fields$type, "History vector type")
    if (!type %in% c("logical", "integer", "double", "character")) {
      history_abort("History vector has an unsupported atomic type")
    }
    values <- history_plain_array(fields$values, "History vector values")
    names <- history_validate_names(fields$names, values, state)
    values <- lapply(values, function(item) {
      history_budget_node(state, depth + 1L)
      item_fields <- history_object_fields(
        item,
        c("tag", "value"),
        "History vector element"
      )
      if (!identical(item_fields$tag, type)) {
        history_abort(
          "History vector element tag does not match its vector type"
        )
      }
      item_value <- item_fields$value
      if (is.null(item_value)) {
        return(list(tag = type, value = NULL))
      }
      if (type == "logical") {
        if (
          !is.null(attributes(item_value)) ||
            !is.logical(item_value) ||
            length(item_value) != 1L ||
            is.na(item_value)
        ) {
          history_abort("History logical value is invalid")
        }
      } else if (type == "character") {
        item_value <- history_plain_string(
          item_value,
          "History character value"
        )
        history_budget_text(state, item_value)
      } else {
        item_value <- history_plain_string(item_value, "History numeric value")
        if (type == "integer") {
          if (!grepl("^-?[0-9]+$", item_value)) {
            history_abort("History integer value is invalid")
          }
          numeric_value <- suppressWarnings(as.numeric(item_value))
          if (
            is.na(numeric_value) ||
              numeric_value < -2147483647 ||
              numeric_value > 2147483647
          ) {
            history_abort("History integer value is out of range")
          }
          item_value <- as.character(as.integer(numeric_value))
        } else {
          numeric_value <- history_parse_double(
            item_value,
            "History double value"
          )
          item_value <- formatC(
            numeric_value,
            digits = 17L,
            format = "g",
            decimal.mark = "."
          )
          history_budget_text(state, item_value)
        }
      }
      list(tag = type, value = item_value)
    })
    return(list(tag = tag, type = type, names = names, values = values))
  }
  history_abort(paste0("Unknown history data tag: ", tag))
}

history_validate_names <- function(names, values, state) {
  if (is.null(names)) {
    return(NULL)
  }
  names <- history_plain_array(names, "History names")
  value_names <- vapply(
    names,
    function(name) {
      history_plain_string(name, "History name", nonempty = TRUE)
    },
    character(1)
  )
  if (length(value_names) != length(values) || anyDuplicated(value_names)) {
    history_abort("History names must be unique and match the value count")
  }
  history_budget_text(state, value_names)
  unname(as.list(value_names))
}

history_parse_double <- function(value, label) {
  if (
    !is.null(attributes(value)) ||
      !is.character(value) ||
      length(value) != 1L ||
      is.na(value)
  ) {
    history_abort(paste0(label, " is invalid"))
  }
  parsed <- tryCatch(
    jsonlite::parse_json(value, simplifyVector = FALSE),
    error = function(error) NULL
  )
  if (
    is.null(parsed) ||
      !is.numeric(parsed) ||
      length(parsed) != 1L ||
      !is.null(attributes(parsed)) ||
      is.na(parsed) ||
      !is.finite(parsed)
  ) {
    history_abort(paste0(label, " is invalid"))
  }

  # JSON parses the integer token -0 as integer zero, losing its sign bit.
  if (identical(trimws(value), "-0")) {
    return(-0.0)
  }
  as.double(parsed)
}

history_data_string <- function(value, label, nonempty, state) {
  value <- history_validate_data(value, state)
  history_require_data_string(value, label, nonempty)
  value
}

history_require_data_string <- function(value, label, nonempty) {
  if (
    !identical(value$tag, "vector") ||
      !identical(value$type, "character") ||
      length(value$values) != 1L ||
      is.null(value$values[[1L]]$value) ||
      (nonempty && !nzchar(value$values[[1L]]$value))
  ) {
    history_abort(paste0(label, " must be a character scalar"))
  }
  invisible(NULL)
}

history_require_data_kind <- function(
  value,
  expected,
  label,
  allow_null = FALSE
) {
  if (identical(value$tag, "null") && allow_null) {
    return(invisible(NULL))
  }
  kind <- if (identical(value$tag, "list")) {
    "list"
  } else if (identical(value$tag, "vector")) {
    value$type
  } else {
    value$tag
  }
  if (!identical(kind, expected)) {
    history_abort(paste0(label, " must be ", expected))
  }
  invisible(NULL)
}

history_validate_tool_pairs <- function(turns) {
  requests <- list()
  results <- character()
  pending <- character()
  for (turn in turns) {
    if (identical(turn$role, "assistant") && length(pending) > 0L) {
      history_abort(
        "An assistant turn cannot follow unresolved tool requests"
      )
    }
    for (content in turn$contents) {
      if (identical(content$type, "tool_request")) {
        if (!identical(turn$role, "assistant")) {
          history_abort("Tool requests must appear in assistant turns")
        }
        id <- content$id
        if (id %in% names(requests)) {
          history_abort("Tool request IDs must be unique within a history")
        }
        requests[id] <- list(content)
        pending <- c(pending, id)
      } else if (identical(content$type, "tool_result")) {
        if (!identical(turn$role, "user")) {
          history_abort("Tool results must appear in user turns")
        }
        request <- content$request
        if (is.null(request)) {
          history_abort("Tool results must identify their originating request")
        }
        id <- request$id
        if (id %in% results) {
          history_abort("A tool request cannot have duplicate results")
        }
        if (!id %in% pending) {
          history_abort("Tool result has no preceding matching request")
        }
        if (!identical(requests[[id]], request)) {
          history_abort("Tool result does not match its originating request")
        }
        results <- c(results, id)
        pending <- setdiff(pending, id)
      }
    }
  }
  if (length(pending) > 0L) {
    history_abort("Conversation history contains an unresolved tool request")
  }
  invisible(NULL)
}

history_value_from_node <- function(node) {
  if (identical(node$tag, "null")) {
    return(NULL)
  }
  if (identical(node$tag, "list")) {
    values <- lapply(node$values, history_value_from_node)
    if (!is.null(node$names)) {
      names(values) <- vapply(node$names, identity, character(1))
    }
    return(values)
  }
  values <- lapply(node$values, function(item) {
    if (is.null(item$value)) {
      return(switch(
        node$type,
        logical = NA,
        integer = NA_integer_,
        double = NA_real_,
        character = NA_character_
      ))
    }
    switch(
      node$type,
      logical = item$value,
      integer = as.integer(item$value),
      double = history_parse_double(item$value, "History double value"),
      character = item$value
    )
  })
  value <- switch(
    node$type,
    logical = vapply(values, identity, logical(1)),
    integer = vapply(values, identity, integer(1)),
    double = vapply(values, identity, double(1)),
    character = vapply(values, identity, character(1))
  )
  if (!is.null(node$names)) {
    names(value) <- vapply(node$names, identity, character(1))
  }
  value
}

history_value_from_tool_result <- function(value) {
  if (identical(value$tag, "data")) {
    return(history_value_from_node(value$data))
  }
  if (identical(value$tag, "content")) {
    return(history_decode_content(value$content))
  }
  lapply(value$contents, history_decode_content)
}

history_decode_content <- function(content) {
  type <- content$type
  if (type == "text") {
    return(ellmer::ContentText(history_value_from_node(content$text)))
  }
  if (type == "json") {
    return(ellmer_content_json(
      data = history_value_from_node(content$data),
      string = history_value_from_node(content$string)
    ))
  }
  if (type == "thinking") {
    return(ellmer::ContentThinking(
      history_value_from_node(content$thinking),
      extra = history_value_from_node(content$extra)
    ))
  }
  if (type == "image_inline") {
    return(ellmer::ContentImageInline(
      type = history_value_from_node(content$media_type),
      data = history_value_from_node(content$data)
    ))
  }
  if (type == "image_remote") {
    return(ellmer::ContentImageRemote(
      url = history_value_from_node(content$url),
      detail = history_value_from_node(content$detail)
    ))
  }
  if (type == "pdf") {
    return(ellmer::ContentPDF(
      type = history_value_from_node(content$media_type),
      data = history_value_from_node(content$data),
      filename = history_value_from_node(content$filename),
      url = history_value_from_node(content$url)
    ))
  }
  if (type == "tool_request") {
    return(history_decode_tool_request(content))
  }
  if (type == "tool_result") {
    request <- if (is.null(content$request)) {
      NULL
    } else {
      history_decode_tool_request(content$request)
    }
    return(ellmer::ContentToolResult(
      value = history_value_from_tool_result(content$value),
      error = history_value_from_node(content$error),
      extra = history_value_from_node(content$extra),
      request = request
    ))
  }
  history_abort(paste0("Unknown conversation content tag: ", type))
}

history_decode_tool_request <- function(request) {
  ellmer::ContentToolRequest(
    id = request$id,
    name = request$name,
    arguments = history_value_from_node(request$arguments),
    tool = NULL,
    extra = history_value_from_node(request$extra)
  )
}

history_decode_turn <- function(turn) {
  contents <- lapply(turn$contents, history_decode_content)
  if (identical(turn$role, "user")) {
    return(ellmer::UserTurn(contents = contents))
  }
  ellmer::AssistantTurn(
    contents = contents,
    json = history_value_from_node(turn$json),
    tokens = c(NA_real_, NA_real_, NA_real_),
    cost = NA_real_,
    duration = NA_real_,
    finish_reason = history_value_from_node(turn$finish_reason)
  )
}

history_to_json <- function(history) {
  as.character(jsonlite::toJSON(
    unclass(history),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = FALSE
  ))
}

history_check_json_size <- function(history_or_json) {
  json <- if (is.character(history_or_json) && length(history_or_json) == 1L) {
    history_or_json
  } else {
    history_to_json(history_or_json)
  }
  if (nchar(json, type = "bytes") > history_limits()$json_bytes) {
    history_abort("Conversation history JSON exceeds the maximum file size")
  }
  invisible(NULL)
}

history_path <- function(path) {
  path <- history_plain_string(path, "History path", nonempty = TRUE)
  path
}

history_reject_duplicate_json_fields <- function(json) {
  chars <- tryCatch(
    strsplit(enc2utf8(json), "", fixed = TRUE)[[1L]],
    error = function(error) {
      history_abort("Conversation history JSON encoding is invalid")
    }
  )
  length_chars <- length(chars)
  if (length_chars == 0L) {
    history_abort("Conversation history is not valid JSON")
  }
  nodes <- 0L
  limits <- history_limits()

  skip_space <- function(index) {
    while (
      index <= length_chars && chars[[index]] %in% c(" ", "\t", "\r", "\n")
    ) {
      index <- index + 1L
    }
    index
  }

  parse_string <- function(index) {
    start <- index
    index <- index + 1L
    while (index <= length_chars) {
      if (chars[[index]] == "\\") {
        index <- index + 2L
      } else if (chars[[index]] == "\"") {
        token <- paste(chars[start:index], collapse = "")
        value <- jsonlite::fromJSON(token, simplifyVector = FALSE)
        return(list(value = value, next_index = index + 1L))
      } else {
        index <- index + 1L
      }
    }
    history_abort("Conversation history JSON contains an invalid string")
  }

  parse_value <- function(index, depth = 1L) {
    nodes <<- nodes + 1L
    if (
      nodes > limits$parser_nodes ||
        depth > 2L * limits$depth + 16L
    ) {
      history_abort("Conversation history JSON exceeds parser limits")
    }
    index <- skip_space(index)
    if (chars[[index]] == "\"") {
      return(parse_string(index)$next_index)
    }
    if (chars[[index]] == "{") {
      index <- skip_space(index + 1L)
      seen <- new.env(hash = TRUE, parent = emptyenv())
      if (chars[[index]] == "}") {
        return(index + 1L)
      }
      repeat {
        if (chars[[index]] != "\"") {
          history_abort("Conversation history JSON contains an invalid object")
        }
        parsed_key <- parse_string(index)
        key <- parsed_key$value
        binding <- paste0(
          "field_",
          jsonlite::base64_enc(charToRaw(enc2utf8(key)))
        )
        if (exists(binding, envir = seen, inherits = FALSE)) {
          history_abort("Conversation history JSON contains duplicate fields")
        }
        assign(binding, TRUE, envir = seen)
        index <- skip_space(parsed_key$next_index)
        index <- skip_space(index + 1L)
        index <- parse_value(index, depth + 1L)
        index <- skip_space(index)
        if (chars[[index]] == "}") {
          return(index + 1L)
        }
        index <- skip_space(index + 1L)
      }
    }
    if (chars[[index]] == "[") {
      index <- skip_space(index + 1L)
      if (chars[[index]] == "]") {
        return(index + 1L)
      }
      repeat {
        index <- parse_value(index, depth + 1L)
        index <- skip_space(index)
        if (chars[[index]] == "]") {
          return(index + 1L)
        }
        index <- skip_space(index + 1L)
      }
    }
    while (
      index <= length_chars &&
        !chars[[index]] %in% c(",", "]", "}", " ", "\t", "\r", "\n")
    ) {
      index <- index + 1L
    }
    index
  }

  tryCatch(
    parse_value(1L),
    error = function(error) {
      if (inherits(error, "dsprrr_history_error")) {
        stop(error)
      }
      history_abort("Conversation history JSON structure is invalid")
    }
  )
  invisible(NULL)
}
