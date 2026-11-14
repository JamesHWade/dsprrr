#' Validate ReAct finalization configuration
#' @noRd
validate_react_finalization <- function(value) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !value %in% c("structured", "submit")
  ) {
    cli::cli_abort(
      "{.arg finalization} must be {.val structured} or {.val submit}",
      class = "dsprrr_react_finalization_error"
    )
  }
  value
}

#' Reject user tools that collide with the reserved submit tool
#' @noRd
validate_react_submit_tool_names <- function(user_tools, chat_tools = list()) {
  user_names <- if (length(user_tools) == 0L) {
    character()
  } else {
    vapply(user_tools, function(tool) tool@name, character(1))
  }
  chat_names <- if (length(chat_tools) == 0L) {
    character()
  } else {
    vapply(chat_tools, function(tool) tool@name, character(1))
  }
  if ("submit" %in% c(user_names, chat_names)) {
    cli::cli_abort(
      c(
        "The tool name {.val submit} is reserved for ReAct finalization",
        "i" = "Rename the user-defined tool before using {.arg finalization = \"submit\"}."
      ),
      class = "dsprrr_react_submit_tool_conflict"
    )
  }
  invisible(NULL)
}

#' Build the submit tool from a ReAct output schema
#' @noRd
react_submit_tool <- function(output_type, validation_state = NULL) {
  if (rlm_type_contains_json_schema(output_type)) {
    cli::cli_abort(
      c(
        "ReAct submit finalization cannot validate opaque JSON schemas",
        "i" = paste(
          "Use ellmer type_string(), type_number(), type_integer(),",
          "type_boolean(), type_enum(), type_array(), or type_object()."
        )
      ),
      class = "dsprrr_react_submit_schema_error"
    )
  }

  bare_output <- !inherits(output_type, "ellmer::TypeObject")
  if (is.null(validation_state)) {
    validation_state <- new.env(parent = emptyenv())
  }
  validation_state$invalid_submit <- NULL
  arguments <- if (bare_output) {
    list(value = output_type)
  } else {
    properties <- as.list(output_type@properties)
    properties[
      !vapply(properties, inherits, logical(1), "ellmer::TypeIgnore")
    ]
  }

  argument_names <- names(arguments) %||% character()
  if (
    !all(nzchar(argument_names)) ||
      anyDuplicated(argument_names) ||
      "..." %in% argument_names
  ) {
    cli::cli_abort(
      "The ReAct output schema cannot be represented as submit tool arguments",
      class = "dsprrr_react_submit_schema_error"
    )
  }

  submit_impl <- function(values) {
    invalid_submit <- validation_state$invalid_submit
    validation_state$invalid_submit <- NULL
    if (inherits(invalid_submit, "dsprrr_react_submit_validation_error")) {
      ellmer::tool_reject(paste(
        "The submitted output does not match the declared schema:",
        conditionMessage(invalid_submit),
        "Correct the output and call submit again."
      ))
    }
    tryCatch(
      react_submit_validate(
        values,
        output_type,
        bare_output,
        json_shape = FALSE
      ),
      dsprrr_react_submit_validation_error = function(error) {
        ellmer::tool_reject(paste(
          "The submitted output does not match the declared schema:",
          conditionMessage(error),
          "Correct the output and call submit again."
        ))
      }
    )
    "Submission accepted."
  }

  tool_env <- new.env(parent = environment())
  tool_env$.react_submit_impl <- submit_impl
  # Let the validator report missing required values as a repairable tool
  # error instead of letting R reject a missing function argument first.
  tool_formals <- as.pairlist(stats::setNames(
    rep(list(NULL), length(arguments)),
    argument_names
  ))
  tool_fun <- rlang::new_function(
    tool_formals,
    quote(.react_submit_impl(as.list(environment()))),
    env = tool_env
  )

  ellmer::tool(
    fun = tool_fun,
    name = "submit",
    description = paste(
      "Submit the final answer that satisfies the declared output schema.",
      "Call this tool once when the answer is ready."
    ),
    arguments = arguments
  )
}

#' Prepare a Chat for one submit-finalized ReAct call
#' @noRd
react_submit_prepare <- function(chat, user_tools, output_type) {
  required_methods <- c(
    "get_tools",
    "set_tools",
    "register_tool",
    "on_request_end",
    "get_turns",
    "set_turns"
  )
  missing_methods <- required_methods[
    !vapply(
      required_methods,
      function(name) is.function(chat[[name]]),
      logical(1)
    )
  ]
  if (length(missing_methods) > 0L) {
    cli::cli_abort(
      c(
        "The ellmer Chat cannot run ReAct submit finalization",
        "x" = "Missing public Chat methods: {.field {missing_methods}}",
        "i" = "Use an ellmer Chat with tool and request-end callback support."
      ),
      class = "dsprrr_react_submit_chat_error"
    )
  }

  original_tools <- chat$get_tools()
  if (!is.list(original_tools)) {
    cli::cli_abort(
      "The ellmer Chat returned an invalid tool registry",
      class = "dsprrr_react_submit_chat_error"
    )
  }
  validate_react_submit_tool_names(user_tools, original_tools)
  validation_state <- new.env(parent = emptyenv())
  submit_tool <- react_submit_tool(output_type, validation_state)

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
  chat$register_tool(submit_tool)
  prepared <- TRUE

  list(
    original_tools = original_tools,
    submit_tool = submit_tool,
    validation_state = validation_state
  )
}

#' Restore Chat state changed for submit finalization
#' @noRd
react_submit_cleanup <- function(chat, prepared, remove_callback = NULL) {
  callback_error <- NULL
  if (is.function(remove_callback)) {
    tryCatch(
      remove_callback(),
      error = function(error) {
        callback_error <<- error
      }
    )
  }
  registry_error <- tryCatch(
    {
      chat$set_tools(prepared$original_tools)
      NULL
    },
    error = identity
  )
  if (!is.null(callback_error)) {
    stop(callback_error)
  }
  if (!is.null(registry_error)) {
    stop(registry_error)
  }
  invisible(NULL)
}

#' Register a submit callback and validate its remover
#' @noRd
react_submit_on_request_end <- function(chat, callback) {
  remove_callback <- chat$on_request_end(callback)
  if (!is.function(remove_callback)) {
    cli::cli_abort(
      "The ellmer Chat did not return a request-end callback remover",
      class = "dsprrr_react_submit_chat_error"
    )
  }
  remove_callback
}

#' Check JSON array and object shapes against a submit schema
#' @noRd
react_submit_validate_json_shape <- function(value, type, context, class) {
  if (is.null(value) || inherits(type, "ellmer::TypeIgnore")) {
    return(invisible(NULL))
  }

  if (inherits(type, "ellmer::TypeArray")) {
    if (
      !is.list(value) ||
        is.data.frame(value) ||
        !is.null(names(value))
    ) {
      cli::cli_abort(
        "{context} must be a JSON array",
        class = class
      )
    }
    for (index in seq_along(value)) {
      react_submit_validate_json_shape(
        value[[index]],
        type@items,
        paste0(context, "[[", index, "]]"),
        class
      )
    }
    return(invisible(NULL))
  }

  if (inherits(type, "ellmer::TypeObject")) {
    fields <- names(value)
    if (
      !is.list(value) ||
        is.data.frame(value) ||
        is.null(fields)
    ) {
      cli::cli_abort(
        "{context} must be a JSON object",
        class = class
      )
    }
    properties <- as.list(type@properties)
    for (field in fields) {
      if (field %in% names(properties)) {
        react_submit_validate_json_shape(
          value[[field]],
          properties[[field]],
          paste0(context, "$", field),
          class
        )
      }
    }
  }
  invisible(NULL)
}

#' Check nested object names before ignored properties can be stripped
#' @noRd
react_submit_validate_object_names <- function(
  value,
  type,
  context,
  class
) {
  if (is.null(value) || inherits(type, "ellmer::TypeIgnore")) {
    return(invisible(NULL))
  }

  properties <- if (inherits(type, "ellmer::TypeObject")) {
    as.list(type@properties)
  } else {
    NULL
  }
  item_type <- if (inherits(type, "ellmer::TypeArray")) {
    type@items
  } else {
    NULL
  }

  validate_names <- function(fields, object_context) {
    if (anyNA(fields) || !all(nzchar(fields)) || anyDuplicated(fields)) {
      cli::cli_abort(
        "{object_context} must have unique, non-empty field names",
        class = class
      )
    }
  }

  if (is.data.frame(value)) {
    fields <- names(value) %||% character()
    validate_names(fields, context)
    if (
      inherits(type, "ellmer::TypeArray") &&
        inherits(item_type, "ellmer::TypeObject")
    ) {
      for (index in seq_len(nrow(value))) {
        row <- lapply(value, function(column) column[[index]])
        names(row) <- fields
        react_submit_validate_object_names(
          row,
          item_type,
          paste0(context, "[[", index, "]]"),
          class
        )
      }
    } else if (inherits(type, "ellmer::TypeObject")) {
      for (field in fields) {
        property <- properties[[field]]
        react_submit_validate_object_names(
          value[[field]],
          property,
          paste0(context, "$", field),
          class
        )
      }
    }
    return(invisible(NULL))
  }

  if (!is.list(value)) {
    return(invisible(NULL))
  }
  fields <- names(value)
  if (!is.null(fields)) {
    validate_names(fields, context)
  }

  for (index in seq_along(value)) {
    field <- if (is.null(fields)) "" else fields[[index]]
    child_type <- if (!is.null(item_type)) {
      item_type
    } else if (
      !is.null(properties) &&
        nzchar(field) &&
        field %in% names(properties)
    ) {
      properties[[field]]
    } else {
      NULL
    }
    child_context <- if (nzchar(field)) {
      paste0(context, "$", field)
    } else {
      paste0(context, "[[", index, "]]")
    }
    react_submit_validate_object_names(
      value[[index]],
      child_type,
      child_context,
      class
    )
  }
  invisible(NULL)
}

#' Validate a submit request against the declared output schema
#' @noRd
react_submit_validate <- function(
  arguments,
  output_type,
  bare_output,
  json_shape = TRUE
) {
  context <- "ReAct submit output"
  class <- "dsprrr_react_submit_validation_error"
  if (
    isTRUE(json_shape) &&
      (is.null(arguments) ||
        !is.list(arguments) ||
        is.data.frame(arguments) ||
        is.null(names(arguments)))
  ) {
    cli::cli_abort(
      "{context} arguments must be a JSON object",
      class = class
    )
  }
  if (is.null(arguments)) {
    arguments <- list()
  }
  if (bare_output) {
    argument_names <- names(arguments) %||% character()
    if (
      !is.list(arguments) ||
        is.data.frame(arguments) ||
        (length(arguments) > 0L && is.null(names(arguments))) ||
        !all(nzchar(argument_names)) ||
        anyDuplicated(argument_names) ||
        length(setdiff(argument_names, "value")) > 0L
    ) {
      cli::cli_abort(
        "{context} must contain only one {.field value} argument",
        class = class
      )
    }
    if (isTRUE(output_type@required) && !"value" %in% argument_names) {
      cli::cli_abort(
        "{context} omitted the required {.field value} argument",
        class = class
      )
    }
    value <- if ("value" %in% argument_names) {
      arguments[["value"]]
    } else {
      NULL
    }
    react_submit_validate_object_names(
      value,
      output_type,
      paste0(context, "$value"),
      class
    )
    if (isTRUE(json_shape)) {
      react_submit_validate_json_shape(
        value,
        output_type,
        paste0(context, "$value"),
        class
      )
    }
    value <- flex_strip_ignored_runtime_values(
      value,
      output_type,
      context,
      class
    )
    flex_validate_runtime_value(
      value,
      output_type,
      context,
      class
    )
    return(value)
  }

  react_submit_validate_object_names(
    arguments,
    output_type,
    context,
    class
  )
  if (isTRUE(json_shape)) {
    react_submit_validate_json_shape(
      arguments,
      output_type,
      context,
      class
    )
  }
  value <- flex_strip_ignored_runtime_values(
    arguments,
    output_type,
    context,
    class
  )
  properties <- as.list(output_type@properties)
  visible_properties <- properties[
    !vapply(properties, inherits, logical(1), "ellmer::TypeIgnore")
  ]
  if (length(visible_properties) == 0L && length(value) == 0L) {
    value <- stats::setNames(list(), character())
  }
  flex_validate_output_record(
    value = value,
    output_types = visible_properties,
    context = context,
    class = class,
    scalar = FALSE
  )
}

#' Create a typed signal for an accepted submit request
#' @noRd
react_submit_completed <- function(request, value, invocation_marker) {
  rlang::abort(
    "ReAct submit request accepted",
    class = "dsprrr_react_submit_completed",
    request = request,
    value = value,
    invocation_marker = invocation_marker
  )
}

#' Append the paired tool result using ellmer's public Chat API
#' @noRd
react_submit_complete <- function(chat, request, value) {
  result <- ellmer::ContentToolResult(
    request = request,
    value = as.character(jsonlite::toJSON(
      value,
      auto_unbox = TRUE,
      null = "null",
      dataframe = "rows"
    ))
  )
  turns <- chat$get_turns()
  chat$set_turns(c(
    turns,
    list(ellmer::UserTurn(contents = list(result)))
  ))
  invisible(result)
}
