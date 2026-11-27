# Shared ellmer::Type schema introspection and runtime value validation

rlm_type_contains_json_schema <- function(type) {
  if (inherits(type, "ellmer::TypeJsonSchema")) {
    return(TRUE)
  }
  if (inherits(type, "ellmer::TypeArray")) {
    return(rlm_type_contains_json_schema(type@items))
  }
  if (inherits(type, "ellmer::TypeObject")) {
    return(any(vapply(
      type@properties,
      rlm_type_contains_json_schema,
      logical(1)
    )))
  }
  FALSE
}


flex_strip_ignored_runtime_values <- function(
  value,
  type,
  context,
  class,
  kind = "output"
) {
  if (is.null(value) || inherits(type, "ellmer::TypeIgnore")) {
    return(value)
  }
  if (inherits(type, "ellmer::TypeArray")) {
    if (is.data.frame(value) && inherits(type@items, "ellmer::TypeObject")) {
      return(flex_strip_ignored_runtime_values(
        value,
        type@items,
        context,
        class,
        kind
      ))
    }
    if (is.list(value) && is.null(names(value))) {
      return(lapply(seq_along(value), function(index) {
        flex_strip_ignored_runtime_values(
          value[[index]],
          type@items,
          paste0(context, "[[", index, "]]"),
          class,
          kind
        )
      }))
    }
    return(value)
  }
  if (!inherits(type, "ellmer::TypeObject")) {
    return(value)
  }

  properties <- as.list(type@properties)
  ignored <- names(properties)[vapply(
    properties,
    inherits,
    logical(1),
    "ellmer::TypeIgnore"
  )]
  if (is.data.frame(value)) {
    for (field in intersect(ignored, names(value))) {
      column <- value[[field]]
      non_null <- if (is.list(column)) {
        vapply(column, Negate(is.null), logical(1))
      } else {
        rep(TRUE, length(column))
      }
      if (any(non_null)) {
        flex_runtime_validation_abort(
          "{context}${field} is ignored and must be NULL",
          class,
          kind,
          context,
          field = field
        )
      }
    }
    value <- value[setdiff(names(value), ignored)]
    for (field in intersect(names(properties), names(value))) {
      property <- properties[[field]]
      column <- value[[field]]
      if (inherits(property, "ellmer::TypeObject") && is.data.frame(column)) {
        value[[field]] <- flex_strip_ignored_runtime_values(
          column,
          property,
          paste0(context, "$", field),
          class,
          kind
        )
      } else if (inherits(property, "ellmer::TypeArray") && is.list(column)) {
        value[[field]] <- lapply(seq_along(column), function(index) {
          flex_strip_ignored_runtime_values(
            column[[index]],
            property,
            paste0(context, "$", field, "[[", index, "]]"),
            class,
            kind
          )
        })
      }
    }
    return(value)
  }
  if (!is.list(value) || (length(value) > 0L && is.null(names(value)))) {
    return(value)
  }
  for (field in intersect(ignored, names(value))) {
    if (!is.null(value[[field]])) {
      flex_runtime_validation_abort(
        "{context}${field} is ignored and must be NULL",
        class,
        kind,
        context,
        field = field
      )
    }
  }
  value <- value[setdiff(names(value), ignored)]
  for (field in intersect(names(properties), names(value))) {
    value[field] <- list(flex_strip_ignored_runtime_values(
      value[[field]],
      properties[[field]],
      paste0(context, "$", field),
      class,
      kind
    ))
  }
  if (length(value) == 0L) {
    names(value) <- character()
  }
  value
}

flex_data_frame_object_row <- function(
  value,
  index,
  type,
  context,
  class,
  kind
) {
  properties <- as.list(type@properties)
  ignored <- names(properties)[vapply(
    properties,
    inherits,
    logical(1),
    "ellmer::TypeIgnore"
  )]
  visible <- properties[setdiff(names(properties), ignored)]
  required <- names(visible)[vapply(
    visible,
    function(property) isTRUE(property@required),
    logical(1)
  )]
  missing <- setdiff(required, names(value))
  extra <- setdiff(names(value), names(properties))
  if (length(missing) > 0L) {
    flex_runtime_validation_abort(
      "{context} omitted required fields: {.field {missing}}",
      class,
      kind,
      context,
      missing = missing
    )
  }
  if (length(extra) > 0L && !isTRUE(type@additional_properties)) {
    flex_runtime_validation_abort(
      "{context} included unknown fields: {.field {extra}}",
      class,
      kind,
      context,
      extra = extra
    )
  }
  for (field in intersect(names(visible), names(value))) {
    property <- visible[[field]]
    column <- value[[field]]
    if (inherits(property, "ellmer::TypeObject") && is.data.frame(column)) {
      if (
        !isTRUE(property@required) &&
          isTRUE(ellmer_parallel_object_row_missing(column, index, property))
      ) {
        next
      }
      flex_data_frame_object_row(
        column,
        index,
        property,
        paste0(context, "$", field),
        class,
        kind
      )
      next
    }
    if (index > length(column)) {
      flex_runtime_validation_abort(
        "{context}${field} ended before row {index}",
        class,
        kind,
        context,
        field = field,
        index = index
      )
    }
    cell <- column[[index]]
    if (
      !isTRUE(property@required) &&
        length(cell) == 1L &&
        is.atomic(cell) &&
        is.na(cell)
    ) {
      cell <- NULL
    }
    flex_validate_runtime_value(
      cell,
      property,
      paste0(context, "$", field),
      class,
      kind = kind
    )
  }
  visible_type <- type
  visible_type@properties <- visible
  tryCatch(
    ellmer_parallel_response_row(value, index, visible_type),
    error = function(error) {
      flex_runtime_validation_abort(
        "Could not reconstruct {context}",
        class,
        kind,
        context,
        parent = error
      )
    }
  )
}

flex_validate_runtime_value <- function(
  value,
  type,
  context,
  class,
  kind = c("output", "input")
) {
  kind <- match.arg(kind)
  abort_value <- function(message, ...) {
    caller_env <- parent.frame()
    flex_runtime_validation_abort(
      message,
      class = class,
      kind = kind,
      context = context,
      ...,
      .envir = caller_env
    )
  }
  if (inherits(type, "ellmer::TypeIgnore")) {
    return(invisible(NULL))
  }
  required <- tryCatch(isTRUE(type@required), error = function(error) TRUE)
  if (is.null(value)) {
    if (required) {
      abort_value("{context} is required and cannot be NULL")
    }
    return(invisible(NULL))
  }

  if (inherits(type, "ellmer::TypeBasic")) {
    type_name <- type@type
    valid <- switch(
      type_name,
      string = is.character(value) && length(value) == 1L && !is.na(value),
      number = is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value),
      integer = is.numeric(value) &&
        length(value) == 1L &&
        !is.na(value) &&
        is.finite(value) &&
        value == floor(value),
      boolean = is.logical(value) && length(value) == 1L && !is.na(value),
      FALSE
    )
    if (!valid) {
      abort_value(
        "{context} must be one non-missing {.val {type_name}} value",
        type_name = type_name
      )
    }
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeEnum")) {
    if (
      !is.character(value) ||
        length(value) != 1L ||
        is.na(value) ||
        !value %in% type@values
    ) {
      allowed <- type@values
      abort_value(
        "{context} must be one of {.val {allowed}}",
        allowed = allowed
      )
    }
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeArray")) {
    if (is.data.frame(value)) {
      if (!inherits(type@items, "ellmer::TypeObject")) {
        abort_value("{context} must be an array, not a data frame")
      }
      for (index in seq_len(nrow(value))) {
        record <- flex_data_frame_object_row(
          value,
          index,
          type@items,
          paste0(context, "[[", index, "]]"),
          class,
          kind
        )
        flex_validate_runtime_value(
          record,
          type@items,
          paste0(context, "[[", index, "]]"),
          class,
          kind = kind
        )
      }
      return(invisible(NULL))
    }
    if (
      !(is.atomic(value) || is.list(value)) ||
        (!is.null(names(value)) && any(nzchar(names(value))))
    ) {
      abort_value("{context} must be an unnamed array")
    }
    for (index in seq_along(value)) {
      flex_validate_runtime_value(
        value[[index]],
        type@items,
        paste0(context, "[[", index, "]]"),
        class,
        kind = kind
      )
    }
    return(invisible(NULL))
  }
  if (inherits(type, "ellmer::TypeObject")) {
    if (!is.list(value) || is.data.frame(value) || is.null(names(value))) {
      abort_value("{context} must be a named object")
    }
    fields <- names(value)
    if (!all(nzchar(fields)) || anyDuplicated(fields)) {
      abort_value("{context} must have unique, non-empty field names")
    }
    properties <- as.list(type@properties)
    properties <- properties[
      !vapply(
        properties,
        inherits,
        logical(1),
        "ellmer::TypeIgnore"
      )
    ]
    required_fields <- names(properties)[vapply(
      properties,
      function(property) isTRUE(property@required),
      logical(1)
    )]
    missing <- setdiff(required_fields, fields)
    if (length(missing) > 0L) {
      abort_value(
        "{context} omitted required fields: {.field {missing}}",
        missing = missing
      )
    }
    extra <- setdiff(fields, names(properties))
    if (length(extra) > 0L && !isTRUE(type@additional_properties)) {
      abort_value(
        "{context} included unknown fields: {.field {extra}}",
        extra = extra
      )
    }
    for (field in intersect(names(properties), fields)) {
      flex_validate_runtime_value(
        value[[field]],
        properties[[field]],
        paste0(context, "$", field),
        class,
        kind = kind
      )
    }
    return(invisible(NULL))
  }

  abort_value("{context} uses an unsupported runtime type")
}

flex_validate_output_record <- function(
  value,
  output_types,
  context,
  class,
  scalar = TRUE
) {
  fields <- names(output_types)
  if (scalar && length(fields) == 1L) {
    value <- stats::setNames(list(value), fields)
  } else if (
    scalar &&
      length(fields) == 0L &&
      (is.null(value) || (is.list(value) && length(value) == 0L))
  ) {
    value <- stats::setNames(list(), character())
  }
  if (!is.list(value) || is.null(names(value))) {
    flex_output_abort(
      "{context} must be a named output record",
      class = class
    )
  }
  actual <- names(value)
  if (!all(nzchar(actual)) || anyDuplicated(actual)) {
    flex_output_abort(
      "{context} must have unique, non-empty output names",
      class = class
    )
  }
  required_fields <- fields[vapply(
    output_types,
    function(type) isTRUE(type@required),
    logical(1)
  )]
  missing <- setdiff(required_fields, actual)
  extra <- setdiff(actual, fields)
  if (length(missing) > 0L || length(extra) > 0L) {
    messages <- c("{context} does not match its declared output fields")
    if (length(missing) > 0L) {
      messages <- c(messages, "x" = "Missing required: {.field {missing}}.")
    }
    if (length(extra) > 0L) {
      messages <- c(messages, "x" = "Unknown: {.field {extra}}.")
    }
    flex_output_abort(messages, class = class)
  }
  present <- intersect(fields, actual)
  value <- value[present]
  for (field in present) {
    flex_validate_runtime_value(
      value[[field]],
      output_types[[field]],
      paste0(context, "$", field),
      class
    )
  }
  value
}

flex_output_record <- function(value, output_types, step_name, output_type) {
  context <- paste0("Flex step ", step_name, " output")
  value <- flex_strip_ignored_runtime_values(
    value,
    output_type,
    context,
    class = "dsprrr_flex_step_output_error"
  )
  flex_validate_output_record(
    value,
    output_types,
    context = context,
    class = "dsprrr_flex_step_output_error",
    scalar = !inherits(output_type, "ellmer::TypeObject")
  )
}
