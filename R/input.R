#' Create an input specification for a Signature
#'
#' @description
#' Create an input specification with flexible type notation.
#' Supports ellmer types, string shortcuts, or S7 classes for backward compatibility.
#'
#' @param name Character string naming the input
#' @param type Input type specification. Can be:
#'   - An ellmer type object (e.g., `type_string()`, `type_number()`)
#'   - A string shortcut (e.g., "string", "number", "boolean")
#'   - An S7 class (for backward compatibility)
#'   - NULL/missing (defaults to string type)
#' @param description Optional description of the input. When type is a string
#'   shortcut or NULL, this description will be passed to the ellmer type
#' @param ... Additional metadata for the input
#'
#' @return A list with class "dsprrr_input" containing the input specification
#'
#' @examples
#' # Using ellmer types (recommended for consistency with outputs)
#' input("text", ellmer::type_string())
#' input("age", ellmer::type_number())
#' input("active", ellmer::type_boolean())
#'
#' # Using string shortcuts (simple and readable)
#' input("text", "string")
#' input("count", "integer")
#' input("score", "number")
#'
#' # Type optional (defaults to string)
#' input("name")
#' input("name", description = "User's name")
#'
#' # With ellmer types for structured data
#' input("tags", ellmer::type_array(ellmer::type_string()))
#' input("status", ellmer::type_enum(c("pending", "active", "done")))
#'
#' @export
input <- function(name, type = NULL, description = NULL, ...) {
  dots <- list(...)
  type_explicit <- dots$.type_explicit
  dots$.type_explicit <- NULL

  if (is.null(type_explicit)) {
    type_explicit <- !missing(type) && !is.null(type)
  }

  # Normalize the type specification, passing description if needed
  normalized_type <- normalize_input_type(type, description)

  # Extract description from the type if not provided separately
  if (is.null(description) && !is.null(normalized_type@description)) {
    description <- normalized_type@description
  }

  # Create the input specification
  structure(
    c(list(
      name = name,
      type = normalized_type, # Store the ellmer type
      description = description,
      .type_explicit = isTRUE(type_explicit)
    ), dots),
    class = "dsprrr_input"
  )
}

#' Normalize input type specification
#' @noRd
normalize_input_type <- function(type, description = NULL) {
  # Load ellmer
  rlang::check_installed("ellmer", reason = "for type specifications")

  # Handle NULL/missing - default to string with description
  if (is.null(type)) {
    return(ellmer::type_string(description = description))
  }

  # If it's already an ellmer type, return as-is
  # (don't override its existing description)
  if (is_ellmer_type(type)) {
    return(type)
  }

  # If it's a string shortcut, convert to ellmer type with description
  if (is.character(type) && length(type) == 1) {
    return(string_to_ellmer_type(type, description))
  }

  # If it's an S7 class (backward compatibility), convert to ellmer
  # Check for common S7 classes by identity
  if (
    identical(type, S7::class_character) ||
      identical(type, S7::class_double) ||
      identical(type, S7::class_integer) ||
      identical(type, S7::class_logical) ||
      identical(type, S7::class_list) ||
      identical(type, S7::class_any) ||
      inherits(type, "S7_class")
  ) {
    return(s7_class_to_ellmer_type(type))
  }

  # Fallback to string type with description
  cli::cli_warn("Unknown type specification, defaulting to string")
  ellmer::type_string(description = description)
}

#' Convert string shortcut to ellmer type
#' @noRd
string_to_ellmer_type <- function(type_str, description = NULL) {
  type_str <- tolower(trimws(type_str))

  switch(
    type_str,
    # String types
    "string" = ellmer::type_string(description = description),
    "str" = ellmer::type_string(description = description),
    "text" = ellmer::type_string(description = description),
    "character" = ellmer::type_string(description = description),

    # Numeric types
    "number" = ellmer::type_number(description = description),
    "numeric" = ellmer::type_number(description = description),
    "float" = ellmer::type_number(description = description),
    "double" = ellmer::type_number(description = description),

    # Integer types
    "integer" = ellmer::type_integer(description = description),
    "int" = ellmer::type_integer(description = description),

    # Boolean types
    "boolean" = ellmer::type_boolean(description = description),
    "bool" = ellmer::type_boolean(description = description),
    "logical" = ellmer::type_boolean(description = description),

    # Array types (can't pass description to nested type)
    "array" = ellmer::type_array(
      items = ellmer::type_string(),
      description = description
    ),
    "list" = ellmer::type_array(
      items = ellmer::type_string(),
      description = description
    ),

    # Object type
    "object" = ellmer::type_object(.description = description),
    "dict" = ellmer::type_object(.description = description),

    # Default
    {
      cli::cli_warn("Unknown type '{type_str}', defaulting to string")
      ellmer::type_string(description = description)
    }
  )
}

#' Convert S7 class to ellmer type (for backward compatibility)
#' @noRd
s7_class_to_ellmer_type <- function(s7_class) {
  if (identical(s7_class, S7::class_character)) {
    return(ellmer::type_string())
  } else if (identical(s7_class, S7::class_double)) {
    return(ellmer::type_number())
  } else if (identical(s7_class, S7::class_integer)) {
    return(ellmer::type_integer())
  } else if (identical(s7_class, S7::class_logical)) {
    return(ellmer::type_boolean())
  } else if (identical(s7_class, S7::class_list)) {
    return(ellmer::type_array(items = ellmer::type_string()))
  } else {
    # Default for any other type
    return(ellmer::type_string())
  }
}

#' Convert ellmer type to S7 class (for internal use)
#' @noRd
type_to_s7_class <- function(ellmer_type) {
  if (inherits(ellmer_type, "ellmer::TypeBasic")) {
    # Check the specific type - S7 objects use @ not $
    type_name <- ellmer_type@type

    if (type_name == "string") {
      return(S7::class_character)
    } else if (type_name == "number") {
      return(S7::class_double)
    } else if (type_name == "integer") {
      return(S7::class_integer)
    } else if (type_name == "boolean") {
      return(S7::class_logical)
    }
  } else if (inherits(ellmer_type, "ellmer::TypeArray")) {
    return(S7::class_list)
  } else if (inherits(ellmer_type, "ellmer::TypeEnum")) {
    return(S7::class_character) # Enums are strings
  } else if (inherits(ellmer_type, "ellmer::TypeObject")) {
    return(S7::class_list)
  }

  # Default
  S7::class_any
}

#' Check if object is a dsprrr input
#' @noRd
is_dsprrr_input <- function(x) {
  inherits(x, "dsprrr_input")
}

#' Create typed input helpers for common cases
#' @name input_helpers
#' @param name Name of the input field
#' @param description Optional description of the input
#' @param ... Additional arguments passed to the type constructor
#' @param values For input_enum, the allowed values
#' @param item_type For input_array, the type of array items
#' @export
input_string <- function(name, description = NULL, ...) {
  input(
    name,
    type = ellmer::type_string(description = description, ...),
    description = description
  )
}

#' @rdname input_helpers
#' @export
input_number <- function(name, description = NULL, ...) {
  input(
    name,
    type = ellmer::type_number(description = description, ...),
    description = description
  )
}

#' @rdname input_helpers
#' @export
input_boolean <- function(name, description = NULL, ...) {
  input(
    name,
    type = ellmer::type_boolean(description = description, ...),
    description = description
  )
}

#' @rdname input_helpers
#' @export
input_integer <- function(name, description = NULL, ...) {
  input(
    name,
    type = ellmer::type_integer(description = description, ...),
    description = description
  )
}

#' @rdname input_helpers
#' @export
input_enum <- function(name, values, description = NULL, ...) {
  input(
    name,
    type = ellmer::type_enum(values = values, description = description, ...),
    description = description
  )
}

#' @rdname input_helpers
#' @export
input_array <- function(
  name,
  item_type = ellmer::type_string(),
  description = NULL,
  ...
) {
  input(
    name,
    type = ellmer::type_array(
      items = item_type,
      description = description,
      ...
    ),
    description = description
  )
}

#' @rdname input_helpers
#' @export
input_object <- function(name, ..., description = NULL) {
  input(
    name,
    type = ellmer::type_object(.description = description, ...),
    description = description
  )
}
