#' Create an input specification for a Signature
#'
#' @description
#' Create an input specification using an ellmer type or a canonical type label.
#'
#' @param name Character string naming the input
#' @param type An ellmer type object, one of `"string"`, `"number"`,
#'   `"integer"`, `"boolean"`, `"array"`, or `"object"`, or `NULL` to use
#'   a string type.
#' @param description Optional description of the input. When `type` is a
#'   canonical label or `NULL`, this description is passed to the ellmer type.
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
#' # Using canonical labels
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

  if ("class" %in% names(dots)) {
    cli::cli_abort(c(
      "{.arg class} is not a supported input specification",
      "i" = paste0(
        "Pass an ellmer type or a canonical type label via {.arg type}: ",
        "{.val string}, {.val number}, {.val integer}, {.val boolean}, ",
        "{.val array}, or {.val object}."
      )
    ))
  }

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
    c(
      list(
        name = name,
        type = normalized_type, # Store the ellmer type
        description = description,
        .type_explicit = isTRUE(type_explicit)
      ),
      dots
    ),
    class = "dsprrr_input"
  )
}

#' Normalize input type specification
#' @noRd
normalize_input_type <- function(type, description = NULL) {
  if (is.null(type)) {
    return(ellmer::type_string(description = description))
  }

  if (is_ellmer_type(type)) {
    return(type)
  }

  if (
    is.character(type) &&
      length(type) == 1L &&
      !is.na(type) &&
      type %in% canonical_input_types()
  ) {
    return(string_to_ellmer_type(type, description))
  }

  cli::cli_abort(c(
    "Unsupported {.arg type} for {.fn input}",
    "i" = paste0(
      "Use an ellmer type or exactly one of: ",
      "{.val {canonical_input_types()}}."
    )
  ))
}

#' Return canonical string labels accepted by input()
#' @noRd
canonical_input_types <- function() {
  c("string", "number", "integer", "boolean", "array", "object")
}

#' Convert a canonical string label to an ellmer type
#' @noRd
string_to_ellmer_type <- function(type_str, description = NULL) {
  switch(
    type_str,
    "string" = ellmer::type_string(description = description),
    "number" = ellmer::type_number(description = description),
    "integer" = ellmer::type_integer(description = description),
    "boolean" = ellmer::type_boolean(description = description),
    "array" = ellmer::type_array(
      items = ellmer::type_string(),
      description = description
    ),
    "object" = ellmer::type_object(.description = description)
  )
}

#' Check if object is a dsprrr input
#' @noRd
is_dsprrr_input <- function(x) {
  inherits(x, "dsprrr_input")
}
