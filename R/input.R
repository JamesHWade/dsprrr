#' Describe one signature input
#'
#' @description
#' `input()` describes one input field for the explicit form of
#' [signature()]: its name, its type and an optional description.
#'
#' @param name The field name. Callers pass the value under this name, as in
#'   `run(mod, review = "...")`.
#' @param type An ellmer type, one of the labels `"string"`, `"number"`,
#'   `"integer"`, `"boolean"`, `"array"` (an array of strings) or `"object"`,
#'   or `NULL` for a string.
#' @param description Optional description. Unless the module has its own
#'   template, it is written above the value in the prompt, as
#'   `# description`. With a label or `NULL` `type`, it also becomes the
#'   ellmer type's description.
#' @param ... Extra fields stored in the specification.
#'
#' @return A list of class `dsprrr_input` with elements `name`, `type` (an
#'   ellmer type) and `description`.
#'
#' @family signatures
#' @examples
#' review <- input("review", description = "A customer review")
#' stars <- input("stars", "integer")
#' tags <- input("tags", ellmer::type_array(ellmer::type_string()))
#'
#' # Inputs make up the explicit form of a signature
#' signature(
#'   inputs = list(review, stars, tags),
#'   output_type = ellmer::type_object(summary = ellmer::type_string())
#' )
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
