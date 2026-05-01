#' Convert a Signature to JSON Schema
#'
#' @description
#' Converts a dsprrr [Signature] output contract to a plain R list matching JSON
#' Schema. This is useful for runtimes that accept JSON schema structured output
#' definitions, including agent runtimes built on ellmer-compatible contracts.
#'
#' @param signature A [Signature] object or signature string.
#'
#' @return A named list containing a JSON Schema representation of the signature
#'   output type.
#' @export
#'
#' @examples
#' schema <- signature_to_json_schema(
#'   "question -> answer, confidence: number, citations: array(string)"
#' )
signature_to_json_schema <- function(signature) {
  if (is.character(signature)) {
    signature <- parse_signature(signature)
  }

  if (!inherits(signature, "dsprrr::Signature")) {
    cli::cli_abort("{.arg signature} must be a Signature object or string")
  }

  ellmer_type_to_json_schema(signature@output_type)
}

#' Convert an ellmer type object to JSON Schema
#' @noRd
ellmer_type_to_json_schema <- function(type) {
  schema <- if (inherits(type, "ellmer::TypeBasic")) {
    list(type = type@type)
  } else if (inherits(type, "ellmer::TypeEnum")) {
    list(type = "string", enum = type@values)
  } else if (inherits(type, "ellmer::TypeArray")) {
    list(
      type = "array",
      items = ellmer_type_to_json_schema(type@items)
    )
  } else if (inherits(type, "ellmer::TypeObject")) {
    properties <- lapply(type@properties, ellmer_type_to_json_schema)
    required <- names(type@properties)[
      vapply(type@properties, function(prop) isTRUE(prop@required), logical(1))
    ]
    list(
      type = "object",
      properties = properties,
      required = required,
      additionalProperties = isTRUE(type@additional_properties)
    )
  } else {
    cli::cli_warn(c(
      "Unsupported ellmer type in signature schema conversion",
      "i" = "Falling back to {.code {\"type\": \"string\"}} for {.cls {class(type)[1]}}"
    ))
    list(type = "string")
  }

  schema <- add_ellmer_schema_description(schema, type)
  schema
}

#' Add an ellmer description slot to a JSON schema fragment
#' @noRd
add_ellmer_schema_description <- function(schema, type) {
  description <- tryCatch(type@description, error = function(e) NULL)
  if (!is.null(description) && nzchar(description)) {
    schema$description <- description
  }
  schema
}

#' Return output field names for object-shaped output types
#' @noRd
output_field_names <- function(output_type) {
  if (inherits(output_type, "ellmer::TypeObject")) {
    names(output_type@properties)
  } else {
    character()
  }
}
