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
  if (inherits(type, "ellmer::TypeIgnore")) {
    return(NULL)
  }

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
    properties <- properties[!vapply(properties, is.null, logical(1))]
    required <- names(type@properties)[
      vapply(
        type@properties,
        function(prop) {
          isTRUE(prop@required) && !inherits(prop, "ellmer::TypeIgnore")
        },
        logical(1)
      )
    ]
    list(
      type = "object",
      properties = properties,
      required = required,
      additionalProperties = isTRUE(type@additional_properties)
    )
  } else {
    cli::cli_abort(c(
      "Unsupported ellmer type in signature schema conversion",
      "x" = "Cannot convert {.cls {class(type)[1]}} to JSON Schema",
      "i" = "Supported types: TypeBasic, TypeEnum, TypeArray, TypeObject, TypeIgnore"
    ))
  }

  add_ellmer_schema_description(schema, type)
}

#' Add an ellmer description slot to a JSON schema fragment
#' @noRd
add_ellmer_schema_description <- function(schema, type) {
  description <- type@description
  if (length(description) > 0 && nzchar(description)) {
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
