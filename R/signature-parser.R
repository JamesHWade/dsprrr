#' Parse DSPy-style signature string (Internal)
#'
#' @description
#' Internal function that parses a compact string notation for signatures.
#' Users should use `signature()` instead.
#'
#' @param signature_str Character string in DSPy format
#' @param instructions Optional instructions for the signature
#'
#' @return A Signature object
#' @keywords internal
#' @noRd
parse_signature <- function(signature_str, instructions = "") {
  # Split by arrow
  parts <- strsplit(signature_str, "\\s*->\\s*")[[1]]

  if (length(parts) != 2) {
    cli::cli_abort(
      "Signature must have format: 'inputs -> output' or 'inputs -> output: type'"
    )
  }

  # Parse inputs
  inputs_str <- trimws(parts[1])
  inputs <- parse_inputs(inputs_str)

  # Parse output
  output_str <- trimws(parts[2])
  output_type <- parse_output(output_str)

  # Generate default instructions if none provided (similar to DSPy)
  if (nchar(instructions) == 0) {
    # Get input field names
    input_names <- if (length(inputs) > 0) {
      paste0(
        "`",
        vapply(inputs, function(x) x$name, character(1)),
        "`",
        collapse = ", "
      )
    } else {
      "no inputs"
    }

    # Get output field names from the output string
    # For simple cases like "sentiment", use that
    # For typed cases like "sentiment: string", extract the field name
    # For multiple outputs like "sentiment, issues: array(string)", parse them
    output_field_names <- if (grepl(",", output_str)) {
      # Multiple outputs - extract field names
      fields <- split_respecting_nesting(output_str, ",")
      field_names <- character()
      for (field in fields) {
        # Extract name before colon if present
        name <- trimws(gsub(":.*", "", field))
        field_names <- c(field_names, name)
      }
      paste0("`", field_names, "`", collapse = ", ")
    } else {
      # Single output - extract name before colon if present
      name <- trimws(gsub(":.*", "", output_str))
      paste0("`", name, "`")
    }

    # Generate instructions similar to DSPy
    instructions <- paste0(
      "Given the fields ",
      input_names,
      ", produce the fields ",
      output_field_names,
      "."
    )
  }

  # Create Signature
  Signature(
    inputs = inputs,
    output_type = output_type,
    instructions = instructions
  )
}

#' Parse input specification from string
#' @noRd
parse_inputs <- function(inputs_str) {
  if (inputs_str == "") {
    return(list())
  }

  # Split by comma, respecting nested structures
  input_specs <- split_respecting_nesting(inputs_str, ",")

  # Create input objects
  inputs <- lapply(input_specs, function(spec) {
    spec <- trimws(spec)
    # Check for type annotation (e.g., "text: string" or "choices: list[string]")
    if (grepl(":", spec)) {
      # Find the first colon not inside brackets
      chars <- strsplit(spec, "")[[1]]
      depth <- 0
      colon_pos <- -1
      for (i in seq_along(chars)) {
        if (chars[i] %in% c("[", "(")) {
          depth <- depth + 1
        }
        if (chars[i] %in% c("]", ")")) {
          depth <- depth - 1
        }
        if (chars[i] == ":" && depth == 0) {
          colon_pos <- i
          break
        }
      }

      if (colon_pos > 0) {
        name <- trimws(substr(spec, 1, colon_pos - 1))
        type_str <- trimws(substr(spec, colon_pos + 1, nchar(spec)))
        # Parse the type string to get the ellmer type
        type <- parse_type_string(type_str)
      } else {
        name <- spec
        type <- NULL
      }
    } else {
      name <- spec
      type <- NULL # Will default to string
    }

    input(
      name = trimws(name),
      type = type,
      description = paste("Input:", trimws(name))
    )
  })

  inputs
}

#' Parse output specification from string
#' @noRd
parse_output <- function(output_str) {
  # First, check if there are commas at depth 0 BEFORE any colons
  # This indicates multiple outputs like "sentiment, issues: array(string)"
  chars <- strsplit(output_str, "")[[1]]
  depth <- 0
  first_comma_pos <- -1
  first_colon_pos <- -1

  for (i in seq_along(chars)) {
    if (chars[i] %in% c("[", "(")) {
      depth <- depth + 1
    }
    if (chars[i] %in% c("]", ")")) {
      depth <- depth - 1
    }
    if (chars[i] == "," && depth == 0 && first_comma_pos == -1) {
      first_comma_pos <- i
    }
    if (chars[i] == ":" && depth == 0 && first_colon_pos == -1) {
      first_colon_pos <- i
    }
  }

  # If we have a comma before a colon (or no colon), it's likely multiple outputs
  if (
    first_comma_pos > 0 &&
      (first_colon_pos == -1 || first_comma_pos < first_colon_pos)
  ) {
    # Check if the comma is part of a type specification or truly separates fields
    # Look at the string before the first comma
    before_comma <- substr(output_str, 1, first_comma_pos - 1)

    # If before_comma contains a colon, it might be a single complex output
    # Otherwise, it's multiple outputs
    if (!grepl(":", before_comma)) {
      return(parse_multiple_outputs(output_str))
    }
  }

  # Original logic for single output with type specification
  if (grepl(":", output_str)) {
    # Find the first colon not inside brackets/parens
    chars <- strsplit(output_str, "")[[1]]
    depth <- 0
    colon_pos <- -1
    for (i in seq_along(chars)) {
      if (chars[i] %in% c("[", "(")) {
        depth <- depth + 1
      }
      if (chars[i] %in% c("]", ")")) {
        depth <- depth - 1
      }
      if (chars[i] == ":" && depth == 0) {
        colon_pos <- i
        break
      }
    }

    if (colon_pos > 0) {
      # Check if there are multiple outputs by looking for commas outside of type specs
      # after the first field
      remainder <- substr(output_str, colon_pos + 1, nchar(output_str))
      # Look for commas at depth 0 in the remainder
      has_multiple <- FALSE
      depth <- 0
      chars_remainder <- strsplit(remainder, "")[[1]]
      for (i in seq_along(chars_remainder)) {
        if (chars_remainder[i] %in% c("[", "(")) {
          depth <- depth + 1
        }
        if (chars_remainder[i] %in% c("]", ")")) {
          depth <- depth - 1
        }
        if (chars_remainder[i] == "," && depth == 0) {
          # Check if this comma is followed by another field (name:)
          rest <- substr(remainder, i + 1, nchar(remainder))
          if (grepl("^\\s*\\w+\\s*:", rest) || grepl("^\\s*\\w+\\s*$", rest)) {
            has_multiple <- TRUE
            break
          }
        }
      }

      if (has_multiple) {
        return(parse_multiple_outputs(output_str))
      }

      # Single output with type
      output_name <- trimws(substr(output_str, 1, colon_pos - 1))
      type_str <- trimws(substr(output_str, colon_pos + 1, nchar(output_str)))

      # Wrap single outputs in an object type for proper field access
      fields <- list()
      fields[[output_name]] <- parse_type_string(type_str, output_name)
      return(do.call(ellmer::type_object, fields))
    }
  }

  # Check for multiple outputs without types (e.g., "answer, confidence")
  if (grepl(",", output_str)) {
    # Simple check: if no colons or brackets, treat as multiple outputs
    if (!grepl("[\\[\\(]", output_str)) {
      return(parse_multiple_outputs(output_str))
    }
  }

  # Single output case without type annotation
  output_name <- trimws(output_str)
  type_str <- "string" # Default type

  # For single outputs, wrap in an object type to get proper field access
  # This ensures we get back a named list/object instead of raw JSON string
  fields <- list()
  fields[[output_name]] <- parse_type_string(type_str, output_name)
  do.call(ellmer::type_object, fields)
}

#' Parse multiple output fields
#' @noRd
parse_multiple_outputs <- function(output_str) {
  # Split by comma, but be careful with nested structures
  outputs <- split_respecting_nesting(output_str, ",")

  # Parse each output field
  fields <- list()
  for (output in outputs) {
    output <- trimws(output)

    # Parse field name and type
    if (grepl(":", output)) {
      parts <- strsplit(output, "\\s*:\\s*")[[1]]
      field_name <- trimws(parts[1])
      type_str <- trimws(parts[2])
    } else {
      field_name <- trimws(output)
      type_str <- "string"
    }

    # Parse the type and add to fields
    fields[[field_name]] <- parse_type_string(type_str, field_name)
  }

  # Return as an ellmer object type
  do.call(ellmer::type_object, fields)
}

#' Split string respecting nested structures
#' @noRd
split_respecting_nesting <- function(str, delimiter) {
  result <- list()
  current <- ""
  depth <- 0
  in_quotes <- FALSE
  quote_char <- NULL

  chars <- strsplit(str, "")[[1]]

  for (i in seq_along(chars)) {
    char <- chars[i]

    # Handle quotes
    if (char %in% c("'", '"') && (i == 1 || chars[i - 1] != "\\")) {
      if (!in_quotes) {
        in_quotes <- TRUE
        quote_char <- char
      } else if (char == quote_char) {
        in_quotes <- FALSE
        quote_char <- NULL
      }
    }

    # Track nesting depth
    if (!in_quotes) {
      if (char %in% c("[", "(", "{")) {
        depth <- depth + 1
      } else if (char %in% c("]", ")", "}")) {
        depth <- depth - 1
      }

      # Split at delimiter when not nested
      if (char == delimiter && depth == 0) {
        result <- append(result, trimws(current))
        current <- ""
        next
      }
    }

    current <- paste0(current, char)
  }

  # Add the last part
  if (nchar(current) > 0) {
    result <- append(result, trimws(current))
  }

  unlist(result)
}

#' Parse type string to ellmer type object
#' @noRd
parse_type_string <- function(type_str, field_name = NULL) {
  # Load ellmer types
  if (!requireNamespace("ellmer", quietly = TRUE)) {
    cli::cli_abort("Package 'ellmer' is required for type specifications")
  }

  # Handle common type patterns
  type_str <- trimws(type_str)

  # Handle Optional types
  if (grepl("^Optional\\[", type_str)) {
    inner_type_str <- sub("^Optional\\[(.*)\\]$", "\\1", type_str)
    inner_type <- parse_type_string(inner_type_str, field_name)
    # Set required to FALSE for Optional types
    if (!is.null(inner_type) && inherits(inner_type, "ellmer::Type")) {
      inner_type@required <- FALSE
    }
    return(inner_type)
  }

  # Handle Union types (simplified - just use first type for now)
  if (grepl("^Union\\[", type_str)) {
    types_str <- sub("^Union\\[(.*)\\]$", "\\1", type_str)
    # Split by comma, respecting nesting
    types <- split_respecting_nesting(types_str, ",")
    if (length(types) > 0) {
      # For now, use the first type and warn
      if (length(types) > 1) {
        cli::cli_warn(
          "Union types not fully supported. Using first type: {types[1]}"
        )
      }
      return(parse_type_string(types[1], field_name))
    }
  }

  # Handle dict/dictionary types
  if (grepl("^(dict|dictionary)\\[", type_str)) {
    # Parse dict[key_type, value_type] notation
    inner_str <- sub("^(dict|dictionary)\\[(.*)\\]$", "\\2", type_str)
    types <- split_respecting_nesting(inner_str, ",")

    if (length(types) == 2) {
      # For dict types, we'll use type_object with additional_properties
      # This is a simplification - ideally we'd have a specific dict type
      return(ellmer::type_object(.additional_properties = TRUE))
    }
    # Fallback to generic object
    return(ellmer::type_object())
  }

  # Simple types
  if (type_str == "string" || type_str == "str") {
    return(ellmer::type_string())
  } else if (
    type_str == "float" || type_str == "number" || type_str == "numeric"
  ) {
    return(ellmer::type_number())
  } else if (type_str == "int" || type_str == "integer") {
    return(ellmer::type_integer())
  } else if (
    type_str == "bool" || type_str == "boolean" || type_str == "logical"
  ) {
    return(ellmer::type_boolean())
  }

  # Enum types - multiple formats
  # enum('positive', 'negative', 'neutral')
  # enum(positive, negative, neutral)
  # Literal['positive', 'negative', 'neutral']
  if (grepl("^enum\\s*\\(", type_str) || grepl("^Literal\\s*\\[", type_str)) {
    # Extract the values
    if (grepl("^enum", type_str)) {
      values_str <- sub("^enum\\s*\\((.*)\\)$", "\\1", type_str)
    } else {
      values_str <- sub("^Literal\\s*\\[(.*)\\]$", "\\1", type_str)
    }

    # Parse values (handle both quoted and unquoted)
    values <- parse_enum_values(values_str)
    return(ellmer::type_enum(values = values))
  }

  # Array types
  # array(string)
  # list[string]
  # string[]
  # list[dict[str, str]]
  if (
    grepl("^array\\s*\\(", type_str) ||
      grepl("^list\\s*\\[", type_str) ||
      grepl("\\[\\]$", type_str)
  ) {
    if (grepl("^array\\s*\\(", type_str)) {
      inner_type_str <- sub("^array\\s*\\((.*)\\)$", "\\1", type_str)
    } else if (grepl("^list\\s*\\[", type_str)) {
      # Handle list[...] notation more carefully
      # Find the content between [ and the last ]
      start_pos <- regexpr("\\[", type_str)[1]
      if (start_pos > 0) {
        # Find matching closing bracket
        chars <- strsplit(type_str, "")[[1]]
        depth <- 0
        end_pos <- -1
        for (i in start_pos:length(chars)) {
          if (chars[i] == "[") {
            depth <- depth + 1
          }
          if (chars[i] == "]") {
            depth <- depth - 1
            if (depth == 0) {
              end_pos <- i
              break
            }
          }
        }
        if (end_pos > start_pos) {
          inner_type_str <- substr(type_str, start_pos + 1, end_pos - 1)
        } else {
          inner_type_str <- "string"
        }
      } else {
        inner_type_str <- "string"
      }
    } else {
      inner_type_str <- sub("\\[\\]$", "", type_str)
    }

    inner_type <- parse_type_string(inner_type_str)
    return(ellmer::type_array(items = inner_type))
  }

  # Number with bounds
  # number[0, 100]
  # float(0, 1)
  if (grepl("(number|float|numeric)\\s*[\\[\\(]", type_str)) {
    bounds_match <- regmatches(
      type_str,
      regexpr("[\\[\\(]([^\\]\\)]+)[\\]\\)]", type_str)
    )
    if (length(bounds_match) > 0) {
      bounds_str <- gsub("[\\[\\(\\]\\)]", "", bounds_match)
      bounds <- as.numeric(strsplit(bounds_str, "\\s*,\\s*")[[1]])
      if (length(bounds) == 2) {
        return(ellmer::type_number(minimum = bounds[1], maximum = bounds[2]))
      }
    }
    return(ellmer::type_number())
  }

  # String with length constraints
  # string[50, 200]
  if (grepl("string\\s*\\[", type_str)) {
    bounds_match <- regmatches(type_str, regexpr("\\[([^\\]]+)\\]", type_str))
    if (length(bounds_match) > 0) {
      bounds_str <- gsub("[\\[\\]]", "", bounds_match)
      bounds <- as.numeric(strsplit(bounds_str, "\\s*,\\s*")[[1]])
      if (length(bounds) == 1) {
        return(ellmer::type_string(max_length = bounds[1]))
      } else if (length(bounds) == 2) {
        return(ellmer::type_string(
          min_length = bounds[1],
          max_length = bounds[2]
        ))
      }
    }
  }

  # Object types (simplified for now)
  # object or dict
  if (type_str == "object" || type_str == "dict") {
    return(ellmer::type_object())
  }

  # Default fallback
  ellmer::type_string()
}

#' Parse enum values from a string
#' @noRd
parse_enum_values <- function(values_str) {
  # Remove quotes and split by comma
  values_str <- gsub("['\"]", "", values_str)
  values <- strsplit(values_str, "\\s*,\\s*")[[1]]
  trimws(values)
}
