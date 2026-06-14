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
  # Validate input is a non-empty string

  if (!is.character(signature_str) || length(signature_str) != 1) {
    cli::cli_abort(c(
      "Signature must be a single character string",
      "x" = "You provided: {.cls {class(signature_str)[1]}}",
      "i" = "Example: {.code signature('question -> answer')}"
    ))
  }

  signature_str <- trimws(signature_str)
  if (nchar(signature_str) == 0) {
    cli::cli_abort(c(
      "Signature cannot be empty",
      "i" = "Example: {.code signature('question -> answer')}"
    ))
  }

  # Check for common arrow mistakes before splitting
  arrow_suggestion <- detect_arrow_mistake(signature_str)
  if (!is.null(arrow_suggestion)) {
    cli::cli_abort(c(
      "Invalid arrow in signature",
      "x" = "You provided: {.val {signature_str}}",
      "i" = arrow_suggestion$message,
      "i" = "Corrected: {.code {arrow_suggestion$corrected}}"
    ))
  }

  # Split by arrow
  parts <- strsplit(signature_str, "\\s*->\\s*")[[1]]

  if (length(parts) != 2) {
    # Provide helpful error messages based on what's wrong
    if (length(parts) == 1) {
      # No arrow found - suggest likely fix
      suggestion <- suggest_signature_fix(signature_str)
      cli::cli_abort(c(
        "Missing {.code ->} separator in signature",
        "x" = "You provided: {.val {signature_str}}",
        "i" = suggestion
      ))
    } else {
      # Multiple arrows
      cli::cli_abort(c(
        "Multiple {.code ->} separators found in signature",
        "x" = "You provided: {.val {signature_str}}",
        "i" = "Use exactly one {.code ->} to separate inputs from outputs",
        "i" = "Example: {.code 'input1, input2 -> output'}"
      ))
    }
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
    output_field_names <- if (grepl(",", output_str, fixed = TRUE)) {
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
    type_explicit <- FALSE

    # Check for type annotation (e.g., "text: string" or "choices: list[string]")
    if (grepl(":", spec, fixed = TRUE)) {
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
        type_explicit <- TRUE
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
      description = paste("Input:", trimws(name)),
      .type_explicit = type_explicit
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
    if (!grepl(":", before_comma, fixed = TRUE)) {
      return(parse_multiple_outputs(output_str))
    }
  }

  # Original logic for single output with type specification
  if (grepl(":", output_str, fixed = TRUE)) {
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
  if (grepl(",", output_str, fixed = TRUE)) {
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
    if (grepl(":", output, fixed = TRUE)) {
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
  rlang::check_installed("ellmer", reason = "for type specifications")

  # Handle common type patterns
  type_str <- trimws(type_str)

  # Catch unknown / misspelled simple types early with a helpful error,
  # rather than silently falling through to the type_string() default below.
  validate_type_string(type_str)

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

# ============================================================================
# Error Detection and Suggestion Helpers
# ============================================================================

#' Detect common arrow mistakes in signature string
#' @noRd
detect_arrow_mistake <- function(signature_str) {
  # Common wrong arrows that users might type
  # Order matters: check longer patterns first to avoid partial matches
  wrong_arrows <- list(
    list(pattern = "-->", name = "double dash arrow"),
    list(pattern = "=>", name = "fat arrow"),
    list(pattern = "<-", name = "left arrow"),
    list(pattern = "~>", name = "tilde arrow")
  )

  for (arrow in wrong_arrows) {
    if (grepl(arrow$pattern, signature_str, fixed = TRUE)) {
      # Create corrected version
      corrected <- gsub(arrow$pattern, "->", signature_str, fixed = TRUE)
      return(list(
        message = paste0("Use {.code ->} not {.code ", arrow$pattern, "}"),
        corrected = corrected
      ))
    }
  }

  # Only if no wrong arrows found, check if there's a correct arrow
  # If there's already a correct arrow, no mistake to report
  NULL
}

#' Suggest a fix for a signature without an arrow
#' @noRd
suggest_signature_fix <- function(signature_str) {
  # Check if it looks like space-separated words (common mistake)
  words <- strsplit(trimws(signature_str), "\\s+")[[1]]

  if (length(words) == 2) {
    # Two words - probably meant "input -> output"
    return(paste0(
      "Did you mean: {.code '",
      words[1],
      " -> ",
      words[2],
      "'}"
    ))
  }

  if (length(words) > 2) {
    # Multiple words - guess the split point
    # If there's a comma, split there
    if (grepl(",", signature_str, fixed = TRUE)) {
      # Has commas - probably multiple inputs
      return(paste0(
        "Add {.code ->} between inputs and output. ",
        "Example: {.code 'input1, input2 -> output'}"
      ))
    }
    # No commas - last word is probably the output
    inputs <- paste(words[-length(words)], collapse = ", ")
    output <- words[length(words)]
    return(paste0(
      "Did you mean: {.code '",
      inputs,
      " -> ",
      output,
      "'}"
    ))
  }

  # Single word or other case
  paste0(
    "Add inputs and {.code ->} separator. ",
    "Example: {.code 'question -> answer'}"
  )
}

#' Validate type string and provide helpful error
#' @noRd
validate_type_string <- function(type_str, context = "output") {
  type_str <- trimws(type_str)

  # Check for empty type after colon

  if (nchar(type_str) == 0) {
    cli::cli_abort(c(
      "Type annotation is incomplete",
      "x" = "Missing type after {.code :}",
      "i" = "Available types: {.code string}, {.code number}, {.code boolean}, {.code enum(...)}, {.code array(...)}"
    ))
  }

  # Check for common type mistakes
  type_lower <- tolower(type_str)

  # Check for Python-style types
  python_types <- list(
    list(wrong = "str", correct = "string"),
    list(wrong = "int", correct = "integer"),
    list(wrong = "float", correct = "number"),
    list(wrong = "bool", correct = "boolean")
  )

  for (pt in python_types) {
    if (type_lower == pt$wrong) {
      # These are actually supported, so no error needed
      return(NULL)
    }
  }

  # Check for completely unknown types
  known_types <- c(
    "string",
    "str",
    "number",
    "float",
    "numeric",
    "integer",
    "int",
    "boolean",
    "bool",
    "logical",
    "object",
    "dict"
  )

  # Check if it starts with a known constructor
  known_constructors <- c(
    "enum",
    "array",
    "list",
    "Literal",
    "Optional",
    "Union"
  )

  is_known <- type_lower %in%
    known_types ||
    any(vapply(
      known_constructors,
      function(c) grepl(paste0("^", tolower(c)), type_lower),
      logical(1)
    ))

  if (!is_known && !grepl("\\[|\\(", type_str)) {
    # Unknown simple type
    suggestion <- find_closest_match(
      type_str,
      c(known_types, known_constructors)
    )
    cli::cli_abort(c(
      "Unknown type: {.val {type_str}}",
      if (!is.null(suggestion)) {
        c("i" = "Did you mean {.code {suggestion}}?")
      },
      "i" = "Available simple types: {.code string}, {.code number}, {.code integer}, {.code boolean}",
      "i" = "Available complex types: {.code enum('a', 'b')}, {.code array(string)}, {.code object}"
    ))
  }

  NULL
}

#' Find closest match using Levenshtein distance
#' @noRd
find_closest_match <- function(input, candidates, max_distance = 3) {
  if (length(candidates) == 0) {
    return(NULL)
  }

  # Simple Levenshtein distance implementation
  levenshtein <- function(s1, s2) {
    s1 <- tolower(s1)
    s2 <- tolower(s2)

    if (nchar(s1) == 0) {
      return(nchar(s2))
    }
    if (nchar(s2) == 0) {
      return(nchar(s1))
    }

    m <- nchar(s1)
    n <- nchar(s2)

    d <- matrix(0, m + 1, n + 1)
    d[, 1] <- 0:m
    d[1, ] <- 0:n

    s1_chars <- strsplit(s1, "")[[1]]
    s2_chars <- strsplit(s2, "")[[1]]

    for (i in 2:(m + 1)) {
      for (j in 2:(n + 1)) {
        cost <- if (s1_chars[i - 1] == s2_chars[j - 1]) 0 else 1
        d[i, j] <- min(
          d[i - 1, j] + 1,
          d[i, j - 1] + 1,
          d[i - 1, j - 1] + cost
        )
      }
    }

    d[m + 1, n + 1]
  }

  # Find closest match
  distances <- vapply(candidates, function(c) levenshtein(input, c), numeric(1))
  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  if (min_dist <= max_distance) {
    return(candidates[min_idx])
  }

  NULL
}
