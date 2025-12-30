#' @rdname signature
#' @export
Signature <- S7::new_class(
  "Signature",
  properties = list(
    inputs = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        if (!is.list(value)) {
          return("inputs must be a list")
        }

        for (i in seq_along(value)) {
          inp <- value[[i]]
          if (!is_dsprrr_input(inp)) {
            return(sprintf("Input %d must be created with input() function", i))
          }
        }
        NULL
      }
    ),
    output_type = S7::new_property(
      S7::class_any,
      validator = function(value) {
        if (is.null(value)) {
          return("output_type cannot be NULL")
        }

        # Check if it's an ellmer type object
        if (!is_ellmer_type(value)) {
          return(
            "output_type must be an ellmer type object (e.g., ellmer::type_string())"
          )
        }
        NULL
      }
    ),
    instructions = S7::new_property(
      S7::class_character,
      default = "",
      validator = function(value) {
        if (!is.character(value) || length(value) != 1) {
          return("instructions must be a single character string")
        }
        NULL
      }
    )
  ),
  validator = function(self) {
    # Additional cross-property validation if needed
    if (length(self@inputs) == 0 && nchar(self@instructions) == 0) {
      return("Signature must have either inputs or instructions defined")
    }
    NULL
  }
)

#' Print method for Signature
#' @noRd
print_signature <- function(x, ...) {
  cli::cli_h2("Signature")

  if (length(x@inputs) > 0) {
    cli::cli_h3("Inputs")
    for (i in seq_along(x@inputs)) {
      inp <- x@inputs[[i]]
      name <- if (!is.null(inp$name)) inp$name else paste0("input_", i)
      desc <- if (!is.null(inp$description)) inp$description else ""
      cli::cli_li("{.field {name}}: {desc}")
    }
  }

  cli::cli_h3("Output")
  cli::cli_text("Type: {.cls {class(x@output_type)[1]}}")

  if (nchar(x@instructions) > 0) {
    cli::cli_h3("Instructions")
    cli::cli_text(x@instructions)
  }

  invisible(x)
}

#' Create a Signature for LLM Operations
#'
#' @description
#' The primary function for creating signatures. Accepts either DSPy-style
#' string notation or explicit arguments.
#'
#' @param x Either a string in DSPy format ("inputs -> output") or NULL
#' @param inputs List of input specifications (when using explicit notation)
#' @param output_type An ellmer type object (when using explicit notation)
#' @param instructions Optional instructions for the operation
#' @param ... Additional arguments
#'
#' @return A Signature object
#' @export
#' @examples
#' # String notation (recommended for simple cases)
#' sig1 <- signature("text -> sentiment")
#' sig2 <- signature("context, question -> answer: string")
#' sig3 <- signature("text -> label: enum('positive', 'negative', 'neutral')")
#'
#' # Explicit notation (for complex cases)
#' sig4 <- signature(
#'   inputs = list(
#'     input("text", description = "Text to analyze")
#'   ),
#'   output_type = ellmer::type_object(
#'     sentiment = ellmer::type_string(),
#'     confidence = ellmer::type_number()
#'   ),
#'   instructions = "Analyze the text"
#' )
signature <- function(
  x = NULL,
  inputs = NULL,
  output_type = NULL,
  instructions = "",
  ...
) {
  # If first argument is a string, parse it as DSPy-style notation
  if (is.character(x) && !is.null(x)) {
    return(parse_signature(x, instructions))
  }

  # Otherwise, use explicit notation
  if (!is.null(inputs) && !is.null(output_type)) {
    return(Signature(
      inputs = inputs,
      output_type = output_type,
      instructions = instructions
    ))
  }

  cli::cli_abort(c(
    "Invalid signature specification",
    "i" = "Use either string notation: signature('text -> sentiment')",
    "i" = "Or explicit notation: signature(inputs = list(...), output_type = ...)"
  ))
}
