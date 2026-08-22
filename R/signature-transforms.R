#' Signature Transforms for Advanced Reasoning Modules
#'
#' @description
#' Functions for transforming signatures to enable different reasoning patterns.
#' These are composable transforms that modify the output type of a signature
#' to include additional fields like chain-of-thought reasoning.
#'
#' @name signature-transforms
NULL

#' Derive a Signature with New Instructions
#'
#' @description
#' `with_instructions()` replaces a signature's instructions while preserving
#' its input and output fields. `append_instructions()` layers additional
#' instructions after the existing text, separated by a blank line. Both
#' functions return a new signature object and never mutate the original.
#'
#' @param x A signature object created by [signature()], or string notation
#'   such as `"question -> answer"`.
#' @param instructions A single, non-missing character string. Empty text is
#'   allowed when the resulting signature remains valid; for
#'   `append_instructions()` it is a no-op.
#'
#' @return A new signature object.
#' @export
#' @rdname signature-transforms
#' @examples
#' base <- signature(
#'   "text -> summary",
#'   instructions = "Summarize the text."
#' )
#'
#' concise <- append_instructions(base, "Use at most 30 words.")
#' base@instructions
#' concise@instructions
#'
#' replaced <- with_instructions(base, "Return one sentence.")
with_instructions <- function(x, instructions) {
  sig <- signature_transform_input(x)
  instructions <- validate_signature_instructions(instructions)

  Signature(
    inputs = sig@inputs,
    output_type = sig@output_type,
    instructions = instructions
  )
}

#' @export
#' @rdname signature-transforms
append_instructions <- function(x, instructions) {
  sig <- signature_transform_input(x)
  instructions <- validate_signature_instructions(instructions)

  combined <- if (!nzchar(instructions)) {
    sig@instructions
  } else if (!nzchar(sig@instructions)) {
    instructions
  } else {
    paste(sig@instructions, instructions, sep = "\n\n")
  }

  Signature(
    inputs = sig@inputs,
    output_type = sig@output_type,
    instructions = combined
  )
}

#' Coerce a signature-transform input
#' @noRd
signature_transform_input <- function(x) {
  if (S7::S7_inherits(x, Signature)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(signature(x))
  }

  cli::cli_abort(
    c(
      "{.arg x} must be a Signature or one signature string",
      "x" = "Received {.cls {class(x)[1]}}."
    ),
    class = "dsprrr_signature_transform_error"
  )
}

#' Add Chain-of-Thought Reasoning to a Signature
#'
#' @description
#' Transforms a signature to include a reasoning field before the original output.
#' This implements the Chain-of-Thought prompting pattern where the model is
#' asked to "show its work" before providing the final answer.
#'
#' @param x A signature object created by [signature()], or string notation
#'   such as `"question -> answer"`.
#' @param prefix Character. The prefix for the reasoning field description.
#'   Default uses DSPy-style "Let's think step by step" prompt.
#' @param reasoning_field Character. Name of the reasoning field to add.
#'   Default is "reasoning".
#' @param instructions Character. Optional new instructions for the signature.
#'   If NULL (default), original instructions are preserved with reasoning context.
#' @param ... Additional arguments (unused)
#'
#' @return A new signature object with a reasoning field added to its output
#'   type.
#'
#' @details
#' The transform works by:
#' 1. Extracting existing output fields from the signature's output_type
#' 2. Creating a new output_type with reasoning as the first field
#' 3. Adding appropriate description to guide the model
#'
#' The reasoning field is always placed first to encourage the model to
#' reason before answering (per Chain-of-Thought research).
#'
#' @export
#' @examples
#' # Basic usage with string notation
#' sig <- with_reasoning("question -> answer")
#'
#' # Custom prefix
#' sig <- with_reasoning(
#'   "math_problem -> solution",
#'   prefix = "Let me solve this step by step:"
#' )
#'
#' # With explicit signature
#' sig <- signature("context, question -> answer")
#' cot_sig <- with_reasoning(sig)
with_reasoning <- function(
  x,
  prefix = "Let's think step by step in order to",
  reasoning_field = "reasoning",
  instructions = NULL,
  ...
) {
  # Coerce to Signature if string
  sig <- if (is.character(x)) {
    signature(x)
  } else if (S7::S7_inherits(x, Signature)) {
    x
  } else {
    cli::cli_abort(c(
      "Invalid input to with_reasoning()",
      "x" = "Expected a Signature object or string notation",
      "i" = "You provided: {.cls {class(x)[1]}}"
    ))
  }

  # Extract existing output fields
  original_type <- sig@output_type
  original_fields <- extract_output_fields(original_type)

  # Build reasoning description
  reasoning_desc <- paste0(
    "Reasoning: ",
    prefix,
    " produce the ",
    describe_output_fields(original_fields),
    "."
  )

  # Create new output type with reasoning first
  new_fields <- list()
  new_fields[[reasoning_field]] <- ellmer::type_string(
    description = reasoning_desc
  )

  # Add original fields
  for (name in names(original_fields)) {
    new_fields[[name]] <- original_fields[[name]]
  }

  new_output_type <- do.call(ellmer::type_object, new_fields)

  # Build new instructions
  new_instructions <- if (!is.null(instructions)) {
    instructions
  } else if (nchar(sig@instructions) > 0) {
    # Preserve original instructions, add reasoning context
    paste0(
      sig@instructions,
      " Think through your reasoning step by step before providing the answer."
    )
  } else {
    # Generate default with reasoning context
    input_names <- if (length(sig@inputs) > 0) {
      paste0(
        "`",
        vapply(sig@inputs, function(inp) inp$name, character(1)),
        "`",
        collapse = ", "
      )
    } else {
      "the inputs"
    }
    output_names <- paste0("`", names(original_fields), "`", collapse = ", ")
    paste0(
      "Given ",
      input_names,
      ", think step by step and produce ",
      output_names,
      "."
    )
  }

  # Return new signature
  Signature(
    inputs = sig@inputs,
    output_type = new_output_type,
    instructions = new_instructions
  )
}

#' Create a Chain-of-Thought Module
#'
#' @description
#' Convenience function that creates a PredictModule with chain-of-thought
#' reasoning enabled. This is equivalent to calling `with_reasoning()` on
#' a signature and then creating a module from it.
#'
#' @param x A signature object created by [signature()], or string notation.
#' @param prefix Character. The prefix for the reasoning field.
#' @param chat Optional ellmer Chat object.
#' @param template Optional glue template.
#' @param demos Optional demonstration examples.
#' @param config Optional prediction configuration.
#' @param ... Must be empty.
#'
#' @return A PredictModule with reasoning enabled
#'
#' @export
#' @examples
#' # Create a chain-of-thought QA module
#' mod <- chain_of_thought("question -> answer")
#'
#' # Use it like any other module
#' # result <- run(mod, question = "What is 15 * 24?", .llm = llm)
#' # result$reasoning contains step-by-step reasoning
#' # result$answer contains the final answer
chain_of_thought <- function(
  x,
  prefix = "Let's think step by step in order to",
  chat = NULL,
  template = "",
  demos = list(),
  config = list(),
  ...
) {
  reject_partial_argument_matches(sys.call(), sys.function())
  reject_constructor_arguments("chain_of_thought", ...)

  cot_sig <- with_reasoning(x, prefix = prefix)
  mod <- module(
    cot_sig,
    chat = chat,
    template = template,
    demos = demos,
    config = config
  )
  stamp_module_kind(mod, "chain_of_thought")
}

#' Extract Output Fields from an ellmer Type
#'
#' @description
#' Internal helper to extract field definitions from an ellmer type object.
#' Handles both simple types (wrapping them in a named field) and object types.
#'
#' @param type_obj An ellmer type object
#' @return Named list of ellmer type objects representing output fields
#' @noRd
extract_output_fields <- function(type_obj) {
  if (is.null(type_obj)) {
    # Default to answer field
    return(list(answer = ellmer::type_string()))
  }

  # Check for S4/S7 with @properties slot (ellmer TypeObject)
  if (methods::.hasSlot(type_obj, "properties")) {
    props <- type_obj@properties
    if (length(props) > 0) {
      return(props)
    }
    # Empty object - default to answer
    return(list(answer = ellmer::type_string()))
  }

  # Simple type (string, number, etc.) - wrap in answer field
  # Preserve the original type
  list(answer = type_obj)
}

#' Describe Output Fields for Reasoning Prompt
#'
#' @description
#' Creates a human-readable description of output fields for the reasoning prompt.
#'
#' @param fields Named list of ellmer type objects
#' @return Character string describing the fields
#' @noRd
describe_output_fields <- function(fields) {
  if (length(fields) == 0) {
    return("output")
  }

  if (length(fields) == 1) {
    return(names(fields)[1])
  }

  # Multiple fields: "field1, field2, and field3"
  field_names <- names(fields)
  if (length(field_names) == 2) {
    paste(field_names, collapse = " and ")
  } else {
    paste0(
      paste(field_names[-length(field_names)], collapse = ", "),
      ", and ",
      field_names[length(field_names)]
    )
  }
}

#' Check if a Signature has Chain-of-Thought
#'
#' @description
#' Tests whether a signature has been transformed with `with_reasoning()`.
#' Checks for the presence of a reasoning field in the output type.
#'
#' @param sig A signature object created by [signature()].
#' @param reasoning_field Character. Name of reasoning field to check for.
#' @return Logical. TRUE if signature has chain-of-thought reasoning.
#'
#' @export
#' @examples
#' sig <- signature("question -> answer")
#' has_reasoning(sig)
#' # FALSE
#'
#' cot_sig <- with_reasoning(sig)
#' has_reasoning(cot_sig)
#' # TRUE
has_reasoning <- function(sig, reasoning_field = "reasoning") {
  # Check if it's a Signature (S7 class check)
  if (!S7::S7_inherits(sig, Signature)) {
    return(FALSE)
  }

  output_type <- sig@output_type

  # Check if it's an object type with the reasoning field
  if (methods::.hasSlot(output_type, "properties")) {
    props <- output_type@properties
    return(reasoning_field %in% names(props))
  }

  FALSE
}

#' Remove Chain-of-Thought from a Signature
#'
#' @description
#' Reverses the `with_reasoning()` transform by removing the reasoning field
#' from the output type. Useful for comparing reasoning vs non-reasoning
#' module performance.
#'
#' @param sig A signature object, typically created with [with_reasoning()].
#' @param reasoning_field Character. Name of reasoning field to remove.
#' @return A new signature object without the reasoning field.
#'
#' @export
#' @examples
#' cot_sig <- with_reasoning("question -> answer")
#' plain_sig <- without_reasoning(cot_sig)
without_reasoning <- function(sig, reasoning_field = "reasoning") {
  if (!S7::S7_inherits(sig, Signature)) {
    cli::cli_abort("Expected a Signature object")
  }

  if (!has_reasoning(sig, reasoning_field)) {
    return(sig)
  }

  output_type <- sig@output_type
  props <- output_type@properties

  # Remove reasoning field
  props[[reasoning_field]] <- NULL

  if (length(props) == 0) {
    # No fields left - default to string
    new_output_type <- ellmer::type_object(answer = ellmer::type_string())
  } else if (length(props) == 1) {
    # Single field remaining - could unwrap but keep as object for consistency
    new_output_type <- do.call(ellmer::type_object, props)
  } else {
    new_output_type <- do.call(ellmer::type_object, props)
  }

  Signature(
    inputs = sig@inputs,
    output_type = new_output_type,
    instructions = sig@instructions
  )
}
