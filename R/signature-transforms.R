#' Replace or extend signature instructions
#'
#' @description
#' `with_instructions()` replaces a signature's instructions.
#' `append_instructions()` adds text after the existing instructions,
#' separated by a blank line. Both keep the input and output fields and return
#' a new signature; the original is unchanged.
#'
#' @param x A signature from [signature()], or a signature string such as
#'   `"question -> answer"`.
#' @param instructions One string. Empty text is allowed when the signature
#'   stays valid; `append_instructions()` then returns the instructions
#'   unchanged.
#'
#' @return A new signature.
#' @family signatures
#' @export
#' @examples
#' base <- signature(
#'   "text -> summary",
#'   instructions = "Summarize the text."
#' )
#'
#' concise <- append_instructions(base, "Use at most 30 words.")
#' concise@instructions
#'
#' with_instructions(base, "Return one sentence.")@instructions
#' base@instructions
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
#' @rdname with_instructions
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

#' Add a reasoning field to a signature
#'
#' @description
#' `with_reasoning()` adds a string output field, `reasoning` by default,
#' before the existing output fields, so the model writes out its reasoning
#' before it answers (chain-of-thought prompting). [chain_of_thought()] builds
#' a module from the result.
#'
#' @param x A signature from [signature()], or a signature string such as
#'   `"question -> answer"`.
#' @param prefix Start of the reasoning field's description. The description
#'   reads "Reasoning: `prefix` produce the `fields`.", where `fields` names
#'   the original output fields.
#' @param reasoning_field Name of the added field.
#' @param instructions New instructions. With `NULL` (the default), existing
#'   instructions get " Think through your reasoning step by step before
#'   providing the answer." appended, and empty instructions are replaced by
#'   "Given `inputs`, think step by step and produce `outputs`."
#' @param ... Ignored.
#'
#' @return A new signature whose output is an object with the reasoning field
#'   first, followed by the original output fields. A bare output type becomes
#'   a field named `answer`.
#'
#' @export
#' @family signatures
#' @examples
#' with_reasoning("question -> answer")
#'
#' # The prefix goes into the description of the reasoning field
#' sig <- with_reasoning(
#'   "math_problem -> solution: float",
#'   prefix = "Let me solve this step by step, then"
#' )
#' sig@output_type@properties$reasoning@description
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

#' Create a chain-of-thought module
#'
#' @description
#' `chain_of_thought()` is `module(with_reasoning(x))`: a prediction module
#' whose output starts with a `reasoning` field, so the model reasons step by
#' step before it gives the other outputs.
#'
#' @param x A signature from [signature()], or a signature string.
#' @param prefix Start of the reasoning field's description; see
#'   [with_reasoning()].
#' @param chat,template,demos,config As in [module()].
#' @param ... Must be empty.
#'
#' @return A prediction module, as from [module()]. [run()] returns the
#'   reasoning along with the other outputs, for example
#'   `list(reasoning = "...", answer = "...")`.
#'
#' @export
#' @family program constructors
#' @examples
#' solver <- chain_of_thought("question -> answer: float")
#' solver
#'
#' \dontrun{
#' result <- run(
#'   solver,
#'   question = "What is 15 * 24?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' result$reasoning
#' result$answer
#' }
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

#' Test whether a signature has a reasoning field
#'
#' @description
#' `has_reasoning()` checks whether a signature's output has a field named
#' `reasoning_field`, as added by [with_reasoning()].
#'
#' @param sig A signature from [signature()].
#' @param reasoning_field Name of the field to look for.
#' @return `TRUE` or `FALSE`. Anything other than a signature gives `FALSE`.
#'
#' @export
#' @family signatures
#' @examples
#' sig <- signature("question -> answer")
#' has_reasoning(sig)
#' has_reasoning(with_reasoning(sig))
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

#' Remove the reasoning field from a signature
#'
#' @description
#' `without_reasoning()` drops the reasoning field that [with_reasoning()]
#' added, for example to compare a module with and without chain-of-thought.
#' The instructions are kept as they are.
#'
#' @param sig A signature from [signature()], usually one returned by
#'   [with_reasoning()].
#' @param reasoning_field Name of the field to remove.
#' @return A new signature without the field, or `sig` unchanged if it has no
#'   such field. If no output field would remain, the output becomes a single
#'   string field named `answer`.
#'
#' @export
#' @family signatures
#' @examples
#' cot <- with_reasoning("question -> answer")
#' without_reasoning(cot)
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
