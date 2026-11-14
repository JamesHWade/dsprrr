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

  if (reasoning_field %in% names(original_fields)) {
    cli::cli_abort(
      "Cannot add {.field {reasoning_field}} because the signature already has that output field",
      class = "dsprrr_signature_transform_error"
    )
  }

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
#' `chain_of_thought()` creates a prediction module with one of three
#' reasoning modes. The default, `"rationale"`, keeps the existing behavior:
#' it adds a `reasoning` output field before the requested fields. `"plan"`
#' adds a short, explicit `plan` field before the requested fields. `"native"`
#' keeps the signature unchanged and exposes provider-supplied reasoning
#' metadata when the provider records it in ellmer turns.
#'
#' @param x A signature from [signature()], or a signature string.
#' @param prefix Start of the reasoning field's description; see
#'   [with_reasoning()]. Used only when `mode = "rationale"`.
#' @param chat,template,demos,config As in [module()].
#' @param mode The reasoning behavior: `"rationale"` (the default), `"native"`
#'   or `"plan"`. `"native"` does not select a model or enable provider
#'   reasoning by itself. Configure provider parameters such as
#'   `reasoning_effort` through `chat` or `config`, using ellmer's supported
#'   parameters.
#' @param ... Must be empty.
#'
#' @return A prediction module, as from [module()]. The `"rationale"` and
#'   `"plan"` modes include their field in the output. With `"native"`, the
#'   output keeps the signature's fields; on a structured result,
#'   `get_metadata()` exposes `native_reasoning` as a list of ellmer
#'   `ContentThinking` objects from the current call, or `NULL` when the
#'   provider did not expose any. It never synthesizes a reasoning string.
#'
#' Native metadata is supported in structured results from synchronous scalar
#'   [run()] calls and direct [run_async()] calls. Native mode errors before
#'   provider work for batch or streaming execution, whose current result
#'   contracts do not preserve these turns.
#'
#' @export
#' @family program constructors
#' @examples
#' solver <- chain_of_thought("question -> answer: float")
#' solver
#'
#' native_solver <- chain_of_thought(
#'   "question -> answer: float",
#'   mode = "native",
#'   config = list(reasoning_effort = "low")
#' )
#'
#' \dontrun{
#' result <- run(
#'   solver,
#'   question = "What is 15 * 24?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' result$reasoning
#' result$answer
#'
#' native_result <- run(
#'   native_solver,
#'   question = "What is 15 * 24?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   .return_format = "structured"
#' )
#' get_metadata(native_result)$native_reasoning
#' }
chain_of_thought <- function(
  x,
  prefix = "Let's think step by step in order to",
  chat = NULL,
  template = "",
  demos = list(),
  config = list(),
  mode = c("rationale", "native", "plan"),
  ...
) {
  reject_partial_argument_matches(sys.call(), sys.function())
  reject_constructor_arguments("chain_of_thought", ...)

  mode <- if (identical(mode, c("rationale", "native", "plan"))) {
    "rationale"
  } else {
    validate_reasoning_mode(mode)
  }
  config <- normalize_module_config(config)
  if (!is.null(config$reasoning_mode)) {
    configured_mode <- validate_reasoning_mode(config$reasoning_mode)
    if (!identical(configured_mode, mode)) {
      cli::cli_abort(
        c(
          "Conflicting chain-of-thought modes",
          "x" = "{.arg mode} is {.val {mode}}, but `config$reasoning_mode` is {.val {configured_mode}}."
        ),
        class = "dsprrr_reasoning_mode_error"
      )
    }
  }
  config$reasoning_mode <- mode

  cot_sig <- switch(
    mode,
    rationale = with_reasoning(x, prefix = prefix),
    native = signature_transform_input(x),
    plan = with_plan(x)
  )
  mod <- module(
    cot_sig,
    chat = chat,
    template = template,
    demos = demos,
    config = config
  )
  stamp_module_kind(mod, "chain_of_thought")
}

#' Add a concise plan field to a signature
#' @noRd
with_plan <- function(x) {
  sig <- signature_transform_input(x)
  original_fields <- extract_output_fields(sig@output_type)

  if ("plan" %in% names(original_fields)) {
    cli::cli_abort(
      "Cannot add {.field plan} because the signature already has that output field",
      class = "dsprrr_signature_transform_error"
    )
  }

  plan_type <- ellmer::type_string(
    description = paste0(
      "A brief plan of no more than three high-level steps. ",
      "Do not include detailed reasoning."
    )
  )
  output_fields <- c(list(plan = plan_type), original_fields)
  output_type <- do.call(ellmer::type_object, output_fields)

  input_names <- if (length(sig@inputs) > 0L) {
    paste0(
      "`",
      vapply(sig@inputs, function(input) input$name, character(1)),
      "`",
      collapse = ", "
    )
  } else {
    "the inputs"
  }
  output_names <- paste0("`", names(original_fields), "`", collapse = ", ")
  plan_instructions <- paste0(
    "Given ",
    input_names,
    ", give a brief plan of no more than three high-level steps in `plan`. ",
    "Do not include detailed reasoning. Then produce ",
    output_names,
    "."
  )
  instructions <- if (nzchar(sig@instructions)) {
    paste(sig@instructions, plan_instructions, sep = "\n\n")
  } else {
    plan_instructions
  }

  Signature(
    inputs = sig@inputs,
    output_type = output_type,
    instructions = instructions
  )
}

#' Validate a reasoning mode value
#' @noRd
validate_reasoning_mode <- function(mode) {
  choices <- c("rationale", "native", "plan")
  if (
    !is.character(mode) ||
      length(mode) != 1L ||
      is.na(mode) ||
      !mode %in% choices
  ) {
    cli::cli_abort(
      c(
        "Invalid chain-of-thought mode",
        "x" = "{.arg mode} must be one of {.val {choices}}."
      ),
      class = "dsprrr_reasoning_mode_error"
    )
  }
  mode
}

#' Get a module's reasoning mode, preserving old chain-of-thought artifacts
#' @noRd
module_reasoning_mode <- function(module) {
  mode <- module$config$reasoning_mode
  if (is.null(mode)) {
    if (identical(module$config$.module_kind, "chain_of_thought")) {
      return("rationale")
    }
    return(NULL)
  }
  validate_reasoning_mode(mode)
}

#' Return provider-supplied thinking blocks from assistant turns
#' @noRd
native_reasoning_from_turns <- function(turns) {
  if (!is.list(turns) || length(turns) == 0L) {
    return(NULL)
  }

  content <- list()
  for (turn in turns) {
    if (!inherits(turn, "ellmer::AssistantTurn")) {
      next
    }
    turn_content <- tryCatch(turn@contents, error = function(e) list())
    if (!is.list(turn_content)) {
      next
    }
    content <- c(
      content,
      Filter(
        function(item) inherits(item, "ellmer::ContentThinking"),
        turn_content
      )
    )
  }

  if (length(content) == 0L) NULL else content
}

#' Find the path to a module configured for one reasoning mode
#' @noRd
module_reasoning_mode_path <- function(module, mode) {
  mode <- validate_reasoning_mode(mode)
  seen <- character()

  visit <- function(value, path) {
    if (inherits(value, "Module")) {
      address <- format(rlang::obj_address(value))
      if (address %in% seen) {
        return(NULL)
      }
      seen <<- c(seen, address)
      if (identical(module_reasoning_mode(value), mode)) {
        return(path)
      }
      return(visit(module_children(value), path))
    }

    if (is.list(value)) {
      for (i in seq_along(value)) {
        name <- names(value)[i]
        label <- if (!is.null(name) && !is.na(name) && nzchar(name)) {
          name
        } else {
          as.character(i)
        }
        found <- visit(value[[i]], paste0(path, "/", label))
        if (!is.null(found)) {
          return(found)
        }
      }
    }
    NULL
  }

  visit(module, "$")
}

#' Reject native reasoning when execution would discard provider turns
#' @noRd
assert_native_reasoning_metadata_supported <- function(module, operation) {
  path <- module_reasoning_mode_path(module, "native")
  if (is.null(path)) {
    return(invisible(module))
  }

  cli::cli_abort(
    c(
      "{.fn {operation}} cannot preserve native reasoning metadata",
      "x" = "Native reasoning is configured at graph path {.code {path}}.",
      "i" = "Use a synchronous scalar {.fn run} call with {.code .return_format = \"structured\"}."
    ),
    class = "dsprrr_reasoning_metadata_unsupported",
    operation = operation,
    module_path = path
  )
}

#' Reject native reasoning in batch paths without per-row thinking turns
#' @noRd
assert_native_reasoning_batch_supported <- function(module) {
  path <- module_reasoning_mode_path(module, "native")
  if (is.null(path)) {
    return(invisible(module))
  }

  cli::cli_abort(
    c(
      "Batch execution cannot preserve native reasoning metadata",
      "x" = "Native reasoning is configured at graph path {.code {path}}.",
      "i" = "Run one input at a time with {.code .return_format = \"structured\"}."
    ),
    class = c(
      "dsprrr_reasoning_batch_unsupported",
      "dsprrr_batch_unsupported_module"
    ),
    module_path = path
  )
}

#' Reject text-only streaming for CoT modes with structured output contracts
#' @noRd
assert_reasoning_text_stream_supported <- function(module) {
  for (mode in c("native", "plan")) {
    path <- module_reasoning_mode_path(module, mode)
    if (is.null(path)) {
      next
    }

    contract <- if (identical(mode, "native")) {
      "provider reasoning metadata"
    } else {
      "the explicit plan output field"
    }
    cli::cli_abort(
      c(
        "stream_async() cannot preserve {contract}",
        "x" = "Mode {.val {mode}} is configured at graph path {.code {path}}.",
        "i" = "Use {.fn run_async} for structured output, or {.fn run} when native metadata is needed."
      ),
      class = "dsprrr_reasoning_metadata_unsupported",
      operation = "stream_async",
      module_path = path,
      reasoning_mode = mode
    )
  }
  invisible(module)
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
