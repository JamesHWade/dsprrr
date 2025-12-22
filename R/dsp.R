#' Declarative Structured Prediction
#'
#' @description
#' The `dsp()` function provides a pipe-friendly interface for structured
#' LLM predictions using dsprrr signatures. It's designed to feel familiar
#' to ellmer users while providing the power of declarative signatures.
#'
#' @details
#' `dsp()` is the simplest way to use dsprrr. It takes a Chat object and a
#' signature, builds a prompt from the provided inputs, and returns the
#' structured output. For more control (optimization, tracing), use
#' `as_module()` to create a reusable Module.
#'
#' The function dispatches on the first argument:
#' - `dsp.Chat`: When piping from an ellmer Chat object
#' - `dsp.character`: When providing a signature string (uses default Chat)
#' - `dsp.dsprrr::Signature`: When providing a Signature object
#'
#' @param x A Chat object, signature string, or Signature object.
#' @param signature When `x` is a Chat, the signature (string or Signature object).
#' @param ... Named arguments matching the signature's inputs.
#' @param .instructions Optional additional instructions (appended to signature instructions).
#' @param .echo Whether to echo output. See [ellmer::Chat] for options.
#'
#' @return The structured output according to the signature's output type.
#'   For single-field outputs, returns just the value. For multi-field outputs,
#'   returns a named list.
#'
#' @export
#' @examples
#' \dontrun{
#' # Pipe-friendly usage with Chat
#' chat <- ellmer::chat_openai()
#' chat |> dsp("question -> answer", question = "What is 2+2?")
#'
#' # With complex output types
#' chat |> dsp(
#'   "text -> sentiment: enum('positive', 'negative', 'neutral')",
#'   text = "I love this package!"
#' )
#'
#' # Using default Chat (auto-detected from env vars)
#' dsp("question -> answer", question = "What is the capital of France?")
#'
#' # With additional instructions
#' chat |> dsp(
#'   "text -> summary",
#'   text = long_article,
#'   .instructions = "Keep the summary under 50 words"
#' )
#' }
dsp <- function(x, ...) {
  UseMethod("dsp")
}

#' @rdname dsp
#' @export
dsp.Chat <- function(x, signature, ..., .instructions = NULL, .echo = "none") {
  chat <- x

  # Parse signature if string
  sig <- if (is.character(signature)) {
    signature(signature)
  } else if (inherits(signature, "dsprrr::Signature")) {
    signature
  } else {
    cli::cli_abort(c(
      "Invalid signature",
      "x" = "{.arg signature} must be a string or Signature object",
      "i" = "Example: {.code 'question -> answer'}"
    ))
  }

  # Extract inputs from ...
  inputs <- list(...)

  # Validate inputs against signature
  validate_dsp_inputs(sig, inputs)

  # Build prompt from signature and inputs
  prompt <- build_dsp_prompt(sig, inputs)

  # Combine instructions
  instructions <- sig@instructions
  if (!is.null(.instructions) && nzchar(.instructions)) {
    instructions <- paste(instructions, .instructions, sep = "\n\n")
  }

  # Build full prompt with instructions
  full_prompt <- if (nzchar(instructions)) {
    paste(instructions, prompt, sep = "\n\n")
  } else {
    prompt
  }

  # Call chat_structured
  result <- tryCatch(
    {
      chat$chat_structured(
        full_prompt,
        type = sig@output_type,
        echo = .echo
      )
    },
    error = function(e) {
      cli::cli_abort(
        "dsp() failed: {e$message}",
        parent = e
      )
    }
  )

  # Store trace for debugging
  store_last_trace(list(
    signature = sig,
    inputs = inputs,
    prompt = full_prompt,
    output = result,
    timestamp = Sys.time()
  ))

  # Simplify single-field results
  simplify_output(result, sig@output_type)
}

#' @rdname dsp
#' @export
dsp.character <- function(x, ..., .instructions = NULL, .echo = "none") {
  # x is the signature string, use default chat
  chat <- get_default_chat()
  dsp.Chat(chat, signature = x, ..., .instructions = .instructions, .echo = .echo)
}

#' @rdname dsp
#' @export
`dsp.dsprrr::Signature` <- function(x, ..., .instructions = NULL, .echo = "none") {
  # x is a Signature object, use default chat
  chat <- get_default_chat()
  dsp.Chat(chat, signature = x, ..., .instructions = .instructions, .echo = .echo)
}

#' Create a Module from a Chat
#'
#' @description
#' Creates a dsprrr Module that wraps an ellmer Chat object. The Module
#' can be used for repeated predictions, batch processing, and optimization.
#'
#' @param x A Chat object, signature string, or Signature object.
#' @param signature When `x` is a Chat, the signature (string or Signature object).
#' @param ... Additional arguments passed to `module()`.
#'
#' @return A Module object (R6) that wraps the Chat.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create module from Chat
#' mod <- chat_openai() |> as_module("text -> sentiment")
#'
#' # Use the module
#' mod$predict(text = "I love this!")
#'
#' # Optimize
#' mod$optimize_grid(trainset, metric = metric_exact_match())
#'
#' # Access underlying Chat
#' mod$chat$chat("Hello!")
#' }
as_module <- function(x, ...) {
  UseMethod("as_module")
}

#' @rdname as_module
#' @export
as_module.Chat <- function(x, signature, ...) {
  chat <- x

  # Parse signature if string
  sig <- if (is.character(signature)) {
    signature(signature)
  } else if (inherits(signature, "dsprrr::Signature")) {
    signature
  } else {
    cli::cli_abort(c(
      "Invalid signature",
      "x" = "{.arg signature} must be a string or Signature object"
    ))
  }

  # Create module with the chat attached
  module(sig, type = "predict", chat = chat, ...)
}

#' @rdname as_module
#' @export
as_module.character <- function(x, ...) {
  # x is a signature string, use default chat
  chat <- get_default_chat()
  as_module.Chat(chat, signature = x, ...)
}

#' @rdname as_module
#' @export
`as_module.dsprrr::Signature` <- function(x, ...) {
  # x is a Signature, use default chat
  chat <- get_default_chat()
  as_module.Chat(chat, signature = x, ...)
}

#' Get the Last DSP Trace
#'
#' @description
#' Returns the trace from the most recent `dsp()` call. Useful for
#' debugging and understanding what happened.
#'
#' @return A list containing:
#'   - `signature`: The Signature object used
#'   - `inputs`: The inputs provided
#'   - `prompt`: The full prompt sent to the LLM
#'   - `output`: The raw output from the LLM
#'   - `timestamp`: When the call was made
#'
#' @export
#' @examples
#' \dontrun{
#' dsp("q -> a", q = "What is 2+2?")
#' trace <- last_trace()
#' trace$prompt  # See the prompt that was sent
#' }
last_trace <- function() {
  .dsprrr_env$last_trace
}

# Internal: Store trace for last_trace()
store_last_trace <- function(trace) {
  .dsprrr_env$last_trace <- trace
}

# Internal: Validate inputs against signature
validate_dsp_inputs <- function(sig, inputs) {
  if (length(sig@inputs) == 0) {
    return(invisible(NULL))
  }

  required_names <- vapply(sig@inputs, function(x) x$name, character(1))
  provided_names <- names(inputs)

  # Check for missing required inputs
  missing <- setdiff(required_names, provided_names)
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing required inputs",
      "x" = "Missing: {.field {missing}}",
      "i" = "Signature requires: {.field {required_names}}"
    ))
  }

  # Warn about extra inputs
  extra <- setdiff(provided_names, required_names)
  if (length(extra) > 0) {
    cli::cli_warn("Ignoring unknown inputs: {.field {extra}}")
  }

  invisible(NULL)
}

# Internal: Build prompt from signature and inputs
build_dsp_prompt <- function(sig, inputs) {
  # Format each input
  input_lines <- character()

  for (input_spec in sig@inputs) {
    name <- input_spec$name
    value <- inputs[[name]]

    # Add description as comment if present
    if (!is.null(input_spec$description) && nzchar(input_spec$description)) {
      input_lines <- c(input_lines, paste0("# ", input_spec$description))
    }

    # Add the input value
    if (is.character(value) && length(value) == 1) {
      input_lines <- c(input_lines, paste0(name, ": ", value))
    } else {
      # For complex values, use JSON
      input_lines <- c(input_lines, paste0(
        name, ": ",
        jsonlite::toJSON(value, auto_unbox = TRUE)
      ))
    }
  }

  paste(input_lines, collapse = "\n")
}

# Internal: Simplify output for single-field results
simplify_output <- function(result, output_type) {
  # If output is a single-field object, extract just that field
  if (inherits(output_type, "ellmer::TypeObject") &&
      length(output_type@properties) == 1) {
    field_name <- names(output_type@properties)[1]
    if (!is.null(result[[field_name]])) {
      return(result[[field_name]])
    }
  }
  result
}
