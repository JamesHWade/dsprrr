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
#' @param .simplify Logical. If `TRUE` (default), single-field outputs are
#'   simplified to just the value. If `FALSE`, always returns a named list.
#'
#' @return The structured output according to the signature's output type.
#'   When `.simplify = TRUE` (default): single-field outputs return just the
#'   value, multi-field outputs return a named list.
#'   When `.simplify = FALSE`: always returns a named list.
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
#'
#' # Control output simplification
#' dsp("q -> answer", q = "Hi", .simplify = TRUE)   # Returns: "Hello"
#' dsp("q -> answer", q = "Hi", .simplify = FALSE)  # Returns: list(answer = "Hello")
#' }
dsp <- function(x, ...) {
  UseMethod("dsp")
}

#' @rdname dsp
#' @export
dsp.Chat <- function(
    x,
    signature,
    ...,
    .instructions = NULL,
    .echo = "none",
    .simplify = TRUE
) {
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

  # Get model info for error context
  model_name <- tryCatch(chat$get_model(), error = function(e) "unknown")
  provider_name <- tryCatch(
    detect_provider_name(chat),
    error = function(e) "unknown"
  )

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
      # Provide context-rich error message
      wrap_llm_error(e, model_name, provider_name, full_prompt)
    }
  )

  # Capture Turn objects for metadata extraction
  assistant_turn <- tryCatch(
    chat$last_turn(role = "assistant"),
    error = function(e) NULL
  )
  user_turn <- tryCatch(
    chat$last_turn(role = "user"),
    error = function(e) NULL
  )

  # Extract model name
  model <- tryCatch(chat$get_model(), error = function(e) NA_character_)

  # Store trace for debugging
  store_last_trace(list(
    signature = sig,
    inputs = inputs,
    prompt = full_prompt,
    output = result,
    timestamp = Sys.time(),
    user_turn = user_turn,
    assistant_turn = assistant_turn,
    model = model
  ))

  # Simplify single-field results (if requested)
  simplify_output(result, sig@output_type, simplify = .simplify)
}

#' @rdname dsp
#' @export
dsp.character <- function(
    x,
    ...,
    .instructions = NULL,
    .echo = "none",
    .simplify = TRUE
) {
  # x is the signature string, use default chat
  chat <- get_default_chat()
  dsp.Chat(
    chat,
    signature = x,
    ...,
    .instructions = .instructions,
    .echo = .echo,
    .simplify = .simplify
  )
}

#' @rdname dsp
#' @export
`dsp.dsprrr::Signature` <- function(
    x,
    ...,
    .instructions = NULL,
    .echo = "none",
    .simplify = TRUE
) {
  # x is a Signature object, use default chat
  chat <- get_default_chat()
  dsp.Chat(
    chat,
    signature = x,
    ...,
    .instructions = .instructions,
    .echo = .echo,
    .simplify = .simplify
  )
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
#' trace <- get_last_trace()
#' trace$prompt  # See the prompt that was sent
#' }
get_last_trace <- function() {
  .dsprrr_env$last_trace
}

# Internal: Store trace for get_last_trace() and global history
store_last_trace <- function(trace) {
  .dsprrr_env$last_trace <- trace
  # Also add to global prompt history for inspect_history()
  add_to_global_history(trace, source = "dsp()")
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
    # Build error message with suggestions
    error_parts <- c("Missing required inputs")

    for (field in missing) {
      error_parts <- c(error_parts, "x" = "Missing: {.field {field}}")

      # Check if there's a close match in provided names
      if (length(provided_names) > 0) {
        suggestion <- find_closest_match(field, provided_names)
        if (!is.null(suggestion)) {
          error_parts <- c(
            error_parts,
            " " = "  Did you mean: {.field {suggestion}}?"
          )
        }
      }
    }

    error_parts <- c(
      error_parts,
      "i" = "Signature requires: {.field {required_names}}"
    )

    cli::cli_abort(error_parts)
  }

  # Warn about extra inputs with suggestions
  extra <- setdiff(provided_names, required_names)
  if (length(extra) > 0) {
    for (field in extra) {
      suggestion <- find_closest_match(field, required_names)
      if (!is.null(suggestion)) {
        cli::cli_warn(c(
          "Unknown input: {.field {field}}",
          "i" = "Did you mean: {.field {suggestion}}?",
          "i" = "Available fields: {.field {required_names}}"
        ))
      } else {
        cli::cli_warn(c(
          "Ignoring unknown input: {.field {field}}",
          "i" = "Available fields: {.field {required_names}}"
        ))
      }
    }
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
      input_lines <- c(
        input_lines,
        paste0(
          name,
          ": ",
          jsonlite::toJSON(value, auto_unbox = TRUE)
        )
      )
    }
  }

  paste(input_lines, collapse = "\n")
}

# Internal: Simplify output for single-field results
simplify_output <- function(result, output_type, simplify = TRUE) {
  # If simplification disabled, always return the full result

  if (!simplify) {
    return(result)
  }

  # If output is a single-field object, extract just that field
  if (
    inherits(output_type, "ellmer::TypeObject") &&
      length(output_type@properties) == 1
  ) {
    field_name <- names(output_type@properties)[1]
    if (!is.null(result[[field_name]])) {
      return(result[[field_name]])
    }
  }
  result
}

# Internal: Wrap LLM errors with helpful context
wrap_llm_error <- function(e, model_name, provider_name, prompt) {
  # Parse the error message to provide helpful suggestions
  error_msg <- conditionMessage(e)
  error_lower <- tolower(error_msg)

  # Build context-aware error message
  error_parts <- c("LLM call failed")

  # Add the original error
  error_parts <- c(error_parts, "x" = error_msg)

  # Add model context
  error_parts <- c(
    error_parts,
    "i" = "Model: {model_name} via {provider_name}"
  )

  # Detect common error patterns and provide suggestions
  if (grepl("rate.?limit|too.?many.?requests|429", error_lower)) {
    error_parts <- c(
      error_parts,
      "!" = "Rate limit exceeded",
      "i" = "Suggestion: Wait a few seconds and try again, or use a different model"
    )
  } else if (grepl("api.?key|auth|401|403|unauthorized", error_lower)) {
    error_parts <- c(
      error_parts,
      "!" = "Authentication failed",
      "i" = "Check that your API key is set correctly",
      "i" = "Run {.code dsprrr_sitrep()} to check configuration"
    )
  } else if (grepl("timeout|timed.?out", error_lower)) {
    error_parts <- c(
      error_parts,
      "!" = "Request timed out",
      "i" = "Try reducing prompt length or using a faster model"
    )
  } else if (grepl("context.?length|token.?limit|too.?long", error_lower)) {
    prompt_len <- nchar(prompt)
    error_parts <- c(
      error_parts,
      "!" = "Prompt too long ({prompt_len} characters)",
      "i" = "Reduce input size or use a model with larger context window"
    )
  } else if (grepl("invalid.?json|parse|format", error_lower)) {
    error_parts <- c(
      error_parts,
      "!" = "Response parsing failed",
      "i" = "The LLM returned invalid JSON. Try simplifying the output type",
      "i" = "Check {.code get_last_prompt()} to see the raw response"
    )
  } else if (grepl("content.?filter|safety|blocked", error_lower)) {
    error_parts <- c(
      error_parts,
      "!" = "Content was blocked by safety filters",
      "i" = "Rephrase your input to avoid triggering content filters"
    )
  }

  # Add debugging tip
  error_parts <- c(
    error_parts,
    "i" = "Use {.code get_last_prompt()} to inspect the prompt that was sent"
  )

  cli::cli_abort(error_parts, parent = e)
}
