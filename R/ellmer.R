#' Deep-copy an ellmer type object
#' @noRd
copy_ellmer_type <- function(type) {
  unserialize(serialize(type, NULL))
}

#' Convert a DSPrrr Module to an ellmer Tool
#'
#' @description
#' Creates an ellmer-compatible tool from a dsprrr module. This allows modules
#' to be used as tools in ellmer Chat objects, enabling agentic workflows where
#' the LLM can call dsprrr modules as part of its reasoning.
#'
#' @param module A DSPrrr module (created with [module()]).
#' @param name Optional tool name. Defaults to a name derived from the signature.
#' @param description Optional tool description. Defaults to the signature's
#'   instructions or a generated description.
#' @param .llm Optional ellmer Chat object for the module to use when called.
#'   If not provided, the module's stored chat or default chat is used.
#' @param annotations Optional ellmer tool annotations list, passed through to
#'   [ellmer::tool()]. This lets downstream runtimes reason about properties
#'   such as read-only or destructive behavior without dsprrr depending on them.
#' @param output Tool result serialization mode:
#'   - `"auto"` returns the native module result.
#'   - `"json"` returns stable compact JSON.
#'   - `"text"` returns the primary output field as text when possible, or JSON
#'     text otherwise.
#'   - `"raw"` returns the native module result unchanged.
#' @param copy Whether tool calls should use the supplied module directly
#'   (`"none"`) or a fresh deep copy (`"deep"`).
#' @param error Tool error handling:
#'   - `"reject"` returns a structured recoverable error observation.
#'   - `"abort"` propagates the original error.
#'   - `"return"` returns a structured error object.
#'
#' @return A `ToolDef` object from ellmer, suitable for use with
#'   `ellmer::Chat$register_tool()`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a sentiment analysis module
#' sentiment_mod <- module(
#'   signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
#'   type = "predict"
#' )
#'
#' # Convert to ellmer tool
#' sentiment_tool <- as_ellmer_tool(sentiment_mod, name = "analyze_sentiment")
#'
#' # Register with a Chat for agentic use
#' chat <- ellmer::chat_openai()
#' chat$register_tool(sentiment_tool)
#'
#' # Now the LLM can use the sentiment tool
#' chat$chat("Analyze the sentiment of: 'I love this product!'")
#' }
as_ellmer_tool <- function(
  module,
  name = NULL,
  description = NULL,
  .llm = NULL,
  annotations = list(),
  output = c("auto", "json", "text", "raw"),
  copy = c("none", "deep"),
  error = c("reject", "abort", "return")
) {
  output <- match.arg(output)
  copy <- match.arg(copy)
  error <- match.arg(error)

  if (!inherits(module, "Module")) {
    cli::cli_abort(c(
      "{.arg module} must be a DSPrrr Module object",
      "x" = "Got {.cls {class(module)[1]}}",
      "i" = "Create a module with {.code module()} or {.code as_module()}"
    ))
  }

  # Generate name from signature if not provided
  if (is.null(name)) {
    # Try to extract a meaningful name from the signature
    sig_inputs <- module$signature@inputs
    if (length(sig_inputs) > 0) {
      input_names <- vapply(sig_inputs, function(x) x$name, character(1))
      name <- paste0("dsprrr_", paste(input_names, collapse = "_"))
    } else {
      name <- "dsprrr_module"
    }
  }

  # Generate description from signature if not provided
  if (is.null(description)) {
    instructions <- module$signature@instructions
    if (nzchar(instructions)) {
      description <- instructions
    } else {
      # Generate from input/output structure
      sig_inputs <- module$signature@inputs
      input_names <- if (length(sig_inputs) > 0) {
        vapply(sig_inputs, function(x) x$name, character(1))
      } else {
        "input"
      }
      description <- paste(
        "Process",
        paste(input_names, collapse = ", "),
        "and return structured output"
      )
    }
  }

  # Build argument specification for ellmer::tool() from signature inputs
  arg_specs <- list()
  sig_inputs <- module$signature@inputs
  for (input_spec in sig_inputs) {
    input_name <- input_spec$name
    input_desc <- input_spec$description %||% paste("The", input_name, "value")
    ellmer_type <- copy_ellmer_type(input_spec$type)

    if (is.null(ellmer_type@description) && !is.null(input_desc)) {
      ellmer_type@description <- input_desc
    }

    arg_specs[[input_name]] <- ellmer_type
  }

  # Capture module and llm in closure
  captured_module <- module
  captured_llm <- .llm
  captured_name <- name
  captured_output <- output
  captured_copy <- copy
  captured_error <- error

  # Create a function with named parameters matching the signature inputs
  # ellmer::tool() requires argument names to match function formals
  input_names <- vapply(
    module$signature@inputs,
    function(x) x$name,
    character(1)
  )

  # Create formal arguments list (all default to missing)
  tool_formals <- rlang::set_names(
    rep(list(rlang::missing_arg()), length(input_names)),
    input_names
  )

  # Build function body using bquote to inject the captured variables
  tool_body <- bquote({
    call <- match.call()
    provided_names <- names(as.list(call)[-1])
    inputs <- if (length(provided_names) > 0) {
      mget(provided_names, envir = environment(), inherits = FALSE)
    } else {
      list()
    }

    invoke_ellmer_tool_module(
      module = .(captured_module),
      inputs = inputs,
      .llm = .(captured_llm),
      tool_name = .(captured_name),
      output = .(captured_output),
      copy = .(captured_copy),
      error = .(captured_error)
    )
  })

  # Create the function with proper signature
  # Use the package namespace so `run` and other functions are available
  tool_fn <- rlang::new_function(
    tool_formals,
    tool_body,
    env = rlang::ns_env("dsprrr")
  )

  # Create the ellmer ToolDef using ellmer::tool()
  ellmer::tool(
    tool_fn,
    name = name,
    description = description,
    arguments = arg_specs,
    annotations = annotations
  )
}

#' Invoke a module-backed ellmer tool
#' @noRd
invoke_ellmer_tool_module <- function(
  module,
  inputs,
  .llm,
  tool_name,
  output,
  copy,
  error
) {
  working_module <- if (copy == "deep") {
    module$copy(deep = TRUE)
  } else {
    module
  }

  tryCatch(
    {
      result <- do.call(
        run,
        c(
          list(working_module),
          inputs,
          list(.llm = .llm, .return_format = "simple")
        )
      )
      format_ellmer_tool_output(result, working_module$signature@output_type, output)
    },
    error = function(err) {
      if (error == "abort") {
        stop(err)
      }

      structure_ellmer_tool_error(err, tool_name)
    }
  )
}

#' Convert a module result to the requested ellmer tool output shape
#' @noRd
format_ellmer_tool_output <- function(result, output_type, output) {
  result <- order_tool_result_fields(result, output_type)

  switch(
    output,
    raw = result,
    auto = result,
    json = as.character(jsonlite::toJSON(
      result,
      auto_unbox = TRUE,
      null = "null",
      dataframe = "rows",
      POSIXt = "ISO8601"
    )),
    text = tool_result_text(result, output_type)
  )
}

#' Order named tool result fields according to the signature output type
#' @noRd
order_tool_result_fields <- function(result, output_type) {
  output_names <- output_field_names(output_type)
  if (
    length(output_names) == 0 ||
      !is.list(result) ||
      is.null(names(result))
  ) {
    return(result)
  }

  known <- intersect(output_names, names(result))
  extra <- setdiff(names(result), output_names)
  result[c(known, extra)]
}

#' Render a tool result as text
#' @noRd
tool_result_text <- function(result, output_type) {
  primary <- primary_output_field(output_type)
  if (!is.null(primary) && is.list(result) && primary %in% names(result)) {
    value <- result[[primary]]
    if (is.atomic(value) && length(value) == 1) {
      return(as.character(value))
    }
  }

  if (is.atomic(result) && length(result) == 1) {
    return(as.character(result))
  }

  as.character(jsonlite::toJSON(
    result,
    auto_unbox = TRUE,
    null = "null",
    dataframe = "rows",
    POSIXt = "ISO8601"
  ))
}

#' Return the clear primary output field for a type, if any
#' @noRd
primary_output_field <- function(output_type) {
  output_names <- output_field_names(output_type)
  if (length(output_names) == 1) {
    output_names[[1]]
  } else {
    NULL
  }
}

#' Structured recoverable tool error
#' @noRd
structure_ellmer_tool_error <- function(err, tool_name) {
  list(
    error = TRUE,
    type = class(err)[[1]],
    message = conditionMessage(err),
    tool = tool_name
  )
}

#' Register a DSPrrr Module as a Tool in a Chat
#'
#' @description
#' Convenience function that creates an ellmer tool from a module and registers
#' it with a Chat object in one step.
#'
#' @param chat An ellmer Chat object.
#' @param module A DSPrrr module.
#' @param name Optional tool name.
#' @param description Optional tool description.
#' @param .llm Optional ellmer Chat object for the module to use when called.
#'   If not provided, the module's stored chat or default chat is used.
#' @param annotations Optional ellmer tool annotations list, passed through to
#'   [ellmer::tool()].
#' @param output Tool result serialization mode. See [as_ellmer_tool()].
#' @param copy Whether tool calls should use the supplied module directly or a
#'   fresh deep copy. See [as_ellmer_tool()].
#' @param error Tool error handling mode. See [as_ellmer_tool()].
#'
#' @return The Chat object (invisibly), with the tool registered.
#'
#' @export
#' @examples
#' \dontrun{
#' chat <- ellmer::chat_openai()
#' mod <- module(signature("query -> answer"), type = "predict")
#'
#' # Register in one step
#' register_dsprrr_tool(chat, mod, name = "knowledge_lookup")
#'
#' # Use the tool
#' chat$chat("Use the knowledge_lookup tool to answer: What is R?")
#' }
register_dsprrr_tool <- function(
  chat,
  module,
  name = NULL,
  description = NULL,
  .llm = NULL,
  annotations = list(),
  output = c("auto", "json", "text", "raw"),
  copy = c("none", "deep"),
  error = c("reject", "abort", "return")
) {
  if (!inherits(chat, "Chat")) {
    cli::cli_abort(c(
      "{.arg chat} must be an ellmer Chat object",
      "x" = "Got {.cls {class(chat)[1]}}"
    ))
  }

  output <- match.arg(output)
  copy <- match.arg(copy)
  error <- match.arg(error)

  # Create the ellmer ToolDef from the module

  tool_def <- as_ellmer_tool(
    module,
    name = name,
    description = description,
    .llm = .llm,
    annotations = annotations,
    output = output,
    copy = copy,
    error = error
  )

  # Register the ToolDef with the Chat
  chat$register_tool(tool_def)

  invisible(chat)
}
