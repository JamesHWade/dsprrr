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
#'
#' @return A function suitable for use with `ellmer::Chat$register_tool()`.
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
  .llm = NULL
) {
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

  # Build argument specification from signature inputs
  arg_specs <- list()
  sig_inputs <- module$signature@inputs
  for (input_spec in sig_inputs) {
    input_name <- input_spec$name
    input_desc <- input_spec$description %||% paste("The", input_name, "value")
    input_type <- input_spec$type

    # Map ellmer types to JSON schema types
    json_type <- if (inherits(input_type, "ellmer::TypeString")) {
      "string"
    } else if (inherits(input_type, "ellmer::TypeNumber")) {
      "number"
    } else if (inherits(input_type, "ellmer::TypeInteger")) {
      "integer"
    } else if (inherits(input_type, "ellmer::TypeBoolean")) {
      "boolean"
    } else if (inherits(input_type, "ellmer::TypeEnum")) {
      "string"  # Enums are strings with constraints
    } else {
      "string"  # Default to string
    }

    arg_specs[[input_name]] <- list(
      type = json_type,
      description = input_desc
    )
  }

  # Capture module and llm in closure
  captured_module <- module
  captured_llm <- .llm

  # Create the tool function
  tool_fn <- function(...) {
    inputs <- list(...)

    # Run the module
    result <- tryCatch(
      {
        run(
          captured_module,
          !!!inputs,
          .llm = captured_llm,
          .return_format = "simple"
        )
      },
      error = function(e) {
        paste("Error:", e$message)
      }
    )

    # Format result for LLM consumption
    if (is.list(result) && !is.null(names(result))) {
      jsonlite::toJSON(result, auto_unbox = TRUE)
    } else if (is.character(result)) {
      result
    } else {
      as.character(result)
    }
  }

  # Attach metadata for ellmer tool registration
  attr(tool_fn, "name") <- name
  attr(tool_fn, "description") <- description
  attr(tool_fn, "arguments") <- arg_specs

  # Set class for ellmer compatibility
  class(tool_fn) <- c("dsprrr_tool", "function")

  tool_fn
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
  description = NULL
) {
  if (!inherits(chat, "Chat")) {
    cli::cli_abort(c(
      "{.arg chat} must be an ellmer Chat object",
      "x" = "Got {.cls {class(chat)[1]}}"
    ))
  }

  tool <- as_ellmer_tool(module, name = name, description = description)

  # Register with ellmer
  # ellmer expects tools registered via register_tool() method
  chat$register_tool(
    tool,
    name = attr(tool, "name"),
    description = attr(tool, "description")
  )

  invisible(chat)
}

#' Print method for dsprrr_tool
#' @param x A dsprrr_tool object
#' @param ... Additional arguments (unused)
#' @export
print.dsprrr_tool <- function(x, ...) {
  cli::cli_h3("DSPrrr Tool")
  cli::cli_text("{.field Name}: {attr(x, 'name')}")
  cli::cli_text("{.field Description}: {attr(x, 'description')}")

  args <- attr(x, "arguments")
  if (length(args) > 0) {
    cli::cli_text("{.field Arguments}:")
    for (name in names(args)) {
      arg <- args[[name]]
      cli::cli_text("  {.field {name}} ({arg$type}): {arg$description}")
    }
  }

  invisible(x)
}
