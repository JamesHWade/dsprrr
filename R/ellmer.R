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
#'   - `"auto"` returns the native module result with fields ordered to match
#'     the signature.
#'   - `"json"` returns compact JSON with signature-ordered top-level fields.
#'   - `"text"` returns the primary output field as text when possible, or JSON
#'     text otherwise.
#'   - `"raw"` returns the native module result unchanged, without field
#'     reordering.
#' @param copy Whether tool calls should use the supplied module directly
#'   (`"none"`) or a fresh deep copy (`"deep"`).
#' @param error Tool error handling:
#'   - `"reject"` (default) returns a structured recoverable error observation
#'     suitable for surfacing back to an LLM. The condition class is preserved
#'     in `$type`.
#'   - `"abort"` propagates the original error to the caller.
#'   - `"return"` signals the error as a classed `dsprrr_tool_error` condition
#'     carrying the structured observation in `$payload`. Callers can install
#'     a `withCallingHandlers()` to inspect the failure without aborting.
#' @param trace_context A named, JSON-compatible list captured by the tool and
#'   propagated to dsprrr execution metadata and traces. The tool's declared
#'   result schema is unchanged.
#'
#' @return A `ToolDef` object from ellmer, suitable for use with
#'   `ellmer::Chat$register_tool()`.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create a sentiment analysis module
#' sentiment_mod <- module(
#'   signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
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
  error = c("reject", "abort", "return"),
  trace_context = list()
) {
  trace_context <- trace_context_validate(
    trace_context,
    arg = "trace_context"
  )
  output <- match.arg(output)
  copy <- match.arg(copy)
  error <- match.arg(error)

  if (!inherits(module, "Module")) {
    cli::cli_abort(c(
      "{.arg module} must be a DSPrrr Module object",
      "x" = "Got {.cls {class(module)[1]}}",
      "i" = "Create a module with {.fn module}"
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

    if (
      !inherits(ellmer_type, "ellmer::TypeIgnore") &&
        length(ellmer_type@description) == 0 &&
        !is.null(input_desc)
    ) {
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
  captured_trace_context <- trace_context

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
      error = .(captured_error),
      trace_context = .(captured_trace_context)
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
  error,
  trace_context
) {
  working_module <- if (copy == "deep") {
    module$copy(deep = TRUE)
  } else {
    module
  }

  result <- tryCatch(
    do.call(
      run,
      c(
        list(working_module),
        inputs,
        list(
          .llm = .llm,
          .return_format = "simple",
          .trace_context = trace_context
        )
      )
    ),
    error = function(err) handle_ellmer_tool_error(err, tool_name, error)
  )

  if (inherits(result, "dsprrr_tool_observation")) {
    return(result)
  }

  format_ellmer_tool_output(
    result,
    working_module$signature@output_type,
    output
  )
}

#' Apply the tool error mode to a captured run() error
#' @noRd
handle_ellmer_tool_error <- function(err, tool_name, mode) {
  if (mode == "abort") {
    stop(err)
  }

  observation <- structure_ellmer_tool_error(err, tool_name)
  cli::cli_warn(
    c(
      "Tool {.field {tool_name}} failed: {conditionMessage(err)}",
      "i" = "Returning structured error observation ({.code error = \"{mode}\"})"
    ),
    class = "dsprrr_tool_error_warning",
    .frequency = "always"
  )

  if (mode == "return") {
    rlang::abort(
      conditionMessage(err),
      class = c("dsprrr_tool_error", class(err)),
      payload = observation,
      parent = err
    )
  }

  observation
}

#' Convert a module result to the requested ellmer tool output shape
#' @noRd
format_ellmer_tool_output <- function(result, output_type, output) {
  if (output != "raw") {
    result <- order_tool_result_fields(result, output_type)
  }

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
  structure(
    list(
      error = TRUE,
      type = class(err)[[1]],
      message = conditionMessage(err),
      tool = tool_name
    ),
    class = c("dsprrr_tool_observation", "list")
  )
}
