#' Deep-copy an ellmer type object
#' @noRd
copy_ellmer_type <- function(type) {
  unserialize(serialize(type, NULL))
}

#' Turn a module into an ellmer tool
#'
#' @description
#' `as_ellmer_tool()` wraps a module as an ellmer tool, so a Chat, or a
#' [react()] agent, can call it during a conversation. The tool's arguments
#' are the module's input fields, with their types and descriptions.
#'
#' @param module A module, such as one created with [module()] or
#'   [module_fn()].
#' @param name Tool name. Defaults to `dsprrr_` followed by the input field
#'   names.
#' @param description Tool description for the model. Defaults to the
#'   signature's instructions.
#' @param .llm The chat the module uses when the tool is called. With `NULL`,
#'   the module's own chat or the default chat (see [get_default_chat()]).
#' @param annotations Tool annotations from [ellmer::tool_annotations()],
#'   which tell a chat or an agent runtime what the tool may do. With `NULL`
#'   (the default), they are inferred from the module. A prediction module,
#'   such as one from [module()] or [chain_of_thought()], only sends its
#'   inputs to the model provider of its chat and changes nothing, so its
#'   tool is marked read-only and closed-world (`read_only_hint = TRUE`,
#'   `open_world_hint = FALSE`). A module compiled with [KNNFewShot()] is
#'   marked the same way when the module it wraps is; its `vectorizer` is
#'   assumed only to compute embeddings. Other modules get no annotations,
#'   because they can run your functions, tools or code: for example
#'   [module_fn()], [react()], [code_act()], [rlm_module()], [flex()],
#'   pipelines and wrappers such as [refine()]. Use `list()` for no
#'   annotations, or pass your own. Agent runtimes may restrict tools without
#'   annotations, for example by treating them as destructive or as needing
#'   network access.
#' @param output How the tool returns its result:
#'   - `"auto"` (the default): the module's output with fields in signature
#'     order.
#'   - `"json"`: compact JSON with fields in signature order.
#'   - `"text"`: the main output field as text if possible, otherwise JSON.
#'   - `"raw"`: the module's output unchanged.
#' @param copy `"none"` (the default) calls `module` itself, so its traces
#'   accumulate; `"deep"` calls a fresh deep copy each time.
#' @param error What happens when the module fails:
#'   - `"reject"` (the default): the tool returns an error description the
#'     model can read and react to. Its `$type` holds the condition class.
#'   - `"abort"`: the error propagates to the caller.
#'   - `"return"`: a `dsprrr_tool_error` condition carrying the error
#'     description in `$payload` is signalled, which a
#'     [withCallingHandlers()] handler can inspect.
#' @param trace_context A named, JSON-compatible list recorded in the
#'   metadata and traces of every call made through the tool, as in [run()].
#'
#' @return An ellmer tool definition (`ToolDef`) for `Chat$register_tool()` or
#'   the `tools` argument of [react()]. It can also be called directly with
#'   the input fields as arguments.
#'
#' @export
#' @family integrations
#' @examples
#' shout <- module_fn("text -> reply", function(text) toupper(text))
#' shout_tool <- as_ellmer_tool(shout, name = "shout", description = "Upper-case text.")
#' shout_tool(text = "quiet please")
#'
#' \dontrun{
#' sentiment <- module(
#'   signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
#' )
#' sentiment_tool <- as_ellmer_tool(sentiment, name = "analyze_sentiment")
#'
#' chat <- ellmer::chat_openai(model = "gpt-6-luna")
#' chat$register_tool(sentiment_tool)
#' chat$chat("What is the sentiment of: 'I love this product!'")
#' }
as_ellmer_tool <- function(
  module,
  name = NULL,
  description = NULL,
  .llm = NULL,
  annotations = NULL,
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
  annotations <- annotations %||% module_tool_annotations(module)

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
  sig_inputs <- caller_input_specs(module)
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
    caller_input_specs(module),
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

#' Tool annotations inferred from a module
#'
#' A Predict module formats a prompt and makes structured requests to its
#' chat, whose provider the host chose; ellmer does not run tools for
#' structured requests. Subclasses such as ReAct and Flex can run tools or
#' code, so only the exact class qualifies, and a KNN wrapper qualifies
#' through the module it wraps. Everything else claims nothing.
#' @noRd
module_tool_annotations <- function(module) {
  if (module_only_predicts(module)) {
    ellmer::tool_annotations(read_only_hint = TRUE, open_world_hint = FALSE)
  } else {
    list()
  }
}

#' @noRd
module_only_predicts <- function(module) {
  switch(
    class(module)[[1L]],
    PredictModule = TRUE,
    KNNFewShotModule = module_only_predicts(module$module),
    FALSE
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

#' Provider properties that ellmer 0.5.0 moved to the Model object
#'
#' Reading them through the Provider is deprecated and returns a stale copy,
#' so provider inspection skips them and reads the Chat's Model instead.
#' @noRd
ellmer_deprecated_provider_props <- c("model", "params", "extra_args")

#' Inspect a Provider without touching deprecated forwarding properties
#' @noRd
ellmer_provider_props <- function(provider) {
  names <- setdiff(S7::prop_names(provider), ellmer_deprecated_provider_props)
  stats::setNames(
    lapply(names, function(name) S7::prop(provider, name)),
    names
  )
}

#' The Model object carrying a Chat's name, params, and extra args
#' @noRd
ellmer_chat_model <- function(chat) {
  model <- tryCatch(chat$get_model_object(), error = function(e) NULL)
  if (inherits(model, "ellmer::Model")) model else NULL
}
