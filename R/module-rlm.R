#' Recursive Language Model (RLM) Module
#'
#' @description
#' An experimental inference-time analyst for inputs whose useful evidence is
#' too large, irregular, or unpredictable to place in one prompt. RLM keeps the
#' inputs in an R environment and lets the model iteratively inspect summaries,
#' run computations, and decide what to examine next.
#'
#' @details
#' Each input is available under `.context`. Generated R code can use ordinary R
#' plus `peek()`, `search()`, value-returning `llm_query()` calls, declared host
#' tools, and `SUBMIT(...)`. RLM requires a runner whose `policy()` advertises
#' `persistent = TRUE`; variables therefore remain available across turns within
#' one invocation. Invalid submitted fields or types become iteration errors
#' that the model can repair. If no valid submission is produced, the separate
#' `extract` predictor performs one typed fallback.
#' RLM validates explicit ellmer string, number, integer, boolean, enum, array,
#' and object outputs. Opaque `TypeJsonSchema` nodes are rejected at
#' construction because they cannot participate in this strict repair loop.
#'
#' The `generate_action` and `extract` predictors are graph-visible through
#' [named_modules()]. GEPA can tune them; nested MIPROv2 is instruction-only
#' with bootstrapped demos disabled. BootstrapFewShot and LabeledFewShot reject
#' programs containing an RLM until predictor-local demonstrations are
#' available.
#' Model-visible execution evidence defaults to a 10,000-character head-and-tail view
#' after any runner-level transport limit; that formatted evidence is retained
#' in the returned trajectory. Trace state retains hashes and sizes for input
#' objects, not their full values.
#' Structured metadata separates logical action, recursive, and extraction
#' counts from verified provider calls. A child-predictor cache hit contributes
#' zero provider calls and zero current-run usage; totals remain `NA` whenever
#' every contributing provider turn cannot be verified.
#'
#' For generated code, prefer a fresh sandboxed [mcp_repl_runner()] factory.
#' Its managed transport is intentionally bounded and is best for compact,
#' JSON-compatible context. Its default policy disables network access but
#' allows writes within the configured workspace. Declared host tools execute
#' in the dsprrr host process, outside that guest sandbox. This backend requires
#' the suggested `mcptools` package and Posit's external `mcp-repl` executable.
#' For large data frames or richer local R objects,
#' `r_code_runner(persistent = TRUE)` can stage context once, but it is
#' trusted-input-only: the child process retains the host user's file, network,
#' and environment permissions.
#'
#' Supply exactly one runtime source. A caller-owned `runner` is reused and
#' never closed by dsprrr. An `interpreter_factory` creates one invocation-owned
#' runner which dsprrr shuts down on success, error, or interrupt. Factory-backed
#' RLM supports [run_async()] and isolated [run_dataset()] execution; token
#' streaming is unavailable. A direct [run()] call always stages each supplied
#' value as one REPL variable, regardless of its R length. Use [run_dataset()]
#' for multiple invocations, with list-columns for data frames, vectors, lists,
#' matrices, or other rich per-row values.
#'
#' @examples
#' \dontrun{
#' analyst <- rlm_module(
#'   signature = "document, question -> answer",
#'   interpreter_factory = function() mcp_repl_runner(timeout = 30)
#' )
#' compact_doc <- "Section 1: ...\nSection 2: ..."
#' result <- run(
#'   analyst,
#'   document = compact_doc,
#'   question = "What evidence supports the conclusion?",
#'   .llm = ellmer::chat_openai()
#' )
#' }
#'
#' @name module-rlm
NULL


#' Create a Recursive Language Model (RLM) Module
#'
#' @description
#' Create an RLM whose implementation can adaptively explore R objects at
#' inference time. Use RLM when the inspection path is not known in advance;
#' use ordinary R when that path becomes stable, or Flex when labeled examples
#' should discover a reusable implementation.
#'
#' @param signature A Signature object or string notation defining inputs/outputs
#'   with explicit ellmer string, number, integer, boolean, enum, array, or
#'   object output types. Opaque `TypeJsonSchema` outputs are unsupported.
#' @param runner Optional caller-owned code runner implementing `execute()` and
#'   `policy()`. Its policy must declare `persistent = TRUE`. It is retained,
#'   never automatically closed, and must not be shared concurrently. For the
#'   trusted callr backend, use `r_code_runner(persistent = TRUE)`.
#' @param max_iterations Maximum REPL iterations before fallback (default 20)
#' @param max_iters DSPy 3.3-compatible alias for `max_iterations`. Supply only
#'   one of these arguments.
#' @param interpreter_factory Optional zero-argument function returning a fresh
#'   runner with `execute()`, `policy()`, optional `start()`, and idempotent
#'   terminal `shutdown()` or `close()`. Its policy must advertise
#'   `persistent = TRUE` for RLM.
#'   Supply exactly one of `runner` and `interpreter_factory`.
#' @param max_llm_calls Maximum recursive LLM calls allowed (default 50)
#' @param max_output_chars Maximum model-visible characters per execution output.
#'   Longer output is shown as a head-and-tail excerpt. Default 10000.
#' @param sub_lm Optional ellmer Chat for recursive queries. `NULL` inherits the
#'   invocation's outer Chat. Set `max_llm_calls = 0` to disable recursion.
#' @param verbose Logical. Print execution progress (default FALSE)
#' @param tools Named list of user-defined host functions or ellmer ToolDef
#'   objects. Guest code emits an
#'   invocation-bound request, dsprrr validates it and invokes the original
#'   function in the host,
#'   and the guest is replayed with the response. Closures are never deparsed or
#'   serialized into generated code. These tools execute in the host process,
#'   outside the guest runner sandbox, with the host's permissions. ToolDef
#'   schemas guide generation; the callable must still enforce semantic
#'   constraints beyond the bridge's lossless JSON-compatible value checks.
#'   A protocol safety ceiling permits at most 1,000 host-tool calls in one
#'   generated R step.
#' @param ... Additional arguments passed to the module
#'
#' @return An RLMModule object
#'
#' @export
#' @examples
#' \dontrun{
#' analyst <- rlm_module(
#'   "document, question -> answer",
#'   interpreter_factory = function() mcp_repl_runner(timeout = 30)
#' )
#' result <- run(
#'   analyst,
#'   document = "Owner: team-a\nObligation: rotate keys quarterly",
#'   question = "Which obligations have no owner?",
#'   .llm = ellmer::chat_openai()
#' )
#' }
rlm_module <- function(
  signature,
  runner = NULL,
  max_iterations = 20L,
  max_llm_calls = 50L,
  max_output_chars = 10000L,
  sub_lm = NULL,
  verbose = FALSE,
  tools = list(),
  max_iters = NULL,
  ...,
  interpreter_factory = NULL
) {
  binding <- normalize_code_runner_binding(
    runner = runner,
    interpreter_factory = interpreter_factory,
    module_name = "RLM"
  )

  # Parse signature if string
  if (is.character(signature)) {
    signature <- signature(signature)
  }

  if (!S7::S7_inherits(signature, Signature)) {
    cli::cli_abort(c(
      "signature must be a Signature object or string notation",
      "x" = "You provided: {.cls {class(signature)[1]}}"
    ))
  }

  if (!is.null(max_iters)) {
    if (!missing(max_iterations)) {
      cli::cli_abort(
        "Supply only one of {.arg max_iterations} and {.arg max_iters}",
        class = "dsprrr_rlm_argument_conflict"
      )
    }
    max_iterations <- max_iters
  }

  # Validate before coercion so fractional, vector, infinite, and out-of-range
  # values cannot be silently truncated or converted to NA.
  max_iterations <- normalize_rlm_bound(
    max_iterations,
    "max_iterations",
    minimum = 1L
  )
  max_llm_calls <- normalize_rlm_bound(
    max_llm_calls,
    "max_llm_calls",
    minimum = 0L
  )
  max_output_chars <- normalize_rlm_bound(
    max_output_chars,
    "max_output_chars",
    minimum = 1L
  )

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    cli::cli_abort(
      "{.arg verbose} must be TRUE or FALSE",
      class = "dsprrr_rlm_argument_error"
    )
  }

  validate_rlm_sub_lm(sub_lm)

  validate_rlm_tools(tools)

  RLMModule$new(
    signature = signature,
    runner = binding$runner,
    interpreter_factory = binding$interpreter_factory,
    max_iterations = max_iterations,
    max_llm_calls = max_llm_calls,
    max_output_chars = max_output_chars,
    sub_lm = sub_lm,
    verbose = verbose,
    tools = tools,
    ...
  )
}


validate_rlm_sub_lm <- function(sub_lm) {
  if (is.null(sub_lm)) {
    return(invisible(sub_lm))
  }
  chat <- tryCatch(sub_lm[["chat"]], error = function(e) NULL)
  if (!is.function(chat)) {
    cli::cli_abort(
      c(
        "{.arg sub_lm} must be an ellmer Chat or compatible object",
        "i" = "It must provide a {.code chat(prompt)} method."
      ),
      class = "dsprrr_rlm_sub_lm_error"
    )
  }
  invisible(sub_lm)
}


rlm_type_contains_json_schema <- function(type) {
  if (inherits(type, "ellmer::TypeJsonSchema")) {
    return(TRUE)
  }
  if (inherits(type, "ellmer::TypeArray")) {
    return(rlm_type_contains_json_schema(type@items))
  }
  if (inherits(type, "ellmer::TypeObject")) {
    return(any(vapply(
      type@properties,
      rlm_type_contains_json_schema,
      logical(1)
    )))
  }
  FALSE
}


validate_rlm_output_type <- function(type) {
  if (rlm_type_contains_json_schema(type)) {
    cli::cli_abort(
      c(
        "RLM cannot strictly validate opaque TypeJsonSchema outputs",
        "i" = paste(
          "Use ellmer type_string(), type_number(), type_integer(),",
          "type_boolean(), type_enum(), type_array(), or type_object()."
        )
      ),
      class = c(
        "dsprrr_rlm_json_schema_unsupported",
        "dsprrr_rlm_signature_error"
      )
    )
  }
  invisible(type)
}


clone_rlm_chat <- function(chat) {
  clone <- tryCatch(chat[["clone"]], error = function(e) NULL)
  if (is.function(clone)) {
    cloned <- tryCatch(
      clone(deep = TRUE),
      error = function(e) clone()
    )
    set_turns <- tryCatch(cloned[["set_turns"]], error = function(e) NULL)
    if (is.function(set_turns)) {
      set_turns(list())
    }
    return(cloned)
  }
  chat
}


new_rlm_action_module <- function() {
  PredictModule$new(
    signature = signature(
      inputs = list(input(
        "state",
        type = ellmer::type_string(),
        description = "RLM task, available variables, and prior REPL trajectory"
      )),
      output_type = ellmer::type_object(
        reasoning = ellmer::type_string(
          description = "Brief rationale for the next operation"
        ),
        code = ellmer::type_string(
          description = "One non-empty R expression or block to execute"
        )
      ),
      instructions = paste(
        "Choose the next useful R operation for an iterative investigation.",
        "Use the supplied state as the complete source of task and trajectory context."
      )
    )
  )
}


new_rlm_extract_module <- function(output_type) {
  PredictModule$new(
    signature = signature(
      inputs = list(input(
        "state",
        type = ellmer::type_string(),
        description = "Original task and bounded RLM trajectory"
      )),
      output_type = output_type,
      instructions = paste(
        "Return the best supported typed answer from the trajectory.",
        "Do not invent evidence that the RLM did not observe."
      )
    )
  )
}


rlm_predictor_abort <- function(predictor, message, class) {
  cli::cli_abort(
    c(
      "RLM {.field {predictor}} predictor has an incompatible signature",
      "x" = message
    ),
    class = c(
      class,
      "dsprrr_rlm_predictor_contract_error",
      "dsprrr_rlm_predictor_error"
    ),
    predictor = predictor
  )
}


rlm_predictor_type_contract <- function(type) {
  required <- tryCatch(isTRUE(type@required), error = function(error) NULL)
  if (is.null(required)) {
    return(NULL)
  }

  if (inherits(type, "ellmer::TypeIgnore")) {
    return(list(kind = "ignore", required = required))
  }
  if (inherits(type, "ellmer::TypeJsonSchema")) {
    return(list(
      kind = "json_schema",
      json = type@json,
      required = required
    ))
  }
  if (inherits(type, "ellmer::TypeBasic")) {
    return(list(
      kind = "basic",
      type = type@type,
      required = required
    ))
  }
  if (inherits(type, "ellmer::TypeEnum")) {
    return(list(
      kind = "enum",
      values = type@values,
      required = required
    ))
  }
  if (inherits(type, "ellmer::TypeArray")) {
    items <- rlm_predictor_type_contract(type@items)
    if (is.null(items)) {
      return(NULL)
    }
    return(list(
      kind = "array",
      items = items,
      required = required
    ))
  }
  if (inherits(type, "ellmer::TypeObject")) {
    properties <- lapply(type@properties, rlm_predictor_type_contract)
    if (any(vapply(properties, is.null, logical(1)))) {
      return(NULL)
    }
    return(list(
      kind = "object",
      properties = properties,
      required = required,
      additional_properties = isTRUE(type@additional_properties)
    ))
  }
  NULL
}


rlm_predictor_required_string <- function(type) {
  inherits(type, "ellmer::TypeBasic") &&
    identical(type@type, "string") &&
    isTRUE(type@required)
}


validate_rlm_predictor_state_input <- function(signature, predictor, class) {
  input_names <- vapply(signature@inputs, `[[`, character(1), "name")
  valid <- length(signature@inputs) == 1L &&
    identical(input_names, "state") &&
    rlm_predictor_required_string(signature@inputs[[1L]]$type)
  if (!valid) {
    rlm_predictor_abort(
      predictor,
      "It must accept exactly one required string input named {.field state}.",
      class
    )
  }
  invisible(signature)
}


validate_rlm_action_predictor <- function(module) {
  predictor <- "generate_action"
  class <- "dsprrr_rlm_generate_action_contract_error"
  signature <- tryCatch(module$signature, error = function(error) NULL)
  if (!S7::S7_inherits(signature, Signature)) {
    rlm_predictor_abort(
      predictor,
      "It must expose a dsprrr Signature.",
      class
    )
  }
  validate_rlm_predictor_state_input(signature, predictor, class)

  output_type <- signature@output_type
  valid <- inherits(output_type, "ellmer::TypeObject") &&
    isTRUE(output_type@required) &&
    !isTRUE(output_type@additional_properties) &&
    identical(names(output_type@properties), c("reasoning", "code")) &&
    all(vapply(
      output_type@properties,
      rlm_predictor_required_string,
      logical(1)
    ))
  if (!valid) {
    rlm_predictor_abort(
      predictor,
      paste(
        "It must return exactly the required string fields",
        "{.field reasoning} and {.field code}."
      ),
      class
    )
  }
  invisible(module)
}


validate_rlm_extract_predictor <- function(module, output_type) {
  predictor <- "extract"
  class <- "dsprrr_rlm_extract_contract_error"
  signature <- tryCatch(module$signature, error = function(error) NULL)
  if (!S7::S7_inherits(signature, Signature)) {
    rlm_predictor_abort(
      predictor,
      "It must expose a dsprrr Signature.",
      class
    )
  }
  validate_rlm_predictor_state_input(signature, predictor, class)

  actual <- rlm_predictor_type_contract(signature@output_type)
  expected <- rlm_predictor_type_contract(output_type)
  if (is.null(actual) || is.null(expected) || !identical(actual, expected)) {
    rlm_predictor_abort(
      predictor,
      paste(
        "Its output type must equal the RLM output type, including field",
        "structure and required flags. Descriptions may differ."
      ),
      class
    )
  }
  invisible(module)
}


validate_rlm_predictor_children <- function(children, output_type) {
  if (
    !is.list(children) ||
      !identical(names(children), c("generate_action", "extract")) ||
      !inherits(children$generate_action, "Module") ||
      !inherits(children$extract, "Module")
  ) {
    cli::cli_abort(
      "RLM graph children must be named generate_action and extract modules",
      class = c(
        "dsprrr_rlm_predictor_structure_error",
        "dsprrr_rlm_predictor_error"
      )
    )
  }
  validate_rlm_action_predictor(children$generate_action)
  validate_rlm_extract_predictor(children$extract, output_type)
  invisible(children)
}


rlm_reserved_tool_names <- function() {
  c(
    ".context",
    "SUBMIT",
    "print",
    "peek",
    "search",
    "llm_query",
    "llm_query_batched",
    "rlm_query",
    "rlm_query_batch"
  )
}

normalize_rlm_bound <- function(value, name, minimum) {
  valid <- is.numeric(value) &&
    length(value) == 1L &&
    !is.na(value) &&
    is.finite(value) &&
    value == floor(value) &&
    value >= minimum &&
    value <= .Machine$integer.max
  if (!valid) {
    message <- switch(
      name,
      max_iterations = "max_iterations must be at least 1",
      max_llm_calls = "max_llm_calls must be non-negative",
      max_output_chars = "max_output_chars must be a positive integer",
      paste0(name, " is outside its supported integer range")
    )
    cli::cli_abort(message, class = "dsprrr_rlm_bounds_error")
  }
  as.integer(value)
}

validate_rlm_tools <- function(tools) {
  if (!is.list(tools)) {
    cli::cli_abort(
      c(
        "tools must be a named list of functions",
        "x" = "You provided: {.cls {class(tools)[1]}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }
  if (length(tools) == 0L) {
    return(invisible(tools))
  }

  tool_names <- names(tools)
  if (is.null(tool_names)) {
    cli::cli_abort(
      c(
        "tools must be a named list",
        "i" = "Example: {.code tools = list(my_tool = function(...) ...)}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }
  if (
    anyNA(tool_names) ||
      !all(nzchar(tool_names))
  ) {
    cli::cli_abort(
      c(
        "tools must have non-empty, non-missing names",
        "i" = "Example: {.code tools = list(my_tool = function(...) ...)}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }
  if (anyDuplicated(tool_names)) {
    duplicates <- unique(tool_names[duplicated(tool_names)])
    cli::cli_abort(
      c(
        "Tool names must be unique",
        "x" = "Duplicate name{?s}: {.val {duplicates}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  non_functions <- vapply(tools, Negate(is.function), logical(1))
  if (any(non_functions)) {
    bad_names <- tool_names[non_functions]
    cli::cli_abort(
      c(
        "All tools must be functions",
        "x" = "Non-function tool{?s}: {.val {bad_names}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  invalid_names <- tool_names[
    make.names(tool_names) != tool_names |
      tool_names == "..." |
      grepl("^\\.\\.[0-9]+$", tool_names)
  ]
  if (length(invalid_names) > 0L) {
    cli::cli_abort(
      c(
        "Tool names must be valid R identifiers",
        "x" = "Invalid name{?s}: {.val {invalid_names}}",
        "i" = "Names cannot use ellipsis pronouns such as ... or ..1."
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  collisions <- intersect(tool_names, rlm_reserved_tool_names())
  if (length(collisions) > 0L) {
    cli::cli_abort(
      c(
        "Tool names conflict with built-in RLM tools",
        "x" = "Reserved name{?s}: {.val {collisions}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  internal_collisions <- tool_names[grepl("^\\.rlm_", tool_names)]
  if (length(internal_collisions) > 0L) {
    cli::cli_abort(
      c(
        "Tool names conflict with internal RLM bindings",
        "x" = "Internal name{?s}: {.val {internal_collisions}}"
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  base_collisions <- intersect(tool_names, ls(baseenv(), all.names = TRUE))
  if (length(base_collisions) > 0L) {
    cli::cli_abort(
      c(
        "Tool names must not mask base R functions",
        "x" = "Base name{?s}: {.val {base_collisions}}",
        "i" = "Use a domain-specific tool name instead."
      ),
      class = "dsprrr_rlm_tools_error"
    )
  }

  invisible(tools)
}


format_rlm_tool_contract <- function(tool, name) {
  description <- NULL
  argument_text <- NULL

  if (inherits(tool, "ellmer::ToolDef")) {
    description <- tool@description
    properties <- tool@arguments@properties
    if (length(properties) > 0L) {
      argument_text <- vapply(
        names(properties),
        function(argument) {
          spec <- properties[[argument]]
          required <- if (isTRUE(spec@required)) "" else " = NULL"
          paste0(argument, ": ", format_ellmer_type(spec), required)
        },
        character(1)
      )
    }
  }

  if (is.null(argument_text)) {
    fmls <- formals(tool)
    if (!is.null(fmls)) {
      missing_defaults <- vapply(fmls, rlang::is_missing, logical(1))
      argument_text <- vapply(
        names(fmls),
        function(argument) {
          if (isTRUE(missing_defaults[[argument]])) {
            argument
          } else {
            paste0(argument, " = <default>")
          }
        },
        character(1)
      )
    }
    description <- attr(tool, "description", exact = TRUE) %||% description
  }

  signature_text <- paste0(
    name,
    "(",
    paste(argument_text %||% character(), collapse = ", "),
    ")"
  )
  if (
    is.character(description) &&
      length(description) == 1L &&
      !is.na(description) &&
      nzchar(description)
  ) {
    paste0("- `", signature_text, "`: ", description)
  } else {
    paste0("- `", signature_text, "`: User-defined host tool")
  }
}

normalize_rlm_sub_lm_text <- function(response) {
  text <- if (is.character(response)) {
    response
  } else if (inherits(response, "S7_object")) {
    tryCatch(response@text, error = function(e) NULL)
  } else if (is.list(response) && !is.null(response$text)) {
    response$text
  } else {
    NULL
  }
  if (
    !is.character(text) ||
      length(text) != 1L ||
      is.na(text) ||
      !nzchar(text)
  ) {
    cli::cli_abort(
      c(
        "Recursive sub-LM returned an invalid response",
        "i" = "Expected one non-empty text response, got {.cls {class(response)[1]}}."
      ),
      class = "dsprrr_rlm_sub_lm_response_error"
    )
  }
  text
}


is_rlm_provider_error <- function(error) {
  inherits(
    error,
    c(
      "httr2_error",
      "httr2_failure",
      "ellmer_error",
      "ellmer::LMError",
      "dsprrr_rlm_provider_error"
    )
  )
}


#' RLM Module R6 Class
#'
#' @description
#' R6 class implementing the Recursive Language Model pattern: LLM-driven
#' REPL exploration of context with programmatic tools.
#'
#' @keywords internal
#' @noRd
RLMModule <- R6::R6Class(
  "RLMModule",
  inherit = Module,
  public = list(
    #' @field runner Code runner for code execution
    runner = NULL,

    #' @field interpreter_factory Factory for an invocation-owned code runner
    interpreter_factory = NULL,

    #' @field max_iterations Maximum REPL iterations before fallback
    max_iterations = NULL,

    #' @field max_llm_calls Maximum recursive LLM calls
    max_llm_calls = NULL,

    #' @field max_output_chars Maximum output size per execution
    max_output_chars = NULL,

    #' @field sub_lm Optional LLM for recursive queries
    sub_lm = NULL,

    #' @field verbose Whether to print execution progress
    verbose = NULL,

    #' @field tools User-defined REPL tools
    tools = NULL,

    #' @field generate_action Optimizable predictor for the next R operation
    generate_action = NULL,

    #' @field extract Optimizable predictor for typed fallback extraction
    extract = NULL,

    #' @description
    #' Initialize an RLMModule
    #'
    #' @param signature Signature object defining inputs/outputs
    #' @param runner Code runner for code execution
    #' @param max_iterations Maximum REPL iterations
    #' @param max_llm_calls Maximum recursive LLM calls
    #' @param max_output_chars Maximum output size
    #' @param sub_lm Optional LLM for recursive queries
    #' @param verbose Whether to print progress
    #' @param tools User-defined tools
    #' @param config Optional configuration list
    #' @param chat Optional ellmer Chat object
    #' @param interpreter_factory Optional zero-argument invocation-owned runner
    #'   factory. Supply exactly one of this and `runner`.
    initialize = function(
      signature,
      runner = NULL,
      max_iterations = 20L,
      max_llm_calls = 50L,
      max_output_chars = 10000L,
      sub_lm = NULL,
      verbose = FALSE,
      tools = list(),
      config = list(),
      chat = NULL,
      interpreter_factory = NULL,
      generate_action = NULL,
      extract = NULL
    ) {
      binding <- normalize_code_runner_binding(
        runner = runner,
        interpreter_factory = interpreter_factory,
        module_name = "RLM"
      )
      if (!S7::S7_inherits(signature, Signature)) {
        cli::cli_abort(
          "{.arg signature} must be a Signature object",
          class = "dsprrr_rlm_signature_error"
        )
      }
      validate_rlm_output_type(signature@output_type)
      signature_inputs <- vapply(
        signature@inputs,
        function(input) input$name,
        character(1)
      )
      if (anyDuplicated(signature_inputs)) {
        duplicates <- unique(signature_inputs[duplicated(signature_inputs)])
        cli::cli_abort(
          c(
            "RLM signature input names must be unique",
            "x" = "Duplicate name{?s}: {.field {duplicates}}"
          ),
          class = "dsprrr_rlm_signature_error"
        )
      }
      reserved_replay_fields <- c(
        rlm_control_replay_field(),
        rlm_host_tool_replay_field(),
        rlm_query_replay_field()
      )
      if (any(reserved_replay_fields %in% signature_inputs)) {
        reserved <- intersect(reserved_replay_fields, signature_inputs)
        cli::cli_abort(
          "RLM signature input {.field {reserved}} is reserved for bridge replay",
          class = "dsprrr_rlm_signature_error"
        )
      }
      validate_rlm_tools(tools)
      max_iterations <- normalize_rlm_bound(
        max_iterations,
        "max_iterations",
        minimum = 1L
      )
      max_llm_calls <- normalize_rlm_bound(
        max_llm_calls,
        "max_llm_calls",
        minimum = 0L
      )
      max_output_chars <- normalize_rlm_bound(
        max_output_chars,
        "max_output_chars",
        minimum = 1L
      )
      if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
        cli::cli_abort(
          "{.arg verbose} must be TRUE or FALSE",
          class = "dsprrr_rlm_argument_error"
        )
      }
      validate_rlm_sub_lm(sub_lm)
      generate_action <- generate_action %||% new_rlm_action_module()
      extract <- extract %||% new_rlm_extract_module(signature@output_type)
      validate_rlm_predictor_children(
        list(generate_action = generate_action, extract = extract),
        signature@output_type
      )
      super$initialize(
        signature = signature,
        config = config,
        chat = chat
      )

      self$runner <- binding$runner
      self$interpreter_factory <- binding$interpreter_factory
      self$max_iterations <- max_iterations
      self$max_llm_calls <- max_llm_calls
      self$max_output_chars <- max_output_chars
      self$sub_lm <- sub_lm
      self$verbose <- verbose
      self$tools <- tools
      self$generate_action <- generate_action
      self$extract <- extract

      # Store REPL history per execution
      self$state$repl_history <- list()
      self$state$trace_sequence <- 0
    },

    #' @description
    #' Execute the RLM workflow
    #'
    #' @param batch Named list or data frame of inputs
    #' @param .llm Optional ellmer chat object
    #' @param trace Logical whether to record trace information
    #' @param .cache Logical or `NULL`. Per-call structured-response cache control
    #' @param ... Additional arguments
    #' @return Tibble with output, chat, metadata columns
    forward = function(
      batch,
      .llm = NULL,
      trace = TRUE,
      .cache = NULL,
      ...
    ) {
      validate_cache_arg(.cache)
      # Handle inputs
      if (is.data.frame(batch)) {
        if (nrow(batch) != 1L) {
          cli::cli_abort(
            c(
              "RLM forward() accepts exactly one data-frame row",
              "x" = "Received {nrow(batch)} rows.",
              "i" = "Use {.fn run} with an interpreter factory for isolated batch execution."
            ),
            class = "dsprrr_rlm_batch_error"
          )
        }
        inputs <- as.list(batch[1, , drop = FALSE])
      } else {
        inputs <- batch
      }
      expected_inputs <- vapply(
        self$signature@inputs,
        function(input) input$name,
        character(1)
      )
      valid_empty_inputs <- is.list(inputs) &&
        length(inputs) == 0L &&
        length(expected_inputs) == 0L
      if (
        !is.list(inputs) ||
          (!valid_empty_inputs && is.null(names(inputs))) ||
          anyNA(names(inputs)) ||
          !all(nzchar(names(inputs))) ||
          anyDuplicated(names(inputs))
      ) {
        cli::cli_abort(
          "RLM inputs must be supplied as a named list or data frame",
          class = "dsprrr_rlm_input_error"
        )
      }
      required_inputs <- vapply(
        self$signature@inputs,
        function(input) {
          tryCatch(isTRUE(input$type@required), error = function(e) TRUE)
        },
        logical(1)
      )
      missing_inputs <- setdiff(
        expected_inputs[required_inputs],
        names(inputs)
      )
      unexpected_inputs <- setdiff(names(inputs), expected_inputs)
      if (length(missing_inputs) > 0L || length(unexpected_inputs) > 0L) {
        cli::cli_abort(
          c(
            "RLM inputs must match declared signature fields",
            if (length(missing_inputs) > 0L) {
              c("x" = "Missing: {.field {missing_inputs}}")
            },
            if (length(unexpected_inputs) > 0L) {
              c("x" = "Unexpected: {.field {unexpected_inputs}}")
            }
          ),
          class = "dsprrr_rlm_input_error"
        )
      }

      # Get LLM - clone for fresh conversation
      base_llm <- .llm %||% self$chat %||% get_default_chat()
      if (is.null(base_llm)) {
        cli::cli_abort("No LLM provided. Pass .llm or set a default chat.")
      }

      llm <- clone_rlm_chat(base_llm)
      sub_lm <- self$sub_lm %||% base_llm

      with_code_runner_lease(
        self$runner,
        self$interpreter_factory,
        "RLM",
        function(runner, lease) {
          start_time <- Sys.time()

          if (!isTRUE(lease$policy$persistent)) {
            cli::cli_abort(
              c(
                "RLM requires a persistent interpreter",
                "x" = "The selected runner starts a fresh execution state for each turn.",
                "i" = "Use {.code r_code_runner(persistent = TRUE)} for trusted inputs or {.fn mcp_repl_runner} for managed execution."
              ),
              class = "dsprrr_rlm_persistent_runner_error"
            )
          }

          # Initialize call counter (shared across recursions)
          call_counter <- new.env()
          call_counter$count <- 0L
          call_counter$provider_calls <- 0L
          call_counter$provider_calls_known <- TRUE
          call_counter$usage <- list()

          prepare_context <- tryCatch(
            runner[["prepare_context"]],
            error = function(e) NULL
          )
          context_prepared <- FALSE
          if (isTRUE(lease$policy$persistent) && is.function(prepare_context)) {
            prepare_context(inputs)
            context_prepared <- TRUE
          }
          session_state_id <- paste0(".dsprrr_rlm_state_", rlm_control_nonce())
          if (!isTRUE(lease$owned)) {
            on.exit(
              private$cleanup_invocation_state(runner, session_state_id),
              add = TRUE
            )
          }

          # Build context description for system prompt
          context_desc <- private$describe_context(inputs)

          # Build system prompt
          system_prompt <- private$build_system_prompt(
            context_desc,
            has_sub_lm = self$max_llm_calls > 0L
          )

          # REPL iteration loop
          history <- list()
          final_answer <- NULL
          fallback_metadata <- NULL
          result_chat <- llm

          for (iter in seq_len(self$max_iterations)) {
            if (self$verbose) {
              cli::cli_alert_info("RLM Iteration {iter}/{self$max_iterations}")
            }

            # Build prompt for this iteration
            prompt <- private$build_iteration_prompt(
              system_prompt,
              history,
              iter
            )

            # Get LLM response (code generation)
            response <- private$get_code_response(
              base_llm,
              prompt,
              trace = trace,
              .cache = .cache
            )
            result_chat <- response$chat

            if (self$verbose) {
              cli::cli_alert(
                "Code generated: {substr(response$code, 1, 100)}..."
              )
            }

            # Execute code with RLM tools injected
            exec_result <- private$execute_with_rlm_tools(
              response$code,
              inputs,
              call_counter,
              runner,
              lease$policy,
              sub_lm = sub_lm,
              context_prepared = context_prepared,
              session_state_id = session_state_id
            )

            # Record in history
            history[[iter]] <- list(
              iteration = iter,
              reasoning = response$reasoning,
              code = response$code,
              output = exec_result$formatted_output,
              success = exec_result$success,
              is_final = exec_result$is_final,
              action_metadata = private$compact_action_metadata(
                response$metadata
              ),
              execution_metadata = list(
                duration_ms = exec_result$raw_result$duration_ms %||% NA_real_,
                error_type = exec_result$raw_result$error_type %||% NULL
              )
            )

            # Check for SUBMIT() termination
            if (isTRUE(exec_result$success) && isTRUE(exec_result$is_final)) {
              final_answer <- exec_result$final_value
              if (self$verbose) {
                cli::cli_alert_success("SUBMIT called with answer")
              }
              break
            }

            # Check for errors - feed back to LLM for retry
            if (!exec_result$success) {
              if (self$verbose) {
                cli::cli_alert_warning(
                  "Iteration {iter}: Code execution failed - {exec_result$error}"
                )
              }
              # Error will be in history, LLM can see and fix
              next
            }

            if (self$verbose) {
              cli::cli_alert(
                "Output: {substr(exec_result$formatted_output, 1, 200)}..."
              )
            }
          }

          final_source <- "submit"

          # Fallback extract if no SUBMIT()
          if (is.null(final_answer)) {
            final_source <- "fallback"
            cli::cli_warn(c(
              "RLM reached max_iterations ({self$max_iterations}) without SUBMIT()",
              "i" = "Using fallback extraction from trajectory",
              "i" = "Consider increasing max_iterations or simplifying the query"
            ))
            fallback_result <- private$extract_fallback(
              inputs,
              history,
              base_llm,
              trace = trace,
              .cache = .cache
            )
            final_answer <- fallback_result$value
            fallback_metadata <- fallback_result$metadata
            result_chat <- fallback_result$chat
          }

          output <- private$build_output(final_answer, source = final_source)
          usage <- private$summarize_action_usage(
            history,
            fallback_metadata = fallback_metadata,
            recursive_metadata = call_counter$usage
          )
          action_calls <- length(history)
          extraction_calls <- as.integer(identical(final_source, "fallback"))
          action_provider_calls <- private$summarize_provider_calls(
            lapply(history, function(entry) entry$action_metadata %||% list())
          )
          extraction_provider_calls <- if (extraction_calls == 1L) {
            private$metadata_provider_calls(fallback_metadata)
          } else {
            0L
          }
          recursive_provider_calls <- if (
            isTRUE(call_counter$provider_calls_known)
          ) {
            as.integer(call_counter$provider_calls)
          } else {
            NA_integer_
          }
          provider_calls <- private$sum_provider_call_counts(c(
            action_provider_calls,
            recursive_provider_calls,
            extraction_provider_calls
          ))

          # Store REPL history
          if (trace) {
            self$state$trace_sequence <- self$state$trace_sequence + 1
            input_summary <- private$summarize_trace_inputs(inputs)
            self$state$repl_history <- private$append_bounded_trace(
              self$state$repl_history,
              list(
                timestamp = start_time,
                inputs = input_summary,
                history = history,
                final_answer = output,
                iterations_used = length(history),
                llm_calls_used = call_counter$count
              )
            )
            trace_entry <- list(
              timestamp = start_time,
              inputs = input_summary,
              output = output,
              history = history,
              latency_ms = as.numeric(difftime(
                Sys.time(),
                start_time,
                units = "secs"
              )) *
                1000,
              tokens = usage,
              input_tokens = usage$input_tokens,
              cached_input_tokens = usage$cached_input_tokens,
              output_tokens = usage$output_tokens,
              total_tokens = usage$total_tokens,
              cost = usage$cost,
              provider_calls = provider_calls,
              action_calls = action_calls,
              action_provider_calls = action_provider_calls,
              recursive_calls = call_counter$count,
              recursive_provider_calls = recursive_provider_calls,
              extraction_calls = extraction_calls,
              extraction_provider_calls = extraction_provider_calls,
              model = private$rlm_model_name(base_llm),
              output_source = final_source
            )
            self$state$traces <- private$append_bounded_trace(
              self$state$traces,
              trace_entry
            )
            add_to_global_history(trace_entry, source = "RLMModule")
          }

          duration_secs <- as.numeric(difftime(
            Sys.time(),
            start_time,
            units = "secs"
          ))
          duration_ms <- duration_secs * 1000

          # Build metadata
          metadata <- list(
            model = "rlm",
            iterations = length(history),
            max_iterations = self$max_iterations,
            llm_calls = call_counter$count,
            max_llm_calls = self$max_llm_calls,
            output_source = final_source,
            duration_ms = round(duration_ms, 2),
            repl_history = history,
            input_tokens = usage$input_tokens,
            cached_input_tokens = usage$cached_input_tokens,
            output_tokens = usage$output_tokens,
            total_tokens = usage$total_tokens,
            cost = usage$cost,
            provider_calls = provider_calls,
            action_calls = action_calls,
            action_provider_calls = action_provider_calls,
            recursive_calls = call_counter$count,
            recursive_provider_calls = recursive_provider_calls,
            extraction_calls = extraction_calls,
            extraction_provider_calls = extraction_provider_calls,
            runner_policy = lease$policy_summary,
            runner_lifecycle = if (lease$owned) {
              "invocation-owned"
            } else {
              "caller-owned"
            }
          )

          tibble::tibble(
            output = list(output),
            chat = list(result_chat),
            metadata = list(metadata)
          )
        }
      )
    },

    #' @description
    #' Get REPL history for inspection
    #' @return List of REPL execution records
    get_repl_history = function() {
      self$state$repl_history
    },

    #' @description
    #' Create a fresh copy of this module
    #' @return New RLMModule with same settings
    reset_copy = function() {
      artifact_copy_runtime(
        self,
        RLMModule$new(
          signature = self$signature,
          runner = self$runner,
          interpreter_factory = self$interpreter_factory,
          max_iterations = self$max_iterations,
          max_llm_calls = self$max_llm_calls,
          max_output_chars = self$max_output_chars,
          sub_lm = self$sub_lm,
          verbose = self$verbose,
          tools = self$tools,
          config = self$config,
          chat = self$chat,
          generate_action = self$generate_action$reset_copy(),
          extract = self$extract$reset_copy()
        )
      )
    },

    #' @description
    #' Create a deep copy while preserving the configured runtime source
    #' @return New RLMModule with copied state
    deepcopy = function() {
      copied <- RLMModule$new(
        signature = self$signature,
        runner = self$runner,
        max_iterations = self$max_iterations,
        max_llm_calls = self$max_llm_calls,
        max_output_chars = self$max_output_chars,
        sub_lm = self$sub_lm,
        verbose = self$verbose,
        tools = self$tools,
        config = lapply(self$config, identity),
        chat = self$chat,
        interpreter_factory = self$interpreter_factory,
        generate_action = self$generate_action$deepcopy(),
        extract = self$extract$deepcopy()
      )
      copied$state <- lapply(self$state, identity)
      artifact_copy_runtime(self, copied)
    },

    #' @description
    #' Expose the action and extraction predictors to graph optimizers
    #' @return Named list of child modules
    graph_children = function() {
      list(
        generate_action = self$generate_action,
        extract = self$extract
      )
    },

    #' @description
    #' Replace the action and extraction predictors
    #' @param children Named child-module list
    #' @return The module, invisibly
    set_graph_children = function(children) {
      self$validate_graph_children(children)
      self$generate_action <- children$generate_action
      self$extract <- children$extract
      invisible(self)
    },

    #' @description
    #' Validate replacement RLM predictor children
    #' @param children Named child-module list
    #' @return The children, invisibly
    validate_graph_children = function(children) {
      validate_rlm_predictor_children(children, self$signature@output_type)
    },

    #' @description
    #' Print method for RLMModule
    print = function() {
      # Format signature
      input_names <- vapply(
        self$signature@inputs,
        function(x) x$name,
        character(1)
      )
      sig_str <- paste0(
        paste(input_names, collapse = ", "),
        " -> ",
        private$get_output_names()
      )

      cli::cli_h3("RLMModule")
      cli::cli_bullets(c(
        "*" = "Signature: {sig_str}",
        "*" = "Max iterations: {.val {self$max_iterations}}",
        "*" = "Max LLM calls: {.val {self$max_llm_calls}}",
        "*" = "Runner: {code_runner_binding_label(self$runner, self$interpreter_factory)}",
        "*" = "Recursive queries: {.val {if (self$max_llm_calls == 0L) 'disabled' else if (is.null(self$sub_lm)) 'outer LM' else 'separate sub-LM'}}",
        "*" = "Custom tools: {.val {length(self$tools)}}"
      ))
      invisible(self)
    }
  ),

  private = list(
    #' Remove invocation-private state from a reusable caller-owned runner
    cleanup_invocation_state = function(runner, session_state_id) {
      state_name <- encodeString(session_state_id, quote = "\"")
      cleanup_code <- paste0(
        "base::local({\n",
        "  .root <- base::parent.env(base::environment())\n",
        "  if (base::exists(",
        state_name,
        ", envir = .root, inherits = FALSE)) {\n",
        "    base::rm(list = ",
        state_name,
        ", envir = .root)\n",
        "  }\n",
        "  base::invisible(TRUE)\n",
        "})"
      )

      cleanup_error <- tryCatch(
        {
          result <- runner$execute(cleanup_code, context = list())
          if (!isTRUE(result$success)) {
            stop(result$error %||% "runner rejected invocation cleanup")
          }
          release_context <- tryCatch(
            runner[["release_context"]],
            error = function(e) NULL
          )
          if (is.function(release_context)) {
            release_context()
          }
          NULL
        },
        interrupt = function(e) e,
        error = function(e) e
      )
      if (inherits(cleanup_error, "condition")) {
        cli::cli_warn(
          c(
            "RLM could not fully clear its caller-owned interpreter state",
            "x" = conditionMessage(cleanup_error),
            "i" = "Reset or close the runner before reusing it."
          ),
          class = "dsprrr_rlm_cleanup_warning"
        )
      }
      invisible(NULL)
    },

    #' Get output field names from signature for display
    get_output_names = function() {
      paste(private$get_output_field_names(), collapse = ", ")
    },

    #' Get output field specs from signature
    get_output_specs = function() {
      output_type <- self$signature@output_type
      if (methods::.hasSlot(output_type, "properties")) {
        props <- output_type@properties
        props <- props[
          !vapply(
            props,
            inherits,
            logical(1),
            what = "ellmer::TypeIgnore"
          )
        ]
        return(props)
      }
      if (inherits(output_type, "ellmer::TypeIgnore")) {
        return(list())
      }
      list(answer = output_type)
    },

    #' Get required output field names from signature
    get_required_output_field_names = function() {
      specs <- private$get_output_specs()
      names(specs)[vapply(
        specs,
        function(spec) {
          tryCatch(isTRUE(spec@required), error = function(e) TRUE)
        },
        logical(1)
      )]
    },

    #' Get output field names from signature
    get_output_field_names = function() {
      names(private$get_output_specs())
    },

    #' Format a true-length head-and-tail excerpt
    format_excerpt = function(text, max_chars = self$max_output_chars) {
      if (is.null(text) || length(text) == 0L) {
        return("")
      }
      text <- paste(as.character(text), collapse = "\n")
      total <- nchar(text, type = "chars")
      if (total <= max_chars) {
        return(text)
      }
      marker <- paste0("\n... [", total, " total characters] ...\n")
      if (nchar(marker) + 2L > max_chars) {
        return(substr(text, 1L, max_chars))
      }
      omitted <- total
      for (iteration in seq_len(10L)) {
        marker <- paste0(
          "\n... [",
          omitted,
          " characters omitted; total ",
          total,
          "] ...\n"
        )
        if (nchar(marker) + 2L > max_chars) {
          return(substr(text, 1L, max_chars))
        }
        visible <- max_chars - nchar(marker)
        head_chars <- ceiling(visible / 2)
        tail_chars <- floor(visible / 2)
        next_omitted <- total - head_chars - tail_chars
        if (identical(next_omitted, omitted)) {
          break
        }
        omitted <- next_omitted
      }
      paste0(
        substr(text, 1L, head_chars),
        marker,
        substr(text, total - tail_chars + 1L, total)
      )
    },

    #' Preview one context value without embedding the full object
    preview_context_value = function(value) {
      text <- if (is.character(value) && length(value) == 1L) {
        value
      } else if (is.data.frame(value)) {
        paste(
          utils::capture.output(utils::str(value, max.level = 1L)),
          collapse = " "
        )
      } else {
        paste(
          utils::capture.output(utils::str(value, max.level = 1L)),
          collapse = " "
        )
      }
      private$format_excerpt(text, max_chars = 1000L)
    },

    #' Describe context variables for the system prompt
    describe_context = function(inputs) {
      if (length(inputs) == 0) {
        return("No context variables available.")
      }

      descriptions <- vapply(
        names(inputs),
        function(name) {
          val <- inputs[[name]]
          input_index <- match(
            name,
            vapply(
              self$signature@inputs,
              function(spec) spec$name,
              character(1)
            )
          )
          input_spec <- self$signature@inputs[[input_index]]
          type_str <- class(val)[1]
          size_str <- if (is.character(val)) {
            total_chars <- sum(nchar(val))
            paste0(total_chars, " characters")
          } else if (is.data.frame(val)) {
            paste0(nrow(val), " rows x ", ncol(val), " cols")
          } else if (is.list(val)) {
            paste0(length(val), " elements")
          } else if (is.vector(val)) {
            paste0(length(val), " elements")
          } else {
            "1 object"
          }

          preview <- private$preview_context_value(val)
          declared <- format_ellmer_type(input_spec$type, verbose = TRUE)
          description <- input_spec$description %||% ""

          paste0(
            "- `.context$",
            name,
            "` (",
            type_str,
            ", ",
            size_str,
            "; declared ",
            declared,
            ")",
            if (nzchar(description)) paste0(" - ", description) else "",
            "\n",
            "  Preview: ",
            preview
          )
        },
        character(1)
      )

      paste(descriptions, collapse = "\n\n")
    },

    #' Build the system prompt for RLM
    build_system_prompt = function(context_desc, has_sub_lm = TRUE) {
      output_fields <- private$get_output_field_names()
      output_specs <- private$get_output_specs()
      required_fields <- private$get_required_output_field_names()
      submit_usage <- if (length(required_fields) == 0L) {
        "SUBMIT()"
      } else {
        paste0(
          "SUBMIT(",
          paste0(required_fields, " = ...", collapse = ", "),
          ")"
        )
      }

      # Build tool descriptions
      tool_desc <- c(
        paste0(
          "- `",
          submit_usage,
          "`: Submit final output field values and terminate"
        ),
        "- `peek(var, start = 1, end = 1000)`: View a character slice of a variable",
        "- `search(var, pattern)`: Regex search in variable, returns matches"
      )

      if (has_sub_lm) {
        tool_desc <- c(
          tool_desc,
          "- `llm_query(query, context_slice = NULL)`: Ask a sub-question to another LLM",
          "- `llm_query_batched(queries, slices = NULL)`: Batch multiple sub-questions"
        )
      }

      if (length(self$tools) > 0) {
        tool_desc <- c(
          tool_desc,
          vapply(
            names(self$tools),
            function(name) format_rlm_tool_contract(self$tools[[name]], name),
            character(1)
          )
        )
      }

      output_contract <- vapply(
        names(output_specs),
        function(name) {
          spec <- output_specs[[name]]
          description <- spec@description %||% ""
          paste0(
            "- `",
            name,
            "`: ",
            format_ellmer_type(spec, verbose = TRUE),
            if (!name %in% required_fields) " (optional)" else "",
            if (nzchar(description)) paste0(" - ", description) else ""
          )
        },
        character(1)
      )

      task_instructions <- self$signature@instructions %||% ""
      if (!nzchar(task_instructions)) {
        task_instructions <- "Answer the declared task using only supported evidence."
      }

      glue::glue(
        "
You are working in an R REPL environment. Your goal is to answer the query by
writing R code to explore and analyze the provided context.

## Task
{task_instructions}

## Available Variables
{context_desc}

## Declared Output
{paste(output_contract, collapse = '
')}

## Available Functions
{paste(tool_desc, collapse = '
')}

## Rules
1. Explore the context programmatically - don't ask to see all content at once
2. Use peek() to examine slices of large text
3. Use search() to find specific patterns
4. Break complex queries into smaller sub-questions{if (has_sub_lm) ' using llm_query(); it returns a value you can assign and combine' else ''}
5. Call {submit_usage} when you have the final answer
6. You have {self$max_iterations} iterations{if (has_sub_lm) paste0(' and ', self$max_llm_calls, ' LLM calls') else ''}
7. Submitted values must satisfy the declared output types; invalid submissions are returned as repairable errors
8. Keep work before llm_query(), host-tool calls, and SUBMIT() read-only because bridge replay can repeat direct external side effects

## Response Format
Return JSON with:
- \"reasoning\": Your thought process for this step
- \"code\": R code to execute (single string)

The code's output will be shown to you. Continue until you call SUBMIT().
"
      )
    },

    #' Build prompt for a specific iteration
    build_iteration_prompt = function(system_prompt, history, iter) {
      if (iter == 1) {
        # First iteration - just system prompt
        return(system_prompt)
      }

      # Build history context
      history_parts <- vapply(
        history,
        function(h) {
          glue::glue(
            "
## Iteration {h$iteration}
Reasoning: {h$reasoning}

Code:
```r
{h$code}
```

{if (h$success) 'Output:' else 'Error:'}
{private$format_excerpt(h$output)}
{if (h$is_final) '(SUBMIT was called)' else ''}
"
          )
        },
        character(1)
      )

      paste0(
        system_prompt,
        "\n\n## Previous Iterations\n",
        paste(history_parts, collapse = "\n"),
        "\n\n## Next Step\nContinue exploring or call SUBMIT() with your answer."
      )
    },

    #' Get code response from LLM
    get_code_response = function(
      llm,
      prompt,
      trace = TRUE,
      .cache = NULL
    ) {
      action_llm <- clone_rlm_chat(llm)
      action_result <- tryCatch(
        self$generate_action$forward(
          list(state = prompt),
          .llm = action_llm,
          trace = FALSE,
          .cache = .cache
        ),
        error = function(e) {
          cli::cli_abort(
            c(
              "Failed to get code from LLM",
              "x" = "Error: {e$message}"
            ),
            class = "dsprrr_rlm_action_error",
            parent = e
          )
        }
      )
      result <- action_result$output[[1L]]

      valid_code <- is.character(result$code) &&
        length(result$code) == 1L &&
        !is.na(result$code) &&
        nzchar(trimws(result$code))
      if (!valid_code) {
        cli::cli_abort(
          c(
            "LLM returned invalid response",
            "i" = "{.field code} must be one non-empty R source string."
          ),
          class = "dsprrr_rlm_action_error"
        )
      }
      metadata <- private$normalize_predictor_metadata(
        self$generate_action,
        action_result$metadata[[1L]] %||% list()
      )

      list(
        reasoning = result$reasoning %||% "",
        code = strip_rlm_code_fences(result$code),
        metadata = metadata,
        chat = action_result$chat[[1L]] %||% action_llm
      )
    },

    #' Execute code with RLM tools injected
    execute_with_rlm_tools = function(
      code,
      inputs,
      call_counter,
      runner,
      runner_policy,
      sub_lm,
      context_prepared = FALSE,
      session_state_id
    ) {
      code <- strip_rlm_code_fences(code)
      control_nonce <- rlm_control_nonce()

      # Build RLM prelude that defines tools
      rlm_prelude <- create_rlm_prelude(
        max_llm_calls = self$max_llm_calls,
        has_sub_lm = self$max_llm_calls > 0L,
        custom_tools = self$tools,
        output_fields = private$get_output_field_names(),
        required_output_fields = private$get_required_output_field_names(),
        control_nonce = control_nonce,
        control_frame_limit = runner_policy$rlm_control_frame_limit %||% Inf
      )

      state_name <- encodeString(session_state_id, quote = "\"")
      user_code <- encodeString(code, quote = "\"")
      prelude_code <- encodeString(rlm_prelude, quote = "\"")

      # Each bridge replay starts from the same RLM assignment state. Ordinary
      # bindings are committed only after the complete block succeeds. Direct
      # external effects are not transactional and can repeat. A non-local
      # restart suspends query, tool, and final controls, so guest tryCatch()
      # cannot continue past a privileged boundary.
      combined_code <- paste0(
        "base::local({\n",
        "  # Replay-isolated RLM user-code bindings\n",
        "  .rlm_root <- base::parent.env(base::environment())\n",
        "  .rlm_state_name <- ",
        state_name,
        "\n",
        "  if (!base::exists(.rlm_state_name, envir = .rlm_root, inherits = FALSE)) {\n",
        "    base::assign(.rlm_state_name, base::new.env(parent = .rlm_root), envir = .rlm_root)\n",
        "  }\n",
        "  .rlm_state <- base::get(.rlm_state_name, envir = .rlm_root, inherits = FALSE)\n",
        "  .rlm_tx <- base::new.env(parent = .rlm_state)\n",
        "  .rlm_tx$.context <- .context\n",
        "  base::eval(base::parse(text = ",
        prelude_code,
        "), envir = .rlm_tx)\n",
        "  .rlm_injected_names <- base::ls(.rlm_tx, all.names = TRUE)\n",
        "  .rlm_step <- base::withRestarts(\n",
        "    {\n",
        "      .rlm_value <- base::eval(base::parse(text = ",
        user_code,
        "), envir = .rlm_tx)\n",
        "      base::list(control = FALSE, value = .rlm_value)\n",
        "    },\n",
        "    .dsprrr_rlm_control = function(frame) {\n",
        "      base::list(control = TRUE, value = frame)\n",
        "    }\n",
        "  )\n",
        "  if (base::isTRUE(.rlm_step$control)) {\n",
        "    .rlm_step$value\n",
        "  } else {\n",
        "    .rlm_names <- base::setdiff(\n",
        "      base::ls(.rlm_tx, all.names = TRUE),\n",
        "      base::c('.context', .rlm_injected_names)\n",
        "    )\n",
        "    for (.rlm_name in .rlm_names) {\n",
        "      base::assign(.rlm_name, base::get(.rlm_name, envir = .rlm_tx, inherits = FALSE), envir = .rlm_state)\n",
        "    }\n",
        "    .rlm_step$value\n",
        "  }\n",
        "})\n"
      )

      # Execute with inputs as context
      control_replay <- list()
      repeat {
        execution_context <- if (isTRUE(context_prepared)) list() else inputs
        execution_context[[rlm_control_replay_field()]] <- control_replay
        result <- execute_code_runner(
          runner,
          combined_code,
          context = execution_context,
          .control_nonce = control_nonce
        )

        control_value <- NULL
        for (candidate in list(
          result$result,
          result$stdout,
          result$stderr,
          result$error
        )) {
          if (!isTRUE(result$success) && !is.character(candidate)) {
            next
          }
          decoded <- decode_rlm_control(candidate, control_nonce)
          if (!is.null(decoded)) {
            control_value <- decoded
            break
          }
        }
        if (
          !is_rlm_host_tool_request(control_value) &&
            !is_rlm_query_request(control_value)
        ) {
          if (is_rlm_final(control_value)) {
            control_index <- attr(
              control_value,
              "rlm_control_index",
              exact = TRUE
            )
            if (!identical(control_index, length(control_replay) + 1L)) {
              cli::cli_abort(
                "RLM final control diverged from the recorded replay sequence",
                class = "dsprrr_rlm_control_error"
              )
            }
          }
          break
        }

        if (!identical(control_value$index, length(control_replay) + 1L)) {
          cli::cli_abort(
            "RLM controls were returned out of replay order",
            class = "dsprrr_rlm_control_error"
          )
        }

        if (is_rlm_query_request(control_value)) {
          request <- control_value
          query_outcome <- private$process_rlm_query(
            request,
            call_counter,
            sub_lm
          )
          control_replay[[length(control_replay) + 1L]] <- list(
            kind = "query",
            request = unclass(request),
            success = query_outcome$success,
            value = query_outcome$value,
            error = query_outcome$error
          )
          next
        }

        tool_calls <- sum(vapply(
          control_replay,
          function(record) identical(record$kind, "host_tool"),
          logical(1)
        ))
        if (tool_calls >= 1000L) {
          cli::cli_abort(
            "RLM exceeded the per-iteration host-tool bridge limit",
            class = "dsprrr_rlm_host_tool_limit_error"
          )
        }
        request <- control_value
        if (!request$name %in% names(self$tools)) {
          cli::cli_abort(
            "RLM requested unknown host tool {.val {request$name}}",
            class = "dsprrr_rlm_host_tool_protocol_error"
          )
        }
        tool_outcome <- tryCatch(
          list(
            success = TRUE,
            value = do.call(self$tools[[request$name]], request$arguments),
            error = NULL
          ),
          interrupt = function(condition) stop(condition),
          error = function(condition) {
            list(
              success = FALSE,
              value = NULL,
              error = conditionMessage(condition)
            )
          }
        )
        control_replay[[length(control_replay) + 1L]] <- c(
          list(kind = "host_tool", request = unclass(request)),
          tool_outcome
        )
      }

      if (!isTRUE(result$success) && !is_rlm_final(control_value)) {
        control_value <- NULL
      }

      # Detect and strictly validate SUBMIT before terminating the loop.
      is_final <- is_rlm_final(control_value)
      final_value <- if (is_final) {
        extract_rlm_final(control_value)
      } else {
        NULL
      }

      if (is_final) {
        validated <- tryCatch(
          list(
            value = private$normalize_final_answer(
              final_value,
              source = "submit"
            ),
            error = NULL
          ),
          error = function(error) list(value = NULL, error = error)
        )
        if (inherits(validated$error, "condition")) {
          message <- conditionMessage(validated$error)
          return(list(
            success = FALSE,
            is_final = FALSE,
            final_value = NULL,
            formatted_output = paste0("Error: ", message),
            error = message,
            raw_result = result
          ))
        }
        final_value <- validated$value
      }

      # Format output for history
      formatted_output <- if (is_final) {
        "[SUBMIT accepted]"
      } else if (result$success) {
        private$format_execution_output(result)
      } else {
        paste("Error:", result$error %||% "Unknown error")
      }
      formatted_output <- private$format_excerpt(formatted_output)

      list(
        success = is_final || isTRUE(result$success),
        is_final = is_final,
        final_value = final_value,
        formatted_output = formatted_output,
        error = if (is_final) NULL else result$error,
        raw_result = result
      )
    },

    #' Process an rlm_query request (single or batch)
    #'
    #' @return List with success, formatted_output, error fields
    process_rlm_query = function(request, call_counter, sub_lm) {
      call_counter$provider_calls <- call_counter$provider_calls %||% 0L
      call_counter$provider_calls_known <-
        call_counter$provider_calls_known %||% TRUE

      # Handle batch queries
      if (isTRUE(request$batch)) {
        return(private$process_rlm_query_batch(request, call_counter, sub_lm))
      }

      # Single query processing
      # Check call limit
      if (call_counter$count >= self$max_llm_calls) {
        error_msg <- paste0(
          "Maximum LLM calls (",
          self$max_llm_calls,
          ") exceeded"
        )
        cli::cli_warn(error_msg)
        return(list(
          success = FALSE,
          value = NULL,
          formatted_output = paste0("Error: ", error_msg),
          error = error_msg
        ))
      }

      call_counter$count <- call_counter$count + 1L

      query <- request$query
      context_slice <- request$context

      prompt <- if (!is.null(context_slice)) {
        paste0("Context:\n", context_slice, "\n\nQuestion: ", query)
      } else {
        query
      }

      query_llm <- clone_rlm_chat(sub_lm)
      turns_before <- batch_chat_turns(query_llm)
      result <- tryCatch(
        {
          response <- query_llm$chat(prompt)
          text <- normalize_rlm_sub_lm_text(response)
          list(
            success = TRUE,
            value = text,
            formatted_output = paste0(
              "Query result: ",
              private$format_excerpt(text)
            ),
            error = NULL,
            metadata = chat_usage_metadata(query_llm, turns_before)
          )
        },
        error = function(e) {
          if (inherits(e, "dsprrr_rlm_sub_lm_response_error")) {
            stop(e)
          }
          if (!is_rlm_provider_error(e)) {
            stop(e)
          }
          cli::cli_warn(c(
            "Recursive LLM query failed",
            "x" = "Error: {e$message}",
            "i" = "Query: {substr(query, 1, 100)}..."
          ))
          list(
            success = FALSE,
            value = NULL,
            formatted_output = paste0("Query error: ", e$message),
            error = e$message,
            metadata = c(
              canonical_usage_metadata(),
              list(provider_calls = NA_integer_)
            )
          )
        }
      )
      private$record_recursive_provider_calls(call_counter, result$metadata)
      call_counter$usage <- append(
        call_counter$usage,
        list(result$metadata %||% canonical_usage_metadata())
      )

      result
    },

    #' Process batch rlm_query requests
    #'
    #' @return List with success, formatted_output, error fields
    process_rlm_query_batch = function(request, call_counter, sub_lm) {
      call_counter$provider_calls <- call_counter$provider_calls %||% 0L
      call_counter$provider_calls_known <-
        call_counter$provider_calls_known %||% TRUE

      queries <- request$queries
      slices <- request$slices
      n_queries <- length(queries)

      # Check if we have enough calls remaining
      remaining_calls <- self$max_llm_calls - call_counter$count
      if (n_queries > remaining_calls) {
        error_msg <- paste0(
          "Batch of ",
          n_queries,
          " queries would exceed limit. ",
          "Remaining calls: ",
          remaining_calls
        )
        cli::cli_warn(error_msg)
        return(list(
          success = FALSE,
          value = NULL,
          formatted_output = paste0("Error: ", error_msg),
          error = error_msg
        ))
      }

      if (n_queries == 0L) {
        return(list(
          success = TRUE,
          value = character(),
          formatted_output = "",
          error = NULL
        ))
      }

      # Reserve call budget up-front to ensure consistent accounting
      call_counter$count <- call_counter$count + n_queries

      prompts <- vapply(
        seq_len(n_queries),
        function(i) {
          query <- queries[[i]]
          context_slice <- if (!is.null(slices)) slices[[i]] else NULL
          if (!is.null(context_slice)) {
            paste0("Context:\n", context_slice, "\n\nQuestion: ", query)
          } else {
            query
          }
        },
        character(1)
      )

      batch_result <- private$run_batched_sub_lm_queries(prompts, sub_lm)
      results <- batch_result$results
      errors <- batch_result$errors
      if (isTRUE(batch_result$provider_calls_known)) {
        call_counter$provider_calls <- call_counter$provider_calls +
          batch_result$provider_calls
      } else {
        call_counter$provider_calls_known <- FALSE
      }
      call_counter$usage <- append(
        call_counter$usage,
        batch_result$usage %||%
          rep(
            list(canonical_usage_metadata()),
            n_queries
          )
      )

      if (length(errors) > 0) {
        cli::cli_warn(c(
          "Some batch queries failed",
          "x" = errors
        ))
      }

      formatted_output <- paste(
        vapply(
          seq_len(n_queries),
          function(index) {
            value <- results[[index]] %||% ""
            paste0(
              "Query ",
              index,
              " result: ",
              private$format_excerpt(value)
            )
          },
          character(1)
        ),
        collapse = "\n\n"
      )

      list(
        success = TRUE,
        value = vapply(results, as.character, character(1)),
        formatted_output = private$format_excerpt(formatted_output),
        error = NULL,
        errors = errors
      )
    },

    #' Run a batch of sub-LM prompts with bounded parallelism
    run_batched_sub_lm_queries = function(prompts, sub_lm) {
      n_queries <- length(prompts)

      # For a single query, avoid parallel overhead
      if (n_queries <= 1L) {
        return(private$run_batched_sub_lm_queries_sequential(prompts, sub_lm))
      }

      has_parallel_chat <- exists(
        "parallel_chat",
        envir = asNamespace("ellmer"),
        inherits = FALSE
      )

      if (!has_parallel_chat) {
        cli::cli_inform(c(
          "i" = "Falling back to sequential batch queries.",
          "i" = "{.fn ellmer::parallel_chat} not available; upgrade ellmer for parallel execution."
        ))
        return(private$run_batched_sub_lm_queries_sequential(prompts, sub_lm))
      }

      max_active <- private$get_rlm_batch_max_active(n_queries)
      batch_llm <- clone_rlm_chat(sub_lm)
      turns_before <- batch_chat_turns(batch_llm)

      parallel_turns <- tryCatch(
        {
          ellmer::parallel_chat(
            chat = batch_llm,
            prompts = as.list(prompts),
            max_active = max_active,
            on_error = "continue"
          )
        },
        interrupt = function(i) stop(i),
        error = function(e) {
          e
        }
      )

      if (inherits(parallel_turns, "error")) {
        cli::cli_abort(
          c(
            "Recursive LLM batch transport failed",
            "x" = conditionMessage(parallel_turns)
          ),
          class = "dsprrr_rlm_batch_transport_error",
          parent = parallel_turns
        )
      }

      if (
        is.null(parallel_turns) ||
          !is.list(parallel_turns) ||
          length(parallel_turns) != n_queries
      ) {
        cli::cli_abort(
          "Recursive LLM batch returned an invalid response shape",
          class = "dsprrr_rlm_batch_transport_error"
        )
      }

      results <- vector("list", n_queries)
      errors <- character()
      usage <- vector("list", n_queries)

      for (i in seq_len(n_queries)) {
        parsed <- private$extract_parallel_query_result(
          parallel_turns[[i]],
          i,
          turns_before = turns_before
        )
        results[[i]] <- parsed$result
        usage[[i]] <- parsed$metadata
        if (!is.null(parsed$error)) {
          errors <- c(errors, parsed$error)
        }
      }

      provider_calls <- private$summarize_provider_calls(usage)
      list(
        results = results,
        errors = errors,
        usage = usage,
        provider_calls = provider_calls,
        provider_calls_known = !is.na(provider_calls)
      )
    },

    #' Sequential fallback for batched sub-LM queries
    run_batched_sub_lm_queries_sequential = function(prompts, sub_lm) {
      n_queries <- length(prompts)
      results <- vector("list", n_queries)
      errors <- character()
      usage <- vector("list", n_queries)

      for (i in seq_len(n_queries)) {
        query_llm <- clone_rlm_chat(sub_lm)
        turns_before <- batch_chat_turns(query_llm)
        query_failed <- FALSE
        results[[i]] <- tryCatch(
          {
            normalize_rlm_sub_lm_text(query_llm$chat(prompts[[i]]))
          },
          error = function(e) {
            if (inherits(e, "dsprrr_rlm_sub_lm_response_error")) {
              stop(e)
            }
            if (!is_rlm_provider_error(e)) {
              stop(e)
            }
            query_failed <<- TRUE
            errors <<- c(errors, paste0("Query ", i, ": ", e$message))
            paste0("[ERROR] ", e$message)
          }
        )
        usage[[i]] <- if (query_failed) {
          c(
            canonical_usage_metadata(),
            list(provider_calls = NA_integer_)
          )
        } else {
          chat_usage_metadata(query_llm, turns_before)
        }
      }

      provider_calls <- private$summarize_provider_calls(usage)
      list(
        results = results,
        errors = errors,
        usage = usage,
        provider_calls = provider_calls,
        provider_calls_known = !is.na(provider_calls)
      )
    },

    #' Extract a text result from ellmer::parallel_chat() output
    extract_parallel_query_result = function(
      turn_or_error,
      index,
      turns_before = NULL
    ) {
      if (is.null(turn_or_error)) {
        cli::cli_abort(
          "Recursive LLM batch returned a missing result at position {index}",
          class = "dsprrr_rlm_batch_transport_error"
        )
      }
      if (inherits(turn_or_error, "error")) {
        if (!is_rlm_provider_error(turn_or_error)) {
          cli::cli_abort(
            c(
              "Recursive LLM batch returned an unexpected error at position {index}",
              "x" = conditionMessage(turn_or_error)
            ),
            class = "dsprrr_rlm_batch_transport_error",
            parent = turn_or_error
          )
        }
        msg <- conditionMessage(turn_or_error)
        return(list(
          result = paste0("[ERROR] ", msg),
          error = paste0("Query ", index, ": ", msg),
          metadata = c(
            canonical_usage_metadata(),
            list(provider_calls = NA_integer_)
          )
        ))
      }

      text <- tryCatch(
        {
          turn <- turn_or_error$last_turn()
          if (inherits(turn, "S7_object")) {
            turn@text
          } else if (is.list(turn) && !is.null(turn$text)) {
            turn$text
          } else if (is.character(turn)) {
            turn[[1]]
          } else {
            NULL
          }
        },
        error = function(e) {
          cli::cli_warn(c(
            "Failed to extract text from parallel query result {index}.",
            "x" = "{e$message}"
          ))
          NULL
        }
      )

      list(
        result = normalize_rlm_sub_lm_text(text),
        error = NULL,
        metadata = chat_usage_metadata(turn_or_error, turns_before)
      )
    },

    #' Determine bounded parallelism for RLM batch calls
    get_rlm_batch_max_active = function(n_queries) {
      max_active <- getOption("dsprrr.rlm_batch_max_active", 10L)
      if (
        !is.numeric(max_active) ||
          length(max_active) != 1L ||
          is.na(max_active) ||
          max_active < 1
      ) {
        max_active <- 10L
      }
      as.integer(min(n_queries, floor(max_active)))
    },

    #' Format execution output for history
    format_execution_output = function(result) {
      parts <- character()

      # Safely check stdout (may be NULL or missing)
      stdout_val <- result$stdout
      if (
        !is.null(stdout_val) &&
          is.character(stdout_val) &&
          nchar(stdout_val) > 0
      ) {
        parts <- c(parts, paste0("stdout:\n", stdout_val))
      }

      # Safely check messages (may be NULL or missing)
      messages_val <- result$messages
      if (
        !is.null(messages_val) &&
          is.character(messages_val) &&
          nchar(messages_val) > 0
      ) {
        parts <- c(parts, paste0("messages:\n", messages_val))
      }

      # Safely check warnings (may be NULL or missing)
      warnings_val <- result$warnings
      if (
        !is.null(warnings_val) &&
          is.character(warnings_val) &&
          nchar(warnings_val) > 0
      ) {
        parts <- c(parts, paste0("warnings:\n", warnings_val))
      }

      if (!is.null(result$result)) {
        result_str <- tryCatch(
          {
            if (is.data.frame(result$result)) {
              rows <- nrow(result$result)
              shown <- if (rows <= 20L) {
                result$result
              } else {
                rbind(
                  utils::head(result$result, 10L),
                  utils::tail(result$result, 10L)
                )
              }
              rendered <- paste(
                utils::capture.output(print(shown)),
                collapse = "\n"
              )
              if (rows > 20L) {
                paste0(
                  rendered,
                  "\n... [",
                  rows - 20L,
                  " data-frame rows omitted; ",
                  rows,
                  " total]"
                )
              } else {
                rendered
              }
            } else if (
              is.atomic(result$result) && length(result$result) <= 10
            ) {
              paste(result$result, collapse = ", ")
            } else {
              paste(utils::capture.output(str(result$result)), collapse = "\n")
            }
          },
          error = function(e) deparse(result$result)[1]
        )
        parts <- c(parts, paste0("result:\n", result_str))
      }

      if (length(parts) == 0) {
        return("[No output]")
      }

      private$format_excerpt(paste(parts, collapse = "\n\n"))
    },

    #' Extract answer via fallback when max_iterations reached
    extract_fallback = function(
      inputs,
      history,
      llm,
      trace = TRUE,
      .cache = NULL
    ) {
      # Build trajectory summary
      trajectory <- vapply(
        history,
        function(h) {
          glue::glue(
            "Iteration {h$iteration}:
Reasoning: {h$reasoning}
Code: {h$code}
Output: {private$format_excerpt(h$output, max_chars = self$max_output_chars)}"
          )
        },
        character(1)
      )

      input_context <- private$format_inputs_for_prompt(inputs)
      variable_info <- private$describe_context(inputs)

      prompt <- glue::glue(
        "
The RLM agent ran out of iterations before calling SUBMIT().
Based on the exploration trajectory below, extract the best possible answer.

## Original Task Instructions
{self$signature@instructions}

## Original Query
{input_context}

## Available Variables
{variable_info}

## Exploration Trajectory
{paste(trajectory, collapse = '
---
')}

## Task
Based on the above exploration, provide the final answer to the original query.
Be concise and direct. If the exploration was incomplete, provide the best
answer possible with what was discovered.
"
      )

      fallback_result <- tryCatch(
        self$extract$forward(
          list(state = prompt),
          .llm = clone_rlm_chat(llm),
          trace = FALSE,
          .cache = .cache
        ),
        error = function(e) {
          cli::cli_abort(
            c(
              "RLM fallback extraction failed",
              "x" = "{conditionMessage(e)}"
            ),
            class = "dsprrr_rlm_fallback_error",
            parent = e
          )
        }
      )
      value <- private$normalize_final_answer(
        fallback_result$output[[1L]],
        source = "fallback"
      )
      metadata <- private$normalize_predictor_metadata(
        self$extract,
        fallback_result$metadata[[1L]] %||% list()
      )
      list(
        value = value,
        metadata = metadata,
        chat = fallback_result$chat[[1L]] %||% llm
      )
    },

    #' Format inputs for prompt display
    format_inputs_for_prompt = function(inputs) {
      parts <- vapply(
        names(inputs),
        function(name) {
          val <- inputs[[name]]
          if (is.character(val) && length(val) == 1) {
            if (nchar(val) > 500) {
              val <- paste0(substr(val, 1, 500), "... [truncated]")
            }
            paste0(name, ": ", val)
          } else {
            paste0(name, ": ", deparse(val, width.cutoff = 500)[1])
          }
        },
        character(1)
      )
      paste(parts, collapse = "\n")
    },

    #' Summarize context without retaining source values in module state
    summarize_trace_inputs = function(inputs) {
      lapply(inputs, function(value) {
        list(
          class = class(value),
          length = length(value),
          bytes = as.numeric(utils::object.size(value)),
          sha256 = digest::digest(value, algo = "sha256")
        )
      })
    },

    #' Append one trace while keeping module state bounded
    append_bounded_trace = function(records, record) {
      limit <- getOption("dsprrr.rlm_trace_limit", 100L)
      if (
        !is.numeric(limit) ||
          length(limit) != 1L ||
          is.na(limit) ||
          !is.finite(limit) ||
          limit < 1L
      ) {
        limit <- 100L
      }
      records <- append(records, list(record))
      if (length(records) > floor(limit)) {
        records <- utils::tail(records, floor(limit))
      }
      records
    },

    #' Aggregate action and fallback usage into the standard result contract
    summarize_action_usage = function(
      history,
      fallback_metadata = NULL,
      recursive_metadata = list()
    ) {
      metadata <- lapply(history, function(entry) {
        entry$action_metadata %||% list()
      })
      if (!is.null(fallback_metadata)) {
        metadata <- append(metadata, list(fallback_metadata))
      }
      metadata <- append(metadata, recursive_metadata)
      usage_fields <- c(
        "input_tokens",
        "cached_input_tokens",
        "output_tokens",
        "total_tokens",
        "cost"
      )
      metadata <- lapply(metadata, function(entry) {
        if (identical(entry$cache %||% NULL, "hit")) {
          entry[usage_fields] <- list(0L, 0L, 0L, 0L, 0)
        }
        entry
      })
      sum_field <- function(field, integer = FALSE) {
        values <- vapply(
          metadata,
          function(entry) {
            value <- entry[[field]] %||% NA_real_
            if (!is.numeric(value) || length(value) != 1L) NA_real_ else value
          },
          numeric(1)
        )
        total <- if (length(values) == 0L || anyNA(values)) {
          NA_real_
        } else {
          sum(values, na.rm = TRUE)
        }
        if (integer && !is.na(total)) as.integer(total) else total
      }
      list(
        input_tokens = sum_field("input_tokens", integer = TRUE),
        cached_input_tokens = sum_field("cached_input_tokens", integer = TRUE),
        output_tokens = sum_field("output_tokens", integer = TRUE),
        total_tokens = sum_field("total_tokens", integer = TRUE),
        cost = sum_field("cost")
      )
    },

    #' Read a verified provider-call count from child metadata
    metadata_provider_calls = function(metadata) {
      if (!is.list(metadata)) {
        return(NA_integer_)
      }
      explicit <- metadata$provider_calls
      if (
        is.numeric(explicit) &&
          length(explicit) == 1L &&
          !is.na(explicit) &&
          is.finite(explicit) &&
          explicit >= 0 &&
          explicit == floor(explicit) &&
          explicit <= .Machine$integer.max
      ) {
        return(as.integer(explicit))
      }
      cache <- metadata$cache
      if (
        !is.character(cache) ||
          length(cache) != 1L ||
          is.na(cache)
      ) {
        return(NA_integer_)
      }
      switch(
        cache,
        hit = 0L,
        miss = 1L,
        bypass = 1L,
        NA_integer_
      )
    },

    #' Attach predictor-aware provider accounting before compacting metadata
    normalize_predictor_metadata = function(module, metadata) {
      if (!is.list(metadata)) {
        metadata <- list()
      }
      metadata$provider_calls <- optimizer_metadata_provider_calls(
        module,
        metadata
      )
      metadata
    },

    #' Sum provider-call counts while preserving unknown and overflow states
    sum_provider_call_counts = function(counts) {
      if (length(counts) == 0L) {
        return(0L)
      }
      valid <- is.numeric(counts) &&
        !anyNA(counts) &&
        all(is.finite(counts)) &&
        all(counts >= 0) &&
        all(counts == floor(counts))
      if (!valid) {
        return(NA_integer_)
      }
      total <- sum(as.double(counts))
      if (total > .Machine$integer.max) {
        return(NA_integer_)
      }
      as.integer(total)
    },

    #' Sum provider-call evidence from a list of metadata records
    summarize_provider_calls = function(metadata) {
      counts <- vapply(
        metadata,
        private$metadata_provider_calls,
        integer(1)
      )
      private$sum_provider_call_counts(counts)
    },

    #' Add recursive provider calls or mark the aggregate unknown
    record_recursive_provider_calls = function(call_counter, metadata) {
      count <- private$metadata_provider_calls(metadata)
      total <- private$sum_provider_call_counts(c(
        call_counter$provider_calls %||% 0L,
        count
      ))
      if (is.na(total)) {
        call_counter$provider_calls_known <- FALSE
      } else {
        call_counter$provider_calls <- total
      }
      invisible(call_counter)
    },

    #' Keep usage evidence without retaining repeated model-visible prompts
    compact_action_metadata = function(metadata) {
      fields <- c(
        "model",
        "prompt_length",
        "input_tokens",
        "cached_input_tokens",
        "output_tokens",
        "total_tokens",
        "cost",
        "duration_s",
        "latency_ms",
        "cache",
        "provider_calls"
      )
      metadata[intersect(fields, names(metadata))]
    },

    #' Get a model name without making model metadata mandatory for test doubles
    rlm_model_name = function(llm) {
      get_model <- tryCatch(llm[["get_model"]], error = function(e) NULL)
      if (!is.function(get_model)) {
        return(NA_character_)
      }
      model <- tryCatch(get_model(), error = function(e) NA_character_)
      if (!is.character(model) || length(model) != 1L) NA_character_ else model
    },

    #' Coerce final answer payload into named signature fields
    normalize_final_answer = function(
      answer,
      source = c("submit", "fallback")
    ) {
      source <- match.arg(source)
      output_specs <- private$get_output_specs()
      output_fields <- names(output_specs)
      required_fields <- private$get_required_output_field_names()

      label <- if (identical(source, "submit")) "SUBMIT" else "Fallback"
      normalized <- NULL
      if (
        is.list(answer) && length(answer) == 0L && length(required_fields) == 0L
      ) {
        normalized <- list()
      } else if (
        is.list(answer) && !is.null(names(answer)) && all(nzchar(names(answer)))
      ) {
        answer_names <- names(answer)
        if (
          length(output_fields) == 1L &&
            !output_fields[[1L]] %in% answer_names &&
            identical(answer_names, "answer")
        ) {
          normalized <- stats::setNames(list(answer[["answer"]]), output_fields)
        } else {
          missing <- setdiff(required_fields, answer_names)
          extra <- setdiff(answer_names, output_fields)
          if (length(missing) > 0L || length(extra) > 0L) {
            cli::cli_abort(
              c(
                "{label} output does not match the signature",
                "x" = "Expected fields: {.val {output_fields}}",
                "x" = "Received fields: {.val {answer_names}}"
              ),
              class = "dsprrr_rlm_output_validation_error"
            )
          }
          normalized <- answer[intersect(output_fields, answer_names)]
        }
      } else if (is.list(answer) && length(answer) == length(output_fields)) {
        normalized <- stats::setNames(answer, output_fields)
      } else if (is.atomic(answer) && length(answer) == length(output_fields)) {
        normalized <- stats::setNames(as.list(answer), output_fields)
      } else if (length(output_fields) == 1L) {
        normalized <- stats::setNames(list(answer), output_fields)
      } else {
        cli::cli_abort(
          c(
            "{label} output could not be aligned to the signature",
            "x" = "Expected fields: {.val {output_fields}}"
          ),
          class = "dsprrr_rlm_output_validation_error"
        )
      }

      for (field in names(normalized)) {
        normalized[[field]] <- private$coerce_value_to_type(
          normalized[[field]],
          output_specs[[field]],
          path = field
        )
      }

      normalized
    },

    #' Coerce a value to an ellmer type when practical
    coerce_value_to_type = function(value, type_spec, path = "value") {
      if (is.null(type_spec)) {
        return(value)
      }

      required <- tryCatch(isTRUE(type_spec@required), error = function(e) TRUE)
      if (is.null(value)) {
        if (!required) {
          return(NULL)
        }
        private$abort_output_type(path, "a non-null value", value)
      }

      if (inherits(type_spec, "ellmer::TypeBasic")) {
        type_name <- type_spec@type
        if (identical(type_name, "string")) {
          if (!is.atomic(value) || length(value) != 1L || is.na(value)) {
            private$abort_output_type(path, "one string", value)
          }
          return(as.character(value))
        }
        if (identical(type_name, "number")) {
          if (!is.atomic(value) || is.logical(value) || length(value) != 1L) {
            private$abort_output_type(path, "one finite number", value)
          }
          candidate <- suppressWarnings(as.numeric(value))
          if (
            length(candidate) != 1L || is.na(candidate) || !is.finite(candidate)
          ) {
            private$abort_output_type(path, "one finite number", value)
          }
          return(candidate)
        }
        if (identical(type_name, "integer")) {
          if (!is.atomic(value) || is.logical(value) || length(value) != 1L) {
            private$abort_output_type(path, "one integer", value)
          }
          candidate <- suppressWarnings(as.numeric(value))
          valid <- length(candidate) == 1L &&
            !is.na(candidate) &&
            is.finite(candidate) &&
            candidate == floor(candidate) &&
            abs(candidate) <= .Machine$integer.max
          if (!valid) {
            private$abort_output_type(path, "one integer", value)
          }
          return(as.integer(candidate))
        }
        if (identical(type_name, "boolean")) {
          if (is.logical(value) && length(value) == 1L && !is.na(value)) {
            return(value)
          }
          if (is.character(value) && length(value) == 1L && !is.na(value)) {
            candidate <- tolower(value)
            if (candidate %in% c("true", "false")) {
              return(identical(candidate, "true"))
            }
          }
          private$abort_output_type(path, "TRUE or FALSE", value)
        }
        return(value)
      }

      if (inherits(type_spec, "ellmer::TypeEnum")) {
        allowed <- as.character(type_spec@values)
        if (!is.atomic(value) || length(value) != 1L || is.na(value)) {
          private$abort_output_type(path, "one allowed enum value", value)
        }
        candidate <- as.character(value)
        if (candidate %in% allowed) {
          return(candidate)
        }
        case_match <- match(tolower(candidate), tolower(allowed))
        if (!is.na(case_match)) {
          return(allowed[[case_match]])
        }
        private$abort_output_type(
          path,
          paste0("one of ", paste(allowed, collapse = ", ")),
          value
        )
      } else if (inherits(type_spec, "ellmer::TypeArray")) {
        items <- if (is.list(value)) value else as.list(value)
        if (length(items) == 0L) {
          return(items)
        }
        coerced <- lapply(
          seq_along(items),
          function(index) {
            private$coerce_value_to_type(
              items[[index]],
              type_spec@items,
              path = paste0(path, "[[", index, "]]")
            )
          }
        )

        if (
          all(vapply(
            coerced,
            function(x) is.atomic(x) && length(x) == 1,
            logical(1)
          ))
        ) {
          return(unlist(coerced, use.names = FALSE))
        }
        coerced
      } else if (inherits(type_spec, "ellmer::TypeObject")) {
        if (
          !is.list(value) ||
            (length(value) > 0L &&
              (is.null(names(value)) || !all(nzchar(names(value)))))
        ) {
          private$abort_output_type(path, "a named object", value)
        }
        properties <- type_spec@properties
        properties <- properties[
          !vapply(
            properties,
            inherits,
            logical(1),
            what = "ellmer::TypeIgnore"
          )
        ]
        required_fields <- names(properties)[vapply(
          properties,
          function(spec) isTRUE(spec@required),
          logical(1)
        )]
        missing <- setdiff(required_fields, names(value))
        extra <- setdiff(names(value), names(properties))
        if (length(missing) > 0L) {
          private$abort_output_type(
            path,
            paste0("an object containing ", paste(missing, collapse = ", ")),
            value
          )
        }
        if (length(extra) > 0L && !isTRUE(type_spec@additional_properties)) {
          private$abort_output_type(
            path,
            paste0(
              "an object without extra fields: ",
              paste(extra, collapse = ", ")
            ),
            value
          )
        }
        normalized <- value
        for (name in intersect(names(properties), names(value))) {
          normalized[[name]] <- private$coerce_value_to_type(
            value[[name]],
            properties[[name]],
            path = paste0(path, "$", name)
          )
        }
        normalized
      } else {
        value
      }
    },

    #' Raise one consistent recoverable output-contract error
    abort_output_type = function(path, expected, value) {
      observed <- if (is.null(value)) {
        "NULL"
      } else {
        paste0(class(value)[[1L]], " of length ", length(value))
      }
      cli::cli_abort(
        c(
          "RLM output field {.field {path}} has the wrong type",
          "x" = "Expected {expected}; received {observed}."
        ),
        class = "dsprrr_rlm_output_validation_error"
      )
    },

    #' Build output matching signature
    build_output = function(answer, source = c("submit", "fallback")) {
      source <- match.arg(source)
      private$normalize_final_answer(answer, source = source)
    }
  )
)


#' Run a Recursive Language Model in one call
#'
#' @description
#' Run a one-off RLM investigation. By default this creates a fresh managed
#' [mcp_repl_runner()] for the invocation. Its default OS sandbox disables
#' network access but permits writes inside the allowed workspace. Pass
#' `.runner` or `.interpreter_factory` to select another execution backend. For
#' repeated use, optimization, or explicit lifecycle control, create an
#' [rlm_module()] instead. The managed default requires the suggested
#' `mcptools` package and Posit's external `mcp-repl` executable; see
#' [mcp_repl_runner()] for setup and transport limits.
#'
#' @param signature A Signature object or string notation defining inputs/outputs
#'   (e.g., `"question -> answer"`)
#' @param ... Named signature inputs and [run()] controls such as
#'   `.return_format`. Every supplied input is one scalar REPL variable,
#'   including vectors, lists, matrices, and data frames. To run multiple
#'   investigations, create an [rlm_module()] and call [run_dataset()]; store
#'   rich per-row values in list-columns.
#' @param .llm An ellmer Chat object. If `NULL`, uses the default Chat from
#'   [get_default_chat()].
#' @param .timeout Numeric. Maximum execution time in seconds per code
#'   evaluation for the implicit managed MCP runner. Explicit runners and
#'   factories own their timeout settings. Default 30.
#' @param .max_iterations Integer. Maximum REPL iterations before fallback.
#'   Default 20.
#' @param .max_llm_calls Integer. Maximum recursive LLM calls allowed.
#'   Default 50.
#' @param .max_output_chars Maximum model-visible characters per execution
#'   output. Default 10000.
#' @param .sub_lm Optional ellmer Chat for recursive `llm_query()` calls.
#'   `NULL` inherits `.llm`; use `.max_llm_calls = 0` to disable recursion.
#' @param .tools Named list of user-defined R functions or ellmer ToolDef
#'   objects available in the REPL. They execute in the dsprrr host process,
#'   outside the guest runner sandbox.
#' @param .verbose Logical. Print execution progress. Default `FALSE`.
#' @param .runner Optional caller-owned runner. Supply at most one of this and
#'   `.interpreter_factory`. Its policy must advertise `persistent = TRUE`.
#' @param .interpreter_factory Optional zero-argument factory for a fresh,
#'   invocation-owned runner. When both execution arguments are `NULL`, a
#'   managed `mcp_repl_runner()` factory is used. Custom factories must return
#'   a runner whose policy advertises `persistent = TRUE`.
#'
#' @return With `.return_format = "simple"` (the default), the output record
#'   according to the signature. With `.return_format = "structured"`, a
#'   `dsprrr_result` containing `output`, `chat`, and `metadata`.
#'
#' @export
#' @examples
#' \dontrun{
#' result <- rlm(
#'   "document, question -> answer",
#'   document = "Owner: team-a\nObligation: rotate keys quarterly",
#'   question = "What are the main themes?",
#'   .llm = ellmer::chat_openai(),
#'   .max_iterations = 4L,
#'   .max_llm_calls = 0L
#' )
#'
#' # Large or rich local R objects require explicit trusted execution.
#' sessions <- data.frame(
#'   release = c("2.3.9", "2.4.0"),
#'   converted = c(TRUE, FALSE)
#' )
#' local_runner <- r_code_runner(persistent = TRUE)
#' result <- rlm("sessions, question -> answer", sessions = sessions,
#'   question = "Where did conversion fall?",
#'   .llm = ellmer::chat_openai(),
#'   .runner = local_runner)
#' local_runner$close()
#' }
#'
#' @seealso
#' * [rlm_module()] for creating reusable RLM modules
#' * [r_code_runner()] for configuring the code execution backend
#' * [mcp_repl_runner()] for managed sandboxed execution
#' * [run()] for executing modules
#' * [dsp()] for simple one-shot LLM calls (no code execution)
rlm <- function(
  signature,
  ...,
  .llm = NULL,
  .timeout = 30,
  .max_iterations = 20L,
  .max_llm_calls = 50L,
  .max_output_chars = 10000L,
  .sub_lm = NULL,
  .tools = list(),
  .verbose = FALSE,
  .runner = NULL,
  .interpreter_factory = NULL
) {
  if (is.null(.runner) && is.null(.interpreter_factory)) {
    timeout <- .timeout
    max_output_chars <- .max_output_chars
    .interpreter_factory <- function() {
      mcp_repl_runner(
        timeout = timeout,
        max_output_chars = max_output_chars
      )
    }
  }

  mod <- rlm_module(
    signature = signature,
    runner = .runner,
    interpreter_factory = .interpreter_factory,
    max_iterations = .max_iterations,
    max_llm_calls = .max_llm_calls,
    max_output_chars = .max_output_chars,
    sub_lm = .sub_lm,
    verbose = .verbose,
    tools = .tools
  )

  run(mod, ..., .llm = .llm)
}
