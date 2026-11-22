#' Recursive Language Model (RLM) module
#'
#' @description
#' A module that explores its inputs with model-written R code. Documented
#' with [rlm_module()].
#'
#' @name module-rlm
#' @noRd
NULL

#' Create an RLM module that explores its inputs with R code
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' `rlm_module()` creates a recursive language model (RLM) module for inputs
#' whose useful evidence is too large, irregular or unpredictable to fit in one
#' prompt. The inputs stay in an R session. In each step the model writes R
#' code to inspect them, the runner executes it, and the model decides what to
#' look at next, until it submits an answer. [rlm()] runs a one-off
#' investigation.
#'
#' Use RLM when the inspection path is not known in advance. Use ordinary R
#' once that path is stable, or [flex()] when labeled examples should discover
#' a reusable implementation.
#'
#' @details
#' ## What the generated code can use
#'
#' Each input is available under `.context`. Besides ordinary R, the code can
#' call:
#'
#' * `SUBMIT(...)` to finish with the output values, by position or by name.
#' * `peek(var, start, end)` to look at a slice of a string (characters) or a
#'   vector (elements).
#' * `search(var, pattern)` to search a variable with a regular expression.
#' * `llm_query(query, context_slice)` and
#'   `llm_query_batched(queries, slices)` to ask a model a sub-question and
#'   get the answer back as a value. These count against `max_llm_calls`.
#' * The functions supplied in `tools`.
#'
#' A query slice may be a text string or an ordered list of records such as
#' `list(list(type = "text", text = "..."), list(type = "image_url",
#' url = "https://example.com/image.png", detail = "auto"))`. Inline images use
#' `type = "image_base64"`, an image MIME type, and base64 data up to 524,288
#' bytes. Image URLs must use HTTPS, omit user info, and be at most 2,048 bytes;
#' local paths and PDF records are unsupported. The managed
#' `mcp_repl_runner()` control frame defaults to 3,000 bytes, so inline data
#' must also fit that transport limit. These JSON-native records work without
#' ellmer in the guest process.
#'
#' Sub-queries and tools run in the host R process, not in the runner: the
#' generated code pauses, dsprrr handles the request, and the code is replayed
#' with the response. Variables persist across steps within one call, so the
#' runner's `policy()` must declare `persistent = TRUE`. Invalid submissions
#' become errors that the model can repair. If no valid submission is made in
#' `max_iterations` steps, the `extract` predictor makes one final typed
#' attempt.
#'
#' RLM checks string, number, integer, boolean, enum, array and object
#' outputs. Opaque `TypeJsonSchema` outputs are rejected when the module is
#' created, because they cannot be checked in the repair loop.
#'
#' `reasoning_mode` changes the action predictor, not the declared final
#' output. `"rationale"` uses `reasoning` and `code` action
#' fields. `"native"` requests code only and records provider-supplied
#' `ContentThinking` blocks in `get_metadata()` when they are present; the
#' structured metadata also has `native_reasoning_action_indices`, aligned
#' with those blocks. `"plan"` records a concise explicit `plan` with each
#' action. Retrieve native metadata from structured scalar [run()] results.
#' Batch, asynchronous and streaming paths reject native RLMs before execution.
#' A custom `generate_action` module must accept one required string `state`
#' input and return required string fields for the selected mode. Native custom
#' actions also set `config$reasoning_mode = "native"` so provider thinking is
#' captured using the same current-turn contract. The constructor validates
#' these contracts without rewriting the supplied module.
#'
#' ## Runners
#'
#' Supply exactly one of `runner` and `interpreter_factory`. For model-written
#' code, prefer a factory that returns a fresh sandboxed [mcp_repl_runner()].
#' That transport is bounded and works best for compact, JSON-compatible
#' inputs; its default sandbox disables network access but allows writes in
#' the workspace. It needs the suggested mcptools package and Posit's
#' `mcp-repl` executable. For large data frames or other rich R objects,
#' `r_code_runner(persistent = TRUE)` can stage the inputs once, but it runs
#' with your permissions, so use it for trusted input only. `tools` always run
#' in the host process with your permissions.
#'
#' A `runner` you supply is reused and never shut down by dsprrr; a runner from
#' `interpreter_factory` belongs to one call and is shut down when it ends.
#' [run_async()] and isolated [run_dataset()] execution work with a factory;
#' token streaming is unavailable. A direct [run()] call stages each input as
#' one variable, whatever its length. For several investigations, use
#' [run_dataset()] with list-columns for data frames, vectors, lists or other
#' rich values.
#'
#' ## Explicit conversation history
#'
#' A scalar synchronous [run()] can continue an RLM conversation by supplying
#' `.history = conversation_history()` or a history returned by [get_history()].
#' Set `.return_format = "structured"` to retain the next history in result
#' metadata. Every explicit-history call requires `interpreter_factory`; dsprrr
#' starts and shuts down one invocation-owned runner for each call. The factory
#' must return a fresh persistent interpreter. Previous task previews, observed
#' code and output are rendered into both action and fallback prompts as
#' bounded textual evidence. Previous variables and staged objects are not
#' restored, and historical code is not replayed. Context values in the record
#' are the bounded previews that were visible to the model. Histories are
#' sensitive, and a call fails explicitly if appending would exceed the
#' documented portable-history limits. ReAct and RLM histories cannot be mixed;
#' batch, dataset, asynchronous and streaming entrypoints reject explicit
#' history.
#'
#' ## Optimization and traces
#'
#' The `generate_action` and `extract` predictors appear in [named_modules()],
#' so [GEPA()] can tune them and [MIPROv2()] can tune their instructions (with
#' `max_bootstrapped_demos = 0L`). [BootstrapFewShot()] and [LabeledFewShot()]
#' reject programs that contain an RLM.
#'
#' The model sees at most `max_output_chars` characters of each execution
#' result, as a head-and-tail excerpt, and that view is kept in the returned
#' trajectory. Traces keep hashes and sizes of the inputs, not their values.
#' Structured metadata counts action, recursive and extraction calls
#' separately from verified provider calls. A cache hit counts as zero provider
#' calls, and totals are `NA` when any provider turn cannot be verified.
#'
#' @param signature A [signature()] object or a signature string. Outputs must
#'   use explicit string, number, integer, boolean, enum, array or object
#'   types.
#' @param runner A persistent code runner you own, such as
#'   `r_code_runner(persistent = TRUE)`. Its policy must declare
#'   `persistent = TRUE`, and it must not be used by two calls at the same
#'   time.
#' @param interpreter_factory A function with no arguments that returns a
#'   fresh persistent runner for each call, such as
#'   `function() mcp_repl_runner()`.
#' @param max_iterations Integer maximum number of code steps before the
#'   fallback extraction (default `20L`).
#' @param max_llm_calls Integer maximum number of sub-queries (default `50L`).
#'   Use `0L` to disable them.
#' @param max_output_chars Maximum number of characters of each execution
#'   result shown to the model (default `10000L`).
#' @param reasoning_mode How the action predictor handles reasoning:
#'   `"rationale"` (the default) returns a short `reasoning` field,
#'   `"native"` asks for code only and exposes provider-supplied thinking
#'   blocks in structured result metadata when available, and `"plan"` returns
#'   a concise explicit `plan` with the code. Native thinking is not synthesized
#'   when the provider does not return it.
#' @param sub_lm Optional ellmer Chat for sub-queries. `NULL` uses the chat of
#'   the call.
#' @param verbose Whether to print progress (default `FALSE`).
#' @param tools Named list of R functions or ellmer tools that the generated
#'   code can call. They run in the host process with your permissions. The
#'   generated code sends a request that dsprrr checks before calling the
#'   function; arguments are limited to JSON-compatible values (logical,
#'   integer, double, character, `NULL` and nested lists), and one step may
#'   make at most 1,000 tool calls. Tool definitions guide the model, but the
#'   function must still check its arguments.
#' @param config Optional module configuration, such as runtime settings.
#' @param chat Optional ellmer Chat stored on the module.
#' @param generate_action Optional replacement for the predictor that writes
#'   the code (advanced).
#' @param extract Optional replacement for the fallback extraction predictor
#'   (advanced).
#' @param ... Must be empty.
#'
#' @return An RLM module.
#'
#' @export
#' @family program constructors
#' @family code execution
#' @examplesIf rlang::is_installed("callr")
#' # Trusted input only: r_code_runner() runs with your permissions
#' runner <- r_code_runner(persistent = TRUE)
#' analyst <- rlm_module("document, question -> answer", runner = runner)
#' analyst
#' runner$shutdown()
#'
#' \dontrun{
#' # A fresh sandboxed session for each call
#' analyst <- rlm_module(
#'   "document, question -> answer",
#'   interpreter_factory = function() mcp_repl_runner(timeout = 30)
#' )
#' run(
#'   analyst,
#'   document = "Owner: team-a\nObligation: rotate keys quarterly",
#'   question = "Which obligations have no owner?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' }
rlm_module <- function(
  signature,
  runner = NULL,
  interpreter_factory = NULL,
  max_iterations = 20L,
  max_llm_calls = 50L,
  max_output_chars = 10000L,
  sub_lm = NULL,
  verbose = FALSE,
  tools = list(),
  config = list(),
  chat = NULL,
  generate_action = NULL,
  extract = NULL,
  reasoning_mode = c("rationale", "native", "plan"),
  ...
) {
  reject_partial_argument_matches(sys.call(), sys.function())
  reject_constructor_arguments("rlm_module", ...)

  binding <- normalize_code_runner_binding(
    runner = runner,
    interpreter_factory = interpreter_factory,
    module_name = "RLM"
  )

  # Parse signature if string
  if (is.character(signature)) {
    signature <- signature(signature)
  }

  reasoning <- normalize_rlm_reasoning_config(reasoning_mode, config)
  reasoning_mode <- reasoning$mode
  config <- reasoning$config

  if (!S7::S7_inherits(signature, Signature)) {
    cli::cli_abort(c(
      "signature must be a Signature object or string notation",
      "x" = "You provided: {.cls {class(signature)[1]}}"
    ))
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

  mod <- RLMModule$new(
    signature = signature,
    reasoning_mode = reasoning_mode,
    runner = binding$runner,
    interpreter_factory = binding$interpreter_factory,
    max_iterations = max_iterations,
    max_llm_calls = max_llm_calls,
    max_output_chars = max_output_chars,
    sub_lm = sub_lm,
    verbose = verbose,
    tools = tools,
    config = config,
    chat = chat,
    generate_action = generate_action,
    extract = extract
  )
  stamp_module_kind(mod, "rlm")
}


normalize_rlm_reasoning_config <- function(reasoning_mode, config) {
  choices <- c("rationale", "native", "plan")
  mode <- if (identical(reasoning_mode, choices)) {
    "rationale"
  } else {
    validate_rlm_reasoning_mode(reasoning_mode)
  }
  config <- normalize_module_config(config)
  if (!is.null(config$reasoning_mode)) {
    configured_mode <- validate_rlm_reasoning_mode(
      config$reasoning_mode,
      arg = "config$reasoning_mode"
    )
    if (!identical(configured_mode, mode)) {
      cli::cli_abort(
        c(
          "Conflicting RLM reasoning modes",
          "x" = "{.arg reasoning_mode} is {.val {mode}}, but `config$reasoning_mode` is {.val {configured_mode}}."
        ),
        class = "dsprrr_reasoning_mode_error"
      )
    }
  }
  config$reasoning_mode <- mode
  list(mode = mode, config = config)
}


validate_rlm_reasoning_mode <- function(mode, arg = "reasoning_mode") {
  choices <- c("rationale", "native", "plan")
  valid <- is.character(mode) &&
    length(mode) == 1L &&
    !is.na(mode) &&
    mode %in% choices
  if (!valid) {
    message <- if (identical(arg, "reasoning_mode")) {
      "{.arg reasoning_mode} must be one of {.val {choices}}."
    } else {
      "{.code config$reasoning_mode} must be one of {.val {choices}}."
    }
    cli::cli_abort(
      c(
        "Invalid RLM reasoning mode",
        "x" = message
      ),
      class = "dsprrr_reasoning_mode_error"
    )
  }
  mode
}


validate_rlm_sub_lm <- function(sub_lm) {
  if (is.null(sub_lm)) {
    return(invisible(sub_lm))
  }
  if (!is_ellmer_chat(sub_lm)) {
    cli::cli_abort(
      "{.arg sub_lm} must be an ellmer Chat R6 object",
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
  clone_ellmer_chat(chat, arg = "sub_lm", reset_turns = TRUE)
}


rlm_chat_prompt <- function(chat, prompt) {
  content <- if (is.list(prompt)) prompt else list(prompt)
  do.call(chat$chat, content)
}


new_rlm_action_module <- function(reasoning_mode = "rationale") {
  reasoning_mode <- validate_rlm_reasoning_mode(reasoning_mode)
  properties <- switch(
    reasoning_mode,
    rationale = list(
      reasoning = ellmer::type_string(
        description = "Brief rationale for the next operation"
      ),
      code = ellmer::type_string(
        description = "One non-empty R expression or block to execute"
      )
    ),
    native = list(
      code = ellmer::type_string(
        description = "One non-empty R expression or block to execute"
      )
    ),
    plan = list(
      plan = ellmer::type_string(
        description = "A concise high-level plan for this operation"
      ),
      code = ellmer::type_string(
        description = "One non-empty R expression or block to execute"
      )
    )
  )
  config <- if (identical(reasoning_mode, "native")) {
    list(reasoning_mode = "native")
  } else {
    list()
  }
  action_instructions <- c(
    "Choose the next useful R operation for an iterative investigation.",
    "Use the supplied state as the complete source of task and trajectory context."
  )
  instruction_suffix <- switch(
    reasoning_mode,
    rationale = NULL,
    native = "Return code only; provider reasoning is recorded as metadata when available.",
    plan = "Include a concise high-level plan with the code."
  )
  if (!is.null(instruction_suffix)) {
    action_instructions <- c(action_instructions, instruction_suffix)
  }

  PredictModule$new(
    signature = signature(
      inputs = list(input(
        "state",
        type = ellmer::type_string(),
        description = "RLM task, available variables, and prior REPL trajectory"
      )),
      output_type = do.call(ellmer::type_object, properties),
      instructions = paste(action_instructions, collapse = " ")
    ),
    config = config
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


validate_rlm_action_predictor <- function(module, reasoning_mode) {
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
  expected_fields <- switch(
    reasoning_mode,
    rationale = c("reasoning", "code"),
    native = "code",
    plan = c("plan", "code")
  )
  valid <- inherits(output_type, "ellmer::TypeObject") &&
    isTRUE(output_type@required) &&
    all(expected_fields %in% names(output_type@properties)) &&
    all(vapply(
      output_type@properties[expected_fields],
      rlm_predictor_required_string,
      logical(1)
    ))
  if (identical(reasoning_mode, "rationale")) {
    valid <- valid &&
      !isTRUE(output_type@additional_properties) &&
      identical(names(output_type@properties), expected_fields) &&
      all(vapply(
        output_type@properties,
        rlm_predictor_required_string,
        logical(1)
      ))
  } else {
    valid <- valid && !"reasoning" %in% names(output_type@properties)
  }
  if (!valid) {
    expected_description <- switch(
      reasoning_mode,
      rationale = "exactly the required string fields {.field reasoning} and {.field code}",
      native = "a required string {.field code} field and no {.field reasoning} output field",
      plan = "required string {.field plan} and {.field code} fields"
    )
    rlm_predictor_abort(
      predictor,
      paste("It must return", expected_description, "."),
      class
    )
  }
  configured_mode <- module_reasoning_mode(module)
  if (
    (!is.null(configured_mode) &&
      !identical(configured_mode, reasoning_mode)) ||
      (identical(reasoning_mode, "native") &&
        !identical(configured_mode, "native"))
  ) {
    expected_mode <- if (identical(reasoning_mode, "native")) {
      "{.val native} so provider thinking metadata is captured"
    } else {
      "absent or {.val {reasoning_mode}}"
    }
    rlm_predictor_abort(
      predictor,
      paste0("Its configured reasoning mode must be ", expected_mode, "."),
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


validate_rlm_predictor_children <- function(
  children,
  output_type,
  reasoning_mode = "rationale"
) {
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
  validate_rlm_action_predictor(children$generate_action, reasoning_mode)
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
    "llm_query_batched"
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
  as.character(text)
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
    #' @param reasoning_mode How the action predictor handles reasoning
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
      extract = NULL,
      reasoning_mode = c("rationale", "native", "plan")
    ) {
      reasoning <- normalize_rlm_reasoning_config(reasoning_mode, config)
      reasoning_mode <- reasoning$mode
      config <- reasoning$config

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
      generate_action <- generate_action %||%
        new_rlm_action_module(
          reasoning_mode
        )
      extract <- extract %||% new_rlm_extract_module(signature@output_type)
      validate_rlm_predictor_children(
        list(generate_action = generate_action, extract = extract),
        signature@output_type,
        reasoning_mode = reasoning_mode
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
      .history = NULL,
      ...
    ) {
      prepared <- rlm_prepare_invocation_phase(
        module = self,
        batch = batch,
        .llm = .llm,
        .cache = .cache,
        .history = .history
      )
      with_code_runner_lease(
        self$runner,
        self$interpreter_factory,
        "RLM",
        function(runner, lease) {
          start_time <- Sys.time()
          session <- rlm_prepare_runner(runner, lease, prepared$inputs)
          session_state_id <- paste0(".dsprrr_rlm_state_", rlm_control_nonce())
          if (!isTRUE(lease$owned)) {
            on.exit(
              private$cleanup_invocation_state(runner, session_state_id),
              add = TRUE
            )
          }
          operations <- list(
            describe_context = private$describe_context,
            build_system_prompt = private$build_system_prompt,
            build_iteration_prompt = private$build_iteration_prompt,
            get_code_response = private$get_code_response,
            execute_with_rlm_tools = private$execute_with_rlm_tools,
            compact_action_metadata = private$compact_action_metadata,
            extract_fallback = private$extract_fallback,
            build_output = private$build_output,
            summarize_action_usage = private$summarize_action_usage,
            metadata_provider_calls = private$metadata_provider_calls,
            summarize_provider_calls = private$summarize_provider_calls,
            sum_provider_call_counts = private$sum_provider_call_counts,
            summarize_trace_inputs = private$summarize_trace_inputs,
            append_bounded_trace = private$append_bounded_trace,
            rlm_model_name = private$rlm_model_name,
            aggregate_native_reasoning = private$aggregate_native_reasoning
          )
          execution <- rlm_execute_invocation_phase(
            module = self,
            prepared = prepared,
            runner = runner,
            lease = lease,
            operations = operations,
            session = session,
            session_state_id = session_state_id,
            start_time = start_time,
            trace = trace
          )
          rlm_finalize_invocation_phase(
            module = self,
            prepared = prepared,
            execution = execution,
            operations = operations,
            trace = trace
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
          reasoning_mode = module_reasoning_mode(self),
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
        reasoning_mode = module_reasoning_mode(self),
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
      validate_rlm_predictor_children(
        children,
        self$signature@output_type,
        reasoning_mode = module_reasoning_mode(self)
      )
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
            "i" = "Reset or shut down the runner before reusing it."
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
      rlm_format_excerpt(text, max_chars = 1000L)
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
    build_system_prompt = function(
      context_desc,
      has_sub_lm = TRUE,
      prior_context = ""
    ) {
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
          "- `llm_query_batched(queries, slices = NULL)`: Batch multiple sub-questions",
          paste0(
            "  A slice may be text or an ordered list of records: ",
            "list(list(type = 'text', text = '...'), ",
            "list(type = 'image_url', url = 'https://...', detail = 'auto'))."
          ),
          paste0(
            "  Inline images use ",
            "list(type = 'image_base64', mime_type = 'image/png', data = '...') ",
            "with at most 524288 base64 bytes. Image URLs must use HTTPS, omit ",
            "user info, and be no more than 2048 bytes; ",
            "local paths and PDF records are unsupported."
          ),
          paste0(
            "  The managed mcp-repl runner defaults to a 3000-byte control-frame ",
            "limit and rejects larger requests."
          )
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
      prior_section <- if (nzchar(prior_context)) {
        paste0(
          "## Prior RLM Invocation History\n",
          prior_context,
          "\n\n"
        )
      } else {
        ""
      }

      response_format <- switch(
        module_reasoning_mode(self),
        rationale = paste(
          '- "reasoning": Your thought process for this step',
          '- "code": R code to execute (single string)',
          sep = "\n"
        ),
        native = '- "code": R code to execute (single string)',
        plan = paste(
          '- "plan": A concise high-level plan for this operation',
          '- "code": R code to execute (single string)',
          sep = "\n"
        )
      )

      glue::glue(
        "
You are working in an R REPL environment. Your goal is to answer the query by
writing R code to explore and analyze the provided context.

## Task
{task_instructions}

{prior_section}## Available Variables
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
{response_format}

The code's output will be shown to you. Continue until you call SUBMIT().
"
      )
    },

    #' Render the mode-specific annotation retained for one RLM action
    format_action_annotation = function(action) {
      mode <- module_reasoning_mode(self)
      field <- switch(
        mode,
        rationale = "reasoning",
        native = NULL,
        plan = "plan"
      )
      if (is.null(field) || !field %in% names(action)) {
        return("")
      }
      label <- if (identical(mode, "plan")) "Plan" else "Reasoning"
      value <- action[[field]]
      if (is.null(value)) {
        value <- ""
      }
      paste0(label, ": ", paste(as.character(value), collapse = " "))
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
          annotation <- private$format_action_annotation(h)
          glue::glue(
            "
## Iteration {h$iteration}
{annotation}

Code:
```r
{h$code}
```

{if (h$success) 'Output:' else 'Error:'}
{rlm_format_excerpt(h$output, self$max_output_chars)}
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
      reasoning_mode <- module_reasoning_mode(self)

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
      if (identical(reasoning_mode, "plan")) {
        valid_plan <- is.character(result$plan) &&
          length(result$plan) == 1L &&
          !is.na(result$plan) &&
          nzchar(trimws(result$plan))
        if (!valid_plan) {
          cli::cli_abort(
            c(
              "LLM returned an invalid RLM action",
              "i" = "{.field plan} must be one non-empty string."
            ),
            class = "dsprrr_rlm_action_error"
          )
        }
      }
      metadata <- private$normalize_predictor_metadata(
        self$generate_action,
        action_result$metadata[[1L]] %||% list()
      )
      if (identical(reasoning_mode, "native")) {
        native_reasoning <- metadata$native_reasoning
        if (
          !is.null(native_reasoning) &&
            (!is.list(native_reasoning) ||
              !all(vapply(
                native_reasoning,
                inherits,
                logical(1),
                "ellmer::ContentThinking"
              )))
        ) {
          cli::cli_abort(
            c(
              "Native RLM action metadata is incompatible",
              "x" = "{.field native_reasoning} must be a list of ellmer ContentThinking objects or NULL."
            ),
            class = "dsprrr_rlm_action_metadata_error"
          )
        }
      }

      response <- list()
      if (identical(reasoning_mode, "rationale")) {
        response$reasoning <- result$reasoning %||% ""
      } else if (identical(reasoning_mode, "plan")) {
        response$plan <- result$plan %||% ""
      }
      response$code <- strip_rlm_code_fences(result$code)
      response$metadata <- metadata
      response$chat <- action_result$chat[[1L]] %||% action_llm
      response
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
      control_max_bytes <- runner_policy$rlm_control_frame_limit %||% NULL
      repeat {
        execution_context <- if (isTRUE(context_prepared)) list() else inputs
        execution_context[[rlm_control_replay_field()]] <- control_replay
        result <- execute_code_runner(
          runner,
          combined_code,
          context = execution_context,
          .control_nonce = control_nonce,
          .control_max_bytes = control_max_bytes
        )

        control_value <- NULL
        control_size_error <- NULL
        for (candidate in list(
          result$result,
          result$stdout,
          result$stderr,
          result$error
        )) {
          if (!isTRUE(result$success) && !is.character(candidate)) {
            next
          }
          decoded <- tryCatch(
            decode_rlm_control(
              candidate,
              control_nonce,
              max_bytes = control_max_bytes
            ),
            dsprrr_rlm_control_frame_size_error = identity
          )
          if (inherits(decoded, "dsprrr_rlm_control_frame_size_error")) {
            control_size_error <- decoded
            break
          }
          if (!is.null(decoded)) {
            control_value <- decoded
            break
          }
        }
        if (!is.null(control_size_error)) {
          message <- conditionMessage(control_size_error)
          return(list(
            success = FALSE,
            is_final = FALSE,
            final_value = NULL,
            formatted_output = paste0("Error: ", message),
            error = message,
            raw_result = result
          ))
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
        rlm_format_execution_output(result, self$max_output_chars)
      } else {
        paste("Error:", result$error %||% "Unknown error")
      }
      formatted_output <- rlm_format_excerpt(
        formatted_output,
        self$max_output_chars
      )

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
    build_rlm_query_prompt = function(query, context_slice) {
      if (is.null(context_slice)) {
        return(query)
      }
      if (is.character(context_slice)) {
        return(paste0(
          "Context:\n",
          context_slice,
          "\n\nQuestion: ",
          query
        ))
      }

      content <- lapply(context_slice, rlm_query_content_to_ellmer)
      c(
        list(ellmer::ContentText("Context:\n")),
        content,
        list(ellmer::ContentText(paste0("\n\nQuestion: ", query)))
      )
    },

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
      prompt <- private$build_rlm_query_prompt(query, context_slice)

      query_llm <- clone_rlm_chat(sub_lm)
      turns_before <- batch_chat_turns(query_llm)
      result <- tryCatch(
        {
          response <- rlm_chat_prompt(query_llm, prompt)
          text <- normalize_rlm_sub_lm_text(response)
          list(
            success = TRUE,
            value = text,
            formatted_output = paste0(
              "Query result: ",
              rlm_format_excerpt(text, self$max_output_chars)
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

      prompts <- lapply(
        seq_len(n_queries),
        function(i) {
          query <- queries[[i]]
          context_slice <- if (!is.null(slices)) slices[[i]] else NULL
          private$build_rlm_query_prompt(query, context_slice)
        }
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
              rlm_format_excerpt(value, self$max_output_chars)
            )
          },
          character(1)
        ),
        collapse = "\n\n"
      )

      list(
        success = TRUE,
        value = vapply(results, as.character, character(1)),
        formatted_output = rlm_format_excerpt(
          formatted_output,
          self$max_output_chars
        ),
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

      has_parallel_chat <- ellmer_function_available("parallel_chat")

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
            normalize_rlm_sub_lm_text(
              rlm_chat_prompt(query_llm, prompts[[i]])
            )
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

    #' Extract answer via fallback when max_iterations reached
    extract_fallback = function(
      inputs,
      history,
      llm,
      trace = TRUE,
      .cache = NULL,
      prior_context = "",
      context_desc = NULL
    ) {
      # Build trajectory summary
      trajectory <- vapply(
        history,
        function(h) {
          annotation <- private$format_action_annotation(h)
          glue::glue(
            "Iteration {h$iteration}:
{annotation}
Code: {h$code}
Output: {rlm_format_excerpt(h$output, max_chars = self$max_output_chars)}"
          )
        },
        character(1)
      )

      input_context <- private$format_inputs_for_prompt(inputs)
      variable_info <- if (is.null(context_desc)) {
        private$describe_context(inputs)
      } else {
        context_desc
      }
      prior_section <- if (nzchar(prior_context)) {
        paste0(
          "## Prior RLM Invocation History\n",
          prior_context,
          "\n\n"
        )
      } else {
        ""
      }

      prompt <- glue::glue(
        "
The RLM agent ran out of iterations before calling SUBMIT().
Based on the exploration trajectory below, extract the best possible answer.

{prior_section}## Original Task Instructions
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
        "provider_calls",
        "reasoning_mode",
        "native_reasoning"
      )
      metadata[intersect(fields, names(metadata))]
    },

    #' Flatten provider thinking blocks and retain their action positions
    aggregate_native_reasoning = function(history) {
      contents <- list()
      action_indices <- integer()
      for (i in seq_along(history)) {
        blocks <- history[[i]]$action_metadata$native_reasoning
        if (!is.list(blocks) || length(blocks) == 0L) {
          next
        }
        blocks <- Filter(
          function(block) inherits(block, "ellmer::ContentThinking"),
          blocks
        )
        if (length(blocks) == 0L) {
          next
        }
        contents <- c(contents, blocks)
        action_indices <- c(action_indices, rep.int(i, length(blocks)))
      }
      list(
        contents = if (length(contents) == 0L) NULL else contents,
        action_indices = if (length(action_indices) == 0L) {
          NULL
        } else {
          action_indices
        }
      )
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


#' Run a recursive language model (RLM) in one call
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description
#' `rlm()` runs a one-off RLM investigation: it builds an [rlm_module()], runs
#' it on the inputs in `...`, and returns the result. By default each call gets
#' a fresh managed [mcp_repl_runner()], whose sandbox disables network access
#' but allows writes in its workspace; this needs the suggested mcptools
#' package and Posit's `mcp-repl` executable. Pass `.runner` or
#' `.interpreter_factory` to use another backend. For repeated use,
#' optimization or control over the runner's lifetime, create an
#' [rlm_module()] instead.
#'
#' @param signature A [signature()] object or a signature string such as
#'   `"question -> answer"`.
#' @param ... Named inputs, plus [run()] options such as `.return_format` and
#'   `.history`.
#'   Each input is staged as one variable, including vectors, lists, matrices
#'   and data frames. For several investigations, create an [rlm_module()] and
#'   use [run_dataset()] with list-columns.
#' @param .llm An ellmer Chat. `NULL` uses the default chat from
#'   [get_default_chat()].
#' @param .timeout Maximum execution time per code step, in seconds, for the
#'   default managed runner (default 30). Runners you supply use their own
#'   timeout.
#' @param .max_iterations Integer maximum number of code steps before the
#'   fallback extraction (default `20L`).
#' @param .max_llm_calls Integer maximum number of sub-queries (default `50L`).
#' @param .max_output_chars Maximum number of characters of each execution
#'   result shown to the model (default `10000L`).
#' @param .reasoning_mode Action reasoning behavior: `"rationale"` (the
#'   default), `"native"` (code only, with provider thinking metadata when
#'   available), or `"plan"` (a concise plan and code).
#' @param .sub_lm Optional ellmer Chat for `llm_query()` sub-queries. `NULL`
#'   uses `.llm`; set `.max_llm_calls = 0L` to disable sub-queries.
#' @param .tools Named list of R functions or ellmer tools that the generated
#'   code can call. They run in the host R process, outside the sandbox.
#' @param .verbose Whether to print progress (default `FALSE`).
#' @param .runner Optional persistent runner you own. Supply at most one of
#'   this and `.interpreter_factory`.
#' @param .interpreter_factory Optional function with no arguments that returns
#'   a fresh persistent runner for the call. When both this and `.runner` are
#'   `NULL`, a managed [mcp_repl_runner()] is used.
#'
#' @return With `.return_format = "simple"` (the default), a named list with
#'   the signature's outputs. With `.return_format = "structured"`, a
#'   `dsprrr_result` with `output`, `chat` and `metadata`.
#'
#' @export
#' @family program constructors
#' @family code execution
#' @examples
#' \dontrun{
#' result <- rlm(
#'   "document, question -> answer",
#'   document = "Owner: team-a\nObligation: rotate keys quarterly",
#'   question = "What are the main themes?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   .max_iterations = 4L,
#'   .max_llm_calls = 0L
#' )
#'
#' # Native provider thinking is available on structured scalar results
#' native_result <- rlm(
#'   "document, question -> answer",
#'   document = "Owner: team-a\nObligation: rotate keys quarterly",
#'   question = "What are the main themes?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   .reasoning_mode = "native",
#'   .return_format = "structured"
#' )
#' get_metadata(native_result)$native_reasoning
#'
#' # Rich local R objects need a trusted runner that can stage them
#' sessions <- data.frame(
#'   release = c("2.3.9", "2.4.0"),
#'   converted = c(TRUE, FALSE)
#' )
#' local_runner <- r_code_runner(persistent = TRUE)
#' result <- rlm(
#'   "sessions, question -> answer",
#'   sessions = sessions,
#'   question = "Where did conversion fall?",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   .runner = local_runner
#' )
#' local_runner$shutdown()
#' }
rlm <- function(
  signature,
  ...,
  .llm = NULL,
  .timeout = 30,
  .max_iterations = 20L,
  .max_llm_calls = 50L,
  .max_output_chars = 10000L,
  .reasoning_mode = c("rationale", "native", "plan"),
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
    reasoning_mode = .reasoning_mode,
    sub_lm = .sub_lm,
    verbose = .verbose,
    tools = .tools
  )

  run(mod, ..., .llm = .llm)
}
