#' Create an LLM Module
#'
#' @description
#' The primary function for creating executable LLM modules. Supports
#' "predict" for standard structured prediction, "react" for ReAct-style
#' tool-using modules, "chain_of_thought" for step-by-step reasoning,
#' "multichain" for multi-chain comparison, "program_of_thought" for code
#' execution modules, and experimental "flex" for declarative predictor graphs.
#'
#' @param signature A Signature object defining the module's interface
#' @param type Character string specifying the module type:
#'   - `"predict"` (default): Standard prediction module
#'   - `"react"`: ReAct-style module with tool support
#'   - `"chain_of_thought"`: Adds step-by-step reasoning to the signature
#'   - `"multichain"`: MultiChainComparison module for ensemble reasoning
#'   - `"program_of_thought"`: Code execution module (requires a runtime source)
#'   - `"codeact"`: Hybrid agent with tools + code execution (requires a runtime source)
#'   - `"rlm"`: Recursive Language Model for REPL-based context exploration
#'     (requires a runtime source)
#'   - `"flex"`: Experimental declarative predictor graph
#' @param tools Optional tools configuration:
#'   - for `type = "react"` or `type = "codeact"`: list of ellmer ToolDef objects.
#'   - for `type = "rlm"`: named list of R functions injected into the REPL.
#'   If provided with `type = "predict"`, automatically upgrades to react.
#' @param max_iterations Maximum iterations for ReAct, CodeAct, or RLM modules
#'   created through this generic factory (default: 10). For CodeAct it also
#'   caps tool calls within one invocation; exceeding that inner budget errors.
#' @param M Number of reasoning chains for multichain (default: 3)
#' @param temperature Temperature for multichain diversity (default: 0.7)
#' @param runner Optional caller-owned code runner implementing `execute()` and
#'   `policy()` for code execution types. It is never automatically closed.
#' @param max_iters Maximum code repair iterations for program_of_thought
#'   (default: 3), or the DSPy 3.3-compatible alias for RLM's
#'   `max_iterations`. For RLM, supply only one spelling.
#' @param extract_answer Logical. For program_of_thought, whether to use LLM
#'   to extract final answer from execution result (default: TRUE)
#' @param template Optional glue template for prompt generation
#' @param demos Optional list of demonstration examples
#' @param config Optional configuration list
#' @param chat Optional ellmer Chat object for LLM operations. If provided, the
#'   module will use this Chat for all predictions unless overridden with `.llm`.
#' @param interpreter_factory Optional zero-argument factory for
#'   program-of-thought, CodeAct, and RLM modules. It creates one fresh runner
#'   per invocation. It must implement terminal `close()`, which dsprrr calls
#'   exactly once on success, error, or interrupt. Supply exactly one of
#'   `runner` and `interpreter_factory` for code-executing types.
#' @param module_src Optional version 1 declarative JSON source for
#'   `type = "flex"`.
#' @param max_predictor_calls Maximum predictor steps allowed by a Flex source.
#' @param ... Additional arguments forwarded to [rlm_module()] when
#'   `type = "rlm"`. Reserved and required to be empty for `type = "flex"`.
#'
#' @return A module object (R6) that can be executed with `run()`
#' @export
#' @examples
#' # Create a simple prediction module
#' classifier <- signature("text -> sentiment") |>
#'   module(type = "predict", template = "Analyze: {text}")
#'
#' # With demonstrations
#' qa <- signature("context, question -> answer") |>
#'   module(
#'     type = "predict",
#'     demos = list(
#'       list(
#'         inputs = list(context = "...", question = "..."),
#'         output = "..."
#'       )
#'     )
#'   )
#'
#' # Create a multichain comparison module
#' mcc <- signature("question -> answer") |>
#'   module(type = "multichain", M = 5, temperature = 0.8)
#'
#' \dontrun{
#' # Execute the module (requires an llm object)
#' llm <- ellmer::chat_openai()
#' result <- classifier |> run(text = "Great package!", .llm = llm)
#'
#' # Or create module with Chat attached
#' classifier <- signature("text -> sentiment") |>
#'   module(type = "predict", chat = chat_openai())
#' result <- classifier |> run(text = "Great package!")  # No .llm needed
#'
#' # Create a ReAct module with tools
#' search_tool <- ellmer::tool(
#'   search_fn,
#'   description = "Search for information",
#'   arguments = list(query = ellmer::type_string())
#' )
#' agent <- signature("question -> answer") |>
#'   module(type = "react", tools = list(search_tool), chat = chat_openai())
#' }
module <- function(
  signature,
  type = "predict",
  tools = NULL,
  max_iterations = 10L,
  M = 3L,
  temperature = 0.7,
  runner = NULL,
  max_iters = 3L,
  extract_answer = TRUE,
  template = "",
  demos = list(),
  config = list(),
  chat = NULL,
  ...,
  interpreter_factory = NULL,
  module_src = NULL,
  max_predictor_calls = 100L
) {
  # Validate signature
  if (!inherits(signature, "dsprrr::Signature")) {
    cli::cli_abort(c(
      "First argument must be a Signature object",
      "x" = "Got {.cls {class(signature)[1]}}",
      "i" = "Create one with: {.code signature('question -> answer')}",
      "i" = "Or use explicit form: {.code Signature(inputs = list(input('q')), ...)}"
    ))
  }

  # Validate chat if provided
  if (!is.null(chat) && !inherits(chat, "Chat")) {
    cli::cli_abort(c(
      "{.arg chat} must be an ellmer Chat object",
      "x" = "Got {.cls {class(chat)[1]}}",
      "i" = "Create one with: {.code ellmer::chat_openai()} or {.code ellmer::chat_claude()}"
    ))
  }

  # Auto-upgrade to react if tools are provided
  if (!is.null(tools) && length(tools) > 0 && type == "predict") {
    type <- "react"
  }

  # Validate type
  type <- match.arg(
    type,
    c(
      "predict",
      "react",
      "chain_of_thought",
      "multichain",
      "program_of_thought",
      "codeact",
      "rlm",
      "flex"
    )
  )

  code_execution_types <- c("program_of_thought", "codeact", "rlm")
  if (!type %in% code_execution_types && !is.null(interpreter_factory)) {
    cli::cli_abort(
      "{.arg interpreter_factory} is only supported for code-executing module types.",
      class = "dsprrr_module_type_argument_error"
    )
  }
  if (!identical(type, "flex") && !is.null(module_src)) {
    cli::cli_abort(
      "{.arg module_src} is only supported when {.code type = \"flex\"}.",
      class = "dsprrr_module_type_argument_error"
    )
  }
  if (!identical(type, "flex") && !missing(max_predictor_calls)) {
    cli::cli_abort(
      "{.arg max_predictor_calls} is only supported when {.code type = \"flex\"}.",
      class = "dsprrr_module_type_argument_error"
    )
  }

  if (
    type %in%
      code_execution_types &&
      is.null(runner) &&
      is.null(interpreter_factory)
  ) {
    cli::cli_abort(
      c(
        "{type} requires a runner or interpreter_factory",
        "i" = "Use {.code runner = r_code_runner()} for a caller-owned runner.",
        "i" = "Use {.code interpreter_factory = r_code_runner} for a fresh runner per invocation."
      ),
      class = "dsprrr_interpreter_binding_error"
    )
  }

  # Create the appropriate R6 module based on type
  mod <- switch(
    type,
    predict = PredictModule$new(
      signature = signature,
      template = template,
      demos = demos,
      config = config,
      chat = chat
    ),
    react = ReactModule$new(
      signature = signature,
      tools = tools %||% list(),
      max_iterations = max_iterations,
      template = template,
      demos = demos,
      config = config,
      chat = chat
    ),
    chain_of_thought = PredictModule$new(
      signature = with_reasoning(signature),
      template = template,
      demos = demos,
      config = config,
      chat = chat
    ),
    multichain = MultiChainComparisonModule$new(
      signature = signature,
      M = M,
      temperature = temperature,
      config = config,
      chat = chat
    ),
    program_of_thought = {
      ProgramOfThoughtModule$new(
        signature = signature,
        runner = runner,
        max_iters = max_iters,
        extract_answer = extract_answer,
        config = config,
        chat = chat,
        interpreter_factory = interpreter_factory
      )
    },
    codeact = {
      CodeActModule$new(
        signature = signature,
        tools = tools %||% list(),
        runner = runner,
        max_iterations = max_iterations,
        config = config,
        chat = chat,
        interpreter_factory = interpreter_factory
      )
    },
    rlm = {
      rlm_args <- list(
        signature = signature,
        runner = runner,
        interpreter_factory = interpreter_factory,
        tools = tools %||% list(),
        config = config,
        chat = chat
      )
      if (!missing(max_iters)) {
        if (!missing(max_iterations)) {
          cli::cli_abort(
            "Supply only one of {.arg max_iterations} and {.arg max_iters}",
            class = "dsprrr_rlm_argument_conflict"
          )
        }
        rlm_args$max_iters <- max_iters
      } else {
        # Preserve the generic factory's historical RLM default of 10 while
        # allowing an explicitly supplied DSPy 3.3 spelling to take effect.
        rlm_args$max_iterations <- max_iterations
      }
      do.call(rlm_module, c(rlm_args, list(...)))
    },
    flex = {
      rlang::check_dots_empty()
      flex(
        signature = signature,
        module_src = module_src,
        max_predictor_calls = max_predictor_calls,
        config = config,
        chat = chat
      )
    },
    cli::cli_abort("Unknown module type: {type}")
  )

  mod$config <- normalize_module_config(mod$config)
  mod$config$.module_kind <- type

  mod
}
