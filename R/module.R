#' Create an LLM Module
#'
#' @description
#' The primary function for creating executable LLM modules. Supports
#' "predict" for standard structured prediction, "react" for ReAct-style
#' tool-using modules, "multichain" for multi-chain comparison, and
#' "program_of_thought" for code execution modules.
#'
#' @param signature A Signature object defining the module's interface
#' @param type Character string specifying the module type:
#'   - `"predict"` (default): Standard prediction module
#'   - `"react"`: ReAct-style module with tool support
#'   - `"multichain"`: MultiChainComparison module for ensemble reasoning
#'   - `"program_of_thought"`: Code execution module (requires runner)
#'   - `"codeact"`: Hybrid agent with tools + code execution (requires runner)
#' @param tools Optional list of ellmer ToolDef objects for react modules.
#'   If provided with `type = "predict"`, automatically upgrades to react.
#' @param max_iterations Maximum ReAct iterations (default: 10, only for react)
#' @param M Number of reasoning chains for multichain (default: 3)
#' @param temperature Temperature for multichain diversity (default: 0.7)
#' @param runner RCodeRunner for program_of_thought modules. Required for
#'   code execution types. Create with `r_code_runner()`.
#' @param max_iters Maximum code repair iterations for program_of_thought
#'   (default: 3)
#' @param extract_answer Logical. For program_of_thought, whether to use LLM
#'   to extract final answer from execution result (default: TRUE)
#' @param template Optional glue template for prompt generation
#' @param demos Optional list of demonstration examples
#' @param config Optional configuration list
#' @param chat Optional ellmer Chat object for LLM operations. If provided, the
#'   module will use this Chat for all predictions unless overridden with `.llm`.
#' @param ... Additional arguments for future module types
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
  ...
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
    c("predict", "react", "multichain", "program_of_thought", "codeact")
  )

  # Create the appropriate R6 module based on type
  switch(
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
    multichain = MultiChainComparisonModule$new(
      signature = signature,
      M = M,
      temperature = temperature,
      config = config,
      chat = chat
    ),
    program_of_thought = {
      if (is.null(runner)) {
        cli::cli_abort(c(
          "program_of_thought requires a runner",
          "i" = "Create one with: {.code runner <- r_code_runner()}",
          "i" = "Then pass it: {.code module(..., runner = runner)}"
        ))
      }
      ProgramOfThoughtModule$new(
        signature = signature,
        runner = runner,
        max_iters = max_iters,
        extract_answer = extract_answer,
        config = config,
        chat = chat
      )
    },
    codeact = {
      if (is.null(runner)) {
        cli::cli_abort(c(
          "codeact requires a runner",
          "i" = "Create one with: {.code runner <- r_code_runner()}",
          "i" = "Then pass it: {.code module(..., runner = runner)}"
        ))
      }
      CodeActModule$new(
        signature = signature,
        tools = tools %||% list(),
        runner = runner,
        max_iterations = max_iterations,
        config = config,
        chat = chat
      )
    },
    cli::cli_abort("Unknown module type: {type}")
  )
}
