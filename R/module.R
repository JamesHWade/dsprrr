#' Create an LLM Module
#'
#' @description
#' The primary function for creating executable LLM modules. Supports
#' "predict" for standard structured prediction and "react" for ReAct-style
#' tool-using modules.
#'
#' @param signature A Signature object defining the module's interface
#' @param type Character string specifying the module type:
#'   - `"predict"` (default): Standard prediction module
#'   - `"react"`: ReAct-style module with tool support
#' @param tools Optional list of ellmer ToolDef objects for react modules.
#'   If provided with `type = "predict"`, automatically upgrades to react.
#' @param max_iterations Maximum ReAct iterations (default: 10, only for react)
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
module <- function(signature, type = "predict", tools = NULL, max_iterations = 10L,
                   template = "", demos = list(), config = list(), chat = NULL, ...) {
  # Validate signature
  if (!inherits(signature, "dsprrr::Signature")) {
    cli::cli_abort("First argument must be a Signature object")
  }

  # Validate chat if provided
  if (!is.null(chat) && !inherits(chat, "Chat")) {
    cli::cli_abort("{.arg chat} must be an ellmer Chat object")
  }

  # Auto-upgrade to react if tools are provided
  if (!is.null(tools) && length(tools) > 0 && type == "predict") {
    type <- "react"
  }

  # Validate type
  type <- match.arg(type, c("predict", "react"))

  # Create the appropriate R6 module based on type
  switch(type,
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
    cli::cli_abort("Unknown module type: {type}")
  )
}