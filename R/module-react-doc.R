#' ReAct Module
#'
#' @description
#' A ReAct-style module that can call tools during execution.
#' Use this to build tool-using agents that alternate between reasoning,
#' tool calls, and observations before producing a final structured answer.
#'
#' @details
#' Create a ReAct module with `module(type = "react", tools = list(...))`.
#' If you pass `tools` with `type = "predict"`, the module automatically
#' upgrades to `react`. ellmer executes the tool-calling loop internally; dsprrr
#' preserves its native turn history and tool-call IDs, enforces
#' `max_iterations`, and then produces a structured output based on the module
#' signature. Multiple tool calls in one assistant turn count as one iteration.
#'
#' @examples
#' \dontrun{
#' library(dsprrr)
#' library(ellmer)
#'
#' search_tool <- ellmer::tool(
#'   function(query) "Search results...",
#'   description = "Search for information",
#'   arguments = list(query = ellmer::type_string())
#' )
#'
#' agent <- module(
#'   signature("question -> answer"),
#'   type = "react",
#'   tools = list(search_tool),
#'   max_iterations = 8L
#' )
#'
#' result <- run(agent, question = "What is ReAct?", .llm = llm)
#'
#' # Batch execution
#' questions <- tibble::tibble(question = c("Q1", "Q2"))
#' results <- run_dataset(agent, questions, .llm = llm)
#' }
#'
#' @seealso
#' * [module()] for creating modules
#' * [ragnar_tool()] and [create_search_tool()] for search tools
#' * [code_act()] for tool + code execution agents
#'
#' @name module-react
NULL
