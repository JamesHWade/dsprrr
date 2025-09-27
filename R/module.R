#' Create an LLM Module
#'
#' @description
#' The primary function for creating executable LLM modules. Currently supports
#' "predict" type modules with planned support for additional types.
#'
#' @param signature A Signature object defining the module's interface
#' @param type Character string specifying the module type (currently only "predict")
#' @param template Optional glue template for prompt generation
#' @param demos Optional list of demonstration examples
#' @param config Optional configuration list
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
#' }
module <- function(signature, type = "predict", template = "", demos = list(),
                   config = list(), ...) {
  # Validate signature
  if (!inherits(signature, "dsprrr::Signature")) {
    cli::cli_abort("First argument must be a Signature object")
  }

  # Validate type
  type <- match.arg(type, c("predict"))  # Add more types here in the future

  # Create the appropriate R6 module based on type
  switch(type,
    predict = PredictModule$new(
      signature = signature,
      template = template,
      demos = demos,
      config = config
    ),
    # Future module types can be added here
    # chainofthought = ChainOfThoughtModule$new(...),
    # react = ReactModule$new(...),
    cli::cli_abort("Unknown module type: {type}")
  )
}