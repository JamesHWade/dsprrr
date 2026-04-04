#' Asynchronous Module Operations
#'
#' @description
#' Functions for running modules asynchronously using promises.
#' Useful for parallel execution or non-blocking operations.
#'
#' @name async
NULL

#' Run a module asynchronously
#'
#' @description
#' Executes a module and returns a promise that resolves to the result.
#' Useful for running multiple modules in parallel.
#'
#' @param module A dsprrr Module object
#' @param ... Named inputs matching the module's signature
#' @param .llm Optional ellmer Chat object
#'
#' @return A promise that resolves to the structured output
#'
#' @export
#' @examples
#' \dontrun{
#' # Run multiple modules in parallel
#' promises <- list(
#'   run_async(mod1, question = "Q1"),
#'   run_async(mod2, question = "Q2"),
#'   run_async(mod3, question = "Q3")
#' )
#'
#' # Wait for all to complete
#' promises::promise_all(.list = promises) |>
#'   promises::then(function(results) {
#'     # Process results
#'   })
#' }
run_async <- function(module, ..., .llm = NULL) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr Module object")
  }

  inputs <- list(...)
  request <- build_module_request(module, inputs)
  llm <- resolve_module_llm(module, .llm = .llm)

  # Use ellmer's async method
  llm$chat_structured_async(
    request$payload,
    type = module$signature@output_type
  )
}

#' Stream module output asynchronously
#'
#' @description
#' Streams text output from a module asynchronously.
#' Returns a promise that resolves to an async generator.
#'
#' @param module A dsprrr Module object
#' @param ... Named inputs matching the module's signature
#' @param .llm Optional ellmer Chat object
#'
#' @return A promise that resolves to an async generator
#'
#' @export
#' @examples
#' \dontrun{
#' stream_async(mod, question = "Write a story") |>
#'   promises::then(function(gen) {
#'     # Process async generator
#'   })
#' }
stream_async <- function(module, ..., .llm = NULL) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a dsprrr Module object")
  }

  inputs <- list(...)
  request <- build_module_request(module, inputs)
  llm <- resolve_module_llm(module, .llm = .llm)

  # Use ellmer's async stream method
  llm$stream_async(request$payload)
}

#' Build a simple prompt from inputs
#'
#' @description
#' Helper function to build a prompt from inputs without accessing
#' private module methods. Used by async functions.
#'
#' @param inputs Named list of input values
#' @param input_specs List of input specifications from signature
#'
#' @return Character string prompt
#'
#' @keywords internal
#' @noRd
build_simple_prompt <- function(inputs, input_specs) {
  if (length(input_specs) == 0) {
    return("")
  }

  input_lines <- character()
  for (spec in input_specs) {
    name <- spec$name
    if (name %in% names(inputs)) {
      value <- inputs[[name]]
      input_lines <- c(input_lines, paste0(name, ": ", value))
    }
  }

  if (length(input_lines) > 0) {
    paste(c("Input:", input_lines), collapse = "\n")
  } else {
    ""
  }
}
