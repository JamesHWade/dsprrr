#' Export Module Traces
#'
#' @description
#' Export traces from a module as a tidy tibble for analysis and visualization.
#'
#' @param module A DSPrrr module with recorded traces
#' @param include_prompts Logical; whether to include full prompts in the output (default FALSE)
#' @param include_outputs Logical; whether to include full outputs in the output (default FALSE)
#'
#' @return A tibble with one row per trace containing:
#'   - timestamp: When the trace was recorded
#'   - latency_ms: Response time in milliseconds
#'   - input_tokens: Number of input tokens used
#'   - output_tokens: Number of output tokens generated
#'   - total_tokens: Total tokens (input + output)
#'   - cost: Cost in USD (if available from the provider)
#'   - model: The model name/version used
#'   - prompt_length: Character length of the prompt
#'   - prompt: The full prompt text (if include_prompts = TRUE)
#'   - output: The model output (if include_outputs = TRUE)
#'
#' @export
#' @examples
#' \dontrun{
#' # Get basic trace metrics
#' traces <- export_traces(my_module)
#'
#' # Include full prompts and outputs for analysis
#' full_traces <- export_traces(my_module,
#'                              include_prompts = TRUE,
#'                              include_outputs = TRUE)
#'
#' # Visualize token usage over time
#' library(ggplot2)
#' ggplot(traces, aes(x = timestamp, y = total_tokens)) +
#'   geom_line() +
#'   geom_point()
#' }
export_traces <- function(module, include_prompts = FALSE, include_outputs = FALSE) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("module must be a DSPrrr Module object")
  }

  traces <- module$state$traces

  if (length(traces) == 0) {
    cli::cli_inform("No traces recorded in this module")
    return(tibble::tibble())
  }

  # Start with basic metrics
  result <- module$get_traces()

  # Add optional fields
  if (include_prompts) {
    result$prompt <- vapply(traces, function(x) x$prompt %||% NA_character_, character(1))
  }

  if (include_outputs) {
    result$output <- lapply(traces, function(x) x$output)
  }

  result
}

#' Summarize Module Traces
#'
#' @description
#' Provide a statistical summary of module traces for performance analysis.
#'
#' @param module A DSPrrr module with recorded traces
#'
#' @return A list containing:
#'   - n_traces: Number of traces
#'   - total_tokens: Total tokens used across all traces
#'   - total_cost: Total cost in USD
#'   - avg_latency_ms: Average latency per request
#'   - avg_tokens_per_request: Average tokens per request
#'   - token_breakdown: List with input/output token totals
#'   - model_usage: Table of requests per model
#'
#' @export
#' @examples
#' \dontrun{
#' summary <- summarize_traces(my_module)
#' print(summary)
#' }
summarize_traces <- function(module) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("module must be a DSPrrr Module object")
  }

  summary <- module$trace_summary()
  traces_df <- module$get_traces()

  # Add model usage breakdown if we have traces
  if (nrow(traces_df) > 0) {
    model_counts <- table(traces_df$model)
    summary$model_usage <- as.data.frame(model_counts)
    names(summary$model_usage) <- c("model", "n_requests")

    summary$avg_latency_ms <- mean(traces_df$latency_ms, na.rm = TRUE)
    summary$avg_tokens_per_request <- mean(traces_df$total_tokens, na.rm = TRUE)

    summary$token_breakdown <- list(
      input = summary$total_input_tokens,
      output = summary$total_output_tokens,
      ratio = if (summary$total_input_tokens > 0) {
        round(summary$total_output_tokens / summary$total_input_tokens, 2)
      } else NA
    )
  }

  class(summary) <- c("dsprrr_trace_summary", class(summary))
  summary
}

#' @export
print.dsprrr_trace_summary <- function(x, ...) {
  cli::cli_h2("Module Trace Summary")

  if (x$n_traces == 0) {
    cli::cli_text("No traces recorded")
    return(invisible(x))
  }

  cli::cli_h3("Usage Metrics")
  cli::cli_bullets(c(
    "*" = "{x$n_traces} request{?s}",
    "*" = "{x$total_tokens} total tokens",
    "*" = "Input/Output ratio: {x$token_breakdown$ratio %||% 'N/A'}",
    "*" = "Total cost: ${format(x$total_cost, digits = 4)}"
  ))

  cli::cli_h3("Performance")
  cli::cli_bullets(c(
    "*" = "Average latency: {round(x$avg_latency_ms, 1)}ms",
    "*" = "Total time: {round(x$total_latency_ms/1000, 2)}s",
    "*" = "Tokens/request: {round(x$avg_tokens_per_request, 1)}"
  ))

  if (!is.null(x$model_usage) && nrow(x$model_usage) > 0) {
    cli::cli_h3("Models Used")
    for (i in seq_len(nrow(x$model_usage))) {
      cli::cli_text("  {x$model_usage$model[i]}: {x$model_usage$n_requests[i]} request{?s}")
    }
  }

  invisible(x)
}

#' Clear Module Traces
#'
#' @description
#' Clear all recorded traces from a module while preserving other state.
#'
#' @param module A DSPrrr module
#' @return The module (invisibly) with traces cleared
#'
#' @export
#' @examples
#' \dontrun{
#' # Clear traces after analysis
#' my_module <- clear_traces(my_module)
#' }
clear_traces <- function(module) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("module must be a DSPrrr Module object")
  }

  n_cleared <- length(module$state$traces)
  module$state$traces <- list()

  if (n_cleared > 0) {
    cli::cli_inform("Cleared {n_cleared} trace{?s}")
  }

  invisible(module)
}