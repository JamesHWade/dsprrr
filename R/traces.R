#' Export a module's traces as a tibble
#'
#' @description
#' Every call of a module records a trace on it: when it ran, how long it
#' took, the tokens and cost, and the model. `export_traces()` returns these
#' traces as a tibble with one row per call, for analysis or plotting.
#' Prompts and outputs are left out unless you ask for them, because they
#' can contain sensitive data.
#'
#' @param module A module.
#' @param include_prompts If `TRUE`, add the prompts, as `prompt` (plain
#'   text), `prompt_markdown` and `prompt_html`.
#' @param include_outputs If `TRUE`, add the outputs and responses, as
#'   `output` (a list-column), `turns`, `response`, `response_text`,
#'   `response_markdown` and `response_html`.
#'
#' @return A tibble with one row per trace and the columns `timestamp`,
#'   `latency_ms`, `input_tokens`, `cached_input_tokens`, `output_tokens`,
#'   `total_tokens`, `cost` (in US dollars, when known), `model`,
#'   `prompt_length`, `program_artifact_id` and `trace_context`, plus the
#'   columns requested above. A module without traces gives an empty tibble
#'   and a message.
#'
#' @export
#' @family inspection
#' @examples
#' shout <- module_fn("text -> reply", function(text) toupper(text))
#' run(shout, text = "hello")
#' run(shout, text = "goodbye")
#' export_traces(shout)
#' export_traces(shout, include_outputs = TRUE)$output
export_traces <- function(
  module,
  include_prompts = FALSE,
  include_outputs = FALSE
) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("module must be a DSPrrr Module object")
  }

  traces <- module$state$traces

  if (length(traces) == 0) {
    cli::cli_inform("No traces recorded in this module")
    return(module$get_traces())
  }

  # Start with basic metrics. `get_traces()` always carries the prompt and
  # response text, so drop them unless the caller asked for them.
  result <- module$get_traces()
  if (!include_prompts) {
    result$prompt <- NULL
  }
  if (!include_outputs) {
    result$response <- NULL
  }

  # Add optional fields
  if (include_prompts) {
    result$prompt <- vapply(traces, trace_prompt_text, character(1))
    result$prompt_markdown <- vapply(
      traces,
      trace_prompt_markdown,
      character(1)
    )
    result$prompt_html <- vapply(traces, trace_prompt_html, character(1))
  }

  if (include_outputs) {
    result$output <- lapply(traces, function(x) x$output)
    result$turns <- lapply(traces, function(x) {
      x$turns %||% Filter(Negate(is.null), list(x$user_turn, x$assistant_turn))
    })
    result$response_text <- vapply(traces, trace_response_text, character(1))
    result$response_markdown <- vapply(
      traces,
      trace_response_markdown,
      character(1)
    )
    result$response_html <- vapply(
      traces,
      trace_response_html,
      character(1)
    )
  }

  result
}

#' Summarize a module's traces
#'
#' @description
#' `summarize_traces()` totals a module's traces: the number of calls,
#' tokens, cost and latency, and the calls per model. Printing the result
#' gives a short report.
#'
#' @param module A module.
#'
#' @return A list of class `dsprrr_trace_summary` with `n_traces`,
#'   `total_tokens`, `total_latency_ms` and `total_cost` (in US dollars, `NA`
#'   when a cost is unknown). For a module with traces, it also has
#'   `total_input_tokens`, `total_output_tokens`, `total_cached_tokens`,
#'   `total_duration_s`, `avg_latency_ms`, `avg_tokens_per_request`,
#'   `token_breakdown` (input and output totals and their ratio) and
#'   `model_usage` (a data frame of calls per model).
#'
#' @export
#' @family inspection
#' @examples
#' \dontrun{
#' qa <- module(signature("question -> answer"))
#' run(qa, question = "What is 2 + 2?", .llm = ellmer::chat_openai(model = "gpt-6-luna"))
#' summarize_traces(qa)
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
      } else {
        NA
      }
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
      cli::cli_text(
        "  {x$model_usage$model[i]}: {x$model_usage$n_requests[i]} request{?s}"
      )
    }
  }

  invisible(x)
}

#' Clear a module's traces
#'
#' @description
#' `clear_traces()` removes the traces recorded on a module and keeps
#' everything else, such as its demos and settings. The module is changed in
#' place. The session's prompt history is separate; see
#' [clear_prompt_history()].
#'
#' @param module A module.
#' @return The module, invisibly. A message reports how many traces were
#'   removed.
#'
#' @export
#' @family inspection
#' @examples
#' shout <- module_fn("text -> reply", function(text) toupper(text))
#' run(shout, text = "hello")
#' clear_traces(shout)
#' nrow(export_traces(shout))
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
