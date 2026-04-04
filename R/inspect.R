#' Prompt Visibility and Inspection
#'
#' @description
#' Functions for inspecting LLM prompts and responses. These tools help with
#' debugging by showing exactly what was sent to the LLM and what was returned.
#'
#' @name prompt-visibility
NULL

# Initialize global prompt history in .dsprrr_env (done in chat-default.R)
# .dsprrr_env$prompt_history <- list()
# .dsprrr_env$prompt_history_max <- 100

#' Get the Last Prompt
#'
#' @description
#' Returns detailed information about the most recent LLM call, including
#' the prompt sent, response received, and metadata like tokens and cost.
#' Works with both `dsp()` calls and module-based calls.
#'
#' @return A `dsprrr_prompt_inspection` object containing:
#'   - `prompt`: The full prompt sent to the LLM
#'   - `response`: The LLM's response
#'   - `model`: The model used
#'   - `tokens_in`: Input tokens used
#'   - `tokens_out`: Output tokens generated
#'   - `cost`: Cost in USD (if available)
#'   - `timestamp`: When the call was made
#'   - `source`: Where the call originated ("dsp()" or module name)
#'
#' Returns `NULL` if no LLM calls have been made.
#'
#' @export
#' @examples
#' \dontrun{
#' # Make an LLM call
#' dsp("question -> answer", question = "What is 2+2?")
#'
#' # Inspect what happened
#' get_last_prompt()
#' #> ─── Last Prompt ───────────────────────────────────
#' #> System: Given the fields `question`, produce the fields `answer`.
#' #>
#' #> User: question: What is 2+2?
#' #>
#' #> ─── Response ──────────────────────────────────────
#' #> Assistant: {"answer": "4"}
#' #>
#' #> ─── Metadata ──────────────────────────────────────
#' #> Model: gpt-4o-mini | Tokens: 45 in, 12 out | Cost: $0.0001
#' }
get_last_prompt <- function() {
  history <- .dsprrr_env$prompt_history
  if (is.null(history) || length(history) == 0) {
    cli::cli_inform("No LLM calls recorded yet")
    return(invisible(NULL))
  }

  # Get the most recent entry
  entry <- history[[length(history)]]
  create_prompt_inspection(entry)
}

#' Inspect LLM Call History
#'
#' @description
#' Returns a tibble of recent LLM calls across all modules and `dsp()` calls.
#' Similar to DSPy's `dspy.inspect_history(n)`.
#'
#' @param n Number of recent calls to return. Default is 10.
#' @param include_prompts Logical; whether to include full prompt text.
#'   Default is TRUE.
#' @param include_responses Logical; whether to include full response text.
#'   Default is TRUE.
#'
#' @return A tibble with one row per LLM call containing:
#'   - `timestamp`: When the call was made
#'   - `source`: Where the call originated ("dsp()" or module class name)
#'   - `model`: The model used
#'   - `tokens_in`: Input tokens
#'   - `tokens_out`: Output tokens
#'   - `cost`: Cost in USD (if available)
#'   - `duration_s`: Duration in seconds (if available)
#'   - `prompt`: Full prompt text (if `include_prompts = TRUE`)
#'   - `response`: Full response text (if `include_responses = TRUE`)
#'
#' @export
#' @examples
#' \dontrun{
#' # View last 5 LLM calls
#' inspect_history(n = 5)
#'
#' # Get history as tibble for analysis
#' history <- inspect_history(n = 20)
#' sum(history$cost)  # Total cost
#' }
inspect_history <- function(
  n = 10,
  include_prompts = TRUE,
  include_responses = TRUE
) {
  history <- .dsprrr_env$prompt_history
  if (is.null(history) || length(history) == 0) {
    cli::cli_inform("No LLM calls recorded yet")
    return(tibble::tibble())
  }

  # Get the last n entries (or all if fewer)
  n_available <- length(history)
  n_to_get <- min(n, n_available)
  start_idx <- n_available - n_to_get + 1
  entries <- history[start_idx:n_available]

  # Build tibble from entries
  result <- tibble::tibble(
    timestamp = vapply(
      entries,
      function(e) format(e$timestamp, "%Y-%m-%d %H:%M:%S"),
      character(1)
    ),
    source = vapply(
      entries,
      function(e) e$source %||% "unknown",
      character(1)
    ),
    model = vapply(
      entries,
      function(e) e$model %||% NA_character_,
      character(1)
    ),
    tokens_in = vapply(
      entries,
      function(e) e$tokens_in %||% NA_integer_,
      integer(1)
    ),
    tokens_out = vapply(
      entries,
      function(e) e$tokens_out %||% NA_integer_,
      integer(1)
    ),
    cost = vapply(
      entries,
      function(e) e$cost %||% NA_real_,
      numeric(1)
    ),
    duration_s = vapply(
      entries,
      function(e) e$duration_s %||% NA_real_,
      numeric(1)
    )
  )

  if (include_prompts) {
    result$prompt <- vapply(
      entries,
      function(e) e$prompt %||% NA_character_,
      character(1)
    )
  }

  if (include_responses) {
    result$response <- vapply(
      entries,
      function(e) e$response %||% NA_character_,
      character(1)
    )
  }

  result
}

#' Clear Prompt History
#'
#' @description
#' Clears the global prompt history. Useful for testing or to free memory.
#'
#' @return Invisibly returns the number of entries cleared.
#'
#' @export
#' @examples
#' \dontrun{
#' # Clear history
#' clear_prompt_history()
#' }
clear_prompt_history <- function() {
  n_cleared <- length(.dsprrr_env$prompt_history %||% list())
  .dsprrr_env$prompt_history <- list()

  if (n_cleared > 0) {
    cli::cli_inform("Cleared {n_cleared} prompt history entr{?y/ies}")
  }

  invisible(n_cleared)
}

# Internal: Add an entry to the global prompt history
# Called from dsp.R and module-predict.R
add_to_global_history <- function(trace, source = "unknown") {
  tryCatch(
    {
      # Initialize history if needed
      if (is.null(.dsprrr_env$prompt_history)) {
        .dsprrr_env$prompt_history <- list()
      }

      # Extract data from trace
      entry <- extract_history_entry(trace, source)

      # Append to history
      .dsprrr_env$prompt_history <- append(
        .dsprrr_env$prompt_history,
        list(entry)
      )

      # Prune if needed (ring buffer behavior)
      max_history <- getOption(
        "dsprrr.prompt_history_max",
        default = 100
      )
      if (length(.dsprrr_env$prompt_history) > max_history) {
        .dsprrr_env$prompt_history <- utils::tail(
          .dsprrr_env$prompt_history,
          max_history
        )
      }

      invisible(NULL)
    },
    error = function(e) {
      # Always warn about history capture failures so users know something is wrong
      # Use a condition class so advanced users can catch/suppress if needed
      # Use "regularly" frequency so repeated errors are visible
      cli::cli_warn(
        c(
          "Failed to capture prompt history",
          "i" = "Error: {e$message}",
          "i" = "This is a non-critical error; your code will continue to work.",
          "i" = "Set {.code options(dsprrr.verbose = TRUE)} for more details."
        ),
        class = "dsprrr_history_capture_warning",
        .frequency = "regularly",
        .frequency_id = "add_history_error"
      )
      invisible(NULL)
    }
  )
}

# Internal: Extract a standardized history entry from a trace
extract_history_entry <- function(trace, source) {
  # Handle different trace formats (dsp() vs module)
  entry <- list(
    timestamp = trace$timestamp %||% Sys.time(),
    source = source
  )

  # Extract prompt - try multiple sources
  if (!is.null(trace$prompt)) {
    entry$prompt <- trace$prompt
  } else if (!is.null(trace$user_turn)) {
    # Extract from ellmer UserTurn object
    entry$prompt <- extract_turn_text(trace$user_turn)
  }

  # Extract response
  if (!is.null(trace$output)) {
    entry$response <- format_output_for_history(trace$output)
  } else if (!is.null(trace$assistant_turn)) {
    entry$response <- extract_turn_text(trace$assistant_turn)
  }

  # Extract metadata from assistant_turn if available
  # Wrap in tryCatch since S7 slot access can fail for incompatible objects
  if (!is.null(trace$assistant_turn)) {
    at <- trace$assistant_turn
    tryCatch(
      {
        # ellmer AssistantTurn stores tokens as c(input, output, cached)
        if (
          !is.null(at@tokens) &&
            is.numeric(at@tokens) &&
            length(at@tokens) >= 2
        ) {
          entry$tokens_in <- as.integer(at@tokens[1])
          entry$tokens_out <- as.integer(at@tokens[2])
        }
        if (!is.null(at@cost)) {
          entry$cost <- at@cost
        }
        if (!is.null(at@duration)) {
          entry$duration_s <- at@duration
        }
      },
      error = function(e) {
        # S7 slot access failed - object may not be compatible
        # This is non-critical, just skip metadata extraction
        if (getOption("dsprrr.verbose", FALSE)) {
          cli::cli_warn(
            "Failed to extract metadata from assistant turn: {e$message}",
            .frequency = "once",
            .frequency_id = "extract_turn_metadata_error"
          )
        }
      }
    )
  }

  # Try legacy metadata fields
  if (is.null(entry$tokens_in) && !is.null(trace$input_tokens)) {
    entry$tokens_in <- as.integer(trace$input_tokens)
  }
  if (is.null(entry$tokens_out) && !is.null(trace$output_tokens)) {
    entry$tokens_out <- as.integer(trace$output_tokens)
  }
  if (is.null(entry$cost) && !is.null(trace$cost)) {
    entry$cost <- trace$cost
  }
  if (is.null(entry$duration_s) && !is.null(trace$duration_s)) {
    entry$duration_s <- trace$duration_s
  }

  # Extract model name
  entry$model <- trace$model %||% NA_character_

  entry
}

# Internal: Extract text content from an ellmer Turn object
extract_turn_text <- function(turn) {
  if (is.null(turn)) {
    return(NA_character_)
  }

  tryCatch(
    render_turn_text(turn),
    error = function(e) {
      # Warn in verbose mode to help with debugging during development
      if (getOption("dsprrr.verbose", FALSE)) {
        cli::cli_warn(
          "Failed to extract text from Turn object: {e$message}",
          .frequency = "once",
          .frequency_id = "extract_turn_text_error"
        )
      }
      NA_character_
    }
  )
}

# Internal: Format output for history storage
format_output_for_history <- function(output) {
  if (is.null(output)) {
    return(NA_character_)
  }

  tryCatch(
    {
      if (is.character(output) && length(output) == 1) {
        output
      } else if (is.list(output)) {
        jsonlite::toJSON(output, auto_unbox = TRUE, pretty = FALSE)
      } else {
        as.character(output)
      }
    },
    error = function(e) {
      # Warn in verbose mode to help with debugging during development
      if (getOption("dsprrr.verbose", FALSE)) {
        cli::cli_warn(
          "Failed to format output for history: {e$message}",
          .frequency = "once",
          .frequency_id = "format_output_error"
        )
      }
      tryCatch(as.character(output), error = function(e2) NA_character_)
    }
  )
}

# Internal: Create a prompt inspection object
create_prompt_inspection <- function(entry) {
  structure(
    list(
      prompt = entry$prompt %||% NA_character_,
      response = entry$response %||% NA_character_,
      model = entry$model %||% NA_character_,
      tokens_in = entry$tokens_in %||% NA_integer_,
      tokens_out = entry$tokens_out %||% NA_integer_,
      cost = entry$cost %||% NA_real_,
      duration_s = entry$duration_s %||% NA_real_,
      timestamp = entry$timestamp %||% Sys.time(),
      source = entry$source %||% "unknown"
    ),
    class = "dsprrr_prompt_inspection"
  )
}

#' @export
print.dsprrr_prompt_inspection <- function(x, ...) {
  cli::cli_h2("Prompt Inspection")

  # Source and timestamp
  cli::cli_text(
    "{.emph {x$source}} at {format(x$timestamp, '%Y-%m-%d %H:%M:%S')}"
  )
  cli::cat_line()

  # Prompt section
  cli::cli_h3("Prompt")
  if (!is.na(x$prompt) && nzchar(x$prompt)) {
    # Truncate if very long
    prompt_display <- if (nchar(x$prompt) > 500) {
      paste0(substr(x$prompt, 1, 500), "\n... (truncated)")
    } else {
      x$prompt
    }
    cli::cat_line(prompt_display)
  } else {
    cli::cli_text("{.emph No prompt available}")
  }
  cli::cat_line()

  # Response section
  cli::cli_h3("Response")
  if (!is.na(x$response) && nzchar(x$response)) {
    response_display <- if (nchar(x$response) > 500) {
      paste0(substr(x$response, 1, 500), "\n... (truncated)")
    } else {
      x$response
    }
    cli::cat_line(response_display)
  } else {
    cli::cli_text("{.emph No response available}")
  }
  cli::cat_line()

  # Metadata section
  cli::cli_h3("Metadata")
  metadata_parts <- character()

  if (!is.na(x$model)) {
    metadata_parts <- c(metadata_parts, paste("Model:", x$model))
  }

  if (!is.na(x$tokens_in) && !is.na(x$tokens_out)) {
    metadata_parts <- c(
      metadata_parts,
      paste0("Tokens: ", x$tokens_in, " in, ", x$tokens_out, " out")
    )
  }

  if (!is.na(x$cost) && x$cost > 0) {
    metadata_parts <- c(
      metadata_parts,
      paste0("Cost: $", format(x$cost, digits = 4, scientific = FALSE))
    )
  }

  if (!is.na(x$duration_s)) {
    metadata_parts <- c(
      metadata_parts,
      paste0("Duration: ", round(x$duration_s, 2), "s")
    )
  }

  if (length(metadata_parts) > 0) {
    cli::cli_text(paste(metadata_parts, collapse = " | "))
  } else {
    cli::cli_text("{.emph No metadata available}")
  }

  invisible(x)
}
