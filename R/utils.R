#' Check if object inherits from ellmer type
#'
#' @noRd
is_ellmer_type <- function(x) {
  inherits(x, "ellmer::Type") ||
    inherits(x, "ellmer::TypeBasic") ||
    inherits(x, "ellmer::TypeEnum") ||
    inherits(x, "ellmer::TypeArray") ||
    inherits(x, "ellmer::TypeObject") ||
    inherits(x, "ellmer::TypeJsonSchema")
}

#' Determine if vignettes should be evaluated
#'
#' Checks for vcr cassettes or API credentials to determine
#' if vignette code should be executed. Always returns FALSE during
#' R CMD check to avoid cassette mismatch errors.
#'
#' @return Logical indicating whether to evaluate vignette code
#' @export
#' @keywords internal
eval_vignette <- function() {
  # Skip during R CMD check or CI (cassettes may not match current code)
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
    return(FALSE)
  }
  if (nzchar(Sys.getenv("CI"))) {
    return(FALSE)
  }

  name <- tools::file_path_sans_ext(knitr::current_input())

  # Check if vcr cassettes exist for this vignette
  cassettes <- dir("_vcr", pattern = paste0(name, "*"))
  has_cassette <- length(cassettes) > 0

  # Check if API keys are available
  has_key <- has_ellmer_credentials()

  # Suppress echo for cleaner vignettes
  options(ellmer_echo = "none")

  # Evaluate if we have keys OR cassettes
  has_key || has_cassette
}

#' Check for ellmer credentials
#'
#' @return Logical indicating if any LLM API keys are available
#' @keywords internal
has_ellmer_credentials <- function() {
  any(
    nzchar(Sys.getenv("OPENAI_API_KEY")),
    nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    nzchar(Sys.getenv("GOOGLE_GEMINI_API_KEY"))
  )
}

#' Find closest match for "Did you mean?" suggestions
#'
#' Uses Levenshtein distance to find the closest match to a given string
#' from a set of valid options.
#'
#' @param input The input string to match
#' @param valid_options Character vector of valid options
#' @param max_distance Maximum edit distance to consider (default 3)
#' @return The closest match, or NULL if no match within max_distance
#' @noRd
find_closest_match <- function(input, valid_options, max_distance = 3L) {
  if (length(valid_options) == 0) {
    return(NULL)
  }

  # Calculate distances
  distances <- vapply(
    valid_options,
    function(opt) {
      as.integer(utils::adist(tolower(input), tolower(opt))[1, 1])
    },
    integer(1)
  )

  # Find minimum distance

  min_idx <- which.min(distances)
  min_dist <- distances[min_idx]

  # Only suggest if within max_distance
  if (min_dist <= max_distance) {
    valid_options[min_idx]
  } else {
    NULL
  }
}

#' Format "Did you mean?" suggestion for error message
#'
#' @param input The input that didn't match
#' @param valid_options Character vector of valid options
#' @param max_distance Maximum edit distance to consider
#' @return A cli-formatted string, or NULL if no suggestion
#' @noRd
suggest_match <- function(input, valid_options, max_distance = 3L) {
  match <- find_closest_match(input, valid_options, max_distance)
  if (!is.null(match)) {
    paste0("Did you mean {.field ", match, "}?")
  } else {
    NULL
  }
}

#' Validate call inputs against a signature
#' @noRd
validate_signature_inputs <- function(
  sig,
  inputs,
  missing = c("error", "warn", "ignore"),
  extra = c("warn", "ignore"),
  type = c("warn", "ignore"),
  context = "inputs"
) {
  missing <- match.arg(missing)
  extra <- match.arg(extra)
  type <- match.arg(type)

  if (length(sig@inputs) == 0) {
    return(invisible(NULL))
  }

  required_names <- vapply(sig@inputs, function(x) x$name, character(1))
  provided_names <- names(inputs) %||% character()

  missing_names <- setdiff(required_names, provided_names)
  if (length(missing_names) > 0 && missing != "ignore") {
    message <- build_missing_input_message(
      missing_names = missing_names,
      provided_names = provided_names,
      required_names = required_names,
      context = context
    )

    if (missing == "error") {
      cli::cli_abort(message)
    } else {
      cli::cli_warn(message, class = "dsprrr_missing_input_warning")
    }
  }

  extra_names <- setdiff(provided_names, required_names)
  if (length(extra_names) > 0 && extra == "warn") {
    for (field in extra_names) {
      suggestion <- find_closest_match(field, required_names)
      if (!is.null(suggestion)) {
        cli::cli_warn(
          c(
            "Unknown input: {.field {field}}",
            "i" = "Did you mean: {.field {suggestion}}?",
            "i" = "Available fields: {.field {required_names}}"
          ),
          class = "dsprrr_extra_input_warning"
        )
      } else {
        cli::cli_warn(
          c(
            "Extra input not declared in the signature: {.field {field}}",
            "i" = "The field remains available to custom templates but is not declared in the signature.",
            "i" = "Signature fields: {.field {required_names}}"
          ),
          class = "dsprrr_extra_input_warning"
        )
      }
    }
  }

  if (
    type == "warn" && isTRUE(getOption("dsprrr.warn_on_type_mismatch", TRUE))
  ) {
    warn_signature_type_mismatches(sig, inputs)
  }

  invisible(NULL)
}

#' Build a missing-input message with suggestions
#' @noRd
build_missing_input_message <- function(
  missing_names,
  provided_names,
  required_names,
  context = "inputs"
) {
  msg <- c("Missing required {context}: {.field {missing_names}}")

  for (missing in missing_names) {
    suggestion <- suggest_match(missing, provided_names)
    if (!is.null(suggestion)) {
      msg <- c(msg, "i" = suggestion)
    }
  }

  c(
    msg,
    "i" = "Signature expects: {.field {required_names}}",
    if (length(provided_names) > 0) {
      c("i" = "You provided: {.field {provided_names}}")
    }
  )
}

#' Warn when provided values do not match declared input types
#' @noRd
warn_signature_type_mismatches <- function(sig, inputs) {
  for (input_spec in sig@inputs) {
    name <- input_spec$name
    if (!name %in% names(inputs)) {
      next
    }

    value <- inputs[[name]]
    expected_type <- input_spec$type

    if (
      !isTRUE(input_spec$.type_explicit) ||
        is.null(value) ||
        is_content_input(value)
    ) {
      next
    }

    if (!input_value_matches_type(value, expected_type)) {
      expected <- format_ellmer_type(expected_type, verbose = TRUE)
      actual <- paste(class(value), collapse = "/")
      cli::cli_warn(
        c(
          "Type mismatch for input {.field {name}}",
          "i" = "Expected {.val {expected}} from the signature, got {.cls {actual}}.",
          "i" = "Disable with {.code options(dsprrr.warn_on_type_mismatch = FALSE)}."
        ),
        class = "dsprrr_type_mismatch_warning"
      )
    }
  }

  invisible(NULL)
}

#' Check whether an R value is compatible with an ellmer type
#' @noRd
input_value_matches_type <- function(value, expected_type) {
  if (
    is.null(expected_type) || inherits(expected_type, "ellmer::TypeJsonSchema")
  ) {
    return(TRUE)
  }

  if (inherits(expected_type, "ellmer::TypeBasic")) {
    type_name <- expected_type@type
    return(switch(
      type_name,
      "string" = is.character(value),
      "number" = is.numeric(value),
      "integer" = is.integer(value) ||
        (is.numeric(value) && all(is.na(value) | value == floor(value))),
      "boolean" = is.logical(value),
      TRUE
    ))
  }

  if (inherits(expected_type, "ellmer::TypeEnum")) {
    return(
      is.character(value) && all(is.na(value) | value %in% expected_type@values)
    )
  }

  if (inherits(expected_type, "ellmer::TypeArray")) {
    return(is.atomic(value) || is.list(value))
  }

  if (inherits(expected_type, "ellmer::TypeObject")) {
    return(is.list(value) || is.data.frame(value) || is.environment(value))
  }

  TRUE
}

#' Check if a model is a reasoning model
#'
#' Reasoning models (OpenAI o1/o3/o4-mini, GPT-5 series) use different
#' parameters than traditional models. They don't support `temperature`
#' or `top_p`, instead using `reasoning_effort` (low/medium/high).
#'
#' @param model_name Character string of the model name (e.g., "o3", "gpt-5").
#' @return Logical indicating whether the model is a reasoning model.
#' @export
#' @examples
#' is_reasoning_model("gpt-4o")      # FALSE
#' is_reasoning_model("o3")          # TRUE
#' is_reasoning_model("o4-mini")     # TRUE
#' is_reasoning_model("gpt-5")       # TRUE
is_reasoning_model <- function(model_name) {
  if (is.null(model_name) || is.na(model_name) || !nzchar(model_name)) {
    return(FALSE)
  }
  model_lower <- tolower(model_name)

  reasoning_patterns <- c(
    "^o[0-9]", # o1, o3, o4-mini
    "^gpt-5", # gpt-5 series
    "-reasoning", # explicit reasoning suffix
    "reasoning" # generic reasoning indicator
  )

  any(vapply(reasoning_patterns, function(p) grepl(p, model_lower), logical(1)))
}

#' Get default parameters for a provider
#'
#' Returns sensible default parameter values and capability flags
#' for different LLM providers.
#'
#' @param provider Character string identifying the provider
#'   (e.g., "openai", "anthropic", "google").
#' @return A named list with default values and capabilities.
#' @export
#' @examples
#' provider_defaults("openai")
#' provider_defaults("anthropic")
provider_defaults <- function(provider) {
  defaults <- list(
    openai = list(
      temperature = 0.7,
      supports_json_schema = TRUE,
      supports_reasoning = TRUE
    ),
    anthropic = list(
      temperature = 1.0,
      supports_json_schema = TRUE,
      supports_extended_thinking = TRUE
    ),
    google = list(
      temperature = 0.7,
      supports_json_schema = TRUE
    ),
    ollama = list(
      temperature = 0.7,
      supports_json_schema = FALSE
    )
  )
  defaults[[tolower(provider)]] %||% list(temperature = 0.7)
}

#' Render an ellmer Turn as plain text
#' @noRd
render_turn_text <- function(turn) {
  render_turn_content(turn, format = "text")
}

#' Render an ellmer Turn as markdown
#' @noRd
render_turn_markdown <- function(turn) {
  render_turn_content(turn, format = "markdown")
}

#' Render an ellmer Turn as HTML
#' @noRd
render_turn_html <- function(turn) {
  render_turn_content(turn, format = "html")
}

#' Render an ellmer Turn with fallbacks for non-text content
#' @noRd
render_turn_content <- function(turn, format = c("text", "markdown", "html")) {
  format <- match.arg(format)

  if (is.null(turn)) {
    return(NA_character_)
  }

  rendered <- tryCatch(
    {
      switch(
        format,
        text = ellmer::contents_text(turn),
        markdown = ellmer::contents_markdown(turn),
        html = ellmer::contents_html(turn)
      )
    },
    error = function(e) NULL
  )

  if (!is.null(rendered) && length(rendered) == 1 && nzchar(rendered)) {
    return(as.character(rendered))
  }

  contents <- tryCatch(turn@contents, error = function(e) list())
  if (length(contents) == 0) {
    return(NA_character_)
  }

  parts <- vapply(
    contents,
    render_content_summary,
    character(1),
    format = format
  )
  parts <- parts[nzchar(parts)]

  if (length(parts) == 0) {
    NA_character_
  } else {
    paste(parts, collapse = if (identical(format, "html")) "" else "\n")
  }
}

#' Summarise a single ellmer content object
#' @noRd
render_content_summary <- function(
  content,
  format = c("text", "markdown", "html")
) {
  format <- match.arg(format)

  wrap <- function(text) {
    if (identical(format, "html")) {
      safe_text <- gsub("&", "&amp;", text, fixed = TRUE)
      safe_text <- gsub("<", "&lt;", safe_text, fixed = TRUE)
      safe_text <- gsub(">", "&gt;", safe_text, fixed = TRUE)
      paste0("<p>", safe_text, "</p>")
    } else {
      text
    }
  }

  is_content_class <- function(name) {
    any(grepl(paste0(name, "$"), class(content)))
  }

  if (is_content_class("ContentText")) {
    return(content@text %||% "")
  }

  if (is_content_class("ContentToolRequest")) {
    args <- jsonlite::toJSON(
      content@arguments,
      auto_unbox = TRUE,
      pretty = FALSE
    )
    return(wrap(paste0("[tool request] ", content@name, " ", args)))
  }

  if (is_content_class("ContentToolResult")) {
    result <- if (!is.null(content@error)) {
      paste0("error: ", content@error)
    } else {
      format_output(content@value)
    }
    tool_name <- tryCatch(content@request@name, error = function(e) "tool")
    return(wrap(paste0("[tool result] ", tool_name, " ", result)))
  }

  if (
    is_content_class("ContentImageRemote") ||
      is_content_class("ContentImageInline")
  ) {
    label <- paste0("[image] ", class(content)[1])
    return(wrap(label))
  }

  if (is_content_class("ContentPDF")) {
    return(wrap("[pdf]"))
  }

  wrap(paste0("[content] ", class(content)[1]))
}

#' Extract token metrics from a trace entry
#' @noRd
trace_tokens <- function(trace) {
  if (is.list(trace$tokens)) {
    return(list(
      input_tokens = as.integer(trace$tokens$input_tokens %||% NA_integer_),
      output_tokens = as.integer(trace$tokens$output_tokens %||% NA_integer_),
      cached_input_tokens = as.integer(
        trace$tokens$cached_input_tokens %||% NA_integer_
      ),
      total_tokens = as.integer(trace$tokens$total_tokens %||% NA_integer_)
    ))
  }

  assistant_turn <- trace$assistant_turn
  if (!is.null(assistant_turn) && !is.null(assistant_turn@tokens)) {
    tokens <- assistant_turn@tokens
    return(list(
      input_tokens = as.integer(tokens[1] %||% NA_integer_),
      output_tokens = as.integer(tokens[2] %||% NA_integer_),
      cached_input_tokens = as.integer(tokens[3] %||% NA_integer_),
      total_tokens = as.integer(sum(tokens[1:2], na.rm = TRUE))
    ))
  }

  list(
    input_tokens = as.integer(trace$input_tokens %||% NA_integer_),
    output_tokens = as.integer(trace$output_tokens %||% NA_integer_),
    cached_input_tokens = as.integer(
      trace$cached_input_tokens %||% NA_integer_
    ),
    total_tokens = as.integer(trace$total_tokens %||% NA_integer_)
  )
}

#' Extract cost from a trace entry
#' @noRd
trace_cost <- function(trace) {
  trace$cost %||%
    trace$total_cost %||%
    tryCatch(trace$assistant_turn@cost, error = function(e) NA_real_)
}

#' Extract latency from a trace entry
#' @noRd
trace_latency_ms <- function(trace) {
  trace$latency_ms %||%
    tryCatch(
      (trace$assistant_turn@duration %||% NA_real_) * 1000,
      error = function(e) NA_real_
    )
}

#' Extract prompt text from a trace entry
#' @noRd
trace_prompt_text <- function(trace) {
  trace$prompt %||% render_turn_text(trace$user_turn)
}

#' Extract prompt markdown from a trace entry
#' @noRd
trace_prompt_markdown <- function(trace) {
  render_turn_markdown(trace$user_turn)
}

#' Extract prompt HTML from a trace entry
#' @noRd
trace_prompt_html <- function(trace) {
  render_turn_html(trace$user_turn)
}

#' Extract response text from a trace entry
#' @noRd
trace_response_text <- function(trace) {
  if (!is.null(trace$assistant_turn)) {
    render_turn_text(trace$assistant_turn)
  } else if (!is.null(trace$output)) {
    format_output(trace$output)
  } else {
    NA_character_
  }
}

#' Extract response markdown from a trace entry
#' @noRd
trace_response_markdown <- function(trace) {
  if (!is.null(trace$assistant_turn)) {
    render_turn_markdown(trace$assistant_turn)
  } else {
    NA_character_
  }
}

#' Extract response HTML from a trace entry
#' @noRd
trace_response_html <- function(trace) {
  if (!is.null(trace$assistant_turn)) {
    render_turn_html(trace$assistant_turn)
  } else {
    NA_character_
  }
}
