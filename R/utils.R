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
  # Skip during R CMD check (cassettes may not match current code)
  if (nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
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
  distances <- vapply(valid_options, function(opt) {
    as.integer(utils::adist(tolower(input), tolower(opt))[1, 1])
  }, integer(1))

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
    "^o[0-9]",           # o1, o3, o4-mini
    "^gpt-5",            # gpt-5 series
    "-reasoning",        # explicit reasoning suffix
    "reasoning"          # generic reasoning indicator
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
