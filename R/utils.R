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
