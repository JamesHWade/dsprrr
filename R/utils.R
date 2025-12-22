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
