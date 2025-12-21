# dsprrr Development Environment
# This file is sourced when R starts in this project directory

# Only run in interactive sessions
if (interactive()) {
  # Load devtools for package development
  suppressMessages(require(devtools, quietly = TRUE))

  # Helpful message on startup
  message("\n dsprrr development environment loaded")
  message(" - devtools::load_all() to load package")
  message(" - devtools::test() to run tests")
  message(" - devtools::check() for R CMD check\n")

  # Check for required API keys
  api_keys <- c(
    ANTHROPIC_API_KEY = Sys.getenv("ANTHROPIC_API_KEY"),
    OPENAI_API_KEY = Sys.getenv("OPENAI_API_KEY")
  )

  missing_keys <- names(api_keys)[api_keys == ""]
  if (length(missing_keys) > 0) {
    message(" API keys not set: ", paste(missing_keys, collapse = ", "))
    message(" Set in .Renviron or export before running LLM tests\n")
  }

  # Set options for better development experience
  options(
    # Warn on partial argument matching
    warnPartialMatchArgs = TRUE,
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE,
    # Show more traceback on errors
    error = rlang::entrace,
    # Better tibble printing
    tibble.print_max = 20,
    tibble.width = 100
  )
}
