#!/usr/bin/env Rscript

# Script to record vcr cassettes for vignettes and tests
# This should be run by maintainers with API keys set
# Usage: Rscript inst/scripts/record-cassettes.R

# Check for API keys
check_api_keys <- function() {
  keys_present <- c(
    OPENAI = nzchar(Sys.getenv("OPENAI_API_KEY")),
    ANTHROPIC = nzchar(Sys.getenv("ANTHROPIC_API_KEY")),
    GEMINI = nzchar(Sys.getenv("GOOGLE_GEMINI_API_KEY"))
  )

  if (!any(keys_present)) {
    stop("No API keys found. Please set at least one of:\n",
         "  - OPENAI_API_KEY\n",
         "  - ANTHROPIC_API_KEY\n",
         "  - GOOGLE_GEMINI_API_KEY")
  }

  message("Found API keys for: ", paste(names(keys_present)[keys_present], collapse = ", "))
}

# Record vignette cassettes
record_vignettes <- function() {
  message("\n=== Recording vignette cassettes ===\n")

  vignette_dir <- "vignettes"
  vcr_dir <- file.path(vignette_dir, "_vcr")

  # Create vcr directory if needed
  if (!dir.exists(vcr_dir)) {
    dir.create(vcr_dir, recursive = TRUE)
  }

  # Find all vignettes
  vignettes <- list.files(vignette_dir, pattern = "\\.Rmd$", full.names = TRUE)

  for (vignette in vignettes) {
    message("Recording cassettes for: ", basename(vignette))
    tryCatch({
      # Set environment to force evaluation
      withr::local_envvar(list(VITALS_SHOULD_EVAL = "true"))

      # Render vignette (this will record cassettes)
      rmarkdown::render(
        vignette,
        output_format = "html_document",
        quiet = TRUE,
        envir = new.env()
      )
      message("  ✓ Success")
    }, error = function(e) {
      message("  ✗ Error: ", e$message)
    })
  }
}

# Record test cassettes
record_tests <- function() {
  message("\n=== Recording test cassettes ===\n")

  # Load the package
  devtools::load_all()

  # Run integration tests (these use vcr)
  message("Running integration tests...")
  testthat::test_file("tests/testthat/test-integration.R")
}

# Clean up old cassettes
clean_cassettes <- function() {
  message("\n=== Cleaning old cassettes ===\n")

  # Remove old vignette cassettes
  old_vignettes <- list.files("vignettes/_vcr", pattern = "\\.yml$", full.names = TRUE)
  if (length(old_vignettes) > 0) {
    message("Removing ", length(old_vignettes), " old vignette cassettes")
    file.remove(old_vignettes)
  }

  # Remove old test cassettes
  old_tests <- list.files("tests/testthat/_vcr", pattern = "\\.yml$", full.names = TRUE)
  if (length(old_tests) > 0) {
    message("Removing ", length(old_tests), " old test cassettes")
    file.remove(old_tests)
  }
}

# Main execution
main <- function() {
  message("=================================")
  message("dsprrr Cassette Recording Script")
  message("=================================")

  # Check prerequisites
  check_api_keys()

  # Ask user if they want to clean old cassettes
  if (interactive()) {
    clean <- readline("Clean old cassettes first? (y/n): ")
    if (tolower(clean) == "y") {
      clean_cassettes()
    }
  }

  # Record new cassettes
  record_vignettes()
  record_tests()

  message("\n=== Recording complete ===")
  message("Cassettes have been saved to:")
  message("  - vignettes/_vcr/")
  message("  - tests/testthat/_vcr/")
  message("\nRemember to commit these cassettes if updating the package!")
}

# Run if executed directly
if (!interactive()) {
  main()
}