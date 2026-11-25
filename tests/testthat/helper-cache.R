# Cache test helpers

#' Reset cache state for tests
#'
#' Helper function to reset cache state between tests. Automatically restores
#' cache configuration when the test completes.
#'
#' @param .env Environment for deferred cleanup (default: parent.frame())
#' @noRd
local_reset_cache <- function(.env = parent.frame()) {
  withr::defer(
    {
      # Restore the memory-only configuration from setup.R
      dsprrr:::configure_cache(enable_disk = FALSE)
      dsprrr:::clear_cache()
      dsprrr::clear_prompt_history()
    },
    envir = .env
  )
  dsprrr:::clear_cache()
  dsprrr::clear_prompt_history()
}
