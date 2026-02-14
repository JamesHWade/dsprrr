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
      # Reset to defaults
      dsprrr:::configure_cache()
      dsprrr:::clear_cache()
      dsprrr::clear_prompt_history()
    },
    envir = .env
  )
  dsprrr:::clear_cache()
  dsprrr::clear_prompt_history()
}
