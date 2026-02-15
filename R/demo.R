#' Run the Interactive RLM Demo
#'
#' @description
#' Launch an interactive Shiny app that demonstrates how Recursive Language
#' Models (RLMs) work. The app includes pre-recorded traces showing RLM
#' exploration of the bslib source code, with playback controls, educational
#' annotations, and an optional live execution mode.
#'
#' @param port Port to run the app on. If NULL, Shiny picks an available port.
#' @param launch.browser Whether to open the app in a browser (default TRUE).
#'
#' @details
#' The demo app has two modes:
#' - **Replay mode** (default): Watch pre-recorded RLM traces with playback
#'   controls. No API key needed.
#' - **Live mode**: Run your own RLM queries using an OpenAI API key.
#'
#' @return Invisibly returns the Shiny app object.
#'
#' @export
#' @examples
#' \dontrun{
#' run_demo()
#' }
run_demo <- function(port = NULL, launch.browser = TRUE) {
  rlang::check_installed("shiny", reason = "to run the interactive demo")

  app_dir <- system.file("shiny-demo", "r", package = "dsprrr")

  if (!nzchar(app_dir)) {
    cli::cli_abort(c(
      "Demo app not found in installed package",
      "i" = "Make sure dsprrr is installed with {.code devtools::install()}"
    ))
  }

  shiny::runApp(app_dir, port = port, launch.browser = launch.browser)
}
