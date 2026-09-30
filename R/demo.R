#' Run the interactive RLM demo
#'
#' @description
#' `run_demo()` opens a Shiny app that shows how recursive language models
#' ([rlm_module()]) work. It replays recorded traces of an RLM exploring the
#' bslib source code, with playback controls and annotations, and can also
#' run your own queries.
#'
#' @param port The port to run the app on. With `NULL`, Shiny picks one.
#' @param launch.browser If `TRUE` (the default), open the app in a browser.
#'
#' @details
#' Replay mode, the default, needs no API key. Live mode runs your own RLM
#' queries and needs an OpenAI API key. The app needs the shiny package.
#'
#' @return The value returned by [shiny::runApp()] when the app stops.
#'
#' @seealso [rlm_module()]
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
