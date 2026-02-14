# shiny-react utility functions for R
# Source: https://github.com/wch/create-shiny-react-app/blob/main/templates/2-scaffold/r/shinyreact.R
# License: MIT 2025, Posit Software, PBC

library(shiny)

page_bare <- function(..., title = NULL, lang = NULL) {
  ui <- list(
    shiny:::jqueryDependency(),
    if (!is.null(title)) tags$head(tags$title(title)),
    ...
  )
  attr(ui, "lang") <- lang
  ui
}

page_react <- function(
  ...,
  title = NULL,
  js_file = "main.js",
  css_file = "main.css",
  lang = "en"
) {
  page_bare(
    title = title,
    tags$head(
      if (!is.null(js_file)) tags$script(src = js_file, type = "module"),
      if (!is.null(css_file)) tags$link(href = css_file, rel = "stylesheet")
    ),
    tags$div(id = "root"),
    ...
  )
}

render_json <- function(
  expr,
  env = parent.frame(),
  quoted = FALSE,
  outputArgs = list(),
  sep = " "
) {
  func <- installExprFunction(
    expr,
    "func",
    env,
    quoted,
    label = "render_json"
  )

  createRenderFunction(
    func,
    function(value, session, name, ...) {
      value
    },
    function(...) {
      stop("Not implemented")
    },
    outputArgs
  )
}

post_message <- function(session, type, data) {
  namespaced_type <- session$ns(type)
  session$sendCustomMessage(
    "shinyReactMessage",
    list(
      type = namespaced_type,
      data = data
    )
  )
}
