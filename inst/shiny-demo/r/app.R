library(shiny)

source("shinyreact.R", local = TRUE)
source("trace-data.R", local = TRUE)
source("rlm-live.R", local = TRUE)

server <- function(input, output, session) {
  # ---- Available Runs ----
  output$available_runs <- render_json({
    list_available_runs()
  })

  # ---- Replay Mode ----
  output$trace_data <- render_json({
    req(input$selected_run)
    load_trace(input$selected_run)
  })

  # ---- Live Mode ----
  observeEvent(input$start_live_run, {
    req(input$mode == "live")
    config <- input$start_live_run
    run_live_rlm(session, config)
  })
}

shinyApp(
  ui = page_react(title = "How RLMs Work - dsprrr Interactive Demo"),
  server = server
)
