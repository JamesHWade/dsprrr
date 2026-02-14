# trace-data.R - Load and serve pre-recorded RLM traces

#' List available pre-recorded runs
#' @return Named list of run metadata
list_available_runs <- function() {
  data_dir <- file.path(
    system.file("shiny-demo", package = "dsprrr"),
    "data", "runs"
  )

  # Fallback for development (running from inst/shiny-demo/r/)

  if (!nzchar(data_dir) || !dir.exists(data_dir)) {
    data_dir <- file.path("..", "data", "runs")
  }

  if (!dir.exists(data_dir)) {
    return(list())
  }

  json_files <- list.files(data_dir, pattern = "\\.json$", full.names = TRUE)

  runs <- lapply(json_files, function(f) {
    tryCatch(
      {
        data <- jsonlite::fromJSON(f, simplifyVector = FALSE)
        list(
          id = data$run_id,
          label = data$run_id,
          question = data$question %||% "",
          model = data$model %||% "",
          iterations = data$iterations_used %||% length(data$iterations)
        )
      },
      error = function(e) NULL
    )
  })

  Filter(Negate(is.null), runs)
}

#' Load a specific trace by run_id
#' @param run_id Character string identifying the run
#' @return Parsed trace data as list, or NULL
load_trace <- function(run_id) {
  data_dir <- file.path(
    system.file("shiny-demo", package = "dsprrr"),
    "data", "runs"
  )

  if (!nzchar(data_dir) || !dir.exists(data_dir)) {
    data_dir <- file.path("..", "data", "runs")
  }


  filename <- paste0(run_id, ".json")
  filepath <- file.path(data_dir, filename)

  if (!file.exists(filepath)) {
    return(NULL)
  }

  tryCatch(
    jsonlite::fromJSON(filepath, simplifyVector = FALSE),
    error = function(e) {
      message("Failed to load trace: ", e$message)
      NULL
    }
  )
}
