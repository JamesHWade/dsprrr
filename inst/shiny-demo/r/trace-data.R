# trace-data.R - Load and serve pre-recorded RLM traces

#' List available pre-recorded runs
#' @return Named list of run metadata
list_available_runs <- function() {
  pkg_dir <- system.file("shiny-demo", package = "dsprrr")
  data_dir <- if (nzchar(pkg_dir)) {
    file.path(pkg_dir, "data", "runs")
  } else {
    # Fallback for development (running from inst/shiny-demo/r/)
    file.path("..", "data", "runs")
  }

  if (!dir.exists(data_dir)) {
    return(list())
  }

  json_files <- list.files(data_dir, pattern = "\\.json$", full.names = TRUE)

  runs <- lapply(json_files, function(f) {
    tryCatch(
      {
        data <- jsonlite::fromJSON(f, simplifyVector = FALSE)
        n_iter <- data$iterations_used %||% length(data$iterations)
        llm_calls <- data$llm_calls_used %||% 1L

        description <- if (llm_calls > 1L) {
          sprintf("%d iterations, %d LLM calls", n_iter, llm_calls)
        } else {
          sprintf("%d iterations", n_iter)
        }

        list(
          id = data$run_id,
          label = data$run_id,
          description = description,
          question = data$question %||% "",
          model = data$model %||% "",
          iterations = n_iter,
          total_tokens = data$total_tokens %||% NULL
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
  # Validate run_id to prevent path traversal
  if (!grepl("^[A-Za-z0-9_-]+$", run_id)) {
    message("[load_trace] Invalid run_id: ", run_id)
    return(NULL)
  }

  pkg_dir <- system.file("shiny-demo", package = "dsprrr")
  data_dir <- if (nzchar(pkg_dir)) {
    file.path(pkg_dir, "data", "runs")
  } else {
    file.path("..", "data", "runs")
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
