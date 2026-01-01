# Optimizer Logging Infrastructure
#
# Trial logging and persistence for optimizers:
# - Trial records
# - Trial log collections
# - JSONL persistence
# - Log directory management

#' Trial Record
#'
#' @description
#' S7 class representing a single optimization trial. Captures all metadata
#' needed to reproduce and analyze the trial.
#'
#' @param trial_id Unique identifier for this trial.
#' @param optimizer_name Name of the optimizer that produced this trial.
#' @param params List of parameters used in this trial.
#' @param metric_summary List with mean_score, std_error, n_evaluated, n_errors.
#' @param cost_summary List with tokens_in, tokens_out, total_tokens, total_cost.
#' @param start_time POSIXct timestamp when trial started.
#' @param end_time POSIXct timestamp when trial ended.
#' @param notes Optional character string with additional notes.
#' @param compiled_artifact_ref Optional reference to compiled module (file path or object).
#' @param status Trial status: "pending", "running", "completed", "failed".
#'
#' @export
Trial <- S7::new_class(
  "Trial",
  properties = list(
    trial_id = S7::new_property(S7::class_character, default = ""),
    optimizer_name = S7::new_property(S7::class_character, default = ""),
    params = S7::new_property(S7::class_list, default = list()),
    metric_summary = S7::new_property(S7::class_list, default = list()),
    cost_summary = S7::new_property(S7::class_list, default = list()),
    start_time = S7::new_property(S7::class_any, default = NULL),
    end_time = S7::new_property(S7::class_any, default = NULL),
    notes = S7::new_property(S7::class_character, default = ""),
    compiled_artifact_ref = S7::new_property(S7::class_any, default = NULL),
    status = S7::new_property(
      S7::class_character,
      default = "pending",
      validator = function(value) {
        valid <- c("pending", "running", "completed", "failed")
        if (!value %in% valid) {
          return(sprintf(
            "status must be one of: %s",
            paste(valid, collapse = ", ")
          ))
        }
        NULL
      }
    )
  )
)

#' Create a Trial Record
#'
#' @description
#' Convenience function to create a Trial record with auto-generated ID.
#'
#' @param optimizer_name Name of the optimizer.
#' @param params List of parameters for this trial.
#' @param trial_id Optional trial ID. If NULL, auto-generated.
#' @param notes Optional notes.
#'
#' @return A Trial object.
#' @export
#'
#' @examples
#' trial <- create_trial(
#'   optimizer_name = "BootstrapFewShot",
#'   params = list(max_demos = 4, temperature = 0.7)
#' )
create_trial <- function(
  optimizer_name,
  params = list(),
  trial_id = NULL,
  notes = ""
) {
  if (is.null(trial_id)) {
    trial_id <- generate_trial_id()
  }

  Trial(
    trial_id = trial_id,
    optimizer_name = optimizer_name,
    params = params,
    start_time = Sys.time(),
    notes = notes,
    status = "pending"
  )
}

#' Start a Trial
#'
#' @description
#' Mark a trial as running and record the start time.
#'
#' @param trial A Trial object.
#'
#' @return Updated Trial object with status "running".
#' @noRd
start_trial <- function(trial) {
  Trial(
    trial_id = trial@trial_id,
    optimizer_name = trial@optimizer_name,
    params = trial@params,
    metric_summary = trial@metric_summary,
    cost_summary = trial@cost_summary,
    start_time = Sys.time(),
    end_time = trial@end_time,
    notes = trial@notes,
    compiled_artifact_ref = trial@compiled_artifact_ref,
    status = "running"
  )
}

#' Complete a Trial
#'
#' @description
#' Mark a trial as completed with evaluation results.
#'
#' @param trial A Trial object.
#' @param eval_result An EvalResult object from eval_program().
#' @param compiled_artifact_ref Optional reference to the compiled module.
#' @param notes Optional additional notes.
#'
#' @return Updated Trial object with status "completed".
#' @export
complete_trial <- function(
  trial,
  eval_result,
  compiled_artifact_ref = NULL,
  notes = NULL
) {
  metric_summary <- list(
    mean_score = eval_result@mean_score,
    std_error = eval_result@std_error,
    n_evaluated = eval_result@n_evaluated,
    n_errors = eval_result@n_errors
  )

  cost_summary <- list(
    total_tokens = eval_result@total_tokens,
    total_cost = eval_result@total_cost,
    latency_ms = eval_result@total_latency_ms
  )

  Trial(
    trial_id = trial@trial_id,
    optimizer_name = trial@optimizer_name,
    params = trial@params,
    metric_summary = metric_summary,
    cost_summary = cost_summary,
    start_time = trial@start_time,
    end_time = Sys.time(),
    notes = notes %||% trial@notes,
    compiled_artifact_ref = compiled_artifact_ref,
    status = "completed"
  )
}

#' Fail a Trial
#'
#' @description
#' Mark a trial as failed with an error message.
#'
#' @param trial A Trial object.
#' @param error_message Error message explaining the failure.
#'
#' @return Updated Trial object with status "failed".
#' @noRd
fail_trial <- function(trial, error_message) {
  Trial(
    trial_id = trial@trial_id,
    optimizer_name = trial@optimizer_name,
    params = trial@params,
    metric_summary = trial@metric_summary,
    cost_summary = trial@cost_summary,
    start_time = trial@start_time,
    end_time = Sys.time(),
    notes = paste0(trial@notes, "\nError: ", error_message),
    compiled_artifact_ref = trial@compiled_artifact_ref,
    status = "failed"
  )
}

#' Trial Log
#'
#' @description
#' R6 class for managing a collection of trials with optional persistence.
#' Provides methods for adding trials, querying results, and saving to disk.
#'
#' @export
TrialLog <- R6::R6Class(
  "TrialLog",
  public = list(
    #' @field optimizer_name Name of the optimizer using this log.
    optimizer_name = NULL,

    #' @field log_dir Directory for persistence (NULL for in-memory only).
    log_dir = NULL,

    #' @field trials List of Trial objects.
    trials = NULL,

    #' @field metadata Additional metadata about the optimization run.
    metadata = NULL,

    #' @description
    #' Create a new TrialLog.
    #'
    #' @param optimizer_name Name of the optimizer.
    #' @param log_dir Optional directory for persistence.
    #' @param metadata Optional metadata list.
    initialize = function(optimizer_name, log_dir = NULL, metadata = NULL) {
      self$optimizer_name <- optimizer_name
      self$log_dir <- log_dir
      self$trials <- list()
      self$metadata <- metadata %||%
        list(
          created_at = Sys.time(),
          r_version = R.version.string
        )

      # Create log directory if specified
      if (!is.null(log_dir)) {
        if (!dir.exists(log_dir)) {
          dir.create(log_dir, recursive = TRUE)
        }
      }

      invisible(self)
    },

    #' @description
    #' Add a trial to the log.
    #'
    #' @param trial A Trial object.
    #' @param persist Whether to immediately persist to disk if log_dir is set.
    add_trial = function(trial, persist = TRUE) {
      if (!inherits(trial, "dsprrr::Trial")) {
        cli::cli_abort("{.arg trial} must be a Trial object")
      }

      self$trials <- append(self$trials, list(trial))

      if (persist && !is.null(self$log_dir)) {
        self$save()
      }

      invisible(self)
    },

    #' @description
    #' Get the number of trials.
    #'
    #' @return Integer count of trials.
    n_trials = function() {
      length(self$trials)
    },

    #' @description
    #' Get trials as a tibble.
    #'
    #' @return A tibble with one row per trial.
    as_tibble = function() {
      if (length(self$trials) == 0) {
        return(tibble::tibble(
          trial_id = character(),
          optimizer_name = character(),
          status = character(),
          mean_score = numeric(),
          n_evaluated = integer(),
          n_errors = integer(),
          total_tokens = integer(),
          total_cost = numeric(),
          latency_ms = numeric(),
          start_time = .POSIXct(numeric()),
          end_time = .POSIXct(numeric()),
          params = list(),
          notes = character()
        ))
      }

      tibble::tibble(
        trial_id = vapply(
          self$trials,
          function(t) t@trial_id,
          character(1)
        ),
        optimizer_name = vapply(
          self$trials,
          function(t) t@optimizer_name,
          character(1)
        ),
        status = vapply(
          self$trials,
          function(t) t@status,
          character(1)
        ),
        mean_score = vapply(
          self$trials,
          function(t) t@metric_summary$mean_score %||% NA_real_,
          numeric(1)
        ),
        n_evaluated = vapply(
          self$trials,
          function(t) as.integer(t@metric_summary$n_evaluated %||% 0L),
          integer(1)
        ),
        n_errors = vapply(
          self$trials,
          function(t) as.integer(t@metric_summary$n_errors %||% 0L),
          integer(1)
        ),
        total_tokens = vapply(
          self$trials,
          function(t) as.integer(t@cost_summary$total_tokens %||% 0L),
          integer(1)
        ),
        total_cost = vapply(
          self$trials,
          function(t) t@cost_summary$total_cost %||% NA_real_,
          numeric(1)
        ),
        latency_ms = vapply(
          self$trials,
          function(t) t@cost_summary$latency_ms %||% NA_real_,
          numeric(1)
        ),
        start_time = do.call(
          c,
          lapply(
            self$trials,
            function(t) t@start_time %||% as.POSIXct(NA)
          )
        ),
        end_time = do.call(
          c,
          lapply(
            self$trials,
            function(t) t@end_time %||% as.POSIXct(NA)
          )
        ),
        params = lapply(self$trials, function(t) t@params),
        notes = vapply(
          self$trials,
          function(t) t@notes,
          character(1)
        )
      )
    },

    #' @description
    #' Get the best trial by score.
    #'
    #' @param objective "maximize" or "minimize".
    #' @return The best Trial object, or NULL if no completed trials.
    best_trial = function(objective = "maximize") {
      completed <- Filter(
        function(t) t@status == "completed",
        self$trials
      )

      if (length(completed) == 0) {
        return(NULL)
      }

      scores <- vapply(
        completed,
        function(t) t@metric_summary$mean_score %||% NA_real_,
        numeric(1)
      )

      valid <- !is.na(scores)
      if (!any(valid)) {
        return(NULL)
      }

      best_idx <- if (objective == "maximize") {
        which.max(scores[valid])
      } else {
        which.min(scores[valid])
      }

      completed[valid][[best_idx]]
    },

    #' @description
    #' Get summary statistics for all trials.
    #'
    #' @return A list with summary statistics.
    summary = function() {
      completed <- Filter(
        function(t) t@status == "completed",
        self$trials
      )

      scores <- vapply(
        completed,
        function(t) t@metric_summary$mean_score %||% NA_real_,
        numeric(1)
      )

      tokens <- vapply(
        completed,
        function(t) as.integer(t@cost_summary$total_tokens %||% 0L),
        integer(1)
      )

      costs <- vapply(
        completed,
        function(t) t@cost_summary$total_cost %||% 0,
        numeric(1)
      )

      list(
        n_trials = length(self$trials),
        n_completed = length(completed),
        n_failed = sum(vapply(
          self$trials,
          function(t) t@status == "failed",
          logical(1)
        )),
        best_score = if (length(scores) > 0 && !all(is.na(scores))) {
          max(scores, na.rm = TRUE)
        } else {
          NA_real_
        },
        mean_score = mean(scores, na.rm = TRUE),
        total_tokens = sum(tokens, na.rm = TRUE),
        total_cost = sum(costs, na.rm = TRUE)
      )
    },

    #' @description
    #' Save the trial log to disk.
    #'
    #' @param dir Optional directory override.
    #' @return Invisibly returns self. Throws error on critical failure.
    save = function(dir = NULL) {
      save_dir <- dir %||% self$log_dir

      if (is.null(save_dir)) {
        cli::cli_warn("No log_dir specified; cannot save trial log")
        return(invisible(self))
      }

      # Create directory with error handling
      if (!dir.exists(save_dir)) {
        tryCatch(
          dir.create(save_dir, recursive = TRUE),
          error = function(e) {
            cli::cli_abort(
              c(
                "Failed to create log directory",
                "x" = "Path: {.path {save_dir}}",
                "i" = "Error: {conditionMessage(e)}"
              ),
              class = "dsprrr_save_error"
            )
          }
        )
      }

      # Save trials as JSONL with error handling
      trials_path <- file.path(save_dir, "trials.jsonl")
      tryCatch(
        write_trials_jsonl(self$trials, trials_path),
        error = function(e) {
          cli::cli_abort(
            c(
              "Failed to save trials",
              "x" = "Path: {.path {trials_path}}",
              "i" = "Error: {conditionMessage(e)}"
            ),
            class = "dsprrr_save_error"
          )
        }
      )

      # Save metadata with error handling
      metadata_path <- file.path(save_dir, "metadata.json")
      meta <- self$metadata
      meta$optimizer_name <- self$optimizer_name
      meta$n_trials <- length(self$trials)
      meta$saved_at <- Sys.time()

      tryCatch(
        jsonlite::write_json(
          meta,
          metadata_path,
          auto_unbox = TRUE,
          pretty = TRUE
        ),
        error = function(e) {
          cli::cli_warn(
            c(
              "Failed to save metadata",
              "x" = "Path: {.path {metadata_path}}",
              "i" = "Error: {conditionMessage(e)}"
            ),
            class = "dsprrr_save_warning"
          )
        }
      )

      # Save best program if available (non-critical, warn on failure)
      best <- self$best_trial()
      if (!is.null(best) && !is.null(best@compiled_artifact_ref)) {
        if (inherits(best@compiled_artifact_ref, "Module")) {
          tryCatch(
            saveRDS(
              best@compiled_artifact_ref,
              file.path(save_dir, "best_program.rds")
            ),
            error = function(e) {
              cli::cli_warn(
                c(
                  "Failed to save best program",
                  "i" = "Error: {conditionMessage(e)}"
                ),
                class = "dsprrr_save_warning"
              )
            }
          )
        }
      }

      # Write README (non-critical, warn on failure)
      readme_path <- file.path(save_dir, "README.md")
      summary <- self$summary()
      readme_content <- sprintf(
        "# Optimizer Log: %s\n\n- Trials: %d\n- Best Score: %.4f\n- Total Cost: $%.4f\n- Created: %s\n",
        self$optimizer_name,
        summary$n_trials,
        summary$best_score %||% NA,
        summary$total_cost,
        format(self$metadata$created_at, "%Y-%m-%d %H:%M:%S")
      )
      tryCatch(
        writeLines(readme_content, readme_path),
        error = function(e) {
          cli::cli_warn(
            c("Failed to save README", "i" = "Error: {conditionMessage(e)}"),
            class = "dsprrr_save_warning"
          )
        }
      )

      if (isTRUE(getOption("dsprrr.verbose"))) {
        cli::cli_alert_success("Saved trial log to {.path {save_dir}}")
      }

      invisible(self)
    },

    #' @description
    #' Print the trial log summary.
    print = function() {
      cli::cli_h3("Trial Log: {self$optimizer_name}")

      summary <- self$summary()
      cli::cli_text(
        "{.field Trials}: {summary$n_trials} ({summary$n_completed} completed, {summary$n_failed} failed)"
      )

      if (!is.na(summary$best_score)) {
        cli::cli_text("{.field Best Score}: {round(summary$best_score, 4)}")
      }

      if (summary$total_tokens > 0) {
        cli::cli_text("{.field Total Tokens}: {summary$total_tokens}")
      }

      if (summary$total_cost > 0) {
        cli::cli_text(
          "{.field Total Cost}: ${format(summary$total_cost, digits = 4)}"
        )
      }

      if (!is.null(self$log_dir)) {
        cli::cli_text("{.field Log Dir}: {.path {self$log_dir}}")
      }

      invisible(self)
    }
  )
)

#' Write Trials to JSONL File
#'
#' @description
#' Write a list of Trial objects to a JSONL (JSON Lines) file.
#' Each trial is written as a single JSON object on its own line.
#'
#' @param trials List of Trial objects.
#' @param path File path for the JSONL file.
#' @param append Whether to append to existing file. Default is FALSE.
#'
#' @return Invisibly returns the path.
#' @export
#'
#' @examples
#' \dontrun{
#' trials <- list(
#'   create_trial("BootstrapFewShot", list(k = 4)),
#'   create_trial("BootstrapFewShot", list(k = 8))
#' )
#' write_trials_jsonl(trials, "trials.jsonl")
#' }
write_trials_jsonl <- function(trials, path, append = FALSE) {
  if (length(trials) == 0) {
    if (!append) {
      writeLines(character(0), path)
    }
    return(invisible(path))
  }

  lines <- vapply(
    trials,
    function(trial) {
      trial_list <- list(
        trial_id = trial@trial_id,
        optimizer_name = trial@optimizer_name,
        params = trial@params,
        metric_summary = trial@metric_summary,
        cost_summary = trial@cost_summary,
        start_time = format(trial@start_time, "%Y-%m-%dT%H:%M:%S"),
        end_time = if (!is.null(trial@end_time)) {
          format(trial@end_time, "%Y-%m-%dT%H:%M:%S")
        } else {
          NULL
        },
        notes = trial@notes,
        status = trial@status
      )
      jsonlite::toJSON(trial_list, auto_unbox = TRUE)
    },
    character(1)
  )

  if (append && file.exists(path)) {
    cat(lines, file = path, sep = "\n", append = TRUE)
  } else {
    writeLines(lines, path)
  }

  invisible(path)
}

#' Read Trials from JSONL File
#'
#' @description
#' Read Trial objects from a JSONL file.
#'
#' @param path File path to the JSONL file.
#'
#' @return A list of Trial objects.
#' @export
#'
#' @examples
#' \dontrun{
#' trials <- read_trials_jsonl("trials.jsonl")
#' }
read_trials_jsonl <- function(path) {
  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  lines <- readLines(path, warn = FALSE)
  lines <- lines[nzchar(trimws(lines))] # Remove empty lines

  if (length(lines) == 0) {
    return(list())
  }

  # Helper to check if a value is valid for timestamp parsing
  is_valid_timestamp <- function(x) {
    !is.null(x) && length(x) > 0 && !is.na(x) && nzchar(x)
  }

  # Parse each line with error handling
  parsed_trials <- lapply(seq_along(lines), function(i) {
    line <- lines[[i]]

    tryCatch(
      {
        data <- jsonlite::fromJSON(line)

        # Parse timestamps
        start_time <- if (is_valid_timestamp(data$start_time)) {
          as.POSIXct(data$start_time, format = "%Y-%m-%dT%H:%M:%S")
        } else {
          NULL
        }

        end_time <- if (is_valid_timestamp(data$end_time)) {
          as.POSIXct(data$end_time, format = "%Y-%m-%dT%H:%M:%S")
        } else {
          NULL
        }

        Trial(
          trial_id = data$trial_id %||% "",
          optimizer_name = data$optimizer_name %||% "",
          params = as.list(data$params %||% list()),
          metric_summary = as.list(data$metric_summary %||% list()),
          cost_summary = as.list(data$cost_summary %||% list()),
          start_time = start_time,
          end_time = end_time,
          notes = data$notes %||% "",
          status = data$status %||% "pending"
        )
      },
      error = function(e) {
        cli::cli_warn(
          c(
            "Failed to parse trial on line {i}",
            "i" = "Error: {conditionMessage(e)}",
            "i" = "Line content: {substr(line, 1, 100)}..."
          ),
          class = "dsprrr_parse_warning"
        )
        NULL
      }
    )
  })

  # Filter out failed parses (NULL values)
  Filter(Negate(is.null), parsed_trials)
}

#' Load Trial Log from Directory
#'
#' @description
#' Load a TrialLog from a directory that was previously saved.
#'
#' @param log_dir Path to the log directory.
#'
#' @return A TrialLog object.
#' @export
#'
#' @examples
#' \dontrun{
#' log <- load_trial_log("logs/my_optimizer/")
#' log$as_tibble()
#' }
load_trial_log <- function(log_dir) {
  if (!dir.exists(log_dir)) {
    cli::cli_abort("Directory not found: {.path {log_dir}}")
  }

  # Load metadata
  metadata_path <- file.path(log_dir, "metadata.json")
  metadata <- if (file.exists(metadata_path)) {
    jsonlite::fromJSON(metadata_path)
  } else {
    list()
  }

  optimizer_name <- metadata$optimizer_name %||% "unknown"

  # Load trials
  trials_path <- file.path(log_dir, "trials.jsonl")
  trials <- if (file.exists(trials_path)) {
    read_trials_jsonl(trials_path)
  } else {
    list()
  }

  # Create log object
  log <- TrialLog$new(
    optimizer_name = optimizer_name,
    log_dir = log_dir,
    metadata = metadata
  )

  # Add trials without persisting (already on disk)
  for (trial in trials) {
    log$add_trial(trial, persist = FALSE)
  }

  log
}

#' Print method for Trial
#' @param x A Trial object
#' @param ... Additional arguments (unused)
#' @export
print.Trial <- function(x, ...) {
  cli::cli_h3("Trial: {x@trial_id}")

  status_icon <- switch(
    x@status,
    completed = cli::symbol$tick,
    failed = cli::symbol$cross,
    running = cli::symbol$pointer,
    cli::symbol$bullet
  )

  cli::cli_text("{status_icon} {.field Status}: {x@status}")
  cli::cli_text("{.field Optimizer}: {x@optimizer_name}")

  if (length(x@params) > 0) {
    cli::cli_text("{.field Params}: {paste(names(x@params), collapse = ', ')}")
  }

  if (!is.null(x@metric_summary$mean_score)) {
    cli::cli_text("{.field Score}: {round(x@metric_summary$mean_score, 4)}")
  }

  if (!is.null(x@start_time)) {
    cli::cli_text(
      "{.field Started}: {format(x@start_time, '%Y-%m-%d %H:%M:%S')}"
    )
  }

  invisible(x)
}

# Register S7 print method
S7::method(print, Trial) <- print.Trial
