#' Orchestration Helpers for Production Workflows
#'
#' @description
#' Functions for persisting module configurations, traces, and evaluation results
#' using the pins package. These helpers enable reproducible LLM workflows by
#' saving and loading module state across sessions.
#'
#' @name orchestration
#' @family orchestration
NULL

# ---- Module Configuration Persistence ----

#' Pin a Module Configuration
#'
#' @description
#' Save a complete module program artifact to a pins board for later retrieval.
#' This uses the versioned manifest documented in [program-artifact], including
#' nested programs and shared module identity.
#'
#' @param board A pins board object (e.g., from `pins::board_folder()`)
#' @param name Character name for the pin
#' @param module A DSPrrr module whose configuration should be saved
#' @param description Optional description for the pin
#' @param versioned Logical; whether to version the pin (default TRUE)
#' @param registry Named runtime registry; see [program-artifact].
#' @param trusted Whether trusted runtime values may be embedded. The default is
#'   `FALSE`.
#' @param ... Additional arguments passed to `pins::pin_write()`
#'
#' @return The pin name (invisibly)
#'
#' @details
#' The pinned configuration includes:
#' - Signature specification (inputs, output type, instructions)
#' - Module configuration (temperature, prompt_style, etc.)
#' - Optimization state (best parameters, trials summary)
#' - Metadata (module type, creation timestamp, package version)
#'
#' @export
#' @family orchestration
#'
#' @examples
#' \dontrun{
#' # Create a board and pin a module configuration
#' board <- pins::board_folder("pins")
#'
#' mod <- signature("text -> sentiment") |>
#'   module(type = "predict") |>
#'   optimize_grid(devset, metric = exact_match)
#'
#' pin_module_config(board, "sentiment-classifier-v1", mod,
#'                   description = "Optimized sentiment classifier")
#'
#' # Later, retrieve and reconstruct the module
#' config <- pins::pin_read(board, "sentiment-classifier-v1")
#' restored_mod <- restore_module_config(config)
#' }
pin_module_config <- function(
  board,
  name,
  module,
  description = NULL,
  versioned = TRUE,
  ...,
  registry = list(),
  trusted = FALSE
) {
  rlang::check_installed("pins", reason = "to save module configurations")

  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  config_data <- program_artifact(
    module,
    registry = registry,
    trusted = trusted
  )

  # Write to pins
  pins::pin_write(
    board = board,
    x = config_data,
    name = name,
    description = description %||% paste("dsprrr program artifact:", name),
    type = "rds",
    versioned = versioned,
    ...
  )

  root <- config_data$graph$nodes[[config_data$root]]
  cli::cli_inform(c(
    "v" = "Pinned program artifact: {.val {name}}",
    "i" = "Root module: {.cls {root$class}}",
    "i" = "Graph nodes: {.val {length(config_data$graph$nodes)}}",
    "i" = "Compiled: {.val {isTRUE(root$state$compiled)}}"
  ))

  invisible(name)
}


#' Restore a Module from Pinned Configuration
#'
#' @description
#' Reconstruct a module from a previously pinned configuration. This allows
#' you to load optimized modules in new sessions or different projects.
#'
#' @param config A configuration list (from `pins::pin_read()`)
#' @param registry Named runtime registry used to resolve stored IDs.
#' @param trusted Whether embedded runtime values may be restored. The default
#'   is `FALSE`.
#'
#' @return A DSPrrr module with the restored configuration
#'
#' @export
#' @family orchestration
#'
#' @examples
#' \dontrun{
#' # Read pinned config and restore module
#' board <- pins::board_folder("pins")
#' config <- pins::pin_read(board, "sentiment-classifier-v1")
#' mod <- restore_module_config(config)
#'
#' # Use the restored module
#' result <- run(mod, text = "This is great!", .llm = llm)
#' }
restore_module_config <- function(
  config,
  registry = list(),
  trusted = FALSE
) {
  mod <- restore_program_artifact(
    config,
    registry = registry,
    trusted = trusted
  )

  cli::cli_inform(c(
    "v" = "Restored program artifact",
    "i" = "Root module: {.cls {class(mod)[1]}}",
    "i" = "Artifact version: {.val {artifact_format_version()}}"
  ))

  mod
}

# ---- Trace Persistence ----

#' Pin Module Traces
#'
#' @description
#' Save module execution traces to a pins board. Traces include timing,
#' token usage, and optionally the full prompts and outputs.
#'
#' @param board A pins board object
#' @param name Character name for the pin
#' @param module A DSPrrr module with recorded traces
#' @param include_prompts Logical; include full prompts (default FALSE)
#' @param include_outputs Logical; include full outputs (default FALSE)
#' @param description Optional description for the pin
#' @param ... Additional arguments passed to `pins::pin_write()`
#'
#' @return The pin name (invisibly)
#'
#' @export
#' @family orchestration
#'
#' @examples
#' \dontrun{
#' board <- pins::board_folder("pins")
#'
#' # Run some predictions to generate traces
#' results <- run(mod, text = test_texts, .llm = llm)
#'
#' # Save traces for later analysis
#' pin_trace(board, "experiment-2024-01-traces", mod,
#'           include_prompts = TRUE,
#'           description = "Production run traces")
#' }
pin_trace <- function(
  board,
  name,
  module,
  include_prompts = FALSE,
  include_outputs = FALSE,
  description = NULL,
  ...
) {
  rlang::check_installed("pins", reason = "to save module traces")

  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  traces_df <- export_traces(
    module,
    include_prompts = include_prompts,
    include_outputs = include_outputs
  )

  if (nrow(traces_df) == 0) {
    cli::cli_warn("No traces to pin for module {.val {name}}")
    return(invisible(name))
  }

  # Add metadata
  trace_data <- list(
    traces = traces_df,
    summary = summarize_traces(module),
    metadata = list(
      module_type = class(module)[1],
      n_traces = nrow(traces_df),
      created_at = Sys.time(),
      include_prompts = include_prompts,
      include_outputs = include_outputs
    )
  )

  pins::pin_write(
    board = board,
    x = trace_data,
    name = name,
    description = description %||% paste("dsprrr traces:", name),
    type = "rds",
    ...
  )

  cli::cli_inform(c(
    "v" = "Pinned {nrow(traces_df)} trace{?s}: {.val {name}}",
    "i" = "Total tokens: {trace_data$summary$total_tokens}"
  ))

  invisible(name)
}


# ---- Vitals Log Persistence ----

#' Pin Vitals Evaluation Log
#'
#' @description
#' Save evaluation results from a vitals Task run to a pins board.
#' This enables tracking model performance over time and across experiments.
#'
#' @param board A pins board object
#' @param name Character name for the pin
#' @param eval_result Evaluation result from `evaluate()` or a vitals Task
#' @param module Optional module that was evaluated (for additional metadata)
#' @param description Optional description for the pin
#' @param ... Additional arguments passed to `pins::pin_write()`
#'
#' @return The pin name (invisibly)
#'
#' @export
#' @family orchestration
#'
#' @examples
#' \dontrun{
#' board <- pins::board_folder("pins")
#'
#' # Evaluate module on test set
#' eval_result <- evaluate(mod, test_data, metric = exact_match)
#'
#' # Pin the evaluation results
#' pin_vitals_log(board, "sentiment-eval-v1", eval_result,
#'                module = mod,
#'                description = "Test set evaluation")
#' }
pin_vitals_log <- function(
  board,
  name,
  eval_result,
  module = NULL,
  description = NULL,
  ...
) {
  rlang::check_installed("pins", reason = "to save evaluation logs")

  # Handle different result types
  log_data <- if (inherits(eval_result, "dsprrr_evaluation")) {
    list(
      type = "dsprrr_evaluation",
      mean_score = eval_result$mean_score,
      scores = eval_result$scores,
      n_evaluated = eval_result$n_evaluated,
      n_errors = eval_result$n_errors,
      predictions = eval_result$predictions,
      metadata = eval_result$metadata
    )
  } else if (is.list(eval_result)) {
    list(
      type = "generic",
      data = eval_result
    )
  } else {
    cli::cli_abort(
      "Unsupported evaluation result type: {.cls {class(eval_result)}}"
    )
  }

  # Add module metadata if provided
  if (!is.null(module) && inherits(module, "Module")) {
    log_data$module_info <- list(
      module_type = class(module)[1],
      compiled = module$is_compiled(),
      signature_inputs = vapply(
        module$signature@inputs,
        function(x) x$name,
        character(1)
      )
    )
  }

  # Add timestamp
  log_data$created_at <- Sys.time()
  log_data$dsprrr_version <- as.character(utils::packageVersion("dsprrr"))

  pins::pin_write(
    board = board,
    x = log_data,
    name = name,
    description = description %||% paste("dsprrr evaluation:", name),
    type = "rds",
    ...
  )

  cli::cli_inform(c(
    "v" = "Pinned evaluation log: {.val {name}}",
    "i" = "Mean score: {.val {round(log_data$mean_score %||% NA, 3)}}"
  ))

  invisible(name)
}


# ---- Workflow Templates ----

#' Use dsprrr Workflow Templates
#'
#' @description
#' Copy workflow templates (targets pipeline, Quarto report) to your project.
#' These templates provide starting points for production LLM workflows.
#'
#' @param template Which template to use: "targets", "quarto", or "all"
#' @param path Destination directory (default: current directory)
#' @param overwrite Logical; overwrite existing files (default FALSE)
#'
#' @return Character vector of created file paths (invisibly)
#'
#' @export
#' @family orchestration
#'
#' @examples
#' \dontrun{
#' # Copy the targets pipeline template
#' use_dsprrr_template("targets")
#'
#' # Copy all templates
#' use_dsprrr_template("all", path = "workflows/")
#' }
use_dsprrr_template <- function(
  template = c("targets", "quarto", "all"),
  path = ".",
  overwrite = FALSE
) {
  template <- match.arg(template)

  template_dir <- system.file("templates", package = "dsprrr")
  if (template_dir == "") {
    cli::cli_abort(
      "Template directory not found. Is dsprrr installed correctly?
"
    )
  }

  created <- character()

  # Helper to copy a template
  copy_template <- function(from, to) {
    if (file.exists(to) && !overwrite) {
      cli::cli_warn(
        "File already exists: {.file {to}}. Use {.code overwrite = TRUE} to replace."
      )
      return(NULL)
    }

    dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
    file.copy(from, to, overwrite = overwrite)
    cli::cli_inform("Created: {.file {to}}")
    to
  }

  if (template %in% c("targets", "all")) {
    targets_src <- file.path(template_dir, "targets", "_targets.R")
    if (file.exists(targets_src)) {
      result <- copy_template(targets_src, file.path(path, "_targets.R"))
      if (!is.null(result)) created <- c(created, result)
    }
  }

  if (template %in% c("quarto", "all")) {
    quarto_src <- file.path(template_dir, "quarto", "report.qmd")
    if (file.exists(quarto_src)) {
      result <- copy_template(quarto_src, file.path(path, "report.qmd"))
      if (!is.null(result)) created <- c(created, result)
    }
  }

  if (length(created) == 0) {
    cli::cli_inform("No templates were created.")
  }

  invisible(created)
}


# ---- Workflow Validation ----

#' Validate Workflow Configuration
#'
#' @description
#' Check that a workflow has all required components configured correctly.
#' Useful for validating pipelines before running expensive LLM operations.
#'
#' @param module A DSPrrr module to validate
#' @param data Optional data to validate against the module's signature
#' @param board Optional pins board to check for accessibility
#'
#' @return A list with validation results (invisibly). Prints a summary.
#'
#' @export
#' @family orchestration
#'
#' @examples
#' \dontrun{
#' mod <- signature("text -> sentiment") |> module(type = "predict")
#' validate_workflow(mod, data = test_data)
#' }
validate_workflow <- function(module, data = NULL, board = NULL) {
  results <- list(
    valid = TRUE,
    checks = list()
  )

  # Check module
  if (!inherits(module, "Module")) {
    results$valid <- FALSE
    results$checks$module <- list(
      passed = FALSE,
      message = "module is not a DSPrrr Module object"
    )
  } else {
    results$checks$module <- list(
      passed = TRUE,
      message = paste("Module type:", class(module)[1])
    )

    # Check signature
    n_inputs <- length(module$signature@inputs)
    results$checks$signature <- list(
      passed = n_inputs > 0,
      message = paste(n_inputs, "input(s) defined")
    )
  }

  # Check data compatibility
  if (!is.null(data) && inherits(module, "Module")) {
    required_cols <- vapply(
      module$signature@inputs,
      function(x) x$name,
      character(1)
    )
    present_cols <- intersect(required_cols, names(data))
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
      results$valid <- FALSE
      results$checks$data <- list(
        passed = FALSE,
        message = paste(
          "Missing columns:",
          paste(missing_cols, collapse = ", ")
        )
      )
    } else {
      results$checks$data <- list(
        passed = TRUE,
        message = paste(
          nrow(data),
          "rows,",
          length(present_cols),
          "required columns present"
        )
      )
    }
  }

  # Check board
  if (!is.null(board)) {
    if (inherits(board, "pins_board")) {
      results$checks$board <- list(
        passed = TRUE,
        message = paste("Board type:", class(board)[1])
      )
    } else {
      results$valid <- FALSE
      results$checks$board <- list(
        passed = FALSE,
        message = "board is not a valid pins board"
      )
    }
  }

  # Print summary
  cli::cli_h2("Workflow Validation")

  for (check_name in names(results$checks)) {
    check <- results$checks[[check_name]]
    if (check$passed) {
      cli::cli_alert_success("{check_name}: {check$message}")
    } else {
      cli::cli_alert_danger("{check_name}: {check$message}")
    }
  }

  if (results$valid) {
    cli::cli_alert_success("Workflow validation passed")
  } else {
    cli::cli_alert_danger("Workflow validation failed")
  }

  invisible(results)
}
