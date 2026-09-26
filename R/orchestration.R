#' Orchestration helpers
#'
#' Pins, workflow templates and workflow checks. Each function is documented
#' on its own page.
#'
#' @name orchestration
#' @noRd
NULL

# ---- Module Configuration Persistence ----

#' Pin a program to a pins board
#'
#' @description
#' `pin_module_config()` saves a complete program, including nested modules,
#' demonstrations and optimization results, to a pins board as an `.rds` pin.
#' Read it back with `pins::pin_read()` and rebuild the program with
#' [restore_module_config()].
#'
#' @details
#' The pin holds the program artifact described in [program_artifact()]:
#' signatures, configuration, demonstrations, optimization results and the
#' structure of composed programs. Chats, credentials, caches and traces are
#' not saved. Functions such as tools or retrievers are saved only as names in
#' `registry`, or embedded with `trusted = TRUE`.
#'
#' @param board A pins board, such as `pins::board_folder("pins")`.
#' @param name Name of the pin.
#' @param module The program to save.
#' @param description Optional pin description. The default is
#'   `"dsprrr program artifact: <name>"`.
#' @param versioned Whether pins keeps earlier versions (default `TRUE`).
#' @param ... Further arguments passed to `pins::pin_write()`.
#' @param registry Named runtime registry; see [program_artifact()].
#' @param trusted Whether runtime values may be embedded (default `FALSE`);
#'   see [program_artifact()].
#'
#' @return `name`, invisibly.
#'
#' @export
#' @family persistence
#'
#' @examplesIf rlang::is_installed("pins")
#' classifier <- module(signature("text -> sentiment"))
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "It's okay"),
#'   sentiment = c("positive", "negative", "neutral")
#' )
#' compiled <- compile(classifier, LabeledFewShot(k = 2L), trainset)
#'
#' board <- pins::board_temp()
#' pin_module_config(board, "sentiment-classifier", compiled)
#'
#' # Later, or in another session
#' artifact <- pins::pin_read(board, "sentiment-classifier")
#' restored <- restore_module_config(artifact)
#' length(restored$demos)
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


#' Rebuild a program from a saved artifact
#'
#' @description
#' `restore_module_config()` rebuilds a program from a program artifact, such
#' as one read from a pin written by [pin_module_config()] or created by
#' [program_artifact()]. The restored program has the saved signatures,
#' configuration and demonstrations but no chat; pass one at run time.
#'
#' @param config A program artifact, for example from `pins::pin_read()`.
#' @param registry Named runtime registry used to resolve saved function names;
#'   see [program_artifact()].
#' @param trusted Whether embedded runtime values may be restored (default
#'   `FALSE`); see [program_artifact()].
#'
#' @return The restored program.
#'
#' @export
#' @family persistence
#'
#' @examplesIf rlang::is_installed("pins")
#' classifier <- module(signature("text -> sentiment"))
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "It's okay"),
#'   sentiment = c("positive", "negative", "neutral")
#' )
#' compiled <- compile(classifier, LabeledFewShot(k = 2L), trainset)
#'
#' board <- pins::board_temp()
#' pin_module_config(board, "sentiment-classifier", compiled)
#'
#' artifact <- pins::pin_read(board, "sentiment-classifier")
#' restored <- restore_module_config(artifact)
#' restored$is_compiled()
#'
#' \dontrun{
#' run(
#'   restored,
#'   text = "This is great!",
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
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

#' Pin a module's traces to a pins board
#'
#' @description
#' `pin_trace()` saves a module's execution traces (timing, token use, cost
#' and, optionally, prompts and outputs) to a pins board, together with a
#' summary from [summarize_traces()]. Use it to keep a record of a run for
#' later analysis.
#'
#' @details
#' The pin is a list with `traces` (from [export_traces()]), `summary` and
#' `metadata` (module class, number of traces, creation time and the include
#' flags). A module without traces is not pinned; a warning is raised instead.
#'
#' @param board A pins board.
#' @param name Name of the pin.
#' @param module A module that has been run.
#' @param include_prompts Whether to include the full prompts (default
#'   `FALSE`).
#' @param include_outputs Whether to include the full outputs (default
#'   `FALSE`).
#' @param description Optional pin description.
#' @param ... Further arguments passed to `pins::pin_write()`.
#'
#' @return `name`, invisibly.
#'
#' @export
#' @family persistence
#'
#' @examples
#' \dontrun{
#' board <- pins::board_folder("pins")
#' classifier <- module(signature("text -> sentiment"))
#' run(
#'   classifier,
#'   text = c("Great!", "Terrible."),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#'
#' pin_trace(
#'   board,
#'   "sentiment-traces",
#'   classifier,
#'   include_prompts = TRUE,
#'   description = "Production run traces"
#' )
#' pins::pin_read(board, "sentiment-traces")$summary
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

#' Pin evaluation results to a pins board
#'
#' @description
#' `pin_vitals_log()` saves evaluation results to a pins board so you can
#' track a module's performance across runs and experiments.
#'
#' @details
#' For an [evaluate()] result, the pin keeps the mean score, per-example
#' scores, counts, predictions and metadata. Any other list or data frame,
#' such as the samples from a vitals `Task`'s `$get_samples()`, is stored as it
#' is under `data`. A vitals `Task` object itself is not a list and is
#' rejected. The pin also records the dsprrr version, the time and, when
#' `module` is given, the module's class, compiled status and inputs.
#'
#' @param board A pins board.
#' @param name Name of the pin.
#' @param eval_result An [evaluate()] result, or a list or data frame of
#'   results.
#' @param module Optional module that was evaluated, for metadata.
#' @param description Optional pin description.
#' @param ... Further arguments passed to `pins::pin_write()`.
#'
#' @return `name`, invisibly.
#'
#' @export
#' @family persistence
#'
#' @examples
#' \dontrun{
#' board <- pins::board_folder("pins")
#'
#' eval_result <- evaluate(
#'   classifier,
#'   testset,
#'   metric = metric_exact_match(field = "sentiment"),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' pin_vitals_log(
#'   board,
#'   "sentiment-eval",
#'   eval_result,
#'   module = classifier,
#'   description = "Test set evaluation"
#' )
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

#' Copy a workflow template into a project
#'
#' @description
#' `use_dsprrr_template()` copies starter files for running dsprrr in a
#' pipeline: a targets pipeline (`_targets.R`) that prepares data, optimizes
#' and evaluates a module and pins the results, and a Quarto report
#' (`report.qmd`). Edit the copies to fit your project.
#'
#' @param template `"targets"`, `"quarto"` or `"all"`.
#' @param path Directory to copy into (default: the working directory). It is
#'   created if needed.
#' @param overwrite Whether to replace existing files (default `FALSE`).
#'   Existing files are otherwise skipped with a warning.
#'
#' @return The paths of the created files, invisibly.
#'
#' @export
#' @family integrations
#'
#' @examples
#' project <- file.path(tempdir(), "my-project")
#' use_dsprrr_template("targets", path = project)
#' list.files(project)
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

#' Check a module and its data before a run
#'
#' @description
#' `validate_workflow()` runs quick checks before an expensive batch run or
#' pipeline step: that `module` is a dsprrr module, how many inputs its
#' signature has, that `data` has a column for every input, and that `board`
#' is a pins board. It prints one line per check and makes no model calls.
#'
#' @param module The module to check.
#' @param data Optional data frame to check against the signature's inputs.
#' @param board Optional pins board. Only its class is checked; the board is
#'   not accessed.
#'
#' @return Invisibly, a list with `valid` (`FALSE` when the module, data or
#'   board check fails) and `checks` (one list per check with `passed` and
#'   `message`).
#'
#' @export
#' @family integrations
#'
#' @examples
#' classifier <- module(signature("text -> sentiment"))
#' reviews <- data.frame(text = c("Great!", "Awful."))
#' validate_workflow(classifier, data = reviews)
#'
#' # A missing input column fails the check
#' result <- validate_workflow(classifier, data = data.frame(review = "Great!"))
#' result$valid
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
