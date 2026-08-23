#' Inspect an Optimization Result
#'
#' `optimization_result()` is the stable, read-only boundary for learning what
#' an optimizer did. Every dsprrr teleprompter reports the same core fields;
#' optimizer-specific evidence lives under a namespaced `extensions` entry.
#'
#' @param program A dsprrr module returned by [compile()] or [optimize_grid()].
#'
#' @return A `dsprrr_optimization_result` with fields:
#'   * `version`: Result schema version.
#'   * `optimizer`: Optimizer identity.
#'   * `status`: Either `"completed"` or `"partial"`.
#'   * `baseline_score`, `best_score`, and `best_trial`: Comparable outcome
#'     measures when the optimizer evaluates candidates.
#'   * `best_params`: Winning parameter values.
#'   * `trials`: Trial-level evidence as a tibble.
#'   * `lineage`: How the winning candidate was derived.
#'   * `budget`: Planned and consumed optimization budget.
#'   * `stop_reason`: Why the optimizer stopped.
#'   * `extensions`: Optimizer-specific evidence, namespaced by optimizer.
#'
#' Returns `NULL` when `program` has not been optimized.
#'
#' @export
#' @family optimizer accessors
#'
#' @examples
#' if (FALSE) {
#' optimized <- compile(program, GEPA(metric = metric), trainset)
#' result <- optimization_result(optimized)
#' result$best_score
#' result$trials
#' result$extensions$gepa
#' }
optimization_result <- function(program) {
  assert_optimization_program(program)
  result <- program$state$optimization_result
  if (is.null(result)) {
    return(NULL)
  }

  structure(
    rlang::duplicate(result, shallow = FALSE),
    class = c("dsprrr_optimization_result", "list")
  )
}

#' @export
#' @param x An optimization result.
#' @param ... Additional arguments, currently unused.
#' @rdname optimization_result
print.dsprrr_optimization_result <- function(x, ...) {
  cat("<dsprrr_optimization_result>\n", sep = "")
  cat("Optimizer: ", x$optimizer, "\n", sep = "")
  cat("Status: ", x$status, "\n", sep = "")
  if (!is.na(x$best_score)) {
    cat("Best score: ", format(x$best_score, digits = 6L), "\n", sep = "")
  }
  cat("Trials: ", nrow(x$trials), "\n", sep = "")
  if (!is.na(x$stop_reason)) {
    cat("Stopped: ", x$stop_reason, "\n", sep = "")
  }
  invisible(x)
}

assert_optimization_program <- function(program) {
  if (!inherits(program, "Module")) {
    cli::cli_abort(
      "{.arg program} must be a dsprrr Module object",
      class = "dsprrr_optimization_result_error"
    )
  }
  invisible(program)
}

optimization_result_abort <- function(message) {
  cli::cli_abort(
    message,
    class = "dsprrr_optimization_result_error",
    .envir = parent.frame()
  )
}

optimization_result_scalar_number <- function(value, field) {
  if (is.null(value)) {
    return(NA_real_)
  }
  if (!is.numeric(value) || length(value) != 1L || is.nan(value)) {
    optimization_result_abort(
      "{.arg {field}} must be one numeric value or NULL"
    )
  }
  as.numeric(value)
}

optimization_result_scalar_integer <- function(value, field) {
  if (is.null(value)) {
    return(NA_integer_)
  }
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      value < 1 ||
      value != as.integer(value)
  ) {
    optimization_result_abort(
      "{.arg {field}} must be one positive integer or NULL"
    )
  }
  as.integer(value)
}

optimization_result_list <- function(value, field) {
  if (is.null(value)) {
    return(list())
  }
  if (!is.list(value)) {
    optimization_result_abort("{.arg {field}} must be a list")
  }
  if (optimization_result_has_runtime_value(value)) {
    optimization_result_abort(
      "{.arg {field}} must contain persistable values, not functions or environments"
    )
  }
  value
}

optimization_result_has_runtime_value <- function(value) {
  if (is.function(value) || is.environment(value)) {
    return(TRUE)
  }
  if (typeof(value) %in% c("externalptr", "weakref")) {
    return(TRUE)
  }
  if (is.list(value)) {
    return(any(vapply(
      value,
      optimization_result_has_runtime_value,
      logical(1)
    )))
  }
  FALSE
}

optimization_result_record_valid <- function(result) {
  expected_names <- c(
    "version",
    "optimizer",
    "status",
    "baseline_score",
    "best_score",
    "best_trial",
    "best_params",
    "trials",
    "lineage",
    "budget",
    "stop_reason",
    "extensions"
  )
  is.list(result) &&
    identical(names(result), expected_names) &&
    identical(result$version, 1L) &&
    is.character(result$optimizer) &&
    length(result$optimizer) == 1L &&
    !is.na(result$optimizer) &&
    nzchar(result$optimizer) &&
    is.character(result$status) &&
    length(result$status) == 1L &&
    result$status %in% c("completed", "partial") &&
    optimization_result_number_valid(result$baseline_score) &&
    optimization_result_number_valid(result$best_score) &&
    optimization_result_trial_valid(result$best_trial) &&
    is.list(result$best_params) &&
    is.data.frame(result$trials) &&
    is.list(result$lineage) &&
    is.list(result$budget) &&
    is.character(result$stop_reason) &&
    length(result$stop_reason) == 1L &&
    is.list(result$extensions) &&
    length(result$extensions) == 1L &&
    identical(
      names(result$extensions),
      optimization_extension_key(result$optimizer)
    ) &&
    !optimization_result_has_runtime_value(result)
}

optimization_result_number_valid <- function(value) {
  is.numeric(value) && length(value) == 1L && !is.nan(value)
}

optimization_result_trial_valid <- function(value) {
  is.integer(value) &&
    length(value) == 1L &&
    (is.na(value) || value >= 1L)
}

optimization_stop_reason <- function(budget, default = "completed") {
  reason <- budget$stop_reason
  if (is.null(reason)) {
    return(default)
  }
  if (is.character(reason) && length(reason) == 1L && !is.na(reason)) {
    return(reason)
  }
  if (is.list(reason)) {
    return(reason$code %||% reason$reason %||% reason$message %||% "budget")
  }
  "budget"
}

optimization_extension_key <- function(optimizer) {
  key <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", optimizer, perl = TRUE)
  key <- gsub("[^A-Za-z0-9]+", "_", key, perl = TRUE)
  key <- gsub("^_+|_+$", "", key, perl = TRUE)
  tolower(key)
}

new_optimization_result <- function(
  optimizer,
  status = "completed",
  baseline_score = NULL,
  best_score = NULL,
  best_trial = NULL,
  best_params = list(),
  trials = tibble::tibble(),
  lineage = list(),
  budget = list(),
  stop_reason = NULL,
  extensions = list()
) {
  if (
    !is.character(optimizer) ||
      length(optimizer) != 1L ||
      is.na(optimizer) ||
      !nzchar(trimws(optimizer))
  ) {
    optimization_result_abort("{.arg optimizer} must be one non-empty string")
  }
  if (
    !is.character(status) ||
      length(status) != 1L ||
      is.na(status) ||
      !status %in% c("completed", "partial")
  ) {
    optimization_result_abort(
      "{.arg status} must be {.val completed} or {.val partial}"
    )
  }
  if (is.null(trials)) {
    trials <- tibble::tibble()
  }
  if (!is.data.frame(trials)) {
    optimization_result_abort("{.arg trials} must be a data frame")
  }
  best_params <- optimization_result_list(best_params, "best_params")
  lineage <- optimization_result_list(lineage, "lineage")
  budget <- optimization_result_list(budget, "budget")
  extensions <- optimization_result_list(extensions, "extensions")
  if (
    !is.null(stop_reason) &&
      (!is.character(stop_reason) ||
        length(stop_reason) != 1L ||
        is.na(stop_reason) ||
        !nzchar(stop_reason))
  ) {
    optimization_result_abort(
      "{.arg stop_reason} must be one non-empty string or NULL"
    )
  }

  namespaced_extensions <- list(extensions)
  names(namespaced_extensions) <- optimization_extension_key(optimizer)

  list(
    version = 1L,
    optimizer = optimizer,
    status = status,
    baseline_score = optimization_result_scalar_number(
      baseline_score,
      "baseline_score"
    ),
    best_score = optimization_result_scalar_number(best_score, "best_score"),
    best_trial = optimization_result_scalar_integer(best_trial, "best_trial"),
    best_params = best_params,
    trials = tibble::as_tibble(trials),
    lineage = lineage,
    budget = budget,
    stop_reason = stop_reason %||% NA_character_,
    extensions = namespaced_extensions
  )
}

set_optimization_result <- function(program, result) {
  assert_optimization_program(program)
  program$state$optimization_result <- result
  program$state$compiled <- TRUE
  program$state$best_score <- if (is.na(result$best_score)) {
    NULL
  } else {
    result$best_score
  }
  program$state$best_trial <- if (is.na(result$best_trial)) {
    NULL
  } else {
    result$best_trial
  }
  program$state$best_params <- result$best_params
  program$state$trials <- result$trials
  program$config$compiled <- TRUE
  program$config$teleprompter <- result$optimizer
  program$config$optimizer <- NULL
  invisible(program)
}

record_optimization_result <- function(
  program,
  optimizer,
  status = "completed",
  baseline_score = NULL,
  best_score = NULL,
  best_trial = NULL,
  best_params = list(),
  trials = NULL,
  lineage = list(),
  budget = list(),
  stop_reason = NULL,
  extensions = list()
) {
  assert_optimization_program(program)
  trials <- trials %||% program$state$trials %||% tibble::tibble()
  result <- new_optimization_result(
    optimizer = optimizer,
    status = status,
    baseline_score = baseline_score,
    best_score = best_score,
    best_trial = best_trial,
    best_params = best_params,
    trials = trials,
    lineage = lineage,
    budget = budget,
    stop_reason = stop_reason,
    extensions = extensions
  )
  set_optimization_result(program, result)
}
