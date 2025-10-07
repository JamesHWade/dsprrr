#' Grid Search Optimisation
#'
#' @description
#' Optimise a DSPrrr module over a grid of candidate configurations. Accepts
#' either an explicit `grid` data frame or a set of parameter definitions that
#' can be expanded into a grid (named lists or tidymodels parameter sets).
#'
#' @param module A DSPrrr module (created via [module()]).
#' @param devset Development dataset containing columns required by the module's
#'   signature plus any fields consumed by the metric.
#' @param metric Metric function applied per example. Defaults to
#'   [metric_exact_match()]. Use [as_dsprrr_metric()] to adapt yardstick/vitals
#'   metrics.
#' @param grid Optional data frame/tibble of candidate configurations.
#' @param parameters Optional named list or tidymodels parameter set used to
#'   generate a grid when `grid` is not supplied.
#' @param objective Optimisation direction. `"maximize"` (default) selects the
#'   highest metric value; `"minimize"` selects the lowest.
#' @param .llm Optional ellmer chat object reused during optimisation.
#' @param control Named list of control options. Recognised entries:
#'   `progress` (logical), `parallel` (logical forwarded to [evaluate()]),
#'   `evaluation_progress` (logical), `grid_type` (`"regular"` or `"random"`),
#'   `grid_levels` (integer, for regular grids), and `grid_size` (integer, for
#'   random grids).
#' @param ... Additional arguments forwarded to [evaluate()].
#'
#' @return The optimised module (modified in place, invisibly).
#' @export
optimize_grid <- function(module, ...) {
  UseMethod("optimize_grid")
}

#' @export
optimize_grid.Module <- function(module,
                                 devset,
                                 metric = metric_exact_match(),
                                 grid = NULL,
                                 parameters = NULL,
                                 objective = c("maximize", "minimize"),
                                 .llm = NULL,
                                 control = list(),
                                 ...) {
  objective <- match.arg(objective)

  module$optimize_grid(
    devset = devset,
    metric = metric,
    grid = grid,
    parameters = parameters,
    objective = objective,
    .llm = .llm,
    control = control,
    ...
  )

  module
}

# Internal helpers --------------------------------------------------------

#' Merge optimisation control defaults
#' @keywords internal
merge_optimization_control <- function(control) {
  defaults <- list(
    progress = interactive(),
    parallel = FALSE,
    evaluation_progress = FALSE,
    grid_type = "regular",
    grid_levels = 3L,
    grid_size = NULL
  )

  if (is.null(control)) {
    control <- list()
  }

  merged <- utils::modifyList(defaults, control, keep.null = TRUE)
  merged$grid_type <- tolower(merged$grid_type %||% "regular")
  merged$grid_type <- match.arg(merged$grid_type, c("regular", "random"))

  merged$grid_levels <- as.integer(merged$grid_levels %||% 3L)
  if (isTRUE(merged$grid_levels < 1L)) {
    merged$grid_levels <- 1L
  }

  if (is.null(merged$grid_size) && identical(merged$grid_type, "random")) {
    merged$grid_size <- max(10L, merged$grid_levels)
  }

  merged
}

#' Prepare optimisation grid
#' @keywords internal
prepare_candidate_grid <- function(parameters, grid, control) {
  if (!is.null(grid)) {
    if (is.list(grid) && !is.data.frame(grid)) {
      grid <- expand_grid_from_list(grid)
    }

    if (!is.data.frame(grid)) {
      cli::cli_abort("grid must be a data frame/tibble or a named list of parameter values")
    }

    candidate_grid <- tibble::as_tibble(grid)
  } else {
    if (is.null(parameters)) {
      cli::cli_abort("Provide either `grid` or `parameters` for optimisation")
    }

    candidate_grid <- generate_grid_from_parameters(parameters, control)
  }

  if (nrow(candidate_grid) == 0) {
    cli::cli_abort("The optimisation grid expanded to zero rows")
  }

  candidate_grid
}

#' Generate grid from parameter definition
#' @keywords internal
generate_grid_from_parameters <- function(parameters, control) {
  if (is_dials_parameters(parameters)) {
    rlang::check_installed("dials", reason = "to expand tidymodels parameter sets")

    if (identical(control$grid_type, "regular")) {
      grid <- dials::grid_regular(parameters, levels = control$grid_levels)
    } else {
      size <- control$grid_size %||% control$grid_levels
      grid <- dials::grid_random(parameters, size = size)
    }

    tibble::as_tibble(grid)
  } else if (is.list(parameters)) {
    expand_grid_from_list(parameters)
  } else {
    cli::cli_abort("parameters must be a named list or tidymodels parameter set")
  }
}

#' Expand a named list into a grid
#' @keywords internal
expand_grid_from_list <- function(parameters) {
  if (length(parameters) == 0) {
    cli::cli_abort("Parameter list must contain at least one element")
  }

  unnamed <- names(parameters)
  if (is.null(unnamed) || any(unnamed == "")) {
    cli::cli_abort("All parameters must be named")
  }

  grid <- do.call(
    expand.grid,
    c(parameters, list(stringsAsFactors = FALSE))
  )

  tibble::as_tibble(grid)
}

#' Check for tidymodels parameter set
#' @keywords internal
is_dials_parameters <- function(x) {
  inherits(x, "param_set") || inherits(x, "parameters")
}
