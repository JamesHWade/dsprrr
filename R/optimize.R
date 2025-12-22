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

#' @rdname optimize_grid
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

#' @keywords internal
signature_parameter_defaults <- function(signature, prefix = "input") {
  defaults <- list()

  if (!inherits(signature, "dsprrr::Signature") || length(signature@inputs) == 0) {
    return(defaults)
  }

  for (inp in signature@inputs) {
    input_name <- inp$name
    if (is.null(input_name) || !nzchar(input_name)) {
      input_name <- "input"
    }
    type <- inp$type

    if (inherits(type, "ellmer::TypeEnum")) {
      param_name <- paste0(prefix, "_", input_name)
      defaults[[param_name]] <- type@values
    }
  }

  defaults
}

#' Suggest tidymodels parameters for a module
#'
#' @description
#' Construct a `dials::parameters()` set from the information available on a
#' module. Numeric parameters (e.g., `temperature`, `top_p`) derive their ranges
#' from observed optimisation trials (if present) or fall back to sensible
#' defaults. Qualitative parameters (e.g., `prompt_style`) are converted to
#' value sets.
#'
#' @param module A DSPrrr module (created with [module()]).
#' @param include Optional character vector restricting which parameters are
#'   returned. Defaults to all parameters discovered in the module configuration
#'   and optimisation trials.
#' @param exclude Character vector of parameter names to ignore. Defaults to
#'   internal bookkeeping fields such as `id` and `instructions`.
#'
#' @return A [`dials::parameters`] object describing the candidate tunables.
#'   Returns an empty parameter set when no tunables are discovered.
#' @export
#' @examples
#' \dontrun{
#' sig <- signature("text -> sentiment")
#' mod <- module(sig, type = "predict", config = list(temperature = 0.2))
#' optimize_grid(mod,
#'   devset = tibble::tibble(text = "sample", target = "positive"),
#'   parameters = list(temperature = c(0.1, 0.5))
#' )
#' module_parameter_set(mod)
#' }
module_parameter_set <- function(module,
                                 include = NULL,
                                 exclude = c("id", "instructions", "instructions_suffix")) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("module must be a DSPrrr Module object")
  }

  rlang::check_installed("dials", reason = "to build parameter sets")

  param_values <- list()

  # Seed with config values
  config_values <- module$config
  if (length(config_values) > 0) {
    for (name in names(config_values)) {
      value <- config_values[[name]]
      if (is.atomic(value) && length(value) == 1) {
        param_values[[name]] <- c(param_values[[name]], value)
      }
    }
  }

  # Incorporate optimisation trials if available
  trials <- module$state$trials
  if (is.data.frame(trials) && nrow(trials) > 0) {
    trial_params <- trials$parameters
    for (params in trial_params) {
      if (!is.list(params)) next
      for (name in names(params)) {
        param_values[[name]] <- c(param_values[[name]], params[[name]])
      }
    }
  }

  if (!is.null(include)) {
    param_values <- param_values[intersect(names(param_values), include)]
  } else {
    param_values <- param_values[setdiff(names(param_values), exclude)]
  }

  sig_defaults <- signature_parameter_defaults(module$signature)
  for (name in names(sig_defaults)) {
    should_include <- is.null(include) || name %in% include
    if (should_include && !(name %in% exclude) && is.null(param_values[[name]])) {
      param_values[[name]] <- sig_defaults[[name]]
    }
  }

  known_defaults <- list(
    temperature = c(0, 1),
    top_p = c(0, 1),
    frequency_penalty = c(-2, 2),
    presence_penalty = c(-2, 2),
    max_output_tokens = c(32, 4096)
  )

  for (name in names(known_defaults)) {
    should_include <- (is.null(include) || name %in% include) && !(name %in% exclude)
    if (should_include && !name %in% names(param_values)) {
      param_values[[name]] <- known_defaults[[name]]
    }
  }

  build_param <- function(name, values) {
    values <- values[!vapply(values, is.null, logical(1))]
    values <- unlist(values, recursive = TRUE, use.names = FALSE)
    values <- values[!is.na(values)]
    if (length(values) == 0) {
      return(NULL)
    }

    label <- stats::setNames(paste("Module", name), name)

    if (all(values %in% c(TRUE, FALSE))) {
      unique_vals <- sort(unique(as.logical(values)))
      return(dials::new_qual_param(
        type = "logical",
        values = unique_vals,
        label = label
      ))
    }

    if (is.numeric(values)) {
      rng <- range(values, na.rm = TRUE)
      if (rng[1] == rng[2]) {
        rng <- rng + c(-0.1, 0.1)
      }
      return(dials::new_quant_param(
        type = "double",
        range = rng,
        inclusive = c(TRUE, TRUE),
        label = label
      ))
    }

    if (is.character(values)) {
      unique_vals <- sort(unique(values))
      return(dials::new_qual_param(
        type = "character",
        values = unique_vals,
        label = label
      ))
    }

    NULL
  }

  params <- vector("list", length(param_values))
  param_names <- names(param_values)
  for (i in seq_along(param_values)) {
    params[[i]] <- build_param(param_names[[i]], param_values[[i]])
  }
  params <- params[!vapply(params, is.null, logical(1))]

  if (length(params) == 0) {
    return(dials::parameters())
  }

  do.call(dials::parameters, params)
}

#' Summarise optimisation trials for a module
#'
#' @description
#' Provide a tidy summary of the optimisation trials recorded on a module.
#' Useful for reporting best scores, average performance, and highlighting the
#' winning parameter combination.
#'
#' @param module A DSPrrr module that has been optimised with [optimize_grid()].
#' @param objective Optimisation direction; `"maximize"` (default) selects the
#'   highest score, `"minimize"` selects the lowest.
#'
#' @return A tibble with one row containing:
#'   * `n_trials`: number of trials evaluated.
#'   * `best_trial`: identifier of the best-performing trial.
#'   * `best_score`: best score achieved.
#'   * `mean_score`: mean across all scores.
#'   * `std_error`: standard error of the scores.
#'   * `best_params`: list-column containing the best parameter set.
#'   * `trials`: list-column containing the full trials tibble.
#'
#' @export
#' @examples
#' \dontrun{
#' summary <- module_trials_summary(my_module)
#' summary$best_params
#' }
module_trials_summary <- function(module, objective = c("maximize", "minimize")) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("module must be a DSPrrr Module object")
  }

  objective <- match.arg(objective)
  trials <- module$state$trials

  if (!is.data.frame(trials) || nrow(trials) == 0) {
    return(tibble::tibble(
      n_trials = 0L,
      best_trial = NA_integer_,
      best_score = NA_real_,
      mean_score = NA_real_,
      std_error = NA_real_,
      best_params = list(NULL),
      trials = list(tibble::tibble())
    ))
  }

  scores <- trials$score
  best_idx <- if (objective == "maximize") which.max(scores) else which.min(scores)
  best_score <- scores[best_idx]
  best_trial <- trials$trial_id[best_idx]
  best_params <- trials$parameters[[best_idx]]

  mean_score <- mean(scores, na.rm = TRUE)
  sd_score <- stats::sd(scores, na.rm = TRUE)
  std_error <- if (is.na(sd_score)) NA_real_ else sd_score / sqrt(length(scores))

  tibble::tibble(
    n_trials = nrow(trials),
    best_trial = best_trial,
    best_score = best_score,
    mean_score = mean_score,
    std_error = std_error,
    best_params = list(best_params),
    trials = list(trials)
  )
}

#' Summarise optimisation metrics per trial
#'
#' @description
#' Flatten the optimisation trials recorded on a module into a tidy data frame
#' containing per-trial metric summaries. Useful for producing tables or
#' visualisations comparing trial performance. When yardstick metrics are
#' supplied, the function also computes those metrics for each trial using the
#' stored evaluation datasets.
#' 
#' @param module A DSPrrr module optimised with [optimize_grid()].
#' @param metrics Optional yardstick metric (or metric set) to compute for each
#'   trial.
#' @param truth Column name (string) containing the ground-truth labels when
#'   computing yardstick metrics.
#' @param estimate Column name (string) containing the model predictions when
#'   computing yardstick metrics.
#' @param ... Additional arguments passed to yardstick metrics.
#'
#' @return A tibble with one row per trial containing columns:
#'   * `trial_id` - trial identifier.
#'   * `score` - overall score recorded for the trial.
#'   * `mean_score`, `median_score`, `std_dev` - summary statistics across the
#'     evaluation scores.
#'   * `n_evaluated`, `n_errors` - counts reported by the evaluation.
#'   * `params` - list-column with the parameters evaluated in the trial.
#'   * `scores` - list-column with the raw per-example scores (if available).
#'   * `yardstick` - list-column containing yardstick metric results when
#'     requested.
#' @export
#' @examples
#' \dontrun{
#' trial_metrics <- module_metric_summary(my_module)
#' yardstick_metrics <- module_metric_summary(
#'   my_module,
#'   metrics = yardstick::metric_set(yardstick::accuracy),
#'   truth = target,
#'   estimate = result
#' )
#' }
module_metric_summary <- function(module,
                                  metrics = NULL,
                                  truth = NULL,
                                  estimate = NULL,
                                  ...) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("module must be a DSPrrr Module object")
  }

  trials <- module$state$trials

  if (!is.data.frame(trials) || nrow(trials) == 0) {
    return(tibble::tibble(
      trial_id = integer(0),
      score = numeric(0),
      mean_score = numeric(0),
      median_score = numeric(0),
      std_dev = numeric(0),
      n_evaluated = integer(0),
      n_errors = integer(0),
      params = list(),
      scores = list(),
      yardstick = list()
    ))
  }

  yardstick_metrics <- NULL
  truth_sym <- estimate_sym <- NULL
  if (!is.null(metrics)) {
    rlang::check_installed("yardstick")
    if (is.null(truth) || is.null(estimate)) {
      cli::cli_abort("Provide `truth` and `estimate` column names when supplying yardstick metrics.")
    }
    if (inherits(metrics, "metric_set")) {
      yardstick_metrics <- metrics
    } else {
      yardstick_metrics <- do.call(yardstick::metric_set, as.list(metrics))
    }
    truth_sym <- if (is.character(truth)) rlang::sym(truth) else rlang::ensym(truth)
    estimate_sym <- if (is.character(estimate)) rlang::sym(estimate) else rlang::ensym(estimate)
  }

  rows <- vector("list", nrow(trials))

  for (i in seq_len(nrow(trials))) {
    eval <- trials$evaluation[[i]]
    scores <- eval$scores %||% numeric()
    score_mean <- eval$mean_score %||% if (length(scores)) mean(scores, na.rm = TRUE) else trials$score[[i]]
    score_median <- if (length(scores)) stats::median(scores, na.rm = TRUE) else NA_real_
    score_sd <- if (length(scores) > 1) stats::sd(scores, na.rm = TRUE) else NA_real_
    n_eval <- eval$n_evaluated %||% if (length(scores)) sum(!is.na(scores)) else NA_integer_
    n_err <- eval$n_errors %||% if (length(scores)) sum(is.na(scores)) else NA_integer_

    yardstick_results <- NULL
    if (!is.null(yardstick_metrics) && !is.null(eval$dataset)) {
      data <- eval$dataset
      estimate_col <- rlang::as_string(estimate_sym)
      truth_col <- rlang::as_string(truth_sym)

      coerce_col <- function(vec) {
        if (is.list(vec)) {
          vec <- vapply(vec, function(x) {
            if (length(x) == 0) NA_character_ else as.character(x[[1]])
          }, character(1))
        }
        vec
      }

      if (estimate_col %in% names(data)) {
        data[[estimate_col]] <- coerce_col(data[[estimate_col]])
      }

      truth_vec <- NULL

      if (!(truth_col %in% names(data))) {
        cli::cli_warn("Truth column '{truth_col}' not found in evaluation dataset for trial {trials$trial_id[[i]]}.")
      } else if (!(estimate_col %in% names(data))) {
        cli::cli_warn("Estimate column '{estimate_col}' not found in evaluation dataset for trial {trials$trial_id[[i]]}.")
      } else {
        truth_vec <- coerce_col(data[[truth_col]])
        est_vec <- coerce_col(data[[estimate_col]])
        levels_union <- unique(c(stats::na.omit(truth_vec), stats::na.omit(est_vec)))
        if (length(levels_union) == 0) {
          cli::cli_warn("Unable to determine class levels for trial {trials$trial_id[[i]]}; skipping yardstick metrics.")
        } else {
          data[[truth_col]] <- factor(truth_vec, levels = levels_union)
          data[[estimate_col]] <- factor(est_vec, levels = levels_union)

          yardstick_results <- tryCatch(
            yardstick_metrics(
              data,
              truth = !!truth_sym,
              estimate = !!estimate_sym,
              ...
            ),
            error = function(e) {
              cli::cli_warn("Failed to compute yardstick metrics for trial {trials$trial_id[[i]]}: {e$message}")
              NULL
            }
          )
        }
      }
    }

    rows[[i]] <- tibble::tibble(
      trial_id = trials$trial_id[[i]],
      score = trials$score[[i]],
      mean_score = score_mean,
      median_score = score_median,
      std_dev = score_sd,
      n_evaluated = n_eval,
      n_errors = n_err,
      params = list(trials$parameters[[i]]),
      scores = list(scores),
      yardstick = list(yardstick_results)
    )
  }

  result <- rows[[1]]
  if (length(rows) > 1) {
    for (i in 2:length(rows)) {
      result <- tibble::add_row(result, !!!rows[[i]])
    }
  }

  result
}
