#' Grid search over module settings
#'
#' @description
#' `optimize_grid()` evaluates a module once for every row of a grid of
#' settings, such as different `reasoning_effort` values or instructions, and
#' applies the best-scoring row to the module. It modifies the module in
#' place, unlike [compile()], which returns a new program.
#'
#' @details
#' Give the candidates as `grid`, a data frame with one row per candidate, or
#' as `parameters`, which is expanded into a grid: a named list is crossed with
#' [expand.grid()], and a tidymodels parameter set (see [module_parameters()])
#' is expanded with `dials::grid_regular()` or `dials::grid_random()`,
#' depending on `control`. One of the two is required.
#'
#' These grid columns change what the module sends:
#'
#' * Runtime settings, sent to the chat: `temperature`, `top_p`,
#'   `reasoning_effort`, `frequency_penalty`, `presence_penalty`, `max_tokens`,
#'   `max_output_tokens` and `service_tier`. Reasoning models restrict
#'   sampling settings: gpt-6-luna, for example, accepts `temperature` and
#'   `top_p` only when `reasoning_effort` is `"none"`.
#' * `instructions` replaces the signature's instructions, and
#'   `instructions_suffix` is appended to them.
#' * `template` replaces the prompt template.
#'
#' Other columns are stored in `module$config` but do not change the prompt.
#'
#' Each candidate is evaluated on a copy of the module with [evaluate()], so a
#' grid of `n` rows makes up to `n * nrow(data)` model calls. The trials are
#' stored on the module; read them with [module_trials()], [top_trials()] or
#' [optimization_result()].
#'
#' @param module A module, such as one created with [module()].
#' @param data A data frame with the signature's input columns and the columns
#'   the metric compares.
#' @param metric A metric function called as `metric(prediction, expected)`.
#'   The default, [metric_exact_match()] without a `field`, compares the one
#'   output field that also names a column of `data`; pass `field` to choose
#'   the column explicitly. Use [as_dsprrr_metric()] to adapt a vitals scorer.
#' @param grid A data frame with one row per candidate, or a named list that is
#'   expanded like `parameters`.
#' @param parameters Used when `grid` is `NULL`: a named list of values to
#'   cross, or a tidymodels parameter set such as the one returned by
#'   [module_parameters()].
#' @param objective `"maximize"` (the default) keeps the highest mean score;
#'   `"minimize"` keeps the lowest.
#' @param .llm Optional ellmer Chat used for every evaluation.
#' @param control A named list of options: `progress` (a progress bar over
#'   candidates; default `interactive()`), `evaluation_progress` (a bar inside
#'   each evaluation; default `FALSE`), and, for tidymodels parameter sets,
#'   `grid_type` (`"regular"`, the default, or `"random"`), `grid_levels`
#'   (levels per parameter in a regular grid; default `3L`) and `grid_size`
#'   (candidates in a random grid; default `max(10L, grid_levels)`). A
#'   `parallel` entry is accepted but has no effect; pass `.concurrency`
#'   through `...` to evaluate rows concurrently.
#' @param ... Further arguments passed to [evaluate()], such as
#'   `.concurrency = concurrency_control(max_active = 4L)`.
#'
#' @return The module, modified in place: the best row's settings are applied
#'   and the trials are recorded. When no candidate produces a score, the
#'   settings are left unchanged and a warning is raised.
#' @family grid search
#' @examples
#' \dontrun{
#' classifier <- module(signature("text -> sentiment"))
#' devset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "It's okay"),
#'   sentiment = c("positive", "negative", "neutral")
#' )
#'
#' optimize_grid(
#'   classifier,
#'   data = devset,
#'   metric = metric_exact_match(field = "sentiment"),
#'   grid = data.frame(reasoning_effort = c("none", "low", "medium")),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#'
#' # `classifier` now carries the best setting and the trials
#' classifier$config$reasoning_effort
#' module_trials(classifier)
#'
#' # Named lists are crossed into a grid
#' optimize_grid(
#'   classifier,
#'   data = devset,
#'   metric = metric_exact_match(field = "sentiment"),
#'   parameters = list(
#'     reasoning_effort = c("none", "low"),
#'     instructions_suffix = c("Answer with one word.", "Be decisive.")
#'   ),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' }
#' @export
optimize_grid <- function(module, ...) {
  UseMethod("optimize_grid")
}

#' @rdname optimize_grid
#' @export
optimize_grid.Module <- function(
  module,
  data,
  metric = metric_exact_match(),
  grid = NULL,
  parameters = NULL,
  objective = c("maximize", "minimize"),
  .llm = NULL,
  control = list(),
  ...
) {
  objective <- match.arg(objective)

  module$optimize_grid(
    data = data,
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

#' Merge optimization control defaults
#' @noRd
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

#' Prepare optimization grid
#' @noRd
prepare_candidate_grid <- function(parameters, grid, control) {
  if (!is.null(grid)) {
    if (is.list(grid) && !is.data.frame(grid)) {
      grid <- expand_grid_from_list(grid)
    }

    if (!is.data.frame(grid)) {
      cli::cli_abort(
        "grid must be a data frame/tibble or a named list of parameter values"
      )
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
#' @noRd
generate_grid_from_parameters <- function(parameters, control) {
  if (is_dials_parameters(parameters)) {
    rlang::check_installed(
      "dials",
      reason = "to expand tidymodels parameter sets"
    )

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
    cli::cli_abort(
      "parameters must be a named list or tidymodels parameter set"
    )
  }
}

#' Expand a named list into a grid
#' @noRd
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
  # expand.grid() records its inputs in "out.attrs"; drop it so best_params
  # stay plain values that save_program() and pins can persist.
  attr(grid, "out.attrs") <- NULL

  tibble::as_tibble(grid)
}

#' Check for tidymodels parameter set
#' @noRd
is_dials_parameters <- function(x) {
  inherits(x, "param_set") || inherits(x, "parameters")
}

#' Enum input values as default qualitative parameters
#' @noRd
signature_parameter_defaults <- function(signature, prefix = "input") {
  defaults <- list()

  if (
    !inherits(signature, "dsprrr::Signature") || length(signature@inputs) == 0
  ) {
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

#' Build a tidymodels parameter set for a module
#'
#' @description
#' `module_parameters()` returns a `dials::parameters()` set describing values
#' of a module that can be tuned. Pass it to [optimize_grid()] as `parameters`
#' to search a regular or random grid over those values.
#'
#' @details
#' Candidate parameters come from:
#'
#' * single values in `module$config` and `module$config$params`;
#' * the parameters of trials recorded by [optimize_grid()];
#' * enum inputs of the signature, as `input_<name>` with the enum levels;
#' * runtime settings with default ranges: `temperature` and `top_p` in
#'   `[0, 1]`, `frequency_penalty` and `presence_penalty` in `[-2, 2]`, and
#'   `max_output_tokens` in `[32, 4096]`.
#'
#' Numeric values become quantitative parameters spanning the observed range
#' (plus or minus 0.1 around a single value); character and logical values
#' become qualitative parameters. For a reasoning model (see
#' [is_reasoning_model()]), `temperature` and `top_p` are dropped and
#' `reasoning_effort` (`"low"`, `"medium"`, `"high"`) is added.
#'
#' Only runtime settings, `instructions`, `instructions_suffix` and `template`
#' change what [optimize_grid()] sends to the model. Other parameters, such as
#' `input_<name>` or internal config fields like `.module_kind`, are stored in
#' the module's config without effect, so use `include` to keep the ones you
#' mean to tune.
#'
#' @param module A module, such as one created with [module()].
#' @param model Optional model name, such as `"gpt-6-luna"`. For a reasoning
#'   model, `temperature` and `top_p` are replaced by `reasoning_effort`.
#' @param include Optional character vector of parameter names to keep.
#' @param exclude Character vector of parameter names to drop when `include` is
#'   `NULL`. The default drops `id`, `instructions` and `instructions_suffix`.
#'
#' @return A `dials::parameters()` object, empty when nothing tunable is found.
#' @family grid search
#' @export
#' @examplesIf rlang::is_installed("dials")
#' mod <- module(
#'   signature("text -> sentiment"),
#'   config = list(temperature = 0.2)
#' )
#' module_parameters(mod, include = c("temperature", "top_p"))
#'
#' # Reasoning models tune reasoning_effort instead of temperature
#' module_parameters(
#'   mod,
#'   model = "gpt-6-luna",
#'   include = c("temperature", "reasoning_effort")
#' )
module_parameters <- function(
  module,
  model = NULL,
  include = NULL,
  exclude = c("id", "instructions", "instructions_suffix")
) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("module must be a DSPrrr Module object")
  }

  rlang::check_installed("dials", reason = "to build parameter sets")

  param_values <- list()

  # Seed with config values
  config_values <- module$config
  if (length(config_values) > 0) {
    for (name in names(config_values)) {
      if (identical(name, "params") && is.list(config_values[[name]])) {
        for (param_name in names(config_values[[name]])) {
          param_value <- config_values[[name]][[param_name]]
          if (is.atomic(param_value) && length(param_value) == 1) {
            param_values[[param_name]] <- c(
              param_values[[param_name]],
              param_value
            )
          }
        }
        next
      }

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
      if (!is.list(params)) {
        next
      }
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
    if (
      should_include && !(name %in% exclude) && is.null(param_values[[name]])
    ) {
      param_values[[name]] <- sig_defaults[[name]]
    }
  }

  # Check if this is a reasoning model

  is_reasoning <- if (!is.null(model)) {
    is_reasoning_model(model)
  } else {
    FALSE
  }

  # Define known defaults based on model type
  if (is_reasoning) {
    # Reasoning models don't support temperature/top_p
    # They use reasoning_effort instead
    known_defaults <- list(
      reasoning_effort = c("low", "medium", "high"),
      frequency_penalty = c(-2, 2),
      presence_penalty = c(-2, 2),
      max_output_tokens = c(32, 4096)
    )
    # Also exclude temperature/top_p from any existing param_values
    param_values <- param_values[
      !names(param_values) %in% c("temperature", "top_p")
    ]
  } else {
    # Traditional models use temperature/top_p
    known_defaults <- list(
      temperature = c(0, 1),
      top_p = c(0, 1),
      frequency_penalty = c(-2, 2),
      presence_penalty = c(-2, 2),
      max_output_tokens = c(32, 4096)
    )
  }

  for (name in names(known_defaults)) {
    should_include <- (is.null(include) || name %in% include) &&
      !(name %in% exclude)
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

#' Summarize grid search trials
#'
#' @description
#' `module_trials()` summarizes the trials that [optimize_grid()] recorded on a
#' module: how many were run, the best trial, its score and parameters, and
#' the mean and standard error of all trial scores. It reads only the
#' grid-search trials in `module$state$trials`, which [optimize_grid()] and
#' [GridSearchTeleprompter()] write. For other optimizers, use
#' [optimization_result()] or [top_trials()].
#'
#' @param module A module.
#' @param objective `"maximize"` (the default) or `"minimize"`: which end of
#'   the scores counts as best.
#'
#' @return A one-row tibble with columns:
#'   * `n_trials`: number of trials.
#'   * `best_trial`: identifier of the best trial.
#'   * `best_score`: its score.
#'   * `mean_score`, `std_error`: mean and standard error of the trial scores.
#'   * `best_params`: list-column with the best trial's parameters.
#'   * `trials`: list-column with the full trials tibble.
#'
#'   Without trials, `n_trials` is 0 and the scores are `NA`. When every trial
#'   failed, a warning is raised and the scores are `NA`.
#' @family grid search
#' @export
#' @examples
#' mod <- module(signature("text -> sentiment"))
#' module_trials(mod)
#'
#' \dontrun{
#' optimize_grid(
#'   mod,
#'   data = devset,
#'   metric = metric_exact_match(field = "sentiment"),
#'   grid = data.frame(reasoning_effort = c("none", "low", "medium")),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' module_trials(mod)$best_params
#' }
module_trials <- function(
  module,
  objective = c("maximize", "minimize")
) {
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
  if (all(is.na(scores))) {
    cli::cli_warn(c(
      "All {nrow(trials)} optimization trial{?s} failed (no valid scores).",
      "i" = "Check that the LLM is reachable and the metric returns numeric scores.",
      "i" = "Inspect the {.field trials} list-column for per-trial details."
    ))
    return(tibble::tibble(
      n_trials = nrow(trials),
      best_trial = NA_integer_,
      best_score = NA_real_,
      mean_score = NA_real_,
      std_error = NA_real_,
      best_params = list(NULL),
      trials = list(trials)
    ))
  }
  best_idx <- if (objective == "maximize") {
    which.max(scores)
  } else {
    which.min(scores)
  }
  best_score <- scores[best_idx]
  best_trial <- trials$trial_id[best_idx]
  best_params <- trials$parameters[[best_idx]]

  mean_score <- mean(scores, na.rm = TRUE)
  sd_score <- stats::sd(scores, na.rm = TRUE)
  std_error <- if (is.na(sd_score)) {
    NA_real_
  } else {
    sd_score / sqrt(length(scores))
  }

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

#' Per-trial rows from a grid search
#'
#' @description
#' `module_metrics()` returns one row per grid-search trial recorded on a
#' module by [optimize_grid()] (or [GridSearchTeleprompter()]). Like
#' [module_trials()], it reads only `module$state$trials`.
#'
#' @details
#' The per-example evaluation results are not kept with the trials, so
#' `median_score`, `std_dev`, `n_evaluated` and `n_errors` are `NA`, `scores`
#' is empty, and the yardstick metrics requested with `metrics` are not
#' computed: the `yardstick` column is always `NULL`. Use [module_trials()] or
#' [top_trials()] for trial scores.
#'
#' @param module A module.
#' @param metrics Optional yardstick metric or metric set. Currently never
#'   computed; see Details.
#' @param truth,estimate Column names for the yardstick metrics. Required when
#'   `metrics` is supplied.
#' @param ... Passed to the yardstick metrics.
#'
#' @return A tibble with one row per trial and columns `trial_id`, `score`,
#'   `mean_score` (the trial score), `median_score`, `std_dev`,
#'   `n_evaluated`, `n_errors`, `params` (list-column of the trial's
#'   parameters), `scores` and `yardstick`.
#' @family grid search
#' @export
#' @examples
#' # Without trials the result is an empty tibble with these columns
#' module_metrics(module(signature("text -> sentiment")))
module_metrics <- function(
  module,
  metrics = NULL,
  truth = NULL,
  estimate = NULL,
  ...
) {
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
      cli::cli_abort(
        "Provide `truth` and `estimate` column names when supplying yardstick metrics."
      )
    }
    if (inherits(metrics, "metric_set")) {
      yardstick_metrics <- metrics
    } else {
      yardstick_metrics <- do.call(yardstick::metric_set, as.list(metrics))
    }
    truth_sym <- if (is.character(truth)) {
      rlang::sym(truth)
    } else {
      rlang::ensym(truth)
    }
    estimate_sym <- if (is.character(estimate)) {
      rlang::sym(estimate)
    } else {
      rlang::ensym(estimate)
    }
  }

  rows <- vector("list", nrow(trials))

  for (i in seq_len(nrow(trials))) {
    eval <- trials$evaluation[[i]]
    scores <- eval$scores %||% numeric()
    score_mean <- eval$mean_score %||%
      if (length(scores)) mean(scores, na.rm = TRUE) else trials$score[[i]]
    score_median <- if (length(scores)) {
      stats::median(scores, na.rm = TRUE)
    } else {
      NA_real_
    }
    score_sd <- if (length(scores) > 1) {
      stats::sd(scores, na.rm = TRUE)
    } else {
      NA_real_
    }
    n_eval <- eval$n_evaluated %||%
      if (length(scores)) sum(!is.na(scores)) else NA_integer_
    n_err <- eval$n_errors %||%
      if (length(scores)) sum(is.na(scores)) else NA_integer_

    yardstick_results <- NULL
    if (!is.null(yardstick_metrics) && !is.null(eval$dataset)) {
      data <- eval$dataset
      estimate_col <- rlang::as_string(estimate_sym)
      truth_col <- rlang::as_string(truth_sym)

      coerce_col <- function(vec) {
        if (is.list(vec)) {
          vec <- vapply(
            vec,
            function(x) {
              if (length(x) == 0) NA_character_ else as.character(x[[1]])
            },
            character(1)
          )
        }
        vec
      }

      if (estimate_col %in% names(data)) {
        data[[estimate_col]] <- coerce_col(data[[estimate_col]])
      }

      truth_vec <- NULL

      if (!(truth_col %in% names(data))) {
        cli::cli_warn(
          "Truth column '{truth_col}' not found in evaluation dataset for trial {trials$trial_id[[i]]}."
        )
      } else if (!(estimate_col %in% names(data))) {
        cli::cli_warn(
          "Estimate column '{estimate_col}' not found in evaluation dataset for trial {trials$trial_id[[i]]}."
        )
      } else {
        truth_vec <- coerce_col(data[[truth_col]])
        est_vec <- coerce_col(data[[estimate_col]])
        levels_union <- unique(c(
          stats::na.omit(truth_vec),
          stats::na.omit(est_vec)
        ))
        if (length(levels_union) == 0) {
          cli::cli_warn(
            "Unable to determine class levels for trial {trials$trial_id[[i]]}; skipping yardstick metrics."
          )
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
              cli::cli_warn(
                "Failed to compute yardstick metrics for trial {trials$trial_id[[i]]}: {e$message}"
              )
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
