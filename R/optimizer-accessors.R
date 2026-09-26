#' Helpers for optimization results
#'
#' Functions for inspecting and reusing optimization results. Each function is
#' documented on its own page.
#'
#' @name optimizer-accessors
#' @noRd
NULL

#' Best parameters found by an optimizer
#'
#' @description
#' `best_params()` returns the winning parameter values recorded by [compile()]
#' or [optimize_grid()], such as the best `temperature` of a grid search or the
#' number of demonstrations chosen by [LabeledFewShot()]. It reads
#' `optimization_result(module)$best_params`.
#'
#' @param module A module returned by [compile()] or modified by
#'   [optimize_grid()].
#' @param flatten If `TRUE` (the default), length-one list elements are
#'   unwrapped to plain values. If `FALSE`, the parameters are returned as
#'   stored.
#'
#' @return A named list of parameters. For a module that has not been
#'   optimized, `NULL` with a warning.
#'
#' @export
#' @family optimization results
#'
#' @examples
#' classifier <- module(signature("text -> sentiment"))
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "It's okay"),
#'   sentiment = c("positive", "negative", "neutral")
#' )
#' compiled <- compile(classifier, LabeledFewShot(k = 2L), trainset)
#' best_params(compiled)
best_params <- function(module, flatten = TRUE) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  if (!module$is_compiled()) {
    cli::cli_warn("Module has not been optimized; no best parameters available")
    return(NULL)
  }

  result <- optimization_result(module)
  params <- result$best_params

  if (is.null(params)) {
    return(NULL)
  }

  if (flatten && is.list(params)) {
    # Flatten any data.frame-like list elements to single values
    params <- lapply(params, function(x) {
      if (length(x) == 1) x[[1]] else x
    })
  }

  params
}


#' Demonstrations attached to a module
#'
#' @description
#' `best_demos()` returns the few-shot demonstrations a module currently
#' carries, such as those chosen by [LabeledFewShot()] or [BootstrapFewShot()].
#'
#' @param module A module.
#' @param as_tibble If `TRUE`, return a tibble with one row per demonstration
#'   and one column per input and output field. If `FALSE` (the default),
#'   return the list of demonstrations as stored.
#'
#' @return A list or tibble of demonstrations, or `NULL` when the module has
#'   none.
#'
#' @export
#' @family optimization results
#'
#' @examples
#' classifier <- module(signature("text -> sentiment"))
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "It's okay"),
#'   sentiment = c("positive", "negative", "neutral")
#' )
#' compiled <- compile(classifier, LabeledFewShot(k = 2L), trainset)
#' best_demos(compiled, as_tibble = TRUE)
#'
#' # An uncompiled module has none
#' best_demos(classifier)
best_demos <- function(module, as_tibble = FALSE) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  if (!"demos" %in% names(module)) {
    return(NULL)
  }
  demos <- module$demos

  if (is.null(demos) || length(demos) == 0) {
    return(NULL)
  }

  if (as_tibble) {
    # Convert list of demos to tibble
    if (is.list(demos) && !is.data.frame(demos)) {
      # Each demo is typically a list with input/output fields
      tibble::as_tibble(do.call(rbind, lapply(demos, as.data.frame)))
    } else if (is.data.frame(demos)) {
      tibble::as_tibble(demos)
    } else {
      tibble::tibble(demo = demos)
    }
  } else {
    demos
  }
}


#' Copy optimized settings to another module
#'
#' @description
#' `apply_best_config()` copies the best parameters, the demonstrations and
#' the optimization result from an optimized module to another module, for
#' example to reuse a compiled configuration on a module with a different
#' chat.
#'
#' @param source An optimized module, returned by [compile()] or modified by
#'   [optimize_grid()].
#' @param target The module to update. It is modified in place. If `NULL`, a
#'   fresh copy of `source` is created and updated.
#' @param include What to copy: `"all"` (the default), `"params"` (the best
#'   parameters, such as `temperature`) or `"demos"` (the demonstrations).
#'
#' @return The updated target module, invisibly.
#'
#' @export
#' @family optimization results
#'
#' @examples
#' classifier <- module(signature("text -> sentiment"))
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "It's okay"),
#'   sentiment = c("positive", "negative", "neutral")
#' )
#' compiled <- compile(classifier, LabeledFewShot(k = 2L), trainset)
#'
#' fresh <- module(signature("text -> sentiment"))
#' apply_best_config(compiled, fresh, include = "demos")
#' length(fresh$demos)
apply_best_config <- function(
  source,
  target = NULL,
  include = c("all", "params", "demos")
) {
  if (!inherits(source, "Module")) {
    cli::cli_abort("{.arg source} must be a DSPrrr Module object")
  }

  include <- match.arg(include)

  if (is.null(target)) {
    # Create a fresh copy
    target <- source$copy(deep = TRUE)
    target$reset()
  } else if (!inherits(target, "Module")) {
    cli::cli_abort("{.arg target} must be a DSPrrr Module object or NULL")
  }

  # Apply best parameters
  if (include %in% c("all", "params")) {
    params <- best_params(source, flatten = TRUE)
    if (!is.null(params)) {
      for (name in names(params)) {
        target$config[[name]] <- params[[name]]
      }
      # Apply via hook if available
      if (is.function(target$apply_optimization_params)) {
        target$apply_optimization_params(params)
      }
    }
  }

  # Apply demos
  if (include %in% c("all", "demos")) {
    demos <- best_demos(source)
    if (!is.null(demos)) {
      if (!"demos" %in% names(target)) {
        cli::cli_abort(
          c(
            "{.arg target} does not support demonstrations",
            "i" = "Use a Predict module when transferring {.val demos}."
          ),
          class = "dsprrr_demo_transfer_error"
        )
      }
      target$demos <- demos
    }
  }

  # Carry the complete result contract when the source was optimized.
  source_result <- source$state$optimization_result
  if (!is.null(source_result)) {
    set_optimization_result(
      target,
      rlang::duplicate(source_result, shallow = FALSE)
    )
  }

  invisible(target)
}


#' Highest-scoring optimization trials
#'
#' @description
#' `top_trials()` returns the `k` best trials of an optimized module, taken
#' from `optimization_result(x)$trials`, or of a [TrialLog].
#'
#' @param x A module returned by [compile()] or modified by [optimize_grid()],
#'   or a [TrialLog].
#' @param k Integer number of trials to return (default `5L`).
#' @param objective `"maximize"` (the default) sorts the highest scores first;
#'   `"minimize"` sorts the lowest first.
#'
#' @return A tibble with the top `k` trials. Modules are sorted by `score`
#'   (or `mean_score`), trial logs by `mean_score`. When there are no trials,
#'   a warning and an empty tibble.
#'
#' @export
#' @family optimization results
#'
#' @examples
#' \dontrun{
#' classifier <- module(signature("text -> sentiment"))
#' optimize_grid(
#'   classifier,
#'   data = devset,
#'   metric = metric_exact_match(field = "sentiment"),
#'   grid = data.frame(reasoning_effort = c("none", "low", "medium")),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' top_trials(classifier, k = 2L)
#'
#' # Trials saved by an optimizer's `log_dir`
#' top_trials(load_trial_log("logs/my-run"), k = 10L)
#' }
top_trials <- function(x, k = 5L, objective = c("maximize", "minimize")) {
  UseMethod("top_trials")
}

#' @export
top_trials.Module <- function(
  x,
  k = 5L,
  objective = c("maximize", "minimize")
) {
  objective <- match.arg(objective)
  k <- as.integer(k)

  result <- optimization_result(x)
  trials <- result$trials %||% tibble::tibble()
  if (!"score" %in% names(trials) && "mean_score" %in% names(trials)) {
    trials$score <- trials$mean_score
  }

  if (
    !is.data.frame(trials) || nrow(trials) == 0 || !"score" %in% names(trials)
  ) {
    cli::cli_warn("Module has no optimization trials")
    return(tibble::tibble(
      trial_id = integer(),
      score = numeric(),
      parameters = list()
    ))
  }

  # Sort by score
  if (objective == "maximize") {
    trials <- trials[order(trials$score, decreasing = TRUE, na.last = TRUE), ]
  } else {
    trials <- trials[order(trials$score, decreasing = FALSE, na.last = TRUE), ]
  }

  # Take top k
  n <- min(k, nrow(trials))
  trials[seq_len(n), ]
}

#' @export
top_trials.TrialLog <- function(
  x,
  k = 5L,
  objective = c("maximize", "minimize")
) {
  objective <- match.arg(objective)
  k <- as.integer(k)

  trials_tbl <- x$as_tibble()

  if (nrow(trials_tbl) == 0) {
    cli::cli_warn("TrialLog has no trials")
    return(trials_tbl)
  }

  # Sort by mean_score
  if (objective == "maximize") {
    trials_tbl <- trials_tbl[
      order(trials_tbl$mean_score, decreasing = TRUE, na.last = TRUE),
    ]
  } else {
    trials_tbl <- trials_tbl[
      order(trials_tbl$mean_score, decreasing = FALSE, na.last = TRUE),
    ]
  }

  # Take top k
  n <- min(k, nrow(trials_tbl))
  trials_tbl[seq_len(n), ]
}


#' Compare a module's settings with baseline values
#'
#' @description
#' `config_diff()` lists the values in `module$config` next to baseline values,
#' changed rows first. Use it to see which settings an optimizer changed.
#'
#' @details
#' The default baseline assumes provider defaults: `temperature = 1`,
#' `top_p = 1`, `frequency_penalty = 0` and `presence_penalty = 0`. A value
#' the module never set is shown as `"<default>"` and counts as changed, as do
#' internal fields such as `.module_kind`, so pass a `baseline` that matches
#' your starting configuration for a meaningful comparison.
#'
#' @param module A module.
#' @param baseline Optional named list of baseline values; it overrides the
#'   default baseline entry by entry.
#'
#' @return A tibble with columns `parameter`, `before` and `after` (values
#'   formatted as text) and `changed` (logical).
#'
#' @export
#' @family optimization results
#'
#' @examples
#' mod <- module(signature("text -> sentiment"), config = list(temperature = 0))
#' config_diff(mod, baseline = list(temperature = 0.7))
config_diff <- function(module, baseline = NULL) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  # Default baseline values for common parameters
  default_baseline <- list(
    temperature = 1.0,
    top_p = 1.0,
    frequency_penalty = 0,
    presence_penalty = 0
  )

  if (!is.null(baseline)) {
    baseline <- utils::modifyList(default_baseline, baseline)
  } else {
    baseline <- default_baseline
  }

  current <- module$config
  all_params <- unique(c(names(baseline), names(current)))

  # Filter to only relevant parameters
  relevant_params <- all_params[
    !all_params %in%
      c(
        "demos",
        "compiled",
        "teleprompter"
      )
  ]

  # Return empty tibble if no relevant parameters

  if (length(relevant_params) == 0) {
    return(tibble::tibble(
      parameter = character(),
      before = character(),
      after = character(),
      changed = logical()
    ))
  }

  rows <- lapply(relevant_params, function(param) {
    before <- baseline[[param]]
    after <- current[[param]]

    # Handle NULL values
    before_str <- if (is.null(before)) "<default>" else format_value(before)
    after_str <- if (is.null(after)) "<default>" else format_value(after)

    changed <- !identical(before, after)

    tibble::tibble(
      parameter = param,
      before = before_str,
      after = after_str,
      changed = changed
    )
  })

  result <- do.call(rbind, rows)

  # Sort changed items first
  result[order(!result$changed), ]
}


#' Export a program as standalone R code
#'
#' @description
#' `export_module_code()` writes R code that rebuilds a program: the complete
#' program artifact (see [program_artifact()]) followed by a call to
#' [restore_module_config()]. Nested programs and exact output schemas are
#' preserved.
#'
#' @param module A module or composed program.
#' @param name Variable name for the program in the generated code (default
#'   `"mod"`).
#' @param include_demos Whether to include the demonstrations (default
#'   `TRUE`).
#' @param file Optional path. When given, the code is written there; an
#'   existing file is replaced only after the new code parses.
#' @param registry,trusted As in [program_artifact()]. Standalone code cannot
#'   embed registry or trusted runtime references, so programs that need them
#'   are rejected; use [save_program()] for those.
#'
#' @return The code as a single string, invisibly when `file` is given.
#'
#' @export
#' @family optimization results
#' @family persistence
#'
#' @examples
#' mod <- module(signature("text -> sentiment"))
#' path <- tempfile(fileext = ".R")
#' export_module_code(mod, name = "sentiment_mod", file = path)
#'
#' # Running the file rebuilds the program
#' source(path)
#' sentiment_mod
export_module_code <- function(
  module,
  name = "mod",
  include_demos = TRUE,
  file = NULL,
  registry = list(),
  trusted = FALSE
) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }
  if (
    !is.character(name) ||
      length(name) != 1L ||
      is.na(name) ||
      !nzchar(name) ||
      !identical(make.names(name), name)
  ) {
    cli::cli_abort("{.arg name} must be one syntactic R name")
  }
  if (
    !is.logical(include_demos) ||
      length(include_demos) != 1L ||
      is.na(include_demos)
  ) {
    cli::cli_abort("{.arg include_demos} must be TRUE or FALSE")
  }

  artifact <- program_artifact(
    module,
    registry = registry,
    trusted = trusted
  )
  if (!include_demos) {
    artifact <- artifact_strip_demos(artifact)
  }
  runtime_kinds <- artifact_runtime_kinds(artifact)
  unsupported <- intersect(runtime_kinds, c("registry", "trusted"))
  if (length(unsupported) > 0L) {
    cli::cli_abort(
      c(
        "Standalone R code cannot embed this program's runtime dependencies",
        "x" = "Artifact contains {.val {unsupported}} runtime reference{?s}.",
        "i" = "Use {.fn save_program} with a registry-backed deployment contract instead."
      ),
      class = "dsprrr_artifact_code_export_unsupported"
    )
  }

  dump <- utils::capture.output(dput(artifact))
  lines <- c(
    "# dsprrr program artifact",
    paste0("# Format version: ", artifact$format_version),
    paste0(name, " <- local({"),
    "  artifact <-",
    paste0("  ", dump),
    "  dsprrr::restore_module_config(artifact)",
    "})"
  )
  code <- paste(lines, collapse = "\n")

  if (!is.null(file)) {
    artifact_atomic_write_lines(lines, file)
    cli::cli_inform("Module code written to {.file {file}}")
    invisible(code)
  } else {
    code
  }
}


#' Summarize an optimization result
#'
#' @description
#' `optimization_summary()` condenses [optimization_result()] into the numbers
#' most often reported: the number of trials, the best score and parameters,
#' the score range, the total cost and the improvement over the baseline.
#'
#' @param module A module returned by [compile()] or modified by
#'   [optimize_grid()].
#' @param x A `dsprrr_optimization_summary` object.
#' @param ... Unused.
#'
#' @return A `dsprrr_optimization_summary` list with `n_trials`,
#'   `best_score`, `best_trial`, `best_params`, `score_range` (minimum and
#'   maximum trial scores), `total_cost` (when trials record it),
#'   `improvement` (best score minus the baseline or first score) and
#'   `compiled`. `print()` shows it and returns it invisibly.
#'
#' @export
#' @family optimization results
#'
#' @examples
#' classifier <- module(signature("text -> sentiment"))
#' trainset <- data.frame(
#'   text = c("I love it!", "Terrible experience", "It's okay"),
#'   sentiment = c("positive", "negative", "neutral")
#' )
#' compiled <- compile(classifier, LabeledFewShot(k = 2L), trainset)
#' optimization_summary(compiled)$best_params
#'
#' \dontrun{
#' # After a search with several trials, printing gives an overview
#' optimize_grid(
#'   classifier,
#'   data = trainset,
#'   metric = metric_exact_match(field = "sentiment"),
#'   grid = data.frame(reasoning_effort = c("none", "low")),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' optimization_summary(classifier)
#' }
optimization_summary <- function(module) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  result <- optimization_result(module)
  trials <- result$trials %||% tibble::tibble()

  if (!is.data.frame(trials) || nrow(trials) == 0) {
    return(structure(
      list(
        n_trials = 0L,
        best_score = result$best_score %||% NA_real_,
        best_trial = result$best_trial %||% NA_integer_,
        best_params = result$best_params %||% list(),
        score_range = c(NA_real_, NA_real_),
        total_cost = NA_real_,
        improvement = NA_real_,
        compiled = !is.null(result)
      ),
      class = "dsprrr_optimization_summary"
    ))
  }

  scores <- if ("score" %in% names(trials)) {
    trials$score
  } else if ("mean_score" %in% names(trials)) {
    trials$mean_score
  } else {
    rep(NA_real_, nrow(trials))
  }
  valid_scores <- scores[!is.na(scores)]

  first_score <- result$baseline_score
  if (is.na(first_score) && length(valid_scores) > 0L) {
    first_score <- valid_scores[[1]]
  }
  best_score <- result$best_score
  improvement <- if (!is.na(first_score) && !is.na(best_score)) {
    best_score - first_score
  } else {
    NA_real_
  }

  total_cost <- if ("total_cost" %in% names(trials)) {
    tryCatch(sum_cost_values(trials$total_cost), error = function(e) NA_real_)
  } else {
    NA_real_
  }
  score_range <- if (length(valid_scores) > 0L) {
    range(valid_scores)
  } else {
    c(NA_real_, NA_real_)
  }

  structure(
    list(
      n_trials = nrow(trials),
      best_score = best_score,
      best_trial = result$best_trial,
      best_params = result$best_params,
      score_range = score_range,
      total_cost = total_cost,
      improvement = improvement,
      compiled = !is.null(result)
    ),
    class = "dsprrr_optimization_summary"
  )
}

#' @rdname optimization_summary
#' @export
print.dsprrr_optimization_summary <- function(x, ...) {
  cli::cli_h3("Optimization Summary")

  if (x$n_trials == 0) {
    cli::cli_alert_info("No optimization trials recorded")
    return(invisible(x))
  }

  status_icon <- if (x$compiled) cli::symbol$tick else cli::symbol$cross
  cli::cli_text("{status_icon} Compiled: {.val {x$compiled}}")
  cli::cli_text("{.field Trials}: {x$n_trials}")
  cli::cli_text(
    "{.field Best Score}: {round(x$best_score, 4)} (trial {x$best_trial})"
  )
  cli::cli_text(
    "{.field Score Range}: [{round(x$score_range[1], 4)}, {round(x$score_range[2], 4)}]"
  )

  if (!is.na(x$improvement) && x$improvement != 0) {
    direction <- if (x$improvement > 0) "+" else ""
    cli::cli_text(
      "{.field Improvement}: {direction}{round(x$improvement, 4)}"
    )
  }
  if (!is.na(x$total_cost) && x$total_cost > 0) {
    cli::cli_text("{.field Total Cost}: ${format(x$total_cost, digits = 4)}")
  }
  if (!is.null(x$best_params) && length(x$best_params) > 0) {
    cli::cli_text("{.field Best Parameters}:")
    for (name in names(x$best_params)) {
      val <- x$best_params[[name]]
      if (length(val) == 1) {
        cli::cli_text("    {name}: {.val {val}}")
      }
    }
  }
  invisible(x)
}

#' Format a value for display
#' @noRd
format_value <- function(x) {
  if (is.null(x)) {
    return("<null>")
  }
  if (is.list(x) && !is.data.frame(x)) {
    if (length(x) == 1) {
      return(format_value(x[[1]]))
    }
    return(paste0("[", length(x), " items]"))
  }
  if (is.character(x) && length(x) == 1 && nchar(x) > 50) {
    return(paste0(substr(x, 1, 47), "..."))
  }
  if (is.numeric(x) && length(x) == 1) {
    return(format(round(x, 4), nsmall = if (x %% 1 == 0) 0 else 2))
  }
  as.character(x)
}

#' Format a value for R code generation
#' @noRd
format_value_for_code <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.character(x)) {
    escaped <- gsub("\"", "\\\\\"", x)
    escaped <- gsub("\n", "\\\\n", escaped)
    return(paste0("\"", escaped, "\""))
  }
  if (is.numeric(x)) {
    return(as.character(x))
  }
  if (is.logical(x)) {
    return(if (x) "TRUE" else "FALSE")
  }
  if (is.list(x)) {
    items <- vapply(
      names(x),
      function(n) {
        paste0(n, " = ", format_value_for_code(x[[n]]))
      },
      character(1)
    )
    return(paste0("list(", paste(items, collapse = ", "), ")"))
  }
  deparse(x)
}
