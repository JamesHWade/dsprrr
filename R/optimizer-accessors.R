#' Optimizer Convenience Functions
#'
#' @description
#' Helper functions for inspecting, extracting, and working with
#' optimization results from compiled modules and trial logs.
#'
#' @name optimizer-accessors
NULL

#' Extract Best Parameters from a Module
#'
#' @description
#' Get the best parameter configuration from an optimized module.
#' This is the parameter set that achieved the highest (or lowest, for
#' minimization) score during optimization.
#'
#' @param module A DSPrrr module that has been optimized.
#' @param flatten Logical; if TRUE (default), return a simple named list.
#'   If FALSE, return the parameters as stored (may include nested structure).
#'
#' @return A named list of the best parameters, or NULL if the module
#'   has not been optimized.
#'
#' @export
#' @family optimizer accessors
#'
#' @examples
#' if (FALSE) {
#' mod <- module(signature("text -> sentiment"), type = "predict")
#' mod$optimize_grid(
#'   data = train_data,
#'   metric = metric_exact_match(),
#'   parameters = list(temperature = c(0.3, 0.7, 1.0))
#' )
#' best_params(mod)
#' # $temperature
#' # [1] 0.7
#' }
best_params <- function(module, flatten = TRUE) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  if (!module$is_compiled()) {
    cli::cli_warn("Module has not been optimized; no best parameters available")
    return(NULL)
  }

  params <- module$state$best_params

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


#' Extract Best Demos from a Compiled Module
#'
#' @description
#' Get the few-shot demonstration examples from a compiled module.
#' Returns the demos that were selected during optimization (e.g., by
#' LabeledFewShot or BootstrapFewShot teleprompters).
#'
#' @param module A DSPrrr module that has been compiled with demos.
#' @param as_tibble Logical; if TRUE, return demos as a tibble.
#'   If FALSE (default), return as a list.
#'
#' @return A list or tibble of demonstration examples, or NULL if
#'   the module has no demos.
#'
#' @export
#' @family optimizer accessors
#'
#' @examples
#' if (FALSE) {
#' tp <- LabeledFewShot(k = 4L)
#' compiled <- compile(tp, mod, trainset)
#' demos <- best_demos(compiled)
#' }
best_demos <- function(module, as_tibble = FALSE) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  # PredictModule stores demos in $demos field, fall back to config$demos
  demos <- module$demos %||% module$config$demos

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


#' Apply Best Configuration from One Module to Another
#'
#' @description
#' Copy the optimized configuration (best parameters, demos, etc.) from
#' a compiled module to a new or existing module. Useful for transferring
#' optimization results to a fresh module instance.
#'
#' @param source A compiled DSPrrr module with optimization results.
#' @param target A DSPrrr module to apply the configuration to.
#'   If NULL, a copy of the source module is created.
#' @param include Character vector specifying what to copy:
#'   - "params": Best parameter values (temperature, etc.)
#'   - "demos": Few-shot demonstration examples
#'   - "all": Both params and demos (default)
#'
#' @return The target module with the applied configuration (modified in place
#'   if target was provided, otherwise a new module).
#'
#' @export
#' @family optimizer accessors
#'
#' @examples
#' if (FALSE) {
#' # Transfer optimization from one module to another
#' optimized <- mod$optimize_grid(data, metric, parameters)
#' new_mod <- module(signature, type = "predict")
#' apply_best_config(optimized, new_mod)
#'
#' # Create a fresh copy with the optimized config
#' fresh <- apply_best_config(optimized, target = NULL)
#' }
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
      # PredictModule uses $demos field, other modules use config$demos
      if ("demos" %in% names(target)) {
        target$demos <- demos
      } else {
        target$config$demos <- demos
      }
    }
  }

  # Mark as compiled if source was compiled
  if (source$is_compiled()) {
    target$state$compiled <- TRUE
    target$state$best_score <- source$state$best_score
  }

  invisible(target)
}


#' Get Top Performing Trials
#'
#' @description
#' Extract the top k trials from a module's optimization history or
#' a TrialLog, ranked by score.
#'
#' @param x A DSPrrr module with optimization trials, or a TrialLog object.
#' @param k Integer; number of top trials to return. Default is 5.
#' @param objective Optimization direction: "maximize" (default) or "minimize".
#'
#' @return A tibble with the top k trials, including trial_id, score,
#'   parameters, and other trial metadata.
#'
#' @export
#' @family optimizer accessors
#'
#' @examples
#' if (FALSE) {
#' # Get top 3 trials from module
#' top_trials(mod, k = 3)
#'
#' # Get top trials from a TrialLog
#' log <- load_trial_log("path/to/logs")
#' top_trials(log, k = 10, objective = "minimize")
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

  trials <- x$state$trials

  if (!is.data.frame(trials) || nrow(trials) == 0) {
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


#' Compare Module Configuration Before and After Optimization
#'
#' @description
#' Show what configuration values changed during optimization.
#' Useful for understanding the effect of optimization on module settings.
#'
#' @param module A DSPrrr module (preferably compiled).
#' @param baseline Optional named list of baseline configuration values
#'   to compare against. If NULL, uses reasonable defaults.
#'
#' @return A tibble with columns:
#'   - `parameter`: Parameter name
#'   - `before`: Value before optimization (or default)
#'   - `after`: Current value
#'   - `changed`: Logical indicating if value changed
#'
#' @export
#' @family optimizer accessors
#'
#' @examples
#' if (FALSE) {
#' mod <- module(signature("text -> sentiment"), type = "predict")
#' mod$optimize_grid(data, metric, parameters = list(temperature = c(0.3, 1.0)))
#' config_diff(mod)
#' }
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


#' Export Module Configuration as R Code
#'
#' @description
#' Generate R code that recreates a module with its current configuration.
#' Useful for documenting optimized configurations and ensuring reproducibility.
#'
#' @param module A DSPrrr module to export.
#' @param name Character; variable name for the module in generated code.
#'   Default is "mod".
#' @param include_demos Logical; whether to include demonstration examples
#'   in the generated code. Default is TRUE.
#' @param file Optional file path to write the code to. If NULL (default),
#'   returns the code as a character string.
#'
#' @return If `file` is NULL, returns the R code as a character string
#'   (invisibly). If `file` is specified, writes to file and returns invisibly.
#'
#' @export
#' @family optimizer accessors
#'
#' @examples
#' if (FALSE) {
#' mod <- module(signature("text -> sentiment"), type = "predict")
#' mod$optimize_grid(data, metric, parameters = list(temperature = c(0.3, 1.0)))
#'
#' # Get code as string
#' code <- export_module_code(mod)
#' cat(code)
#'
#' # Write to file
#' export_module_code(mod, file = "optimized_module.R")
#' }
export_module_code <- function(
  module,
  name = "mod",
  include_demos = TRUE,
  file = NULL
) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  lines <- character()

  # Header comment
  lines <- c(lines, "# DSPrrr Module Configuration")
  lines <- c(
    lines,
    paste0(
      "# Generated: ",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
  if (module$is_compiled()) {
    lines <- c(
      lines,
      paste0(
        "# Best score: ",
        round(module$state$best_score %||% NA, 4)
      )
    )
  }
  lines <- c(lines, "")

  # Signature
  sig <- module$signature
  input_names <- vapply(sig@inputs, function(x) x$name, character(1))

  if (length(input_names) > 0) {
    sig_str <- paste0(
      paste(input_names, collapse = ", "),
      " -> output"
    )
  } else {
    sig_str <- "input -> output"
  }

  lines <- c(lines, "# Create signature")
  if (nzchar(sig@instructions)) {
    lines <- c(
      lines,
      paste0(
        "sig <- signature(\"",
        sig_str,
        "\","
      )
    )
    # Format instructions nicely
    instructions_escaped <- gsub("\"", "\\\\\"", sig@instructions)
    instructions_escaped <- gsub("\n", "\\\\n", instructions_escaped)
    lines <- c(
      lines,
      paste0(
        "  instructions = \"",
        instructions_escaped,
        "\""
      )
    )
    lines <- c(lines, ")")
  } else {
    lines <- c(lines, paste0("sig <- signature(\"", sig_str, "\")"))
  }
  lines <- c(lines, "")

  # Module creation
  lines <- c(lines, "# Create module")
  lines <- c(lines, paste0(name, " <- module(sig, type = \"predict\")"))
  lines <- c(lines, "")

  # Apply configuration
  config <- module$config
  config_params <- config[
    !names(config) %in% c("demos", "compiled", "teleprompter")
  ]

  if (length(config_params) > 0) {
    lines <- c(lines, "# Apply optimized configuration")
    for (param_name in names(config_params)) {
      val <- config_params[[param_name]]
      if (!is.null(val) && length(val) > 0) {
        val_str <- format_value_for_code(val)
        lines <- c(lines, paste0(name, "$config$", param_name, " <- ", val_str))
      }
    }
    lines <- c(lines, "")
  }

  # Demos - PredictModule uses $demos field
  demos <- module$demos %||% module$config$demos
  if (include_demos && !is.null(demos) && length(demos) > 0) {
    lines <- c(lines, "# Few-shot demonstrations")
    lines <- c(lines, paste0(name, "$demos <- list("))
    for (i in seq_along(demos)) {
      demo <- demos[[i]]
      demo_parts <- vapply(
        names(demo),
        function(n) {
          paste0(n, " = ", format_value_for_code(demo[[n]]))
        },
        character(1)
      )
      demo_str <- paste0("  list(", paste(demo_parts, collapse = ", "), ")")
      if (i < length(demos)) {
        demo_str <- paste0(demo_str, ",")
      }
      lines <- c(lines, demo_str)
    }
    lines <- c(lines, ")")
    lines <- c(lines, "")
  }

  # Mark as compiled
  if (module$is_compiled()) {
    lines <- c(lines, "# Mark as compiled")
    lines <- c(lines, paste0(name, "$state$compiled <- TRUE"))
    if (!is.null(module$state$best_score)) {
      lines <- c(
        lines,
        paste0(
          name,
          "$state$best_score <- ",
          module$state$best_score
        )
      )
    }
  }

  code <- paste(lines, collapse = "\n")

  if (!is.null(file)) {
    writeLines(code, file)
    cli::cli_inform("Module code written to {.file {file}}")
    invisible(code)
  } else {
    code
  }
}


#' Get Optimization Summary
#'
#' @description
#' Get a concise summary of optimization results for a module.
#' Combines information from trials, best parameters, and cost tracking.
#'
#' @param module A DSPrrr module with optimization history.
#'
#' @return A list with:
#'   - `n_trials`: Number of trials evaluated
#'   - `best_score`: Best score achieved
#'   - `best_trial`: ID of the best trial
#'   - `best_params`: Best parameter configuration
#'   - `score_range`: Min and max scores across trials
#'   - `total_cost`: Total cost of optimization (if tracked)
#'   - `improvement`: Score improvement from first to best trial
#'
#' @export
#' @family optimizer accessors
#'
#' @examples
#' if (FALSE) {
#' mod$optimize_grid(data, metric, parameters)
#' summary <- optimization_summary(mod)
#' print(summary)
#' }
optimization_summary <- function(module) {
  if (!inherits(module, "Module")) {
    cli::cli_abort("{.arg module} must be a DSPrrr Module object")
  }

  trials <- module$state$trials

  if (!is.data.frame(trials) || nrow(trials) == 0) {
    return(structure(
      list(
        n_trials = 0L,
        best_score = NA_real_,
        best_trial = NA_integer_,
        best_params = NULL,
        score_range = c(NA_real_, NA_real_),
        total_cost = NA_real_,
        improvement = NA_real_,
        compiled = FALSE
      ),
      class = "dsprrr_optimization_summary"
    ))
  }

  scores <- trials$score
  valid_scores <- scores[!is.na(scores)]

  # Calculate improvement (first valid score to best)
  first_score <- valid_scores[1]
  best_score <- module$state$best_score %||% max(valid_scores, na.rm = TRUE)
  improvement <- if (!is.na(first_score) && !is.na(best_score)) {
    best_score - first_score
  } else {
    NA_real_
  }

  # Try to get total cost from evaluations
  total_cost <- tryCatch(
    {
      costs <- vapply(
        trials$evaluation,
        function(e) {
          if (is.list(e) && "total_cost" %in% names(e)) {
            e$total_cost %||% 0
          } else {
            0
          }
        },
        numeric(1)
      )
      sum(costs, na.rm = TRUE)
    },
    error = function(e) NA_real_
  )

  structure(
    list(
      n_trials = nrow(trials),
      best_score = best_score,
      best_trial = module$state$best_trial,
      best_params = module$state$best_params,
      score_range = range(valid_scores, na.rm = TRUE),
      total_cost = total_cost,
      improvement = improvement,
      compiled = module$is_compiled()
    ),
    class = "dsprrr_optimization_summary"
  )
}


#' Print method for optimization summary
#' @param x An optimization summary object
#' @param ... Additional arguments (unused)
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


# ---- Helper Functions ----

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
