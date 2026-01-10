# Optimizer Core Infrastructure
#
# Shared infrastructure for all optimizers:
# - Evaluation runner
# - Candidate generation
# - Cost tracking
# - Reproducibility (seed support)
# - Budget/stopping conditions

#' Optimizer Control Parameters
#'
#' @description
#' S7 class for configuring optimizer behavior. Provides consistent control
#' parameters across all optimizer types.
#'
#' @param seed Random seed for reproducibility. Default is NULL (no seed).
#' @param max_trials Maximum number of trials to run. Default is NULL (unlimited).
#' @param max_errors Maximum consecutive errors before stopping. Default is 5.
#' @param num_threads Number of threads for parallel evaluation. Default is 1.
#' @param progress Whether to display progress. Default is TRUE in interactive sessions.
#' @param log_dir Directory for trial logging. Default is NULL (no logging).
#' @param verbose Whether to print detailed output. Default is FALSE
#'
#' @export
OptimizerControl <- S7::new_class(
  "OptimizerControl",
  properties = list(
    seed = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && (!is.numeric(value) || length(value) != 1)) {
          return("seed must be a single numeric value or NULL")
        }
        NULL
      }
    ),
    max_trials = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && (!is.numeric(value) || value < 1)) {
          return("max_trials must be a positive integer or NULL")
        }
        NULL
      }
    ),
    max_errors = S7::new_property(
      S7::class_integer,
      default = 5L,
      validator = function(value) {
        if (value < 0) {
          return("max_errors must be non-negative")
        }
        NULL
      }
    ),
    num_threads = S7::new_property(
      S7::class_integer,
      default = 1L,
      validator = function(value) {
        if (value < 1) {
          return("num_threads must be at least 1")
        }
        NULL
      }
    ),
    progress = S7::new_property(
      S7::class_logical,
      default = NA,
      validator = function(value) {
        if (length(value) != 1) {
          return("progress must be a single logical value")
        }
        NULL
      }
    ),
    log_dir = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is.character(value)) {
          return("log_dir must be a character string or NULL")
        }
        NULL
      }
    ),
    verbose = S7::new_property(
      S7::class_logical,
      default = FALSE
    )
  )
)

#' Create Optimizer Control
#'
#' @description
#' Convenience function to create an OptimizerControl object with defaults.
#'
#' @inheritParams OptimizerControl
#' @return An OptimizerControl object
#' @export
#'
#' @examples
#' # Default control
#' ctrl <- optimizer_control()
#'
#' # With specific settings
#' ctrl <- optimizer_control(seed = 42L, max_trials = 100L, log_dir = "logs/")
optimizer_control <- function(
  seed = NULL,
  max_trials = NULL,
  max_errors = 5L,
  num_threads = 1L,
  progress = NA,
  log_dir = NULL,
  verbose = FALSE
) {
  # Resolve progress default
  if (is.na(progress)) {
    progress <- interactive()
  }

  OptimizerControl(
    seed = seed,
    max_trials = if (!is.null(max_trials)) as.integer(max_trials) else NULL,
    max_errors = as.integer(max_errors),
    num_threads = as.integer(num_threads),
    progress = progress,
    log_dir = log_dir,
    verbose = verbose
  )
}

#' Evaluation Result
#'
#' @description
#' S7 class representing the result of evaluating a program on a dataset.
#' Contains per-example results plus aggregated summary statistics.
#'
#' @noRd
EvalResult <- S7::new_class(
  "EvalResult",
  properties = list(
    # Per-example data
    examples = S7::new_property(
      S7::class_data.frame,
      default = quote(data.frame())
    ),
    # Aggregated summary
    mean_score = S7::new_property(S7::class_numeric, default = NA_real_),
    std_error = S7::new_property(S7::class_any, default = NA_real_),
    n_evaluated = S7::new_property(S7::class_integer, default = 0L),
    n_errors = S7::new_property(S7::class_integer, default = 0L),
    # Cost tracking
    total_tokens = S7::new_property(S7::class_integer, default = 0L),
    total_cost = S7::new_property(S7::class_numeric, default = 0),
    # Timing
    total_latency_ms = S7::new_property(S7::class_numeric, default = 0),
    start_time = S7::new_property(S7::class_any, default = NULL),
    end_time = S7::new_property(S7::class_any, default = NULL),
    # Epochs support (for multi-epoch evaluation)
    epochs = S7::new_property(S7::class_integer, default = 1L),
    epoch_scores = S7::new_property(S7::class_list, default = list()),
    score_std = S7::new_property(S7::class_any, default = NA_real_),
    ci_lower = S7::new_property(S7::class_any, default = NA_real_),
    ci_upper = S7::new_property(S7::class_any, default = NA_real_)
  )
)

#' Evaluate a Program on a Dataset
#'
#' @description
#' Standard evaluation function for optimizers. Executes a module on a dataset,
#' applies a metric to each example, and returns detailed per-example results
#' plus aggregated statistics.
#'
#' This is the core evaluation function used by all optimizers. It wraps
#' [evaluate()] with enhanced output including:
#' - Per-example timing and error information
#' - Aggregated cost tracking
#' - Standard error computation
#' - Multi-epoch evaluation for statistical significance (when epochs > 1)
#'
#' @param program A DSPrrr module to evaluate.
#' @param dataset A data frame containing test examples.
#' @param metric A metric function for scoring predictions.
#' @param .llm Optional ellmer Chat object for LLM calls.
#' @param control An OptimizerControl object or NULL for defaults.
#' @param epochs Integer; number of times to repeat evaluation for statistical
#'   significance. Defaults to 1L. When > 1, computes std and confidence intervals.
#' @param ... Additional arguments passed to [evaluate()].
#'
#' @return An EvalResult object containing:
#'   - `examples`: tibble with per-example inputs, expected, predicted, score, error, latency
#'   - `mean_score`: mean score across successful evaluations
#'   - `std_error`: standard error of the mean
#'   - `n_evaluated`: number of successful evaluations
#'   - `n_errors`: number of failed evaluations
#'   - `total_tokens`: total tokens used
#'   - `total_cost`: total cost in USD
#'   - `total_latency_ms`: total time in milliseconds
#'
#'   When `epochs > 1`, additional fields:
#'   - `epochs`: number of epochs run
#'   - `epoch_scores`: list of score vectors, one per epoch
#'   - `score_std`: standard deviation of mean scores across epochs
#'   - `ci_lower`, `ci_upper`: 95% confidence interval bounds
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sig <- signature("question -> answer")
#' mod <- module(sig, type = "predict")
#'
#' dataset <- tibble::tibble(
#'   question = c("What is 2+2?", "What is 3+3?"),
#'   answer = c("4", "6")
#' )
#'
#' result <- eval_program(
#'   mod,
#'   dataset,
#'   metric = metric_exact_match(field = "answer"),
#'   .llm = ellmer::chat_openai()
#' )
#'
#' result@mean_score
#' result@examples
#' }
eval_program <- function(
  program,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  epochs = 1L,
  ...
) {
  # Validate inputs
  if (!inherits(program, "Module")) {
    cli::cli_abort("{.arg program} must be a DSPrrr Module object")
  }

  if (!is.data.frame(dataset)) {
    cli::cli_abort("{.arg dataset} must be a data frame or tibble")
  }

  if (!is.function(metric)) {
    cli::cli_abort("{.arg metric} must be a function")
  }

  # Use default control if not provided
  if (is.null(control)) {
    control <- optimizer_control()
  }

  # Handle empty dataset
  if (nrow(dataset) == 0) {
    return(EvalResult(
      examples = tibble::tibble(),
      mean_score = NA_real_,
      std_error = NA_real_,
      n_evaluated = 0L,
      n_errors = 0L
    ))
  }

  start_time <- Sys.time()

  # Clear traces before evaluation to get accurate cost for this run
  # Use copy_module to properly preserve custom class behavior (e.g., mock modules in tests)
  program_copy <- copy_module(program)

  # Run evaluation using existing evaluate() function with error handling
  eval_result <- tryCatch(
    {
      evaluate(
        program_copy,
        data = dataset,
        metric = metric,
        .llm = .llm,
        .parallel = control@num_threads > 1L,
        .progress = control@progress,
        .return_format = "structured",
        epochs = epochs,
        ...
      )
    },
    error = function(e) {
      cli::cli_warn(
        "Evaluation failed: {conditionMessage(e)}",
        class = "dsprrr_eval_error"
      )
      # Return a result structure indicating total failure
      list(
        scores = rep(NA_real_, nrow(dataset)),
        predictions = rep(NA_character_, nrow(dataset)),
        errors = rep(conditionMessage(e), nrow(dataset)),
        mean_score = NA_real_,
        n_evaluated = 0L,
        n_errors = nrow(dataset),
        epoch_scores = NULL,
        score_std = NA_real_,
        ci_95 = c(NA_real_, NA_real_)
      )
    }
  )

  end_time <- Sys.time()
  total_latency_ms <- as.numeric(difftime(
    end_time,
    start_time,
    units = "secs"
  )) *
    1000

  # Extract cost information from traces
  cost_info <- extract_cost_from_module(program_copy)

  # Build per-example tibble
  n <- nrow(dataset)
  examples <- tibble::tibble(
    row_id = seq_len(n),
    score = eval_result$scores,
    error = if (length(eval_result$errors) == n) {
      eval_result$errors
    } else {
      rep(NA_character_, n)
    },
    predicted = eval_result$predictions
  )

  # Add input columns from dataset
  input_names <- get_input_names(program$signature)
  for (name in input_names) {
    if (name %in% names(dataset)) {
      examples[[paste0("input_", name)]] <- dataset[[name]]
    }
  }

  # Compute standard error
  valid_scores <- eval_result$scores[!is.na(eval_result$scores)]
  std_error <- if (length(valid_scores) > 1) {
    stats::sd(valid_scores) / sqrt(length(valid_scores))
  } else {
    NA_real_
  }

  # Extract epoch-specific fields if present
  epoch_count <- as.integer(epochs)
  epoch_scores_list <- if (!is.null(eval_result$epoch_scores)) {
    eval_result$epoch_scores
  } else {
    list()
  }
  score_std <- if (!is.null(eval_result$score_std)) {
    eval_result$score_std
  } else {
    NA_real_
  }
  ci_values <- if (!is.null(eval_result$ci_95)) {
    eval_result$ci_95
  } else {
    c(NA_real_, NA_real_)
  }

  EvalResult(
    examples = examples,
    mean_score = eval_result$mean_score,
    std_error = std_error,
    n_evaluated = as.integer(eval_result$n_evaluated),
    n_errors = as.integer(eval_result$n_errors),
    total_tokens = as.integer(cost_info$total_tokens),
    total_cost = cost_info$total_cost,
    total_latency_ms = total_latency_ms,
    start_time = start_time,
    end_time = end_time,
    epochs = epoch_count,
    epoch_scores = epoch_scores_list,
    score_std = score_std,
    ci_lower = ci_values[1],
    ci_upper = ci_values[2]
  )
}

#' Sample from a Dataset Deterministically
#'
#' @description
#' Sample rows from a dataset with optional seed for reproducibility.
#' Used by optimizers for consistent train/validation splits and
#' demo selection.
#'
#' @param dataset A data frame to sample from.
#' @param n Number of rows to sample. If NULL or greater than nrow(dataset),
#'   returns the full dataset.
#' @param seed Random seed for reproducibility. If NULL, sampling is random.
#' @param replace Whether to sample with replacement. Default is FALSE.
#'
#' @return A data frame containing the sampled rows.
#' @export
#'
#' @examples
#' df <- tibble::tibble(x = 1:10, y = letters[1:10])
#'
#' # Deterministic sampling
#' sample1 <- sample_dataset(df, n = 5, seed = 42)
#' sample2 <- sample_dataset(df, n = 5, seed = 42)
#' identical(sample1, sample2)  # TRUE
#'
#' # Random sampling (different each time)
#' sample3 <- sample_dataset(df, n = 5)
sample_dataset <- function(dataset, n = NULL, seed = NULL, replace = FALSE) {
  if (!is.data.frame(dataset)) {
    cli::cli_abort("{.arg dataset} must be a data frame")
  }

  total_rows <- nrow(dataset)

  # Handle edge cases
  if (total_rows == 0) {
    return(dataset)
  }

  if (is.null(n) || n >= total_rows) {
    if (!replace) {
      return(dataset)
    }
  }

  # Sample with proper RNG state management
  if (!is.null(seed)) {
    # Save current RNG state
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }

    set.seed(seed)

    # Restore RNG state on exit
    on.exit(
      {
        if (is.null(old_seed)) {
          rm(".Random.seed", envir = globalenv())
        } else {
          assign(".Random.seed", old_seed, envir = globalenv())
        }
      },
      add = TRUE
    )
  }

  # Sample indices
  indices <- sample(total_rows, size = min(n, total_rows), replace = replace)

  dataset[indices, , drop = FALSE]
}

#' Split Dataset into Train and Validation Sets
#'
#' @description
#' Split a dataset into training and validation portions with optional
#' seed for reproducibility.
#'
#' @param dataset A data frame to split.
#' @param prop Proportion of data for training. Default is 0.8.
#' @param seed Random seed for reproducibility.
#'
#' @return A list with `train` and `val` data frames.
#' @export
#'
#' @examples
#' df <- tibble::tibble(x = 1:100)
#' split <- split_dataset(df, prop = 0.8, seed = 42)
#' nrow(split$train)  # ~80
#' nrow(split$val)    # ~20
split_dataset <- function(dataset, prop = 0.8, seed = NULL) {
  if (!is.data.frame(dataset)) {
    cli::cli_abort("{.arg dataset} must be a data frame")
  }

  if (prop <= 0 || prop >= 1) {
    cli::cli_abort("{.arg prop} must be between 0 and 1 (exclusive)")
  }

  n <- nrow(dataset)

  if (n == 0) {
    return(list(train = dataset, val = dataset))
  }

  # Split with proper RNG state management
  if (!is.null(seed)) {
    # Save current RNG state
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }

    set.seed(seed)

    # Restore RNG state on exit
    on.exit(
      {
        if (is.null(old_seed)) {
          rm(".Random.seed", envir = globalenv())
        } else {
          assign(".Random.seed", old_seed, envir = globalenv())
        }
      },
      add = TRUE
    )
  }

  n_train <- floor(n * prop)
  train_indices <- sample(n, n_train)
  val_indices <- setdiff(seq_len(n), train_indices)

  list(
    train = dataset[train_indices, , drop = FALSE],
    val = dataset[val_indices, , drop = FALSE]
  )
}

#' Extract Cost Information from a Module
#'
#' @description
#' Extract token usage and cost information from a module's traces.
#' Used internally by optimizers for cost tracking.
#'
#' @param module A DSPrrr module with recorded traces.
#'
#' @return A list with:
#'   - `tokens_in`: total input tokens
#'   - `tokens_out`: total output tokens
#'   - `total_tokens`: total tokens (in + out)
#'   - `total_cost`: total cost in USD (may be NA if not available)
#'
#' @noRd
extract_cost_from_module <- function(module) {
  if (!inherits(module, "Module")) {
    return(list(
      tokens_in = 0L,
      tokens_out = 0L,
      total_tokens = 0L,
      total_cost = NA_real_
    ))
  }

  summary <- module$trace_summary()

  list(
    tokens_in = as.integer(summary$total_input_tokens %||% 0),
    tokens_out = as.integer(summary$total_output_tokens %||% 0),
    total_tokens = as.integer(summary$total_tokens %||% 0),
    total_cost = summary$total_cost %||% NA_real_
  )
}

#' Cost Summary
#'
#' @description
#' S7 class for tracking cumulative costs across optimizer trials.
#'
#' @noRd
CostSummary <- S7::new_class(
  "CostSummary",
  properties = list(
    tokens_in = S7::new_property(S7::class_integer, default = 0L),
    tokens_out = S7::new_property(S7::class_integer, default = 0L),
    total_tokens = S7::new_property(S7::class_integer, default = 0L),
    total_cost = S7::new_property(S7::class_numeric, default = 0),
    n_calls = S7::new_property(S7::class_integer, default = 0L)
  )
)

#' Update Cost Summary
#'
#' @description
#' Add cost information from a module to a cumulative CostSummary.
#'
#' @param summary A CostSummary object.
#' @param module A Module with traces, or a cost list from extract_cost_from_module.
#'
#' @return Updated CostSummary object.
#' @noRd
update_cost_summary <- function(summary, module) {
  if (inherits(module, "Module")) {
    cost <- extract_cost_from_module(module)
  } else if (is.list(module)) {
    cost <- module
  } else {
    return(summary)
  }

  # Helper to safely get numeric value, treating NULL and NA as 0
  safe_num <- function(x, default = 0) {
    if (is.null(x) || length(x) == 0 || is.na(x)) default else x
  }

  CostSummary(
    tokens_in = summary@tokens_in + as.integer(safe_num(cost$tokens_in, 0L)),
    tokens_out = summary@tokens_out + as.integer(safe_num(cost$tokens_out, 0L)),
    total_tokens = summary@total_tokens +
      as.integer(safe_num(cost$total_tokens, 0L)),
    total_cost = summary@total_cost + safe_num(cost$total_cost, 0),
    n_calls = summary@n_calls + 1L
  )
}

#' Check Budget Stopping Condition
#'
#' @description
#' Check if an optimization run should stop based on budget constraints.
#'
#' @param trial_count Current number of trials completed.
#' @param error_count Current number of consecutive errors.
#' @param control OptimizerControl object with budget settings.
#'
#' @return A list with:
#'   - `should_stop`: logical indicating if optimization should stop
#'   - `reason`: character explaining why (or NULL if not stopping)
#'
#' @noRd
check_budget <- function(trial_count, error_count, control) {
  # Check max trials
  if (!is.null(control@max_trials) && !is.na(control@max_trials)) {
    if (trial_count >= control@max_trials) {
      return(list(
        should_stop = TRUE,
        reason = sprintf("Reached max_trials limit (%d)", control@max_trials)
      ))
    }
  }

  # Check max errors
  if (error_count >= control@max_errors) {
    return(list(
      should_stop = TRUE,
      reason = sprintf(
        "Reached max_errors limit (%d consecutive errors)",
        control@max_errors
      )
    ))
  }

  list(should_stop = FALSE, reason = NULL)
}

#' Generate Unique Trial ID
#'
#' @description
#' Generate a unique identifier for a trial, combining timestamp and
#' random suffix.
#'
#' @param prefix Optional prefix for the ID.
#'
#' @return A character string trial ID.
#' @noRd
generate_trial_id <- function(prefix = "trial") {
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  random_suffix <- paste0(
    sample(c(letters, 0:9), 6, replace = TRUE),
    collapse = ""
  )
  paste(prefix, timestamp, random_suffix, sep = "_")
}

# Helper to get input names from signature
get_input_names <- function(signature) {
  if (!inherits(signature, "dsprrr::Signature")) {
    return(character(0))
  }
  vapply(signature@inputs, function(x) x$name, character(1))
}

#' Print method for EvalResult
#' @param x An EvalResult object
#' @param ... Additional arguments (unused)
#' @export
print.EvalResult <- function(x, ...) {
  cli::cli_h3("Evaluation Result")

  if (is.na(x@mean_score)) {
    cli::cli_alert_warning("No successful evaluations")
  } else {
    cli::cli_alert_success("Mean Score: {round(x@mean_score, 4)}")
    if (!is.na(x@std_error)) {
      cli::cli_text("  SE: {round(x@std_error, 4)}")
    }

    # Show epoch statistics if available
    if (!is.na(x@score_std) && x@epochs > 1) {
      cli::cli_text("  SD (across epochs): {round(x@score_std, 4)}")
    }
    if (!is.na(x@ci_lower) && !is.na(x@ci_upper) && x@epochs > 1) {
      cli::cli_text(
        "  95% CI: [{round(x@ci_lower, 4)}, {round(x@ci_upper, 4)}]"
      )
    }
  }

  # Show epoch count if multi-epoch
  if (x@epochs > 1) {
    cli::cli_text("{.field Epochs}: {x@epochs}")
  }

  cli::cli_text(
    "{.field Evaluated}: {x@n_evaluated} / {x@n_evaluated + x@n_errors}"
  )
  if (x@n_errors > 0) {
    cli::cli_alert_warning("{.field Errors}: {x@n_errors}")
  }

  if (x@total_tokens > 0) {
    cli::cli_text("{.field Tokens}: {x@total_tokens}")
  }

  if (!is.na(x@total_cost) && x@total_cost > 0) {
    cli::cli_text("{.field Cost}: ${format(x@total_cost, digits = 4)}")
  }

  cli::cli_text("{.field Latency}: {round(x@total_latency_ms / 1000, 2)}s")

  invisible(x)
}

# Register S7 print method
S7::method(print, EvalResult) <- print.EvalResult
