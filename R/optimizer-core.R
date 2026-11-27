# Shared optimizer controls, evaluation result, and dataset helpers.

optimizer_limit_validator <- function(
  value,
  name,
  integer = FALSE,
  positive = FALSE
) {
  if (is.null(value)) {
    return(NULL)
  }
  if (
    !is.numeric(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      value < (if (positive) 1 else 0) ||
      (integer && value != as.integer(value))
  ) {
    qualifier <- if (positive) "positive" else "non-negative"
    kind <- if (integer) "integer" else "number"
    return(sprintf("%s must be a %s %s or NULL", name, qualifier, kind))
  }
  NULL
}

#' Internal optimizer-control record class
#' @noRd
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
    max_metric_calls = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_metric_calls", integer = TRUE)
      }
    ),
    max_provider_calls = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_provider_calls", integer = TRUE)
      }
    ),
    max_input_tokens = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_input_tokens", integer = TRUE)
      }
    ),
    max_output_tokens = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_output_tokens", integer = TRUE)
      }
    ),
    max_total_tokens = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_total_tokens", integer = TRUE)
      }
    ),
    max_cost = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) optimizer_limit_validator(value, "max_cost")
    ),
    max_elapsed_seconds = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        optimizer_limit_validator(value, "max_elapsed_seconds")
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
    checkpoint_path = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (
          !is.null(value) &&
            (!is.character(value) ||
              length(value) != 1L ||
              is.na(value) ||
              !nzchar(value))
        ) {
          return("checkpoint_path must be one non-empty string or NULL")
        }
        NULL
      }
    ),
    resume = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = function(value) {
        if (length(value) != 1L || is.na(value)) {
          return("resume must be TRUE or FALSE")
        }
        NULL
      }
    ),
    checkpoint_registry = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        if (
          length(value) > 0L &&
            (is.null(names(value)) ||
              anyNA(names(value)) ||
              !all(nzchar(names(value))) ||
              anyDuplicated(names(value)))
        ) {
          return("checkpoint_registry must have unique, non-empty names")
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

#' Budgets and settings for an optimizer run
#'
#' @description
#' `optimizer_control()` collects the limits and options for one optimizer
#' run: error, trial, call, token, cost and time budgets, concurrency, trial
#' logging and checkpoints. Pass it to [compile()] as `control`. It replaces
#' the control that the optimizer would otherwise build from its own
#' `max_errors`, `num_threads` and `log_dir` settings.
#'
#' [BootstrapFewShot()], [BootstrapFewShotWithRandomSearch()], [COPRO()],
#' [MIPROv2()], [SIMBA()], [GEPA()], [AutoResearch()] and [MetaHarness()] use
#' `control`; the other optimizers ignore it.
#'
#' @details
#' When a budget stops a run, the optimizer returns the best program found so
#' far and marks its [optimization_result()] as `"partial"`.
#'
#' Finite metric, provider, token, cost and elapsed-time limits make the
#' optimizer evaluate one row at a time, so a run overshoots a limit by at most
#' one evaluation row that had already started (or one direct provider request
#' made by the optimizer itself). When a cap is finite and a provider does not
#' report its usage, tokens or cost, the run stops rather than guess.
#'
#' [BootstrapFewShot()] and [MIPROv2()] can resume from a checkpoint.
#' [GEPA()] supports resume for sequential `round_robin` searches over plain
#' `Predict` leaves and built-in predictor pipelines when each prediction is
#' plain data. It rejects Flex modules, other selectors, custom module types or
#' Chats, Chats with tools, and Chats with existing turns. GEPA stores each
#' completed proposal and evaluation row. A provider request that finishes
#' after the previous checkpoint was written but before its result is committed
#' can be retried after a crash; GEPA does not guarantee exactly-once provider
#' effects. [SIMBA()] and [COPRO()] respect budgets and return the best partial
#' program, but reject `resume = TRUE`.
#'
#' @param seed Random seed, or `NULL` (the default). Currently only
#'   [BootstrapFewShotWithRandomSearch()] reads it, in place of its own `seed`;
#'   the other optimizers use their own `seed` argument.
#' @param max_trials Maximum number of trials (candidate evaluations), or
#'   `NULL` (the default) for no limit.
#' @param max_errors Non-negative integer (default `5L`). The run stops once
#'   this many evaluations have failed in a row; each success resets the
#'   count, while the total number of errors is still reported. With `0L`, the
#'   first failure stops the run. Outcomes of an evaluation that had already
#'   started are all counted.
#' @param max_metric_calls Maximum number of metric calls, or `NULL` for no
#'   limit.
#' @param max_provider_calls Maximum number of verified provider calls, or
#'   `NULL` for no limit.
#' @param max_input_tokens,max_output_tokens,max_total_tokens Maximum verified
#'   input, output, or input plus output tokens, or `NULL` for no limit.
#' @param max_cost Maximum known provider cost in US dollars, or `NULL` for no
#'   limit.
#' @param max_elapsed_seconds Maximum active run time in seconds, or `NULL` for
#'   no limit. Time between a checkpoint and its resume is not counted.
#' @param num_threads Integer number of rows evaluated at the same time
#'   (default `1L`).
#' @param progress Whether to show progress bars. `NA` (the default) means
#'   `interactive()`.
#' @param log_dir Directory for a [TrialLog] of the run, or `NULL` (the
#'   default). [BootstrapFewShot()] ignores it and uses its own `log_dir`.
#' @param checkpoint_path Optional file for optimizer checkpoints.
#' @param resume Whether to resume from `checkpoint_path` (default `FALSE`).
#' @param checkpoint_registry Named runtime registry used to save and restore
#'   the programs stored in checkpoints; see [program_artifact()].
#' @param verbose Currently unused.
#'
#' @return An `OptimizerControl` object to pass to [compile()] as `control`.
#' @family optimizer building blocks
#' @export
#'
#' @examples
#' optimizer_control()
#'
#' # Stop after 50 trials or US$2 of known cost, whichever comes first
#' ctrl <- optimizer_control(max_trials = 50L, max_cost = 2)
#'
#' \dontrun{
#' compiled <- compile(
#'   program,
#'   MIPROv2(metric = metric_exact_match(field = "answer")),
#'   trainset,
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   control = ctrl
#' )
#' }
optimizer_control <- function(
  seed = NULL,
  max_trials = NULL,
  max_errors = 5L,
  max_metric_calls = NULL,
  max_provider_calls = NULL,
  max_input_tokens = NULL,
  max_output_tokens = NULL,
  max_total_tokens = NULL,
  max_cost = NULL,
  max_elapsed_seconds = NULL,
  num_threads = 1L,
  progress = NA,
  log_dir = NULL,
  checkpoint_path = NULL,
  resume = FALSE,
  checkpoint_registry = list(),
  verbose = FALSE
) {
  # Resolve progress default
  if (is.na(progress)) {
    progress <- interactive()
  }

  integer_limits <- list(
    max_metric_calls = max_metric_calls,
    max_provider_calls = max_provider_calls,
    max_input_tokens = max_input_tokens,
    max_output_tokens = max_output_tokens,
    max_total_tokens = max_total_tokens
  )
  for (name in names(integer_limits)) {
    message <- optimizer_limit_validator(
      integer_limits[[name]],
      name,
      integer = TRUE
    )
    if (!is.null(message)) {
      cli::cli_abort(message, class = "dsprrr_optimizer_control_error")
    }
  }

  control <- OptimizerControl(
    seed = seed,
    max_trials = if (!is.null(max_trials)) as.integer(max_trials) else NULL,
    max_errors = as.integer(max_errors),
    max_metric_calls = if (!is.null(max_metric_calls)) {
      as.integer(max_metric_calls)
    } else {
      NULL
    },
    max_provider_calls = if (!is.null(max_provider_calls)) {
      as.integer(max_provider_calls)
    } else {
      NULL
    },
    max_input_tokens = if (!is.null(max_input_tokens)) {
      as.integer(max_input_tokens)
    } else {
      NULL
    },
    max_output_tokens = if (!is.null(max_output_tokens)) {
      as.integer(max_output_tokens)
    } else {
      NULL
    },
    max_total_tokens = if (!is.null(max_total_tokens)) {
      as.integer(max_total_tokens)
    } else {
      NULL
    },
    max_cost = max_cost,
    max_elapsed_seconds = max_elapsed_seconds,
    num_threads = as.integer(num_threads),
    progress = progress,
    log_dir = log_dir,
    checkpoint_path = checkpoint_path,
    resume = resume,
    checkpoint_registry = checkpoint_registry,
    verbose = verbose
  )

  if (isTRUE(resume) && is.null(checkpoint_path)) {
    cli::cli_abort(
      c(
        "Cannot resume without an optimizer checkpoint",
        "i" = "Supply {.arg checkpoint_path} when {.code resume = TRUE}."
      ),
      class = "dsprrr_optimizer_checkpoint_config_error"
    )
  }

  control
}

optimizer_control_for_teleprompter <- function(
  teleprompter,
  control = NULL,
  num_threads = 1L
) {
  if (!is.null(control)) {
    if (!inherits(control, "dsprrr::OptimizerControl")) {
      cli::cli_abort(
        "{.arg control} must be created by {.fn optimizer_control}",
        class = "dsprrr_optimizer_control_error"
      )
    }
    return(control)
  }
  optimizer_control(
    seed = tryCatch(teleprompter@seed, error = function(e) NULL),
    max_errors = teleprompter@max_errors,
    num_threads = num_threads,
    log_dir = tryCatch(teleprompter@log_dir, error = function(e) NULL)
  )
}

optimizer_require_ledger_only_checkpoint <- function(control, optimizer) {
  if (isTRUE(control@resume)) {
    cli::cli_abort(
      c(
        "{optimizer} does not yet support fine-grained checkpoint resume",
        "i" = "Resource budgets and best-partial return are supported.",
        "i" = "Start without {.code resume = TRUE}; resume support is tracked separately."
      ),
      class = "dsprrr_optimizer_checkpoint_unsupported_optimizer"
    )
  }
  if (!is.null(control@checkpoint_path)) {
    cli::cli_warn(
      c(
        "{optimizer} is running without checkpoint persistence",
        "i" = "Its resource ledger and best partial program remain available in memory."
      ),
      class = "dsprrr_optimizer_checkpoint_unsupported_optimizer"
    )
  }
  invisible(control)
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
    input_tokens = S7::new_property(S7::class_integer, default = 0L),
    output_tokens = S7::new_property(S7::class_integer, default = 0L),
    total_tokens = S7::new_property(S7::class_integer, default = 0L),
    total_cost = S7::new_property(S7::class_numeric, default = 0),
    provider_calls = S7::new_property(S7::class_integer, default = 0L),
    metric_calls = S7::new_property(S7::class_integer, default = 0L),
    provider_usage_unknown = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    token_usage_unknown = S7::new_property(
      S7::class_logical,
      default = FALSE
    ),
    # Timing
    total_latency_ms = S7::new_property(S7::class_numeric, default = 0),
    start_time = S7::new_property(S7::class_any, default = NULL),
    end_time = S7::new_property(S7::class_any, default = NULL),
    # Epochs support (for multi-epoch evaluation)
    epochs = S7::new_property(S7::class_integer, default = 1L),
    epoch_scores = S7::new_property(S7::class_list, default = list()),
    score_std = S7::new_property(S7::class_any, default = NA_real_),
    ci_lower = S7::new_property(S7::class_any, default = NA_real_),
    ci_upper = S7::new_property(S7::class_any, default = NA_real_),
    trace_context = S7::new_property(
      S7::class_list,
      default = list(),
      validator = function(value) {
        trace_context_validate(value, arg = "trace_context")
        NULL
      }
    )
  )
)

#' Evaluate a program with per-example detail
#'
#' @description
#' `eval_program()` runs a program on a dataset, scores every row with a
#' metric, and returns an `EvalResult` with per-example scores, errors,
#' predictions and feedback, plus totals for tokens, cost and time. It is the
#' evaluation the optimizers use internally. Use it when you build your own
#' optimizer or need the same detail; for everyday evaluation, [evaluate()] is
#' simpler.
#'
#' @details
#' The program is copied before it runs, so its traces are not changed. With
#' `epochs` above 1, every row is evaluated that many times, and the result
#' also reports the spread of the per-epoch mean scores.
#'
#' @param program A module or composed program.
#' @param dataset A data frame with the signature's input columns and the
#'   columns the metric compares.
#' @param metric A metric function called as `metric(prediction, expected)`,
#'   such as `metric_exact_match(field = "answer")`.
#' @param .llm Optional ellmer Chat.
#' @param control An [optimizer_control()] object, or `NULL` for the defaults.
#'   Its `num_threads` sets how many rows run at the same time and `progress`
#'   whether to show a progress bar.
#' @param epochs Integer number of times each row is evaluated (default `1L`).
#' @param ... Further arguments passed to [evaluate()].
#' @param .trace_context A named, JSON-compatible list of correlation fields
#'   (such as an experiment ID) copied to the result and to every execution
#'   trace.
#'
#' @return An `EvalResult` S7 object. Read its properties with `@`:
#'   * `examples`: a tibble with one row per example and columns `row_id`,
#'     `score`, `error`, `predicted`, `feedback` (from metrics made with
#'     [metric_with_feedback()]), `program_trace`, and one `input_<name>`
#'     column per input.
#'   * `mean_score`, `std_error`: mean score of the successful rows and its
#'     standard error.
#'   * `n_evaluated`, `n_errors`: counts of successful and failed rows.
#'   * `input_tokens`, `output_tokens`, `total_tokens`, `total_cost`,
#'     `provider_calls`, `metric_calls`: usage totals (`NA` when unknown).
#'   * `total_latency_ms`, `start_time`, `end_time`: timing.
#'   * `epochs`, `epoch_scores`, `score_std`, `ci_lower`, `ci_upper`: with
#'     `epochs` above 1, the per-epoch scores, their standard deviation and a
#'     95% confidence interval.
#'   * `trace_context`: the correlation fields.
#'
#' @family optimizer building blocks
#' @export
#'
#' @examples
#' add_one <- module_fn(
#'   "x -> answer",
#'   function(x) list(answer = as.character(as.numeric(x) + 1))
#' )
#' dataset <- data.frame(x = c("1", "2"), answer = c("2", "3"))
#' result <- eval_program(
#'   add_one,
#'   dataset,
#'   metric = metric_exact_match(field = "answer")
#' )
#' result@mean_score
#' result@examples
#'
#' \dontrun{
#' qa <- module(signature("question -> answer"))
#' qa_data <- data.frame(
#'   question = c("What is 2+2?", "What is 3+3?"),
#'   answer = c("4", "6")
#' )
#'
#' eval_program(
#'   qa,
#'   qa_data,
#'   metric = metric_exact_match(field = "answer"),
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' }
eval_program <- function(
  program,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  epochs = 1L,
  ...,
  .trace_context = list()
) {
  # Validate inputs
  if (!inherits(program, "Module")) {
    cli::cli_abort("{.arg program} must be a DSPrrr Module object")
  }

  trace_context_supplied <- !missing(.trace_context)
  trace_context <- trace_context_resolve(
    .trace_context,
    supplied = trace_context_supplied
  )
  previous_trace_context <- trace_context_enter(
    trace_context,
    program = program,
    inherit_program_id = !trace_context_supplied
  )
  on.exit(trace_context_restore(previous_trace_context), add = TRUE)

  if (!is.data.frame(dataset)) {
    cli::cli_abort("{.arg dataset} must be a data frame or tibble")
  }

  if (!is.function(metric)) {
    cli::cli_abort("{.arg metric} must be a function")
  }

  # Validate epochs
  epochs <- as.integer(epochs)
  if (length(epochs) != 1 || epochs < 1) {
    cli::cli_abort(c(
      "{.arg epochs} must be a positive integer",
      "x" = "Got {epochs}",
      "i" = "Use epochs = 1L for single evaluation or epochs > 1 for multiple runs"
    ))
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
      n_errors = 0L,
      trace_context = trace_context
    ))
  }

  start_time <- Sys.time()

  # Clear traces before evaluation to get accurate cost for this run
  # Use copy_module to properly preserve custom class behavior (e.g., mock modules in tests)
  program_copy <- copy_module(program)

  eval_result <- optimizer_evaluate_dataset(
    program_copy,
    dataset,
    metric,
    .llm,
    control,
    epochs,
    ...
  )
  end_time <- Sys.time()

  optimizer_eval_result(
    program_copy,
    dataset,
    eval_result,
    epochs,
    start_time,
    end_time,
    input_signature = program$signature
  )
}

#' Sample rows reproducibly
#'
#' @description
#' `sample_dataset()` draws `n` rows from a data frame. With a `seed`, the draw
#' is reproducible and the session's random number stream is left as it was.
#'
#' @param dataset A data frame.
#' @param n Number of rows to draw. When `n` is `NULL` or at least
#'   `nrow(dataset)` and `replace = FALSE`, the data is returned unchanged, in
#'   its original order.
#' @param seed Random seed, or `NULL` (the default) to use the current random
#'   number stream.
#' @param replace Whether to draw with replacement (default `FALSE`).
#'
#' @return A data frame with the drawn rows.
#' @family optimizer building blocks
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:10, y = letters[1:10])
#'
#' sample_dataset(df, n = 3, seed = 42)
#' identical(
#'   sample_dataset(df, n = 3, seed = 42),
#'   sample_dataset(df, n = 3, seed = 42)
#' )
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
    withr::local_seed(as.integer(seed))
  }

  # Sample indices
  indices <- sample(total_rows, size = min(n, total_rows), replace = replace)

  dataset[indices, , drop = FALSE]
}

#' Split data into training and validation sets
#'
#' @description
#' `split_dataset()` randomly assigns `floor(prop * nrow(dataset))` rows to a
#' training set and the rest to a validation set, for example to get the
#' `trainset` and `valset` that [compile()] takes.
#'
#' @param dataset A data frame.
#' @param prop Share of rows for training, strictly between 0 and 1 (default
#'   `0.8`).
#' @param seed Random seed, or `NULL` (the default). With a seed, the session's
#'   random number stream is left as it was.
#'
#' @return A list with data frames `train` and `val`.
#' @family optimizer building blocks
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:10)
#' split <- split_dataset(df, prop = 0.8, seed = 42)
#' nrow(split$train)
#' split$val
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
    withr::local_seed(as.integer(seed))
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
#' @param module A dsprrr module with recorded traces.
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

  # Helper to safely get numeric counters, treating NULL and NA as 0
  safe_num <- function(x, default = 0) {
    if (is.null(x) || length(x) == 0 || is.na(x)) default else x
  }

  cost_value <- cost$total_cost
  next_total_cost <- if (
    is.null(cost_value) ||
      length(cost_value) == 0 ||
      is.na(cost_value) ||
      is.na(summary@total_cost)
  ) {
    NA_real_
  } else {
    summary@total_cost + cost_value
  }

  CostSummary(
    tokens_in = summary@tokens_in + as.integer(safe_num(cost$tokens_in, 0L)),
    tokens_out = summary@tokens_out + as.integer(safe_num(cost$tokens_out, 0L)),
    total_tokens = summary@total_tokens +
      as.integer(safe_num(cost$total_tokens, 0L)),
    total_cost = next_total_cost,
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
#' @param control An object created by [optimizer_control()] with budget settings.
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
  reached_error_limit <- if (control@max_errors == 0L) {
    error_count > 0L
  } else {
    error_count >= control@max_errors
  }

  if (reached_error_limit) {
    reason <- if (control@max_errors == 0L) {
      "Reached max_errors limit (first error with max_errors = 0)"
    } else {
      sprintf(
        "Reached max_errors limit (%d consecutive errors)",
        control@max_errors
      )
    }

    return(list(
      should_stop = TRUE,
      reason = reason
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

# Print an EvalResult object through its S7 method.
print_eval_result <- function(x, ...) {
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
