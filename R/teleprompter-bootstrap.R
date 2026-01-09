# BootstrapFewShot Teleprompter
#
# DSPy-style demonstration bootstrapping optimizer.
# Uses a teacher model to generate demonstrations from training examples,
# selecting those that pass a metric threshold.

#' BootstrapFewShot Teleprompter
#'
#' @include teleprompter.R optimizer-core.R
#'
#' @description
#' A teleprompter that bootstraps demonstrations by having a teacher model
#' generate predictions on training examples and selecting successful ones
#' as demonstrations. This is DSPy's foundational optimization approach.
#'
#' The optimizer:
#' 1. Starts with optional labeled demonstrations from the training set
#' 2. Uses a teacher model to generate predictions on remaining examples
#' 3. Evaluates predictions using the provided metric
#' 4. Selects top-scoring predictions as bootstrapped demonstrations
#' 5. Optionally runs multiple rounds, updating the teacher with new demos
#'
#' @param metric A metric function for evaluating predictions (required).
#' @param metric_threshold Minimum score for a demo to be accepted.
#'   If NULL, accepts any successful prediction. Default is NULL.
#' @param max_errors Maximum number of errors allowed during optimization.
#'   Default is 5.
#' @param max_bootstrapped_demos Maximum number of bootstrapped demonstrations
#'   to include. Default is 4.
#' @param max_labeled_demos Maximum number of labeled demonstrations from
#'   the training set. Default is 16.
#' @param max_rounds Number of bootstrap rounds to perform. Default is 1.
#' @param teacher_settings List of settings for the teacher model, such as
#'   `temperature` or `model`. If NULL, defaults to `list(temperature = 0.7)`.
#' @param seed Random seed for reproducibility. Default is NULL.
#' @param log_dir Directory for trial logging. Default is NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a BootstrapFewShot teleprompter
#' tp <- BootstrapFewShot(
#'   metric = metric_exact_match(field = "answer"),
#'   max_bootstrapped_demos = 4L,
#'   max_labeled_demos = 8L
#' )
#'
#' # Compile a module
#' compiled <- compile(tp, qa_module, trainset, .llm = llm)
#' }
BootstrapFewShot <- S7::new_class(
  "BootstrapFewShot",
  parent = Teleprompter,
  properties = list(
    max_bootstrapped_demos = S7::new_property(
      S7::class_integer,
      default = 4L,
      validator = function(value) {
        if (value < 0) {
          return("max_bootstrapped_demos must be non-negative")
        }
        NULL
      }
    ),
    max_labeled_demos = S7::new_property(
      S7::class_integer,
      default = 16L,
      validator = function(value) {
        if (value < 0) {
          return("max_labeled_demos must be non-negative")
        }
        NULL
      }
    ),
    max_rounds = S7::new_property(
      S7::class_integer,
      default = 1L,
      validator = function(value) {
        if (value < 1) {
          return("max_rounds must be at least 1")
        }
        NULL
      }
    ),
    teacher_settings = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is.list(value)) {
          return("teacher_settings must be a list or NULL")
        }
        NULL
      }
    ),
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
    log_dir = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is.character(value)) {
          return("log_dir must be a character string or NULL")
        }
        NULL
      }
    )
  )
)

#' Compile method for BootstrapFewShot
#' @noRd
compile_bootstrap <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  ...
) {
  # Validate inputs
  if (!inherits(program, "Module")) {
    cli::cli_abort("BootstrapFewShot currently only supports Module objects")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("{.arg trainset} must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort("BootstrapFewShot requires a metric function")
  }

  # Apply default teacher settings if not provided
  teacher_settings <- teleprompter@teacher_settings %||% list(temperature = 0.7)

  # Create control from teleprompter settings
  control <- optimizer_control(
    seed = teleprompter@seed,
    max_errors = teleprompter@max_errors,
    log_dir = teleprompter@log_dir
  )

  # Initialize trial logging if log_dir specified
  trial_log <- if (!is.null(teleprompter@log_dir)) {
    TrialLog$new(
      optimizer_name = "BootstrapFewShot",
      log_dir = teleprompter@log_dir
    )
  } else {
    NULL
  }

  # Sample trainset with seed for reproducibility
  trainset <- sample_dataset(
    trainset,
    n = NULL, # Use full dataset
    seed = teleprompter@seed
  )

  # Create optimized copy of the program (student)
  student <- copy_module(program)

  # Create teacher (defaults to student if no special settings)
  teacher <- copy_module(program)

  # Get input names from signature

  input_names <- vapply(
    program$signature@inputs,
    function(x) x$name,
    character(1)
  )

  # Determine output column name
  # First try to get field from metric, then fall back to auto-detection
  output_col <- get_metric_field(teleprompter@metric) %||%
    find_output_column(trainset, input_names)

  if (is.null(output_col)) {
    cli::cli_warn(
      c(
        "No output column found in trainset",
        "i" = "Expected one of: 'output', 'label', 'answer', 'response', 'result', 'y'",
        "i" = "Or any column not in inputs: {.val {input_names}}",
        "i" = "Available columns: {.val {names(trainset)}}",
        "!" = "Metric will receive NULL as expected value for all examples"
      ),
      class = "dsprrr_missing_output_column"
    )
  }

  # Phase 1: Select labeled demos from trainset
  n_labeled <- min(teleprompter@max_labeled_demos, nrow(trainset))
  labeled_demos <- list()

  if (n_labeled > 0) {
    labeled_data <- trainset[seq_len(n_labeled), , drop = FALSE]
    labeled_demos <- format_trainset_as_demos(
      labeled_data,
      program$signature,
      output_col = output_col
    )

    # Add source metadata
    for (i in seq_along(labeled_demos)) {
      labeled_demos[[i]]$source <- "labeled"
      labeled_demos[[i]]$score <- NA_real_
      labeled_demos[[i]]$round <- 0L
    }
  }

  # Remaining examples for bootstrapping
  bootstrap_indices <- if (n_labeled < nrow(trainset)) {
    seq(n_labeled + 1, nrow(trainset))
  } else {
    integer(0)
  }

  # Phase 2: Bootstrap demonstrations
  bootstrapped_demos <- list()
  error_count <- 0
  total_attempts <- 0

  for (round in seq_len(teleprompter@max_rounds)) {
    if (length(bootstrap_indices) == 0) {
      break
    }

    # Update teacher with current demos for this round
    current_demos <- c(labeled_demos, bootstrapped_demos)
    teacher$demos <- current_demos

    # Process each bootstrap candidate
    for (idx in bootstrap_indices) {
      # Check error budget
      budget_check <- check_budget(total_attempts, error_count, control)
      if (budget_check$should_stop) {
        cli::cli_warn(budget_check$reason)
        break
      }

      row <- trainset[idx, , drop = FALSE]
      total_attempts <- total_attempts + 1

      # Extract inputs for this example
      example_inputs <- list()
      for (name in input_names) {
        if (name %in% names(row)) {
          example_inputs[[name]] <- row[[name]]
        }
      }

      # Get expected output
      expected <- if (!is.null(output_col) && output_col %in% names(row)) {
        row[[output_col]]
      } else {
        NULL
      }

      # Run teacher with temperature for diversity
      result <- tryCatch(
        {
          # Apply teacher settings (like temperature)
          run_with_settings(
            teacher,
            example_inputs,
            .llm = .llm,
            settings = teacher_settings
          )
        },
        error = function(e) {
          error_count <<- error_count + 1
          cli::cli_warn(
            c(
              "Bootstrap attempt failed",
              "x" = conditionMessage(e),
              "i" = "Error count: {error_count}/{control@max_errors}"
            ),
            class = "dsprrr_bootstrap_warning"
          )
          NULL
        }
      )

      if (is.null(result)) {
        next
      }

      # Evaluate with metric
      score <- tryCatch(
        {
          teleprompter@metric(result, expected)
        },
        error = function(e) {
          cli::cli_warn(
            c(
              "Metric evaluation failed for example {idx}",
              "x" = conditionMessage(e),
              "i" = "This example will be skipped for bootstrapping"
            ),
            class = "dsprrr_metric_warning"
          )
          NA_real_
        }
      )

      # Check threshold
      passes_threshold <- if (!is.na(score)) {
        if (!is.null(teleprompter@metric_threshold)) {
          score >= teleprompter@metric_threshold
        } else {
          score > 0
        }
      } else {
        FALSE
      }

      if (passes_threshold) {
        demo <- list(
          inputs = example_inputs,
          output = result,
          source = "bootstrapped",
          score = score,
          round = round
        )
        bootstrapped_demos <- append(bootstrapped_demos, list(demo))
      }

      # Check if we have enough bootstrapped demos
      if (length(bootstrapped_demos) >= teleprompter@max_bootstrapped_demos) {
        break
      }
    }

    # Early exit if we have enough demos
    if (length(bootstrapped_demos) >= teleprompter@max_bootstrapped_demos) {
      break
    }
  }

  # Phase 3: Select top bootstrapped demos by score
  if (length(bootstrapped_demos) > teleprompter@max_bootstrapped_demos) {
    scores <- vapply(
      bootstrapped_demos,
      function(d) d$score %||% 0,
      numeric(1)
    )
    top_indices <- order(scores, decreasing = TRUE)[
      seq_len(teleprompter@max_bootstrapped_demos)
    ]
    bootstrapped_demos <- bootstrapped_demos[top_indices]
  }

  # Combine labeled and bootstrapped demos for student
  final_demos <- c(labeled_demos, bootstrapped_demos)
  student$demos <- final_demos

  # Update student state
  student$state$compiled <- TRUE
  student$config$compiled <- TRUE
  student$config$teleprompter <- "BootstrapFewShot"
  student$config$optimizer <- list(
    n_labeled_demos = length(labeled_demos),
    n_bootstrapped_demos = length(bootstrapped_demos),
    total_attempts = total_attempts,
    error_count = error_count,
    max_rounds = teleprompter@max_rounds,
    rounds_completed = min(
      teleprompter@max_rounds,
      ceiling(total_attempts / max(1, length(bootstrap_indices)))
    )
  )

  # Log trial if logging enabled
  if (!is.null(trial_log)) {
    trial <- create_trial(
      optimizer_name = "BootstrapFewShot",
      params = list(
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        max_rounds = teleprompter@max_rounds,
        metric_threshold = teleprompter@metric_threshold
      )
    )

    # If valset provided, evaluate
    if (!is.null(valset)) {
      eval_result <- eval_program(
        student,
        valset,
        teleprompter@metric,
        .llm = .llm,
        control = control
      )
      trial <- complete_trial(
        trial,
        eval_result,
        compiled_artifact_ref = student
      )
    } else {
      # Create a minimal eval result
      trial <- complete_trial(
        trial,
        EvalResult(
          n_evaluated = as.integer(length(bootstrapped_demos)),
          n_errors = as.integer(error_count)
        )
      )
    }

    trial_log$add_trial(trial)
  }

  student
}

#' Run module with specific settings
#' @noRd
run_with_settings <- function(module, inputs, .llm = NULL, settings = list()) {
  # For now, we pass settings through the run call

  # Future: could modify the LLM or module config temporarily

  # Convert inputs list to named arguments
  args <- inputs
  args$.llm <- .llm

  # If temperature is specified, we would ideally pass it to the LLM

  # For now, we rely on the default module behavior
  # TODO: Add support for per-call temperature override in ellmer

  do.call(run, c(list(module), args))
}

#' Find output column in trainset
#' @noRd
find_output_column <- function(trainset, input_names) {
  # Try common names first
  possible_output_names <- c(
    "output",
    "label",
    "answer",
    "response",
    "result",
    "y"
  )

  for (col in possible_output_names) {
    if (col %in% names(trainset)) {
      return(col)
    }
  }

  # Use any column not in inputs
  remaining_cols <- setdiff(names(trainset), input_names)
  if (length(remaining_cols) > 0) {
    return(remaining_cols[1])
  }

  NULL
}

#' Print method for BootstrapFewShot
#' @param x A BootstrapFewShot object
#' @param ... Additional arguments (unused)
#' @export
print.BootstrapFewShot <- function(x, ...) {
  cli::cli_h3("BootstrapFewShot Teleprompter")

  cli::cli_text("{.field max_bootstrapped_demos}: {x@max_bootstrapped_demos}")
  cli::cli_text("{.field max_labeled_demos}: {x@max_labeled_demos}")
  cli::cli_text("{.field max_rounds}: {x@max_rounds}")

  if (!is.null(x@metric_threshold)) {
    cli::cli_text("{.field metric_threshold}: {x@metric_threshold}")
  }

  if (!is.null(x@seed)) {
    cli::cli_text("{.field seed}: {x@seed}")
  }

  invisible(x)
}

# Register S7 print method
S7::method(print, BootstrapFewShot) <- print.BootstrapFewShot
