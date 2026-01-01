# BootstrapFewShotWithRandomSearch Teleprompter
#
# Extends BootstrapFewShot with parallel random search over candidate programs.
# Generates multiple candidate programs with different configurations and
# selects the best based on validation set performance.

#' BootstrapFewShotWithRandomSearch Teleprompter
#'
#' @include teleprompter.R teleprompter-bootstrap.R optimizer-core.R optimizer-logging.R
#'
#' @description
#' A teleprompter that extends BootstrapFewShot with random search over
#' multiple candidate programs. It generates several candidate configurations
#' and selects the best based on validation set performance.
#'
#' The optimizer generates candidates including:
#' 1. Uncompiled baseline program
#' 2. LabeledFewShot-only program
#' 3. BootstrapFewShot with unshuffled examples
#' 4. BootstrapFewShot with various random seeds
#'
#' @param metric A metric function for evaluating predictions (required).
#' @param metric_threshold Minimum score for a demo to be accepted.
#'   If NULL, accepts any successful prediction. Default is NULL.
#' @param max_errors Maximum number of errors allowed during optimization.
#'   Default is 10.
#' @param num_candidate_programs Number of candidate programs to generate.
#'   Default is 16.
#' @param num_threads Number of threads for parallel evaluation. If 1,
#'   runs sequentially. Default is 1.
#' @param stop_at_score Early stopping threshold. If a candidate achieves
#'   this score or higher, stop searching. Default is NULL (no early stop).
#' @param max_bootstrapped_demos Maximum bootstrapped demos per candidate.
#'   Default is 4.
#' @param max_labeled_demos Maximum labeled demos per candidate.
#'   Default is 16.
#' @param max_rounds Number of bootstrap rounds per candidate. Default is 1.
#' @param teacher_settings List of settings for the teacher model.
#'   If NULL, defaults to `list(temperature = 0.7)`.
#' @param seed Random seed for reproducibility. Default is NULL.
#' @param log_dir Directory for trial logging. Default is NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a BootstrapFewShotWithRandomSearch teleprompter
#' tp <- BootstrapFewShotWithRandomSearch(
#'   metric = metric_exact_match(field = "answer"),
#'   num_candidate_programs = 8L,
#'   num_threads = 4L,
#'   stop_at_score = 0.95
#' )
#'
#' # Compile with validation set (required)
#' compiled <- compile(tp, qa_module, trainset, valset = valset, .llm = llm)
#'
#' # Access ranked candidates
#' compiled$config$optimizer$candidate_programs
#' }
BootstrapFewShotWithRandomSearch <- S7::new_class(
  "BootstrapFewShotWithRandomSearch",
  parent = Teleprompter,
  properties = list(
    num_candidate_programs = S7::new_property(
      S7::class_integer,
      default = 16L,
      validator = function(value) {
        if (value < 1) {
          return("num_candidate_programs must be at least 1")
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
    stop_at_score = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (!is.numeric(value) || length(value) != 1) {
            return("stop_at_score must be a single numeric value or NULL")
          }
          if (value < 0 || value > 1) {
            return("stop_at_score must be between 0 and 1")
          }
        }
        NULL
      }
    ),
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

#' Compile method for BootstrapFewShotWithRandomSearch
#' @noRd
compile_bootstrap_rs <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  ...
) {
  # Validate inputs
  if (!inherits(program, "Module")) {
    cli::cli_abort(
      "BootstrapFewShotWithRandomSearch currently only supports Module objects"
    )
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("{.arg trainset} must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort(
      "BootstrapFewShotWithRandomSearch requires a metric function"
    )
  }

  if (is.null(valset)) {
    cli::cli_abort(
      c(
        "BootstrapFewShotWithRandomSearch requires a validation set",
        "i" = "Pass {.arg valset} to evaluate candidate programs"
      )
    )
  }

  # Create control from teleprompter settings
  control <- optimizer_control(
    seed = teleprompter@seed,
    max_errors = teleprompter@max_errors,
    num_threads = teleprompter@num_threads,
    log_dir = teleprompter@log_dir
  )

  # Initialize trial logging if log_dir specified
  trial_log <- if (!is.null(teleprompter@log_dir)) {
    TrialLog$new(
      optimizer_name = "BootstrapFewShotWithRandomSearch",
      log_dir = teleprompter@log_dir
    )
  } else {
    NULL
  }

  # Set seed for reproducibility
  if (!is.null(teleprompter@seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }
    set.seed(teleprompter@seed)
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

  # Generate candidate configurations
  candidates <- generate_candidate_configs(
    teleprompter,
    nrow(trainset),
    teleprompter@seed
  )

  cli::cli_alert_info(
    "Evaluating {length(candidates)} candidate programs on validation set"
  )

  # Compile and evaluate each candidate
  results <- list()
  best_score <- -Inf
  best_candidate <- NULL
  best_program <- NULL

  for (i in seq_along(candidates)) {
    config <- candidates[[i]]

    cli::cli_progress_step(
      "Candidate {i}/{length(candidates)}: {config$name}",
      spinner = TRUE
    )

    # Compile candidate
    compile_error_msg <- NULL
    compiled <- tryCatch(
      {
        compile_candidate(
          config = config,
          program = program,
          trainset = trainset,
          .llm = .llm,
          teleprompter = teleprompter
        )
      },
      error = function(e) {
        compile_error_msg <<- conditionMessage(e)
        cli::cli_warn(
          c(
            "Failed to compile candidate {config$name}",
            "x" = conditionMessage(e)
          ),
          class = "dsprrr_candidate_error"
        )
        NULL
      }
    )

    if (is.null(compiled)) {
      results[[i]] <- list(
        name = config$name,
        config = config,
        program = NULL,
        score = NA_real_,
        error = TRUE,
        error_message = compile_error_msg %||% "Unknown compilation error"
      )
      next
    }

    # Evaluate on validation set
    eval_error_msg <- NULL
    eval_result <- tryCatch(
      {
        eval_program(
          compiled,
          valset,
          teleprompter@metric,
          .llm = .llm,
          control = control
        )
      },
      error = function(e) {
        eval_error_msg <<- conditionMessage(e)
        cli::cli_warn(
          c(
            "Failed to evaluate candidate {config$name}",
            "x" = conditionMessage(e)
          ),
          class = "dsprrr_eval_error"
        )
        NULL
      }
    )

    score <- if (!is.null(eval_result)) eval_result@mean_score else NA_real_
    has_error <- is.null(eval_result)

    results[[i]] <- list(
      name = config$name,
      config = config,
      program = compiled,
      score = score,
      eval_result = eval_result,
      error = has_error,
      error_message = if (has_error) {
        eval_error_msg %||% "Evaluation failed"
      } else {
        NULL
      }
    )

    # Track best
    if (!is.na(score) && score > best_score) {
      best_score <- score
      best_candidate <- config
      best_program <- compiled

      cli::cli_alert_success(
        "New best: {config$name} with score {round(score, 4)}"
      )
    }

    # Log trial
    if (!is.null(trial_log) && !is.null(eval_result)) {
      trial <- create_trial(
        optimizer_name = "BootstrapFewShotWithRandomSearch",
        params = config
      )
      trial <- complete_trial(
        trial,
        eval_result,
        compiled_artifact_ref = compiled
      )
      trial_log$add_trial(trial)
    }

    # Early stopping
    if (
      !is.null(teleprompter@stop_at_score) &&
        !is.na(score) &&
        score >= teleprompter@stop_at_score
    ) {
      cli::cli_alert_success(
        "Early stop: reached target score {teleprompter@stop_at_score}"
      )
      break
    }
  }

  # Rank candidates by score
  scores <- vapply(results, function(r) r$score %||% NA_real_, numeric(1))
  ranked_order <- order(scores, decreasing = TRUE, na.last = TRUE)
  ranked_results <- results[ranked_order]

  # Use best program or abort if all candidates failed
  if (is.null(best_program)) {
    # Collect error messages from failed candidates
    error_msgs <- vapply(
      results,
      function(r) {
        if (isTRUE(r$error)) {
          r$error_message %||% "Unknown error"
        } else {
          NA_character_
        }
      },
      character(1)
    )
    error_msgs <- error_msgs[!is.na(error_msgs)]

    cli::cli_abort(
      c(
        "All {length(results)} candidate programs failed to compile or evaluate",
        "i" = "This indicates a systemic issue with your configuration",
        "x" = if (length(error_msgs) > 0) {
          paste("Sample errors:", paste(utils::head(error_msgs, 3), collapse = "; "))
        } else {
          "No specific error messages captured"
        },
        "i" = "Check your LLM connection, metric function, and trainset format"
      ),
      class = "dsprrr_all_candidates_failed"
    )
  }

  # Update program state
  best_program$state$compiled <- TRUE
  best_program$config$compiled <- TRUE
  best_program$config$teleprompter <- "BootstrapFewShotWithRandomSearch"
  best_program$config$optimizer <- list(
    num_candidates_evaluated = length(results),
    best_candidate = if (!is.null(best_candidate)) {
      best_candidate$name
    } else {
      NA_character_
    },
    best_score = best_score,
    candidate_programs = lapply(ranked_results, function(r) {
      list(
        name = r$name,
        score = r$score,
        config = r$config
      )
    })
  )

  best_program
}

#' Generate candidate configurations for random search
#' @noRd
generate_candidate_configs <- function(teleprompter, n_train, base_seed) {
  configs <- list()
  idx <- 1

  # Candidate 1: Uncompiled baseline
  configs[[idx]] <- list(
    name = "baseline",
    type = "baseline"
  )
  idx <- idx + 1

  # Candidate 2: LabeledFewShot only (no bootstrapping)
  configs[[idx]] <- list(
    name = "labeled_only",
    type = "labeled",
    max_labeled_demos = teleprompter@max_labeled_demos,
    seed = base_seed
  )
  idx <- idx + 1

  # Candidate 3: BootstrapFewShot with unshuffled examples
  configs[[idx]] <- list(
    name = "bootstrap_unshuffled",
    type = "bootstrap",
    max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
    max_labeled_demos = teleprompter@max_labeled_demos,
    max_rounds = teleprompter@max_rounds,
    teacher_settings = teleprompter@teacher_settings,
    seed = NULL, # No shuffling
    shuffle = FALSE
  )
  idx <- idx + 1

  # Remaining candidates: BootstrapFewShot with random seeds
  n_random <- teleprompter@num_candidate_programs - 3
  if (n_random > 0) {
    # Generate random seeds (RNG state already set via base_seed if provided)
    random_seeds <- sample.int(10000, n_random)

    for (i in seq_len(n_random)) {
      configs[[idx]] <- list(
        name = paste0("bootstrap_seed_", random_seeds[i]),
        type = "bootstrap",
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        max_rounds = teleprompter@max_rounds,
        teacher_settings = teleprompter@teacher_settings,
        seed = random_seeds[i],
        shuffle = TRUE
      )
      idx <- idx + 1
    }
  }

  configs
}

#' Compile a single candidate program
#' @noRd
compile_candidate <- function(config, program, trainset, .llm, teleprompter) {
  if (config$type == "baseline") {
    # Return uncompiled copy
    compiled <- copy_module(program)
    compiled$config$candidate_type <- "baseline"
    return(compiled)
  }

  if (config$type == "labeled") {
    # Use LabeledFewShot
    tp <- LabeledFewShot(
      k = config$max_labeled_demos,
      sample = TRUE,
      seed = config$seed %||% 123L
    )
    compiled <- compile(tp, program, trainset)
    compiled$config$candidate_type <- "labeled"
    return(compiled)
  }

  if (config$type == "bootstrap") {
    # Use BootstrapFewShot
    tp <- BootstrapFewShot(
      metric = teleprompter@metric,
      metric_threshold = teleprompter@metric_threshold,
      max_errors = teleprompter@max_errors,
      max_bootstrapped_demos = config$max_bootstrapped_demos,
      max_labeled_demos = config$max_labeled_demos,
      max_rounds = config$max_rounds,
      teacher_settings = config$teacher_settings,
      seed = config$seed
    )
    compiled <- compile(tp, program, trainset, .llm = .llm)
    compiled$config$candidate_type <- "bootstrap"
    return(compiled)
  }

  cli::cli_abort("Unknown candidate type: {config$type}")
}

#' Print method for BootstrapFewShotWithRandomSearch
#' @param x A BootstrapFewShotWithRandomSearch object
#' @param ... Additional arguments (unused)
#' @export
print.BootstrapFewShotWithRandomSearch <- function(x, ...) {
  cli::cli_h3("BootstrapFewShotWithRandomSearch Teleprompter")

  cli::cli_text("{.field num_candidate_programs}: {x@num_candidate_programs}")
  cli::cli_text("{.field num_threads}: {x@num_threads}")
  cli::cli_text("{.field max_bootstrapped_demos}: {x@max_bootstrapped_demos}")
  cli::cli_text("{.field max_labeled_demos}: {x@max_labeled_demos}")
  cli::cli_text("{.field max_rounds}: {x@max_rounds}")

  if (!is.null(x@stop_at_score)) {
    cli::cli_text("{.field stop_at_score}: {x@stop_at_score}")
  }

  if (!is.null(x@metric_threshold)) {
    cli::cli_text("{.field metric_threshold}: {x@metric_threshold}")
  }

  if (!is.null(x@seed)) {
    cli::cli_text("{.field seed}: {x@seed}")
  }

  invisible(x)
}

# Register S7 print method
S7::method(print, BootstrapFewShotWithRandomSearch) <-
  print.BootstrapFewShotWithRandomSearch
