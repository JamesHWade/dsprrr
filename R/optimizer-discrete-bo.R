# Discrete Bayesian Optimization helper
#
# Provides lightweight discrete BO utilities for optimizers like MIPROv2.

#' Discrete BO over candidate configurations
#' @noRd
run_discrete_bo <- function(
  candidates,
  eval_fn,
  control,
  max_trials,
  minibatch_size,
  full_eval_every,
  trial_log = NULL,
  seed = NULL,
  track_stats = TRUE
) {
  if (!is.list(candidates) || length(candidates) == 0) {
    cli::cli_abort("{.arg candidates} must be a non-empty list")
  }

  if (!is.function(eval_fn)) {
    cli::cli_abort("{.arg eval_fn} must be a function")
  }

  if (is.null(control)) {
    control <- optimizer_control()
  }

  if (is.null(max_trials) || max_trials < 1) {
    cli::cli_abort("{.arg max_trials} must be a positive integer")
  }

  if (is.null(minibatch_size) || minibatch_size < 1) {
    cli::cli_abort("{.arg minibatch_size} must be a positive integer")
  }

  if (is.null(full_eval_every) || full_eval_every < 1) {
    full_eval_every <- max_trials + 1L
  }

  stats <- lapply(candidates, function(candidate) {
    list(
      id = candidate$id %||% NA_character_,
      count = 0L,
      mean_score = NA_real_,
      last_score = NA_real_
    )
  })

  trial_history <- list()
  error_count <- 0L
  best_full <- list(score = -Inf, candidate = NULL)
  best_any <- list(score = -Inf, candidate = NULL)

  with_seed <- function(seed, code) {
    if (is.null(seed)) {
      return(code())
    }
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }
    set.seed(seed)
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
    code()
  }

  select_candidate <- function(stats, trial_idx) {
    untried <- which(vapply(stats, function(s) s$count == 0L, logical(1)))
    if (length(untried) > 0) {
      return(sample(untried, 1))
    }

    ucb_scores <- vapply(
      stats,
      function(s) {
        mean_score <- if (is.na(s$mean_score)) -Inf else s$mean_score
        mean_score + sqrt(2 * log(trial_idx) / s$count)
      },
      numeric(1)
    )

    which.max(ucb_scores)
  }

  with_seed(seed, function() {
    for (trial_idx in seq_len(max_trials)) {
      eval_type <- if (trial_idx %% full_eval_every == 0L) {
        "full"
      } else {
        "minibatch"
      }

      candidate_idx <- select_candidate(stats, trial_idx)
      candidate <- candidates[[candidate_idx]]

      eval_result <- tryCatch(
        {
          eval_fn(candidate, eval_type, trial_idx)
        },
        error = function(e) {
          error_count <<- error_count + 1L
          cli::cli_warn(
            c(
              "Evaluation failed for trial {trial_idx}",
              "x" = conditionMessage(e)
            ),
            class = "dsprrr_mipro_eval_error"
          )
          NULL
        }
      )

      score <- NA_real_
      std_error <- NA_real_
      n_evaluated <- 0L
      n_errors <- 0L

      if (!is.null(eval_result)) {
        score <- eval_result@mean_score
        std_error <- eval_result@std_error
        n_evaluated <- eval_result@n_evaluated
        n_errors <- eval_result@n_errors
        stats[[candidate_idx]]$count <- stats[[candidate_idx]]$count + 1L
        stats[[candidate_idx]]$last_score <- score
        if (is.na(stats[[candidate_idx]]$mean_score)) {
          stats[[candidate_idx]]$mean_score <- score
        } else {
          prev <- stats[[candidate_idx]]$mean_score
          n <- stats[[candidate_idx]]$count
          stats[[candidate_idx]]$mean_score <- prev + (score - prev) / n
        }

        if (!is.na(score) && score > best_any$score) {
          best_any <- list(score = score, candidate = candidate)
        }

        if (eval_type == "full" && !is.na(score) && score > best_full$score) {
          best_full <- list(score = score, candidate = candidate)
        }
      }

      if (!is.null(trial_log) && !is.null(eval_result)) {
        trial <- create_trial(
          optimizer_name = "MIPROv2",
          params = c(candidate$params %||% list(), list(eval_type = eval_type))
        )
        trial <- complete_trial(trial, eval_result, notes = eval_type)
        trial_log$add_trial(trial)
      }

      trial_history[[length(trial_history) + 1L]] <- list(
        trial_index = trial_idx,
        candidate_id = candidate$id %||% NA_character_,
        eval_type = eval_type,
        mean_score = score,
        std_error = std_error,
        n_evaluated = n_evaluated,
        n_errors = n_errors,
        demo_id = candidate$demo_id %||% NA_character_,
        instruction_id = candidate$instruction_id %||% NA_character_
      )

      if (error_count > control@max_errors) {
        cli::cli_abort(
          c(
            "Exceeded maximum errors during optimization",
            "i" = "max_errors: {control@max_errors}"
          ),
          class = "dsprrr_mipro_error_limit"
        )
      }
    }
  })

  trial_history_tbl <- if (track_stats) {
    tibble::as_tibble(trial_history)
  } else {
    tibble::tibble()
  }

  best_candidate <- if (!is.null(best_full$candidate)) {
    best_full$candidate
  } else {
    best_any$candidate
  }

  list(
    best_candidate = best_candidate,
    trial_history = trial_history_tbl,
    candidate_stats = stats
  )
}
