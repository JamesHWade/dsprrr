# Discrete Bayesian Optimization helper
#
# Provides lightweight discrete BO utilities for optimizers like MIPROv2.

is_fatal_discrete_bo_error <- function(condition) {
  classes <- class(condition)
  message <- conditionMessage(condition)

  any(grepl("config|invariant", classes, ignore.case = TRUE)) ||
    grepl("\\b(configuration|invariant)\\b", message, ignore.case = TRUE)
}

discrete_bo_initial_stats <- function(candidates) {
  lapply(candidates, function(candidate) {
    list(
      id = candidate$id %||% NA_character_,
      count = 0L,
      mean_score = NA_real_,
      last_score = NA_real_
    )
  })
}

discrete_bo_best_state <- function() {
  list(
    score = -Inf,
    candidate_index = NA_integer_,
    trial_index = NA_integer_
  )
}

discrete_bo_checkpoint_trial_record <- function(trial) {
  record <- trial_json_record(trial)
  token_field <- which(names(record$cost_summary) == "token_usage_unknown")
  if (length(token_field) == 1L) {
    names(record$cost_summary)[[token_field]] <- "tokens_unknown"
  }
  record
}

discrete_bo_restore_trial_record <- function(record) {
  validate_trial_record(
    record,
    class = "dsprrr_optimizer_checkpoint_malformed"
  )
  cost_summary <- record$cost_summary
  token_field <- which(names(cost_summary) == "tokens_unknown")
  if (length(token_field) == 1L) {
    names(cost_summary)[[token_field]] <- "token_usage_unknown"
  }
  parse_time <- function(value) {
    if (is.null(value)) {
      return(NULL)
    }
    parsed <- as.POSIXct(
      value,
      format = "%Y-%m-%dT%H:%M:%S",
      tz = Sys.timezone()
    )
    if (length(parsed) != 1L || is.na(parsed)) {
      cli::cli_abort(
        "Discrete BO trial timestamp is malformed",
        class = "dsprrr_optimizer_checkpoint_malformed"
      )
    }
    parsed
  }
  Trial(
    trial_id = record$trial_id,
    optimizer_name = record$optimizer_name,
    params = record$params,
    metric_summary = record$metric_summary,
    cost_summary = cost_summary,
    start_time = parse_time(record$start_time),
    end_time = parse_time(record$end_time),
    notes = record$notes,
    trace_context = trace_context_validate(
      record$trace_context,
      arg = "trace_context"
    ),
    status = record$status
  )
}

discrete_bo_candidate <- function(candidates, best) {
  index <- best$candidate_index %||% NA_integer_
  if (
    length(index) != 1L ||
      is.na(index) ||
      index < 1L ||
      index > length(candidates)
  ) {
    return(NULL)
  }
  candidates[[index]]
}

discrete_bo_state <- function(
  stats,
  trial_history,
  best_full,
  best_any,
  next_trial,
  active_trial
) {
  list(
    stats = stats,
    trial_history = trial_history,
    best_full = best_full,
    best_any = best_any,
    next_trial = as.integer(next_trial),
    active_trial = active_trial,
    rng = optimizer_checkpoint_capture_rng()
  )
}

discrete_bo_restore_state <- function(state, candidates) {
  if (is.null(state) || length(state) == 0L) {
    return(list(
      stats = discrete_bo_initial_stats(candidates),
      trial_history = list(),
      best_full = discrete_bo_best_state(),
      best_any = discrete_bo_best_state(),
      next_trial = 1L,
      active_trial = NULL,
      rng = NULL
    ))
  }
  expected <- c(
    "stats",
    "trial_history",
    "best_full",
    "best_any",
    "next_trial",
    "active_trial",
    "rng"
  )
  if (
    !is.list(state) ||
      !setequal(names(state), expected) ||
      length(state$stats) != length(candidates) ||
      !is.numeric(state$next_trial) ||
      length(state$next_trial) != 1L ||
      is.na(state$next_trial) ||
      state$next_trial < 1L
  ) {
    cli::cli_abort(
      "Discrete BO resume state is malformed",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  state$next_trial <- as.integer(state$next_trial)
  state
}

discrete_bo_history_table <- function(trial_history, track_stats) {
  if (!track_stats || length(trial_history) == 0L) {
    return(tibble::tibble())
  }
  tibble::tibble(
    trial_index = vapply(trial_history, `[[`, integer(1), "trial_index"),
    candidate_id = vapply(
      trial_history,
      function(x) x$candidate_id %||% NA_character_,
      character(1)
    ),
    eval_type = vapply(
      trial_history,
      function(x) x$eval_type %||% NA_character_,
      character(1)
    ),
    mean_score = vapply(
      trial_history,
      function(x) x$mean_score %||% NA_real_,
      numeric(1)
    ),
    std_error = vapply(
      trial_history,
      function(x) x$std_error %||% NA_real_,
      numeric(1)
    ),
    n_evaluated = vapply(
      trial_history,
      function(x) x$n_evaluated %||% 0L,
      integer(1)
    ),
    n_errors = vapply(
      trial_history,
      function(x) x$n_errors %||% 0L,
      integer(1)
    ),
    demo_id = vapply(
      trial_history,
      function(x) x$demo_id %||% NA_character_,
      character(1)
    ),
    instruction_id = vapply(
      trial_history,
      function(x) x$instruction_id %||% NA_character_,
      character(1)
    )
  )
}

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
  track_stats = TRUE,
  budget = NULL,
  resume_state = NULL,
  on_progress = NULL,
  resumable_eval = FALSE
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
  if (
    !is.logical(resumable_eval) ||
      length(resumable_eval) != 1L ||
      is.na(resumable_eval)
  ) {
    cli::cli_abort("{.arg resumable_eval} must be TRUE or FALSE")
  }

  budget <- budget %||% new_optimizer_budget(control)
  restored <- discrete_bo_restore_state(resume_state, candidates)
  stats <- restored$stats
  trial_history <- restored$trial_history
  best_full <- restored$best_full
  best_any <- restored$best_any
  next_trial <- restored$next_trial
  active_trial <- restored$active_trial

  manage_rng <- !is.null(seed) || !is.null(restored$rng)
  if (manage_rng) {
    outer_rng <- optimizer_checkpoint_capture_rng()
    on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
    if (!is.null(restored$rng)) {
      optimizer_checkpoint_restore_rng(restored$rng)
    } else {
      set.seed(seed)
    }
  }

  notify_progress <- function() {
    if (!is.function(on_progress)) {
      return(invisible(NULL))
    }
    state <- discrete_bo_state(
      stats,
      trial_history,
      best_full,
      best_any,
      next_trial,
      active_trial
    )
    best <- discrete_bo_candidate(candidates, best_full) %||%
      discrete_bo_candidate(candidates, best_any)
    on_progress(state = state, best_candidate = best, budget = budget)
    invisible(NULL)
  }

  select_candidate <- function(trial_idx) {
    untried <- which(vapply(stats, function(s) s$count == 0L, logical(1)))
    if (length(untried) > 0L) {
      if (length(untried) == 1L) {
        return(untried)
      }
      return(sample(untried, 1L))
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

  trial_indices <- if (next_trial > max_trials) {
    integer()
  } else {
    seq.int(next_trial, max_trials)
  }

  for (trial_idx in trial_indices) {
    if (optimizer_budget_stopped(budget)) {
      break
    }
    eval_type <- if (trial_idx %% full_eval_every == 0L) {
      "full"
    } else {
      "minibatch"
    }
    unit_id <- paste0("mipro:bo:trial:", trial_idx)

    if (!is.null(active_trial)) {
      active_names <- c(
        "trial_index",
        "candidate_index",
        "eval_type",
        "partial_records",
        "trial_id",
        "trial_record",
        "result_applied"
      )
      if (
        !is.list(active_trial) ||
          !identical(names(active_trial), active_names) ||
          !identical(active_trial$trial_index, as.integer(trial_idx)) ||
          !is.character(active_trial$trial_id) ||
          length(active_trial$trial_id) != 1L ||
          is.na(active_trial$trial_id) ||
          !nzchar(active_trial$trial_id) ||
          !is.logical(active_trial$result_applied) ||
          length(active_trial$result_applied) != 1L ||
          is.na(active_trial$result_applied)
      ) {
        cli::cli_abort(
          "Discrete BO active trial is malformed or does not match the resume cursor",
          class = "dsprrr_optimizer_checkpoint_malformed"
        )
      }
      candidate_idx <- active_trial$candidate_index
      eval_type <- active_trial$eval_type
    } else {
      if (
        !resumable_eval &&
          !optimizer_budget_preflight(
            budget,
            stage = paste0("discrete_bo_", eval_type),
            planned = list(trials = 1L),
            unit_id = unit_id,
            work_unit = "optimizer_trial",
            max_started = 0L
          )
      ) {
        break
      }
      candidate_idx <- select_candidate(trial_idx)
      active_trial <- list(
        trial_index = as.integer(trial_idx),
        candidate_index = as.integer(candidate_idx),
        eval_type = eval_type,
        partial_records = list(),
        trial_id = generate_trial_id(),
        trial_record = NULL,
        result_applied = FALSE
      )
      notify_progress()
    }

    candidate <- candidates[[candidate_idx]]
    budget_stage <- paste0("discrete_bo_", eval_type)
    eval_condition <- NULL
    eval_result <- tryCatch(
      {
        if (resumable_eval) {
          eval_fn(
            candidate,
            eval_type,
            trial_idx,
            budget = budget,
            unit_id = unit_id,
            partial_records = active_trial$partial_records %||% list(),
            on_progress = function(records, ...) {
              active_trial$partial_records <<- records
              notify_progress()
            }
          )
        } else {
          eval_fn(candidate, eval_type, trial_idx)
        }
      },
      error = function(e) {
        msg <- conditionMessage(e)
        is_auth_error <- any(grepl("auth", class(e), ignore.case = TRUE)) ||
          grepl(
            "auth|401|403|api.?key|invalid.?key|unauthorized",
            msg,
            ignore.case = TRUE
          )
        is_rate_limit <- grepl(
          "rate.?limit|429|quota|too.?many",
          msg,
          ignore.case = TRUE
        )
        if (is_auth_error) {
          cli::cli_abort(
            c(
              "Authentication error - cannot continue optimization",
              "x" = msg,
              "i" = "Please check your API key and permissions"
            ),
            class = "dsprrr_mipro_auth_error"
          )
        }
        if (is_fatal_discrete_bo_error(e)) {
          stop(e)
        }
        eval_condition <<- e
        record_optimizer_outcome(
          budget,
          success = FALSE,
          stage = budget_stage,
          condition = e
        )
        optimizer_budget_count_trial(budget, budget_stage, unit_id)
        if (is_rate_limit) {
          cli::cli_warn(
            c(
              "Rate limit hit for trial {trial_idx}",
              "x" = msg,
              "i" = "Consider reducing num_threads or adding delays"
            ),
            class = "dsprrr_mipro_rate_limit"
          )
        } else {
          cli::cli_warn(
            c(
              "Evaluation failed for trial {trial_idx}",
              "x" = msg,
              "i" = paste0(
                "Consecutive errors: ",
                budget$consecutive_errors,
                "; max_errors: ",
                control@max_errors
              )
            ),
            class = "dsprrr_mipro_eval_error"
          )
        }
        NULL
      }
    )

    if (
      resumable_eval &&
        is.null(eval_condition) &&
        !optimizer_budget_unit_completed(budget, unit_id)
    ) {
      notify_progress()
      break
    }

    score <- NA_real_
    std_error <- NA_real_
    n_evaluated <- 0L
    n_errors <- if (is.null(eval_result)) 1L else 0L

    if (!is.null(eval_result) && !isTRUE(active_trial$result_applied)) {
      if (!resumable_eval) {
        record_eval_result_outcomes(budget, eval_result, budget_stage)
        optimizer_budget_count_trial(budget, budget_stage, unit_id)
        optimizer_budget_complete_unit(budget, unit_id)
      }
      score <- eval_result@mean_score
      std_error <- eval_result@std_error
      n_evaluated <- eval_result@n_evaluated
      n_errors <- eval_result@n_errors
      stats[[candidate_idx]]$count <- stats[[candidate_idx]]$count + 1L
      stats[[candidate_idx]]$last_score <- score
      if (is.na(stats[[candidate_idx]]$mean_score)) {
        stats[[candidate_idx]]$mean_score <- score
      } else {
        previous <- stats[[candidate_idx]]$mean_score
        count <- stats[[candidate_idx]]$count
        stats[[candidate_idx]]$mean_score <- previous +
          (score - previous) / count
      }
      if (!is.na(score) && score > best_any$score) {
        best_any <- list(
          score = score,
          candidate_index = as.integer(candidate_idx),
          trial_index = as.integer(trial_idx)
        )
      }
      if (eval_type == "full" && !is.na(score) && score > best_full$score) {
        best_full <- list(
          score = score,
          candidate_index = as.integer(candidate_idx),
          trial_index = as.integer(trial_idx)
        )
      }
      active_trial$result_applied <- TRUE
      # Persist the search-statistics transition independently from logging.
      # If the process dies before the trial record is constructed, resume
      # must know not to apply the completed evaluation a second time.
      notify_progress()
    } else if (!is.null(eval_result)) {
      score <- eval_result@mean_score
      std_error <- eval_result@std_error
      n_evaluated <- eval_result@n_evaluated
      n_errors <- eval_result@n_errors
    }

    if (!is.null(trial_log) && !is.null(eval_result)) {
      if (is.null(active_trial$trial_record)) {
        trial <- create_trial(
          optimizer_name = "MIPROv2",
          params = c(
            candidate$params %||% list(),
            list(eval_type = eval_type)
          ),
          trial_id = active_trial$trial_id
        )
        trial <- complete_trial(trial, eval_result, notes = eval_type)
        active_trial$trial_record <-
          discrete_bo_checkpoint_trial_record(trial)
        # The durable optimizer checkpoint must know the exact record and
        # whether its result has already updated search statistics before the
        # append. A crash after append can then replay without duplication.
        notify_progress()
      } else {
        trial <- discrete_bo_restore_trial_record(
          active_trial$trial_record
        )
      }
      # The canonical JSONL is part of resumability, not optional telemetry.
      # Immutable-ID conflicts and journal integrity failures must leave the
      # active checkpoint cursor intact for explicit recovery.
      trial_log$add_trial(trial)
    }

    trial_history[[length(trial_history) + 1L]] <- list(
      trial_index = as.integer(trial_idx),
      candidate_id = candidate$id %||% NA_character_,
      eval_type = eval_type,
      mean_score = score,
      std_error = std_error,
      n_evaluated = as.integer(n_evaluated),
      n_errors = as.integer(n_errors),
      demo_id = candidate$demo_id %||% NA_character_,
      instruction_id = candidate$instruction_id %||% NA_character_
    )
    next_trial <- as.integer(trial_idx + 1L)
    active_trial <- NULL
    notify_progress()
  }

  trial_history_tbl <- discrete_bo_history_table(trial_history, track_stats)
  selected_state <- best_full
  best_candidate <- discrete_bo_candidate(candidates, selected_state)
  if (is.null(best_candidate)) {
    selected_state <- best_any
    best_candidate <- discrete_bo_candidate(candidates, selected_state)
    if (!is.null(best_candidate)) {
      cli::cli_warn(
        c(
          "No candidate received full evaluation",
          "i" = "Using best candidate from minibatch evaluations only",
          "i" = "Consider running with more trials to get full evaluations"
        ),
        class = "dsprrr_mipro_fallback_warning"
      )
    }
  }

  final_state <- discrete_bo_state(
    stats,
    trial_history,
    best_full,
    best_any,
    next_trial,
    active_trial
  )
  budget_summary <- optimizer_budget_summary(budget)
  list(
    best_candidate = best_candidate,
    best_score = if (is.finite(selected_state$score)) {
      selected_state$score
    } else {
      NA_real_
    },
    best_trial = if (
      is.null(selected_state$trial_index) ||
        is.na(selected_state$trial_index)
    ) {
      NULL
    } else {
      selected_state$trial_index
    },
    trial_history = trial_history_tbl,
    candidate_stats = stats,
    budget_summary = budget_summary,
    stop_reason = budget_summary$stop_reason,
    error_count = budget_summary$total_errors,
    resume_state = final_state,
    complete = next_trial > max_trials && is.null(active_trial)
  )
}
