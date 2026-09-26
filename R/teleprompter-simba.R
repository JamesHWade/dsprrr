# SIMBA Teleprompter
#
# Stochastic introspective mini-batch ascent, adapted to dsprrr.

#' SIMBA: stochastic introspective mini-batch ascent
#'
#' @include teleprompter.R optimizer-core.R optimizer-logging.R
#'
#' @description
#' `SIMBA()` finds the training rows the program struggles with, turns them
#' into an instruction rule and demonstrations, and keeps the change when the
#' program's score improves. Each step works on a random mini-batch, which is
#' where the name (stochastic introspective mini-batch ascent) comes from.
#'
#' @details
#' The program is first scored on `valset` (or `trainset`). Each of up to
#' `max_steps` steps then:
#'
#' 1. Samples `bsize` rows of `trainset` and runs the current program on them
#'    `num_candidates` times.
#' 2. Rates each row's difficulty as one minus its mean score plus the share
#'    of runs that disagree with the most common output, and takes the
#'    hardest rows (at most `max_demos`).
#' 3. Asks `prompt_model` for a rule based on those rows and appends it to the
#'    instructions. Without `prompt_model`, the rule is the first hard row
#'    written out, as in `SIMBA rule: question: ..., expected: ...`.
#' 4. Adds the hard rows as demonstrations, keeping the latest `max_demos`.
#' 5. Scores the changed program and keeps it only if the score improves.
#'
#' The search stops at the first step that does not improve the score. With
#' dsprrr's response cache on (the default), the repeated runs in step 1
#' return the same cached output, so difficulty reduces to one minus the mean
#' score; call `configure_cache(enable = FALSE)` to measure disagreement.
#'
#' ## Differences from DSPy's SIMBA
#'
#' This is an adapted implementation. It mines hard examples and asks a model
#' for improvement rules, but it does not reproduce every detail of DSPy's
#' SIMBA, such as introspection over trajectories of several candidate
#' programs. Expect similar behavior, not identical results.
#'
#' @param metric A metric function (required), such as
#'   `metric_exact_match(field = "answer")`. Its `field` also names the column
#'   that supplies the demonstrations' outputs.
#' @param metric_threshold Accepted for consistency with the other optimizers
#'   (see [Teleprompter()]). SIMBA prints it but does not use it.
#' @param max_errors Integer; stop after this many consecutive failed
#'   evaluations when [compile()] gets no `control` (default `5L`).
#' @param bsize Integer mini-batch size (default `32L`, capped at the number
#'   of training rows).
#' @param num_candidates Integer number of runs per mini-batch (default `6L`).
#' @param max_steps Integer maximum number of steps (default `8L`).
#' @param max_demos Integer maximum number of demonstrations kept, and of hard
#'   rows taken per step (default `4L`).
#' @param prompt_model Optional ellmer Chat that writes the rules. SIMBA calls
#'   its `$chat()` method directly, so its conversation history grows during
#'   the run. `NULL` (the default) uses the fixed rule described in Details.
#' @param seed Seed for the mini-batch samples (default `0L`), or `NULL`.
#' @param log_dir Directory for a [TrialLog] with one trial per kept step, or
#'   `NULL` (the default).
#'
#' @return A `SIMBA` object to pass to [compile()].
#' @family teleprompters
#' @export
#'
#' @examples
#' tp <- SIMBA(
#'   metric = metric_exact_match(field = "answer"),
#'   bsize = 8L,
#'   num_candidates = 4L,
#'   max_steps = 4L,
#'   max_demos = 2L
#' )
#' tp
#'
#' \dontrun{
#' qa <- module(signature("question -> answer"))
#' trainset <- data.frame(
#'   question = c("Capital of France?", "Capital of Peru?", "Capital of Chad?"),
#'   answer = c("Paris", "Lima", "N'Djamena")
#' )
#' tp <- SIMBA(
#'   metric = metric_exact_match(field = "answer"),
#'   bsize = 8L,
#'   prompt_model = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' compiled <- compile(
#'   qa,
#'   tp,
#'   trainset,
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' optimization_result(compiled)$extensions$simba$rules
#' }
SIMBA <- S7::new_class(
  "SIMBA",
  parent = Teleprompter,
  properties = list(
    bsize = S7::new_property(
      S7::class_integer,
      default = 32L,
      validator = function(value) {
        if (value < 1) {
          return("bsize must be at least 1")
        }
        NULL
      }
    ),
    num_candidates = S7::new_property(
      S7::class_integer,
      default = 6L,
      validator = function(value) {
        if (value < 1) {
          return("num_candidates must be at least 1")
        }
        NULL
      }
    ),
    max_steps = S7::new_property(
      S7::class_integer,
      default = 8L,
      validator = function(value) {
        if (value < 1) {
          return("max_steps must be at least 1")
        }
        NULL
      }
    ),
    max_demos = S7::new_property(
      S7::class_integer,
      default = 4L,
      validator = function(value) {
        if (value < 0) {
          return("max_demos must be non-negative")
        }
        NULL
      }
    ),
    prompt_model = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is_ellmer_chat(value)) {
          return(NULL)
        }
        "prompt_model must be NULL or an ellmer Chat R6 object"
      }
    ),
    seed = S7::new_property(
      S7::class_any,
      default = 0L,
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

#' Compile method for SIMBA
#' @noRd
compile_simba <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  control = NULL,
  ...
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("SIMBA currently only supports Module objects")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("{.arg trainset} must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort("SIMBA requires a metric function")
  }

  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control
  )
  optimizer_require_ledger_only_checkpoint(control, "SIMBA")
  budget <- new_optimizer_budget(control)

  trial_log <- if (!is.null(control@log_dir)) {
    TrialLog$new(
      optimizer_name = "SIMBA",
      log_dir = control@log_dir
    )
  } else {
    NULL
  }

  # Save and restore RNG state to avoid side effects

  if (!is.null(teleprompter@seed) && !is.na(teleprompter@seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv())) {
      get(".Random.seed", envir = globalenv())
    } else {
      NULL
    }
    set.seed(teleprompter@seed)
    on.exit(
      {
        if (is.null(old_seed)) {
          if (exists(".Random.seed", envir = globalenv())) {
            rm(".Random.seed", envir = globalenv())
          }
        } else {
          assign(".Random.seed", old_seed, envir = globalenv())
        }
      },
      add = TRUE
    )
  }

  input_names <- get_input_names(program$signature)
  # First try to get field from metric, then fall back to auto-detection
  output_col <- get_metric_field(teleprompter@metric) %||%
    find_output_column(trainset, input_names)

  if (is.null(output_col)) {
    cli::cli_warn(
      c(
        "No output column found in trainset",
        "i" = "Metric will receive NULL as expected value for all examples"
      ),
      class = "dsprrr_missing_output_column"
    )
  }

  eval_dataset <- valset %||% trainset
  best_program <- copy_module(program)
  best_eval <- optimizer_eval_candidate(
    best_program,
    eval_dataset,
    metric = teleprompter@metric,
    .llm = .llm,
    control = control,
    budget = budget,
    stage = "simba_baseline",
    unit_id = "simba:baseline",
    ...
  )
  best_score <- best_eval@mean_score
  best_complete <- optimizer_budget_unit_completed(budget, "simba:baseline")
  trial_records <- list(tibble::tibble(
    trial_id = 1L,
    step = 0L,
    score = best_score,
    complete = best_complete,
    rule = NA_character_,
    demos_added = 0L
  ))

  rules <- character()
  added_demos <- list()

  for (step in seq_len(teleprompter@max_steps)) {
    if (optimizer_budget_stopped(budget)) {
      break
    }
    minibatch <- sample_dataset(
      trainset,
      n = min(teleprompter@bsize, nrow(trainset)),
      seed = if (!is.null(teleprompter@seed)) {
        teleprompter@seed + step
      } else {
        NULL
      }
    )

    variability <- simba_variability(
      best_program,
      minibatch,
      output_col = output_col,
      metric = teleprompter@metric,
      num_candidates = teleprompter@num_candidates,
      .llm = .llm,
      control = control,
      budget = budget,
      stage = paste0("simba_variability_", step),
      unit_prefix = paste0("simba:step:", step, ":variability")
    )

    hard_examples <- select_simba_hard_examples(
      minibatch,
      variability,
      max_examples = max(1L, min(teleprompter@max_demos, nrow(minibatch)))
    )

    if (nrow(hard_examples) == 0) {
      cli::cli_warn("No hard examples identified; stopping early")
      break
    }

    rule_text <- generate_simba_rule(
      teleprompter@prompt_model,
      hard_examples,
      input_names = input_names,
      output_col = output_col,
      budget = budget,
      unit_id = paste0("simba:step:", step, ":rule")
    )

    candidate <- copy_module(best_program)
    if (!is.null(rule_text) && nzchar(rule_text)) {
      # Create new immutable Signature with updated instructions (don't mutate S7)
      candidate$signature <- Signature(
        inputs = candidate$signature@inputs,
        output_type = candidate$signature@output_type,
        instructions = paste(
          candidate$signature@instructions,
          rule_text,
          sep = "\n\n"
        )
      )
    }

    new_demos <- list()
    if (!is.null(output_col) && teleprompter@max_demos > 0) {
      new_demos <- format_trainset_as_demos(
        hard_examples,
        candidate$signature,
        output_col = output_col
      )
      for (i in seq_along(new_demos)) {
        new_demos[[i]]$source <- "simba"
        new_demos[[i]]$step <- step
      }
      candidate$demos <- apply_simba_demos(
        candidate$demos,
        new_demos,
        teleprompter@max_demos
      )
    }

    if (length(new_demos) == 0 && (is.null(rule_text) || !nzchar(rule_text))) {
      cli::cli_warn("No rule or demos generated; stopping early")
      break
    }

    candidate_unit_id <- paste0("simba:step:", step, ":candidate")
    eval_result <- optimizer_eval_candidate(
      candidate,
      eval_dataset,
      metric = teleprompter@metric,
      .llm = .llm,
      control = control,
      budget = budget,
      stage = "simba_candidate",
      unit_id = candidate_unit_id,
      ...
    )
    score <- eval_result@mean_score
    candidate_complete <- optimizer_budget_unit_completed(
      budget,
      candidate_unit_id
    )
    trial_records[[length(trial_records) + 1L]] <- tibble::tibble(
      trial_id = length(trial_records) + 1L,
      step = step,
      score = score,
      complete = candidate_complete,
      rule = rule_text %||% NA_character_,
      demos_added = length(new_demos)
    )

    improved <- candidate_complete &&
      !is.na(score) &&
      (!best_complete || is.na(best_score) || score > best_score)

    if (improved) {
      best_program <- candidate
      best_score <- score
      best_complete <- TRUE
      if (!is.null(rule_text) && nzchar(rule_text)) {
        rules <- c(rules, rule_text)
      }
      if (length(new_demos) > 0) {
        added_demos <- c(added_demos, new_demos)
      }
    } else {
      cli::cli_alert_info("No improvement at step {step}; stopping early")
      break
    }

    if (!is.null(trial_log) && candidate_complete) {
      trial <- create_trial(
        optimizer_name = "SIMBA",
        params = list(
          step = step,
          bsize = teleprompter@bsize,
          num_candidates = teleprompter@num_candidates,
          hard_examples = nrow(hard_examples),
          rule = rule_text,
          demos_added = length(new_demos)
        )
      )
      trial <- complete_trial(trial, eval_result, compiled_artifact_ref = NULL)
      trial_log$add_trial(trial)
    }
  }

  budget_summary <- optimizer_budget_summary(budget)
  trials <- do.call(rbind, trial_records)
  best_trial <- if (nrow(trials) > 0L) {
    which.max(replace(trials$score, is.na(trials$score), -Inf))
  } else {
    NULL
  }
  record_optimization_result(
    best_program,
    optimizer = "SIMBA",
    status = if (optimizer_budget_stopped(budget)) "partial" else "completed",
    baseline_score = best_eval@mean_score,
    best_score = best_score,
    best_trial = best_trial,
    best_params = list(
      rules_added = length(rules),
      demos_added = length(added_demos)
    ),
    trials = trials,
    budget = budget_summary,
    stop_reason = optimization_stop_reason(budget_summary),
    extensions = list(
      steps = teleprompter@max_steps,
      best_complete = best_complete,
      rules = rules,
      demos = added_demos
    )
  )

  best_program
}

simba_variability <- function(
  program,
  minibatch,
  output_col,
  metric,
  num_candidates,
  .llm = NULL,
  control = NULL,
  budget = NULL,
  stage = "simba_variability",
  unit_prefix = "simba:variability"
) {
  outputs <- vector("list", num_candidates)
  score_outputs <- vector("list", num_candidates)
  successful_runs <- 0L

  for (i in seq_len(num_candidates)) {
    if (!is.null(budget) && optimizer_budget_stopped(budget)) {
      break
    }
    results <- tryCatch(
      {
        optimizer_eval_candidate(
          program,
          minibatch,
          metric = metric,
          .llm = .llm,
          control = control,
          budget = budget,
          stage = stage,
          unit_id = paste0(unit_prefix, ":", i)
        )
      },
      error = function(e) {
        cli::cli_warn(
          c(
            "SIMBA candidate run {i}/{num_candidates} failed",
            "x" = conditionMessage(e),
            "i" = "Continuing with remaining candidates"
          ),
          class = "dsprrr_simba_candidate_warning"
        )
        NULL
      }
    )

    if (!is.null(results) && results@n_evaluated > 0L) {
      predictions_i <- rep(list(NA), nrow(minibatch))
      scores_i <- rep(NA_real_, nrow(minibatch))
      row_ids <- results@examples$row_id
      predictions_i[row_ids] <- results@examples$predicted
      scores_i[row_ids] <- results@examples$score
      outputs[[i]] <- predictions_i
      score_outputs[[i]] <- scores_i
      successful_runs <- successful_runs + 1L
    }
  }

  # Check if we have enough successful candidates
  if (successful_runs == 0L) {
    if (!is.null(budget) && optimizer_budget_stopped(budget)) {
      return(tibble::tibble(
        row_id = integer(),
        variability = numeric(),
        mean_score = numeric(),
        difficulty = numeric()
      ))
    }
    cli::cli_abort(
      c(
        "All {num_candidates} SIMBA candidate runs failed",
        "i" = "Check LLM configuration and network connectivity"
      ),
      class = "dsprrr_simba_all_candidates_failed"
    )
  }

  # Filter out NULL outputs from failed runs
  valid_outputs <- Filter(Negate(is.null), outputs)
  valid_scores <- Filter(Negate(is.null), score_outputs)

  variability <- vector("list", nrow(minibatch))
  for (i in seq_len(nrow(minibatch))) {
    predictions <- lapply(valid_outputs, function(x) x[[i]])
    normalized <- vapply(predictions, normalize_simba_output, character(1))
    variation <- if (successful_runs > 1) {
      freqs <- table(normalized)
      1 - max(freqs) / successful_runs
    } else {
      0
    }

    mean_score <- NA_real_
    if (!is.null(output_col)) {
      row <- minibatch[i, , drop = FALSE]
      scores <- vapply(valid_scores, function(x) x[[i]], numeric(1))
      mean_score <- if (all(is.na(scores))) {
        NA_real_
      } else {
        mean(scores, na.rm = TRUE)
      }
    }

    difficulty <- if (is.na(mean_score)) {
      variation
    } else {
      (1 - mean_score) + variation
    }

    variability[[i]] <- tibble::tibble(
      row_id = i,
      variability = variation,
      mean_score = mean_score,
      difficulty = difficulty
    )
  }

  do.call(rbind, variability)
}

select_simba_hard_examples <- function(minibatch, variability, max_examples) {
  if (nrow(variability) == 0) {
    return(minibatch[0, , drop = FALSE])
  }

  ordered <- order(variability$difficulty, decreasing = TRUE)
  ordered <- ordered[seq_len(min(max_examples, length(ordered)))]
  minibatch[variability$row_id[ordered], , drop = FALSE]
}

generate_simba_rule <- function(
  prompt_model,
  hard_examples,
  input_names,
  output_col,
  budget = NULL,
  unit_id = "simba:rule"
) {
  if (nrow(hard_examples) == 0) {
    return(NULL)
  }

  example_lines <- vector("character", nrow(hard_examples))
  for (i in seq_len(nrow(hard_examples))) {
    row <- hard_examples[i, , drop = FALSE]
    inputs <- vapply(
      input_names,
      function(name) {
        if (name %in% names(row)) {
          paste0(name, ": ", row[[name]])
        } else {
          ""
        }
      },
      character(1)
    )
    inputs <- inputs[nzchar(inputs)]
    expected <- if (!is.null(output_col) && output_col %in% names(row)) {
      paste0("expected: ", row[[output_col]])
    } else {
      "expected: (unknown)"
    }
    example_lines[i] <- paste(c(inputs, expected), collapse = ", ")
  }

  prompt <- paste(
    "You are optimizing an LLM program.",
    "Given these hard examples, provide a concise instruction rule to improve performance.",
    "Return only the rule text.",
    paste(example_lines, collapse = "\n"),
    sep = "\n\n"
  )

  rule <- NULL
  used_fallback <- FALSE

  if (!is.null(budget) && !is.null(prompt_model)) {
    request <- optimizer_budgeted_provider_call(
      budget = budget,
      model = prompt_model,
      stage = "simba_rule",
      unit_id = unit_id,
      call = function() {
        generate_simba_rule(
          prompt_model,
          hard_examples,
          input_names,
          output_col,
          budget = NULL
        )
      },
      success = function(value, condition) {
        is.null(condition) && is.character(value) && nzchar(value)
      }
    )
    return(request$value)
  }

  if (!is.null(prompt_model)) {
    rule <- tryCatch(
      prompt_model$chat(prompt),
      error = function(e) {
        cli::cli_warn(
          c(
            "SIMBA prompt_model Chat$chat() call failed",
            "x" = conditionMessage(e),
            "i" = "Falling back to example-based rule"
          ),
          class = "dsprrr_simba_rule_warning"
        )
        NULL
      }
    )
  }

  if (is.null(rule) || !nzchar(rule)) {
    used_fallback <- TRUE
    rule <- paste(
      "SIMBA rule:",
      example_lines[1]
    )
    if (!is.null(prompt_model)) {
      cli::cli_alert_info(
        "Using example-based rule (prompt_model returned empty)"
      )
    }
  }

  as.character(rule)
}

apply_simba_demos <- function(existing_demos, new_demos, max_demos) {
  combined <- c(existing_demos, new_demos)
  if (length(combined) > max_demos) {
    combined <- utils::tail(combined, max_demos)
  }
  combined
}

normalize_simba_output <- function(output) {
  if (is.null(output)) {
    return(NA_character_)
  }
  if (is.list(output) && !is.data.frame(output)) {
    return(tryCatch(
      jsonlite::toJSON(output, auto_unbox = TRUE, null = "null"),
      error = function(e) {
        cli::cli_warn(
          c(
            "Could not serialize SIMBA output to JSON",
            "x" = conditionMessage(e),
            "i" = "Using fallback string representation"
          ),
          class = "dsprrr_simba_serialize_warning"
        )
        paste0("<non-serializable: ", class(output)[1], ">")
      }
    ))
  }
  tryCatch(
    as.character(output),
    error = function(e) {
      cli::cli_warn(
        c(
          "Could not convert SIMBA output to character",
          "x" = conditionMessage(e)
        ),
        class = "dsprrr_simba_convert_warning"
      )
      paste0("<unconvertible: ", class(output)[1], ">")
    }
  )
}

simba_safe_metric <- function(metric, prediction, row) {
  tryCatch(
    {
      score <- metric(prediction, row)
      if (is.list(score) && "score" %in% names(score)) {
        score <- score$score
      }
      if (is.logical(score)) {
        as.numeric(score)
      } else if (is.numeric(score)) {
        score
      } else {
        cli::cli_warn(
          c(
            "Metric returned unexpected type",
            "i" = "Expected numeric or logical, got {.cls {class(score)}}",
            "i" = "Treating as NA"
          ),
          class = "dsprrr_simba_metric_type_warning"
        )
        NA_real_
      }
    },
    error = function(e) {
      cli::cli_warn(
        c(
          "Metric evaluation failed in SIMBA variability calculation",
          "x" = conditionMessage(e),
          "i" = "This score will be treated as NA"
        ),
        class = "dsprrr_simba_metric_warning"
      )
      NA_real_
    }
  )
}

# Print a SIMBA object through its S7 method.
print_simba <- function(x, ...) {
  cli::cli_h3("SIMBA Teleprompter")

  cli::cli_text("{.field bsize}: {x@bsize}")
  cli::cli_text("{.field num_candidates}: {x@num_candidates}")
  cli::cli_text("{.field max_steps}: {x@max_steps}")
  cli::cli_text("{.field max_demos}: {x@max_demos}")

  if (!is.null(x@metric_threshold)) {
    cli::cli_text("{.field metric_threshold}: {x@metric_threshold}")
  }

  if (!is.null(x@seed)) {
    cli::cli_text("{.field seed}: {x@seed}")
  }

  invisible(x)
}
