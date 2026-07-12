# GEPA Teleprompter
#
# GEPA-lite reflective prompt evolution optimizer.

#' GEPA Teleprompter
#'
#' @include teleprompter.R optimizer-core.R optimizer-logging.R pareto.R
#'
#' @description
#' Genetic/evolutionary prompt optimizer that evolves instruction variants
#' using reflection on failed examples.
#'
#' @details
#' ## Feedback metrics
#'
#' GEPA works best with feedback-aware metrics created via
#' [metric_with_feedback()]. When the metric returns
#' `list(score = , feedback = )`, the textual feedback for failed examples
#' is included in the reflection prompt, giving the reflection LLM concrete
#' guidance on *why* an output was wrong — the key mechanism in the GEPA
#' paper ("GEPA: Reflective Prompt Evolution Can Outperform RL",
#' Agrawal et al., 2025). Plain numeric metrics still work; reflection then
#' sees only inputs, expected, and predicted values.
#'
#' ## Differences from DSPy's GEPA
#'
#' This is an adapted ("GEPA-lite") implementation. It shares the core
#' ideas — reflective mutation of instructions guided by failures and
#' feedback, plus Pareto-frontier selection over multiple metrics — but
#' uses a fixed population/generations evolutionary loop rather than
#' DSPy's budget-driven candidate search, and does not yet support
#' per-component selection in multi-step programs or inference-time
#' search. Expect qualitatively similar behavior, not identical results.
#'
#' @param metrics Named list of metric functions for evaluation.
#' @param metric A single metric function (fallback when `metrics` is NULL).
#' @param metric_threshold Minimum score for an example to be considered successful.
#' @param max_errors Maximum number of errors allowed during optimization.
#' @param population_size Size of the population. Default is 20.
#' @param generations Number of generations to run. Default is 10.
#' @param mutation_rate Probability of mutation. Default is 0.1.
#' @param crossover_rate Probability of crossover. Default is 0.7.
#' @param selection Selection strategy: "pareto" or "current_best".
#' @param seed Random seed for reproducibility.
#' @param log_dir Optional directory for trial logging.
#' @param verbose Whether to print progress messages.
#' @param track_stats Whether to record generation statistics.
#'
#' @examples
#' # A small GEPA run: 6 candidates evolved over 2 generations
#' tp <- GEPA(
#'   metric = metric_exact_match(field = "answer"),
#'   population_size = 6L,
#'   generations = 2L,
#'   seed = 42
#' )
#'
#' \dontrun{
#' # Feedback-aware metrics give the reflection step concrete guidance.
#' # During evaluation the metric receives the full expected row, so
#' # extract the target field explicitly:
#' feedback_metric <- metric_with_feedback(
#'   function(prediction, expected) {
#'     if (identical(as.character(prediction), expected$answer)) {
#'       list(score = 1, feedback = "Correct.")
#'     } else {
#'       list(
#'         score = 0,
#'         feedback = paste0(
#'           "Expected '",
#'           expected$answer,
#'           "' but got '",
#'           prediction,
#'           "'."
#'         )
#'       )
#'     }
#'   },
#'   field = "answer"
#' )
#' tp <- GEPA(metric = feedback_metric, seed = 42)
#'
#' qa <- module(signature("question -> answer"), type = "predict")
#' trainset <- dsp_trainset(
#'   question = c("What is 2 + 2?", "What is the capital of France?"),
#'   answer = c("4", "Paris")
#' )
#' optimized <- compile(tp, qa, trainset)
#' }
#' @export
GEPA <- S7::new_class(
  "GEPA",
  parent = Teleprompter,
  properties = list(
    metrics = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (!is.list(value)) {
            return("metrics must be a list of metric functions")
          }
          if (!all(vapply(value, is.function, logical(1)))) {
            return("metrics must contain only functions")
          }
        }
        NULL
      }
    ),
    population_size = S7::new_property(
      S7::class_integer,
      default = 20L,
      validator = function(value) {
        if (value < 2L) {
          return("population_size must be at least 2")
        }
        NULL
      }
    ),
    generations = S7::new_property(
      S7::class_integer,
      default = 10L,
      validator = function(value) {
        if (value < 1L) {
          return("generations must be at least 1")
        }
        NULL
      }
    ),
    mutation_rate = S7::new_property(
      S7::class_numeric,
      default = 0.1,
      validator = function(value) {
        if (length(value) != 1 || value < 0 || value > 1) {
          return("mutation_rate must be between 0 and 1")
        }
        NULL
      }
    ),
    crossover_rate = S7::new_property(
      S7::class_numeric,
      default = 0.7,
      validator = function(value) {
        if (length(value) != 1 || value < 0 || value > 1) {
          return("crossover_rate must be between 0 and 1")
        }
        NULL
      }
    ),
    selection = S7::new_property(
      S7::class_character,
      default = "pareto",
      validator = function(value) {
        if (!value %in% c("pareto", "current_best")) {
          return("selection must be 'pareto' or 'current_best'")
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
        if (!is.null(value)) {
          if (!is.character(value) || length(value) != 1) {
            return("log_dir must be a single character string or NULL")
          }
        }
        NULL
      }
    ),
    verbose = S7::new_property(
      S7::class_logical,
      default = TRUE
    ),
    track_stats = S7::new_property(
      S7::class_logical,
      default = TRUE
    )
  )
)

#' Compile method for GEPA
#' @noRd
compile_gepa <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  ...
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("GEPA currently only supports Module objects")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("{.arg trainset} must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  metrics <- teleprompter@metrics
  if (is.null(metrics)) {
    if (!is.null(teleprompter@metric)) {
      metrics <- list(quality = teleprompter@metric)
    } else {
      cli::cli_abort("GEPA requires a metrics list or metric function")
    }
  }

  metric_names <- names(metrics)
  if (is.null(metric_names) || any(metric_names == "")) {
    metric_names <- paste0("metric_", seq_along(metrics))
  }

  dataset <- valset %||% trainset

  if (!is.null(teleprompter@seed)) {
    set.seed(teleprompter@seed)
  }

  control <- optimizer_control(
    seed = teleprompter@seed,
    max_errors = teleprompter@max_errors,
    log_dir = teleprompter@log_dir,
    progress = teleprompter@verbose
  )

  trial_log <- if (!is.null(teleprompter@log_dir)) {
    TrialLog$new(optimizer_name = "GEPA", log_dir = teleprompter@log_dir)
  } else {
    NULL
  }

  base_instructions <- program$signature@instructions
  population <- vector("list", teleprompter@population_size)
  population[[1]] <- base_instructions
  for (i in 2:teleprompter@population_size) {
    population[[i]] <- gepa_mutate_instruction(
      base_instructions,
      failed_examples = list(),
      .llm = .llm,
      verbose = teleprompter@verbose
    )
  }

  all_generations <- list()
  selectable_records <- list()
  budget <- new_optimizer_budget(control)
  best_record <- NULL

  for (generation in seq_len(teleprompter@generations)) {
    if (teleprompter@verbose) {
      cli::cli_alert_info(
        "GEPA generation {generation}/{teleprompter@generations}"
      )
    }

    records <- list()

    for (i in seq_along(population)) {
      if (optimizer_budget_stopped(budget)) {
        break
      }

      instructions <- population[[i]]
      scores <- rep(NA_real_, length(metrics))
      failed_examples <- list()
      primary_eval <- NULL
      last_eval <- NULL
      completed_metrics <- 0L

      for (m in seq_along(metrics)) {
        if (optimizer_budget_stopped(budget)) {
          break
        }

        candidate <- copy_module(program)
        candidate$apply_optimization_params(list(instructions = instructions))
        eval_result <- eval_program(
          candidate,
          dataset,
          metric = metrics[[m]],
          .llm = .llm,
          control = control
        )

        if (!S7::S7_inherits(eval_result, EvalResult)) {
          cli::cli_abort(c(
            "eval_program returned invalid result",
            "i" = "Expected EvalResult, got {.cls {class(eval_result)}}"
          ))
        }

        last_eval <- eval_result
        scores[m] <- eval_result@mean_score
        completed_metrics <- m
        record_eval_result_outcomes(
          budget,
          eval_result,
          stage = paste0("gepa_metric_", m)
        )

        if (m == 1) {
          primary_eval <- eval_result
          failed_examples <- gepa_failed_examples(
            eval_result,
            dataset,
            program$signature,
            threshold = teleprompter@metric_threshold
          )
        }
      }

      record <- list(
        instructions = instructions,
        scores = stats::setNames(scores, metric_names),
        failed_examples = failed_examples,
        generation = generation,
        completed_metrics = completed_metrics,
        complete = completed_metrics == length(metrics) && !anyNA(scores)
      )

      records[[length(records) + 1L]] <- record

      if (
        !is.null(trial_log) &&
          isTRUE(record$complete) &&
          !is.null(last_eval)
      ) {
        trial <- create_trial(
          optimizer_name = "GEPA",
          params = list(
            generation = generation,
            index = i,
            instructions = instructions
          )
        )
        trial <- start_trial(trial)
        trial <- complete_trial(trial, primary_eval %||% last_eval)
        trial_log$add_trial(trial)
      }

      if (optimizer_budget_stopped(budget)) {
        break
      }
    }

    complete_records <- Filter(
      function(record) isTRUE(record$complete),
      records
    )
    if (length(complete_records) > 0L) {
      selectable_records <- c(selectable_records, complete_records)

      scores_matrix <- do.call(
        rbind,
        lapply(selectable_records, function(rec) rec$scores)
      )

      if (teleprompter@selection == "pareto" && length(metrics) > 1) {
        ranks <- pareto_ranks(scores_matrix)
        crowding <- pareto_crowding_distance(scores_matrix, ranks)
        best_idx <- select_pareto_best(scores_matrix, ranks, crowding)
      } else {
        best_idx <- which.max(scores_matrix[, 1])
      }

      best_record <- selectable_records[[best_idx]]
    }

    if (isTRUE(teleprompter@track_stats)) {
      all_generations[[generation]] <- list(
        generation = generation,
        population = complete_records
      )
    }

    if (optimizer_budget_stopped(budget)) {
      break
    }

    if (generation == teleprompter@generations) {
      break
    }

    if (length(complete_records) == 0L) {
      break
    }

    population <- gepa_next_generation(
      complete_records,
      teleprompter@population_size,
      teleprompter@mutation_rate,
      teleprompter@crossover_rate,
      teleprompter@selection,
      .llm,
      verbose = teleprompter@verbose
    )
  }

  optimized <- copy_module(program)
  if (!is.null(best_record)) {
    optimized$apply_optimization_params(list(
      instructions = best_record$instructions
    ))
  } else {
    cli::cli_warn(c(
      "GEPA optimization failed to produce any valid candidates",
      "!" = "Returning unmodified program",
      "i" = "All evaluations may have failed or max_errors was exceeded immediately"
    ))
  }

  final_scores <- if (!is.null(best_record)) {
    best_record$scores
  } else {
    stats::setNames(rep(NA_real_, length(metrics)), metric_names)
  }

  frontier <- list()
  if (length(selectable_records) > 0L) {
    scores_matrix <- do.call(
      rbind,
      lapply(selectable_records, function(rec) rec$scores)
    )
    frontier_idx <- pareto_frontier(scores_matrix)
    frontier <- lapply(selectable_records[frontier_idx], function(rec) {
      list(
        instructions = rec$instructions,
        scores = rec$scores
      )
    })
  }

  optimized$config$compiled <- TRUE
  optimized$config$teleprompter <- "GEPA"
  budget_summary <- optimizer_budget_summary(budget)
  optimized$config$optimizer <- list(
    selection = teleprompter@selection,
    population_size = teleprompter@population_size,
    generations = teleprompter@generations,
    best_scores = final_scores,
    pareto_frontier = frontier,
    all_generations = all_generations,
    error_count = budget_summary$total_errors,
    budget_summary = budget_summary,
    stop_reason = budget_summary$stop_reason
  )

  if (!is.null(trial_log)) {
    trial_log$save()
  }

  optimized
}

gepa_failed_examples <- function(
  eval_result,
  dataset,
  signature,
  threshold = NULL
) {
  threshold <- threshold %||% 1
  input_names <- get_input_names(signature)
  output_col <- find_output_column(dataset, input_names)

  scores <- eval_result@examples$score
  feedbacks <- eval_result@examples$feedback %||%
    rep(NA_character_, length(scores))
  failed_idx <- which(is.na(scores) | scores < threshold)

  if (length(failed_idx) == 0) {
    return(list())
  }

  failed_idx <- failed_idx[seq_len(min(length(failed_idx), 5))]

  lapply(failed_idx, function(i) {
    row <- dataset[i, , drop = FALSE]
    inputs <- row[input_names]
    expected <- if (!is.null(output_col) && output_col %in% names(row)) {
      row[[output_col]]
    } else {
      NA
    }
    list(
      inputs = inputs,
      expected = expected,
      predicted = eval_result@examples$predicted[[i]] %||% NA,
      feedback = feedbacks[[i]] %||% NA_character_
    )
  })
}

gepa_next_generation <- function(
  records,
  population_size,
  mutation_rate,
  crossover_rate,
  selection,
  .llm,
  verbose = FALSE
) {
  scores_matrix <- do.call(rbind, lapply(records, function(rec) rec$scores))
  if (selection == "pareto" && ncol(scores_matrix) > 1) {
    ranks <- pareto_ranks(scores_matrix)
    crowding <- pareto_crowding_distance(scores_matrix, ranks)
  } else {
    # For single-objective: rank by primary score (higher score = lower rank)
    primary_scores <- scores_matrix[, 1]
    # Handle NA scores by giving them worst rank
    primary_scores[is.na(primary_scores)] <- -Inf
    # Rank: 1 = best (highest score), higher rank = worse
    ranks <- rank(-primary_scores, ties.method = "min")
    # Use score as crowding for tiebreaking (higher is better)
    crowding <- primary_scores
    crowding[is.infinite(crowding)] <- 0
  }

  population <- vector("list", population_size)

  for (i in seq_len(population_size)) {
    parent1 <- gepa_select_parent(records, ranks, crowding)
    parent2 <- gepa_select_parent(records, ranks, crowding)

    child_instructions <- parent1$instructions
    if (stats::runif(1) < crossover_rate) {
      child_instructions <- gepa_crossover_instructions(
        parent1$instructions,
        parent2$instructions
      )
    }

    if (stats::runif(1) < mutation_rate) {
      child_instructions <- gepa_mutate_instruction(
        child_instructions,
        failed_examples = parent1$failed_examples,
        .llm = .llm,
        verbose = verbose
      )
    }

    population[[i]] <- child_instructions
  }

  population
}

gepa_select_parent <- function(records, ranks, crowding) {
  candidates <- sample(seq_along(records), size = 2)
  a <- candidates[1]
  b <- candidates[2]

  if (ranks[a] < ranks[b]) {
    return(records[[a]])
  }
  if (ranks[b] < ranks[a]) {
    return(records[[b]])
  }

  if (crowding[a] >= crowding[b]) {
    records[[a]]
  } else {
    records[[b]]
  }
}

select_pareto_best <- function(scores_matrix, ranks, crowding) {
  best_rank <- min(ranks)
  candidates <- which(ranks == best_rank)
  if (length(candidates) == 1) {
    return(candidates)
  }

  crowding_scores <- crowding[candidates]
  candidates[which.max(crowding_scores)]
}

gepa_crossover_instructions <- function(parent1, parent2) {
  parent1 <- parent1 %||% ""
  parent2 <- parent2 %||% ""

  if (nchar(parent1) == 0) {
    return(parent2)
  }
  if (nchar(parent2) == 0) {
    return(parent1)
  }

  paste(parent1, parent2, sep = "\n\n")
}

gepa_mutate_instruction <- function(
  instruction,
  failed_examples,
  .llm = NULL,
  verbose = FALSE
) {
  if (is.null(.llm) || is.null(.llm$chat_structured)) {
    return(gepa_fallback_mutation(instruction, failed_examples))
  }

  prompt <- gepa_reflection_prompt(instruction, failed_examples)
  type <- ellmer::type_object(instructions = ellmer::type_string())

  result <- tryCatch(
    .llm$chat_structured(prompt, type = type, echo = "none"),
    error = function(e) {
      cli::cli_warn(c(
        "GEPA reflection LLM call failed, using fallback mutation",
        "x" = conditionMessage(e),
        "i" = "This may degrade optimization quality"
      ))
      NULL
    }
  )

  if (is.list(result) && !is.null(result$instructions)) {
    return(result$instructions)
  }

  if (verbose) {
    cli::cli_warn(c(
      "GEPA reflection returned unexpected format, using fallback mutation",
      "i" = "Expected object with 'instructions' field"
    ))
  }

  gepa_fallback_mutation(instruction, failed_examples)
}

gepa_reflection_prompt <- function(instruction, failed_examples) {
  failures_text <- gepa_format_failures(failed_examples)
  base <- if (nchar(instruction) > 0) {
    instruction
  } else {
    "(no existing instructions)"
  }

  paste(
    "You are improving system instructions for a language model.",
    "Current instructions:",
    base,
    "",
    "Failed examples:",
    failures_text,
    "",
    "Rewrite the instructions to reduce these failures.",
    "Return concise instructions only.",
    sep = "\n"
  )
}

gepa_format_failures <- function(failed_examples) {
  if (length(failed_examples) == 0) {
    return("(none)")
  }

  lines <- vapply(
    failed_examples,
    function(ex) {
      inputs <- ex$inputs
      input_text <- paste(
        names(inputs),
        vapply(inputs, as.character, character(1)),
        sep = ": ",
        collapse = ", "
      )
      line <- paste0(
        "Inputs: ",
        input_text,
        " | Expected: ",
        as.character(ex$expected),
        " | Predicted: ",
        as.character(ex$predicted)
      )
      feedback <- ex$feedback %||% NA_character_
      if (length(feedback) == 1 && !is.na(feedback) && nzchar(feedback)) {
        line <- paste0(line, " | Feedback: ", feedback)
      }
      line
    },
    character(1)
  )

  paste(lines, collapse = "\n")
}

gepa_fallback_mutation <- function(instruction, failed_examples) {
  suffix <- if (length(failed_examples) > 0) {
    "Focus on the failed cases and be more explicit."
  } else {
    "Be more explicit and accurate."
  }

  if (nchar(instruction) == 0) {
    return(suffix)
  }

  paste(instruction, suffix)
}

#' Print method for GEPA
#' @param x A GEPA object
#' @param ... Additional arguments (unused)
#' @export
print.GEPA <- function(x, ...) {
  cli::cli_h3("GEPA Teleprompter")
  cli::cli_text("{.field population_size}: {x@population_size}")
  cli::cli_text("{.field generations}: {x@generations}")
  cli::cli_text("{.field mutation_rate}: {x@mutation_rate}")
  cli::cli_text("{.field crossover_rate}: {x@crossover_rate}")
  cli::cli_text("{.field selection}: {x@selection}")

  if (!is.null(x@seed)) {
    cli::cli_text("{.field seed}: {x@seed}")
  }

  invisible(x)
}

S7::method(print, GEPA) <- print.GEPA
