# GEPA Teleprompter
#
# Adapted GEPA reflective evolution optimizer.

#' GEPA: reflective prompt evolution
#'
#' @include teleprompter.R optimizer-core.R optimizer-logging.R pareto.R
#'
#' @description
#' `GEPA()` improves a program's instructions by reflection. It scores a
#' population of candidate programs, shows a model the rows a candidate got
#' wrong (with the metric's feedback when there is any), and asks it to rewrite
#' the instructions. Over several generations the best candidates are kept,
#' combined and rewritten again. GEPA can also rewrite the complete source of a
#' [flex()] module.
#'
#' Reflection uses the chat passed to [compile()] as `.llm`, the same chat that
#' runs the program; there is no separate reflection model argument. Without
#' `.llm`, no model is asked: rewritten instructions get the fixed sentence
#' "Focus on the failed cases and be more explicit." (or "Be more explicit and
#' accurate.") appended, and Flex sources stay unchanged.
#'
#' @details
#' ## Feedback metrics
#'
#' GEPA works best with metrics created by [metric_with_feedback()]. When the
#' metric returns `list(score = , feedback = )`, the feedback for failed rows
#' goes into the reflection prompt, so the model sees why an output was wrong.
#' This is the key mechanism of the GEPA paper ("GEPA: Reflective Prompt
#' Evolution Can Outperform Reinforcement Learning", Agrawal et al., 2025).
#' With a plain numeric metric, reflection sees only the inputs, the expected
#' values and the predictions.
#'
#' ## How a run works
#'
#' The first population is the original program plus `population_size - 1`
#' rewrites of it. Each generation scores every candidate with every metric on
#' `valset` (or on `trainset` when no `valset` is given), so a run costs about
#' `population_size * generations` full evaluations plus the reflection calls.
#' Parents are picked by tournament. A child takes each component (an
#' instruction or a Flex source) from one of two parents with probability
#' `crossover_rate`, and is rewritten by reflection with probability
#' `mutation_rate`. Reflection sees up to five rows that the parent failed on
#' `trainset`. The best candidate over all generations is returned; the
#' candidates, their scores and lineage are stored in
#' `optimization_result(compiled)$extensions$gepa`.
#'
#' ## Differences from DSPy's GEPA
#'
#' This is an adapted implementation. It keeps reflective rewriting guided by
#' failures and feedback, per-row winner frontiers, component selection,
#' lineage-aware merges and Pareto selection over several metrics, but runs a
#' fixed number of generations instead of DSPy's budget-driven search.
#' Checkpoint resume, cached subsample merge acceptance and inference-time
#' candidate selection are not implemented.
#'
#' A Flex source is optimized as one component: its dynamically built inner
#' predictors are not tuned separately. The source proposer sees the task and
#' signature, field schemas, allowed tools and primitives, and the failed rows
#' with their feedback. Proposed sources are validated before use; an invalid
#' source is recorded with a failure score and can never be selected.
#'
#' @param metrics Optional named list of metric functions. The first one picks
#'   the failed rows shown to reflection; with `selection = "pareto"`, all of
#'   them are used to rank candidates.
#' @param metric A metric function, used when `metrics` is `NULL`. One of the
#'   two is required.
#' @param metric_threshold Rows scoring below this value count as failures and
#'   can be shown to reflection. `NULL` (the default) means 1, so any row short
#'   of a perfect score counts.
#' @param max_errors Integer; stop after this many consecutive failed
#'   evaluations when [compile()] gets no `control` (default `5L`).
#' @param population_size Integer number of candidates per generation (default
#'   `20L`, minimum `2L`).
#' @param generations Integer number of generations (default `10L`).
#' @param mutation_rate Probability that a child is rewritten by reflection
#'   (default `0.1`).
#' @param crossover_rate Probability that a child combines two parents
#'   (default `0.7`).
#' @param selection `"pareto"` (the default) draws parents from candidates that
#'   win on at least one validation row or lie on the Pareto front of the
#'   metrics, and ranks by Pareto front when there are several metrics.
#'   `"current_best"` draws from all valid candidates and ranks by the first
#'   metric.
#' @param component_selector Component mutation strategy. Use
#'   `"round_robin"` to update one component at a time, `"all"` to update all
#'   components atomically, or a function called with `component_ids`,
#'   `candidate`, `failed_examples`, and `context`. The function must return one
#'   or more unique IDs from `component_ids`.
#' @param use_merge Whether to attempt lineage-aware merges of complementary
#'   component changes.
#' @param max_merge_invocations Maximum merge attempts, or `NULL` for no
#'   separate merge-attempt cap.
#' @param seed Optional whole-number random seed (default `NULL`).
#' @param log_dir Directory for a [TrialLog] of the run, or `NULL` (the
#'   default).
#' @param verbose Whether to print progress messages (default `TRUE`).
#' @param track_stats Whether to keep per-generation statistics in the
#'   optimization result (default `TRUE`).
#' @param track_best_outputs Whether to retain each validation row's
#'   highest-scoring output. Requires `track_stats = TRUE`.
#'
#' @return A `GEPA` object to pass to [compile()].
#' @family teleprompters
#' @examples
#' # A small run: 6 candidates over 2 generations
#' tp <- GEPA(
#'   metric = metric_exact_match(field = "answer"),
#'   population_size = 6L,
#'   generations = 2L,
#'   seed = 42L
#' )
#' tp
#'
#' \dontrun{
#' # Feedback tells the reflection step why an answer was wrong. The metric
#' # receives the run result and the whole data row.
#' feedback_metric <- metric_with_feedback(
#'   function(prediction, expected) {
#'     if (identical(prediction$answer, expected$answer)) {
#'       list(score = 1, feedback = "Correct.")
#'     } else {
#'       list(
#'         score = 0,
#'         feedback = paste0(
#'           "Expected '", expected$answer,
#'           "' but got '", prediction$answer, "'."
#'         )
#'       )
#'     }
#'   },
#'   field = "answer"
#' )
#' tp <- GEPA(
#'   metric = feedback_metric,
#'   population_size = 6L,
#'   generations = 2L,
#'   seed = 42L
#' )
#'
#' qa <- module(signature("question -> answer"))
#' trainset <- data.frame(
#'   question = c("What is 2 + 2?", "What is the capital of France?"),
#'   answer = c("4", "Paris")
#' )
#' # `.llm` runs the program and writes the reflections
#' optimized <- compile(
#'   qa,
#'   tp,
#'   trainset,
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna")
#' )
#' optimization_result(optimized)
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
    component_selector = S7::new_property(
      S7::class_any,
      default = "round_robin",
      validator = function(value) {
        valid <- is.function(value) ||
          (is.character(value) &&
            length(value) == 1L &&
            !is.na(value) &&
            value %in% c("round_robin", "all"))
        if (!valid) {
          return(
            "component_selector must be 'round_robin', 'all', or a function"
          )
        }
        NULL
      }
    ),
    use_merge = S7::new_property(
      S7::class_logical,
      default = TRUE,
      validator = function(value) {
        if (length(value) != 1L || is.na(value)) {
          return("use_merge must be TRUE or FALSE")
        }
        NULL
      }
    ),
    max_merge_invocations = S7::new_property(
      S7::class_any,
      default = 5L,
      validator = function(value) {
        valid <- is.null(value) ||
          (is.numeric(value) &&
            length(value) == 1L &&
            !is.na(value) &&
            is.finite(value) &&
            value == floor(value) &&
            value >= 0L &&
            value <= .Machine$integer.max)
        if (!valid) {
          return(
            "max_merge_invocations must be a non-negative integer or NULL"
          )
        }
        NULL
      }
    ),
    seed = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        valid <- is.null(value) ||
          (is.numeric(value) &&
            length(value) == 1L &&
            !is.na(value) &&
            is.finite(value) &&
            value == floor(value) &&
            value >= 0 &&
            value <= .Machine$integer.max)
        if (!valid) {
          return(
            "seed must be one non-missing whole number in R's integer range, or NULL"
          )
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
    ),
    track_best_outputs = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = function(value) {
        if (length(value) != 1L || is.na(value)) {
          return("track_best_outputs must be TRUE or FALSE")
        }
        NULL
      }
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
  control = NULL,
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

  if (
    isTRUE(teleprompter@track_best_outputs) &&
      !isTRUE(teleprompter@track_stats)
  ) {
    cli::cli_abort(
      "{.arg track_best_outputs = TRUE} requires {.arg track_stats = TRUE}",
      class = "dsprrr_gepa_config_error"
    )
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

  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control
  )
  optimizer_require_ledger_only_checkpoint(control, "GEPA")
  budget <- new_optimizer_budget(control)

  trial_log <- if (!is.null(control@log_dir)) {
    TrialLog$new(optimizer_name = "GEPA", log_dir = control@log_dir)
  } else {
    NULL
  }

  return(compile_gepa_components(
    teleprompter = teleprompter,
    program = program,
    discovery_dataset = trainset,
    validation_dataset = valset %||% trainset,
    metrics = metrics,
    metric_names = metric_names,
    .llm = .llm,
    control = control,
    budget = budget,
    trial_log = trial_log
  ))
}

gepa_failed_examples <- function(
  eval_result,
  dataset,
  signature,
  threshold = NULL,
  output_col = NULL
) {
  threshold <- threshold %||% 1
  input_names <- get_input_names(signature)
  if (is.null(output_col)) {
    output_col <- find_output_column(dataset, input_names)
  }

  scores <- eval_result@examples$score
  feedbacks <- eval_result@examples$feedback %||%
    rep(NA_character_, length(scores))
  row_ids <- if ("row_id" %in% names(eval_result@examples)) {
    eval_result@examples[["row_id"]]
  } else {
    seq_along(scores)
  }
  program_traces <- if ("program_trace" %in% names(eval_result@examples)) {
    eval_result@examples[["program_trace"]]
  } else {
    rep(list(NULL), length(scores))
  }
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
      row_id = row_ids[[i]] %||% i,
      inputs = inputs,
      expected = expected,
      predicted = eval_result@examples$predicted[[i]] %||% NA,
      feedback = feedbacks[[i]] %||% NA_character_,
      program_trace = program_traces[[i]]
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
  verbose = FALSE,
  budget = NULL,
  generation = NA_integer_
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
        verbose = verbose,
        budget = budget,
        unit_id = paste0("gepa:generation:", generation, ":mutation:", i)
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
  verbose = FALSE,
  budget = NULL,
  unit_id = "gepa:reflection"
) {
  if (is.null(.llm)) {
    return(gepa_fallback_mutation(instruction, failed_examples))
  }
  assert_ellmer_chat(.llm, arg = ".llm")

  prompt <- gepa_reflection_prompt(instruction, failed_examples)
  type <- ellmer::type_object(instructions = ellmer::type_string())

  if (!is.null(budget)) {
    request <- optimizer_budgeted_provider_call(
      budget = budget,
      model = .llm,
      stage = "gepa_reflection",
      unit_id = unit_id,
      call = function() {
        .llm$chat_structured(prompt, type = type, echo = "none")
      },
      success = function(value, condition) {
        is.null(condition) && is.list(value) && !is.null(value$instructions)
      }
    )
    result <- request$value
    if (!is.null(request$condition)) {
      cli::cli_warn(c(
        "GEPA reflection LLM call failed, using fallback mutation",
        "x" = conditionMessage(request$condition),
        "i" = "This may degrade optimization quality"
      ))
    }
  } else {
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
  }

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

# Print a GEPA object through its S7 method.
print_gepa <- function(x, ...) {
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
