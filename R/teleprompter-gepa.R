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
#' Candidate evaluation uses the chat passed to [compile()] as `.llm`.
#' Reflection proposals use `.reflection_llm`, also passed to [compile()]. If
#' `.reflection_llm` is omitted, an explicitly supplied `.llm` is used for
#' reflection as well. Mutable GEPA searches require one
#' of these chats; GEPA reports a configuration error instead of silently
#' appending a fixed instruction suffix when no reflection chat is available.
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
#' Checkpoint resume is available for sequential searches with the
#' `round_robin` component selector over plain `Predict` leaves and built-in
#' predictor pipelines. Checkpointed predictions must be plain data. Flex
#' modules, other component selectors, custom module types, custom Chats,
#' Chats with tools, and Chats with existing conversation turns are rejected
#' before provider work. Checkpointed searches with mutable components require
#' at least one validation row, using `trainset` when `valset` is omitted.
#' Checkpointed metrics must return finite scores or `NA`; infinite scores are
#' rejected before their evaluation row is saved. Resource limits may change
#' when resuming; the algorithm configuration and runtime identities must match.
#'
#' Completed proposals and evaluation rows are restored without repeating
#' their work. A provider request that finishes after the last checkpoint was
#' written but before its result is committed may be retried after a crash, so
#' checkpoint resume does not guarantee exactly-once provider effects.
#'
#' A Flex source is optimized as one component: its dynamically built inner
#' predictors are not tuned separately. The source proposer sees the task and
#' signature, field schemas, allowed tools and primitives, and the failed rows
#' with their feedback. Proposed sources are validated before use; an invalid
#' source is recorded with a failure score and can never be selected.
#'
#' `.reflection_llm` is the reflection Chat, while `.llm` remains the task Chat
#' for candidate evaluation. When `.reflection_llm` is omitted, an explicitly
#' supplied `.llm` is used for both roles for compatibility. The optimization
#' result records model names and the reflection route in
#' `extensions$gepa$model_routing`. Without checkpointing, an omitted `.llm`
#' leaves `task_model` as `NULL`, and `task_chat_source` records
#' `"runtime_resolution"`. Checkpointed searches additionally report the
#' effective attached/default Chat models when they can be resolved. Both roles
#' share the optimizer's resource budget; reflection requests are attributed to
#' GEPA reflection stages.
#'
#' GEPA deep-clones the reflection Chat for each proposal and clears the
#' clone's conversation turns. Model settings are retained, proposal calls do
#' not share conversation history, and the supplied Chat is not mutated by
#' reflection.
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
#' # `.llm` runs the program; `.reflection_llm` writes the reflections.
#' optimized <- compile(
#'   qa,
#'   tp,
#'   trainset,
#'   .llm = ellmer::chat_openai(model = "gpt-6-luna"),
#'   .reflection_llm = ellmer::chat_openai(model = "gpt-6-astra")
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
  .reflection_llm = NULL,
  control = NULL,
  ...
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("GEPA currently only supports Module objects")
  }

  assert_ellmer_chat(.llm, arg = ".llm", allow_null = TRUE)
  assert_ellmer_chat(
    .reflection_llm,
    arg = ".reflection_llm",
    allow_null = TRUE
  )

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

  reflection_chat <- .reflection_llm %||% .llm
  reflection_chat_source <- if (!is.null(.reflection_llm)) {
    "reflection_llm"
  } else if (!is.null(.llm)) {
    "legacy_task_llm"
  } else {
    "unavailable"
  }
  task_chat_source <- if (is.null(.llm)) {
    "runtime_resolution"
  } else {
    "compile_llm"
  }
  component_specs <- gepa_program_component_specs(program)
  if (length(component_specs) > 0L && is.null(reflection_chat)) {
    cli::cli_abort(
      c(
        "GEPA requires a reflection Chat to propose candidate changes",
        "i" = "Supply {.arg .reflection_llm} to {.fn compile}.",
        "i" = paste(
          "For compatibility, an explicitly supplied {.arg .llm}",
          "is also used for reflection."
        )
      ),
      class = "dsprrr_gepa_reflection_error"
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

  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control
  )

  if (optimizer_checkpoint_enabled(control)) {
    return(compile_gepa_checkpointed(
      teleprompter = teleprompter,
      program = program,
      discovery_dataset = trainset,
      validation_dataset = valset %||% trainset,
      metrics = metrics,
      metric_names = metric_names,
      .llm = .llm,
      .reflection_llm = reflection_chat,
      task_chat_source = task_chat_source,
      reflection_chat_source = reflection_chat_source,
      control = control
    ))
  }

  if (!is.null(teleprompter@seed) && !is.na(teleprompter@seed)) {
    withr::local_seed(as.integer(teleprompter@seed))
  }

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
    .reflection_llm = reflection_chat,
    task_chat_source = task_chat_source,
    reflection_chat_source = reflection_chat_source,
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


select_pareto_best <- function(scores_matrix, ranks, crowding) {
  best_rank <- min(ranks)
  candidates <- which(ranks == best_rank)
  if (length(candidates) == 1) {
    return(candidates)
  }

  crowding_scores <- crowding[candidates]
  candidates[which.max(crowding_scores)]
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

gepa_chat_model_name <- function(chat) {
  if (is.null(chat)) {
    return(NULL)
  }
  ellmer_chat_model_name(chat)
}

gepa_clone_reflection_chat <- function(chat) {
  clone_ellmer_chat(chat, arg = ".reflection_llm", reset_turns = TRUE)
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
