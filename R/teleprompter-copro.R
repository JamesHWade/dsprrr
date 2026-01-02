# COPRO Teleprompter
#
# Coordinate Prompt Optimization - iteratively generates and refines
# instructions using coordinate ascent.

#' COPRO Teleprompter
#'
#' @include teleprompter.R optimizer-core.R optimizer-logging.R
#'
#' @description
#' COPRO (Coordinate Prompt Optimization) automatically refines module
#' instructions using coordinate ascent. At each iteration, it generates
#' multiple candidate instruction variants and selects the best performing
#' one as the baseline for the next iteration.
#'
#' The optimizer:
#' 1. Starts with current module instructions as baseline
#' 2. For each depth iteration:
#'    - Generates `breadth` candidate instruction variants using the prompt_model
#'    - Evaluates each candidate on the validation set
#'    - Selects the best performing instruction
#'    - Uses the best as baseline for the next iteration
#' 3. Returns module with optimized instructions
#'
#' @param metric A metric function for evaluating predictions (required).
#' @param metric_threshold Minimum score required to be considered successful.
#'   If NULL, uses the metric's default threshold.
#' @param max_errors Maximum number of errors allowed during optimization.
#'   Default is 5.
#' @param prompt_model Optional LLM for generating instruction candidates.
#'   If NULL, uses the task model (.llm) with higher temperature.
#' @param breadth Number of instruction candidates to generate per iteration.
#'   Default is 10.
#' @param depth Number of coordinate ascent iterations. Default is 3.
#' @param init_temperature Temperature for instruction generation. Default is 1.4.
#' @param track_stats Whether to track instruction history and scores.
#'   Default is TRUE.
#' @param seed Random seed for reproducibility. Default is 0.
#' @param log_dir Directory for trial logging. Default is NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tp <- COPRO(
#'   metric = metric_exact_match(field = "answer"),
#'   prompt_model = ellmer::chat_openai(),
#'   breadth = 10L,
#'   depth = 3L
#' )
#'
#' compiled <- compile(
#'   tp, qa_module, trainset,
#'   valset = valset, .llm = task_llm
#' )
#'
#' # Access instruction history
#' compiled$config$optimizer$history
#' }
COPRO <- S7::new_class(
  "COPRO",
  parent = Teleprompter,
  properties = list(
    prompt_model = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (is.null(value)) {
          return(NULL)
        }
        if (is.function(value)) {
          return(NULL)
        }
        if (inherits(value, "Chat")) {
          return(NULL)
        }
        if (is.list(value) && "chat" %in% names(value)) {
          return(NULL)
        }
        if (is.list(value) && "chat_structured" %in% names(value)) {
          return(NULL)
        }
        "prompt_model must be NULL, a function, a Chat object, or a list with chat/chat_structured method"
      }
    ),
    breadth = S7::new_property(
      S7::class_integer,
      default = 10L,
      validator = function(value) {
        if (value < 1) {
          return("breadth must be at least 1")
        }
        NULL
      }
    ),
    depth = S7::new_property(
      S7::class_integer,
      default = 3L,
      validator = function(value) {
        if (value < 1) {
          return("depth must be at least 1")
        }
        NULL
      }
    ),
    init_temperature = S7::new_property(
      S7::class_numeric,
      default = 1.4,
      validator = function(value) {
        if (!is.numeric(value) || length(value) != 1 || value <= 0) {
          return("init_temperature must be a single positive numeric value")
        }
        NULL
      }
    ),
    track_stats = S7::new_property(
      S7::class_logical,
      default = TRUE,
      validator = function(value) {
        if (length(value) != 1) {
          return("track_stats must be a single logical value")
        }
        NULL
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

#' Compile method for COPRO
#' @noRd
compile_copro <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  ...
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("COPRO currently only supports Module objects")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("{.arg trainset} must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort("COPRO requires a metric function")
  }

  evalset <- valset %||% trainset

  control <- optimizer_control(
    seed = teleprompter@seed,
    max_errors = teleprompter@max_errors,
    log_dir = teleprompter@log_dir
  )

  trial_log <- if (!is.null(teleprompter@log_dir)) {
    TrialLog$new(
      optimizer_name = "COPRO",
      log_dir = teleprompter@log_dir
    )
  } else {
    NULL
  }

  # Save and restore RNG state
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
  output_col <- find_output_column(trainset, input_names)

  # Get failed examples for targeted improvement
  failed_examples <- identify_failed_examples(
    program = program,
    dataset = trainset,
    metric = teleprompter@metric,
    output_col = output_col,
    input_names = input_names,
    .llm = .llm
  )

  # Initialize with current instructions
  current_instructions <- program$signature@instructions
  if (!nzchar(current_instructions)) {
    current_instructions <- "Complete the task based on the given inputs."
  }

  # Evaluate baseline
  best_program <- copy_module(program)
  best_eval <- eval_program(
    best_program,
    evalset,
    metric = teleprompter@metric,
    .llm = .llm,
    control = control,
    ...
  )
  best_score <- best_eval@mean_score
  best_instructions <- current_instructions

  # Track history
  instruction_history <- list(
    list(
      iteration = 0L,
      instructions = current_instructions,
      score = best_score,
      is_best = TRUE
    )
  )

  error_count <- 0L

  for (iteration in seq_len(teleprompter@depth)) {
    # Check error budget
    budget_check <- check_budget(iteration - 1L, error_count, control)
    if (budget_check$should_stop) {
      cli::cli_warn(budget_check$reason)
      break
    }

    cli::cli_alert_info(
      "COPRO iteration {iteration}/{teleprompter@depth}: generating {teleprompter@breadth} candidates"
    )

    # Generate candidate instructions
    candidates <- generate_copro_candidates(
      current_instructions = best_instructions,
      failed_examples = failed_examples,
      input_names = input_names,
      output_col = output_col,
      breadth = teleprompter@breadth,
      prompt_model = teleprompter@prompt_model,
      .llm = .llm,
      temperature = teleprompter@init_temperature
    )

    if (length(candidates) == 0) {
      cli::cli_warn(
        "No instruction candidates generated at iteration {iteration}"
      )
      error_count <- error_count + 1L
      next
    }

    # Evaluate each candidate
    iteration_best_score <- best_score
    iteration_best_instructions <- best_instructions
    iteration_best_program <- NULL

    for (i in seq_along(candidates)) {
      candidate_instructions <- candidates[[i]]

      # Skip if same as current best
      if (identical(candidate_instructions, best_instructions)) {
        next
      }

      # Create candidate program with new instructions
      candidate_program <- copy_module(program)
      candidate_program$signature <- Signature(
        inputs = candidate_program$signature@inputs,
        output_type = candidate_program$signature@output_type,
        instructions = candidate_instructions
      )

      # Evaluate candidate
      eval_result <- tryCatch(
        {
          eval_program(
            candidate_program,
            evalset,
            metric = teleprompter@metric,
            .llm = .llm,
            control = control,
            ...
          )
        },
        error = function(e) {
          cli::cli_warn(
            c(
              "COPRO candidate {i}/{length(candidates)} evaluation failed",
              "x" = conditionMessage(e)
            ),
            class = "dsprrr_copro_eval_warning"
          )
          error_count <<- error_count + 1L
          NULL
        }
      )

      if (is.null(eval_result)) {
        next
      }

      score <- eval_result@mean_score

      # Track in history
      if (teleprompter@track_stats) {
        instruction_history[[length(instruction_history) + 1L]] <- list(
          iteration = iteration,
          candidate = i,
          instructions = candidate_instructions,
          score = score,
          is_best = FALSE
        )
      }

      # Log trial if logging enabled
      if (!is.null(trial_log)) {
        trial <- create_trial(
          optimizer_name = "COPRO",
          params = list(
            iteration = iteration,
            candidate = i,
            instructions = substr(candidate_instructions, 1, 200)
          )
        )
        trial <- complete_trial(trial, eval_result)
        trial_log$add_trial(trial)
      }

      # Check if this is the best in this iteration
      if (
        !is.na(score) &&
          (is.na(iteration_best_score) || score > iteration_best_score)
      ) {
        iteration_best_score <- score
        iteration_best_instructions <- candidate_instructions
        iteration_best_program <- candidate_program
      }
    }

    # Update best if improved
    if (
      !is.na(iteration_best_score) &&
        (is.na(best_score) || iteration_best_score > best_score)
    ) {
      best_score <- iteration_best_score
      best_instructions <- iteration_best_instructions
      best_program <- iteration_best_program %||% best_program

      # Mark as best in history
      if (teleprompter@track_stats) {
        for (j in seq_along(instruction_history)) {
          if (
            identical(
              instruction_history[[j]]$instructions,
              best_instructions
            ) &&
              instruction_history[[j]]$iteration == iteration
          ) {
            instruction_history[[j]]$is_best <- TRUE
            break
          }
        }
      }

      cli::cli_alert_success(
        "COPRO iteration {iteration}: improved score from {round(best_eval@mean_score, 4)} to {round(best_score, 4)}"
      )

      # Re-identify failed examples for next iteration
      failed_examples <- identify_failed_examples(
        program = best_program,
        dataset = trainset,
        metric = teleprompter@metric,
        output_col = output_col,
        input_names = input_names,
        .llm = .llm
      )
    } else {
      cli::cli_alert_info(
        "COPRO iteration {iteration}: no improvement (best: {round(best_score, 4)})"
      )
    }
  }

  # Finalize the best program
  if (is.null(best_program)) {
    best_program <- copy_module(program)
  }

  # Update signature with best instructions
  best_program$signature <- Signature(
    inputs = best_program$signature@inputs,
    output_type = best_program$signature@output_type,
    instructions = best_instructions
  )

  best_program$state$compiled <- TRUE
  best_program$config$compiled <- TRUE
  best_program$config$teleprompter <- "COPRO"
  best_program$config$optimizer <- list(
    breadth = teleprompter@breadth,
    depth = teleprompter@depth,
    init_temperature = teleprompter@init_temperature,
    final_score = best_score,
    baseline_score = best_eval@mean_score,
    history = if (teleprompter@track_stats) instruction_history else NULL,
    iterations_completed = min(teleprompter@depth, iteration %||% 0L)
  )

  best_program
}

#' Generate COPRO instruction candidates
#' @noRd
generate_copro_candidates <- function(
  current_instructions,
  failed_examples,
  input_names,
  output_col,
  breadth,
  prompt_model,
  .llm = NULL,
  temperature = 1.4
) {
  candidates <- list()

  # Build context about the task from failed examples
  failed_context <- format_copro_failed_examples(
    failed_examples,
    input_names,
    output_col,
    max_examples = 3L
  )

  # Build the instruction generation prompt
  generation_prompt <- paste(
    "You are optimizing instructions for an LLM program.",
    "",
    "Current instructions:",
    "---",
    current_instructions,
    "---",
    "",
    if (nzchar(failed_context)) {
      paste(
        "The program failed on these examples:",
        failed_context,
        "",
        sep = "\n"
      )
    } else {
      ""
    },
    "Generate a NEW, IMPROVED version of the instructions.",
    "The new instructions should:",
    "- Address any failures shown above",
    "- Be clear and specific",
    "- Guide the model to produce correct outputs",
    "",
    "Return ONLY the new instruction text, nothing else.",
    sep = "\n"
  )

  # Generate candidates
  for (i in seq_len(breadth)) {
    candidate <- generate_single_copro_candidate(
      generation_prompt,
      prompt_model = prompt_model,
      .llm = .llm,
      temperature = temperature
    )

    if (!is.null(candidate) && nzchar(candidate)) {
      candidates[[length(candidates) + 1L]] <- candidate
    }
  }

  # Deduplicate candidates
  unique_candidates <- unique(candidates)

  unique_candidates
}

#' Generate a single instruction candidate
#' @noRd
generate_single_copro_candidate <- function(
  prompt,
  prompt_model,
  .llm = NULL,
  temperature = 1.4
) {
  result <- NULL

  # Use prompt_model if provided, otherwise fall back to .llm
  model_to_use <- prompt_model %||% .llm

  if (is.null(model_to_use)) {
    cli::cli_warn(
      c(
        "No prompt_model or .llm provided for instruction generation",
        "i" = "Returning NULL candidate"
      ),
      class = "dsprrr_copro_no_model_warning"
    )
    return(NULL)
  }

  if (inherits(model_to_use, "Chat")) {
    # Handle ellmer Chat objects
    result <- tryCatch(
      model_to_use$chat(prompt),
      error = function(e) {
        cli::cli_warn(
          c(
            "COPRO instruction generation failed",
            "x" = conditionMessage(e)
          ),
          class = "dsprrr_copro_generation_warning"
        )
        NULL
      }
    )
  } else if (is.function(model_to_use)) {
    result <- tryCatch(
      model_to_use(prompt),
      error = function(e) {
        cli::cli_warn(
          c(
            "COPRO instruction generation function failed",
            "x" = conditionMessage(e)
          ),
          class = "dsprrr_copro_generation_warning"
        )
        NULL
      }
    )
  } else if (is.list(model_to_use)) {
    if ("chat" %in% names(model_to_use)) {
      result <- tryCatch(
        model_to_use$chat(prompt),
        error = function(e) {
          cli::cli_warn(
            c(
              "COPRO instruction generation (list$chat) failed",
              "x" = conditionMessage(e)
            ),
            class = "dsprrr_copro_generation_warning"
          )
          NULL
        }
      )
    } else if ("chat_structured" %in% names(model_to_use)) {
      result <- tryCatch(
        model_to_use$chat_structured(prompt, ellmer::type_string()),
        error = function(e) {
          cli::cli_warn(
            c(
              "COPRO instruction generation (chat_structured) failed",
              "x" = conditionMessage(e)
            ),
            class = "dsprrr_copro_generation_warning"
          )
          NULL
        }
      )
    }
  }

  # Clean up result
  if (!is.null(result)) {
    if (is.list(result) && length(result) > 0) {
      result <- result[[1]]
    }
    result <- trimws(as.character(result))
  }

  result
}

#' Identify failed examples for targeted improvement
#' @noRd
identify_failed_examples <- function(
  program,
  dataset,
  metric,
  output_col,
  input_names,
  .llm = NULL,
  max_examples = 5L
) {
  if (nrow(dataset) == 0) {
    return(data.frame())
  }

  # Run predictions on a sample of the dataset
  sample_size <- min(20L, nrow(dataset))
  sample_indices <- sample(nrow(dataset), sample_size)
  sample_data <- dataset[sample_indices, , drop = FALSE]

  # Get predictions
  predictions <- tryCatch(
    {
      result <- run_dataset(
        program,
        sample_data,
        .llm = .llm,
        .parallel = FALSE,
        .progress = FALSE,
        .return_format = "simple"
      )
      result$result
    },
    error = function(e) {
      cli::cli_warn(
        c(
          "Failed to run predictions for identifying failed examples",
          "x" = conditionMessage(e)
        ),
        class = "dsprrr_copro_prediction_warning"
      )
      rep(NA_character_, sample_size)
    }
  )

  # Score each prediction
  failed_rows <- list()

  for (i in seq_len(sample_size)) {
    row <- sample_data[i, , drop = FALSE]
    pred <- predictions[[i]]

    score <- tryCatch(
      {
        metric(pred, row)
      },
      error = function(e) {
        NA_real_
      }
    )

    # Consider failed if score is low or NA
    is_failed <- is.na(score) || score < 0.5

    if (is_failed) {
      failed_entry <- list(
        inputs = list(),
        expected = if (!is.null(output_col) && output_col %in% names(row)) {
          row[[output_col]]
        } else {
          NA_character_
        },
        predicted = pred,
        score = score
      )

      for (name in input_names) {
        if (name %in% names(row)) {
          failed_entry$inputs[[name]] <- row[[name]]
        }
      }

      failed_rows[[length(failed_rows) + 1L]] <- failed_entry
    }
  }

  # Limit to max_examples
  if (length(failed_rows) > max_examples) {
    failed_rows <- failed_rows[seq_len(max_examples)]
  }

  failed_rows
}

#' Format failed examples for the instruction generation prompt
#' @noRd
format_copro_failed_examples <- function(
  failed_examples,
  input_names,
  output_col,
  max_examples = 3L
) {
  if (length(failed_examples) == 0) {
    return("")
  }

  n_examples <- min(length(failed_examples), max_examples)
  lines <- character(n_examples)

  for (i in seq_len(n_examples)) {
    example <- failed_examples[[i]]

    input_parts <- vapply(
      names(example$inputs),
      function(name) {
        paste0(name, ": ", example$inputs[[name]])
      },
      character(1)
    )

    lines[i] <- paste(
      paste("Example", i),
      paste("  Inputs:", paste(input_parts, collapse = ", ")),
      paste("  Expected:", example$expected),
      paste("  Got:", example$predicted),
      sep = "\n"
    )
  }

  paste(lines, collapse = "\n\n")
}

#' Print method for COPRO
#' @param x A COPRO object
#' @param ... Additional arguments (unused)
#' @export
print.COPRO <- function(x, ...) {
  cli::cli_h3("COPRO Teleprompter")

  cli::cli_text("{.field breadth}: {x@breadth}")
  cli::cli_text("{.field depth}: {x@depth}")
  cli::cli_text("{.field init_temperature}: {x@init_temperature}")
  cli::cli_text("{.field track_stats}: {x@track_stats}")

  if (!is.null(x@metric_threshold)) {
    cli::cli_text("{.field metric_threshold}: {x@metric_threshold}")
  }

  if (!is.null(x@seed)) {
    cli::cli_text("{.field seed}: {x@seed}")
  }

  if (!is.null(x@prompt_model)) {
    cli::cli_text("{.field prompt_model}: <provided>")
  }

  invisible(x)
}

# Register S7 print method
S7::method(print, COPRO) <- print.COPRO
