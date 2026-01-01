# SIMBA Teleprompter
#
# Self-improving optimization via hard example mining.

#' SIMBA Teleprompter
#'
#' @include teleprompter.R optimizer-core.R optimizer-logging.R
#'
#' @description
#' SIMBA (self-improving via hard example mining) iteratively samples
#' mini-batches, identifies high-variability examples, and generates
#' improvement rules or demonstrations to improve performance.
#'
#' The optimizer:
#' 1. Evaluates baseline performance on the training set (or validation set).
#' 2. Repeats for up to `max_steps`:
#'    - Samples a mini-batch
#'    - Runs multiple candidates to measure variability
#'    - Identifies hard examples
#'    - Generates a rule and/or adds demos
#'    - Evaluates improvement and keeps changes if better
#'
#' @param metric A metric function for evaluating predictions (required).
#' @param metric_threshold Minimum score required to be considered successful.
#'   If NULL, uses the metric's default threshold.
#' @param max_errors Maximum number of errors allowed during optimization.
#'   Default is 5.
#' @param bsize Mini-batch size for hard example mining. Default is 32.
#' @param num_candidates Number of candidate runs per example to measure
#'   variability. Default is 6.
#' @param max_steps Maximum number of optimization steps. Default is 8.
#' @param max_demos Maximum number of demonstrations to keep. Default is 4.
#' @param prompt_model Optional LLM for rule generation (reflection).
#' @param seed Random seed for reproducibility. Default is 0.
#' @param log_dir Directory for trial logging. Default is NULL.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' tp <- SIMBA(
#'   metric = metric_exact_match(field = "answer"),
#'   bsize = 32L,
#'   num_candidates = 6L,
#'   max_steps = 8L,
#'   max_demos = 4L,
#'   prompt_model = ellmer::chat_openai(),
#'   seed = 0L
#' )
#'
#' compiled <- compile(tp, qa_module, trainset, .llm = llm)
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
      default = NULL
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

  control <- optimizer_control(
    seed = teleprompter@seed,
    max_errors = teleprompter@max_errors,
    log_dir = teleprompter@log_dir
  )

  trial_log <- if (!is.null(teleprompter@log_dir)) {
    TrialLog$new(
      optimizer_name = "SIMBA",
      log_dir = teleprompter@log_dir
    )
  } else {
    NULL
  }

  if (!is.null(teleprompter@seed) && !is.na(teleprompter@seed)) {
    set.seed(teleprompter@seed)
  }

  input_names <- get_input_names(program$signature)
  output_col <- find_output_column(trainset, input_names)

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
  best_eval <- eval_program(
    best_program,
    eval_dataset,
    metric = teleprompter@metric,
    .llm = .llm,
    control = control,
    ...
  )
  best_score <- best_eval@mean_score

  rules <- character()
  added_demos <- list()

  for (step in seq_len(teleprompter@max_steps)) {
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
      .llm = .llm
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
      output_col = output_col
    )

    candidate <- copy_module(best_program)
    if (!is.null(rule_text) && nzchar(rule_text)) {
      candidate$signature@instructions <- paste(
        candidate$signature@instructions,
        rule_text,
        sep = "\n\n"
      )
    }

    new_demos <- list()
    if (!is.null(output_col) && teleprompter@max_demos > 0) {
      new_demos <- format_trainset_as_demos(
        hard_examples,
        candidate$signature
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

    eval_result <- eval_program(
      candidate,
      eval_dataset,
      metric = teleprompter@metric,
      .llm = .llm,
      control = control,
      ...
    )
    score <- eval_result@mean_score

    improved <- !is.na(score) &&
      (is.na(best_score) || score > best_score)

    if (improved) {
      best_program <- candidate
      best_score <- score
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

    if (!is.null(trial_log)) {
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

  best_program$state$compiled <- TRUE
  best_program$config$compiled <- TRUE
  best_program$config$teleprompter <- "SIMBA"
  best_program$config$optimizer <- list(
    steps = teleprompter@max_steps,
    best_score = best_score,
    rules = rules,
    demos_added = added_demos
  )

  best_program
}

simba_variability <- function(
  program,
  minibatch,
  output_col,
  metric,
  num_candidates,
  .llm = NULL
) {
  outputs <- vector("list", num_candidates)
  for (i in seq_len(num_candidates)) {
    results <- run_dataset(
      program,
      minibatch,
      .llm = .llm,
      .parallel = FALSE,
      .progress = FALSE,
      .return_format = "simple"
    )
    outputs[[i]] <- results$result
  }

  variability <- vector("list", nrow(minibatch))
  for (i in seq_len(nrow(minibatch))) {
    predictions <- lapply(outputs, function(x) x[[i]])
    normalized <- vapply(predictions, normalize_simba_output, character(1))
    variation <- if (num_candidates > 1) {
      freqs <- table(normalized)
      1 - max(freqs) / num_candidates
    } else {
      0
    }

    mean_score <- NA_real_
    if (!is.null(output_col)) {
      row <- minibatch[i, , drop = FALSE]
      scores <- vapply(
        predictions,
        function(pred) simba_safe_metric(metric, pred, row),
        numeric(1)
      )
      mean_score <- mean(scores, na.rm = TRUE)
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

  tibble::bind_rows(variability)
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
  output_col
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
          NULL
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
  if (is.function(prompt_model)) {
    rule <- prompt_model(prompt)
  } else if (is.list(prompt_model) && !is.null(prompt_model$chat_structured)) {
    response <- prompt_model$chat_structured(
      prompt,
      ellmer::type_string()
    )
    if (is.character(response)) {
      rule <- response
    } else if (is.list(response)) {
      if ("rule" %in% names(response)) {
        rule <- response$rule
      } else {
        rule <- response[[1]]
      }
    }
  }

  if (is.null(rule) || !nzchar(rule)) {
    rule <- paste(
      "SIMBA rule:",
      example_lines[1]
    )
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
    return(jsonlite::toJSON(output, auto_unbox = TRUE, null = "null"))
  }
  as.character(output)
}

simba_safe_metric <- function(metric, prediction, row) {
  tryCatch(
    {
      score <- metric(prediction, row)
      if (is.logical(score)) {
        as.numeric(score)
      } else if (is.numeric(score)) {
        score
      } else {
        NA_real_
      }
    },
    error = function(e) NA_real_
  )
}

#' Print method for SIMBA
#' @param x A SIMBA object
#' @param ... Additional arguments (unused)
#' @export
print.SIMBA <- function(x, ...) {
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

S7::method(print, SIMBA) <- print.SIMBA
