# MIPROv2 Teleprompter
#
# Implements a lightweight MIPROv2 optimizer with demo bootstrapping,
# instruction candidate generation, and discrete BO over combinations.

#' MIPROv2 Teleprompter
#'
#' @include teleprompter.R teleprompter-bootstrap.R optimizer-core.R
#' @include optimizer-logging.R optimizer-discrete-bo.R
#'
#' @description
#' MIPROv2 jointly optimizes instructions and few-shot demonstrations using
#' a discrete Bayesian optimization loop with minibatch evaluation.
#'
#' @param metric A metric function for evaluating predictions (required).
#' @param prompt_model Optional model to propose instructions.
#' @param task_model Optional model to evaluate tasks. Defaults to .llm.
#' @param teacher_settings List of settings for the teacher model.
#' @param max_bootstrapped_demos Maximum number of bootstrapped demonstrations.
#' @param max_labeled_demos Maximum number of labeled demonstrations.
#' @param auto Auto-tuned settings: "light", "medium", "heavy", or NULL.
#' @param num_candidates Optional override for number of instruction candidates.
#' @param num_threads Number of threads to use for evaluation.
#' @param max_errors Maximum number of errors allowed during optimization.
#' @param seed Random seed for reproducibility.
#' @param init_temperature Initial temperature for instruction proposals.
#' @param track_stats Whether to track trial history.
#' @param log_dir Directory for trial logging.
#' @param metric_threshold Minimum score required for acceptance.
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' tp <- MIPROv2(
#'   metric = metric_exact_match(field = "answer"),
#'   auto = "light",
#'   max_bootstrapped_demos = 4L
#' )
#'
#' compiled <- compile(tp, qa_module, trainset, valset = valset, .llm = llm)
#' }
MIPROv2 <- S7::new_class(
  "MIPROv2",
  parent = Teleprompter,
  properties = list(
    prompt_model = S7::new_property(S7::class_any, default = NULL),
    task_model = S7::new_property(S7::class_any, default = NULL),
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
      default = 4L,
      validator = function(value) {
        if (value < 0) {
          return("max_labeled_demos must be non-negative")
        }
        NULL
      }
    ),
    auto = S7::new_property(
      S7::class_any,
      default = "light",
      validator = function(value) {
        valid <- c("light", "medium", "heavy")
        if (!is.null(value) && !value %in% valid) {
          return("auto must be NULL or one of: light, medium, heavy")
        }
        NULL
      }
    ),
    num_candidates = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value)) {
          if (!is.numeric(value) || length(value) != 1 || value < 1) {
            return("num_candidates must be a positive integer or NULL")
          }
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
    seed = S7::new_property(
      S7::class_any,
      default = 9L,
      validator = function(value) {
        if (!is.null(value) && (!is.numeric(value) || length(value) != 1)) {
          return("seed must be a single numeric value or NULL")
        }
        NULL
      }
    ),
    init_temperature = S7::new_property(
      S7::class_numeric,
      default = 1.0,
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

#' Compile method for MIPROv2
#' @noRd
compile_mipro <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  ...
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("MIPROv2 currently only supports Module objects")
  }

  if (!is.data.frame(trainset)) {
    cli::cli_abort("{.arg trainset} must be a data frame")
  }

  if (nrow(trainset) == 0) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }

  if (is.null(teleprompter@metric)) {
    cli::cli_abort("MIPROv2 requires a metric function")
  }

  evalset <- valset %||% trainset

  control <- optimizer_control(
    seed = teleprompter@seed,
    max_errors = teleprompter@max_errors,
    num_threads = teleprompter@num_threads,
    log_dir = teleprompter@log_dir
  )

  trial_log <- if (!is.null(teleprompter@log_dir)) {
    TrialLog$new(optimizer_name = "MIPROv2", log_dir = teleprompter@log_dir)
  } else {
    NULL
  }

  settings <- resolve_mipro_settings(
    auto = teleprompter@auto,
    num_candidates = teleprompter@num_candidates,
    n_train = nrow(trainset)
  )

  demo_candidates <- generate_mipro_demo_candidates(
    program = program,
    trainset = trainset,
    teleprompter = teleprompter,
    .llm = .llm,
    max_candidates = settings$demo_candidates
  )

  instruction_candidates <- generate_mipro_instruction_candidates(
    program = program,
    trainset = trainset,
    demo_candidates = demo_candidates,
    teleprompter = teleprompter,
    max_candidates = settings$instruction_candidates
  )

  candidate_grid <- expand_mipro_candidates(
    demo_candidates = demo_candidates,
    instruction_candidates = instruction_candidates
  )

  if (length(candidate_grid) == 0) {
    cli::cli_abort(
      c(
        "No valid candidate configurations generated",
        "i" = "Demo candidates: {length(demo_candidates)}",
        "i" = "Instruction candidates: {length(instruction_candidates)}",
        "!" = "Check if trainset has sufficient examples for bootstrapping"
      ),
      class = "dsprrr_mipro_no_candidates"
    )
  }

  minibatch_size <- min(settings$minibatch_size, nrow(trainset))
  full_eval_every <- settings$full_eval_every

  eval_fn <- function(candidate, eval_type, trial_idx) {
    data <- if (eval_type == "full") {
      evalset
    } else {
      sample_dataset(
        trainset,
        n = minibatch_size,
        seed = if (is.null(teleprompter@seed)) {
          NULL
        } else {
          teleprompter@seed + trial_idx
        }
      )
    }

    compiled <- copy_module(program)
    compiled$demos <- candidate$demos
    # Create new signature with updated instructions (S7 objects are immutable)
    compiled$signature <- Signature(
      inputs = compiled$signature@inputs,
      output_type = compiled$signature@output_type,
      instructions = candidate$instructions
    )

    eval_program(
      compiled,
      data,
      teleprompter@metric,
      .llm = teleprompter@task_model %||% .llm,
      control = control
    )
  }

  bo_result <- run_discrete_bo(
    candidates = candidate_grid,
    eval_fn = eval_fn,
    control = control,
    max_trials = settings$trials,
    minibatch_size = minibatch_size,
    full_eval_every = full_eval_every,
    trial_log = trial_log,
    seed = teleprompter@seed,
    track_stats = teleprompter@track_stats
  )

  best_candidate <- bo_result$best_candidate
  if (is.null(best_candidate)) {
    cli::cli_abort("MIPROv2 failed to select a best candidate")
  }

  compiled <- copy_module(program)
  compiled$demos <- best_candidate$demos
  # Create new signature with updated instructions (S7 objects are immutable)
  compiled$signature <- Signature(
    inputs = compiled$signature@inputs,
    output_type = compiled$signature@output_type,
    instructions = best_candidate$instructions
  )
  compiled$state$compiled <- TRUE
  compiled$config$compiled <- TRUE
  compiled$config$teleprompter <- "MIPROv2"
  compiled$config$optimizer <- list(
    auto = teleprompter@auto,
    trials = settings$trials,
    minibatch_size = minibatch_size,
    full_eval_every = full_eval_every,
    demo_candidates = lapply(demo_candidates, function(x) x$params),
    instruction_candidates = lapply(
      instruction_candidates,
      function(x) x$params
    ),
    trial_history = bo_result$trial_history,
    best_config = best_candidate$params,
    budget_summary = bo_result$budget_summary,
    stop_reason = bo_result$stop_reason,
    error_count = bo_result$error_count
  )

  compiled
}

resolve_mipro_settings <- function(auto, num_candidates, n_train) {
  if (is.null(auto)) {
    list(
      trials = as.integer(num_candidates %||% 20L),
      minibatch_size = min(10L, n_train),
      full_eval_every = 5L,
      demo_candidates = 4L,
      instruction_candidates = as.integer(num_candidates %||% 6L)
    )
  } else if (auto == "light") {
    list(
      trials = 20L,
      minibatch_size = min(5L, n_train),
      full_eval_every = 5L,
      demo_candidates = 3L,
      instruction_candidates = 5L
    )
  } else if (auto == "medium") {
    list(
      trials = 50L,
      minibatch_size = min(10L, n_train),
      full_eval_every = 10L,
      demo_candidates = 5L,
      instruction_candidates = 8L
    )
  } else {
    list(
      trials = 100L,
      minibatch_size = min(20L, n_train),
      full_eval_every = 20L,
      demo_candidates = 7L,
      instruction_candidates = 12L
    )
  }
}

generate_mipro_demo_candidates <- function(
  program,
  trainset,
  teleprompter,
  .llm,
  max_candidates
) {
  candidates <- list()

  labeled_tp <- LabeledFewShot(
    k = teleprompter@max_labeled_demos,
    sample = TRUE,
    seed = teleprompter@seed %||% 123L
  )
  labeled_module <- compile_labeled(labeled_tp, program, trainset, .llm = .llm)

  candidates[[length(candidates) + 1L]] <- list(
    id = "labeled",
    demos = labeled_module$demos,
    params = list(
      id = "labeled",
      type = "labeled",
      n_demos = length(labeled_module$demos)
    )
  )

  bootstrap_tp <- BootstrapFewShot(
    metric = teleprompter@metric,
    metric_threshold = teleprompter@metric_threshold,
    max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
    max_labeled_demos = teleprompter@max_labeled_demos,
    max_rounds = 1L,
    teacher_settings = teleprompter@teacher_settings,
    seed = teleprompter@seed %||% 9L
  )

  n_bootstrap <- max(0L, max_candidates - 1L)
  if (n_bootstrap > 0) {
    seeds <- if (!is.null(teleprompter@seed)) {
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
      sample.int(10000L, n_bootstrap)
    } else {
      sample.int(10000L, n_bootstrap)
    }
    for (i in seq_len(n_bootstrap)) {
      bootstrap_tp@seed <- seeds[i]
      boot_result <- tryCatch(
        {
          compile_bootstrap(
            bootstrap_tp,
            program,
            trainset,
            .llm = .llm
          )
        },
        error = function(e) {
          cli::cli_warn(
            c(
              "Bootstrap compilation failed for seed {seeds[i]}",
              "x" = conditionMessage(e),
              "i" = "This candidate will be skipped"
            ),
            class = "dsprrr_mipro_bootstrap_warning"
          )
          NULL
        }
      )

      if (!is.null(boot_result) && length(boot_result$demos) > 0) {
        candidates[[length(candidates) + 1L]] <- list(
          id = paste0("bootstrap_", seeds[i]),
          demos = boot_result$demos,
          params = list(
            id = paste0("bootstrap_", seeds[i]),
            type = "bootstrap",
            seed = seeds[i],
            n_demos = length(boot_result$demos)
          )
        )
      }
    }
  }

  candidates
}

generate_mipro_instruction_candidates <- function(
  program,
  trainset,
  demo_candidates,
  teleprompter,
  max_candidates
) {
  base <- program$signature@instructions
  if (!nzchar(base)) {
    base <- "Use the inputs to produce the requested output."
  }

  dataset_summary <- summarize_mipro_dataset(trainset, program$signature)

  tips <- c(
    "Be concise and accurate.",
    "Reason step-by-step before answering.",
    "Only use information in the inputs.",
    "Avoid assumptions; stick to the data.",
    "Return only the final output field."
  )

  tip_count <- max(0L, max_candidates - 1L)
  sampled_tips <- if (tip_count == 0L) {
    character(0)
  } else if (!is.null(teleprompter@seed)) {
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
    sample(tips, size = min(tip_count, length(tips)))
  } else {
    sample(tips, size = min(tip_count, length(tips)))
  }

  candidates <- list()

  candidates[[length(candidates) + 1L]] <- list(
    id = "base",
    instructions = base,
    params = list(id = "base", tip = NA_character_)
  )

  for (tip in sampled_tips) {
    candidates[[length(candidates) + 1L]] <- list(
      id = paste0("tip_", gsub("[^a-z]+", "_", tolower(tip))),
      instructions = paste(base, dataset_summary, tip, sep = "\n\n"),
      params = list(id = tip, tip = tip)
    )
  }

  candidates
}

summarize_mipro_dataset <- function(trainset, signature) {
  input_names <- vapply(signature@inputs, function(x) x$name, character(1))
  output_col <- find_output_column(trainset, input_names)

  input_preview <- if (length(input_names) > 0) {
    paste(input_names, collapse = ", ")
  } else {
    "(none)"
  }

  output_preview <- output_col %||% "(unknown)"

  paste0(
    "Dataset summary:\n",
    "Inputs: ",
    input_preview,
    "\n",
    "Output: ",
    output_preview
  )
}

expand_mipro_candidates <- function(demo_candidates, instruction_candidates) {
  candidates <- list()
  idx <- 1L

  for (demo in demo_candidates) {
    for (inst in instruction_candidates) {
      candidates[[idx]] <- list(
        id = paste(demo$id, inst$id, sep = "__"),
        demo_id = demo$id,
        instruction_id = inst$id,
        demos = demo$demos,
        instructions = inst$instructions,
        params = list(
          demo_id = demo$id,
          instruction_id = inst$id,
          instructions = inst$instructions,
          n_demos = length(demo$demos)
        )
      )
      idx <- idx + 1L
    }
  }

  candidates
}

#' Print method for MIPROv2
#' @noRd
print_miprov2 <- function(x, ...) {
  cli::cli_h3("MIPROv2 Teleprompter")
  cli::cli_text("{.field auto}: {x@auto %||% 'NULL'}")
  cli::cli_text("{.field max_bootstrapped_demos}: {x@max_bootstrapped_demos}")
  cli::cli_text("{.field max_labeled_demos}: {x@max_labeled_demos}")
  cli::cli_text("{.field seed}: {x@seed %||% 'NULL'}")
  cli::cli_text("{.field num_threads}: {x@num_threads}")
  if (!is.null(x@metric)) {
    cli::cli_text("{.field metric}: <function>")
  }
  if (!is.null(x@log_dir)) {
    cli::cli_text("{.field log_dir}: {x@log_dir}")
  }
  invisible(x)
}
