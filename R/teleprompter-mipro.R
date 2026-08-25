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
#' a discrete Bayesian optimization loop with minibatch evaluation for a root
#' Predict module. For graphs with nested predictors such as RLM, it optimizes
#' child instructions only and requires `max_bootstrapped_demos = 0L`; nested
#' demo bootstrapping fails explicitly until predictor-local evidence is
#' available.
#'
#' @param metric A metric function for evaluating predictions (required).
#' @param task_model Optional ellmer Chat used to evaluate tasks. `NULL` uses
#'   the `.llm` supplied to `compile()`.
#' @param teacher_settings List of settings for the teacher model.
#' @param max_bootstrapped_demos Maximum number of bootstrapped demonstrations.
#' @param max_labeled_demos Maximum number of labeled demonstrations.
#' @param auto Auto-tuned settings: "light", "medium", "heavy", or NULL.
#' @param num_candidates Optional override for number of instruction candidates.
#' @param num_threads Number of threads to use for evaluation.
#' @param max_errors Maximum number of errors allowed during optimization.
#' @param seed Random seed for reproducibility.
#' @param track_stats Whether to track trial history.
#' @param log_dir Directory for trial logging.
#' @param metric_threshold Minimum score required for acceptance.
#'
#' @return A `MIPROv2` teleprompter object.
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
#' qa_module <- module(signature("question -> answer"))
#' trainset <- data.frame(question = "Capital of France?", answer = "Paris")
#' valset <- data.frame(question = "Capital of Japan?", answer = "Tokyo")
#' llm <- ellmer::chat_openai()
#' compiled <- compile(qa_module, tp, trainset, valset = valset, .llm = llm)
#' }
MIPROv2 <- S7::new_class(
  "MIPROv2",
  parent = Teleprompter,
  properties = list(
    task_model = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (!is.null(value) && !is_ellmer_chat(value)) {
          return("task_model must be NULL or an ellmer Chat R6 object")
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
mipro_named_predictors <- function(program, boundaries = "respect") {
  parameters <- named_parameters(
    program,
    include_root = TRUE,
    boundaries = boundaries
  )
  parameters[vapply(
    parameters,
    inherits,
    logical(1),
    what = "PredictModule"
  )]
}

mipro_uses_component_candidates <- function(program) {
  predictors <- mipro_named_predictors(program, boundaries = "cross")
  length(predictors) > 0L &&
    !(length(predictors) == 1L &&
      identical(names(predictors), "$") &&
      identical(predictors[[1L]], program))
}

mipro_apply_candidate <- function(program, candidate) {
  compiled <- copy_module(program)

  if (!is.null(candidate$components)) {
    predictors <- mipro_named_predictors(compiled, boundaries = "respect")
    component_paths <- names(candidate$components)
    predictor_paths <- names(predictors)
    if (
      is.null(component_paths) ||
        !setequal(component_paths, predictor_paths)
    ) {
      cli::cli_abort(
        c(
          "MIPROv2 component candidate does not match the program graph",
          "x" = "Candidate paths: {toString(component_paths)}",
          "x" = "Predictor paths: {toString(predictor_paths)}"
        ),
        class = "dsprrr_mipro_component_mismatch"
      )
    }

    for (path in predictor_paths) {
      component <- candidate$components[[path]]
      predictor <- predictors[[path]]
      if (!is.null(component$demos)) {
        predictor$demos <- lapply(component$demos, identity)
      }
      if (!is.null(component$instructions)) {
        predictor$apply_optimization_params(list(
          instructions = component$instructions
        ))
      }
    }
    return(compiled)
  }

  compiled$demos <- candidate$demos %||% list()
  compiled$signature <- Signature(
    inputs = compiled$signature@inputs,
    output_type = compiled$signature@output_type,
    instructions = candidate$instructions %||%
      compiled$signature@instructions
  )
  compiled
}

mipro_result_best_params <- function(candidate) {
  params <- candidate$params %||% list()
  params$instructions <- NULL
  params
}

mipro_finalize_program <- function(
  compiled,
  settings,
  minibatch_size,
  demo_candidates,
  instruction_candidates,
  bo_result,
  partial = FALSE,
  component_mode = FALSE
) {
  budget_summary <- bo_result$budget_summary
  trials <- bo_result$trial_history %||% tibble::tibble()
  best_candidate <- bo_result$best_candidate
  best_score <- bo_result$best_score %||% NA_real_
  best_trial <- bo_result$best_trial
  details <- list(
    auto = settings$auto,
    planned_trials = settings$trials,
    minibatch_size = minibatch_size,
    full_eval_every = settings$full_eval_every,
    demo_candidates = lapply(demo_candidates, function(x) x$params),
    instruction_candidates = lapply(
      instruction_candidates,
      function(x) x$params
    ),
    candidate_scope = if (component_mode) {
      "predictor_components"
    } else {
      "root_program"
    },
    demo_mode = if (component_mode) "preserved" else "optimized",
    effective_labeled_demos = if (component_mode) 0L else NULL,
    effective_bootstrapped_demos = if (component_mode) 0L else NULL
  )
  record_optimization_result(
    compiled,
    optimizer = "MIPROv2",
    status = if (isTRUE(partial)) "partial" else "completed",
    best_score = best_score,
    best_trial = best_trial,
    best_params = mipro_result_best_params(best_candidate),
    trials = trials,
    lineage = list(best_candidate_id = best_candidate$id %||% NULL),
    budget = budget_summary,
    stop_reason = optimization_stop_reason(budget_summary),
    extensions = details
  )
  compiled
}

#' Compile method for MIPROv2 with deterministic checkpoint resume
#' @noRd
compile_mipro <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  control = NULL,
  ...
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("MIPROv2 currently only supports Module objects")
  }
  if (!is.data.frame(trainset)) {
    cli::cli_abort("{.arg trainset} must be a data frame")
  }
  if (nrow(trainset) == 0L) {
    cli::cli_warn("Empty trainset provided, returning unmodified program")
    return(program)
  }
  if (is.null(teleprompter@metric)) {
    cli::cli_abort("MIPROv2 requires a metric function")
  }

  component_mode <- mipro_uses_component_candidates(program)
  if (component_mode && teleprompter@max_bootstrapped_demos > 0L) {
    cli::cli_abort(
      c(
        "MIPROv2 cannot bootstrap demonstrations for nested predictors yet",
        "x" = paste(
          "The root trace does not provide bounded predictor-local inputs and",
          "outputs for each child."
        ),
        "i" = paste(
          "Set {.arg max_bootstrapped_demos = 0} to optimize child",
          "instructions without changing their demonstrations."
        )
      ),
      class = "dsprrr_mipro_graph_bootstrap_unsupported"
    )
  }

  evalset <- valset %||% trainset
  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control,
    num_threads = teleprompter@num_threads
  )
  settings <- resolve_mipro_settings(
    auto = teleprompter@auto,
    num_candidates = teleprompter@num_candidates,
    n_train = nrow(trainset)
  )
  settings$auto <- teleprompter@auto
  minibatch_size <- min(settings$minibatch_size, nrow(trainset))

  checkpoint_context <- NULL
  if (optimizer_checkpoint_enabled(control)) {
    registry <- artifact_validate_registry(control@checkpoint_registry)
    checkpoint_context <- optimizer_checkpoint_begin(
      optimizer_name = "MIPROv2",
      optimizer_version = 1L,
      program = program,
      data = list(trainset = trainset, valset = valset),
      metric = teleprompter@metric,
      config = list(
        auto = teleprompter@auto,
        num_candidates = teleprompter@num_candidates,
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        teacher_settings = teleprompter@teacher_settings,
        metric_threshold = teleprompter@metric_threshold,
        seed = teleprompter@seed,
        candidate_scope = if (component_mode) {
          "predictor_components"
        } else {
          "root"
        },
        settings = settings,
        demo_runtime = optimizer_checkpoint_effective_runtime_identity(
          program,
          .llm = .llm,
          registry = registry,
          path = "demo_runtime"
        ),
        task_runtime = optimizer_checkpoint_effective_runtime_identity(
          program,
          .llm = teleprompter@task_model %||% .llm,
          registry = registry,
          path = "task_runtime"
        )
      ),
      control = control,
      initial_state = list(
        kind = "mipro_v1",
        demo_generation = NULL,
        instruction_candidates = list(),
        bo = NULL
      ),
      initial_phase = "demo_candidates"
    )
    if (identical(checkpoint_context$phase, "complete")) {
      return(checkpoint_context$best_program)
    }
  }

  budget <- checkpoint_context$budget %||% new_optimizer_budget(control)
  state <- checkpoint_context$search_state %||%
    list(
      kind = "mipro_v1",
      demo_generation = NULL,
      instruction_candidates = list(),
      bo = NULL
    )
  if (
    !is.list(state) ||
      !setequal(
        names(state),
        c("kind", "demo_generation", "instruction_candidates", "bo")
      ) ||
      !identical(state$kind, "mipro_v1")
  ) {
    cli::cli_abort(
      "MIPRO checkpoint search state is malformed",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }

  if (!is.null(checkpoint_context) && isTRUE(checkpoint_context$resumed)) {
    outer_rng <- optimizer_checkpoint_capture_rng()
    optimizer_checkpoint_restore_rng(checkpoint_context$rng)
    on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
  }

  best_partial <- checkpoint_context$best_program %||% copy_module(program)
  write_checkpoint <- function(phase, best_program = best_partial) {
    best_partial <<- best_program
    if (!is.null(checkpoint_context)) {
      optimizer_checkpoint_write(
        checkpoint_context,
        phase = phase,
        search_state = state,
        lineage = list(),
        best_program = best_program
      )
    }
    invisible(NULL)
  }

  demo_result <- if (component_mode) {
    generate_mipro_component_demo_candidates(
      program = program,
      teleprompter = teleprompter,
      budget = budget,
      resume_state = state$demo_generation
    )
  } else {
    generate_mipro_demo_candidates(
      program = program,
      trainset = trainset,
      teleprompter = teleprompter,
      .llm = .llm,
      max_candidates = settings$demo_candidates,
      control = control,
      budget = budget,
      resume_state = state$demo_generation,
      on_progress = function(...) {
        update <- list(...)
        state$demo_generation <<- update$state
        write_checkpoint("demo_candidates", update$best_program)
      },
      return_state = TRUE
    )
  }
  required_demo_fields <- c("candidates", "state", "complete", "budget")
  if (
    !is.list(demo_result) ||
      !all(required_demo_fields %in% names(demo_result)) ||
      !is.logical(demo_result$complete) ||
      length(demo_result$complete) != 1L ||
      is.na(demo_result$complete)
  ) {
    cli::cli_abort(
      "Internal MIPRO demo generation returned an invalid result",
      class = "dsprrr_mipro_internal_error"
    )
  }
  demo_candidates <- demo_result$candidates
  state$demo_generation <- demo_result$state

  if (!isTRUE(demo_result$complete) || optimizer_budget_stopped(budget)) {
    summary <- optimizer_budget_summary(budget)
    partial_result <- list(
      best_candidate = NULL,
      trial_history = tibble::tibble(),
      budget_summary = summary
    )
    partial <- mipro_finalize_program(
      best_partial,
      settings,
      minibatch_size,
      demo_candidates,
      list(),
      partial_result,
      partial = TRUE,
      component_mode = component_mode
    )
    write_checkpoint("demo_candidates", partial)
    return(partial)
  }

  instruction_candidates <- state$instruction_candidates
  if (length(instruction_candidates) == 0L) {
    instruction_candidates <- if (component_mode) {
      generate_mipro_component_instruction_candidates(
        program = program,
        trainset = trainset,
        teleprompter = teleprompter,
        max_candidates = settings$instruction_candidates
      )
    } else {
      generate_mipro_instruction_candidates(
        program = program,
        trainset = trainset,
        demo_candidates = demo_candidates,
        teleprompter = teleprompter,
        max_candidates = settings$instruction_candidates
      )
    }
    state$instruction_candidates <- instruction_candidates
    write_checkpoint("discrete_bo", best_partial)
  }

  candidate_grid <- if (component_mode) {
    expand_mipro_component_candidates(
      demo_candidates = demo_candidates,
      instruction_candidates = instruction_candidates
    )
  } else {
    expand_mipro_candidates(
      demo_candidates = demo_candidates,
      instruction_candidates = instruction_candidates
    )
  }
  if (length(candidate_grid) == 0L) {
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

  trial_log <- if (!is.null(control@log_dir)) {
    TrialLog$new(optimizer_name = "MIPROv2", log_dir = control@log_dir)
  } else {
    NULL
  }

  eval_fn <- function(
    candidate,
    eval_type,
    trial_idx,
    budget,
    unit_id,
    partial_records,
    on_progress
  ) {
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
    compiled <- mipro_apply_candidate(program, candidate)
    optimizer_eval_program(
      compiled,
      data,
      teleprompter@metric,
      .llm = teleprompter@task_model %||% .llm,
      control = control,
      budget = budget,
      stage = paste0("discrete_bo_", eval_type),
      unit_id = unit_id,
      partial_records = partial_records,
      on_progress = on_progress
    )
  }

  bo_result <- run_discrete_bo(
    candidates = candidate_grid,
    eval_fn = eval_fn,
    control = control,
    max_trials = settings$trials,
    minibatch_size = minibatch_size,
    full_eval_every = settings$full_eval_every,
    trial_log = trial_log,
    seed = teleprompter@seed,
    track_stats = teleprompter@track_stats,
    budget = budget,
    resume_state = state$bo,
    resumable_eval = TRUE,
    on_progress = function(...) {
      update <- list(...)
      state$bo <<- update$state
      candidate <- update$best_candidate %||% candidate_grid[[1L]]
      write_checkpoint("discrete_bo", mipro_apply_candidate(program, candidate))
    }
  )
  state$bo <- bo_result$resume_state %||% state$bo

  best_candidate <- bo_result$best_candidate
  if (is.null(best_candidate)) {
    if (optimizer_budget_stopped(budget)) {
      best_candidate <- candidate_grid[[1L]]
    } else {
      cli::cli_abort("MIPROv2 failed to select a best candidate")
    }
  }
  compiled <- mipro_apply_candidate(program, best_candidate)
  bo_result$best_candidate <- best_candidate
  compiled <- mipro_finalize_program(
    compiled,
    settings,
    minibatch_size,
    demo_candidates,
    instruction_candidates,
    bo_result,
    partial = !isTRUE(bo_result$complete),
    component_mode = component_mode
  )
  write_checkpoint(
    if (isTRUE(bo_result$complete)) "complete" else "discrete_bo",
    compiled
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

generate_mipro_component_demo_candidates <- function(
  program,
  teleprompter,
  budget,
  resume_state = NULL
) {
  predictors <- mipro_named_predictors(program)
  if (length(predictors) == 0L) {
    cli::cli_abort(
      "MIPROv2 found no mutable Predict children in the program graph",
      class = "dsprrr_mipro_no_component_predictors"
    )
  }

  state <- resume_state %||%
    list(
      kind = "mipro_component_demos_v1",
      candidates = NULL
    )
  if (
    !is.list(state) ||
      !setequal(names(state), c("kind", "candidates")) ||
      !identical(state$kind, "mipro_component_demos_v1")
  ) {
    cli::cli_abort(
      "MIPRO component demo-candidate state is malformed",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }

  if (is.null(state$candidates)) {
    demo_counts <- vapply(predictors, function(x) length(x$demos), integer(1))
    state$candidates <- list(list(
      id = "preserved_component_demos",
      params = list(
        id = "preserved_component_demos",
        type = "component_instruction_only",
        demo_counts = as.list(demo_counts),
        requested_labeled_demos = teleprompter@max_labeled_demos,
        effective_labeled_demos = 0L,
        effective_bootstrapped_demos = 0L
      )
    ))
  }

  list(
    candidates = state$candidates,
    state = state,
    complete = TRUE,
    budget = budget
  )
}

generate_mipro_demo_candidates <- function(
  program,
  trainset,
  teleprompter,
  .llm,
  max_candidates,
  control = NULL,
  budget = NULL,
  resume_state = NULL,
  on_progress = NULL,
  return_state = FALSE
) {
  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control
  )
  budget <- budget %||% new_optimizer_budget(control)
  state <- resume_state %||%
    list(
      candidates = list(),
      bootstrap_states = list(),
      bootstrap_phases = list(),
      completed_seeds = integer(),
      seeds = NULL
    )
  required <- c(
    "candidates",
    "bootstrap_states",
    "bootstrap_phases",
    "completed_seeds",
    "seeds"
  )
  if (!is.list(state) || !setequal(names(state), required)) {
    cli::cli_abort(
      "MIPRO demo-candidate resume state is malformed",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  candidates <- state$candidates

  report_progress <- function(best_program = program) {
    state$candidates <<- candidates
    if (is.function(on_progress)) {
      on_progress(
        state = state,
        best_program = best_program,
        budget = budget
      )
    }
    invisible(NULL)
  }

  candidate_ids <- vapply(
    candidates,
    function(candidate) candidate$id %||% NA_character_,
    character(1)
  )
  if (!"labeled" %in% candidate_ids) {
    labeled_tp <- LabeledFewShot(
      k = teleprompter@max_labeled_demos,
      sample = TRUE,
      seed = teleprompter@seed %||% 123L
    )
    labeled_module <- compile_labeled(
      labeled_tp,
      program,
      trainset,
      .llm = .llm
    )
    candidates[[length(candidates) + 1L]] <- list(
      id = "labeled",
      demos = labeled_module$demos,
      params = list(
        id = "labeled",
        type = "labeled",
        n_demos = length(labeled_module$demos)
      )
    )
    report_progress(labeled_module)
  }

  n_bootstrap <- max(0L, max_candidates - 1L)
  if (is.null(state$seeds)) {
    state$seeds <- mipro_bootstrap_seeds(teleprompter@seed, n_bootstrap)
    report_progress()
  }
  seeds <- as.integer(state$seeds)
  if (length(seeds) != n_bootstrap) {
    cli::cli_abort(
      "MIPRO bootstrap seed state is incompatible with this run",
      class = "dsprrr_optimizer_checkpoint_malformed"
    )
  }
  if (length(seeds) > 0L) {
    for (seed in seeds) {
      if (seed %in% state$completed_seeds) {
        next
      }
      if (optimizer_budget_stopped(budget)) {
        break
      }
      key <- as.character(seed)
      bootstrap_tp <- BootstrapFewShot(
        metric = teleprompter@metric,
        metric_threshold = teleprompter@metric_threshold,
        max_bootstrapped_demos = teleprompter@max_bootstrapped_demos,
        max_labeled_demos = teleprompter@max_labeled_demos,
        max_rounds = 1L,
        teacher_settings = teleprompter@teacher_settings,
        seed = as.integer(seed)
      )
      boot_result <- tryCatch(
        {
          compile_bootstrap(
            bootstrap_tp,
            program,
            trainset,
            .llm = .llm,
            control = control,
            .optimizer_budget = budget,
            .checkpoint_namespace = paste0("mipro:demo:seed:", seed),
            .resume_state = state$bootstrap_states[[key]],
            .checkpoint_callback = function(...) {
              update <- list(...)
              state$bootstrap_states[[key]] <<- update$state
              state$bootstrap_phases[[key]] <<- update$phase
              report_progress(update$best_program)
            }
          )
        },
        error = function(e) {
          cli::cli_warn(
            c(
              "Bootstrap compilation failed for seed {seed}",
              "x" = conditionMessage(e),
              "i" = "This candidate will be skipped"
            ),
            class = "dsprrr_mipro_bootstrap_warning"
          )
          NULL
        }
      )

      bootstrap_complete <- identical(
        state$bootstrap_phases[[key]],
        "complete"
      )
      if (is.null(boot_result) && !optimizer_budget_stopped(budget)) {
        bootstrap_complete <- TRUE
      }
      if (!bootstrap_complete) {
        break
      }
      state$completed_seeds <- unique(c(state$completed_seeds, seed))
      if (!is.null(boot_result) && length(boot_result$demos) > 0L) {
        candidates[[length(candidates) + 1L]] <- list(
          id = paste0("bootstrap_", seed),
          demos = boot_result$demos,
          params = list(
            id = paste0("bootstrap_", seed),
            type = "bootstrap",
            seed = seed,
            n_demos = length(boot_result$demos)
          )
        )
      }
      report_progress(boot_result %||% program)
    }
  }

  state$candidates <- candidates
  complete <- all(seeds %in% state$completed_seeds)
  if (return_state) {
    return(list(
      candidates = candidates,
      state = state,
      complete = complete,
      budget = budget
    ))
  }
  candidates
}

mipro_bootstrap_seeds <- function(seed, n) {
  if (n <= 0L) {
    return(integer())
  }
  outer_rng <- optimizer_checkpoint_capture_rng()
  on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
  if (!is.null(seed)) {
    set.seed(seed)
  }
  as.integer(sample.int(10000L, n))
}

generate_mipro_instruction_candidates <- function(
  program,
  trainset,
  demo_candidates,
  teleprompter,
  max_candidates
) {
  generate_mipro_instruction_candidates_for_signature(
    signature = program$signature,
    summary_signature = program$signature,
    trainset = trainset,
    max_candidates = max_candidates,
    seed = teleprompter@seed
  )
}

generate_mipro_instruction_candidates_for_signature <- function(
  signature,
  summary_signature,
  trainset,
  max_candidates,
  seed
) {
  base <- signature@instructions
  if (!nzchar(base)) {
    base <- "Use the inputs to produce the requested output."
  }

  dataset_summary <- summarize_mipro_dataset(trainset, summary_signature)

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
  } else if (!is.null(seed)) {
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

generate_mipro_component_instruction_candidates <- function(
  program,
  trainset,
  teleprompter,
  max_candidates
) {
  predictors <- mipro_named_predictors(program)
  if (length(predictors) == 0L) {
    cli::cli_abort(
      "MIPROv2 found no mutable Predict children in the program graph",
      class = "dsprrr_mipro_no_component_predictors"
    )
  }

  by_path <- Map(
    function(predictor, index) {
      seed <- if (is.null(teleprompter@seed)) {
        NULL
      } else {
        as.integer(teleprompter@seed + index - 1L)
      }
      generate_mipro_instruction_candidates_for_signature(
        signature = predictor$signature,
        summary_signature = program$signature,
        trainset = trainset,
        max_candidates = max_candidates,
        seed = seed
      )
    },
    predictor = unname(predictors),
    index = seq_along(predictors)
  )
  names(by_path) <- names(predictors)

  list(list(
    id = "component_instruction_candidates",
    by_path = by_path,
    params = list(
      id = "component_instruction_candidates",
      type = "predictor_components",
      paths = lapply(by_path, function(candidates) {
        lapply(candidates, function(candidate) candidate$params)
      })
    )
  ))
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

mipro_component_instruction_index_sets <- function(by_path) {
  candidate_counts <- lengths(by_path)
  if (length(candidate_counts) == 0L || any(candidate_counts == 0L)) {
    return(list())
  }

  # RLM has two predictors, so its complete component product remains small.
  # Bound generic graphs before materializing their Cartesian product.
  if (prod(as.double(candidate_counts)) <= 512) {
    grid <- do.call(
      expand.grid,
      c(
        unname(lapply(candidate_counts, seq_len)),
        list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
      )
    )
    names(grid) <- names(by_path)
    return(lapply(seq_len(nrow(grid)), function(index) {
      values <- as.integer(grid[index, , drop = TRUE])
      stats::setNames(values, names(by_path))
    }))
  }

  index_sets <- list(stats::setNames(
    rep.int(1L, length(by_path)),
    names(by_path)
  ))

  for (path in names(by_path)) {
    if (candidate_counts[[path]] < 2L) {
      next
    }
    for (index in seq.int(2L, candidate_counts[[path]])) {
      coordinate <- index_sets[[1L]]
      coordinate[[path]] <- index
      index_sets[[length(index_sets) + 1L]] <- coordinate
    }
  }

  max_count <- max(candidate_counts)
  if (max_count >= 2L) {
    for (index in seq.int(2L, max_count)) {
      aligned <- vapply(
        candidate_counts,
        function(count) min(index, count),
        integer(1)
      )
      index_sets[[length(index_sets) + 1L]] <- aligned
    }
  }

  keys <- vapply(index_sets, paste, character(1), collapse = ",")
  index_sets[!duplicated(keys)]
}

expand_mipro_component_candidates <- function(
  demo_candidates,
  instruction_candidates
) {
  if (
    length(instruction_candidates) != 1L ||
      is.null(instruction_candidates[[1L]]$by_path)
  ) {
    cli::cli_abort(
      "MIPRO component instruction candidates are malformed",
      class = "dsprrr_mipro_component_mismatch"
    )
  }

  by_path <- instruction_candidates[[1L]]$by_path
  index_sets <- mipro_component_instruction_index_sets(by_path)
  candidates <- list()
  candidate_index <- 1L

  for (demo in demo_candidates) {
    for (indices in index_sets) {
      selected <- Map(
        function(path, index) by_path[[path]][[index]],
        path = names(by_path),
        index = unname(indices)
      )
      names(selected) <- names(by_path)
      components <- lapply(selected, function(candidate) {
        list(instructions = candidate$instructions)
      })
      instruction_ids <- vapply(selected, `[[`, character(1), "id")
      aggregate_instruction_id <- paste(
        paste(names(instruction_ids), instruction_ids, sep = "="),
        collapse = "|"
      )
      demo_counts <- demo$params$demo_counts %||% list()

      candidates[[candidate_index]] <- list(
        id = sprintf("components_%03d", candidate_index),
        demo_id = demo$id,
        instruction_id = aggregate_instruction_id,
        components = components,
        params = list(
          demo_id = demo$id,
          instruction_ids = as.list(instruction_ids),
          instructions = lapply(selected, `[[`, "instructions"),
          n_demos = sum(unlist(demo_counts), na.rm = TRUE)
        )
      )
      candidate_index <- candidate_index + 1L
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
