# Omni Teleprompter
#
# Best-of exploration followed by a fresh continuation optimizer.

#' Omni Teleprompter
#'
#' @include teleprompter.R optimizer-core.R teleprompter-better-together.R
#'
#' @description
#' A meta-teleprompter that explores several optimization strategies from the
#' same seed program, compares their outputs with one shared validation metric,
#' and seeds a fresh continuation optimizer from the winner.
#'
#' Inspired by the Omni meta-optimizer from the
#' [GEPA project](https://github.com/gepa-ai/gepa), this adapts the explore,
#' pick-best, and continue pattern described in the
#' [GEPA Omni
#' announcement](https://gepa-ai.github.io/gepa/blog/2026/07/22/optimize-anything-omni/)
#' to dsprrr modules. The original program remains a candidate throughout, so a
#' regressing explorer or continuation step cannot replace a better program.
#'
#' `Omni()` does not impose a common budget because dsprrr teleprompters expose
#' different native budget controls. Configure comparable budgets on the
#' explorer objects before constructing `Omni()`. Common validation re-scoring
#' of the seed, each explorer result, and the continuation result is additional
#' evaluation work outside those native optimizer budgets.
#'
#' @param metric Metric function used to compare every candidate on the same
#'   validation set.
#' @param explorers Named list of at least two [Teleprompter] objects. Every
#'   explorer starts from an independent copy of the input program.
#' @param continuation A [Teleprompter] object run from the best exploration
#'   candidate.
#' @param metric_threshold Minimum score required to be considered successful.
#' @param max_errors Maximum number of errors allowed during evaluation.
#' @param valset_ratio Fraction of `trainset` to hold out for candidate
#'   comparison when `valset` is not supplied.
#' @param parallel Whether to compile exploration branches concurrently with
#'   mirai. Parallel exploration requires `.llm = NULL`. Each worker creates
#'   its own default chat from `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`, or
#'   `GOOGLE_API_KEY`.
#' @param num_workers Number of mirai workers for parallel exploration. `NULL`
#'   uses one worker per explorer.
#' @param seed Optional whole-number random seed within R's integer range for
#'   reproducible splitting, sequential exploration, and mirai worker streams.
#' @param verbose Whether to print progress messages.
#'
#' @return An `Omni` teleprompter object.
#' @export
#' @examples
#' \dontrun{
#' metric <- metric_exact_match(field = "answer")
#'
#' tp <- Omni(
#'   metric = metric,
#'   explorers = list(
#'     bootstrap = BootstrapFewShotWithRandomSearch(metric = metric),
#'     gepa = GEPA(metric = metric, population_size = 4L, generations = 2L)
#'   ),
#'   continuation = GEPA(
#'     metric = metric,
#'     population_size = 4L,
#'     generations = 2L
#'   )
#' )
#'
#' compiled <- compile(qa_module, tp, trainset, valset = valset, .llm = llm)
#' optimization_result(compiled)$extensions$omni$candidate_programs
#' }
Omni <- S7::new_class(
  "Omni",
  parent = Teleprompter,
  properties = list(
    explorers = S7::new_property(
      S7::class_list,
      validator = function(value) {
        if (!is.list(value) || length(value) < 2L) {
          return("explorers must be a named list of at least two teleprompters")
        }
        if (is.null(names(value)) || !all(nzchar(names(value)))) {
          return("explorers must be named")
        }
        if (anyDuplicated(names(value))) {
          return("explorer names must be unique")
        }
        if (!all(vapply(value, is_teleprompter, logical(1)))) {
          return("all explorers must be Teleprompter objects")
        }
        NULL
      }
    ),
    continuation = S7::new_property(
      S7::class_any,
      validator = function(value) {
        if (!is_teleprompter(value)) {
          return("continuation must be a Teleprompter object")
        }
        NULL
      }
    ),
    valset_ratio = S7::new_property(
      S7::class_numeric,
      default = 0.1,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || value <= 0 || value >= 1) {
          return("valset_ratio must be a single number in (0, 1)")
        }
        NULL
      }
    ),
    parallel = S7::new_property(
      S7::class_logical,
      default = FALSE,
      validator = function(value) {
        if (length(value) != 1L || is.na(value)) {
          return("parallel must be TRUE or FALSE")
        }
        NULL
      }
    ),
    num_workers = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (
          !is.null(value) &&
            (!is.numeric(value) ||
              length(value) != 1L ||
              is.na(value) ||
              value < 1 ||
              value != as.integer(value))
        ) {
          return("num_workers must be a positive integer or NULL")
        }
        NULL
      }
    ),
    seed = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        error <- validate_omni_seed(value)
        if (!is.null(error)) {
          return(error)
        }
        NULL
      }
    ),
    verbose = S7::new_property(
      S7::class_logical,
      default = TRUE,
      validator = function(value) {
        if (length(value) != 1L || is.na(value)) {
          return("verbose must be TRUE or FALSE")
        }
        NULL
      }
    )
  ),
  constructor = function(
    metric,
    explorers,
    continuation,
    metric_threshold = NULL,
    max_errors = 5L,
    valset_ratio = 0.1,
    parallel = FALSE,
    num_workers = NULL,
    seed = NULL,
    verbose = TRUE
  ) {
    S7::new_object(
      Teleprompter(
        metric = metric,
        metric_threshold = metric_threshold,
        max_errors = as.integer(max_errors)
      ),
      explorers = explorers,
      continuation = continuation,
      valset_ratio = valset_ratio,
      parallel = parallel,
      num_workers = if (is.null(num_workers)) {
        NULL
      } else {
        as.integer(num_workers)
      },
      seed = seed,
      verbose = verbose
    )
  }
)

#' Compile method for Omni
#' @noRd
compile_omni <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  explorer_compile_args = list(),
  continuation_compile_args = list(),
  valset_ratio = teleprompter@valset_ratio,
  parallel = teleprompter@parallel,
  num_workers = teleprompter@num_workers,
  seed = teleprompter@seed,
  ...
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("Omni currently only supports Module objects")
  }
  if (!is.data.frame(trainset) || nrow(trainset) == 0L) {
    cli::cli_abort("{.arg trainset} must be a non-empty data frame")
  }
  if (!is.null(valset) && !is.data.frame(valset)) {
    cli::cli_abort("{.arg valset} must be a data frame or NULL")
  }
  if (is.null(teleprompter@metric)) {
    cli::cli_abort("Omni requires a metric function")
  }

  seed_error <- validate_omni_seed(seed)
  if (!is.null(seed_error)) {
    cli::cli_abort(seed_error)
  }

  explorer_compile_args <- validate_omni_explorer_args(
    explorer_compile_args,
    names(teleprompter@explorers)
  )
  validate_omni_step_args(
    continuation_compile_args,
    arg = "continuation_compile_args"
  )
  validate_omni_parallel_args(parallel, num_workers)
  if (isTRUE(parallel) && !is.null(.llm)) {
    cli::cli_abort(c(
      "Parallel Omni exploration requires {.code .llm = NULL}",
      "i" = "Configure worker-visible default chat credentials, or use {.code parallel = FALSE}"
    ))
  }
  if (isTRUE(parallel) && !omni_provider_env_available()) {
    cli::cli_abort(c(
      "Parallel Omni exploration requires worker-visible provider credentials",
      "i" = "Set {.envvar OPENAI_API_KEY}, {.envvar ANTHROPIC_API_KEY}, or {.envvar GOOGLE_API_KEY}",
      "i" = "Otherwise use {.code parallel = FALSE}"
    ))
  }

  split <- better_together_train_val_split(
    trainset = trainset,
    valset = valset,
    valset_ratio = valset_ratio,
    seed = seed
  )
  working_trainset <- split$trainset
  working_valset <- split$valset

  if (nrow(working_trainset) == 0L) {
    cli::cli_abort(c(
      "No training rows remain after validation split",
      "i" = "Decrease {.arg valset_ratio} or provide a separate {.arg valset}"
    ))
  }
  if (is.null(working_valset) || nrow(working_valset) == 0L) {
    cli::cli_abort(c(
      "Omni requires validation data to compare exploration branches",
      "i" = "Supply {.arg valset} or use a larger {.arg trainset}"
    ))
  }

  if (!is.null(seed)) {
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
  }

  baseline <- copy_module(program)
  baseline_score <- omni_score(
    baseline,
    working_valset,
    teleprompter,
    .llm = .llm
  )
  candidates <- list(omni_candidate(
    id = "baseline",
    program = baseline,
    phase = "baseline",
    optimizer = "baseline",
    score = baseline_score
  ))

  if (teleprompter@verbose) {
    cli::cli_alert_info(
      "Omni baseline score: {format_better_together_score(baseline_score)}"
    )
  }

  branch_results <- omni_compile_explorers(
    teleprompter = teleprompter,
    program = program,
    trainset = working_trainset,
    valset = working_valset,
    .llm = .llm,
    explorer_compile_args = explorer_compile_args,
    parallel = parallel,
    num_workers = num_workers,
    seed = seed
  )

  explorer_names <- names(teleprompter@explorers)
  for (i in seq_along(branch_results)) {
    branch <- branch_results[[i]]
    name <- explorer_names[[i]]
    if (!is.na(branch$error)) {
      cli::cli_warn(c(
        "Omni explorer {.field {name}} failed; continuing with other candidates",
        "x" = branch$error
      ))
      candidates <- append(
        candidates,
        list(omni_candidate(
          id = paste0("explore:", name),
          program = NULL,
          phase = "explore",
          optimizer = name,
          score = NA_real_,
          error = branch$error
        ))
      )
      next
    }

    score <- omni_score(
      branch$program,
      working_valset,
      teleprompter,
      .llm = .llm
    )
    candidates <- append(
      candidates,
      list(omni_candidate(
        id = paste0("explore:", name),
        program = branch$program,
        phase = "explore",
        optimizer = name,
        score = score
      ))
    )

    if (teleprompter@verbose) {
      cli::cli_alert_info(
        "Omni explorer {.field {name}} score: {format_better_together_score(score)}"
      )
    }
  }

  exploration_winner <- omni_select_candidate(candidates)
  continuation_name <- class(teleprompter@continuation)[[1]]

  if (teleprompter@verbose) {
    cli::cli_alert_info(
      "Omni continuation: {.cls {continuation_name}} from {.field {exploration_winner$optimizer}}"
    )
  }

  continuation_result <- tryCatch(
    {
      better_together_compile_step(
        optimizer = teleprompter@continuation,
        program = copy_module(exploration_winner$program),
        trainset = working_trainset,
        valset = working_valset,
        .llm = .llm,
        step_args = continuation_compile_args
      )
    },
    error = function(e) e
  )

  if (inherits(continuation_result, "error")) {
    message <- conditionMessage(continuation_result)
    cli::cli_warn(c(
      "Omni continuation failed; returning the exploration winner",
      "x" = message
    ))
    candidates <- append(
      candidates,
      list(omni_candidate(
        id = "continue",
        program = NULL,
        phase = "continue",
        optimizer = continuation_name,
        score = NA_real_,
        error = message
      ))
    )
  } else {
    continuation_score <- omni_score(
      continuation_result,
      working_valset,
      teleprompter,
      .llm = .llm
    )
    candidates <- append(
      candidates,
      list(omni_candidate(
        id = "continue",
        program = continuation_result,
        phase = "continue",
        optimizer = continuation_name,
        score = continuation_score
      ))
    )

    if (teleprompter@verbose) {
      cli::cli_alert_info(
        "Omni continuation score: {format_better_together_score(continuation_score)}"
      )
    }
  }

  best_candidate <- omni_select_candidate(candidates)
  candidate_programs <- omni_candidates_tbl(
    candidates,
    selected_id = best_candidate$id
  )
  candidate_trials <- candidate_programs
  candidate_trials$program_config <- lapply(
    candidate_programs$program,
    function(candidate) candidate$config
  )
  candidate_trials$program <- NULL
  best_trial <- which(candidate_trials$selected)[[1]]
  compilation_error <- !all(is.na(candidate_programs$error))

  best_program <- copy_module(best_candidate$program)
  best_program$config$best_score <- best_candidate$score
  record_optimization_result(
    best_program,
    optimizer = "Omni",
    baseline_score = baseline_score,
    best_score = best_candidate$score,
    best_trial = best_trial,
    best_params = list(
      phase = best_candidate$phase,
      optimizer = best_candidate$optimizer
    ),
    trials = candidate_trials,
    lineage = list(selected_id = best_candidate$id),
    stop_reason = "completed",
    extensions = list(
      explorers = explorer_names,
      continuation = continuation_name,
      exploration_winner = exploration_winner$optimizer,
      best_phase = best_candidate$phase,
      best_optimizer = best_candidate$optimizer,
      candidate_programs = candidate_trials,
      parallel = isTRUE(parallel),
      flag_compilation_error_occurred = compilation_error
    )
  )

  best_program
}

validate_omni_seed <- function(seed) {
  if (is.null(seed)) {
    return(NULL)
  }
  if (
    !is.numeric(seed) ||
      length(seed) != 1L ||
      is.na(seed) ||
      !is.finite(seed) ||
      seed != trunc(seed) ||
      abs(seed) > .Machine$integer.max
  ) {
    return(
      paste0(
        "seed must be a single whole number between ",
        -.Machine$integer.max,
        " and ",
        .Machine$integer.max,
        ", or NULL"
      )
    )
  }
  NULL
}

omni_provider_env_available <- function() {
  any(nzchar(Sys.getenv(c(
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GOOGLE_API_KEY"
  ))))
}

omni_worker_chat <- function() {
  chat <- auto_detect_chat()
  if (is.null(chat)) {
    cli::cli_abort(c(
      "Omni worker could not create a local Chat",
      "i" = "Set a supported provider API key in the worker environment"
    ))
  }
  chat
}

validate_omni_parallel_args <- function(parallel, num_workers) {
  if (!is.logical(parallel) || length(parallel) != 1L || is.na(parallel)) {
    cli::cli_abort("{.arg parallel} must be TRUE or FALSE")
  }
  if (
    !is.null(num_workers) &&
      (!is.numeric(num_workers) ||
        length(num_workers) != 1L ||
        is.na(num_workers) ||
        num_workers < 1 ||
        num_workers != as.integer(num_workers))
  ) {
    cli::cli_abort("{.arg num_workers} must be a positive integer or NULL")
  }
  invisible(NULL)
}

validate_omni_explorer_args <- function(args, explorer_names) {
  if (!is.list(args)) {
    cli::cli_abort("{.arg explorer_compile_args} must be a named list")
  }
  if (length(args) == 0L) {
    return(args)
  }
  if (is.null(names(args)) || !all(nzchar(names(args)))) {
    cli::cli_abort("{.arg explorer_compile_args} must be named")
  }
  if (anyDuplicated(names(args))) {
    cli::cli_abort(
      "{.arg explorer_compile_args} names must be unique"
    )
  }
  unknown <- setdiff(names(args), explorer_names)
  if (length(unknown) > 0L) {
    cli::cli_abort(c(
      "{.arg explorer_compile_args} contains unknown explorer names",
      "x" = "Unknown: {.field {unknown}}",
      "i" = "Valid names: {.field {explorer_names}}"
    ))
  }
  for (name in names(args)) {
    validate_omni_step_args(
      args[[name]],
      arg = paste0("explorer_compile_args$", name)
    )
  }
  args
}

validate_omni_step_args <- function(args, arg) {
  if (!is.list(args)) {
    cli::cli_abort("{.arg {arg}} must be a list")
  }
  if (
    length(args) > 0L &&
      (is.null(names(args)) || !all(nzchar(names(args))))
  ) {
    cli::cli_abort("{.arg {arg}} must contain only named arguments")
  }
  if (anyDuplicated(names(args))) {
    cli::cli_abort("{.arg {arg}} names must be unique")
  }
  blocked <- intersect(
    names(args),
    c("teleprompter", "program", "student", "trainset", "valset", ".llm")
  )
  if (length(blocked) > 0L) {
    cli::cli_abort(c(
      "{.arg {arg}} cannot override Omni core inputs",
      "x" = "Blocked arguments: {.field {blocked}}"
    ))
  }
  invisible(args)
}

omni_compile_explorers <- function(
  teleprompter,
  program,
  trainset,
  valset,
  .llm,
  explorer_compile_args,
  parallel,
  num_workers,
  seed
) {
  if (isTRUE(parallel)) {
    return(omni_compile_explorers_parallel(
      explorers = teleprompter@explorers,
      program = program,
      trainset = trainset,
      valset = valset,
      explorer_compile_args = explorer_compile_args,
      num_workers = num_workers,
      seed = seed
    ))
  }

  lapply(
    names(teleprompter@explorers),
    function(name) {
      omni_compile_explorer(
        optimizer = teleprompter@explorers[[name]],
        program = program,
        trainset = trainset,
        valset = valset,
        .llm = .llm,
        step_args = explorer_compile_args[[name]] %||% list()
      )
    }
  )
}

omni_compile_explorer <- function(
  optimizer,
  program,
  trainset,
  valset,
  .llm,
  step_args
) {
  tryCatch(
    {
      list(
        program = better_together_compile_step(
          optimizer = optimizer,
          program = copy_module(program),
          trainset = trainset,
          valset = valset,
          .llm = .llm,
          step_args = step_args
        ),
        error = NA_character_
      )
    },
    error = function(e) {
      list(program = NULL, error = conditionMessage(e))
    }
  )
}

omni_compile_explorers_parallel <- function(
  explorers,
  program,
  trainset,
  valset,
  explorer_compile_args,
  num_workers,
  seed
) {
  workers <- min(
    length(explorers),
    num_workers %||% length(explorers)
  )
  profile <- paste0(
    "dsprrr_omni_",
    digest::digest(
      list(Sys.getpid(), unclass(Sys.time()), names(explorers)),
      algo = "xxhash64"
    )
  )
  sync <- isTRUE(getOption("dsprrr.omni_parallel_sync", FALSE))

  mirai::daemons(
    n = workers,
    seed = if (is.null(seed)) NULL else as.integer(seed),
    sync = sync,
    .compute = profile
  )
  on.exit(mirai::daemons(0L, .compute = profile), add = TRUE)

  jobs <- lapply(
    names(explorers),
    function(name) {
      list(
        optimizer = explorers[[name]],
        program = copy_module(program),
        trainset = trainset,
        valset = valset,
        step_args = explorer_compile_args[[name]] %||% list(),
        trace_context = current_trace_context()
      )
    }
  )

  mapped <- mirai::mirai_map(
    jobs,
    function(job, compile_fn, worker_chat_fn) {
      tryCatch(
        {
          worker_llm <- worker_chat_fn()
          call_args <- list(
            teleprompter = job$optimizer,
            program = job$program,
            trainset = job$trainset,
            valset = job$valset,
            .llm = worker_llm,
            .trace_context = job$trace_context
          )
          for (name in names(job$step_args)) {
            call_args[[name]] <- job$step_args[[name]]
          }
          list(
            program = do.call(compile_fn, call_args),
            error = NA_character_
          )
        },
        error = function(e) {
          list(program = NULL, error = conditionMessage(e))
        }
      )
    },
    .args = list(
      compile_fn = compile,
      worker_chat_fn = omni_worker_chat
    ),
    .compute = profile
  )[]

  lapply(
    mapped,
    function(result) {
      if (mirai::is_error_value(result)) {
        message <- result$message %||% "Unknown mirai worker failure"
        return(list(program = NULL, error = message))
      }
      result
    }
  )
}

omni_score <- function(program, valset, teleprompter, .llm) {
  better_together_score(
    program,
    valset,
    teleprompter@metric,
    .llm = .llm,
    max_errors = teleprompter@max_errors,
    verbose = FALSE
  )
}

omni_candidate <- function(
  id,
  program,
  phase,
  optimizer,
  score,
  error = NA_character_
) {
  list(
    id = id,
    program = if (is.null(program)) NULL else copy_module(program),
    phase = phase,
    optimizer = optimizer,
    score = score,
    error = error
  )
}

omni_select_candidate <- function(candidates) {
  scores <- vapply(
    candidates,
    function(candidate) {
      if (is.null(candidate$program) || is.na(candidate$score)) {
        -Inf
      } else {
        candidate$score
      }
    },
    numeric(1)
  )
  candidates[[which.max(scores)]]
}

omni_candidates_tbl <- function(candidates, selected_id) {
  ids <- vapply(candidates, `[[`, character(1), "id")
  tibble::tibble(
    phase = vapply(candidates, `[[`, character(1), "phase"),
    optimizer = vapply(candidates, `[[`, character(1), "optimizer"),
    score = vapply(candidates, `[[`, numeric(1), "score"),
    selected = ids == selected_id,
    error = vapply(candidates, `[[`, character(1), "error"),
    program = lapply(candidates, `[[`, "program")
  )
}

# Print an Omni object through its S7 method.
print_omni <- function(x, ...) {
  cli::cli_h3("Omni Teleprompter")
  cli::cli_text("{.field Explorers}: {.field {names(x@explorers)}}")
  cli::cli_text(
    "{.field Continuation}: {.cls {class(x@continuation)[[1]]}}"
  )
  cli::cli_text("{.field Validation split}: {x@valset_ratio}")
  cli::cli_text("{.field Parallel exploration}: {x@parallel}")
  invisible(x)
}

S7::method(print, Omni) <- print_omni
