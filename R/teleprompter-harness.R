# Agentic optimization harnesses
#
# AutoResearch and Meta-Harness share candidate, evaluation, checkpoint, and
# sandbox machinery while retaining different ownership of the search loop.

harness_common_properties <- function() {
  list(
    max_iterations = S7::new_property(
      S7::class_integer,
      default = 20L,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || value < 1L) {
          return("max_iterations must be a positive integer")
        }
        NULL
      }
    ),
    patience = S7::new_property(
      S7::class_integer,
      default = 6L,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || value < 1L) {
          return("patience must be a positive integer")
        }
        NULL
      }
    ),
    target_score = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (
          !is.null(value) &&
            (!is.numeric(value) ||
              length(value) != 1L ||
              is.na(value) ||
              !is.finite(value))
        ) {
          return("target_score must be one finite number or NULL")
        }
        NULL
      }
    ),
    max_context_examples = S7::new_property(
      S7::class_integer,
      default = 20L,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || value < 1L) {
          return("max_context_examples must be a positive integer")
        }
        NULL
      }
    ),
    max_feedback_examples = S7::new_property(
      S7::class_integer,
      default = 8L,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || value < 1L) {
          return("max_feedback_examples must be a positive integer")
        }
        NULL
      }
    ),
    max_agent_steps = S7::new_property(
      S7::class_integer,
      default = 4L,
      validator = function(value) {
        if (length(value) != 1L || is.na(value) || value < 1L) {
          return("max_agent_steps must be a positive integer")
        }
        NULL
      }
    ),
    sandbox = S7::new_property(
      S7::class_logical,
      default = TRUE,
      validator = function(value) {
        if (length(value) != 1L || is.na(value)) {
          return("sandbox must be TRUE or FALSE")
        }
        NULL
      }
    ),
    seed = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (
          !is.null(value) &&
            (!is.numeric(value) ||
              length(value) != 1L ||
              is.na(value) ||
              !is.finite(value) ||
              value != trunc(value) ||
              abs(value) > .Machine$integer.max)
        ) {
          return("seed must be one whole number in the R integer range or NULL")
        }
        NULL
      }
    ),
    log_dir = S7::new_property(
      S7::class_any,
      default = NULL,
      validator = function(value) {
        if (
          !is.null(value) &&
            (!is.character(value) ||
              length(value) != 1L ||
              is.na(value) ||
              !nzchar(value))
        ) {
          return("log_dir must be one non-empty string or NULL")
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
  )
}

#' AutoResearch Teleprompter
#'
#' @description
#' Runs a persistent research agent that owns an explicit
#' hypothesize-sandbox-evaluate-keep-or-revert loop. The agent can branch from
#' any prior candidate, inspect structured per-example feedback, request
#' sandboxed R experiments, and decide when to finish. dsprrr retains control of
#' budgets, evaluation, checkpointing, and final best-candidate selection.
#'
#' @details
#' `AutoResearch()` is inspired by Andrej Karpathy's
#' [`autoresearch`](https://github.com/karpathy/autoresearch) and the
#' AutoResearch engine in the
#' [GEPA optimize-anything project](https://github.com/gepa-ai/gepa).
#' This is an R-native implementation for dsprrr `Module` graphs, not a port of
#' either command-line harness.
#'
#' Candidates are complete, validated snapshots of every optimizable leaf
#' module. A single experiment can therefore change instructions and templates
#' across multiple pipeline components jointly. Candidate evaluation always
#' runs in the host process through dsprrr's optimizer ledger. Only exploratory
#' R code is sent to `runner`; with the default `sandbox = TRUE`, `runner` must
#' advertise an operating-system sandbox such as [mcp_repl_runner()].
#'
#' @section Compilation arguments:
#' In addition to the standard [compile()] arguments, this teleprompter accepts
#' `.agent_llm` for the research agent, `runner` for sandboxed analysis,
#' `control` for optimizer budgets and checkpointing, and `objective` for
#' multi-objective selection. Named arguments in `...`, such as `.cache`, are
#' forwarded to candidate evaluation.
#'
#' @param metric Metric used to evaluate candidates.
#' @param metric_threshold Optional success threshold inherited from
#'   [Teleprompter].
#' @param max_errors Consecutive optimizer error budget.
#' @param max_iterations Maximum evaluated experiments after the baseline.
#' @param patience Stop after this many evaluated experiments without
#'   improvement.
#' @param target_score Optional score at which optimization stops.
#' @param max_context_examples Maximum training examples exposed to the
#'   research agent.
#' @param max_feedback_examples Maximum failed examples returned after each
#'   evaluation.
#' @param max_agent_steps Maximum consecutive sandbox or invalid actions before
#'   the harness requires evaluation progress.
#' @param sandbox Whether an OS-sandboxed runner is required. Defaults to TRUE.
#' @param seed Optional random seed.
#' @param log_dir Optional directory for a durable [TrialLog].
#' @param verbose Whether to report progress.
#'
#' @return An `AutoResearch` teleprompter.
#' @export
#' @examples
#' \dontrun{
#' research <- AutoResearch(
#'   metric = metric_exact_match(field = "answer"),
#'   max_iterations = 12L
#' )
#' compiled <- compile(
#'   research,
#'   program,
#'   trainset,
#'   valset = valset,
#'   .llm = task_chat,
#'   .agent_llm = research_chat,
#'   runner = mcp_repl_runner(),
#'   control = optimizer_control(
#'     max_trials = 13L,
#'     max_cost = 5,
#'     checkpoint_path = "autoresearch.rds"
#'   )
#' )
#' }
AutoResearch <- S7::new_class(
  "AutoResearch",
  parent = Teleprompter,
  properties = harness_common_properties()
)

#' Meta-Harness Teleprompter
#'
#' @description
#' Runs an outer-loop optimization harness that starts a fresh proposer session
#' on every iteration. Each proposer sees the current frontier, candidate
#' lineage, evaluation traces, and training context, then proposes a batch of
#' joint program edits. The R outer loop validates and evaluates every unique
#' candidate and alone decides what enters the frontier.
#'
#' @details
#' `MetaHarness()` is inspired by the Meta-Harness engine in the
#' [GEPA optimize-anything project](https://github.com/gepa-ai/gepa) and the
#' associated [Meta-Harness paper](https://arxiv.org/abs/2603.28052).
#' It preserves the important separation between an untrusted coding proposer
#' and a trusted evaluator while adapting the candidate representation to
#' dsprrr module graphs.
#'
#' A proposer may request one or more R analyses before submitting its batch.
#' Those analyses execute only through `runner`; with the default
#' `sandbox = TRUE`, the runner must advertise an OS sandbox such as
#' [mcp_repl_runner()]. Proposer sessions are fresh by design, so each
#' iteration must reason from the persisted frontier rather than hidden chat
#' history. The proposer must be an ellmer `Chat`; it is cloned and reset
#' automatically before each iteration.
#'
#' @section Compilation arguments:
#' In addition to the standard [compile()] arguments, this teleprompter accepts
#' `.agent_llm` for the proposer, `runner` for sandboxed analysis, `control` for
#' optimizer budgets and checkpointing, and `objective` for multi-objective
#' selection. Named arguments in `...`, such as `.cache`, are forwarded to
#' candidate evaluation.
#'
#' @inheritParams AutoResearch
#' @param max_candidates_per_iteration Maximum candidates evaluated from one
#'   proposer batch.
#' @param frontier_size Maximum scored candidates summarized to each proposer.
#'
#' @return A `MetaHarness` teleprompter.
#' @export
#' @examples
#' \dontrun{
#' harness <- MetaHarness(
#'   metric = metric_exact_match(field = "answer"),
#'   max_iterations = 8L,
#'   max_candidates_per_iteration = 4L
#' )
#' compiled <- compile(
#'   harness,
#'   program,
#'   trainset,
#'   valset = valset,
#'   .agent_llm = proposer_chat,
#'   runner = mcp_repl_runner()
#' )
#' }
MetaHarness <- S7::new_class(
  "MetaHarness",
  parent = Teleprompter,
  properties = c(
    harness_common_properties(),
    list(
      max_candidates_per_iteration = S7::new_property(
        S7::class_integer,
        default = 4L,
        validator = function(value) {
          if (length(value) != 1L || is.na(value) || value < 1L) {
            return("max_candidates_per_iteration must be a positive integer")
          }
          NULL
        }
      ),
      frontier_size = S7::new_property(
        S7::class_integer,
        default = 8L,
        validator = function(value) {
          if (length(value) != 1L || is.na(value) || value < 1L) {
            return("frontier_size must be a positive integer")
          }
          NULL
        }
      )
    )
  )
)

#' Compile method for AutoResearch
#' @noRd
compile_autoresearch <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  .agent_llm = NULL,
  runner = NULL,
  control = NULL,
  objective = NULL,
  ...
) {
  outer_rng <- optimizer_checkpoint_capture_rng()
  on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
  setup <- harness_setup(
    teleprompter = teleprompter,
    name = "AutoResearch",
    program = program,
    trainset = trainset,
    valset = valset,
    .llm = .llm,
    .agent_llm = .agent_llm,
    runner = runner,
    control = control,
    objective = objective,
    eval_args = list(...)
  )
  on.exit(harness_save_trial_log(setup$trial_log), add = TRUE)

  state <- setup$state
  best_program <- setup$best_program
  if (!isTRUE(state$baseline_complete)) {
    baseline <- harness_evaluate_snapshot(
      setup = setup,
      state = state,
      snapshot = harness_program_snapshot(program),
      parent_id = NULL,
      name = "baseline",
      rationale = "Unmodified seed program.",
      iteration = 0L,
      best_program = best_program,
      stage = "autoresearch_baseline"
    )
    state <- baseline$state
    best_program <- baseline$best_program
    state$baseline_complete <- TRUE
    harness_checkpoint(
      setup$checkpoint,
      "baseline",
      state,
      best_program
    )
  }

  agent <- harness_agent_instance(setup$agent)
  no_eval_steps <- 0L

  while (!harness_search_done(state, teleprompter, setup$budget)) {
    step <- as.integer(state$agent_steps + 1L)
    prompt <- harness_autoresearch_prompt(setup, state)
    response <- harness_agent_call(
      agent = agent,
      prompt = prompt,
      budget = setup$budget,
      stage = "autoresearch_agent",
      unit_id = paste0("autoresearch:agent:", step)
    )
    state$agent_steps <- step

    if (!is.null(response$condition)) {
      state$events <- append(
        state$events,
        list(harness_event(
          "agent_error",
          message = conditionMessage(response$condition)
        ))
      )
      no_eval_steps <- no_eval_steps + 1L
      if (
        no_eval_steps >= teleprompter@max_agent_steps ||
          optimizer_budget_stopped(setup$budget)
      ) {
        state$termination <- "agent_error"
        break
      }
      next
    }
    if (!isTRUE(response$started)) {
      state$termination <- "budget"
      break
    }

    action <- harness_normalize_action(response$value)
    if (identical(action$action, "finish")) {
      state$events <- append(
        state$events,
        list(harness_event(
          "agent_finish",
          rationale = action$rationale
        ))
      )
      state$termination <- "agent_finished"
      break
    }

    if (identical(action$action, "sandbox")) {
      sandbox <- harness_run_sandbox(
        setup,
        state,
        code = action$code,
        rationale = action$rationale
      )
      state <- sandbox$state
      no_eval_steps <- no_eval_steps + 1L
      harness_checkpoint(
        setup$checkpoint,
        "sandbox",
        state,
        best_program
      )
      if (no_eval_steps >= teleprompter@max_agent_steps) {
        state$termination <- "no_evaluation_progress"
      }
      next
    }

    if (!identical(action$action, "propose")) {
      state$events <- append(
        state$events,
        list(harness_event(
          "invalid_action",
          message = "The research agent returned an unknown action."
        ))
      )
      no_eval_steps <- no_eval_steps + 1L
      if (no_eval_steps >= teleprompter@max_agent_steps) {
        state$termination <- "no_evaluation_progress"
      }
      next
    }

    proposals <- action$candidates
    if (length(proposals) == 0L) {
      state$events <- append(
        state$events,
        list(harness_event(
          "invalid_action",
          message = "The research agent proposed no candidate."
        ))
      )
      no_eval_steps <- no_eval_steps + 1L
      if (no_eval_steps >= teleprompter@max_agent_steps) {
        state$termination <- "no_evaluation_progress"
      }
      next
    }

    before <- state$best_score
    iteration <- as.integer(state$iteration + 1L)
    candidates_before <- length(state$candidates)
    evaluated <- harness_evaluate_proposal(
      setup = setup,
      state = state,
      proposal = proposals[[1L]],
      iteration = iteration,
      best_program = best_program,
      stage = "autoresearch_candidate"
    )
    state <- evaluated$state
    best_program <- evaluated$best_program
    accepted <- length(state$candidates) > candidates_before
    no_eval_steps <- if (accepted) {
      0L
    } else {
      no_eval_steps + 1L
    }
    if (no_eval_steps >= teleprompter@max_agent_steps) {
      state$termination <- "no_evaluation_progress"
    }
    if (accepted) {
      state$iteration <- iteration
      state$no_improvement <- if (
        harness_score_improved(state$best_score, before)
      ) {
        0L
      } else {
        as.integer(state$no_improvement + 1L)
      }
    }
    harness_checkpoint(
      setup$checkpoint,
      "search",
      state,
      best_program
    )
  }

  harness_finalize(
    setup = setup,
    state = state,
    best_program = best_program,
    name = "AutoResearch"
  )
}

#' Compile method for MetaHarness
#' @noRd
compile_meta_harness <- function(
  teleprompter,
  program,
  trainset,
  valset = NULL,
  .llm = NULL,
  .agent_llm = NULL,
  runner = NULL,
  control = NULL,
  objective = NULL,
  ...
) {
  outer_rng <- optimizer_checkpoint_capture_rng()
  on.exit(optimizer_checkpoint_restore_rng(outer_rng), add = TRUE)
  setup <- harness_setup(
    teleprompter = teleprompter,
    name = "MetaHarness",
    program = program,
    trainset = trainset,
    valset = valset,
    .llm = .llm,
    .agent_llm = .agent_llm,
    runner = runner,
    control = control,
    objective = objective,
    eval_args = list(...)
  )
  on.exit(harness_save_trial_log(setup$trial_log), add = TRUE)

  state <- setup$state
  best_program <- setup$best_program
  if (!isTRUE(state$baseline_complete)) {
    baseline <- harness_evaluate_snapshot(
      setup = setup,
      state = state,
      snapshot = harness_program_snapshot(program),
      parent_id = NULL,
      name = "baseline",
      rationale = "Unmodified seed program.",
      iteration = 0L,
      best_program = best_program,
      stage = "meta_harness_baseline"
    )
    state <- baseline$state
    best_program <- baseline$best_program
    state$baseline_complete <- TRUE
    harness_checkpoint(
      setup$checkpoint,
      "baseline",
      state,
      best_program
    )
  }

  while (!harness_search_done(state, teleprompter, setup$budget)) {
    state$iteration <- as.integer(state$iteration + 1L)
    iteration <- state$iteration
    agent <- harness_agent_instance(setup$agent, require_fresh = TRUE)
    action <- NULL

    for (step in seq_len(teleprompter@max_agent_steps)) {
      response <- harness_agent_call(
        agent = agent,
        prompt = harness_meta_prompt(setup, state, step),
        budget = setup$budget,
        stage = "meta_harness_agent",
        unit_id = paste0(
          "meta-harness:iteration:",
          iteration,
          ":agent:",
          step
        )
      )
      state$agent_steps <- as.integer(state$agent_steps + 1L)

      if (!is.null(response$condition)) {
        state$events <- append(
          state$events,
          list(harness_event(
            "agent_error",
            iteration = iteration,
            message = conditionMessage(response$condition)
          ))
        )
        action <- NULL
        break
      }
      if (!isTRUE(response$started)) {
        state$termination <- "budget"
        action <- NULL
        break
      }

      action <- harness_normalize_action(response$value)
      if (identical(action$action, "sandbox")) {
        sandbox <- harness_run_sandbox(
          setup,
          state,
          code = action$code,
          rationale = action$rationale
        )
        state <- sandbox$state
        harness_checkpoint(
          setup$checkpoint,
          "sandbox",
          state,
          best_program
        )
        next
      }
      break
    }

    if (is.null(action)) {
      state$no_improvement <- as.integer(state$no_improvement + 1L)
      harness_checkpoint(
        setup$checkpoint,
        "search",
        state,
        best_program
      )
      next
    }
    if (identical(action$action, "finish")) {
      state$events <- append(
        state$events,
        list(harness_event(
          "agent_finish",
          iteration = iteration,
          rationale = action$rationale
        ))
      )
      state$termination <- "agent_finished"
      break
    }
    if (!identical(action$action, "propose")) {
      state$events <- append(
        state$events,
        list(harness_event(
          "invalid_action",
          iteration = iteration,
          message = "The proposer did not submit a candidate batch."
        ))
      )
      state$no_improvement <- as.integer(state$no_improvement + 1L)
      harness_checkpoint(
        setup$checkpoint,
        "search",
        state,
        best_program
      )
      next
    }

    proposals <- utils::head(
      action$candidates,
      teleprompter@max_candidates_per_iteration
    )
    if (length(proposals) == 0L) {
      state$events <- append(
        state$events,
        list(harness_event(
          "invalid_action",
          iteration = iteration,
          message = "The proposer submitted an empty candidate batch."
        ))
      )
      state$no_improvement <- as.integer(state$no_improvement + 1L)
      harness_checkpoint(
        setup$checkpoint,
        "search",
        state,
        best_program
      )
      next
    }
    before <- state$best_score
    for (proposal in proposals) {
      if (optimizer_budget_stopped(setup$budget)) {
        break
      }
      evaluated <- harness_evaluate_proposal(
        setup = setup,
        state = state,
        proposal = proposal,
        iteration = iteration,
        best_program = best_program,
        stage = "meta_harness_candidate"
      )
      state <- evaluated$state
      best_program <- evaluated$best_program
      harness_checkpoint(
        setup$checkpoint,
        "search",
        state,
        best_program
      )
    }
    state$frontier_ids <- harness_frontier_ids(
      state$candidates,
      teleprompter@frontier_size
    )
    state$no_improvement <- if (
      harness_score_improved(state$best_score, before)
    ) {
      0L
    } else {
      as.integer(state$no_improvement + 1L)
    }
    harness_checkpoint(
      setup$checkpoint,
      "search",
      state,
      best_program
    )
  }

  harness_finalize(
    setup = setup,
    state = state,
    best_program = best_program,
    name = "MetaHarness"
  )
}

harness_setup <- function(
  teleprompter,
  name,
  program,
  trainset,
  valset,
  .llm,
  .agent_llm,
  runner,
  control,
  objective,
  eval_args
) {
  if (!inherits(program, "Module")) {
    cli::cli_abort("{name} currently only supports Module objects")
  }
  if (!is.data.frame(trainset) || nrow(trainset) == 0L) {
    cli::cli_abort("{.arg trainset} must be a non-empty data frame")
  }
  if (!is.null(valset) && (!is.data.frame(valset) || nrow(valset) == 0L)) {
    cli::cli_abort("{.arg valset} must be a non-empty data frame or NULL")
  }
  if (!is.function(teleprompter@metric)) {
    cli::cli_abort("{name} requires a metric function")
  }
  if (
    !is.null(objective) &&
      (!is.character(objective) ||
        length(objective) != 1L ||
        is.na(objective))
  ) {
    cli::cli_abort("{.arg objective} must be one string or NULL")
  }
  harness_validate_eval_args(eval_args)

  runner <- harness_validate_runner(runner, required = teleprompter@sandbox)
  agent <- harness_resolve_agent(.agent_llm, .llm)
  if (S7::S7_inherits(teleprompter, MetaHarness)) {
    harness_require_fresh_agent(agent)
  }
  control <- optimizer_control_for_teleprompter(
    teleprompter,
    control = control
  )
  dataset <- valset %||% trainset
  config <- harness_checkpoint_config(teleprompter, objective)
  checkpoint <- optimizer_checkpoint_begin(
    optimizer_name = name,
    optimizer_version = 1L,
    program = program,
    data = dataset,
    metric = teleprompter@metric,
    config = config,
    control = control,
    initial_state = harness_initial_state(),
    initial_phase = "initialized"
  )

  if (!checkpoint$resumed && !is.null(teleprompter@seed)) {
    set.seed(as.integer(teleprompter@seed))
  } else if (checkpoint$resumed) {
    optimizer_checkpoint_restore_rng(checkpoint$rng)
  }

  list(
    teleprompter = teleprompter,
    name = name,
    program = program,
    trainset = trainset,
    dataset = dataset,
    .llm = .llm,
    agent = agent,
    runner = runner,
    control = control,
    budget = checkpoint$budget,
    checkpoint = checkpoint,
    state = harness_restore_state(checkpoint$search_state),
    best_program = checkpoint$best_program,
    objective = objective %||% "",
    eval_args = eval_args,
    trial_log = if (!is.null(control@log_dir)) {
      TrialLog$new(optimizer_name = name, log_dir = control@log_dir)
    } else {
      NULL
    }
  )
}

harness_checkpoint_config <- function(teleprompter, objective) {
  properties <- c(
    "max_iterations",
    "patience",
    "target_score",
    "max_context_examples",
    "max_feedback_examples",
    "max_agent_steps",
    "sandbox",
    "seed"
  )
  if (S7::S7_inherits(teleprompter, MetaHarness)) {
    properties <- c(
      properties,
      "max_candidates_per_iteration",
      "frontier_size"
    )
  }
  values <- lapply(properties, function(name) S7::prop(teleprompter, name))
  names(values) <- properties
  c(values, list(objective = objective %||% ""))
}

harness_initial_state <- function() {
  list(
    baseline_complete = FALSE,
    iteration = 0L,
    agent_steps = 0L,
    no_improvement = 0L,
    best_id = NULL,
    best_score = NA_real_,
    baseline_score = NA_real_,
    frontier_ids = character(),
    candidates = list(),
    events = list(),
    termination = NULL
  )
}

harness_restore_state <- function(state) {
  defaults <- harness_initial_state()
  for (name in intersect(names(state), names(defaults))) {
    defaults[[name]] <- state[[name]]
  }
  defaults$iteration <- as.integer(defaults$iteration)
  defaults$agent_steps <- as.integer(defaults$agent_steps)
  defaults$no_improvement <- as.integer(defaults$no_improvement)
  defaults
}

harness_validate_runner <- function(runner, required) {
  if (!isTRUE(required)) {
    return(NULL)
  }
  if (is.null(runner)) {
    cli::cli_abort(c(
      "An OS-sandboxed code runner is required",
      "i" = "Supply {.code runner = mcp_repl_runner()}.",
      "i" = "Set {.code sandbox = FALSE} only when the agent must not execute code."
    ))
  }
  validate_code_runner(runner)
  policy <- runner$policy()
  if (isTRUE(required) && !isTRUE(policy$sandboxed)) {
    cli::cli_abort(
      c(
        "The agentic harness requires an OS-sandboxed runner",
        "x" = "Runner {.val {policy$backend}} advertises {.code sandboxed = FALSE}.",
        "i" = "Use {.fn mcp_repl_runner} or set {.code sandbox = FALSE} to disable agent code execution."
      ),
      class = "dsprrr_runner_sandbox_required"
    )
  }
  runner
}

harness_validate_eval_args <- function(eval_args) {
  if (!is.list(eval_args)) {
    cli::cli_abort("Internal evaluator arguments must be a list")
  }
  if (
    length(eval_args) > 0L &&
      (is.null(names(eval_args)) || !all(nzchar(names(eval_args))))
  ) {
    cli::cli_abort(
      "Additional harness compile arguments must all be named"
    )
  }
  if (anyDuplicated(names(eval_args))) {
    cli::cli_abort(
      "Additional harness compile argument names must be unique"
    )
  }
  blocked <- intersect(
    names(eval_args),
    c(
      "program",
      "dataset",
      "metric",
      ".llm",
      "control",
      "budget",
      "stage",
      "unit_id"
    )
  )
  if (length(blocked) > 0L) {
    cli::cli_abort(c(
      "Additional compile arguments cannot override harness evaluator inputs",
      "x" = "Blocked arguments: {.field {blocked}}"
    ))
  }
  invisible(eval_args)
}

harness_resolve_agent <- function(.agent_llm, .llm) {
  agent <- .agent_llm %||% .llm
  if (is.null(agent)) {
    agent <- get_default_chat(create = TRUE)
  }
  harness_validate_agent(agent)
}

harness_validate_agent <- function(agent) {
  if (!is_ellmer_chat(agent)) {
    cli::cli_abort(c(
      "Agentic harnesses require an ellmer Chat proposer",
      "x" = "Got {.cls {class(agent)[1]}}.",
      "i" = "Supply an ellmer Chat through {.arg .agent_llm} or {.arg .llm}."
    ))
  }
  agent
}

harness_agent_instance <- function(spec, require_fresh = FALSE) {
  if (isTRUE(require_fresh)) {
    return(harness_fresh_agent(spec))
  }
  harness_validate_agent(spec)
}

harness_require_fresh_agent <- function(spec) {
  harness_validate_agent(spec)
  clone <- tryCatch(
    spec[["clone"]],
    error = function(e) NULL
  )
  if (!is.function(clone)) {
    cli::cli_abort(c(
      "MetaHarness requires a fresh proposer session for every iteration",
      "x" = "The supplied ellmer Chat does not provide {.code clone()}.",
      "i" = "Supply a cloneable ellmer Chat through {.arg .agent_llm}."
    ))
  }
  invisible(spec)
}

harness_fresh_agent <- function(agent) {
  clone_ellmer_chat(
    agent,
    arg = ".agent_llm",
    reset_turns = TRUE
  )
}

harness_action_type <- function() {
  edit <- ellmer::type_object(
    path = ellmer::type_string(
      "Stable module path from the editable program manifest."
    ),
    instructions = ellmer::type_string(
      "Complete replacement instructions; omit to keep unchanged.",
      required = FALSE
    ),
    template = ellmer::type_string(
      "Complete replacement prompt template; omit to keep unchanged.",
      required = FALSE
    )
  )
  candidate <- ellmer::type_object(
    name = ellmer::type_string("Short experiment name."),
    rationale = ellmer::type_string("Testable hypothesis for this candidate."),
    parent_id = ellmer::type_string(
      "Candidate id to branch from; omit to branch from the current best.",
      required = FALSE
    ),
    edits = ellmer::type_array(
      edit,
      "One or more edits across module paths."
    )
  )
  ellmer::type_object(
    action = ellmer::type_enum(
      c("propose", "sandbox", "finish"),
      "propose candidates, run sandboxed R analysis, or finish."
    ),
    rationale = ellmer::type_string(
      "Reason for the selected action."
    ),
    code = ellmer::type_string(
      "R code for a sandbox action; omit otherwise.",
      required = FALSE
    ),
    candidates = ellmer::type_array(
      candidate,
      "Candidate batch for a propose action; omit otherwise.",
      required = FALSE
    )
  )
}

harness_agent_call <- function(
  agent,
  prompt,
  budget,
  stage,
  unit_id
) {
  optimizer_budgeted_provider_call(
    budget = budget,
    model = agent,
    stage = stage,
    unit_id = unit_id,
    call = function() {
      agent$chat_structured(
        prompt,
        type = harness_action_type(),
        echo = "none"
      )
    },
    success = function(value, condition) {
      is.null(condition) && !is.null(value)
    },
    work_unit = "harness_agent_call"
  )
}

harness_normalize_action <- function(value) {
  if (is.character(value) && length(value) == 1L) {
    value <- tryCatch(
      jsonlite::fromJSON(value, simplifyVector = FALSE),
      error = function(e) list()
    )
  }
  if (!is.list(value)) {
    value <- list()
  }
  action <- harness_scalar_text(value$action)
  rationale <- harness_scalar_text(value$rationale)
  code <- harness_scalar_text(value$code)
  candidates <- value$candidates %||% list()
  if (is.data.frame(candidates)) {
    candidates <- split(candidates, seq_len(nrow(candidates)))
  }
  if (!is.list(candidates)) {
    candidates <- list()
  }
  list(
    action = action,
    rationale = rationale,
    code = code,
    candidates = candidates
  )
}

harness_scalar_text <- function(value) {
  if (
    is.null(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !(is.character(value) || is.factor(value))
  ) {
    return("")
  }
  as.character(value)
}

harness_program_snapshot <- function(program) {
  modules <- named_parameters(
    program,
    include_root = TRUE,
    boundaries = "respect"
  )
  if (length(modules) == 0L) {
    cli::cli_abort(c(
      "The program has no optimizable module components",
      "i" = "Agentic harnesses edit leaf modules with {.code apply_optimization_params()}."
    ))
  }

  components <- lapply(
    names(modules),
    function(path) {
      module <- modules[[path]]
      instructions <- tryCatch(
        module$signature@instructions,
        error = function(e) NULL
      )
      template <- tryCatch(module$template, error = function(e) NULL)
      editable <- character()
      if (is.character(instructions) && length(instructions) == 1L) {
        editable <- c(editable, "instructions")
      }
      if (is.character(template) && length(template) == 1L) {
        editable <- c(editable, "template")
      }
      list(
        path = path,
        module_class = class(module)[[1L]],
        editable = editable,
        instructions = if ("instructions" %in% editable) {
          instructions
        } else {
          NULL
        },
        template = if ("template" %in% editable) template else NULL
      )
    }
  )
  names(components) <- names(modules)
  list(components = components)
}

harness_snapshot_fingerprint <- function(snapshot) {
  digest::digest(snapshot, algo = "sha256", serialize = TRUE)
}

harness_apply_snapshot <- function(program, snapshot) {
  candidate <- copy_module(program)
  modules <- named_parameters(
    candidate,
    include_root = TRUE,
    boundaries = "respect"
  )
  snapshot_paths <- names(snapshot$components %||% list())
  if (!setequal(snapshot_paths, names(modules))) {
    cli::cli_abort(c(
      "Candidate module paths do not match the seed program",
      "x" = "Candidate paths: {.val {snapshot_paths}}",
      "i" = "Program paths: {.val {names(modules)}}"
    ))
  }

  for (path in names(modules)) {
    component <- snapshot$components[[path]]
    params <- list()
    if ("instructions" %in% component$editable) {
      params$instructions <- component$instructions
    }
    if ("template" %in% component$editable) {
      params$template <- component$template
    }
    if (length(params) > 0L) {
      modules[[path]]$apply_optimization_params(params)
    }
  }
  candidate
}

harness_candidate_from_proposal <- function(state, proposal) {
  if (!is.list(proposal)) {
    cli::cli_abort("A candidate proposal must be an object")
  }
  parent_id <- as.character(
    proposal$parent_id %||% state$best_id %||% ""
  )[[1L]]
  parent <- harness_candidate_by_id(state$candidates, parent_id)
  if (is.null(parent)) {
    cli::cli_abort(c(
      "Candidate proposal references an unknown parent",
      "x" = "Unknown parent id: {.val {parent_id}}",
      "i" = "Use a candidate id from the supplied frontier or history."
    ))
  }

  snapshot <- parent$snapshot
  edits <- proposal$edits %||% list()
  if (is.data.frame(edits)) {
    edits <- split(edits, seq_len(nrow(edits)))
  }
  if (!is.list(edits) || length(edits) == 0L) {
    cli::cli_abort("Candidate proposal must contain at least one edit")
  }

  for (edit in edits) {
    if (!is.list(edit)) {
      cli::cli_abort("Each candidate edit must be an object")
    }
    path <- as.character(edit$path %||% "")[[1L]]
    if (!path %in% names(snapshot$components)) {
      cli::cli_abort(c(
        "Candidate edit references an unknown module path",
        "x" = "Unknown path: {.val {path}}",
        "i" = "Valid paths: {.val {names(snapshot$components)}}"
      ))
    }
    component <- snapshot$components[[path]]
    changed <- FALSE
    for (field in c("instructions", "template")) {
      if (!field %in% names(edit)) {
        next
      }
      if (!field %in% component$editable) {
        cli::cli_abort(
          "Field {.field {field}} is not editable for module {.val {path}}"
        )
      }
      value <- edit[[field]]
      if (!is.character(value) || length(value) != 1L || is.na(value)) {
        cli::cli_abort(
          "Candidate field {.field {field}} at {.val {path}} must be one string"
        )
      }
      component[[field]] <- value
      changed <- TRUE
    }
    if (!changed) {
      cli::cli_abort(
        "Candidate edit for {.val {path}} changes no editable field"
      )
    }
    snapshot$components[[path]] <- component
  }

  list(
    name = as.character(proposal$name %||% "candidate")[[1L]],
    rationale = as.character(proposal$rationale %||% "")[[1L]],
    parent_id = parent_id,
    snapshot = snapshot,
    fingerprint = harness_snapshot_fingerprint(snapshot)
  )
}

harness_candidate_by_id <- function(candidates, id) {
  if (is.null(id) || !nzchar(id)) {
    return(NULL)
  }
  index <- which(vapply(
    candidates,
    function(candidate) identical(candidate$id, id),
    logical(1)
  ))
  if (length(index) == 0L) NULL else candidates[[index[[1L]]]]
}

harness_candidate_by_fingerprint <- function(candidates, fingerprint) {
  index <- which(vapply(
    candidates,
    function(candidate) identical(candidate$fingerprint, fingerprint),
    logical(1)
  ))
  if (length(index) == 0L) NULL else candidates[[index[[1L]]]]
}

harness_evaluate_proposal <- function(
  setup,
  state,
  proposal,
  iteration,
  best_program,
  stage
) {
  candidate <- tryCatch(
    harness_candidate_from_proposal(state, proposal),
    error = function(e) e
  )
  if (inherits(candidate, "error")) {
    state$events <- append(
      state$events,
      list(harness_event(
        "candidate_rejected",
        iteration = iteration,
        message = conditionMessage(candidate)
      ))
    )
    return(list(state = state, best_program = best_program))
  }

  duplicate <- harness_candidate_by_fingerprint(
    state$candidates,
    candidate$fingerprint
  )
  if (!is.null(duplicate)) {
    state$events <- append(
      state$events,
      list(harness_event(
        "candidate_duplicate",
        iteration = iteration,
        candidate_id = duplicate$id,
        rationale = candidate$rationale
      ))
    )
    return(list(state = state, best_program = best_program))
  }

  harness_evaluate_snapshot(
    setup = setup,
    state = state,
    snapshot = candidate$snapshot,
    parent_id = candidate$parent_id,
    name = candidate$name,
    rationale = candidate$rationale,
    iteration = iteration,
    best_program = best_program,
    stage = stage
  )
}

harness_evaluate_snapshot <- function(
  setup,
  state,
  snapshot,
  parent_id,
  name,
  rationale,
  iteration,
  best_program,
  stage
) {
  fingerprint <- harness_snapshot_fingerprint(snapshot)
  candidate_id <- if (iteration == 0L) {
    paste0("baseline-", substr(fingerprint, 1L, 12L))
  } else {
    paste0(
      "candidate-",
      iteration,
      "-",
      substr(fingerprint, 1L, 12L)
    )
  }
  program <- harness_apply_snapshot(setup$program, snapshot)
  eval_result <- do.call(
    optimizer_eval_candidate,
    c(
      list(
        program = program,
        dataset = setup$dataset,
        metric = setup$teleprompter@metric,
        .llm = setup$.llm,
        control = setup$control,
        budget = setup$budget,
        stage = stage,
        unit_id = paste0(
          tolower(gsub("[^A-Za-z0-9]+", "-", setup$name)),
          ":candidate:",
          fingerprint
        )
      ),
      setup$eval_args
    )
  )
  score <- eval_result@mean_score
  previous_best <- state$best_score
  improved <- is.null(state$best_id) ||
    (!is.na(score) && (is.na(previous_best) || score > previous_best))

  record <- list(
    id = candidate_id,
    parent_id = parent_id,
    name = name,
    rationale = rationale,
    iteration = as.integer(iteration),
    score = as.numeric(score),
    fingerprint = fingerprint,
    improved = isTRUE(improved),
    status = if (is.na(score)) "failed" else "evaluated",
    feedback = harness_eval_feedback(
      eval_result,
      setup$teleprompter@max_feedback_examples
    ),
    snapshot = snapshot
  )
  state$candidates <- append(state$candidates, list(record))
  if (iteration == 0L) {
    state$baseline_score <- score
  }
  if (isTRUE(improved)) {
    state$best_id <- candidate_id
    state$best_score <- score
    best_program <- program
  }
  state$frontier_ids <- harness_frontier_ids(
    state$candidates,
    if (S7::S7_inherits(setup$teleprompter, MetaHarness)) {
      setup$teleprompter@frontier_size
    } else {
      8L
    }
  )
  state$events <- append(
    state$events,
    list(harness_event(
      "candidate_evaluated",
      iteration = iteration,
      candidate_id = candidate_id,
      score = score,
      improved = isTRUE(improved)
    ))
  )
  harness_log_evaluation(setup$trial_log, record, eval_result)

  if (setup$teleprompter@verbose) {
    verb <- if (isTRUE(improved)) "new best" else "evaluated"
    cli::cli_alert_info(
      "{setup$name} {verb}: {.field {name}} = {harness_format_score(score)}"
    )
  }

  list(
    state = state,
    best_program = best_program,
    eval_result = eval_result
  )
}

harness_log_evaluation <- function(trial_log, record, eval_result) {
  if (is.null(trial_log)) {
    return(invisible(NULL))
  }
  trial <- create_trial(
    optimizer_name = trial_log$optimizer_name,
    params = list(
      candidate_id = record$id,
      parent_id = record$parent_id,
      name = record$name,
      iteration = record$iteration,
      rationale = record$rationale,
      fingerprint = record$fingerprint,
      snapshot = record$snapshot
    )
  )
  trial <- start_trial(trial)
  trial <- complete_trial(trial, eval_result)
  trial_log$add_trial(trial)
  invisible(trial)
}

harness_save_trial_log <- function(trial_log) {
  if (!is.null(trial_log)) {
    trial_log$save()
  }
  invisible(trial_log)
}

harness_eval_feedback <- function(eval_result, limit) {
  examples <- eval_result@examples
  if (!is.data.frame(examples) || nrow(examples) == 0L) {
    return(list())
  }
  scores <- examples$score
  order <- order(
    ifelse(is.na(scores), -Inf, scores),
    na.last = FALSE
  )
  examples <- examples[utils::head(order, limit), , drop = FALSE]
  lapply(seq_len(nrow(examples)), function(index) {
    row <- examples[index, , drop = FALSE]
    input_names <- grep("^input_", names(row), value = TRUE)
    list(
      row_id = as.integer(row$row_id[[1L]] %||% index),
      score = as.numeric(row$score[[1L]] %||% NA_real_),
      inputs = stats::setNames(
        lapply(input_names, function(name) {
          harness_value_text(row[[name]][[1L]])
        }),
        sub("^input_", "", input_names)
      ),
      predicted = harness_value_text(row$predicted[[1L]] %||% NA),
      feedback = as.character(row$feedback[[1L]] %||% NA_character_),
      error = as.character(row$error[[1L]] %||% NA_character_)
    )
  })
}

harness_value_text <- function(value, max_chars = 1000L) {
  text <- tryCatch(
    {
      if (is.character(value) && length(value) == 1L) {
        value
      } else {
        as.character(jsonlite::toJSON(
          value,
          auto_unbox = TRUE,
          null = "null",
          na = "null",
          dataframe = "rows"
        ))
      }
    },
    error = function(e) {
      paste(utils::capture.output(utils::str(value)), collapse = "\n")
    }
  )
  if (length(text) == 0L || is.na(text[[1L]])) {
    return("NA")
  }
  text <- paste(text, collapse = "\n")
  if (nchar(text) > max_chars) {
    paste0(substr(text, 1L, max_chars), "... [truncated]")
  } else {
    text
  }
}

harness_frontier_ids <- function(candidates, size) {
  if (length(candidates) == 0L) {
    return(character())
  }
  scores <- vapply(
    candidates,
    function(candidate) {
      score <- candidate$score %||% NA_real_
      if (is.na(score)) -Inf else score
    },
    numeric(1)
  )
  ids <- vapply(candidates, `[[`, character(1), "id")
  ids[utils::head(order(scores, decreasing = TRUE), size)]
}

harness_frontier <- function(state) {
  lapply(state$frontier_ids, function(id) {
    candidate <- harness_candidate_by_id(state$candidates, id)
    list(
      id = candidate$id,
      parent_id = candidate$parent_id,
      name = candidate$name,
      rationale = candidate$rationale,
      score = candidate$score,
      feedback = candidate$feedback,
      snapshot = candidate$snapshot
    )
  })
}

harness_run_sandbox <- function(setup, state, code, rationale) {
  if (is.null(setup$runner)) {
    state$events <- append(
      state$events,
      list(harness_event(
        "sandbox_rejected",
        rationale = rationale,
        message = "Sandbox execution is disabled."
      ))
    )
    return(list(state = state, result = NULL))
  }
  if (
    !is.character(code) || length(code) != 1L || is.na(code) || !nzchar(code)
  ) {
    state$events <- append(
      state$events,
      list(harness_event(
        "sandbox_rejected",
        rationale = rationale,
        message = "Sandbox action did not contain R code."
      ))
    )
    return(list(state = state, result = NULL))
  }
  if (nchar(code) > 12000L) {
    state$events <- append(
      state$events,
      list(harness_event(
        "sandbox_rejected",
        rationale = rationale,
        message = "Sandbox code exceeded the 12,000 character limit."
      ))
    )
    return(list(state = state, result = NULL))
  }

  result <- tryCatch(
    setup$runner$execute(
      code,
      context = list(
        objective = setup$objective,
        program = harness_program_snapshot(setup$program),
        frontier = harness_frontier(state),
        trainset = harness_train_context(
          setup$trainset,
          setup$teleprompter@max_context_examples
        )
      )
    ),
    error = function(e) {
      list(
        success = FALSE,
        error = conditionMessage(e),
        duration_ms = NA_real_
      )
    }
  )
  if (!is.list(result)) {
    result <- list(
      success = FALSE,
      error = paste0(
        "Runner returned unsupported response type: ",
        paste(class(result), collapse = "/")
      ),
      duration_ms = NA_real_
    )
  }
  output <- harness_value_text(
    if (isTRUE(result$success)) result$result else result$error,
    max_chars = 4000L
  )
  state$events <- append(
    state$events,
    list(harness_event(
      "sandbox",
      rationale = rationale,
      success = isTRUE(result$success),
      output = output,
      duration_ms = result$duration_ms %||% NA_real_
    ))
  )
  list(state = state, result = result)
}

harness_train_context <- function(trainset, limit) {
  rows <- utils::head(trainset, limit)
  lapply(seq_len(nrow(rows)), function(index) {
    values <- lapply(
      rows[index, , drop = FALSE],
      function(value) value[[1L]]
    )
    c(list(example_id = paste0("train-", index)), values)
  })
}

harness_autoresearch_prompt <- function(setup, state) {
  paste(
    "You are the persistent research agent inside dsprrr AutoResearch.",
    "Own the experiment loop: form a hypothesis, branch from a prior candidate,",
    "request sandboxed R analysis when useful, evaluate one candidate at a time,",
    "and finish only when further experiments are unlikely to help.",
    "",
    "The evaluator and budgets are controlled by dsprrr. Never attempt to",
    "replace, bypass, or reproduce the evaluator. Validation data is the",
    "optimization set; a final test set remains outside this harness.",
    "",
    "Editable program manifest:",
    harness_json(harness_program_snapshot(setup$program)),
    "",
    "Objective:",
    if (nzchar(setup$objective)) setup$objective else "(maximize the metric)",
    "",
    "Visible training examples:",
    harness_json(harness_train_context(
      setup$trainset,
      setup$teleprompter@max_context_examples
    )),
    "",
    "Current research state:",
    harness_json(harness_state_for_agent(state)),
    "",
    "Return exactly one action. For propose, submit exactly one candidate.",
    "Every edit is a complete replacement value at a valid module path.",
    sep = "\n"
  )
}

harness_meta_prompt <- function(setup, state, step) {
  paste(
    "You are a fresh proposer inside dsprrr MetaHarness.",
    "The trusted R outer loop owns evaluation and frontier selection.",
    "Use the supplied evidence to propose a diverse batch of falsifiable",
    "program edits. You may first request sandboxed R analysis, but you have",
    setup$teleprompter@max_agent_steps,
    "total proposer steps in this iteration.",
    "",
    "Do not attempt to replace or bypass the evaluator. Prefer targeted edits",
    "that explain which failure mode they address. Candidates may jointly edit",
    "multiple module paths.",
    "",
    "Proposer step:",
    step,
    "",
    "Editable program manifest:",
    harness_json(harness_program_snapshot(setup$program)),
    "",
    "Objective:",
    if (nzchar(setup$objective)) setup$objective else "(maximize the metric)",
    "",
    "Visible training examples:",
    harness_json(harness_train_context(
      setup$trainset,
      setup$teleprompter@max_context_examples
    )),
    "",
    "Persisted frontier and history:",
    harness_json(harness_state_for_agent(state)),
    "",
    "For propose, return at most",
    setup$teleprompter@max_candidates_per_iteration,
    "candidates. Every edit is a complete replacement value at a valid path.",
    sep = "\n"
  )
}

harness_state_for_agent <- function(state) {
  recent_events <- utils::tail(state$events, 12L)
  list(
    iteration = state$iteration,
    best_id = state$best_id,
    best_score = state$best_score,
    baseline_score = state$baseline_score,
    no_improvement = state$no_improvement,
    frontier = harness_frontier(state),
    recent_events = recent_events
  )
}

harness_json <- function(value) {
  as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    dataframe = "rows",
    digits = NA,
    pretty = TRUE
  ))
}

harness_event <- function(type, ...) {
  c(
    list(
      type = type,
      at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    list(...)
  )
}

harness_checkpoint <- function(context, phase, state, best_program) {
  optimizer_checkpoint_write(
    context,
    phase = phase,
    search_state = state,
    best_program = best_program
  )
}

harness_score_improved <- function(current, previous) {
  !is.na(current) && (is.na(previous) || current > previous)
}

harness_search_done <- function(state, teleprompter, budget) {
  if (!is.null(state$termination)) {
    return(TRUE)
  }
  if (optimizer_budget_stopped(budget)) {
    return(TRUE)
  }
  if (state$iteration >= teleprompter@max_iterations) {
    return(TRUE)
  }
  if (state$no_improvement >= teleprompter@patience) {
    return(TRUE)
  }
  if (
    !is.null(teleprompter@target_score) &&
      !is.na(state$best_score) &&
      state$best_score >= teleprompter@target_score
  ) {
    return(TRUE)
  }
  FALSE
}

harness_termination <- function(state, teleprompter, budget) {
  if (!is.null(state$termination)) {
    return(state$termination)
  }
  if (optimizer_budget_stopped(budget)) {
    return("budget")
  }
  if (
    !is.null(teleprompter@target_score) &&
      !is.na(state$best_score) &&
      state$best_score >= teleprompter@target_score
  ) {
    return("target_score")
  }
  if (state$no_improvement >= teleprompter@patience) {
    return("patience")
  }
  if (state$iteration >= teleprompter@max_iterations) {
    return("max_iterations")
  }
  "completed"
}

harness_candidates_tbl <- function(state) {
  if (length(state$candidates) == 0L) {
    return(tibble::tibble())
  }
  tibble::tibble(
    id = vapply(state$candidates, `[[`, character(1), "id"),
    parent_id = vapply(
      state$candidates,
      function(candidate) {
        candidate$parent_id %||% NA_character_
      },
      character(1)
    ),
    name = vapply(state$candidates, `[[`, character(1), "name"),
    iteration = vapply(state$candidates, `[[`, integer(1), "iteration"),
    score = vapply(state$candidates, `[[`, numeric(1), "score"),
    selected = vapply(
      state$candidates,
      function(candidate) {
        identical(candidate$id, state$best_id)
      },
      logical(1)
    ),
    improved = vapply(state$candidates, `[[`, logical(1), "improved"),
    status = vapply(state$candidates, `[[`, character(1), "status"),
    rationale = vapply(state$candidates, `[[`, character(1), "rationale"),
    fingerprint = vapply(
      state$candidates,
      `[[`,
      character(1),
      "fingerprint"
    ),
    feedback = lapply(state$candidates, `[[`, "feedback"),
    snapshot = lapply(state$candidates, `[[`, "snapshot")
  )
}

harness_finalize <- function(setup, state, best_program, name) {
  termination <- harness_termination(
    state,
    setup$teleprompter,
    setup$budget
  )
  budget_summary <- optimizer_budget_summary(setup$budget)
  candidates <- harness_candidates_tbl(state)
  runner_policy <- if (is.null(setup$runner)) {
    list(
      backend = "disabled",
      sandboxed = FALSE
    )
  } else {
    setup$runner$policy()
  }

  optimized <- copy_module(best_program)
  optimized$state$compiled <- TRUE
  optimized$state$best_score <- state$best_score
  optimized$state$best_params <- list(candidate_id = state$best_id)
  optimized$config$compiled <- TRUE
  optimized$config$teleprompter <- name
  optimized$config$best_score <- state$best_score
  optimized$config$optimizer <- list(
    name = name,
    implementation = "dsprrr-agentic-harness-v1",
    inspiration = list(
      gepa_omni = "https://github.com/gepa-ai/gepa",
      autoresearch = "https://github.com/karpathy/autoresearch",
      meta_harness = "https://arxiv.org/abs/2603.28052"
    ),
    baseline_score = state$baseline_score,
    best_score = state$best_score,
    best_candidate_id = state$best_id,
    frontier_ids = state$frontier_ids,
    candidates = candidates,
    events = state$events,
    iterations = state$iteration,
    agent_steps = state$agent_steps,
    termination = termination,
    budget_summary = budget_summary,
    stop_reason = budget_summary$stop_reason,
    partial = optimizer_budget_stopped(setup$budget),
    sandbox = runner_policy,
    checkpoint_path = setup$control@checkpoint_path,
    resumed = setup$checkpoint$resumed
  )

  harness_checkpoint(
    setup$checkpoint,
    "completed",
    state,
    optimized
  )
  optimized
}

harness_format_score <- function(score) {
  if (is.na(score)) "NA" else format(round(score, 4L), nsmall = 0L)
}

# Print an AutoResearch object through its S7 method.
print_auto_research <- function(x, ...) {
  cli::cli_h3("AutoResearch Teleprompter")
  cli::cli_text("{.field Max experiments}: {x@max_iterations}")
  cli::cli_text("{.field Patience}: {x@patience}")
  cli::cli_text("{.field OS sandbox required}: {x@sandbox}")
  invisible(x)
}

# Print a MetaHarness object through its S7 method.
print_meta_harness <- function(x, ...) {
  cli::cli_h3("Meta-Harness Teleprompter")
  cli::cli_text("{.field Max iterations}: {x@max_iterations}")
  cli::cli_text(
    "{.field Candidates per iteration}: {x@max_candidates_per_iteration}"
  )
  cli::cli_text("{.field Frontier size}: {x@frontier_size}")
  cli::cli_text("{.field OS sandbox required}: {x@sandbox}")
  invisible(x)
}

S7::method(print, AutoResearch) <- print_auto_research
S7::method(print, MetaHarness) <- print_meta_harness
