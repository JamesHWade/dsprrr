make_harness_task_llm <- function() {
  list(
    chat_structured = function(prompt, type, ...) {
      if (grepl("perfect", prompt, fixed = TRUE)) "yes" else "no"
    }
  )
}

make_harness_agent <- function(responses) {
  index <- 0L
  prompts <- character()
  list(
    chat_structured = function(prompt, type, ...) {
      index <<- index + 1L
      prompts <<- c(prompts, prompt)
      responses[[min(index, length(responses))]]
    },
    prompts = function() prompts
  )
}

make_harness_agent_factory <- function(responses) {
  force(responses)
  function() make_harness_agent(responses)
}

make_harness_runner <- function(output = "[1] 2") {
  inputs <- character()
  runner <- list(
    execute = function(code, context = list()) {
      inputs <<- c(inputs, code)
      list(
        success = TRUE,
        result = output,
        stdout = output,
        stderr = "",
        messages = "",
        warnings = "",
        error = NULL,
        duration_ms = 0
      )
    },
    reset = function() invisible(NULL),
    policy = function() {
      list(
        backend = "verified-test-sandbox",
        trust = "untrusted-code",
        sandboxed = TRUE,
        network_access = "disabled"
      )
    }
  )
  list(runner = runner, inputs = function() inputs)
}

harness_program <- function() {
  module(
    signature("question -> answer", instructions = "seed"),
    type = "predict"
  )
}

harness_data <- function() {
  data.frame(question = "return yes", answer = "yes")
}

harness_metric <- function(prediction, expected) {
  as.numeric(identical(as.character(prediction), expected$answer))
}

candidate_proposal <- function(
  instructions,
  name = instructions,
  parent_id = NULL
) {
  candidate <- list(
    name = name,
    rationale = paste("Test", instructions),
    edits = list(list(path = "$", instructions = instructions))
  )
  if (!is.null(parent_id)) {
    candidate$parent_id <- parent_id
  }
  candidate
}

test_that("AutoResearch owns a persistent sandbox and experiment loop", {
  sandbox <- make_harness_runner()
  agent <- make_harness_agent(list(
    list(
      action = "sandbox",
      rationale = "Check a simple hypothesis.",
      code = "1 + 1"
    ),
    list(
      action = "propose",
      rationale = "Evaluate the hypothesis.",
      candidates = list(candidate_proposal("perfect"))
    ),
    list(
      action = "finish",
      rationale = "The candidate is sufficient."
    )
  ))
  tp <- AutoResearch(
    metric = harness_metric,
    max_iterations = 3L,
    patience = 3L,
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = agent,
    runner = sandbox$runner
  )

  expect_identical(compiled$config$teleprompter, "AutoResearch")
  expect_identical(compiled$signature@instructions, "perfect")
  expect_equal(compiled$config$optimizer$baseline_score, 0)
  expect_equal(compiled$config$optimizer$best_score, 1)
  expect_identical(compiled$config$optimizer$termination, "agent_finished")
  expect_identical(compiled$config$optimizer$agent_steps, 3L)
  expect_identical(compiled$config$optimizer$sandbox$sandboxed, TRUE)
  expect_length(sandbox$inputs(), 1L)

  candidates <- compiled$config$optimizer$candidates
  expect_equal(nrow(candidates), 2L)
  expect_identical(candidates$selected, c(FALSE, TRUE))
  expect_match(agent$prompts()[[2L]], "sandbox")
})

test_that("MetaHarness evaluates a batch and controls the frontier", {
  sandbox <- make_harness_runner()
  agent <- make_harness_agent_factory(list(
    list(
      action = "propose",
      rationale = "Compare a weak and strong edit.",
      candidates = list(
        candidate_proposal("still weak", name = "weak"),
        candidate_proposal("perfect", name = "strong")
      )
    )
  ))
  tp <- MetaHarness(
    metric = harness_metric,
    max_iterations = 1L,
    max_candidates_per_iteration = 2L,
    frontier_size = 2L,
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = agent,
    runner = sandbox$runner
  )

  expect_identical(compiled$config$teleprompter, "MetaHarness")
  expect_identical(compiled$signature@instructions, "perfect")
  expect_identical(compiled$config$optimizer$termination, "max_iterations")
  expect_equal(nrow(compiled$config$optimizer$candidates), 3L)
  expect_length(compiled$config$optimizer$frontier_ids, 2L)
  expect_identical(compiled$config$optimizer$iterations, 1L)
})

test_that("MetaHarness can optimize multiple pipeline components jointly", {
  first <- module(
    signature("question -> middle", instructions = "first-seed"),
    type = "predict"
  )
  second <- module(
    signature("middle -> answer", instructions = "second-seed"),
    type = "predict"
  )
  program <- pipeline(first, second)
  task_llm <- list(
    chat_structured = function(prompt, type, ...) {
      if (grepl("first-perfect", prompt, fixed = TRUE)) {
        return("useful-middle")
      }
      if (
        grepl("second-perfect", prompt, fixed = TRUE) &&
          grepl("useful-middle", prompt, fixed = TRUE)
      ) {
        return("yes")
      }
      "no"
    }
  )
  agent <- make_harness_agent_factory(list(list(
    action = "propose",
    rationale = "Coordinate both stages.",
    candidates = list(list(
      name = "joint",
      rationale = "The stages need a shared intermediate contract.",
      edits = list(
        list(path = "$/steps/1", instructions = "first-perfect"),
        list(path = "$/steps/2", instructions = "second-perfect")
      )
    ))
  )))
  tp <- MetaHarness(
    metric = harness_metric,
    max_iterations = 1L,
    max_candidates_per_iteration = 1L,
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    program,
    data.frame(question = "start", answer = "yes"),
    .llm = task_llm,
    .agent_llm = agent,
    runner = make_harness_runner()$runner,
    .cache = FALSE
  )
  components <- named_parameters(compiled, boundaries = "cross")

  expect_identical(
    components[["$/steps/1"]]$signature@instructions,
    "first-perfect"
  )
  expect_identical(
    components[["$/steps/2"]]$signature@instructions,
    "second-perfect"
  )
  expect_equal(compiled$config$optimizer$best_score, 1)
})

test_that("agentic harnesses materialize both RLM predictor leaves", {
  skip_if_not_installed("callr")
  local_reset_cache()

  proposal <- list(
    name = "tune both RLM predictors",
    rationale = "Action selection and typed extraction must improve together.",
    edits = list(
      list(
        path = "$/generate_action",
        instructions = "ACTION-HARNESS"
      ),
      list(
        path = "$/extract",
        instructions = "EXTRACT-HARNESS"
      )
    )
  )
  response <- list(
    action = "propose",
    rationale = "Tune both graph-visible predictors.",
    candidates = list(proposal)
  )
  cases <- list(
    AutoResearch = list(
      teleprompter = AutoResearch(
        metric = rlm_optimizer_accuracy,
        max_iterations = 1L,
        verbose = FALSE
      ),
      agent = make_harness_agent(list(response))
    ),
    MetaHarness = list(
      teleprompter = MetaHarness(
        metric = rlm_optimizer_accuracy,
        max_iterations = 1L,
        max_candidates_per_iteration = 1L,
        verbose = FALSE
      ),
      agent = make_harness_agent_factory(list(response))
    )
  )

  for (name in names(cases)) {
    runner <- r_code_runner(timeout = 10, persistent = TRUE)
    withr::defer(runner$close())
    expect_identical(runner$policy()$persistent, TRUE)
    program <- make_rlm_optimizer_program(runner)
    chat <- make_rlm_optimizer_chat()
    case <- cases[[name]]

    run <- capture_rlm_optimizer_warnings(
      compile(
        case$teleprompter,
        program,
        data.frame(question = "inspect", answer = "yes"),
        .llm = chat,
        .agent_llm = case$agent,
        runner = make_harness_runner()$runner,
        control = optimizer_control(
          checkpoint_registry = list(rlm_runner = runner)
        ),
        .cache = FALSE
      )
    )
    compiled <- expect_only_rlm_fallback_warnings(run)

    expect_identical(compiled$config$teleprompter, name)
    expect_identical(
      compiled$generate_action$signature@instructions,
      "ACTION-HARNESS"
    )
    expect_identical(
      compiled$extract$signature@instructions,
      "EXTRACT-HARNESS"
    )
    expect_identical(compiled$config$optimizer$baseline_score, 0)
    expect_identical(compiled$config$optimizer$best_score, 1)
    expect_setequal(
      names(named_parameters(compiled, boundaries = "cross")),
      c("$/generate_action", "$/extract")
    )
  }
})

test_that("MetaHarness deduplicates candidates by canonical snapshot", {
  sandbox <- make_harness_runner()
  proposal <- candidate_proposal("perfect")
  agent <- make_harness_agent_factory(list(
    list(
      action = "propose",
      rationale = "Duplicate batch.",
      candidates = list(proposal, proposal)
    )
  ))
  tp <- MetaHarness(
    metric = harness_metric,
    max_iterations = 1L,
    max_candidates_per_iteration = 2L,
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = agent,
    runner = sandbox$runner
  )

  candidates <- compiled$config$optimizer$candidates
  events <- compiled$config$optimizer$events
  event_types <- vapply(events, `[[`, character(1), "type")
  expect_equal(nrow(candidates), 2L)
  expect_in("candidate_duplicate", event_types)
})

test_that("MetaHarness clones custom proposers for every iteration", {
  calls_per_session <- integer()
  TestProposer <- R6::R6Class(
    "FreshMetaHarnessTestProposer",
    public = list(
      calls = 0L,
      chat_structured = function(prompt, type, ...) {
        self$calls <- as.integer(self$calls + 1L)
        calls_per_session <<- c(calls_per_session, self$calls)
        iteration <- regmatches(
          prompt,
          regexpr('"iteration": [0-9]+', prompt)
        )
        instructions <- if (identical(iteration, '"iteration": 1')) {
          "still weak"
        } else {
          "perfect"
        }
        list(
          action = "propose",
          rationale = "Exercise a fresh custom proposer.",
          candidates = list(candidate_proposal(instructions))
        )
      }
    )
  )
  proposer <- TestProposer$new()
  tp <- MetaHarness(
    metric = harness_metric,
    max_iterations = 2L,
    max_candidates_per_iteration = 1L,
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = proposer,
    runner = make_harness_runner()$runner
  )

  expect_identical(calls_per_session, c(1L, 1L))
  expect_identical(proposer$calls, 0L)
  expect_identical(compiled$signature@instructions, "perfect")
})

test_that("MetaHarness rejects custom proposers that cannot be refreshed", {
  tp <- MetaHarness(
    metric = harness_metric,
    max_iterations = 1L,
    verbose = FALSE
  )

  expect_error(
    compile(
      tp,
      harness_program(),
      harness_data(),
      .llm = make_harness_task_llm(),
      .agent_llm = make_harness_agent(list(list(
        action = "finish",
        rationale = "unused"
      ))),
      runner = make_harness_runner()$runner
    ),
    "requires a fresh proposer session"
  )
})

test_that("agentic harnesses require an OS-sandboxed runner by default", {
  tp <- AutoResearch(
    metric = harness_metric,
    max_iterations = 1L,
    verbose = FALSE
  )
  agent <- make_harness_agent(list(list(
    action = "finish",
    rationale = "done"
  )))

  expect_snapshot(
    error = TRUE,
    compile(
      tp,
      harness_program(),
      harness_data(),
      .llm = make_harness_task_llm(),
      .agent_llm = agent
    )
  )

  expect_snapshot(
    error = TRUE,
    compile(
      tp,
      harness_program(),
      harness_data(),
      .llm = make_harness_task_llm(),
      .agent_llm = agent,
      runner = r_code_runner()
    )
  )
})

test_that("agentic harness seeds are bounded and do not leak RNG state", {
  expect_error(
    AutoResearch(metric = harness_metric, seed = 1e20),
    "R integer range"
  )

  set.seed(20260804)
  rng_before <- .Random.seed
  tp <- AutoResearch(
    metric = harness_metric,
    max_iterations = 1L,
    sandbox = FALSE,
    seed = 42L,
    verbose = FALSE
  )
  agent <- make_harness_agent(list(list(
    action = "finish",
    rationale = "done"
  )))

  compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = agent
  )

  expect_identical(.Random.seed, rng_before)
})

test_that("sandbox false disables agent code execution", {
  sandbox <- make_harness_runner()
  agent <- make_harness_agent(list(
    list(
      action = "sandbox",
      rationale = "This must not run.",
      code = "stop('unsafe')"
    ),
    list(
      action = "finish",
      rationale = "done"
    )
  ))
  tp <- AutoResearch(
    metric = harness_metric,
    max_iterations = 1L,
    sandbox = FALSE,
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = agent,
    runner = sandbox$runner
  )

  event_types <- vapply(
    compiled$config$optimizer$events,
    `[[`,
    character(1),
    "type"
  )
  expect_in("sandbox_rejected", event_types)
  expect_length(sandbox$inputs(), 0L)
  expect_identical(compiled$config$optimizer$sandbox$backend, "disabled")
})

test_that("AutoResearch limits count only accepted evaluations", {
  sandbox <- make_harness_runner()
  agent <- make_harness_agent(list(
    list(
      action = "propose",
      rationale = "Invalid path.",
      candidates = list(list(
        name = "invalid",
        rationale = "Use a path that does not exist.",
        edits = list(list(path = "$missing", instructions = "perfect"))
      ))
    ),
    list(
      action = "propose",
      rationale = "Duplicate the baseline.",
      candidates = list(candidate_proposal("seed"))
    ),
    list(
      action = "propose",
      rationale = "Evaluate a valid candidate.",
      candidates = list(candidate_proposal("perfect"))
    )
  ))
  tp <- AutoResearch(
    metric = harness_metric,
    max_iterations = 1L,
    patience = 1L,
    max_agent_steps = 3L,
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = agent,
    runner = sandbox$runner
  )

  events <- compiled$config$optimizer$events
  event_types <- vapply(events, `[[`, character(1), "type")
  expect_in("candidate_rejected", event_types)
  expect_in("candidate_duplicate", event_types)
  expect_equal(nrow(compiled$config$optimizer$candidates), 2L)
  expect_identical(compiled$config$optimizer$iterations, 1L)
  expect_identical(compiled$config$optimizer$termination, "max_iterations")
  expect_identical(compiled$signature@instructions, "perfect")
})

test_that("non-improving candidates never displace the baseline", {
  agent <- make_harness_agent(list(
    list(
      action = "propose",
      rationale = "Try an unevaluable candidate.",
      candidates = list(candidate_proposal("perfect"))
    ),
    list(action = "finish", rationale = "done")
  ))
  tp <- AutoResearch(
    metric = function(prediction, expected) NA_real_,
    max_iterations = 2L,
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = agent,
    runner = make_harness_runner()$runner
  )

  candidates <- compiled$config$optimizer$candidates
  expect_identical(compiled$signature@instructions, "seed")
  expect_identical(candidates$selected, c(TRUE, FALSE))
  expect_identical(candidates$improved, c(TRUE, FALSE))
  expect_equal(compiled$config$optimizer$best_score, 0)
})

test_that("malformed actions and runner failures stay inside the harness", {
  failing_runner <- list(
    execute = function(code, context) stop("sandbox unavailable"),
    policy = function() {
      list(
        backend = "test-sandbox",
        trust = "untrusted-code",
        sandboxed = TRUE
      )
    }
  )
  agent <- make_harness_agent(list(
    list(action = character(), rationale = character()),
    list(
      action = "sandbox",
      rationale = "Exercise failure handling.",
      code = "1 + 1"
    ),
    list(action = "finish", rationale = "done")
  ))
  tp <- AutoResearch(
    metric = harness_metric,
    max_iterations = 1L,
    max_agent_steps = 3L,
    verbose = FALSE
  )

  compiled <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = agent,
    runner = failing_runner
  )

  events <- compiled$config$optimizer$events
  event_types <- vapply(events, `[[`, character(1), "type")
  sandbox_event <- events[[which(event_types == "sandbox")[[1L]]]]
  expect_in("invalid_action", event_types)
  expect_false(sandbox_event$success)
  expect_match(sandbox_event$output, "sandbox unavailable")
  expect_identical(compiled$config$optimizer$termination, "agent_finished")
})

test_that("MetaHarness checkpoints and resumes without repeating baseline", {
  checkpoint <- withr::local_tempfile(fileext = ".rds")
  sandbox <- make_harness_runner()
  tp <- MetaHarness(
    metric = harness_metric,
    max_iterations = 1L,
    max_candidates_per_iteration = 1L,
    verbose = FALSE
  )

  first <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = make_harness_agent_factory(list(list(
      action = "finish",
      rationale = "unused"
    ))),
    runner = sandbox$runner,
    control = optimizer_control(
      max_trials = 1L,
      checkpoint_path = checkpoint
    )
  )
  expect_equal(nrow(first$config$optimizer$candidates), 1L)

  resumed <- compile(
    tp,
    harness_program(),
    harness_data(),
    .llm = make_harness_task_llm(),
    .agent_llm = make_harness_agent_factory(list(list(
      action = "propose",
      rationale = "Resume with one edit.",
      candidates = list(candidate_proposal("perfect"))
    ))),
    runner = sandbox$runner,
    control = optimizer_control(
      max_trials = 3L,
      checkpoint_path = checkpoint,
      resume = TRUE
    )
  )

  expect_identical(resumed$config$optimizer$resumed, TRUE)
  expect_equal(nrow(resumed$config$optimizer$candidates), 2L)
  expect_identical(resumed$signature@instructions, "perfect")
})
