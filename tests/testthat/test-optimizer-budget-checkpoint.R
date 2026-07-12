checkpoint_test_provider <- function(provider, model) {
  chat <- suppressWarnings(switch(
    provider,
    openai = ellmer::chat_openai(
      model = model,
      api_key = "checkpoint-test-key"
    ),
    anthropic = ellmer::chat_anthropic(
      model = model,
      api_key = "checkpoint-test-key"
    ),
    stop("Unknown checkpoint test provider")
  ))
  chat$get_provider()
}

checkpoint_test_chat <- function(
  model,
  provider = "openai",
  counter = NULL,
  answer = "ok"
) {
  provider_object <- checkpoint_test_provider(provider, model)
  local({
    self <- structure(
      list(
        chat_structured = function(prompt, type, ...) {
          if (!is.null(counter)) {
            counter$calls <- counter$calls + 1L
          }
          list(answer = answer)
        },
        clone = function(...) self,
        set_turns = function(turns) invisible(NULL),
        get_provider = function() provider_object,
        get_model = function() model
      ),
      class = "Chat"
    )
    self
  })
}

test_that("optimizer controls validate resource and checkpoint limits", {
  control <- optimizer_control(
    max_metric_calls = 4L,
    max_provider_calls = 3L,
    max_input_tokens = 100L,
    max_output_tokens = 50L,
    max_total_tokens = 150L,
    max_cost = 0.25,
    max_elapsed_seconds = 10,
    checkpoint_path = "optimizer.rds"
  )

  expect_equal(control@max_metric_calls, 4L)
  expect_equal(control@max_provider_calls, 3L)
  expect_equal(control@max_total_tokens, 150L)
  expect_equal(control@max_cost, 0.25)
  expect_equal(control@max_elapsed_seconds, 10)
  expect_identical(control@checkpoint_path, "optimizer.rds")

  expect_error(
    OptimizerControl(max_provider_calls = -1L),
    "non-negative"
  )
  expect_error(
    OptimizerControl(max_total_tokens = 1.5),
    "integer"
  )
  expect_error(
    optimizer_control(resume = TRUE),
    class = "dsprrr_optimizer_checkpoint_config_error"
  )
})

test_that("optimizer error budgets do not become batch cancellation budgets", {
  observed <- NULL
  testthat::local_mocked_bindings(
    evaluate = function(..., .concurrency) {
      observed <<- .concurrency
      list(
        scores = c(NA_real_, NA_real_, 1),
        predictions = list(NA, NA, "ok"),
        errors = c("first", "second", NA_character_),
        mean_score = 1,
        n_evaluated = 1L,
        n_errors = 2L,
        epoch_scores = NULL,
        score_std = NA_real_,
        ci_95 = c(NA_real_, NA_real_)
      )
    },
    .package = "dsprrr"
  )
  result <- eval_program(
    module(signature("question -> answer"), type = "predict"),
    data.frame(question = c("a", "b", "c"), answer = "ok"),
    metric = function(...) 1,
    control = optimizer_control(max_errors = 1L, progress = FALSE)
  )
  expect_identical(observed$max_errors, Inf)
  expect_equal(result@n_errors, 2L)
  expect_equal(nrow(result@examples), 3L)

  budget <- dsprrr:::new_optimizer_budget(
    optimizer_control(max_errors = 1L)
  )
  dsprrr:::record_eval_result_outcomes(budget, result, "candidate")
  summary <- dsprrr:::optimizer_budget_summary(budget)
  expect_equal(summary$attempts, 3L)
  expect_equal(summary$total_errors, 2L)
  expect_true(summary$stopped)
})

test_that("ledger-only optimizers stop at a metric cap and return partial best", {
  calls <- 0L
  testthat::local_mocked_bindings(
    eval_program = function(...) {
      calls <<- calls + 1L
      EvalResult(
        examples = data.frame(
          row_id = 1L,
          score = 1,
          error = NA_character_,
          predicted = "ok",
          feedback = NA_character_
        ),
        mean_score = 1,
        n_evaluated = 1L,
        n_errors = 0L,
        input_tokens = 0L,
        output_tokens = 0L,
        total_tokens = 0L,
        total_cost = 0,
        provider_calls = 0L,
        metric_calls = 1L
      )
    },
    gepa_mutate_instruction = function(instruction, ...) instruction,
    .package = "dsprrr"
  )
  program <- module(signature("question -> answer"), type = "predict")
  data <- data.frame(question = c("a", "b"), answer = "ok")
  control <- function() {
    optimizer_control(
      max_metric_calls = 1L,
      progress = FALSE
    )
  }

  gepa <- dsprrr:::compile_gepa(
    GEPA(
      metric = function(...) 1,
      population_size = 2L,
      generations = 1L,
      verbose = FALSE
    ),
    program,
    data,
    control = control()
  )
  expect_equal(calls, 1L)
  expect_true(gepa$config$optimizer$partial)
  expect_equal(gepa$config$optimizer$budget_summary$metric_calls, 1L)

  calls <- 0L
  simba <- dsprrr:::compile_simba(
    SIMBA(metric = function(...) 1, max_steps = 1L),
    program,
    data,
    control = control()
  )
  expect_equal(calls, 1L)
  expect_true(simba$config$optimizer$partial)
  expect_equal(simba$config$optimizer$budget_summary$metric_calls, 1L)

  calls <- 0L
  copro <- dsprrr:::compile_copro(
    COPRO(metric = function(...) 1, breadth = 1L, depth = 1L),
    program,
    data,
    control = control()
  )
  expect_equal(calls, 1L)
  expect_true(copro$config$optimizer$partial)
  expect_equal(copro$config$optimizer$budget_summary$metric_calls, 1L)
})

test_that("optimizer ledger records exact usage and bounded overshoot", {
  budget <- new_optimizer_budget(optimizer_control(max_total_tokens = 10L))

  expect_true(optimizer_budget_preflight(
    budget,
    "candidate",
    planned = list(total_tokens = 1L),
    unit_id = "candidate:1:row:1",
    work_unit = "evaluation_row"
  ))
  record_optimizer_usage(
    budget,
    list(
      trials = 1L,
      metric_calls = 1L,
      provider_calls = 1L,
      input_tokens = 7L,
      output_tokens = 5L,
      total_tokens = 12L,
      known_cost = 0.02
    ),
    "candidate",
    unit_id = "candidate:1:row:1",
    work_unit = "evaluation_row",
    max_started = 1L
  )

  summary <- optimizer_budget_summary(budget)
  expect_true(summary$stopped)
  expect_identical(summary$stop_reason$code, "max_total_tokens")
  expect_identical(summary$stop_reason$overshoot$amount, 2)
  expect_identical(summary$stop_reason$overshoot$unit, "evaluation_row")
  expect_identical(summary$stop_reason$overshoot$max_started, 1L)
  expect_equal(summary$known_total_tokens, 12L)
  expect_equal(summary$known_cost, 0.02)
  expect_length(summary$overshoots, 1L)
})

test_that("unknown cost is never treated as free", {
  capped <- new_optimizer_budget(optimizer_control(max_cost = 1))
  record_optimizer_usage(
    capped,
    list(provider_calls = 1L, known_cost = NA_real_),
    "provider",
    work_unit = "provider_call"
  )
  capped_summary <- optimizer_budget_summary(capped)

  expect_true(capped_summary$stopped)
  expect_identical(capped_summary$stop_reason$code, "unknown_cost")
  expect_true(is.na(capped_summary$total_cost))
  expect_equal(capped_summary$known_cost, 0)
  expect_equal(capped_summary$unknown_usage$cost, 1L)

  uncapped <- new_optimizer_budget(optimizer_control())
  record_optimizer_usage(
    uncapped,
    list(known_cost = 0.25),
    "known"
  )
  record_optimizer_usage(
    uncapped,
    list(known_cost = NA_real_),
    "unknown"
  )
  uncapped_summary <- optimizer_budget_summary(uncapped)
  expect_false(uncapped_summary$stopped)
  expect_equal(uncapped_summary$known_cost, 0.25)
  expect_true(is.na(uncapped_summary$total_cost))
})

test_that("unknown provider and token usage stop only corresponding caps", {
  provider_budget <- new_optimizer_budget(
    optimizer_control(max_provider_calls = 5L)
  )
  record_optimizer_usage(
    provider_budget,
    list(provider_calls = NA_integer_),
    "opaque_provider",
    work_unit = "evaluation_row"
  )
  expect_identical(
    optimizer_budget_summary(provider_budget)$stop_reason$code,
    "unknown_provider"
  )

  token_budget <- new_optimizer_budget(
    optimizer_control(max_input_tokens = 20L)
  )
  record_optimizer_usage(
    token_budget,
    list(input_tokens = NA_integer_),
    "opaque_tokens",
    work_unit = "evaluation_row"
  )
  expect_identical(
    optimizer_budget_summary(token_budget)$stop_reason$code,
    "unknown_input_tokens"
  )
})

test_that("optimizer elapsed time is monotonic and resumes without downtime", {
  time <- new.env(parent = emptyenv())
  time$value <- 10
  clock <- function() time$value
  budget <- new_optimizer_budget(optimizer_control(), clock = clock)

  time$value <- 12
  expect_equal(optimizer_budget_summary(budget)$elapsed_seconds, 2)
  time$value <- 11
  expect_equal(optimizer_budget_summary(budget)$elapsed_seconds, 2)

  state <- optimizer_budget_state(budget)
  time$value <- 100
  resumed <- new_optimizer_budget(
    optimizer_control(),
    state = state,
    clock = clock
  )
  time$value <- 101
  expect_equal(optimizer_budget_summary(resumed)$elapsed_seconds, 3)
})

test_that("raising a resource cap clears only its resumable stop", {
  first <- new_optimizer_budget(optimizer_control(max_trials = 1L))
  record_optimizer_usage(first, list(trials = 1L), "trial")
  expect_true(optimizer_budget_stopped(first))

  resumed <- new_optimizer_budget(
    optimizer_control(max_trials = 2L),
    state = optimizer_budget_state(first)
  )
  expect_false(optimizer_budget_stopped(resumed))
  expect_equal(optimizer_budget_summary(resumed)$trials, 1L)

  unchanged <- new_optimizer_budget(
    optimizer_control(max_trials = 1L),
    state = optimizer_budget_state(first)
  )
  expect_true(optimizer_budget_stopped(unchanged))
})

test_that("restored budgets fail closed under every stricter current limit", {
  base <- new_optimizer_budget(optimizer_control(max_errors = 10L))
  record_optimizer_usage(
    base,
    list(
      trials = 3L,
      metric_calls = 4L,
      provider_calls = 5L,
      input_tokens = 6L,
      output_tokens = 7L,
      total_tokens = 13L,
      known_cost = 1.5
    ),
    "history"
  )
  state <- optimizer_budget_state(base)
  cases <- list(
    max_trials = list(value = 3L, code = "max_trials"),
    max_metric_calls = list(value = 4L, code = "max_metric_calls"),
    max_provider_calls = list(value = 5L, code = "max_provider_calls"),
    max_input_tokens = list(value = 6L, code = "max_input_tokens"),
    max_output_tokens = list(value = 7L, code = "max_output_tokens"),
    max_total_tokens = list(value = 13L, code = "max_total_tokens"),
    max_cost = list(value = 1.5, code = "max_cost"),
    max_elapsed_seconds = list(value = 0, code = "max_elapsed_seconds")
  )
  for (name in names(cases)) {
    args <- stats::setNames(list(cases[[name]]$value), name)
    resumed <- new_optimizer_budget(
      do.call(optimizer_control, args),
      state = state
    )
    expect_true(optimizer_budget_stopped(resumed), info = name)
    expect_identical(
      optimizer_budget_summary(resumed)$stop_reason$code,
      cases[[name]]$code,
      info = name
    )
    expect_false(
      optimizer_budget_preflight(resumed, "new_work"),
      info = name
    )
  }

  errors <- new_optimizer_budget(optimizer_control(max_errors = 10L))
  record_optimizer_outcome(errors, FALSE, "history", simpleError("one"))
  record_optimizer_outcome(errors, FALSE, "history", simpleError("two"))
  stricter_errors <- new_optimizer_budget(
    optimizer_control(max_errors = 1L),
    state = optimizer_budget_state(errors)
  )
  expect_true(optimizer_budget_stopped(stricter_errors))
  expect_identical(
    optimizer_budget_summary(stricter_errors)$stop_reason$code,
    "max_errors"
  )
  expect_false(optimizer_budget_preflight(stricter_errors, "new_work"))
})

test_that("new finite caps reject restored unknown historical usage", {
  cases <- list(
    max_metric_calls = list(field = "metric_calls", code = "unknown_metric"),
    max_provider_calls = list(
      field = "provider_calls",
      code = "unknown_provider"
    ),
    max_input_tokens = list(
      field = "input_tokens",
      code = "unknown_input_tokens"
    ),
    max_output_tokens = list(
      field = "output_tokens",
      code = "unknown_output_tokens"
    ),
    max_total_tokens = list(
      field = "total_tokens",
      code = "unknown_total_tokens"
    ),
    max_cost = list(field = "known_cost", code = "unknown_cost")
  )
  for (name in names(cases)) {
    base <- new_optimizer_budget(optimizer_control())
    usage <- stats::setNames(list(NA_real_), cases[[name]]$field)
    record_optimizer_usage(base, usage, "history")
    control <- do.call(
      optimizer_control,
      stats::setNames(list(if (name == "max_cost") 1 else 10L), name)
    )
    resumed <- new_optimizer_budget(
      control,
      state = optimizer_budget_state(base)
    )
    expect_true(optimizer_budget_stopped(resumed), info = name)
    expect_identical(
      optimizer_budget_summary(resumed)$stop_reason$code,
      cases[[name]]$code,
      info = name
    )
  }
})

test_that("restored budget state requires exact canonical schemas", {
  state <- optimizer_budget_state(new_optimizer_budget(optimizer_control()))
  outcomes <- new_optimizer_budget(optimizer_control(max_errors = 3L))
  record_optimizer_outcome(outcomes, TRUE, "history")
  record_optimizer_outcome(outcomes, FALSE, "history")
  outcome_state <- optimizer_budget_state(outcomes)

  missing_outcome <- outcome_state
  missing_outcome$attempts <- 3L
  overlapping_outcome <- outcome_state
  overlapping_outcome$attempts <- 1L
  overflowing_partition <- state
  overflowing_partition$attempts <- .Machine$integer.max
  overflowing_partition$successes <- .Machine$integer.max
  overflowing_partition$total_errors <- .Machine$integer.max
  impossible_streak <- outcome_state
  impossible_streak$consecutive_errors <- 2L

  boundary_partition <- state
  boundary_partition$attempts <- .Machine$integer.max
  boundary_partition$successes <- .Machine$integer.max - 1L
  boundary_partition$total_errors <- 1L
  expect_no_warning(expect_no_error(new_optimizer_budget(
    optimizer_control(),
    state = boundary_partition
  )))

  malformed <- list(
    extra_field = c(state, list(unexpected = 1L)),
    fractional_count = within(state, attempts <- 0.5),
    numeric_count = within(state, trials <- 0),
    missing_outcome = missing_outcome,
    overlapping_outcome = overlapping_outcome,
    overflowing_partition = overflowing_partition,
    impossible_streak = impossible_streak,
    malformed_overshoot = within(
      state,
      overshoots <- list(list(resource = "trials", amount = 1))
    )
  )

  stopped <- new_optimizer_budget(optimizer_control(max_trials = 1L))
  record_optimizer_usage(stopped, list(trials = 1L), "trial")
  bad_reason <- optimizer_budget_state(stopped)
  bad_reason$stop_reason$unexpected <- TRUE
  malformed$open_stop_reason <- bad_reason

  for (name in names(malformed)) {
    expect_error(
      new_optimizer_budget(
        optimizer_control(),
        state = malformed[[name]]
      ),
      class = "dsprrr_optimizer_checkpoint_malformed",
      info = name
    )
  }
})

test_that("restored max-error reasons preserve outcome partitions", {
  stopped <- new_optimizer_budget(optimizer_control(max_errors = 1L))
  record_optimizer_outcome(stopped, FALSE, "history")
  state <- optimizer_budget_state(stopped)

  observed_exceeds_errors <- state
  observed_exceeds_errors$stop_reason$observed <- 2L
  unreached_limit <- state
  unreached_limit$stop_reason$limit <- 2L
  reason_ahead_of_ledger <- state
  reason_ahead_of_ledger$stop_reason$attempts <- 2L

  record_optimizer_outcome(stopped, FALSE, "history")
  record_optimizer_outcome(stopped, FALSE, "history")
  impossible_delta <- optimizer_budget_state(stopped)
  impossible_delta$stop_reason$attempts <- 2L

  malformed <- list(
    observed_exceeds_errors = observed_exceeds_errors,
    unreached_limit = unreached_limit,
    reason_ahead_of_ledger = reason_ahead_of_ledger,
    impossible_delta = impossible_delta
  )
  for (name in names(malformed)) {
    expect_error(
      new_optimizer_budget(
        optimizer_control(),
        state = malformed[[name]]
      ),
      class = "dsprrr_optimizer_checkpoint_malformed",
      info = name
    )
  }
})

test_that("budget outcome counters reject overflow atomically", {
  clock <- function() 0
  state <- optimizer_budget_state(new_optimizer_budget(
    optimizer_control(),
    clock = clock
  ))

  success_state <- state
  success_state$attempts <- .Machine$integer.max - 1L
  success_state$successes <- .Machine$integer.max - 1L
  successes <- new_optimizer_budget(
    optimizer_control(),
    state = success_state,
    clock = clock
  )
  expect_no_warning(record_optimizer_outcome(
    successes,
    TRUE,
    "boundary"
  ))
  expect_identical(successes$attempts, .Machine$integer.max)
  expect_identical(successes$successes, .Machine$integer.max)

  before <- optimizer_budget_state(successes)
  condition <- expect_no_warning(expect_error(
    record_optimizer_outcome(successes, TRUE, "overflow"),
    class = "dsprrr_optimizer_budget_overflow"
  ))
  expect_identical(condition$resource, "attempts")
  expect_identical(condition$stage, "overflow")
  expect_identical(optimizer_budget_state(successes), before)

  error_state <- state
  error_state$attempts <- .Machine$integer.max - 1L
  error_state$total_errors <- .Machine$integer.max - 1L
  errors <- new_optimizer_budget(
    optimizer_control(),
    state = error_state,
    clock = clock
  )
  expect_no_warning(record_optimizer_outcome(errors, FALSE, "boundary"))
  expect_identical(errors$attempts, .Machine$integer.max)
  expect_identical(errors$total_errors, .Machine$integer.max)

  batch <- new_optimizer_budget(
    optimizer_control(),
    state = success_state,
    clock = clock
  )
  batch_state <- optimizer_budget_state(batch)
  expect_no_warning(expect_error(
    record_eval_result_outcomes(
      batch,
      EvalResult(n_evaluated = 2L),
      "batch"
    ),
    class = "dsprrr_optimizer_budget_overflow"
  ))
  expect_identical(optimizer_budget_state(batch), batch_state)
})

test_that("resource and unknown-use counters reject overflow atomically", {
  clock <- function() 0
  state <- optimizer_budget_state(new_optimizer_budget(
    optimizer_control(),
    clock = clock
  ))
  resources <- setdiff(optimizer_budget_counter_names(), "known_cost")

  for (resource in resources) {
    boundary <- state
    boundary[[resource]] <- .Machine$integer.max - 1L
    budget <- new_optimizer_budget(
      optimizer_control(),
      state = boundary,
      clock = clock
    )
    usage <- stats::setNames(list(1L), resource)
    expect_no_warning(record_optimizer_usage(budget, usage, "boundary"))
    expect_identical(budget[[resource]], .Machine$integer.max, info = resource)

    before <- optimizer_budget_state(budget)
    expect_no_warning(expect_error(
      record_optimizer_usage(budget, usage, "overflow"),
      class = "dsprrr_optimizer_budget_overflow",
      info = resource
    ))
    expect_identical(optimizer_budget_state(budget), before, info = resource)
  }

  unknown_fields <- c(
    metric_calls = "unknown_metric_calls",
    provider_calls = "unknown_provider_calls",
    input_tokens = "unknown_input_tokens",
    output_tokens = "unknown_output_tokens",
    total_tokens = "unknown_total_tokens",
    known_cost = "unknown_cost_calls"
  )
  for (resource in names(unknown_fields)) {
    counter <- unname(unknown_fields[[resource]])
    boundary <- state
    boundary[[counter]] <- .Machine$integer.max
    budget <- new_optimizer_budget(
      optimizer_control(),
      state = boundary,
      clock = clock
    )
    usage <- stats::setNames(list(NA_real_), resource)
    before <- optimizer_budget_state(budget)
    expect_no_warning(expect_error(
      record_optimizer_usage(budget, usage, "unknown_overflow"),
      class = "dsprrr_optimizer_budget_overflow",
      info = resource
    ))
    expect_identical(optimizer_budget_state(budget), before, info = resource)
  }

  atomic_state <- state
  atomic_state$metric_calls <- .Machine$integer.max
  atomic <- new_optimizer_budget(
    optimizer_control(),
    state = atomic_state,
    clock = clock
  )
  before <- optimizer_budget_state(atomic)
  expect_no_warning(expect_error(
    record_optimizer_usage(
      atomic,
      list(trials = 1L, metric_calls = 1L),
      "atomic"
    ),
    class = "dsprrr_optimizer_budget_overflow"
  ))
  expect_identical(optimizer_budget_state(atomic), before)

  cost_state <- state
  cost_state$known_cost <- .Machine$double.xmax
  cost <- new_optimizer_budget(
    optimizer_control(),
    state = cost_state,
    clock = clock
  )
  before <- optimizer_budget_state(cost)
  expect_no_warning(expect_error(
    record_optimizer_usage(cost, list(known_cost = 1), "cost"),
    class = "dsprrr_optimizer_budget_overflow"
  ))
  expect_identical(optimizer_budget_state(cost), before)
})

test_that("trial accounting and preflight reject capacity overflow", {
  clock <- function() 0
  state <- optimizer_budget_state(new_optimizer_budget(
    optimizer_control(),
    clock = clock
  ))
  state$trials <- .Machine$integer.max - 1L
  budget <- new_optimizer_budget(
    optimizer_control(),
    state = state,
    clock = clock
  )
  expect_no_warning(optimizer_budget_count_trial(
    budget,
    "boundary",
    "trial:max"
  ))
  expect_identical(budget$trials, .Machine$integer.max)
  expect_identical(budget$trial_units, "trial:max")

  before <- optimizer_budget_state(budget)
  expect_no_warning(expect_error(
    optimizer_budget_count_trial(budget, "overflow", "trial:overflow"),
    class = "dsprrr_optimizer_budget_overflow"
  ))
  expect_identical(optimizer_budget_state(budget), before)

  expect_no_warning(expect_error(
    optimizer_budget_preflight(
      budget,
      "preflight",
      planned = list(trials = 1L)
    ),
    class = "dsprrr_optimizer_budget_overflow"
  ))
  expect_identical(optimizer_budget_state(budget), before)

  full <- state
  full$trials <- 0L
  full$attempts <- .Machine$integer.max
  full$successes <- .Machine$integer.max
  outcome_budget <- new_optimizer_budget(
    optimizer_control(),
    state = full,
    clock = clock
  )
  calls <- 0L
  expect_no_warning(expect_error(
    optimizer_budgeted_provider_call(
      outcome_budget,
      model = function(...) NULL,
      stage = "paid_work",
      unit_id = "provider:overflow",
      call = function() {
        calls <<- calls + 1L
      }
    ),
    class = "dsprrr_optimizer_budget_overflow"
  ))
  expect_identical(calls, 0L)
})

test_that("canonical evaluation metadata distinguishes hits and provider calls", {
  program <- module(signature("x -> y"), type = "predict")
  metadata <- list(
    list(
      input_tokens = 8L,
      output_tokens = 2L,
      total_tokens = 10L,
      cost = 0.01,
      cache = "miss",
      error = NA_character_
    ),
    list(
      input_tokens = 99L,
      output_tokens = 99L,
      total_tokens = 198L,
      cost = 1,
      cache = "hit",
      error = NA_character_
    )
  )

  usage <- extract_optimizer_usage(program, metadata)
  expect_equal(usage$provider_calls, 1L)
  expect_equal(usage$tokens_in, 8L)
  expect_equal(usage$tokens_out, 2L)
  expect_equal(usage$total_cost, 0.01)
  expect_false(usage$provider_usage_unknown)
})

test_that("row-sized optimizer evaluation resumes without repeating paid rows", {
  calls <- integer()
  testthat::local_mocked_bindings(
    eval_program = function(program, dataset, metric, ...) {
      row <- as.integer(dataset$x[[1L]])
      calls <<- c(calls, row)
      EvalResult(
        examples = tibble::tibble(
          row_id = 1L,
          score = as.numeric(row),
          error = NA_character_,
          predicted = list(row),
          feedback = NA_character_
        ),
        mean_score = as.numeric(row),
        n_evaluated = 1L,
        n_errors = 0L,
        input_tokens = 2L,
        output_tokens = 1L,
        total_tokens = 3L,
        total_cost = 0.01,
        provider_calls = 1L,
        metric_calls = 1L
      )
    },
    .package = "dsprrr"
  )
  program <- module(signature("x -> y"), type = "predict")
  data <- data.frame(x = 1:5, y = 1:5)
  partial <- list()
  first <- new_optimizer_budget(optimizer_control(max_metric_calls = 2L))
  first_result <- optimizer_eval_program(
    program,
    data,
    metric = function(...) 1,
    budget = first,
    stage = "search",
    unit_id = "trial:1",
    on_progress = function(records, ...) partial <<- records
  )

  expect_identical(calls, 1:2)
  expect_equal(nrow(first_result@examples), 2L)
  expect_identical(
    optimizer_budget_summary(first)$stop_reason$code,
    "max_metric_calls"
  )

  second <- new_optimizer_budget(
    optimizer_control(max_metric_calls = 5L),
    state = optimizer_budget_state(first)
  )
  resumed <- optimizer_eval_program(
    program,
    data,
    metric = function(...) 1,
    budget = second,
    stage = "search",
    unit_id = "trial:1",
    partial_records = partial,
    on_progress = function(records, ...) partial <<- records
  )

  expect_identical(calls, 1:5)
  expect_equal(resumed@examples$score, as.numeric(1:5))
  expect_equal(optimizer_budget_summary(second)$trials, 1L)
  expect_equal(optimizer_budget_summary(second)$metric_calls, 5L)
  expect_true(optimizer_budget_unit_completed(second, "trial:1"))

  uninterrupted_budget <- new_optimizer_budget(
    optimizer_control(max_metric_calls = 5L)
  )
  uninterrupted <- optimizer_eval_program(
    program,
    data,
    metric = function(...) 1,
    budget = uninterrupted_budget,
    stage = "search",
    unit_id = "trial:1"
  )
  expect_equal(resumed@examples$score, uninterrupted@examples$score)
  expect_equal(resumed@mean_score, uninterrupted@mean_score)
})

checkpoint_fixture <- function(path, control = NULL) {
  program <- module(signature("x -> y"), type = "predict")
  metric <- metric_exact_match(field = "y")
  data <- data.frame(x = c("a", "b"), y = c("a", "b"))
  if (is.null(control)) {
    control <- optimizer_control(checkpoint_path = path)
  }
  context <- optimizer_checkpoint_begin(
    "FixtureOptimizer",
    1L,
    program,
    data,
    metric,
    config = list(population_size = 2L),
    control = control,
    initial_state = list(index = 0L)
  )
  list(
    context = context,
    program = program,
    metric = metric,
    data = data
  )
}

test_that("optimizer checkpoints roundtrip through a private atomic manifest", {
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  fixture <- checkpoint_fixture(path)
  record_optimizer_usage(
    fixture$context$budget,
    list(trials = 1L, metric_calls = 2L, known_cost = 0.03),
    "trial"
  )
  optimizer_budget_complete_unit(fixture$context$budget, "trial:1")
  set.seed(42)
  expected_rng <- optimizer_checkpoint_capture_rng()

  optimizer_checkpoint_write(
    fixture$context,
    phase = "search",
    search_state = list(index = 1L, scores = 0.75),
    lineage = list(list(child = "candidate:1", parent = "base")),
    best_program = fixture$program
  )

  checkpoint <- optimizer_checkpoint_read(path)
  expect_identical(checkpoint$format, "dsprrr-optimizer-checkpoint")
  expect_identical(checkpoint$format_version, 1L)
  expect_identical(checkpoint$rng, expected_rng)
  expect_identical(checkpoint$progress$phase, "search")
  expect_equal(checkpoint$budget$trials, 1L)
  expect_identical(checkpoint$budget$completed_units, "trial:1")
  expect_silent(artifact_validate_manifest(checkpoint$best_program))
  if (.Platform$OS.type == "unix") {
    expect_identical(as.character(as.octmode(file.info(path)$mode)), "600")
  }

  resumed_control <- optimizer_control(
    checkpoint_path = path,
    resume = TRUE,
    max_trials = 4L
  )
  resumed <- checkpoint_fixture(path, resumed_control)$context
  expect_true(resumed$resumed)
  expect_identical(resumed$phase, "search")
  expect_equal(resumed$search_state$index, 1L)
  expect_equal(optimizer_budget_summary(resumed$budget)$trials, 1L)
  expect_true(inherits(resumed$best_program, "Module"))
})

test_that("checkpoint integrity and compatibility failures are typed", {
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  fixture <- checkpoint_fixture(path)
  optimizer_checkpoint_write(
    fixture$context,
    "search",
    list(index = 1L),
    best_program = fixture$program
  )

  changed_control <- optimizer_control(checkpoint_path = path, resume = TRUE)
  condition <- expect_error(
    optimizer_checkpoint_begin(
      "FixtureOptimizer",
      1L,
      fixture$program,
      transform(fixture$data, y = c("different", "b")),
      fixture$metric,
      list(population_size = 2L),
      changed_control
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible"
  )
  expect_true(any(vapply(
    condition$differences,
    function(item) identical(item$field, "compatibility.fingerprints.data"),
    logical(1)
  )))

  checkpoint <- readRDS(path)
  checkpoint$progress$phase <- "tampered"
  saveRDS(checkpoint, path)
  expect_error(
    optimizer_checkpoint_read(path),
    class = "dsprrr_optimizer_checkpoint_integrity_error"
  )
})

test_that("checkpoint metadata is closed and canonically typed before integrity", {
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  fixture <- checkpoint_fixture(path)
  optimizer_checkpoint_write(
    fixture$context,
    "search",
    list(index = 1L),
    best_program = fixture$program
  )
  original <- readRDS(path)

  bad_budget <- original
  bad_budget$budget$attempts <- 0.5
  bad_budget$integrity <- optimizer_checkpoint_integrity(bad_budget)
  saveRDS(bad_budget, path)
  expect_error(
    optimizer_checkpoint_read(path),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )

  opaque <- original
  opaque$metadata$written_at <- new.env(parent = emptyenv())
  saveRDS(opaque, path)
  condition <- rlang::catch_cnd(optimizer_checkpoint_read(path))
  expect_true(
    inherits(condition, "dsprrr_optimizer_checkpoint_unsafe_value") ||
      inherits(condition, "dsprrr_optimizer_checkpoint_malformed")
  )

  tampered_timestamp <- original
  tampered_timestamp$metadata$written_at <- "2000-01-01T00:00:00Z"
  saveRDS(tampered_timestamp, path)
  expect_error(
    optimizer_checkpoint_read(path),
    class = "dsprrr_optimizer_checkpoint_integrity_error"
  )

  bad_timestamp <- original
  bad_timestamp$metadata$created_at <- NA_character_
  bad_timestamp$integrity <- optimizer_checkpoint_integrity(bad_timestamp)
  saveRDS(bad_timestamp, path)
  expect_error(
    optimizer_checkpoint_read(path),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )

  bad_version <- original
  bad_version$metadata$artifact_version <- 3
  bad_version$integrity <- optimizer_checkpoint_integrity(bad_version)
  saveRDS(bad_version, path)
  expect_error(
    optimizer_checkpoint_read(path),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )
})

test_that("checkpoint path trust rejects substitution and untrusted inputs", {
  skip_if(.Platform$OS.type != "unix", "POSIX identity checks are unavailable")

  permissive <- withr::local_tempfile(fileext = ".rds")
  unlink(permissive)
  fixture <- checkpoint_fixture(permissive)
  optimizer_checkpoint_write(
    fixture$context,
    "search",
    list(index = 1L),
    best_program = fixture$program
  )
  Sys.chmod(permissive, mode = "0644", use_umask = FALSE)
  expect_error(
    optimizer_checkpoint_read(permissive),
    class = "dsprrr_optimizer_checkpoint_trust_error"
  )

  real <- withr::local_tempfile(fileext = ".rds")
  alias <- paste0(real, "-link")
  withr::defer(unlink(alias, force = TRUE))
  unlink(real)
  fixture <- checkpoint_fixture(real)
  optimizer_checkpoint_write(
    fixture$context,
    "search",
    list(index = 1L),
    best_program = fixture$program
  )
  expect_true(file.symlink(real, alias))
  expect_error(
    optimizer_checkpoint_read(alias),
    class = "dsprrr_optimizer_checkpoint_trust_error"
  )

  substituted <- withr::local_tempfile(fileext = ".rds")
  unlink(substituted)
  fixture <- checkpoint_fixture(substituted)
  testthat::local_mocked_bindings(
    artifact_write_rds = function(value, path) {
      unlink(path, force = TRUE)
      saveRDS(value, path)
      Sys.chmod(path, mode = "0600", use_umask = FALSE)
      invisible(path)
    },
    .package = "dsprrr"
  )
  expect_error(
    optimizer_checkpoint_write(
      fixture$context,
      "search",
      list(index = 1L),
      best_program = fixture$program
    ),
    class = "dsprrr_optimizer_checkpoint_trust_error"
  )
})

test_that("checkpoint publication detects canonical parent retargeting", {
  skip_if(.Platform$OS.type != "unix", "POSIX identity checks are unavailable")
  root <- tempfile("checkpoint-parent-")
  moved <- paste0(root, "-moved")
  dir.create(root, mode = "0700")
  withr::defer(unlink(c(root, moved), recursive = TRUE, force = TRUE))
  path <- file.path(root, "checkpoint.rds")
  fixture <- checkpoint_fixture(path)

  testthat::local_mocked_bindings(
    artifact_write_rds = function(value, stage) {
      stage_name <- basename(stage)
      expect_true(file.rename(root, moved))
      dir.create(root, mode = "0700")
      saveRDS(value, file.path(moved, stage_name))
      invisible(stage)
    },
    .package = "dsprrr"
  )
  expect_error(
    optimizer_checkpoint_write(
      fixture$context,
      "search",
      list(index = 1L),
      best_program = fixture$program
    ),
    class = "dsprrr_optimizer_checkpoint_trust_error"
  )
})

test_that("checkpoint reads detect same-inode rewrites with restored mtime", {
  skip_if(.Platform$OS.type != "unix", "POSIX change times are unavailable")
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  fixture <- checkpoint_fixture(path)
  optimizer_checkpoint_write(
    fixture$context,
    "search",
    list(index = 1L),
    best_program = fixture$program
  )
  before <- fs::file_info(path, follow = FALSE)
  before_mtime <- file.info(path)$mtime
  original_read <- optimizer_checkpoint_read_rds
  rewritten <- FALSE
  testthat::local_mocked_bindings(
    optimizer_checkpoint_read_rds = function(target) {
      value <- original_read(target)
      if (
        !rewritten &&
          identical(
            normalizePath(target, mustWork = TRUE),
            normalizePath(path, mustWork = TRUE)
          )
      ) {
        bytes <- readBin(target, "raw", n = file.info(target)$size)
        Sys.sleep(0.02)
        connection <- file(target, open = "r+b")
        writeBin(bytes, connection)
        close(connection)
        Sys.setFileTime(target, before_mtime)
        rewritten <<- TRUE
      }
      value
    },
    .package = "dsprrr"
  )

  expect_error(
    optimizer_checkpoint_read(path),
    class = "dsprrr_optimizer_checkpoint_trust_error"
  )
  after <- fs::file_info(path, follow = FALSE)
  expect_identical(as.numeric(after$inode), as.numeric(before$inode))
  expect_equal(as.numeric(after$modification_time), as.numeric(before_mtime))
  expect_false(identical(
    as.numeric(after$change_time),
    as.numeric(before$change_time)
  ))
})

test_that("concurrent checkpoint writers reject a stale predecessor", {
  skip_if_not_installed("callr")
  directory <- withr::local_tempdir()
  path <- file.path(directory, "shared-checkpoint.rds")
  package_context <- callr_dsprrr_context()
  package_loader <- callr_load_dsprrr
  gate <- file.path(directory, "go")
  ready <- file.path(directory, c("ready-1", "ready-2"))
  worker <- function(
    package_context,
    package_loader,
    path,
    gate,
    ready,
    writer
  ) {
    namespace <- package_loader(package_context)
    module <- get("module", envir = namespace, inherits = FALSE)
    signature <- get("signature", envir = namespace, inherits = FALSE)
    metric_exact_match <- get(
      "metric_exact_match",
      envir = namespace,
      inherits = FALSE
    )
    optimizer_control <- get(
      "optimizer_control",
      envir = namespace,
      inherits = FALSE
    )
    optimizer_checkpoint_begin <- get(
      "optimizer_checkpoint_begin",
      envir = namespace,
      inherits = FALSE
    )
    optimizer_checkpoint_write <- get(
      "optimizer_checkpoint_write",
      envir = namespace,
      inherits = FALSE
    )
    program <- module(signature("x -> y"), type = "predict")
    metric <- metric_exact_match(field = "y")
    context <- optimizer_checkpoint_begin(
      "ConcurrentOptimizer",
      1L,
      program,
      data.frame(x = "a", y = "a"),
      metric,
      config = list(search = "same"),
      control = optimizer_control(checkpoint_path = path)
    )
    file.create(ready)
    deadline <- Sys.time() + 20
    while (!file.exists(gate) && Sys.time() < deadline) {
      Sys.sleep(0.01)
    }
    if (!file.exists(gate)) {
      stop("checkpoint writer gate timed out")
    }
    options(
      dsprrr.optimizer_checkpoint_lock_hook = function() Sys.sleep(0.15)
    )
    tryCatch(
      {
        optimizer_checkpoint_write(
          context,
          "search",
          list(writer = writer),
          best_program = program
        )
        list(status = "ok", classes = character())
      },
      error = function(e) list(status = "error", classes = class(e))
    )
  }
  processes <- lapply(seq_along(ready), function(i) {
    callr::r_bg(
      worker,
      args = list(
        package_context,
        package_loader,
        path,
        gate,
        ready[[i]],
        paste0("writer-", i)
      ),
      supervise = TRUE
    )
  })
  withr::defer(lapply(processes, function(process) {
    if (process$is_alive()) {
      process$kill()
    }
  }))
  deadline <- Sys.time() + 20
  while (!all(file.exists(ready)) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_identical(all(file.exists(ready)), TRUE)
  file.create(gate)
  lapply(processes, function(process) process$wait(timeout = 20000))
  results <- lapply(processes, function(process) process$get_result())
  statuses <- vapply(results, function(result) result$status, character(1))
  conflicts <- vapply(
    results,
    function(result) {
      "dsprrr_optimizer_checkpoint_conflict" %in% result$classes
    },
    logical(1)
  )

  expect_identical(sum(statuses == "ok"), 1L)
  expect_identical(sum(conflicts), 1L)
  checkpoint <- optimizer_checkpoint_read(path)
  expect_in(
    checkpoint$progress$search_state$writer,
    c("writer-1", "writer-2")
  )
  if (.Platform$OS.type == "unix") {
    lock_path <- file.path(
      directory,
      paste0(".", basename(path), ".lock")
    )
    expect_identical(as.character(as.octmode(file.info(lock_path)$mode)), "600")
  }
})

test_that("checkpoint locks are released when a writer process dies", {
  skip_if_not_installed("callr")
  directory <- withr::local_tempdir()
  path <- file.path(directory, "crash-checkpoint.rds")
  ready <- file.path(directory, "lock-held")
  package_context <- callr_dsprrr_context()
  package_loader <- callr_load_dsprrr
  holder <- callr::r_bg(
    function(package_context, package_loader, path, ready) {
      namespace <- package_loader(package_context)
      optimizer_checkpoint_snapshot <- get(
        "optimizer_checkpoint_snapshot",
        envir = namespace,
        inherits = FALSE
      )
      options(dsprrr.optimizer_checkpoint_lock_hook = function() {
        file.create(ready)
        Sys.sleep(60)
      })
      optimizer_checkpoint_snapshot(path)
    },
    args = list(package_context, package_loader, path, ready),
    supervise = TRUE
  )
  withr::defer(if (holder$is_alive()) holder$kill())
  deadline <- Sys.time() + 20
  while (!file.exists(ready) && Sys.time() < deadline) {
    Sys.sleep(0.01)
  }
  expect_identical(file.exists(ready), TRUE)
  holder$kill()
  holder$wait(timeout = 10000)
  withr::local_options(dsprrr.optimizer_checkpoint_lock_timeout = 2)

  expect_null(optimizer_checkpoint_snapshot(path))
})

test_that("checkpoint publication failure leaves the prior file unchanged", {
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  fixture <- checkpoint_fixture(path)
  first <- optimizer_checkpoint_write(
    fixture$context,
    "search",
    list(index = 1L),
    best_program = fixture$program
  )
  before <- readBin(path, "raw", n = file.info(path)$size)

  testthat::local_mocked_bindings(
    artifact_atomic_replace = function(...) {
      cli::cli_abort("replacement failed", class = "test_replace_failure")
    },
    .package = "dsprrr"
  )
  expect_error(
    optimizer_checkpoint_write(
      fixture$context,
      "search",
      list(index = 2L),
      best_program = fixture$program
    ),
    class = "dsprrr_optimizer_checkpoint_io_error"
  )
  after <- readBin(path, "raw", n = file.info(path)$size)
  expect_identical(after, before)
  expect_identical(first$progress$search_state$index, 1L)
})

test_that("checkpoint fingerprints reject secrets and stateful metrics", {
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  fixture <- checkpoint_fixture(path)

  secret_names <- c(
    "api_key",
    "openaiApiKey",
    "openaiAPIKey",
    "OpenAIAPIKey",
    "OAuthAccessToken",
    "clientSecret",
    "ClientSecret",
    "privateKey"
  )
  for (name in secret_names) {
    unsafe_config <- stats::setNames(list("secret"), name)
    expect_error(
      optimizer_checkpoint_begin(
        "Unsafe",
        1L,
        fixture$program,
        fixture$data,
        fixture$metric,
        config = unsafe_config,
        control = optimizer_control(checkpoint_path = path)
      ),
      class = "dsprrr_optimizer_checkpoint_unsafe_value",
      info = name
    )
  }

  optimizer_checkpoint_write(
    fixture$context,
    "search",
    search_state = list(index = 1L),
    best_program = fixture$program
  )
  before <- readBin(path, "raw", n = file.info(path)$size)
  for (name in secret_names[-1L]) {
    unsafe_state <- stats::setNames(
      list("CHECKPOINT_CAMEL_SECRET_SENTINEL"),
      name
    )
    expect_error(
      optimizer_checkpoint_write(
        fixture$context,
        "search",
        search_state = unsafe_state,
        best_program = fixture$program
      ),
      class = "dsprrr_optimizer_checkpoint_unsafe_value",
      info = name
    )
    expect_identical(
      readBin(path, "raw", n = file.info(path)$size),
      before,
      info = name
    )
  }

  stateful_value <- new.env(parent = emptyenv())
  stateful_value$value <- 1
  stateful_metric <- function(prediction, expected) {
    as.numeric(identical(prediction, expected)) * stateful_value$value
  }
  expect_error(
    optimizer_checkpoint_begin(
      "Unsafe",
      1L,
      fixture$program,
      fixture$data,
      stateful_metric,
      config = list(),
      control = optimizer_control(checkpoint_path = path)
    ),
    class = "dsprrr_optimizer_checkpoint_fingerprint_error"
  )

  expect_error(
    optimizer_checkpoint_write(
      fixture$context,
      "search",
      search_state = list(runtime = new.env()),
      best_program = fixture$program
    ),
    class = "dsprrr_optimizer_checkpoint_unsafe_value"
  )
})

test_that("Bootstrap module checkpoints bind the effective model by hash", {
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  program <- module(signature("question -> answer"), type = "predict")
  data <- data.frame(question = "q", answer = "a")
  teleprompter <- BootstrapFewShot(
    metric = function(...) 1,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 1L
  )
  secret_model <- "model-a?api_key=TOPSECRET"
  compile(
    teleprompter,
    program,
    data,
    .llm = checkpoint_test_chat(secret_model),
    control = optimizer_control(
      max_metric_calls = 0L,
      checkpoint_path = path,
      progress = FALSE
    )
  )
  checkpoint <- optimizer_checkpoint_read(path)
  identity <- checkpoint$compatibility$config$effective_runtime$runtime
  expect_named(
    identity,
    c(
      "kind",
      "provider_class_sha256",
      "provider_name_sha256",
      "base_url_sha256",
      "model_sha256"
    )
  )
  expect_false(any(grepl("TOPSECRET|model-a", unlist(identity))))

  condition <- expect_error(
    compile(
      teleprompter,
      program,
      data,
      .llm = checkpoint_test_chat("model-b"),
      control = optimizer_control(
        max_metric_calls = 1L,
        checkpoint_path = path,
        resume = TRUE,
        progress = FALSE
      )
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible"
  )
  expect_true(any(vapply(
    condition$differences,
    function(item) {
      identical(
        item$field,
        "compatibility.config.effective_runtime.runtime.model_sha256"
      )
    },
    logical(1)
  )))
})

test_that("Bootstrap pipeline checkpoints reject a changed effective model", {
  draft <- module(signature("question -> draft"), type = "predict")
  answer <- module(signature("draft -> answer"), type = "predict")
  program <- draft %>>% answer
  data <- data.frame(question = "q", answer = "a")
  teleprompter <- BootstrapFewShot(
    metric = function(...) 1,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 1L
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  compile(
    teleprompter,
    program,
    data,
    .llm = checkpoint_test_chat("pipeline-model-a"),
    control = optimizer_control(
      max_metric_calls = 0L,
      checkpoint_path = path,
      progress = FALSE
    )
  )

  condition <- expect_error(
    compile(
      teleprompter,
      program,
      data,
      .llm = checkpoint_test_chat("pipeline-model-b"),
      control = optimizer_control(
        max_metric_calls = 1L,
        checkpoint_path = path,
        resume = TRUE,
        progress = FALSE
      )
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible"
  )
  expect_true(any(vapply(
    condition$differences,
    function(item) {
      identical(
        item$field,
        "compatibility.config.effective_runtime.runtime.model_sha256"
      )
    },
    logical(1)
  )))
})

test_that("effective runtime distinguishes providers sharing a model name", {
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  program <- module(signature("question -> answer"), type = "predict")
  data <- data.frame(question = "q", answer = "a")
  metric <- function(...) 1
  teleprompter <- BootstrapFewShot(
    metric = metric,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 1L
  )
  compile(
    teleprompter,
    program,
    data,
    .llm = checkpoint_test_chat("shared-model", provider = "openai"),
    control = optimizer_control(
      max_metric_calls = 0L,
      checkpoint_path = path,
      progress = FALSE
    )
  )

  condition <- expect_error(
    compile(
      teleprompter,
      program,
      data,
      .llm = checkpoint_test_chat(
        "shared-model",
        provider = "anthropic"
      ),
      control = optimizer_control(
        max_metric_calls = 1L,
        checkpoint_path = path,
        resume = TRUE,
        progress = FALSE
      )
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible"
  )
  fields <- vapply(condition$differences, `[[`, character(1), "field")
  expect_true(any(grepl("provider_(class|name)_sha256$", fields)))
  expect_false(any(grepl("model_sha256$", fields)))
})

test_that("effective runtime resolves attached and default Chats", {
  data <- data.frame(question = "q", answer = "a")
  metric <- function(...) 1
  teleprompter <- BootstrapFewShot(
    metric = metric,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 1L
  )

  attached_path <- withr::local_tempfile(fileext = ".rds")
  unlink(attached_path)
  attached_a <- module(
    signature("question -> answer"),
    type = "predict",
    chat = checkpoint_test_chat("attached-a")
  )
  compile(
    teleprompter,
    attached_a,
    data,
    control = optimizer_control(
      max_metric_calls = 0L,
      checkpoint_path = attached_path,
      progress = FALSE
    )
  )
  attached_b <- module(
    signature("question -> answer"),
    type = "predict",
    chat = checkpoint_test_chat("attached-b")
  )
  attached_error <- expect_error(
    compile(
      teleprompter,
      attached_b,
      data,
      control = optimizer_control(
        max_metric_calls = 1L,
        checkpoint_path = attached_path,
        resume = TRUE,
        progress = FALSE
      )
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible"
  )
  attached_fields <- vapply(
    attached_error$differences,
    `[[`,
    character(1),
    "field"
  )
  expect_true(any(grepl("effective_runtime.*model_sha256$", attached_fields)))

  default_path <- withr::local_tempfile(fileext = ".rds")
  unlink(default_path)
  program <- module(signature("question -> answer"), type = "predict")
  withr::local_options(list(
    dsprrr.default_chat = checkpoint_test_chat("default-a")
  ))
  compile(
    teleprompter,
    program,
    data,
    control = optimizer_control(
      max_metric_calls = 0L,
      checkpoint_path = default_path,
      progress = FALSE
    )
  )
  options(dsprrr.default_chat = checkpoint_test_chat("default-b"))
  default_error <- expect_error(
    compile(
      teleprompter,
      program,
      data,
      control = optimizer_control(
        max_metric_calls = 1L,
        checkpoint_path = default_path,
        resume = TRUE,
        progress = FALSE
      )
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible"
  )
  default_fields <- vapply(
    default_error$differences,
    `[[`,
    character(1),
    "field"
  )
  expect_true(any(grepl("effective_runtime.*model_sha256$", default_fields)))
})

test_that("checkpointing fails before work without a stable effective Chat", {
  testthat::local_mocked_bindings(
    get_default_chat = function(create = TRUE) NULL,
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  counter <- 0L
  metric <- function(...) {
    counter <<- counter + 1L
    1
  }
  expect_error(
    compile(
      BootstrapFewShot(
        metric = metric,
        max_labeled_demos = 0L,
        max_bootstrapped_demos = 1L
      ),
      module(signature("question -> answer"), type = "predict"),
      data.frame(question = "q", answer = "a"),
      control = optimizer_control(
        checkpoint_path = path,
        progress = FALSE
      )
    ),
    class = "dsprrr_optimizer_checkpoint_fingerprint_error"
  )
  expect_identical(counter, 0L)
  expect_false(file.exists(path))
})

test_that("BootstrapFewShot checkpoint resume matches uninterrupted search", {
  make_llm <- function(counter) {
    structure(
      list(
        chat_structured = function(prompt, type, ...) {
          counter$calls <- counter$calls + 1L
          list(answer = "yes")
        },
        get_model = function() "bootstrap-checkpoint-model"
      ),
      class = "Chat"
    )
  }
  program <- module(signature("question -> answer"), type = "predict")
  data <- data.frame(
    question = paste0("q", 1:4),
    answer = rep("yes", 4)
  )
  teleprompter <- BootstrapFewShot(
    metric = metric_exact_match(field = "answer"),
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 4L,
    max_rounds = 1L,
    seed = 42L
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  resumed_counter <- new.env(parent = emptyenv())
  resumed_counter$calls <- 0L
  resumed_chat <- make_llm(resumed_counter)
  registry <- list(runtime = resumed_chat)

  partial <- compile(
    teleprompter,
    program,
    data,
    .llm = resumed_chat,
    control = optimizer_control(
      max_metric_calls = 2L,
      checkpoint_path = path,
      checkpoint_registry = registry,
      progress = FALSE
    )
  )
  expect_equal(resumed_counter$calls, 2L)
  expect_equal(partial$config$optimizer$n_bootstrapped_demos, 2L)
  expect_identical(
    partial$config$optimizer$stop_reason$code,
    "max_metric_calls"
  )
  expect_identical(optimizer_checkpoint_read(path)$progress$phase, "bootstrap")

  resumed <- compile(
    teleprompter,
    program,
    data,
    .llm = resumed_chat,
    control = optimizer_control(
      max_metric_calls = 4L,
      checkpoint_path = path,
      checkpoint_registry = registry,
      resume = TRUE,
      progress = FALSE
    )
  )
  expect_equal(resumed_counter$calls, 4L)
  expect_equal(resumed$config$optimizer$n_bootstrapped_demos, 4L)
  expect_identical(optimizer_checkpoint_read(path)$progress$phase, "complete")

  uninterrupted_counter <- new.env(parent = emptyenv())
  uninterrupted_counter$calls <- 0L
  uninterrupted <- compile(
    teleprompter,
    program,
    data,
    .llm = make_llm(uninterrupted_counter),
    control = optimizer_control(max_metric_calls = 4L, progress = FALSE)
  )
  expect_equal(uninterrupted_counter$calls, 4L)
  expect_equal(resumed$demos, uninterrupted$demos)
  expect_equal(
    resumed$config$optimizer$budget_summary$metric_calls,
    uninterrupted$config$optimizer$budget_summary$metric_calls
  )
  expect_equal(resumed$config$optimizer$total_attempts, 4L)
})

test_that("MIPRO resumes an interrupted BO row without repeated provider calls", {
  make_chat <- function(counter) {
    local({
      self <- structure(
        list(
          chat_structured = function(prompt, type, ...) {
            counter$calls <- counter$calls + 1L
            list(answer = "ok")
          },
          clone = function(...) self,
          set_turns = function(turns) invisible(NULL),
          get_model = function() "checkpoint-test-model"
        ),
        class = "Chat"
      )
      self
    })
  }
  metric <- function(prediction, expected) 1
  data <- data.frame(
    question = c("one", "two", "three"),
    answer = "ok"
  )
  program <- module(signature("question -> answer"), type = "predict")
  teleprompter <- MIPROv2(
    metric = metric,
    auto = NULL,
    num_candidates = 5L,
    max_bootstrapped_demos = 0L,
    max_labeled_demos = 1L,
    seed = 42L,
    track_stats = TRUE
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  chat <- make_chat(counter)
  registry <- list(metric = metric, task = chat)

  expect_warning(
    interrupted <- dsprrr:::compile_mipro(
      teleprompter,
      program,
      data,
      .llm = chat,
      control = optimizer_control(
        max_metric_calls = 4L,
        checkpoint_path = path,
        checkpoint_registry = registry,
        progress = FALSE
      )
    ),
    "No candidate received full evaluation"
  )
  expect_true(interrupted$config$optimizer$partial)
  expect_equal(counter$calls, 4L)
  expect_identical(
    interrupted$config$optimizer$stop_reason$code,
    "max_metric_calls"
  )

  resumed <- dsprrr:::compile_mipro(
    teleprompter,
    program,
    data,
    .llm = chat,
    control = optimizer_control(
      max_metric_calls = 15L,
      checkpoint_path = path,
      resume = TRUE,
      checkpoint_registry = registry,
      progress = FALSE
    )
  )
  expect_equal(counter$calls, 15L)
  expect_false(resumed$config$optimizer$partial)

  fresh_counter <- new.env(parent = emptyenv())
  fresh_counter$calls <- 0L
  fresh_chat <- make_chat(fresh_counter)
  fresh <- dsprrr:::compile_mipro(
    teleprompter,
    program,
    data,
    .llm = fresh_chat,
    control = optimizer_control(
      max_metric_calls = 15L,
      progress = FALSE
    )
  )
  expect_equal(fresh_counter$calls, 15L)
  expect_identical(
    resumed$config$optimizer$trial_history,
    fresh$config$optimizer$trial_history
  )
  expect_identical(
    resumed$config$optimizer$best_config,
    fresh$config$optimizer$best_config
  )
  expect_identical(resumed$demos, fresh$demos)
})

test_that("MIPRO replays one durable trial after failure between append and checkpoint", {
  counter <- new.env(parent = emptyenv())
  counter$calls <- 0L
  chat <- local({
    self <- structure(
      list(
        chat_structured = function(prompt, type, ...) {
          counter$calls <- counter$calls + 1L
          list(answer = "ok")
        },
        clone = function(...) self,
        set_turns = function(turns) invisible(NULL),
        get_model = function() "mipro-log-resume-model"
      ),
      class = "Chat"
    )
    self
  })
  metric <- function(...) 1
  registry <- list(metric = metric, task = chat)
  program <- module(signature("question -> answer"), type = "predict")
  data <- data.frame(question = "q", answer = "ok")
  teleprompter <- MIPROv2(
    metric = metric,
    auto = NULL,
    num_candidates = 1L,
    max_bootstrapped_demos = 0L,
    max_labeled_demos = 1L,
    seed = 11L,
    log_dir = tempfile("mipro-resume-log-")
  )
  dir.create(teleprompter@log_dir, mode = "0700")
  withr::defer(unlink(teleprompter@log_dir, recursive = TRUE, force = TRUE))
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  journal <- file.path(teleprompter@log_dir, "trials.jsonl")
  original_write <- optimizer_checkpoint_write

  expect_error(
    testthat::with_mocked_bindings(
      suppressWarnings(compile_mipro(
        teleprompter,
        program,
        data,
        .llm = chat,
        control = optimizer_control(
          checkpoint_path = path,
          checkpoint_registry = registry,
          log_dir = teleprompter@log_dir,
          progress = FALSE
        )
      )),
      optimizer_checkpoint_write = function(
        context,
        phase,
        search_state,
        ...
      ) {
        result <- original_write(context, phase, search_state, ...)
        active <- search_state$bo$active_trial %||% NULL
        if (
          !is.null(active) &&
            isTRUE(active$result_applied) &&
            is.null(active$trial_record)
        ) {
          cli::cli_abort(
            "Injected failure after applying the result",
            class = "test_after_result_apply"
          )
        }
        result
      },
      .package = "dsprrr"
    ),
    class = "test_after_result_apply"
  )
  expect_equal(counter$calls, 1L)
  applied_checkpoint <- optimizer_checkpoint_read(path)
  applied_active <- applied_checkpoint$progress$search_state$bo$active_trial
  expect_true(applied_active$result_applied)
  expect_null(applied_active$trial_record)
  expect_identical(
    applied_checkpoint$progress$search_state$bo$stats[[
      applied_active$candidate_index
    ]]$count,
    1L
  )

  expect_error(
    testthat::with_mocked_bindings(
      suppressWarnings(compile_mipro(
        teleprompter,
        program,
        data,
        .llm = chat,
        control = optimizer_control(
          checkpoint_path = path,
          checkpoint_registry = registry,
          log_dir = teleprompter@log_dir,
          resume = TRUE,
          progress = FALSE
        )
      )),
      optimizer_checkpoint_write = function(...) {
        lines <- if (file.exists(journal)) {
          readLines(journal, warn = FALSE)
        } else {
          character()
        }
        if (length(lines) > 0L) {
          cli::cli_abort(
            "Injected failure after trial append",
            class = "test_after_trial_append"
          )
        }
        original_write(...)
      },
      .package = "dsprrr"
    ),
    class = "test_after_trial_append"
  )
  expect_equal(counter$calls, 1L)
  first <- read_trials_jsonl(journal)
  expect_length(first, 1L)
  interrupted_checkpoint <- optimizer_checkpoint_read(path)
  active <- interrupted_checkpoint$progress$search_state$bo$active_trial
  expect_true(active$result_applied)
  expect_identical(
    interrupted_checkpoint$progress$search_state$bo$stats[[
      active$candidate_index
    ]]$count,
    1L
  )

  conflicting <- discrete_bo_restore_trial_record(
    discrete_bo_checkpoint_trial_record(first[[1L]])
  )
  conflicting@notes <- "conflicting replay record"
  write_trials_jsonl(list(conflicting), journal)
  expect_error(
    suppressWarnings(compile_mipro(
      teleprompter,
      program,
      data,
      .llm = chat,
      control = optimizer_control(
        checkpoint_path = path,
        checkpoint_registry = registry,
        log_dir = teleprompter@log_dir,
        resume = TRUE,
        progress = FALSE
      )
    )),
    class = "dsprrr_trial_id_conflict"
  )
  expect_equal(counter$calls, 1L)
  after_conflict <- optimizer_checkpoint_read(path)
  conflict_active <- after_conflict$progress$search_state$bo$active_trial
  expect_true(conflict_active$result_applied)
  expect_identical(
    after_conflict$progress$search_state$bo$stats[[
      conflict_active$candidate_index
    ]]$count,
    1L
  )
  write_trials_jsonl(first, journal)

  resumed_program <- suppressWarnings(compile_mipro(
    teleprompter,
    program,
    data,
    .llm = chat,
    control = optimizer_control(
      checkpoint_path = path,
      checkpoint_registry = registry,
      log_dir = teleprompter@log_dir,
      resume = TRUE,
      progress = FALSE
    )
  ))
  expect_equal(counter$calls, 1L)
  resumed <- read_trials_jsonl(journal)
  expect_length(resumed, 1L)
  expect_identical(resumed[[1L]]@trial_id, first[[1L]]@trial_id)
  expect_identical(
    sum(vapply(
      optimizer_checkpoint_read(path)$progress$search_state$bo$stats,
      function(stat) stat$count,
      integer(1)
    )),
    1L
  )
  expect_false(resumed_program$config$optimizer$partial)
})
