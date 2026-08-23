test_that("optimization results expose one stable read-only schema", {
  program <- module(signature("question -> answer"))
  trials <- tibble::tibble(
    trial_id = 1:2,
    score = c(0.4, 0.8),
    parameters = list(list(style = "plain"), list(style = "direct")),
    total_cost = c(0.01, 0.02)
  )

  record_optimization_result(
    program,
    optimizer = "TestSearch",
    baseline_score = 0.4,
    best_score = 0.8,
    best_trial = 2L,
    best_params = list(style = "direct"),
    trials = trials,
    lineage = list(parent_trial = 1L),
    budget = list(max_trials = 2L, used_trials = 2L),
    stop_reason = "budget_exhausted",
    extensions = list(search_space = list(style = c("plain", "direct")))
  )

  result <- optimization_result(program)

  expect_s3_class(result, "dsprrr_optimization_result")
  expect_named(
    result,
    c(
      "version",
      "optimizer",
      "status",
      "baseline_score",
      "best_score",
      "best_trial",
      "best_params",
      "trials",
      "lineage",
      "budget",
      "stop_reason",
      "extensions"
    )
  )
  expect_identical(result$version, 1L)
  expect_identical(result$optimizer, "TestSearch")
  expect_identical(result$status, "completed")
  expect_equal(result$best_score, 0.8)
  expect_equal(result$trials, trials)
  expect_named(result$extensions, "test_search")

  result$best_params$style <- "mutated"
  expect_identical(
    optimization_result(program)$best_params$style,
    "direct"
  )
})

test_that("partial optimization results remain usable and compiled", {
  program <- module(signature("question -> answer"))

  record_optimization_result(
    program,
    optimizer = "InterruptedSearch",
    status = "partial",
    best_score = 0.6,
    stop_reason = "max_errors"
  )

  result <- optimization_result(program)
  expect_identical(result$status, "partial")
  expect_identical(result$stop_reason, "max_errors")
  expect_true(program$is_compiled())
  expect_equal(best_params(program), list())
})

test_that("optimization_result returns NULL before optimization", {
  program <- module(signature("question -> answer"))

  expect_null(optimization_result(program))
})

test_that("recording rejects malformed result fields", {
  program <- module(signature("question -> answer"))

  expect_error(
    record_optimization_result(program, optimizer = "", best_score = 1),
    class = "dsprrr_optimization_result_error"
  )
  expect_error(
    record_optimization_result(
      program,
      optimizer = "Search",
      status = "stopped"
    ),
    class = "dsprrr_optimization_result_error"
  )
  expect_error(
    record_optimization_result(
      program,
      optimizer = "Search",
      extensions = list(bad = environment())
    ),
    class = "dsprrr_optimization_result_error"
  )
})

test_that("optimization results survive an artifact round trip", {
  program <- module(signature("question -> answer"))
  record_optimization_result(
    program,
    optimizer = "TestSearch",
    baseline_score = 0.2,
    best_score = 0.9,
    best_trial = 1L,
    best_params = list(style = "direct"),
    trials = tibble::tibble(trial_id = 1L, score = 0.9),
    stop_reason = "completed",
    extensions = list(note = "kept")
  )

  restored <- restore_module_config(program_artifact(program))

  expect_equal(optimization_result(restored), optimization_result(program))
  expect_true(restored$is_compiled())
})

test_that("optimization result print leads with outcome", {
  program <- module(signature("question -> answer"))
  record_optimization_result(
    program,
    optimizer = "TestSearch",
    best_score = 0.8,
    stop_reason = "completed"
  )

  expect_snapshot(print(optimization_result(program)))
})
