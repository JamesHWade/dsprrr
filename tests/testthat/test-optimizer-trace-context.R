test_that("trial lifecycle preserves trace context", {
  context <- list(
    product = "tempest",
    research_run_id = "research-123",
    stage = "extract_claims",
    detail = list(
      paths = list("governed", "grounded"),
      fail_closed = TRUE
    )
  )

  pending <- create_trial(
    "TraceOptimizer",
    trial_id = "trace-trial",
    trace_context = context
  )
  running <- start_trial(pending)
  completed <- complete_trial(
    running,
    EvalResult(mean_score = 1, n_evaluated = 1L)
  )
  failed <- fail_trial(running, "test failure")

  expect_identical(pending@trace_context, context)
  expect_identical(running@trace_context, context)
  expect_identical(completed@trace_context, context)
  expect_identical(failed@trace_context, context)

  previous <- trace_context_enter(context)
  on.exit(trace_context_restore(previous), add = TRUE)
  inherited <- create_trial("TraceOptimizer", trial_id = "inherited-trace")
  cleared <- create_trial(
    "TraceOptimizer",
    trial_id = "cleared-trace",
    trace_context = list()
  )
  expect_identical(inherited@trace_context, context)
  expect_identical(cleared@trace_context, list())

  expect_error(
    Trial(trace_context = list(runtime = globalenv())),
    class = "dsprrr_trace_context_error"
  )
})

test_that("trial persistence round-trips nested trace context", {
  context <- list(
    product = "tempest",
    research_run_id = "research-123",
    detail = list(
      stages = list("extract_claims", "verify_claim_support"),
      policy = list(fail_closed = TRUE, threshold = 0.75),
      precise_score = 0.12345678901234566,
      values = list("governed", "grounded"),
      empty_object = structure(list(), names = character())
    )
  )
  context["optional"] <- list(NULL)
  trial <- create_trial(
    "TraceOptimizer",
    trial_id = "persisted-trace",
    trace_context = context
  )
  path <- file.path(withr::local_tempdir(), "trials.jsonl")

  write_trials_jsonl(list(trial), path)

  persisted <- jsonlite::fromJSON(
    readLines(path, warn = FALSE),
    simplifyVector = FALSE
  )
  restored <- read_trials_jsonl(path)[[1L]]

  expect_identical(persisted$trace_context, context)
  expect_identical(restored@trace_context, context)

  log <- TrialLog$new("TraceOptimizer")
  log$add_trial(restored, persist = FALSE)
  expect_identical(log$as_tibble()$trace_context[[1L]], context)
})

test_that("trial persistence rejects incomplete records", {
  incomplete_line <- paste0(
    '{"trial_id":"incomplete-trace","optimizer_name":"TraceOptimizer",',
    '"params":[],"metric_summary":{"mean_score":0.3333},',
    '"cost_summary":[],"start_time":{},"end_time":{},',
    '"notes":"","status":"pending"}'
  )
  path <- file.path(withr::local_tempdir(), "incomplete-trials.jsonl")
  writeLines(incomplete_line, path)
  Sys.chmod(path, mode = "0600")
  expect_warning(
    expect_length(read_trials_jsonl(path), 0L),
    class = "dsprrr_parse_warning"
  )

  context <- list(product = "tempest", stage = "verify_claim_support")
  contextual <- create_trial(
    "TraceOptimizer",
    trial_id = "checkpoint-trace",
    trace_context = context
  )
  record <- discrete_bo_checkpoint_trial_record(contextual)
  expect_identical(
    discrete_bo_restore_trial_record(record)@trace_context,
    context
  )

  record$trace_context <- NULL
  expect_error(
    discrete_bo_restore_trial_record(record),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )
})

test_that("trial costs reject string missing-value encodings", {
  expect_error(
    dsprrr:::normalize_trial_cost("NA"),
    class = "dsprrr_trial_record_malformed"
  )
})

test_that("unsafe trial records are rejected without echoing their content", {
  sentinel <- "DO-NOT-LEAK-THIS-API-KEY"
  line <- paste0(
    '{"trial_id":"unsafe","optimizer_name":"TraceOptimizer",',
    '"trace_context":{"api_key":"',
    sentinel,
    '"}}'
  )
  path <- file.path(withr::local_tempdir(), "unsafe-trials.jsonl")
  writeLines(line, path)
  Sys.chmod(path, mode = "0600")
  warning <- NULL

  result <- withCallingHandlers(
    read_trials_jsonl(path),
    warning = function(condition) {
      warning <<- condition
      invokeRestart("muffleWarning")
    }
  )

  expect_length(result, 0L)
  expect_s3_class(warning, "dsprrr_parse_warning")
  expect_false(grepl(sentinel, conditionMessage(warning), fixed = TRUE))
})
