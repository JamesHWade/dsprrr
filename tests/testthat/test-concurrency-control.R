concurrency_test_chat <- function(fail_on = NULL) {
  force(fail_on)
  turns <- list()

  structure(
    list(
      get_turns = function(...) turns,
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      get_model = function() "concurrency-test-model",
      chat_structured = function(prompt, ...) {
        prompt <- as.character(prompt)
        if (!is.null(fail_on) && grepl(fail_on, prompt, fixed = TRUE)) {
          error <- simpleError("concurrency test provider failure")
          class(error) <- c("concurrency_test_provider_error", class(error))
          stop(error)
        }
        response <- list(answer = paste0("ok:", prompt))
        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(
              contents = list(ellmer::ContentText(prompt))
            ),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText("ok")),
              tokens = c(2L, 1L, 0L),
              cost = 0.001,
              duration = 0.01
            )
          )
        )
        response
      }
    ),
    class = "Chat"
  )
}

concurrency_test_slow_chat <- function(
  delay,
  marker = NULL,
  fail_on = NULL
) {
  force(delay)
  force(marker)
  force(fail_on)
  structure(
    list(
      chat_structured = function(prompt, ...) {
        prompt <- as.character(prompt)
        if (!is.null(fail_on) && grepl(fail_on, prompt, fixed = TRUE)) {
          error <- simpleError("concurrency test provider failure")
          class(error) <- c("concurrency_test_provider_error", class(error))
          stop(error)
        }
        Sys.sleep(delay)
        if (!is.null(marker)) {
          file.create(marker)
        }
        list(answer = paste0("ok:", prompt))
      }
    ),
    class = "Chat"
  )
}

concurrency_peak <- function(result) {
  metadata <- lapply(result, `[[`, "metadata")
  ended <- vapply(metadata, function(x) as.numeric(x$timestamp), numeric(1))
  started <- ended -
    vapply(
      metadata,
      function(x) x$latency_ms / 1000,
      numeric(1)
    )
  events <- data.frame(
    time = c(started, ended),
    delta = c(rep(1L, length(started)), rep(-1L, length(ended)))
  )
  events <- events[order(events$time, events$delta), , drop = FALSE]
  max(cumsum(events$delta))
}

test_that("concurrency_control validates every numeric limit", {
  invalid <- list(
    list(max_active = -Inf),
    list(max_active = NaN),
    list(task_timeout = -Inf),
    list(task_timeout = NaN),
    list(total_timeout = -Inf),
    list(total_timeout = NaN),
    list(max_errors = -Inf),
    list(max_errors = NaN)
  )

  for (arguments in invalid) {
    expect_error(
      do.call(concurrency_control, arguments),
      class = "dsprrr_concurrency_config_error"
    )
  }
  expect_error(
    concurrency_control(max_active = 1.5),
    class = "dsprrr_concurrency_config_error"
  )
  expect_error(
    concurrency_control(task_timeout = 0),
    class = "dsprrr_concurrency_config_error"
  )
  expect_error(
    concurrency_control(cancel = NA),
    class = "dsprrr_concurrency_config_error"
  )

  control <- concurrency_control(
    max_active = 2,
    task_timeout = Inf,
    total_timeout = Inf,
    max_errors = Inf
  )
  expect_s3_class(control, "dsprrr_concurrency_control")
  expect_identical(control$max_active, 2L)

  before <- dsprrr:::concurrency_elapsed()
  Sys.sleep(0.001)
  after <- dsprrr:::concurrency_elapsed()
  expect_gte(after, before)
})

test_that("explicit controls conflict only with supplied legacy arguments", {
  mod <- module(signature("text -> answer"), type = "predict")
  control <- concurrency_control(backend = "sequential", max_active = 3L)

  result <- run(
    mod,
    text = c("a", "b"),
    .llm = concurrency_test_chat(),
    .concurrency = control,
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )
  expect_identical(result[[1]]$metadata$requested_workers, 3L)
  expect_identical(result[[1]]$metadata$effective_workers, 1L)

  expect_error(
    run(
      mod,
      text = c("a", "b"),
      .llm = concurrency_test_chat(),
      .concurrency = control,
      .parallel = FALSE
    ),
    class = "dsprrr_concurrency_argument_conflict"
  )
})

test_that("run_dataset forwards explicit concurrency without legacy conflicts", {
  mod <- module(signature("text -> answer"), type = "predict")
  result <- run_dataset(
    mod,
    data.frame(text = c("a", "b")),
    .llm = concurrency_test_chat(),
    .concurrency = concurrency_control(
      backend = "sequential",
      max_active = 4L
    ),
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(result$.metadata[[1]]$requested_workers, 4L)
  expect_identical(result$.metadata[[1]]$effective_workers, 1L)
  expect_identical(result$.metadata[[1]]$requested_backend, "sequential")
})

test_that("evaluate forwards explicit concurrency without injecting legacy flags", {
  mod <- module(signature("text -> answer"), type = "predict")
  control <- concurrency_control(backend = "sequential", max_active = 3L)
  result <- evaluate(
    mod,
    data = data.frame(text = c("a", "b"), expected = c("x", "y")),
    metric = function(prediction, expected_row) 1,
    .llm = concurrency_test_chat(),
    .concurrency = control,
    .progress = FALSE
  )

  expect_equal(result$mean_score, 1)
  expect_identical(result$metadata[[1]]$requested_workers, 3L)
  expect_identical(result$metadata[[1]]$effective_workers, 1L)
  expect_error(
    evaluate(
      mod,
      data = data.frame(text = c("a", "b")),
      metric = function(prediction, expected_row) 1,
      .llm = concurrency_test_chat(),
      .concurrency = control,
      .parallel = FALSE
    ),
    class = "dsprrr_concurrency_argument_conflict"
  )
})

test_that("auto is the only backend that falls back and records why", {
  testthat::local_mocked_bindings(
    concurrency_backend_available = function(backend) {
      identical(backend, "sequential")
    },
    .package = "dsprrr"
  )
  mod <- module(signature("text -> answer"), type = "predict")
  chat <- concurrency_test_chat()
  result <- run(
    mod,
    text = c("a", "b"),
    .llm = chat,
    .concurrency = concurrency_control(
      backend = "auto",
      max_active = 3L
    ),
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(result[[1]]$metadata$requested_backend, "auto")
  expect_identical(result[[1]]$metadata$effective_backend, "sequential")
  expect_identical(result[[1]]$metadata$requested_workers, 3L)
  expect_identical(result[[1]]$metadata$effective_workers, 1L)
  expect_match(
    result[[1]]$metadata$fallback_reason,
    "unavailable|trusted ellmer"
  )

  expect_error(
    run(
      mod,
      text = c("a", "b"),
      .llm = chat,
      .concurrency = concurrency_control(backend = "ellmer")
    ),
    class = "dsprrr_concurrency_backend_unavailable"
  )
})

test_that("auto uses sequential execution for one worker and opaque Chats", {
  ellmer_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      ellmer_calls <<- ellmer_calls + 1L
      stop("native ellmer should not run")
    },
    .package = "ellmer"
  )
  mod <- module(signature("text -> answer"), type = "predict")

  one <- run(
    mod,
    text = c("a", "b"),
    .llm = concurrency_test_chat(),
    .concurrency = concurrency_control(backend = "auto", max_active = 1L),
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )
  opaque <- run(
    mod,
    text = c("c", "d"),
    .llm = concurrency_test_chat(),
    .concurrency = concurrency_control(backend = "auto", max_active = 3L),
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(ellmer_calls, 0L)
  expect_true(all(vapply(
    one,
    function(row) identical(row$metadata$effective_backend, "sequential"),
    logical(1)
  )))
  expect_match(one[[1]]$metadata$fallback_reason, "max_active = 1")
  expect_true(all(vapply(
    opaque,
    function(row) identical(row$metadata$effective_backend, "sequential"),
    logical(1)
  )))
  expect_match(opaque[[1]]$metadata$fallback_reason, "trusted ellmer")
})

test_that("ellmer rejects unenforceable timeouts before provider work", {
  calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      calls <<- calls + 1L
      stop("should not run")
    },
    .package = "ellmer"
  )
  mod <- module(signature("text -> answer"), type = "predict")
  expect_error(
    run(
      mod,
      text = c("a", "b"),
      .llm = concurrency_test_chat(),
      .concurrency = concurrency_control(
        backend = "ellmer",
        total_timeout = 1
      )
    ),
    class = "dsprrr_concurrency_unsupported_error"
  )
  expect_identical(calls, 0L)
})

test_that("ellmer receives the exact requested max_active for N one and two", {
  observed <- list()
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(
      chat,
      prompts,
      type,
      max_active,
      on_error,
      ...
    ) {
      observed[[length(observed) + 1L]] <<- c(
        max_active = max_active,
        rows = length(prompts)
      )
      tibble::tibble(
        answer = seq_along(prompts),
        .error = rep(list(NULL), length(prompts))
      )
    },
    .package = "ellmer"
  )

  for (workers in 1:2) {
    mod <- module(signature("text -> answer"), type = "predict")
    run(
      mod,
      text = c("a", "b", "c"),
      .llm = concurrency_test_chat(),
      .concurrency = concurrency_control(
        backend = "ellmer",
        max_active = workers
      ),
      .return_format = "structured",
      .progress = FALSE,
      .cache = FALSE
    )
  }

  expect_identical(
    vapply(observed[1:3], `[[`, numeric(1), "max_active"),
    rep(1, 3)
  )
  expect_identical(
    vapply(observed[4:5], `[[`, numeric(1), "max_active"),
    rep(2, 2)
  )
  expect_identical(
    vapply(observed, `[[`, numeric(1), "rows"),
    c(1, 1, 1, 2, 1)
  )
})

test_that("ellmer error budgets stop later bounded waves", {
  calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(prompts, max_active, ...) {
      calls <<- calls + 1L
      tibble::tibble(
        answer = c(NA_character_, "second"),
        .error = list(simpleError("ellmer wave failed"), NULL)
      )
    },
    .package = "ellmer"
  )
  mod <- module(signature("text -> answer"), type = "predict")
  expect_warning(
    result <- run(
      mod,
      text = letters[1:5],
      .llm = concurrency_test_chat(),
      .concurrency = concurrency_control(
        backend = "ellmer",
        max_active = 2L,
        max_errors = 0L
      ),
      .return_format = "structured",
      .progress = FALSE,
      .cache = FALSE
    ),
    "error budget"
  )

  expect_identical(calls, 1L)
  expect_match(result[[1]]$metadata$error, "ellmer wave failed")
  expect_equal(result[[2]]$output$answer, "second")
  expect_true(all(vapply(
    result[3:5],
    function(row) identical(row$metadata$cancellation_reason, "max_errors"),
    logical(1)
  )))
  expect_identical(
    vapply(result, function(row) row$metadata$batch_index, integer(1)),
    1:5
  )
})

test_that("specialized Predict batches reject controls before topology work", {
  profiles <- 0L
  testthat::local_mocked_bindings(
    new_dsprrr_mirai_profile = function() {
      profiles <<- profiles + 1L
      "should-not-exist"
    },
    .package = "dsprrr"
  )
  mod <- module(signature("question -> answer"), type = "react")
  expect_error(
    run(
      mod,
      question = c("a", "b"),
      .concurrency = concurrency_control(backend = "mirai", max_active = 2L)
    ),
    class = "dsprrr_batch_unsupported_module"
  )
  expect_identical(profiles, 0L)
})

test_that("generic Module batches reject concurrency before forward work", {
  ProbeModule <- R6::R6Class(
    "ConcurrencyProbeModule",
    inherit = dsprrr:::Module,
    public = list(
      calls = 0L,
      initialize = function() {
        super$initialize(signature("text -> answer"))
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$calls <- self$calls + 1L
        tibble::tibble(
          output = list("unexpected"),
          chat = list(NULL),
          metadata = list(list())
        )
      }
    )
  )
  control <- concurrency_control(backend = "ellmer", max_active = 2L)

  mod <- ProbeModule$new()
  expect_error(
    run(mod, text = c("a", "b"), .concurrency = control),
    class = "dsprrr_batch_unsupported_module"
  )
  expect_identical(mod$calls, 0L)
  expect_length(mod$state$traces, 0L)

  direct <- ProbeModule$new()
  expect_error(
    direct$run(text = c("a", "b"), .concurrency = control),
    class = "dsprrr_batch_unsupported_module"
  )
  expect_identical(direct$calls, 0L)
  expect_length(direct$state$traces, 0L)
})

test_that("mirai enforces actual peak concurrency for N one and two", {
  skip_if_not_installed("mirai")
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")

  run_with <- function(workers) {
    mod <- module(signature("text -> answer"), type = "predict")
    mod$chat <- concurrency_test_slow_chat(delay = 0.12)
    run(
      mod,
      text = c("a", "b", "c", "d"),
      .concurrency = concurrency_control(
        backend = "mirai",
        max_active = workers,
        total_timeout = 5
      ),
      .return_format = "structured",
      .progress = FALSE,
      .cache = FALSE
    )
  }

  one <- run_with(1L)
  two <- run_with(2L)
  expect_identical(concurrency_peak(one), 1L)
  expect_identical(concurrency_peak(two), 2L)
  expect_true(all(vapply(
    two,
    function(row) identical(row$metadata$effective_workers, 2L),
    logical(1)
  )))
})

test_that("mirai preserves the user-owned default topology", {
  skip_if_not_installed("mirai")
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")

  before <- mirai::status()$connections
  created_default <- identical(before, 0L)
  if (created_default) {
    mirai::daemons(1L)
    withr::defer(mirai::daemons(0L, sync = TRUE))
    before <- mirai::status()$connections
  }

  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- concurrency_test_chat()
  run(
    mod,
    text = c("a", "b"),
    .concurrency = concurrency_control(
      backend = "mirai",
      max_active = 2L,
      total_timeout = 5
    ),
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(mirai::status()$connections, before)
  probe <- mirai::mirai(42L)
  while (mirai::unresolved(probe)) {
    Sys.sleep(0.005)
  }
  expect_identical(probe[["data"]], 42L)
})

test_that("mirai refuses to claim an occupied named profile", {
  skip_if_not_installed("mirai")
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")

  profile <- paste0(
    "dsprrr-user-owned-",
    Sys.getpid(),
    "-",
    basename(tempfile())
  )
  mirai::daemons(1L, .compute = profile)
  withr::defer(mirai::daemons(0L, sync = TRUE, .compute = profile))
  deadline <- dsprrr:::concurrency_elapsed() + 5
  while (
    mirai::status(.compute = profile)$connections < 1L &&
      dsprrr:::concurrency_elapsed() < deadline
  ) {
    Sys.sleep(0.005)
  }
  before <- mirai::status(.compute = profile)$connections
  expect_identical(before, 1L)
  testthat::local_mocked_bindings(
    new_dsprrr_mirai_profile = function(...) profile,
    .package = "dsprrr"
  )

  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- concurrency_test_chat()
  expect_error(
    run(
      mod,
      text = c("a", "b"),
      .concurrency = concurrency_control(
        backend = "mirai",
        max_active = 1L,
        total_timeout = 5
      ),
      .progress = FALSE,
      .cache = FALSE
    ),
    class = "dsprrr_mirai_profile_collision"
  )
  expect_identical(mirai::status(.compute = profile)$connections, before)
})

test_that("unexpected scheduler aborts drain owned profiles observably", {
  skip_if_not_installed("mirai")
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")

  before <- mirai::status()$connections
  if (identical(before, 0L)) {
    mirai::daemons(1L)
    withr::defer(mirai::daemons(0L, sync = TRUE))
    before <- mirai::status()$connections
  }
  profile <- paste0("dsprrr-forced-abort-", Sys.getpid())
  original_shutdown <- dsprrr:::shutdown_dsprrr_mirai_profile
  testthat::local_mocked_bindings(
    new_dsprrr_mirai_profile = function() profile,
    shutdown_dsprrr_mirai_profile = function(...) {
      original_shutdown(...)
      FALSE
    },
    .package = "dsprrr"
  )
  testthat::local_mocked_bindings(
    mirai = function(...) stop("forced scheduler launch failure"),
    .package = "mirai"
  )

  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- concurrency_test_chat()
  warnings <- list()
  error <- tryCatch(
    withCallingHandlers(
      run(
        mod,
        text = c("a", "b"),
        .concurrency = concurrency_control(
          backend = "mirai",
          max_active = 1L,
          total_timeout = 5
        ),
        .progress = FALSE,
        .cache = FALSE
      ),
      warning = function(warning) {
        warnings <<- c(warnings, list(warning))
        invokeRestart("muffleWarning")
      }
    ),
    error = identity
  )

  expect_s3_class(error, "error")
  expect_match(conditionMessage(error), "forced scheduler launch failure")
  expect_true(any(vapply(
    warnings,
    inherits,
    logical(1),
    what = "dsprrr_mirai_teardown_warning"
  )))
  expect_identical(mirai::status()$connections, before)
  expect_identical(dsprrr:::mirai_profile_is_drained(profile), TRUE)
})

test_that("mirai cleanup completes before warnings become errors", {
  skip_if_not_installed("mirai")
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")

  profile <- paste0("dsprrr-warn2-", Sys.getpid(), "-", basename(tempfile()))
  testthat::local_mocked_bindings(
    new_dsprrr_mirai_profile = function(...) profile,
    .package = "dsprrr"
  )
  withr::local_options(list(warn = 2L))
  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- concurrency_test_slow_chat(delay = 0.01, fail_on = "FAIL")

  expect_error(
    run(
      mod,
      text = c("FAIL", "success"),
      .concurrency = concurrency_control(
        backend = "mirai",
        max_active = 1L,
        total_timeout = 5
      ),
      .progress = FALSE,
      .cache = FALSE
    ),
    class = "error"
  )
  expect_identical(dsprrr:::mirai_profile_is_drained(profile), TRUE)
})

test_that("mirai task timeouts halt work before return", {
  skip_if_not_installed("mirai")
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")
  marker <- withr::local_tempfile()
  unlink(marker)
  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- concurrency_test_slow_chat(delay = 0.25, marker = marker)

  expect_warning(
    result <- run(
      mod,
      text = c("a", "b"),
      .concurrency = concurrency_control(
        backend = "mirai",
        max_active = 1L,
        task_timeout = 0.03,
        total_timeout = 2
      ),
      .return_format = "structured",
      .progress = FALSE,
      .cache = FALSE
    ),
    "timed out"
  )

  Sys.sleep(0.3)
  expect_false(file.exists(marker))
  expect_true(all(vapply(
    result,
    function(row) {
      identical(row$metadata$error_class, "dsprrr_mirai_timeout_error") &&
        identical(row$metadata$cancellation_reason, "task_timeout") &&
        identical(row$metadata$effective_workers, 1L) &&
        row$metadata$latency_ms >= 20 &&
        row$metadata$latency_ms < 500
    },
    logical(1)
  )))
})

test_that("mirai total timeouts cancel active and quarantine queued work", {
  skip_if_not_installed("mirai")
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")
  marker <- withr::local_tempfile()
  unlink(marker)
  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- concurrency_test_slow_chat(delay = 0.25, marker = marker)

  expect_warning(
    result <- run(
      mod,
      text = c("a", "b"),
      .concurrency = concurrency_control(
        backend = "mirai",
        max_active = 1L,
        total_timeout = 0.03
      ),
      .return_format = "structured",
      .progress = FALSE,
      .cache = FALSE
    ),
    "timed out"
  )

  Sys.sleep(0.3)
  expect_false(file.exists(marker))
  expect_true(all(vapply(
    result,
    function(row) {
      identical(row$metadata$error_class, "dsprrr_mirai_timeout_error") &&
        identical(row$metadata$cancellation_reason, "total_timeout")
    },
    logical(1)
  )))
})

test_that("mirai error budgets cancel active work and preserve row order", {
  skip_if_not_installed("mirai")
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")
  marker <- withr::local_tempfile()
  unlink(marker)
  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- concurrency_test_slow_chat(
    fail_on = "FAIL",
    delay = 0.35,
    marker = marker
  )

  expect_warning(
    result <- run(
      mod,
      text = c("FAIL", "active", "queued"),
      .concurrency = concurrency_control(
        backend = "mirai",
        max_active = 2L,
        max_errors = 0L,
        total_timeout = 5,
        cancel = TRUE
      ),
      .return_format = "structured",
      .progress = FALSE,
      .cache = FALSE
    ),
    "error budget"
  )

  Sys.sleep(0.4)
  expect_false(file.exists(marker))
  expect_identical(
    result[[1]]$metadata$error_class,
    "concurrency_test_provider_error"
  )
  expect_true(all(vapply(
    result[2:3],
    function(row) identical(row$metadata$cancellation_reason, "max_errors"),
    logical(1)
  )))
  expect_identical(
    vapply(result, function(row) row$metadata$batch_index, integer(1)),
    1:3
  )
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:3
  )
})

test_that("mirai cancel false drains active work and quarantines queued rows", {
  skip_if_not_installed("mirai")
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")
  marker <- withr::local_tempfile()
  unlink(marker)
  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- concurrency_test_slow_chat(
    fail_on = "FAIL",
    delay = 0.12,
    marker = marker
  )

  expect_warning(
    result <- run(
      mod,
      text = c("FAIL", "active", "queued"),
      .concurrency = concurrency_control(
        backend = "mirai",
        max_active = 2L,
        max_errors = 0L,
        total_timeout = 5,
        cancel = FALSE
      ),
      .return_format = "structured",
      .progress = FALSE,
      .cache = FALSE
    ),
    "error budget"
  )

  expect_true(file.exists(marker))
  expect_identical(
    result[[1]]$metadata$error_class,
    "concurrency_test_provider_error"
  )
  expect_true(is.na(result[[2]]$metadata$error))
  expect_false(result[[2]]$metadata$cancelled)
  expect_identical(result[[3]]$metadata$cancellation_reason, "max_errors")
  expect_true(result[[3]]$metadata$cancelled)
  expect_identical(
    vapply(result, function(row) row$metadata$batch_index, integer(1)),
    1:3
  )
})
