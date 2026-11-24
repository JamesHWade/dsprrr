test_that("batch planning preserves input order and row boundaries", {
  inputs <- list(
    text = c("first", "second"),
    locale = c("en", "en")
  )

  plan <- dsprrr:::plan_batch_execution(inputs, n = 2L)

  expect_identical(plan$n, 2L)
  expect_identical(
    plan$input_sets,
    list(
      list(text = "first", locale = "en"),
      list(text = "second", locale = "en")
    )
  )
  expect_identical(inputs$locale, c("en", "en"))
})

test_that("ellmer phases preserve response order, telemetry, and errors", {
  skip_if_not(
    exists(
      "parallel_chat_structured",
      envir = asNamespace("ellmer"),
      inherits = FALSE
    ),
    "ellmer native batching is unavailable"
  )

  observed <- new.env(parent = emptyenv())
  observed$calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(
      chat,
      prompts,
      type,
      include_tokens,
      include_cost,
      on_error,
      ...
    ) {
      observed$calls <- observed$calls + 1L
      expect_identical(on_error, "continue")
      tibble::tibble(
        answer = c("one", NA_character_),
        input_tokens = c(10L, 0L),
        output_tokens = c(2L, 0L),
        .error = list(NULL, simpleError("second failed"))
      )
    },
    .package = "ellmer"
  )

  mod <- module(signature("text -> answer"), template = "{text}")
  chat <- new_test_chat()
  runtime <- dsprrr:::normalize_concurrency_runtime(
    concurrency_control(backend = "ellmer", max_active = 2L),
    .chat = chat
  )
  plan <- dsprrr:::plan_ellmer_wave(
    module = mod,
    input_sets = list(list(text = "a"), list(text = "b")),
    n = 2L,
    .llm = chat,
    .concurrency = runtime,
    .batch_indices = c(2L, 4L)
  )

  expect_identical(observed$calls, 0L)
  call <- dsprrr:::call_ellmer_wave(plan)
  decoded <- dsprrr:::decode_ellmer_wave(plan, call)
  evaluated <- dsprrr:::evaluate_ellmer_wave(
    plan = plan,
    call = call,
    decoded = decoded,
    .return_format = "structured"
  )
  results <- dsprrr:::collect_backend_traces(evaluated)

  expect_identical(observed$calls, 1L)
  expect_equal(results[[1]]$output$answer, "one")
  expect_identical(results[[1]]$metadata$batch_index, 2L)
  expect_identical(results[[1]]$metadata$total_tokens, 12L)
  expect_true(is.na(results[[2]]$output))
  expect_identical(results[[2]]$metadata$batch_index, 4L)
  expect_match(results[[2]]$metadata$error, "second failed")
  expect_length(attr(results, "dsprrr_traces"), 2L)
})

test_that("mirai planning resolves requests without starting a worker pool", {
  skip_if_not_installed("mirai")
  mod <- module(signature("text -> answer"), template = "{text}")
  chat <- new_test_chat()
  runtime <- dsprrr:::normalize_concurrency_runtime(
    concurrency_control(backend = "mirai", max_active = 1L),
    .chat = chat
  )

  plan <- dsprrr:::plan_mirai_batch(
    module = mod,
    input_sets = list(list(text = "hello")),
    n = 1L,
    .llm = chat,
    .concurrency = runtime
  )

  expect_identical(plan$requests[[1]]$prompt, "hello")
  expect_identical(plan$n, 1L)
  expect_identical(plan$concurrency, runtime)
  expect_true(dsprrr:::mirai_profile_is_unoccupied(plan$compute_profile))
})

test_that("mirai worker records validate at the process boundary", {
  started <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  ended <- started + 1
  record <- list(
    ok = TRUE,
    response = "answer",
    started_at = started,
    ended_at = ended,
    usage = list(
      input_tokens = 3L,
      output_tokens = 2L,
      cached_input_tokens = 0L,
      total_tokens = 5L,
      cost = 0.01,
      duration_s = 1
    ),
    usage_verified = TRUE,
    model = "test-model",
    turns_before = list()
  )

  validated <- dsprrr:::validate_mirai_worker_record(
    record,
    fallback_started_at = started,
    fallback_ended_at = ended
  )

  expect_null(validated$error)
  expect_identical(validated$record$response, "answer")
  expect_identical(validated$usage$total_tokens, 5L)
  expect_identical(validated$model, "test-model")
})

test_that("mirai worker phase returns a serializable provider record", {
  chat <- new_test_chat(
    chat_structured = function(...) "answer"
  )
  record <- dsprrr:::mirai_batch_worker(
    i = 1L,
    requests = list(list(payload = "hello")),
    output_type = ellmer::type_string(),
    llm_template = chat
  )

  expect_true(record$ok)
  expect_identical(record$response, "answer")
  expect_true(is.list(record$usage))
  expect_identical(record$usage_verified, FALSE)
})
