# Contract tests for the explicit RLM invocation phases.

rlm_phase_test_runner <- function() {
  list(
    execute = function(code, context = list()) {
      list(
        success = TRUE,
        result = NULL,
        stdout = "",
        stderr = "",
        messages = "",
        warnings = "",
        error = NULL,
        error_type = NULL,
        duration_ms = 0
      )
    },
    policy = function() {
      list(
        backend = "phase-test",
        trust = "test",
        sandboxed = FALSE,
        persistent = TRUE
      )
    }
  )
}

new_rlm_phase_test_operations <- function(chat) {
  list(
    describe_context = function(inputs) "context",
    build_system_prompt = function(...) "system",
    build_iteration_prompt = function(...) "iteration",
    get_code_response = function(...) {
      list(
        code = "SUBMIT('done')",
        reasoning = "finish",
        metadata = list(cache = "miss", provider_calls = 1L),
        chat = chat
      )
    },
    execute_with_rlm_tools = function(...) {
      list(
        success = TRUE,
        is_final = TRUE,
        final_value = "done",
        formatted_output = "[SUBMIT accepted]",
        error = NULL,
        raw_result = list(duration_ms = 1, error_type = NULL)
      )
    },
    compact_action_metadata = function(metadata) metadata,
    extract_fallback = function(...) stop("fallback should not run"),
    build_output = function(answer, source = c("submit", "fallback")) {
      list(answer = answer)
    },
    summarize_action_usage = function(...) {
      list(
        input_tokens = 1L,
        cached_input_tokens = 0L,
        output_tokens = 1L,
        total_tokens = 2L,
        cost = 0
      )
    },
    metadata_provider_calls = function(metadata) 0L,
    summarize_provider_calls = function(metadata) 1L,
    sum_provider_call_counts = function(counts) as.integer(sum(counts)),
    summarize_trace_inputs = function(inputs) {
      list(
        question = list(
          class = "character",
          length = 1L,
          bytes = 1,
          sha256 = "hash"
        )
      )
    },
    append_bounded_trace = function(records, record) {
      append(records, list(record))
    },
    rlm_model_name = function(llm) "phase-test",
    aggregate_native_reasoning = function(history) {
      list(contents = NULL, action_indices = NULL)
    }
  )
}

test_that("RLM prepare phase has an explicit, validated contract", {
  chat <- new_test_chat()
  module <- rlm_module(
    "question -> answer",
    runner = rlm_phase_test_runner(),
    chat = chat
  )

  prepared <- dsprrr:::rlm_prepare_invocation_phase(
    module,
    list(question = "inspect this"),
    .llm = chat
  )
  expect_identical(names(prepared$inputs), "question")
  expect_identical(prepared$inputs$question, "inspect this")
  expect_identical(prepared$prior_context, "")
  expect_length(module$state$traces, 0L)

  error <- tryCatch(
    dsprrr:::rlm_prepare_invocation_phase(
      module,
      list(unexpected = "field"),
      .llm = chat
    ),
    error = identity
  )
  expect_s3_class(error, "dsprrr_rlm_input_error")
})

test_that("RLM execute and finalize phases preserve trajectory and result shape", {
  chat <- new_test_chat()
  module <- rlm_module(
    "question -> answer",
    runner = rlm_phase_test_runner(),
    chat = chat,
    max_iterations = 2L,
    max_llm_calls = 0L
  )
  prepared <- dsprrr:::rlm_prepare_invocation_phase(
    module,
    list(question = "inspect this"),
    .llm = chat
  )
  lease <- list(
    owned = FALSE,
    policy = rlm_phase_test_runner()$policy(),
    policy_summary = rlm_phase_test_runner()$policy()
  )
  operations <- new_rlm_phase_test_operations(chat)

  runner <- rlm_phase_test_runner()
  session <- rlm_prepare_runner(runner, lease, prepared$inputs)
  executed <- dsprrr:::rlm_execute_invocation_phase(
    module = module,
    prepared = prepared,
    runner = runner,
    lease = lease,
    operations = operations,
    session = session,
    session_state_id = ".phase_test_state",
    start_time = Sys.time(),
    trace = TRUE
  )
  expect_length(executed$history, 1L)
  expect_identical(executed$final_source, "submit")
  expect_identical(executed$call_counter$count, 0L)

  expect_length(module$state$traces, 0L)
  result <- dsprrr:::rlm_finalize_invocation_phase(
    module = module,
    prepared = prepared,
    execution = executed,
    operations = operations,
    trace = TRUE
  )
  expect_named(result, c("output", "chat", "metadata"))
  expect_identical(result$output[[1L]]$answer, "done")
  expect_identical(result$metadata[[1L]]$output_source, "submit")
  expect_identical(result$metadata[[1L]]$iterations, 1L)
  expect_identical(module$state$trace_sequence, 1)
  expect_length(module$state$traces, 1L)
  expect_length(module$state$repl_history, 1L)
  expect_identical(module$state$traces[[1L]]$output$answer, "done")
})

test_that("RLM prepares a caller-owned interpreter before tracing or cleanup", {
  events <- character()
  runner <- rlm_phase_test_runner()
  runner$prepare_context <- function(inputs) {
    events <<- c(events, "prepare")
    stop("context rejected")
  }
  runner$execute <- function(...) {
    events <<- c(events, "execute")
  }
  chat <- new_test_chat(chat_structured = function(...) {
    events <<- c(events, "provider")
  })
  program <- rlm_module("question -> answer", runner = runner, chat = chat)

  error <- rlang::catch_cnd(program$forward(
    list(question = "inspect this"),
    trace = {
      events <<- c(events, "trace")
      TRUE
    }
  ))

  expect_identical(conditionMessage(error), "context rejected")
  expect_identical(events, "prepare")
  expect_length(program$get_repl_history(), 0L)
})
