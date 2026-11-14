reasoning_test_chat <- function(
  response,
  thinking = NULL,
  thinking_extra = list(),
  record_turns = TRUE
) {
  state <- new.env(parent = emptyenv())
  state$turns <- list()
  state$calls <- 0L
  state$output_type <- NULL

  chat <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      state$calls <- state$calls + 1L
      state$output_type <- type
      contents <- list(ellmer::ContentText("provider response"))
      if (!is.null(thinking)) {
        contents <- c(
          list(ellmer::ContentThinking(thinking, thinking_extra)),
          contents
        )
      }
      if (isTRUE(record_turns)) {
        state$turns <- c(
          state$turns,
          list(
            ellmer::UserTurn(
              contents = list(ellmer::ContentText(as.character(prompt)))
            ),
            ellmer::AssistantTurn(contents = contents)
          )
        )
      }
      response
    },
    get_turns = function() state$turns,
    set_turns = function(turns) {
      state$turns <- turns
      invisible(NULL)
    },
    last_turn = function(role = NULL) {
      turns <- if (is.null(role)) {
        state$turns
      } else if (identical(role, "assistant")) {
        Filter(
          function(turn) inherits(turn, "ellmer::AssistantTurn"),
          state$turns
        )
      } else {
        Filter(
          function(turn) inherits(turn, "ellmer::UserTurn"),
          state$turns
        )
      }
      if (length(turns) == 0L) NULL else turns[[length(turns)]]
    },
    get_model = function() "reasoning-test-model"
  )

  list(chat = chat, state = state)
}

test_that("chain_of_thought keeps rationale as the default and adds plans", {
  rationale <- chain_of_thought("question -> answer")
  native <- chain_of_thought("question -> answer", mode = "native")
  plan <- chain_of_thought("question -> answer", mode = "plan")

  expect_identical(rationale$config$reasoning_mode, "rationale")
  expect_identical(
    names(rationale$signature@output_type@properties),
    c("reasoning", "answer")
  )
  expect_identical(native$config$reasoning_mode, "native")
  expect_false(has_reasoning(native$signature))
  expect_identical(
    names(native$signature@output_type@properties),
    "answer"
  )
  expect_identical(plan$config$reasoning_mode, "plan")
  expect_identical(
    names(plan$signature@output_type@properties),
    c("plan", "answer")
  )
  expect_match(
    plan$signature@instructions,
    "no more than three high-level steps"
  )
})

test_that("reasoning modes reject reserved output fields and invalid config", {
  rationale_collision <- rlang::catch_cnd(
    chain_of_thought("question -> reasoning, answer")
  )
  plan_collision <- rlang::catch_cnd(
    chain_of_thought("question -> plan, answer", mode = "plan")
  )
  invalid_mode <- rlang::catch_cnd(
    chain_of_thought("question -> answer", mode = "extended")
  )
  invalid_config <- rlang::catch_cnd(
    module(
      signature("question -> answer"),
      config = list(reasoning_mode = "extended")
    )
  )

  expect_s3_class(rationale_collision, "dsprrr_signature_transform_error")
  expect_s3_class(plan_collision, "dsprrr_signature_transform_error")
  expect_s3_class(invalid_mode, "dsprrr_reasoning_mode_error")
  expect_s3_class(invalid_config, "dsprrr_reasoning_mode_error")
})

test_that("native mode captures only provider ContentThinking metadata", {
  local_reset_cache()
  mock <- reasoning_test_chat(
    response = list(answer = "42"),
    thinking = "provider thinking block"
  )
  solver <- chain_of_thought("question -> answer", mode = "native")

  result <- run(
    solver,
    question = "What is 6 times 7?",
    .llm = mock$chat,
    .cache = FALSE,
    .return_format = "structured"
  )

  expect_identical(get_output(result), list(answer = "42"))
  expect_identical(names(mock$state$output_type@properties), "answer")
  expect_identical(get_metadata(result)$reasoning_mode, "native")
  native_reasoning <- get_metadata(result)$native_reasoning
  expect_length(native_reasoning, 1L)
  expect_s3_class(native_reasoning[[1]], "ellmer::ContentThinking")
  expect_identical(native_reasoning[[1]]@thinking, "provider thinking block")
})

test_that("native mode returns NULL when the provider exposes no thinking", {
  local_reset_cache()
  mock <- reasoning_test_chat(response = list(answer = "42"))
  solver <- chain_of_thought("question -> answer", mode = "native")

  result <- run(
    solver,
    question = "What is 6 times 7?",
    .llm = mock$chat,
    .cache = FALSE,
    .return_format = "structured"
  )

  expect_null(get_metadata(result)$native_reasoning)
  expect_identical(get_output(result), list(answer = "42"))
})

test_that("native mode excludes stale thinking when no current turn is recorded", {
  local_reset_cache()
  mock <- reasoning_test_chat(
    response = list(answer = "42"),
    record_turns = FALSE
  )
  mock$state$turns <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("old question"))),
    ellmer::AssistantTurn(
      contents = list(
        ellmer::ContentThinking("stale provider thought", list())
      )
    )
  )
  solver <- chain_of_thought("question -> answer", mode = "native")

  result <- run(
    solver,
    question = "What is 6 times 7?",
    .llm = mock$chat,
    .cache = FALSE,
    .return_format = "structured"
  )

  expect_null(get_metadata(result)$native_reasoning)
})

test_that("native mode extracts current thinking blocks and extra metadata", {
  local_reset_cache()
  mock <- reasoning_test_chat(
    response = list(answer = "42"),
    thinking = "current provider thought",
    thinking_extra = list(provider = "mock-provider")
  )
  mock$state$turns <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("old question"))),
    ellmer::AssistantTurn(
      contents = list(
        ellmer::ContentThinking("stale provider thought", list())
      )
    )
  )
  solver <- chain_of_thought("question -> answer", mode = "native")

  result <- run(
    solver,
    question = "What is 6 times 7?",
    .llm = mock$chat,
    .cache = FALSE,
    .return_format = "structured"
  )
  native_reasoning <- get_metadata(result)$native_reasoning

  expect_length(native_reasoning, 1L)
  expect_identical(
    native_reasoning[[1]]@thinking,
    "current provider thought"
  )
  expect_identical(
    native_reasoning[[1]]@extra,
    list(provider = "mock-provider")
  )
})

test_that("plan and native outputs remain consumable by wrappers", {
  plan_mock <- reasoning_test_chat(
    response = list(plan = "Multiply 6 by 7.", answer = "42")
  )
  plan_solver <- chain_of_thought("question -> answer", mode = "plan")
  plan_wrapped <- run(
    best_of_n(plan_solver, N = 1L),
    question = "What is 6 times 7?",
    .llm = plan_mock$chat,
    .cache = FALSE
  )

  native_mock <- reasoning_test_chat(
    response = list(answer = "42"),
    thinking = "provider thought"
  )
  native_solver <- chain_of_thought("question -> answer", mode = "native")
  native_wrapped <- run(
    best_of_n(native_solver, N = 1L),
    question = "What is 6 times 7?",
    .llm = native_mock$chat,
    .cache = FALSE
  )

  expect_identical(names(plan_wrapped), c("plan", "answer"))
  expect_identical(names(native_wrapped), "answer")
})

test_that("plan mode keeps its field in batch and async structured calls", {
  local_reset_cache()
  plan_mock <- reasoning_test_chat(
    response = list(plan = "Multiply 6 by 7.", answer = "42")
  )
  solver <- chain_of_thought("question -> answer", mode = "plan")

  batch <- run(
    solver,
    question = c("What is 6 times 7?", "What is 7 times 6?"),
    .llm = plan_mock$chat,
    .cache = FALSE,
    .progress = FALSE,
    .return_format = "structured"
  )
  expect_length(batch, 2L)
  expect_true(all(vapply(
    get_output(batch),
    function(output) identical(names(output), c("plan", "answer")),
    logical(1)
  )))
  expect_true(all(vapply(
    get_metadata(batch),
    function(metadata) identical(metadata$reasoning_mode, "plan"),
    logical(1)
  )))

  plan_mock$chat$chat_structured_async <- function(prompt, type, ...) {
    plan_mock$state$output_type <- type
    list(plan = "Multiply 6 by 7.", answer = "42")
  }
  async <- run_async(
    solver,
    question = "What is 6 times 7?",
    .llm = plan_mock$chat
  )
  expect_identical(names(async), c("plan", "answer"))
  expect_identical(
    names(plan_mock$state$output_type@properties),
    c("plan", "answer")
  )
})

test_that("native and plan modes fail closed on unsupported routes", {
  mock <- reasoning_test_chat(response = list(answer = "42"))
  native <- chain_of_thought("question -> answer", mode = "native")
  plan <- chain_of_thought("question -> answer", mode = "plan")

  batch_error <- rlang::catch_cnd(run(
    native,
    question = c("first", "second"),
    .llm = mock$chat,
    .cache = FALSE
  ))
  dataset_error <- rlang::catch_cnd(run_dataset(
    native,
    data.frame(question = c("first", "second")),
    .llm = mock$chat,
    .cache = FALSE
  ))
  direct_batch_error <- rlang::catch_cnd(native$run(
    question = c("first", "second"),
    .llm = mock$chat,
    .cache = FALSE
  ))
  async_error <- rlang::catch_cnd(run_async(
    native,
    question = "first",
    .llm = mock$chat
  ))
  stream_error <- rlang::catch_cnd(stream_async(
    native,
    question = "first",
    .llm = mock$chat
  ))
  run_stream_error <- rlang::catch_cnd(run_stream(
    native,
    question = "first",
    .llm = mock$chat
  ))
  plan_stream_error <- rlang::catch_cnd(stream_async(
    plan,
    question = "first",
    .llm = mock$chat
  ))

  expect_s3_class(batch_error, "dsprrr_reasoning_batch_unsupported")
  expect_s3_class(dataset_error, "dsprrr_reasoning_batch_unsupported")
  expect_s3_class(direct_batch_error, "dsprrr_reasoning_batch_unsupported")
  expect_s3_class(async_error, "dsprrr_reasoning_metadata_unsupported")
  expect_s3_class(stream_error, "dsprrr_reasoning_metadata_unsupported")
  expect_s3_class(run_stream_error, "dsprrr_reasoning_metadata_unsupported")
  expect_s3_class(plan_stream_error, "dsprrr_reasoning_metadata_unsupported")
  expect_identical(mock$state$calls, 0L)
})

test_that("reasoning mode survives program artifacts and affects identity", {
  native <- chain_of_thought("question -> answer", mode = "native")
  plan <- chain_of_thought("question -> answer", mode = "plan")
  native_artifact <- program_artifact(native)
  plan_artifact <- program_artifact(plan)
  restored <- restore_module_config(native_artifact)

  expect_identical(restored$config$reasoning_mode, "native")
  expect_false(has_reasoning(restored$signature))
  expect_identical(
    program_artifact_id(restored),
    program_artifact_id(native_artifact)
  )
  expect_false(identical(
    program_artifact_id(native_artifact),
    program_artifact_id(plan_artifact)
  ))

  malformed <- native_artifact
  malformed$graph$nodes[[malformed$root]]$config$reasoning_mode <- "extended"
  malformed$integrity <- dsprrr:::artifact_integrity(malformed)
  restore_error <- rlang::catch_cnd(restore_module_config(malformed))
  expect_s3_class(restore_error, "dsprrr_artifact_malformed")
  expect_s3_class(restore_error$parent, "dsprrr_reasoning_mode_error")
})

test_that("cache fingerprints follow the provider request schema", {
  chat <- suppressWarnings(ellmer::chat_openai(
    api_key = "dummy-key",
    model = "model-a"
  ))
  plain <- module(signature("question -> answer"))
  native <- chain_of_thought("question -> answer", mode = "native")
  rationale <- chain_of_thought("question -> answer")
  plan <- chain_of_thought("question -> answer", mode = "plan")

  fingerprint <- function(mod) {
    request <- dsprrr:::build_module_request(
      mod,
      list(question = "What is 6 times 7?")
    )
    dsprrr:::cache_request_fingerprint(
      chat,
      request$payload,
      mod$signature@output_type
    )
  }

  expect_identical(
    dsprrr:::cache_fingerprint_json(fingerprint(plain)),
    dsprrr:::cache_fingerprint_json(fingerprint(native))
  )
  expect_false(identical(
    dsprrr:::cache_fingerprint_json(fingerprint(native)),
    dsprrr:::cache_fingerprint_json(fingerprint(rationale))
  ))
  expect_false(identical(
    dsprrr:::cache_fingerprint_json(fingerprint(native)),
    dsprrr:::cache_fingerprint_json(fingerprint(plan))
  ))
})
