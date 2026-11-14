rlm_reasoning_test_chat <- function(
  action_response,
  thinking = NULL,
  usage = c(11L, 3L, 0L),
  action_responses = NULL
) {
  state <- new.env(parent = emptyenv())
  state$turns <- list()
  state$prompts <- character()
  state$output_types <- list()
  state$calls <- 0L
  state$action_calls <- 0L

  chat <- new_test_chat(
    model = "rlm-reasoning-test",
    chat_structured = function(prompt, type, ...) {
      fields <- if (inherits(type, "ellmer::TypeObject")) {
        names(type@properties)
      } else {
        character()
      }
      state$calls <- state$calls + 1L
      state$prompts <- c(state$prompts, as.character(prompt))
      state$output_types[[length(state$output_types) + 1L]] <- type

      is_action <- "code" %in% fields
      if (is_action) {
        state$action_calls <- state$action_calls + 1L
      }
      response <- if (is_action && !is.null(action_responses)) {
        action_responses[[state$action_calls]]
      } else if (is_action) {
        action_response
      } else {
        list(answer = "fallback answer")
      }
      contents <- list(ellmer::ContentText("structured response"))
      thinking_text <- if (is.list(thinking)) {
        thinking[[min(state$action_calls, length(thinking))]]
      } else if (!is.null(thinking)) {
        thinking[[min(state$action_calls, length(thinking))]]
      } else {
        NULL
      }
      if (is_action && !is.null(thinking_text)) {
        contents <- c(
          list(ellmer::ContentThinking(thinking_text, list(provider = "test"))),
          contents
        )
      }
      state$turns <- c(
        state$turns,
        list(
          ellmer::UserTurn(contents = list(ellmer::ContentText("request"))),
          ellmer::AssistantTurn(
            contents = contents,
            tokens = usage,
            cost = 0.125,
            duration = 0.25
          )
        )
      )
      response
    },
    get_turns = function() state$turns,
    set_turns = function(turns) {
      state$turns <- turns
      invisible(NULL)
    },
    last_turn = function(role = NULL) {
      turns <- if (identical(role, "assistant")) {
        Filter(
          function(turn) inherits(turn, "ellmer::AssistantTurn"),
          state$turns
        )
      } else {
        state$turns
      }
      if (length(turns) == 0L) NULL else turns[[length(turns)]]
    },
    get_model = function() "rlm-reasoning-test"
  )

  list(chat = chat, state = state)
}

rlm_reasoning_test_runner <- function() {
  r_code_runner(timeout = 30, persistent = TRUE)
}

test_that("RLM keeps rationale by default and builds mode-specific actions", {
  rationale <- rlm_module(
    "question -> answer",
    interpreter_factory = function() stop("not used")
  )
  native <- rlm_module(
    "question -> answer",
    interpreter_factory = function() stop("not used"),
    reasoning_mode = "native"
  )
  plan <- rlm_module(
    "question -> answer",
    interpreter_factory = function() stop("not used"),
    reasoning_mode = "plan"
  )

  expect_identical(rationale$config$reasoning_mode, "rationale")
  expect_identical(
    names(rationale$generate_action$signature@output_type@properties),
    c("reasoning", "code")
  )
  expect_identical(native$config$reasoning_mode, "native")
  expect_identical(
    names(native$generate_action$signature@output_type@properties),
    "code"
  )
  expect_identical(native$generate_action$config$reasoning_mode, "native")
  expect_identical(plan$config$reasoning_mode, "plan")
  expect_identical(
    names(plan$generate_action$signature@output_type@properties),
    c("plan", "code")
  )
  expect_match(
    plan$generate_action$signature@output_type@properties$plan@description,
    "concise high-level plan"
  )
})

test_that("RLM action mode config rejects conflicts and custom schemas", {
  runner_factory <- function() stop("not used")
  expect_error(
    rlm_module(
      "question -> answer",
      interpreter_factory = runner_factory,
      reasoning_mode = "native",
      config = list(reasoning_mode = "plan")
    ),
    class = "dsprrr_reasoning_mode_error"
  )
  expect_error(
    rlm_module(
      "question -> answer",
      interpreter_factory = runner_factory,
      reasoning_mode = "extended"
    ),
    class = "dsprrr_reasoning_mode_error"
  )

  rationale_action <- new_rlm_action_module()
  expect_error(
    rlm_module(
      "question -> answer",
      interpreter_factory = runner_factory,
      reasoning_mode = "native",
      generate_action = rationale_action
    ),
    class = "dsprrr_rlm_generate_action_contract_error"
  )
  expect_no_error(
    rlm_module(
      "question -> answer",
      interpreter_factory = runner_factory,
      reasoning_mode = "native",
      generate_action = new_rlm_action_module("native")
    )
  )
})

test_that("RLM native mode returns current thinking and unchanged usage", {
  skip_if_not_installed("callr")
  local_reset_cache()
  runner <- rlm_reasoning_test_runner()
  withr::defer(runner$shutdown())
  mock <- rlm_reasoning_test_chat(
    action_response = list(code = "SUBMIT(42L)"),
    thinking = "provider action thinking"
  )
  program <- rlm_module(
    "question -> answer: integer",
    runner = runner,
    reasoning_mode = "native",
    max_iterations = 1L,
    max_llm_calls = 0L
  )

  result <- run(
    program,
    question = "What is the answer?",
    .llm = mock$chat,
    .cache = FALSE,
    .return_format = "structured"
  )
  metadata <- get_metadata(result)
  action <- metadata$repl_history[[1L]]

  expect_identical(get_output(result), list(answer = 42L))
  expect_identical(names(mock$state$output_types[[1L]]@properties), "code")
  expect_identical(metadata$reasoning_mode, "native")
  expect_length(metadata$native_reasoning, 1L)
  expect_s3_class(metadata$native_reasoning[[1L]], "ellmer::ContentThinking")
  expect_identical(
    metadata$native_reasoning[[1L]]@thinking,
    "provider action thinking"
  )
  expect_identical(metadata$native_reasoning_action_indices, 1L)
  expect_false("reasoning" %in% names(action))
  expect_s3_class(
    action$action_metadata$native_reasoning[[1L]],
    "ellmer::ContentThinking"
  )
  expect_identical(metadata$input_tokens, 11L)
  expect_identical(metadata$output_tokens, 3L)
  expect_identical(metadata$total_tokens, 14L)
  expect_equal(metadata$cost, 0.125)
  expect_identical(metadata$provider_calls, 1L)
})

test_that("RLM native metadata follows action order without double counting", {
  skip_if_not_installed("callr")
  local_reset_cache()
  runner <- rlm_reasoning_test_runner()
  withr::defer(runner$shutdown())
  mock <- rlm_reasoning_test_chat(
    action_response = list(code = "SUBMIT(42L)"),
    action_responses = list(
      list(code = "x <- 1"),
      list(code = "SUBMIT(x + 41L)")
    ),
    thinking = list("first action thinking", "second action thinking")
  )
  program <- rlm_module(
    "question -> answer: integer",
    runner = runner,
    reasoning_mode = "native",
    max_iterations = 2L,
    max_llm_calls = 0L
  )

  result <- run(
    program,
    question = "What is the answer?",
    .llm = mock$chat,
    .cache = FALSE,
    .return_format = "structured"
  )
  metadata <- get_metadata(result)

  expect_identical(get_output(result), list(answer = 42L))
  expect_length(metadata$native_reasoning, 2L)
  expect_identical(
    vapply(
      metadata$native_reasoning,
      function(content) content@thinking,
      character(1)
    ),
    c("first action thinking", "second action thinking")
  )
  expect_identical(metadata$native_reasoning_action_indices, c(1L, 2L))
  expect_identical(metadata$input_tokens, 22L)
  expect_identical(metadata$output_tokens, 6L)
  expect_identical(metadata$total_tokens, 28L)
  expect_equal(metadata$cost, 0.25)
  expect_identical(metadata$provider_calls, 2L)
})

test_that("RLM native mode keeps absent thinking NULL and preserves plans", {
  skip_if_not_installed("callr")
  local_reset_cache()

  runner <- rlm_reasoning_test_runner()
  withr::defer(runner$shutdown())
  no_thinking <- rlm_reasoning_test_chat(
    action_response = list(code = "SUBMIT(7L)")
  )
  no_thinking$state$turns <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("stale request"))),
    ellmer::AssistantTurn(
      contents = list(ellmer::ContentThinking("stale provider thought", list()))
    )
  )
  native_result <- rlm(
    "question -> answer: integer",
    question = "Answer?",
    .llm = no_thinking$chat,
    .runner = runner,
    .reasoning_mode = "native",
    .max_iterations = 1L,
    .max_llm_calls = 0L,
    .cache = FALSE,
    .return_format = "structured"
  )
  expect_null(get_metadata(native_result)$native_reasoning)
  expect_null(get_metadata(native_result)$native_reasoning_action_indices)
  expect_identical(get_output(native_result), list(answer = 7L))

  plan_runner <- rlm_reasoning_test_runner()
  withr::defer(plan_runner$shutdown())
  plan_mock <- rlm_reasoning_test_chat(
    action_response = list(
      plan = "Read the evidence, then submit the checked value.",
      code = "SUBMIT(9L)"
    )
  )
  plan <- rlm_module(
    "question -> answer: integer",
    runner = plan_runner,
    reasoning_mode = "plan",
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  plan_result <- run(
    plan,
    question = "Answer?",
    .llm = plan_mock$chat,
    .cache = FALSE,
    .return_format = "structured"
  )
  plan_metadata <- get_metadata(plan_result)
  plan_action <- plan_metadata$repl_history[[1L]]

  expect_identical(get_output(plan_result), list(answer = 9L))
  expect_identical(plan_metadata$reasoning_mode, "plan")
  expect_identical(
    plan_action$plan,
    "Read the evidence, then submit the checked value."
  )
  expect_false("reasoning" %in% names(plan_action))
})

test_that("RLM native fallback omits rationale and keeps typed output", {
  skip_if_not_installed("callr")
  local_reset_cache()
  runner <- rlm_reasoning_test_runner()
  withr::defer(runner$shutdown())
  mock <- rlm_reasoning_test_chat(action_response = list(code = "1 + 1"))
  program <- rlm_module(
    "question -> answer",
    runner = runner,
    reasoning_mode = "native",
    max_iterations = 1L,
    max_llm_calls = 0L
  )

  result <- NULL
  expect_warning(
    result <- run(
      program,
      question = "What did the inspection find?",
      .llm = mock$chat,
      .cache = FALSE,
      .return_format = "structured"
    ),
    "reached max_iterations"
  )
  metadata <- get_metadata(result)

  expect_identical(get_output(result), list(answer = "fallback answer"))
  expect_identical(metadata$output_source, "fallback")
  expect_false("reasoning" %in% names(metadata$repl_history[[1L]]))
  expect_true(any(grepl("Code: 1 + 1", mock$state$prompts, fixed = TRUE)))
  expect_false(any(grepl("Reasoning:", mock$state$prompts, fixed = TRUE)))
})

test_that("RLM reasoning mode survives artifacts and rejects mismatches", {
  factory_calls <- 0L
  factory <- function() {
    factory_calls <<- factory_calls + 1L
    list(
      execute = function(code, context = list()) {
        list(
          success = TRUE,
          result = NULL,
          stdout = "",
          stderr = "",
          error = "",
          duration_ms = 0
        )
      },
      policy = function() {
        list(
          backend = "artifact-test",
          trust = "test-only",
          sandboxed = TRUE,
          persistent = TRUE
        )
      },
      shutdown = function() invisible(NULL)
    )
  }
  registry <- list(interpreter_factory = factory)

  for (mode in c("native", "plan")) {
    program <- rlm_module(
      "question -> answer",
      interpreter_factory = factory,
      reasoning_mode = mode,
      max_llm_calls = 0L
    )
    artifact <- program_artifact(program, registry = registry)
    restored <- restore_module_config(artifact, registry = registry)

    expect_identical(restored$config$reasoning_mode, mode)
    expect_identical(
      names(restored$generate_action$signature@output_type@properties),
      if (identical(mode, "native")) "code" else c("plan", "code")
    )
    expect_identical(
      program_artifact(restored, registry = registry)$graph,
      artifact$graph
    )
  }
  expect_identical(factory_calls, 0L)

  native <- rlm_module(
    "question -> answer",
    interpreter_factory = factory,
    reasoning_mode = "native",
    max_llm_calls = 0L
  )
  artifact <- program_artifact(native, registry = registry)
  malformed <- artifact
  malformed$graph$nodes[[malformed$root]]$config$reasoning_mode <- "plan"
  malformed$integrity <- artifact_integrity(malformed)
  restore_error <- rlang::catch_cnd(
    restore_module_config(malformed, registry = registry)
  )
  expect_s3_class(restore_error, "dsprrr_artifact_malformed")
  expect_s3_class(
    restore_error$parent,
    "dsprrr_rlm_generate_action_contract_error"
  )
  expect_identical(factory_calls, 0L)
})

test_that("RLM native guards reject unsupported execution before runner work", {
  factory_calls <- 0L
  factory <- function() {
    factory_calls <<- factory_calls + 1L
    stop("must not start")
  }
  native <- rlm_module(
    "question -> answer",
    interpreter_factory = factory,
    reasoning_mode = "native"
  )
  chat <- rlm_reasoning_test_chat(action_response = list(code = "SUBMIT('x')"))

  batch_error <- rlang::catch_cnd(run_dataset(
    native,
    data.frame(question = "one"),
    .llm = chat$chat
  ))
  async_error <- rlang::catch_cnd(run_async(
    native,
    question = "one",
    .llm = chat$chat
  ))

  expect_s3_class(batch_error, "dsprrr_reasoning_batch_unsupported")
  expect_s3_class(async_error, "dsprrr_reasoning_metadata_unsupported")
  expect_identical(factory_calls, 0L)
  expect_identical(chat$state$calls, 0L)
})

test_that("RLM validates current mode and action contracts before work", {
  factory_calls <- 0L
  factory <- function() {
    factory_calls <<- factory_calls + 1L
    stop("must not start")
  }
  chat <- rlm_reasoning_test_chat(action_response = list(code = "SUBMIT('x')"))

  root_mode <- rlm_module(
    "question -> answer",
    interpreter_factory = factory,
    reasoning_mode = "plan",
    generate_action = new_rlm_action_module("plan")
  )
  root_mode$config$reasoning_mode <- "native"
  root_error <- rlang::catch_cnd(run(
    root_mode,
    question = "one",
    .llm = chat$chat
  ))

  child_config <- rlm_module(
    "question -> answer",
    interpreter_factory = factory,
    reasoning_mode = "native"
  )
  child_config$generate_action$config$reasoning_mode <- "plan"
  child_config_error <- rlang::catch_cnd(run(
    child_config,
    question = "one",
    .llm = chat$chat
  ))

  child_signature <- rlm_module(
    "question -> answer",
    interpreter_factory = factory,
    reasoning_mode = "native"
  )
  child_signature$generate_action$signature <-
    new_rlm_action_module("rationale")$signature
  child_signature_error <- rlang::catch_cnd(run(
    child_signature,
    question = "one",
    .llm = chat$chat
  ))

  expect_s3_class(root_error, "dsprrr_rlm_generate_action_contract_error")
  expect_s3_class(
    child_config_error,
    "dsprrr_rlm_generate_action_contract_error"
  )
  expect_s3_class(
    child_signature_error,
    "dsprrr_rlm_generate_action_contract_error"
  )
  expect_identical(factory_calls, 0L)
  expect_identical(chat$state$calls, 0L)
})
