async_contract_real_chat <- function(turns = list()) {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  chat <- suppressWarnings(ellmer::chat_openai(
    api_key = "mock-async-key",
    model = "mock-async-model"
  ))
  chat$set_turns(turns)

  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  state$prompt_parts <- NULL
  state$type <- NULL
  state$run_context <- NULL
  state$complete <- NULL

  async_method <- function(..., type, run_context = NULL) {
    prompt_parts <- list(...)
    state$calls <- state$calls + 1L
    state$prompt_parts <- prompt_parts
    state$type <- type
    state$run_context <- run_context

    promises::promise(function(resolve, reject) {
      state$complete <- function(
        response = list(answer = "mock answer"),
        thinking = NULL,
        thinking_extra = list(),
        error = NULL,
        tokens = c(NA_real_, NA_real_, NA_real_)
      ) {
        user_contents <- lapply(prompt_parts, function(part) {
          if (is_content_input(part)) {
            part
          } else {
            ellmer::ContentText(as.character(part))
          }
        })
        assistant_contents <- list()
        if (!is.null(thinking)) {
          assistant_contents <- list(
            ellmer::ContentThinking(thinking, thinking_extra)
          )
        }
        assistant_contents <- c(
          assistant_contents,
          list(ellmer::ContentText("mock assistant turn"))
        )
        chat$set_turns(c(
          chat$get_turns(),
          list(
            ellmer::UserTurn(contents = user_contents),
            ellmer::AssistantTurn(
              contents = assistant_contents,
              tokens = tokens
            )
          )
        ))
        if (inherits(error, "condition")) {
          reject(error)
        } else {
          resolve(response)
        }
        invisible(NULL)
      }
    })
  }
  override_test_chat_method(chat, "chat_structured_async", async_method)

  list(chat = chat, state = state)
}


async_contract_settle <- function(promise, timeout = 10) {
  state <- new.env(parent = emptyenv())
  state$done <- FALSE
  state$value <- NULL
  state$error <- NULL
  observer <- promises::then(
    promise,
    onFulfilled = function(value) {
      state$value <- value
      state$done <- TRUE
      NULL
    },
    onRejected = function(error) {
      state$error <- error
      state$done <- TRUE
      NULL
    }
  )
  deadline <- Sys.time() + timeout
  while (!state$done && Sys.time() < deadline) {
    later::run_now(0.05)
  }
  invisible(observer)
  if (!state$done) {
    stop("async test promise did not settle", call. = FALSE)
  }
  list(value = state$value, error = state$error)
}


test_that("run_async keeps simple output and records one canonical trace", {
  mock <- async_contract_real_chat()
  solver <- module(
    signature("question -> answer"),
    template = "Question: {question}"
  )

  promise <- run_async(
    solver,
    question = "What is the answer?",
    .llm = mock$chat
  )

  expect_s3_class(promise, "promise")
  expect_length(mock$state$prompt_parts, 1L)
  expect_match(as.character(mock$state$prompt_parts[[1L]]), "Question:")
  expect_false(any(
    names(mock$state$prompt_parts) %in% c("echo", "run_context")
  ))
  expect_identical(mock$state$type, solver$signature@output_type)

  mock$state$complete(response = list(answer = "42"))
  outcome <- async_contract_settle(promise)

  expect_null(outcome$error)
  expect_identical(outcome$value, list(answer = "42"))
  expect_length(solver$state$traces, 1L)
  trace <- solver$state$traces[[1L]]
  expect_identical(trace$output, list(answer = "42"))
  expect_identical(trace$metadata$backend, "async")
  expect_identical(trace$metadata$cache, "bypass")
  expect_true(is.na(trace$metadata$input_tokens))
  expect_true(is.na(trace$metadata$output_tokens))
  expect_true(is.na(trace$metadata$cost))
})


test_that("Module$run_async forwards structured format and decision evidence", {
  mock <- async_contract_real_chat()
  solver <- with_decisions(
    module(signature(
      "question -> flagged: boolean",
      instructions = "Should the content be flagged?"
    )),
    flagged = decision_bool(description = "Should it be flagged?")
  )

  promise <- solver$run_async(
    question = "Review this content",
    .llm = mock$chat,
    .return_format = "structured",
    .trace_context = list(request_id = "async-decision")
  )
  mock$state$complete(
    response = list(flagged = list(probability = 0.9))
  )
  outcome <- async_contract_settle(promise)

  expect_null(outcome$error)
  expect_s3_class(outcome$value, "dsprrr_result")
  expect_identical(outcome$value$output, list(flagged = TRUE))
  expect_true(is_ellmer_chat(outcome$value$chat))
  expect_true(is.list(outcome$value$metadata$decisions$flagged))
  expect_identical(
    outcome$value$metadata$trace_context,
    list(request_id = "async-decision")
  )
  expect_identical(
    mock$state$run_context$dsprrr$trace_context,
    list(request_id = "async-decision")
  )
  expect_length(solver$state$traces, 1L)
})


test_that("async Chat receives normalized multimodal prompt parts", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  request_bodies <- list()
  response_body <- list(
    id = "async_media_response",
    object = "response",
    created_at = 1L,
    status = "completed",
    model = "mock-async-model",
    output = list(list(
      id = "async_media_message",
      type = "message",
      status = "completed",
      role = "assistant",
      content = list(list(
        type = "output_text",
        annotations = list(),
        logprobs = list(),
        text = '{"description":"an image"}'
      ))
    )),
    usage = list(
      input_tokens = 5L,
      input_tokens_details = list(cached_tokens = 0L),
      output_tokens = 2L,
      output_tokens_details = list(reasoning_tokens = 0L),
      total_tokens = 7L
    ),
    service_tier = "default",
    metadata = list()
  )
  withr::local_options(httr2_mock = function(req) {
    request_bodies[[length(request_bodies) + 1L]] <<- req$body$data
    httr2::response(
      headers = list(`content-type` = "application/json"),
      body = charToRaw(jsonlite::toJSON(
        response_body,
        auto_unbox = TRUE,
        null = "null"
      ))
    )
  })
  chat <- suppressWarnings(ellmer::chat_openai(
    api_key = "mock-async-key",
    base_url = "http://127.0.0.1:9/v1",
    model = "mock-async-model"
  ))
  solver <- module(signature("image -> description"))
  image <- ellmer::ContentImageRemote("https://example.com/image.png")

  promise <- run_async(
    solver,
    image = image,
    .llm = chat,
    .return_format = "structured"
  )
  outcome <- async_contract_settle(promise)

  expect_null(outcome$error)
  expect_identical(outcome$value$output, list(description = "an image"))
  expect_length(request_bodies, 1L)
  input <- request_bodies[[1L]]$input
  expect_length(input, 2L)
  expect_identical(input[[1L]]$content[[1L]]$type, "input_text")
  expect_identical(input[[2L]]$content[[1L]]$type, "input_image")
  expect_identical(
    input[[2L]]$content[[1L]]$image_url,
    "https://example.com/image.png"
  )
  expect_false(any(vapply(
    input,
    function(item) {
      any(vapply(
        item$content,
        function(content) {
          identical(content$text, "echo")
        },
        logical(1)
      ))
    },
    logical(1)
  )))
})


test_that("native async metadata contains only current provider thinking", {
  old_turn <- ellmer::AssistantTurn(
    contents = list(
      ellmer::ContentThinking("stale reasoning", list(source = "old"))
    )
  )
  mock <- async_contract_real_chat(turns = list(old_turn))
  solver <- chain_of_thought("question -> answer", mode = "native")
  promise <- run_async(
    solver,
    question = "Why?",
    .llm = mock$chat,
    .return_format = "structured"
  )
  mock$state$complete(
    response = list(answer = "because"),
    thinking = "current reasoning",
    thinking_extra = list(source = "provider")
  )
  outcome <- async_contract_settle(promise)

  expect_null(outcome$error)
  thinking <- outcome$value$metadata$native_reasoning
  expect_length(thinking, 1L)
  expect_s3_class(thinking[[1L]], "ellmer::ContentThinking")
  expect_identical(thinking[[1L]]@thinking, "current reasoning")
  expect_identical(thinking[[1L]]@extra, list(source = "provider"))

  absent_mock <- async_contract_real_chat(turns = list(old_turn))
  absent_solver <- chain_of_thought("question -> answer", mode = "native")
  absent_promise <- run_async(
    absent_solver,
    question = "Why?",
    .llm = absent_mock$chat,
    .return_format = "structured"
  )
  absent_mock$state$complete(response = list(answer = "because"))
  absent <- async_contract_settle(absent_promise)

  expect_null(absent$error)
  expect_null(absent$value$metadata$native_reasoning)
})


test_that("separate async Chats keep trace contexts isolated", {
  left_chat <- async_contract_real_chat()
  right_chat <- async_contract_real_chat()
  left <- module(signature("question -> answer"))
  right <- module(signature("question -> answer"))

  left_promise <- run_async(
    left,
    question = "left",
    .llm = left_chat$chat,
    .trace_context = list(call = "left")
  )
  right_promise <- run_async(
    right,
    question = "right",
    .llm = right_chat$chat,
    .trace_context = list(call = "right")
  )

  expect_identical(current_trace_context(), list())
  expect_identical(
    left_chat$state$run_context$dsprrr$trace_context,
    list(call = "left")
  )
  expect_identical(
    right_chat$state$run_context$dsprrr$trace_context,
    list(call = "right")
  )
  right_chat$state$complete(response = list(answer = "right result"))
  left_chat$state$complete(response = list(answer = "left result"))
  right_result <- async_contract_settle(right_promise)
  left_result <- async_contract_settle(left_promise)

  expect_null(left_result$error)
  expect_null(right_result$error)
  expect_identical(
    left$state$traces[[1L]]$trace_context,
    list(call = "left")
  )
  expect_identical(
    right$state$traces[[1L]]$trace_context,
    list(call = "right")
  )
})


test_that("async Chat reservation blocks overlapping async and sync calls", {
  mock <- async_contract_real_chat()
  solver <- module(signature("question -> answer"))
  pending <- run_async(solver, question = "first", .llm = mock$chat)

  overlap <- rlang::catch_cnd(
    run_async(solver, question = "second", .llm = mock$chat)
  )
  sync_overlap <- rlang::catch_cnd(
    run(solver, question = "sync", .llm = mock$chat)
  )
  react_overlap <- rlang::catch_cnd(
    run(react("question -> answer"), question = "react", .llm = mock$chat)
  )

  expect_s3_class(overlap, "dsprrr_chat_busy")
  expect_identical(overlap$owner_operation, "run_async")
  expect_s3_class(sync_overlap, "dsprrr_chat_busy")
  expect_s3_class(react_overlap, "dsprrr_chat_busy")
  expect_identical(mock$state$calls, 1L)

  mock$state$complete(response = list(answer = "first result"))
  expect_null(async_contract_settle(pending)$error)
  expect_no_error(assert_async_chat_available(mock$chat))
})


test_that("provider rejection records one error trace and releases the Chat", {
  mock <- async_contract_real_chat()
  solver <- module(signature("question -> answer"))
  pending <- run_async(
    solver,
    question = "fail",
    .llm = mock$chat,
    .trace_context = list(call = "failure")
  )
  mock$state$complete(
    error = simpleError("mock async provider failed"),
    tokens = c(11, 7, 0)
  )
  outcome <- async_contract_settle(pending)

  expect_s3_class(outcome$error, "error")
  expect_match(conditionMessage(outcome$error), "mock async provider failed")
  expect_length(solver$state$traces, 1L)
  trace <- solver$state$traces[[1L]]
  expect_identical(trace$metadata$error_stage, "provider")
  expect_match(trace$metadata$error, "mock async provider failed")
  expect_identical(trace$metadata$input_tokens, 11L)
  expect_identical(trace$metadata$output_tokens, 7L)
  expect_identical(trace$trace_context, list(call = "failure"))
  expect_no_error(assert_async_chat_available(mock$chat))
})


test_that("history sink errors roll back the success trace and keep one failure trace", {
  local_reset_cache()
  mock <- async_contract_real_chat()
  solver <- module(signature("question -> answer"))
  context <- list(call = "history-sink")
  history_before <- .dsprrr_env$prompt_history
  generation_before <- prompt_history_generation()
  testthat::local_mocked_bindings(
    add_to_global_history = function(...) {
      stop("history sink failed", call. = FALSE)
    },
    .package = "dsprrr"
  )

  pending <- run_async(
    solver,
    question = "history failure",
    .llm = mock$chat,
    .trace_context = context
  )
  mock$state$complete(response = list(answer = "completed"))
  outcome <- async_contract_settle(pending)

  expect_match(conditionMessage(outcome$error), "history sink failed")
  expect_length(solver$state$traces, 1L)
  trace <- solver$state$traces[[1L]]
  expect_identical(trace$output, NA)
  expect_identical(trace$metadata$error_stage, "completion")
  expect_match(trace$metadata$error, "history sink failed")
  expect_identical(trace$trace_context, context)
  expect_identical(.dsprrr_env$prompt_history, history_before)
  expect_identical(
    prompt_history_generation(),
    generation_before
  )
  expect_no_error(assert_async_chat_available(mock$chat))
})


test_that("partial global history appends roll back on warning errors", {
  local_reset_cache()
  withr::local_options(
    dsprrr.prompt_history_max = NA_real_,
    warn = 2
  )
  real_history_sink <- add_to_global_history
  testthat::local_mocked_bindings(
    add_to_global_history = function(trace, source) {
      withCallingHandlers(
        real_history_sink(trace, source),
        dsprrr_history_capture_warning = function(warning) {
          stop(conditionMessage(warning), call. = FALSE)
        }
      )
    },
    .package = "dsprrr"
  )
  mock <- async_contract_real_chat()
  solver <- module(signature("question -> answer"))
  context <- list(call = "partial-history")
  history_before <- .dsprrr_env$prompt_history
  generation_before <- prompt_history_generation()

  pending <- run_async(
    solver,
    question = "partial history failure",
    .llm = mock$chat,
    .trace_context = context
  )
  mock$state$complete(response = list(answer = "completed"))
  outcome <- async_contract_settle(pending)

  expect_s3_class(outcome$error, "error")
  expect_match(conditionMessage(outcome$error), "history")
  expect_length(solver$state$traces, 1L)
  trace <- solver$state$traces[[1L]]
  expect_identical(trace$output, NA)
  expect_identical(trace$metadata$error_stage, "completion")
  expect_identical(trace$trace_context, context)
  history_after <- .dsprrr_env$prompt_history
  expect_gte(length(history_after), length(history_before))
  expect_lte(length(history_after) - length(history_before), 1L)
  if (length(history_after) > length(history_before)) {
    new_history <- utils::tail(history_after, 1L)[[1L]]
    expect_identical(new_history$metadata$error_stage, "completion")
  }
  expect_equal(
    prompt_history_generation() - generation_before,
    length(history_after) - length(history_before)
  )
  expect_no_error(assert_async_chat_available(mock$chat))
})


test_that("launch, decode, and completion errors record once and release Chat", {
  launch_chat <- async_contract_real_chat()
  launch_solver <- module(signature("question -> answer"))
  override_test_chat_method(
    launch_chat$chat,
    "chat_structured_async",
    function(..., type, run_context = NULL) {
      stop(simpleError("mock launch failure"))
    }
  )
  expect_error(
    run_async(launch_solver, question = "launch", .llm = launch_chat$chat),
    "mock launch failure"
  )
  expect_length(launch_solver$state$traces, 1L)
  expect_identical(
    launch_solver$state$traces[[1L]]$metadata$error_stage,
    "provider"
  )
  expect_no_error(assert_async_chat_available(launch_chat$chat))

  decode_chat <- async_contract_real_chat()
  decode_solver <- with_decisions(
    module(signature(
      "question -> flagged: boolean",
      instructions = "Should the content be flagged?"
    )),
    flagged = decision_bool(description = "Should it be flagged?")
  )
  decode_promise <- run_async(
    decode_solver,
    question = "bad evidence",
    .llm = decode_chat$chat
  )
  decode_chat$state$complete(response = list(flagged = list(probability = 2)))
  decode_outcome <- async_contract_settle(decode_promise)
  expect_s3_class(decode_outcome$error, "dsprrr_decision_evidence_error")
  expect_identical(
    decode_solver$state$traces[[1L]]$metadata$error_stage,
    "decode"
  )
  expect_no_error(assert_async_chat_available(decode_chat$chat))

  completion_chat <- async_contract_real_chat()
  completion_solver <- module(signature("question -> answer"))
  override_test_chat_method(
    completion_chat$chat,
    "clone",
    function(...) stop("mock completion failure", call. = FALSE)
  )
  completion_promise <- run_async(
    completion_solver,
    question = "completion",
    .llm = completion_chat$chat
  )
  completion_chat$state$complete(response = list(answer = "completed"))
  completion_outcome <- async_contract_settle(completion_promise)
  expect_match(
    conditionMessage(completion_outcome$error),
    "mock completion failure"
  )
  expect_length(completion_solver$state$traces, 1L)
  expect_identical(
    completion_solver$state$traces[[1L]]$metadata$error_stage,
    "completion"
  )
  expect_no_error(assert_async_chat_available(completion_chat$chat))
})


test_that("factory-backed structured async results fail before launch", {
  factory <- function() {
    list(
      start = function() invisible(NULL),
      execute = function(code, context = list()) {
        list(success = TRUE, result = "ok")
      },
      policy = function() {
        list(backend = "async-contract", trust = "test", sandboxed = TRUE)
      },
      shutdown = function() invisible(NULL)
    )
  }
  solver <- program_of_thought(
    "question -> answer",
    interpreter_factory = factory,
    extract_answer = FALSE
  )

  expect_error(
    run_async(
      solver,
      question = "structured",
      .llm = async_contract_real_chat()$chat,
      .return_format = "structured"
    ),
    class = "dsprrr_factory_async_structured_unsupported"
  )
})
