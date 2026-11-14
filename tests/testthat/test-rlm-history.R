rlm_history_action_reply <- function(code) {
  list(reasoning = "Use the current evidence", code = code)
}

rlm_history_local_provider <- function(replies, calls, .env = parent.frame()) {
  calls$requests <- list()
  calls$n <- 0L
  testthat::local_mocked_bindings(
    req_perform = function(req) {
      calls$n <- calls$n + 1L
      if (calls$n > length(replies)) {
        cli::cli_abort("Unexpected RLM history provider request")
      }
      calls$requests[[calls$n]] <- req$body$data
      reply <- replies[[calls$n]]
      if (inherits(reply, "condition")) {
        stop(reply)
      }
      answer <- jsonlite::toJSON(
        reply,
        auto_unbox = TRUE,
        null = "null"
      )
      response <- list(
        id = paste0("resp_", calls$n),
        object = "response",
        created_at = 1L,
        status = "completed",
        model = "model-a",
        output = list(list(
          id = paste0("msg_", calls$n),
          type = "message",
          status = "completed",
          role = "assistant",
          content = list(list(
            type = "output_text",
            annotations = list(),
            logprobs = list(),
            text = as.character(answer)
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
      httr2::response(
        headers = list(`content-type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          response,
          auto_unbox = TRUE,
          null = "null"
        ))
      )
    },
    .package = "ellmer",
    .env = .env
  )
}

rlm_history_real_chat <- function() {
  suppressWarnings(ellmer::chat_openai(
    credentials = function() "history-test-key",
    model = "model-a"
  ))
}

rlm_history_recording_factory <- function(state) {
  state$created <- 0L
  state$closed <- 0L
  state$executed <- list()
  function() {
    state$created <- state$created + 1L
    index <- state$created
    inner <- r_code_runner(timeout = 60, persistent = TRUE)
    list(
      execute = function(code, context = list(), .control_nonce = NULL) {
        if (length(state$executed) < index) {
          state$executed[index] <- list(character())
        }
        state$executed[[index]] <- c(
          state$executed[[index]],
          paste(code, collapse = "\n")
        )
        inner$execute(code, context = context)
      },
      prepare_context = function(...) inner$prepare_context(...),
      policy = function() inner$policy(),
      shutdown = function() {
        state$closed <- state$closed + 1L
        inner$shutdown()
        if (isTRUE(state$fail_shutdown)) {
          stop("deliberate runner shutdown failure", call. = FALSE)
        }
      }
    )
  }
}

rlm_history_request_text <- function(request) {
  paste(unlist(request, recursive = TRUE, use.names = FALSE), collapse = "\n")
}

test_that("RLM history continues through fresh interpreters and fallback", {
  skip_if_not_installed("callr")
  calls <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  factory <- rlm_history_recording_factory(state)
  rlm_history_local_provider(
    list(
      rlm_history_action_reply(
        "history_marker_947 <- 41L; SUBMIT('first answer')"
      ),
      rlm_history_action_reply(paste(
        "stopifnot(!exists('history_marker_947', inherits = TRUE));",
        "cat('fresh interpreter')"
      )),
      list(answer = "fallback answer"),
      rlm_history_action_reply(paste(
        "stopifnot(!exists('history_marker_947', inherits = TRUE));",
        "SUBMIT('separate answer')"
      ))
    ),
    calls
  )
  chat <- rlm_history_real_chat()
  supplied_turns <- chat$get_turns()
  module <- rlm_module(
    "question -> answer: string",
    interpreter_factory = factory,
    max_iterations = 1L,
    max_llm_calls = 0L,
    chat = chat
  )

  first <- run(
    module,
    question = "first call context",
    .history = conversation_history(),
    .return_format = "structured",
    .cache = FALSE
  )
  first_history <- get_history(first)
  second <- suppressWarnings(run(
    module,
    question = "second call context",
    .history = first_history,
    .return_format = "structured",
    .cache = FALSE
  ))
  second_history <- get_history(second)
  separate <- run(
    module,
    question = "separate conversation context",
    .history = conversation_history(),
    .return_format = "structured",
    .cache = FALSE
  )
  separate_history <- get_history(separate)

  expect_identical(first$output, list(answer = "first answer"))
  expect_identical(second$output, list(answer = "fallback answer"))
  expect_identical(separate$output, list(answer = "separate answer"))
  expect_identical(first_history$kind, "rlm")
  expect_identical(first_history$version, 2L)
  expect_identical(length(second_history$exchanges), 2L)
  expect_identical(length(separate_history$exchanges), 1L)
  expect_match(
    separate_history$exchanges[[1L]]$task_text,
    "separate conversation context"
  )
  expect_no_match(
    separate_history$exchanges[[1L]]$task_text,
    "first call context"
  )
  expect_match(first_history$exchanges[[1L]]$task_text, "first call context")
  expect_match(
    first_history$exchanges[[1L]]$trajectory[[1L]]$code,
    "history_marker_947 <- 41L",
    fixed = TRUE
  )
  expect_match(
    second_history$exchanges[[2L]]$trajectory[[1L]]$output,
    "fresh interpreter",
    fixed = TRUE
  )
  expect_identical(second_history$exchanges[[2L]]$output_source, "fallback")
  expect_identical(state$created, 3L)
  expect_identical(state$closed, 3L)
  expect_identical(chat$get_turns(), supplied_turns)
  expect_identical(first_history, get_history(first))
  expect_identical(second$metadata$input_tokens, 10L)
  expect_identical(second$metadata$output_tokens, 4L)
  expect_identical(calls$n, 4L)
  expect_no_match(
    paste(state$executed[[2L]], collapse = "\n"),
    "history_marker_947 <- 41L",
    fixed = TRUE
  )

  prior_context <- history_render_rlm(first_history)
  action_request <- rlm_history_request_text(calls$requests[[2L]])
  fallback_request <- rlm_history_request_text(calls$requests[[3L]])
  separate_request <- rlm_history_request_text(calls$requests[[4L]])
  expect_match(
    action_request,
    "=== BEGIN PRIOR RLM EXCHANGE 1 ===",
    fixed = TRUE
  )
  expect_match(
    fallback_request,
    "=== BEGIN PRIOR RLM EXCHANGE 1 ===",
    fixed = TRUE
  )
  expect_match(action_request, "history_marker_947 <- 41L", fixed = TRUE)
  expect_match(fallback_request, "history_marker_947 <- 41L", fixed = TRUE)
  expect_match(action_request, prior_context, fixed = TRUE)
  expect_match(fallback_request, prior_context, fixed = TRUE)
  expect_no_match(
    separate_request,
    "=== BEGIN PRIOR RLM EXCHANGE 1 ===",
    fixed = TRUE
  )
  expect_gt(nchar(prior_context), 0L)

  path <- tempfile(fileext = ".json")
  save_history(second_history, path)
  expect_identical(load_history(path), second_history)
  unlink(path)

  artifact <- program_artifact(
    module,
    registry = list(interpreter_factory = factory)
  )
  artifact_text <- as.character(jsonlite::toJSON(
    artifact,
    auto_unbox = TRUE,
    null = "null"
  ))
  expect_no_match(artifact_text, "history_marker_947", fixed = TRUE)
  expect_no_match(artifact_text, "first call context", fixed = TRUE)
  expect_no_match(artifact_text, "BEGIN PRIOR RLM EXCHANGE", fixed = TRUE)
})

test_that("direct RLM history works with trace disabled", {
  skip_if_not_installed("callr")
  calls <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  rlm_history_local_provider(
    list(rlm_history_action_reply("SUBMIT(23L)")),
    calls
  )
  module <- rlm_module(
    "question -> answer: integer",
    interpreter_factory = rlm_history_recording_factory(state),
    max_llm_calls = 0L
  )
  traces_before <- module$state$traces
  result <- module$forward(
    list(question = "trace-free context"),
    .llm = rlm_history_real_chat(),
    .history = conversation_history(),
    trace = FALSE
  )

  expect_identical(result$output[[1L]], list(answer = 23L))
  expect_identical(result$metadata[[1L]]$conversation_history$kind, "rlm")
  expect_identical(module$state$traces, traces_before)
  expect_identical(state$created, 1L)
  expect_identical(state$closed, 1L)
})

test_that("RLM history preflight rejects unsafe entrypoints before factory work", {
  state <- new.env(parent = emptyenv())
  factory <- rlm_history_recording_factory(state)
  module <- rlm_module(
    "question -> answer",
    interpreter_factory = factory,
    max_llm_calls = 0L
  )
  react_history <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("prior user"))),
    ellmer::AssistantTurn(list(ellmer::ContentText("prior answer")))
  ))

  expect_error(
    run(
      module,
      question = "new task",
      .history = react_history,
      .return_format = "structured"
    ),
    class = "dsprrr_history_kind_error"
  )
  expect_error(
    run(
      module,
      question = "new task",
      .history = conversation_history()
    ),
    class = "dsprrr_history_result_format_error"
  )
  expect_error(
    run_dataset(
      module,
      data.frame(question = character()),
      .history = conversation_history()
    ),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_error(
    run_async(module, question = "new task", .history = conversation_history()),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_error(
    stream_async(
      module,
      question = "new task",
      .history = conversation_history()
    ),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_error(
    run_stream(
      module,
      question = "new task",
      .history = conversation_history()
    ),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_identical(state$created, 0L)
})

test_that("explicit RLM history requires a fresh factory before model resolution", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  module <- rlm_module(
    "question -> answer",
    runner = runner,
    max_llm_calls = 0L
  )

  expect_error(
    module$forward(
      list(question = "unused"),
      .history = conversation_history()
    ),
    class = "dsprrr_rlm_history_factory_error"
  )
})

test_that("postflight history overflow shuts down the lease and preserves input", {
  skip_if_not_installed("callr")
  limit <- history_limits()$rlm_render_bytes
  seed <- history_append_rlm(
    conversation_history(),
    task_text = "x",
    trajectory = list(),
    output = list(),
    output_source = "submit"
  )
  seed_size <- nchar(history_render_rlm(seed), type = "bytes")
  target_size <- as.integer(limit - 32L)
  long_task <- strrep("x", target_size - seed_size + 1L)
  prior <- history_append_rlm(
    conversation_history(),
    task_text = long_task,
    trajectory = list(),
    output = list(),
    output_source = "submit"
  )
  expect_identical(
    nchar(history_render_rlm(prior), type = "bytes"),
    target_size
  )
  prior_before <- prior
  calls <- new.env(parent = emptyenv())
  state <- new.env(parent = emptyenv())
  factory <- rlm_history_recording_factory(state)
  rlm_history_local_provider(
    list(rlm_history_action_reply("SUBMIT('small output')")),
    calls
  )
  module <- rlm_module(
    "question -> answer: string",
    interpreter_factory = factory,
    max_llm_calls = 0L
  )

  expect_error(
    run(
      module,
      question = "current task",
      .llm = rlm_history_real_chat(),
      .history = prior,
      .return_format = "structured",
      .cache = FALSE
    ),
    class = "dsprrr_history_error"
  )
  expect_identical(prior, prior_before)
  expect_identical(state$created, 1L)
  expect_identical(state$closed, 1L)
})

test_that("provider and shutdown errors preserve supplied RLM history", {
  skip_if_not_installed("callr")
  history <- history_append_rlm(
    conversation_history(),
    task_text = "earlier task",
    trajectory = list(),
    output = list(answer = "earlier output"),
    output_source = "submit"
  )
  history_before <- history

  failed_calls <- new.env(parent = emptyenv())
  failed_state <- new.env(parent = emptyenv())
  rlm_history_local_provider(
    list(simpleError("deliberate provider failure")),
    failed_calls
  )
  failed_module <- rlm_module(
    "question -> answer",
    interpreter_factory = rlm_history_recording_factory(failed_state),
    max_llm_calls = 0L
  )
  expect_error(
    failed_module$forward(
      list(question = "provider failure"),
      .llm = rlm_history_real_chat(),
      .history = history,
      trace = FALSE
    ),
    class = "dsprrr_rlm_action_error"
  )
  expect_identical(history, history_before)
  expect_identical(failed_state$created, 1L)
  expect_identical(failed_state$closed, 1L)

  close_calls <- new.env(parent = emptyenv())
  close_state <- new.env(parent = emptyenv())
  close_state$fail_shutdown <- TRUE
  rlm_history_local_provider(
    list(rlm_history_action_reply("SUBMIT('not returned')")),
    close_calls
  )
  close_module <- rlm_module(
    "question -> answer",
    interpreter_factory = rlm_history_recording_factory(close_state),
    max_llm_calls = 0L
  )
  expect_error(
    close_module$forward(
      list(question = "shutdown failure"),
      .llm = rlm_history_real_chat(),
      .history = history,
      trace = FALSE
    ),
    class = "dsprrr_interpreter_close_error"
  )
  expect_identical(history, history_before)
  expect_identical(close_state$created, 1L)
  expect_identical(close_state$closed, 1L)
})
