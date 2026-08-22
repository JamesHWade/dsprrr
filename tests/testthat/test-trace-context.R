trace_context_test_chat <- function(
  response = list(answer = "ok"),
  error = NULL
) {
  chat <- NULL
  chat <- new_test_chat(
    model = "trace-context-test",
    chat_structured = function(prompt, type, ...) {
      chat$parity_state$calls <- chat$parity_state$calls + 1L
      chat$parity_state$prompts <- c(
        chat$parity_state$prompts,
        as.character(prompt)
      )
      if (!is.null(error)) {
        stop(error, call. = FALSE)
      }
      value <- if (is.function(response)) response(type) else response
      chat$turns <- c(
        chat$turns,
        list(
          ellmer::UserTurn(
            contents = list(ellmer::ContentText(as.character(prompt)))
          ),
          ellmer::AssistantTurn(
            contents = list(ellmer::ContentText("ok")),
            tokens = c(2L, 1L, 0L),
            cost = 0.001,
            duration = 0.01
          )
        )
      )
      value
    }
  )
  chat$parity_state <- list(calls = 0L, prompts = character())
  chat$chat_structured_async <- function(prompt, type, ...) {
    chat$parity_state$calls <- chat$parity_state$calls + 1L
    chat$parity_state$prompts <- c(
      chat$parity_state$prompts,
      as.character(prompt)
    )
    "async"
  }
  chat$calls <- function() chat$parity_state$calls
  chat$prompts <- function() chat$parity_state$prompts
  chat
}

local_trace_context_openai_backend <- function(calls, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    req_perform = function(req) {
      calls$n <- calls$n + 1L
      body <- list(
        id = paste0("trace_context_response_", calls$n),
        object = "response",
        created_at = 1L,
        status = "completed",
        model = "trace-context-cache-model",
        output = list(list(
          id = paste0("trace_context_message_", calls$n),
          type = "message",
          status = "completed",
          role = "assistant",
          content = list(list(
            type = "output_text",
            annotations = list(),
            logprobs = list(),
            text = '{"answer":"cached"}'
          ))
        )),
        usage = list(
          input_tokens = 4L,
          input_tokens_details = list(cached_tokens = 0L),
          output_tokens = 2L,
          output_tokens_details = list(reasoning_tokens = 0L),
          total_tokens = 6L
        ),
        service_tier = "default",
        metadata = list()
      )
      getFromNamespace("response", "httr2")(
        headers = list(`content-type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          body,
          auto_unbox = TRUE,
          null = "null"
        ))
      )
    },
    .package = "ellmer",
    .env = .env
  )
  invisible(calls)
}

trace_context_fixture <- function() {
  list(
    product = "tempest",
    research_run_id = "research-123",
    stage = "extract_claims",
    knowledge_snapshot_id = "snapshot-456",
    policy = list(
      fail_closed = TRUE,
      threshold = 0.8,
      paths = list("governed", "grounded")
    )
  )
}

test_that("trace context accepts only plain JSON-compatible named objects", {
  context <- trace_context_fixture()
  context$unicode <- list(`étape` = "vérifier")
  context["optional"] <- list(NULL)
  context$empty_object <- structure(list(), names = character())
  context$array <- list("first", "second")
  validated <- dsprrr:::trace_context_validate(context)

  expect_identical(validated, context)
  context$policy$threshold <- 0.1
  expect_identical(validated$policy$threshold, 0.8)

  invalid <- list(
    1,
    list("unnamed-root"),
    structure(list(1, 2), names = c("named", "")),
    list(nested = structure(list(1, 2), names = c("named", ""))),
    list(value = c(item = 1)),
    list(value = c("first", "second")),
    list(value = NA_character_),
    list(value = NaN),
    list(value = Inf),
    list(value = factor("x")),
    list(value = data.frame(x = 1)),
    list(value = globalenv()),
    list(value = function() NULL),
    list(value = quote(x + 1))
  )
  for (value in invalid) {
    expect_error(
      dsprrr:::trace_context_validate(value),
      class = "dsprrr_trace_context_error"
    )
  }
})

test_that("trace validation rejects dispatching and oversized values safely", {
  called <- new.env(parent = emptyenv())
  called$any_na <- FALSE
  called$finite <- FALSE
  registerS3method(
    "anyNA",
    "evil_trace_value",
    function(x, recursive = FALSE) {
      called$any_na <- TRUE
      FALSE
    },
    envir = asNamespace("dsprrr")
  )
  registerS3method(
    "is.finite",
    "evil_trace_value",
    function(x) {
      called$finite <- TRUE
      TRUE
    },
    envir = asNamespace("dsprrr")
  )
  value <- structure(1, class = "evil_trace_value")

  expect_error(
    dsprrr:::trace_context_validate(list(value = value)),
    class = "dsprrr_trace_context_type_error"
  )
  expect_false(called$any_na)
  expect_false(called$finite)

  deep <- "leaf"
  for (index in seq_len(66L)) {
    deep <- list(deep)
  }
  expect_error(
    dsprrr:::trace_context_validate(list(deep = deep)),
    class = "dsprrr_trace_context_size_error"
  )
})

test_that("trace context rejects credential-like fields without printing values", {
  sentinel <- "DO-NOT-PRINT-THIS-SECRET"
  condition <- rlang::catch_cnd(
    dsprrr:::trace_context_validate(list(api_key = sentinel))
  )

  expect_s3_class(condition, "dsprrr_trace_context_credential_error")
  expect_match(conditionMessage(condition), "api_key", fixed = TRUE)
  expect_false(grepl(sentinel, conditionMessage(condition), fixed = TRUE))
  expect_no_error(dsprrr:::trace_context_validate(list(
    api_keyboard = "layout",
    secretary = "role",
    web_url = "https://example.test"
  )))
})

test_that("scalar and failed runs retain program identity and trace context", {
  context <- trace_context_fixture()
  program <- module(signature("text -> answer"))
  program_id <- program_artifact_id(program)
  chat <- trace_context_test_chat()

  result <- run(
    program,
    text = "hello",
    .llm = chat,
    .cache = FALSE,
    .return_format = "structured",
    .trace_context = context
  )
  trace <- tail(program$state$traces, 1L)[[1L]]

  expect_identical(result$metadata$program_artifact_id, program_id)
  expect_identical(result$metadata$trace_context, context)
  expect_identical(trace$program_artifact_id, program_id)
  expect_identical(trace$trace_context, context)
  expect_identical(program_artifact_id(program), program_id)

  failed <- module(signature("text -> answer"))
  expect_error(
    run(
      failed,
      text = "hello",
      .llm = trace_context_test_chat(error = "provider failed"),
      .cache = FALSE,
      .trace_context = context
    ),
    "provider failed"
  )
  failed_trace <- tail(failed$state$traces, 1L)[[1L]]
  expect_identical(failed_trace$trace_context, context)
})

test_that("public trace views retain program identity and trace context", {
  clear_prompt_history()
  withr::defer(clear_prompt_history())

  context <- trace_context_fixture()
  program <- module(signature("text -> answer"))
  program_id <- program_artifact_id(program)
  run(
    program,
    text = "hello",
    .llm = trace_context_test_chat(),
    .cache = FALSE,
    .trace_context = context
  )

  traces <- program$get_traces()
  exported <- export_traces(program)
  history <- inspect_history(n = 1L)
  last <- get_last_prompt()

  expect_identical(traces$program_artifact_id, program_id)
  expect_identical(traces$trace_context, list(context))
  expect_identical(exported$program_artifact_id, program_id)
  expect_identical(exported$trace_context, list(context))
  expect_identical(history$program_artifact_id, program_id)
  expect_identical(history$trace_context, list(context))
  expect_identical(last$program_artifact_id, program_id)
  expect_identical(last$trace_context, context)

  empty <- module(signature("text -> answer"))
  expect_identical(empty$get_traces()$program_artifact_id, character())
  expect_identical(empty$get_traces()$trace_context, list())
  expect_identical(export_traces(empty)$program_artifact_id, character())
  expect_identical(export_traces(empty)$trace_context, list())
})

test_that("program identity is reserved and cannot be forged", {
  program <- module(signature("text -> answer"))
  chat <- trace_context_test_chat()
  context <- trace_context_fixture()
  context$program_artifact_id <- paste0("sha256:", strrep("0", 64L))

  expect_error(
    run(
      program,
      text = "hello",
      .llm = chat,
      .trace_context = context
    ),
    class = "dsprrr_trace_context_name_error"
  )
  expect_identical(chat$calls(), 0L)
})

test_that("registry-backed programs use a verified bound artifact ID", {
  context <- trace_context_fixture()
  forward <- function(text, ...) list(answer = text)
  program <- module_fn("text -> answer", forward)
  program_id <- program_artifact_id(
    program,
    registry = list(forward = forward)
  )
  result <- run(
    program,
    text = "hello",
    .return_format = "structured",
    .trace_context = context
  )
  trace <- tail(program$state$traces, 1L)[[1L]]

  expect_identical(result$metadata$program_artifact_id, program_id)
  expect_identical(trace$program_artifact_id, program_id)
})

test_that("direct R6 runs intercept trace context before prompt assembly", {
  context <- trace_context_fixture()
  program <- module(signature("text -> answer"))
  program_id <- program_artifact_id(program)
  chat <- trace_context_test_chat()

  result <- program$run(
    text = "hello",
    .llm = chat,
    .cache = FALSE,
    .return_format = "structured",
    .trace_context = context
  )
  trace <- tail(program$state$traces, 1L)[[1L]]

  expect_identical(result$metadata$trace_context, context)
  expect_identical(result$metadata$program_artifact_id, program_id)
  expect_identical(trace$trace_context, context)
  expect_false(any(grepl("tempest", chat$prompts(), fixed = TRUE)))
})

test_that("async run handles carry context without adding it to prompts", {
  context <- trace_context_fixture()
  program <- module(signature("text -> answer"))
  program_id <- program_artifact_id(program)
  chat <- trace_context_test_chat()

  handle <- program$run_async(
    text = "hello",
    .llm = chat,
    .trace_context = context
  )
  fields <- attr(handle, "dsprrr_trace_context", exact = TRUE)

  expect_identical(fields$trace_context, context)
  expect_identical(fields$program_artifact_id, program_id)
  expect_false(any(grepl("tempest", chat$prompts(), fixed = TRUE)))
})

test_that("nested runs inherit context but retain their own program identity", {
  context <- trace_context_fixture()
  inner_forward <- function(text, ...) list(answer = text)
  inner <- module_fn("text -> answer", inner_forward)
  inner_result <- NULL
  outer_forward <- function(text, ...) {
    inner_result <<- run(
      inner,
      text = "inner",
      .cache = FALSE,
      .return_format = "structured"
    )
    list(answer = "outer")
  }
  outer <- module_fn("text -> answer", outer_forward)
  inner_id <- program_artifact_id(
    inner,
    registry = list(inner_forward = inner_forward)
  )
  outer_id <- program_artifact_id(
    outer,
    registry = list(outer_forward = outer_forward)
  )

  outer_result <- run(
    outer,
    text = "outer",
    .cache = FALSE,
    .return_format = "structured",
    .trace_context = context
  )

  expect_identical(inner_result$metadata$trace_context, context)
  expect_identical(inner_result$metadata$program_artifact_id, inner_id)
  expect_identical(
    tail(inner$state$traces, 1L)[[1L]]$program_artifact_id,
    inner_id
  )
  expect_identical(outer_result$metadata$program_artifact_id, outer_id)
})

test_that("outer cleanup does not overwrite an explicit nested context", {
  outer_context <- trace_context_fixture()
  inner_context <- trace_context_fixture()
  inner_context$stage <- "verify_claim_support"
  nested <- FALSE
  program <- NULL
  forward <- function(text, ...) {
    if (!nested) {
      nested <<- TRUE
      run(
        program,
        text = "inner",
        .cache = FALSE,
        .trace_context = inner_context
      )
    }
    list(answer = "outer")
  }
  program <- module_fn("text -> answer", forward)
  program_artifact_id(program, registry = list(forward = forward))

  run(
    program,
    text = "outer",
    .cache = FALSE,
    .trace_context = outer_context
  )
  traces <- tail(program$state$traces, 2L)

  expect_identical(traces[[1L]]$trace_context, inner_context)
  expect_identical(traces[[2L]]$trace_context, outer_context)
})

test_that("trace annotation failures cannot leak invocation context", {
  context <- trace_context_fixture()
  program <- NULL
  forward <- function(text, ...) {
    malformed <- structure(
      list(timestamp = Sys.time()),
      class = "dsprrr_test_malformed_trace"
    )
    program$state$traces <- append(
      program$state$traces,
      list(malformed)
    )
    list(answer = text)
  }
  program <- module_fn("text -> answer", forward)
  assign(
    "[[<-.dsprrr_test_malformed_trace",
    function(x, i, value) stop("malformed trace"),
    envir = globalenv()
  )
  withr::defer(rm(
    "[[<-.dsprrr_test_malformed_trace",
    envir = globalenv()
  ))

  expect_error(
    program$run(
      text = "hello",
      .cache = FALSE,
      .trace_context = context
    ),
    "malformed trace"
  )
  expect_identical(dsprrr:::current_trace_context(), list())
  expect_identical(
    dsprrr:::current_trace_program_artifact_id(),
    NA_character_
  )
})

test_that("batch and evaluation surfaces retain row-aligned trace context", {
  context <- trace_context_fixture()
  program <- module_fn(
    "text -> answer",
    function(text, ...) list(answer = "ok")
  )

  batch <- run(
    program,
    text = c("first", "second"),
    .cache = FALSE,
    .progress = FALSE,
    .return_format = "structured",
    .trace_context = context
  )
  expect_length(batch, 2L)
  expect_true(all(vapply(
    batch,
    function(row) identical(row$metadata$trace_context, context),
    logical(1)
  )))

  evaluation <- evaluate(
    program,
    data = data.frame(text = c("third", "fourth"), answer = "ok"),
    metric = function(prediction, expected) 1,
    .progress = FALSE,
    .return_format = "structured",
    .trace_context = context
  )
  expect_identical(evaluation$trace_context, context)
  expect_true(all(vapply(
    evaluation$metadata,
    function(metadata) identical(metadata$trace_context, context),
    logical(1)
  )))
  expect_true(all(vapply(
    evaluation$traces,
    function(trace) identical(trace$trace_context, context),
    logical(1)
  )))
})

test_that("empty optimizer evaluations validate and retain trace context", {
  context <- trace_context_fixture()
  program <- module(signature("text -> answer"))
  data <- data.frame(text = character(), answer = character())

  result <- eval_program(
    program,
    data,
    metric = function(prediction, expected) 1,
    .trace_context = context
  )
  expect_identical(result@trace_context, context)
  expect_identical(dsprrr:::current_trace_context(), list())

  expect_error(
    eval_program(
      program,
      data,
      metric = function(prediction, expected) 1,
      .trace_context = list(api_token = "never")
    ),
    class = "dsprrr_trace_context_credential_error"
  )
})

test_that("trace context does not partition cache identity", {
  local_reset_cache()
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  local_trace_context_openai_backend(calls)
  base <- suppressWarnings(ellmer::chat_openai(
    api_key = "dummy-key",
    model = "trace-context-cache-model"
  ))
  program <- module(signature("text -> answer"))
  first_context <- trace_context_fixture()
  second_context <- trace_context_fixture()
  second_context$research_run_id <- "research-456"

  first <- run(
    program,
    text = "same request",
    .llm = base$clone(deep = TRUE),
    .cache = TRUE,
    .return_format = "structured",
    .trace_context = first_context
  )
  second <- suppressMessages(run(
    program,
    text = "same request",
    .llm = base$clone(deep = TRUE),
    .cache = TRUE,
    .return_format = "structured",
    .trace_context = second_context
  ))

  expect_identical(calls$n, 1L)
  expect_identical(first$metadata$cache, "miss")
  expect_identical(second$metadata$cache, "hit")
  expect_identical(first$metadata$trace_context, first_context)
  expect_identical(second$metadata$trace_context, second_context)
  expect_identical(
    vapply(
      tail(program$state$traces, 2L),
      function(trace) trace$metadata$cache,
      character(1)
    ),
    c("miss", "hit")
  )
  expect_identical(
    lapply(
      tail(dsprrr:::.dsprrr_env$prompt_history, 2L),
      `[[`,
      "trace_context"
    ),
    list(first_context, second_context)
  )
})

test_that("compile scopes trace context and rejects unsafe context", {
  context <- trace_context_fixture()
  program <- module(signature("text -> answer"))
  trainset <- data.frame(text = "question", answer = "response")

  compiled <- expect_no_error(
    compile(
      LabeledFewShot(k = 1L, sample = FALSE),
      program,
      trainset,
      .trace_context = context
    )
  )
  expect_true(compiled$is_compiled())
  expect_identical(dsprrr:::current_trace_context(), list())

  expect_error(
    compile(
      LabeledFewShot(k = 1L),
      program,
      trainset,
      .trace_context = list(access_token = "never")
    ),
    class = "dsprrr_trace_context_credential_error"
  )
})

test_that("ellmer tools propagate context without changing result schemas", {
  context <- trace_context_fixture()
  program <- module(signature("text -> answer"))
  tool <- as_ellmer_tool(
    program,
    name = "context_tool",
    .llm = trace_context_test_chat(),
    trace_context = context
  )

  result <- tool(text = "hello")
  trace <- tail(program$state$traces, 1L)[[1L]]

  expect_named(result, "answer")
  expect_false("trace_context" %in% names(result))
  expect_identical(trace$trace_context, context)
  expect_error(
    as_ellmer_tool(
      program,
      trace_context = list(credentials = "never")
    ),
    class = "dsprrr_trace_context_credential_error"
  )
})

test_that("Flex predictor events retain trace context", {
  context <- trace_context_fixture()
  source <- as.character(jsonlite::toJSON(
    list(
      schema_version = 1L,
      steps = list(list(
        name = "predict",
        primitive = "predict",
        signature = "$outer",
        inputs = list(question = "$input.question")
      )),
      outputs = list(answer = "$step.predict.answer")
    ),
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
  program <- suppressWarnings(flex(
    "question -> answer",
    module_src = source
  ))
  chat <- trace_context_test_chat(response = function(type) {
    fields <- names(type@properties)
    stats::setNames(as.list(rep("ok", length(fields))), fields)
  })

  result <- run(
    program,
    question = "test",
    .llm = chat,
    .cache = FALSE,
    .return_format = "structured",
    .trace_context = context
  )

  expect_identical(result$metadata$trace_context, context)
  expect_true(all(vapply(
    result$metadata$program_trace_events,
    function(event) identical(event$trace_context, context),
    logical(1)
  )))
  expect_true(length(program$state$traces) > 0L)
  expect_true(all(vapply(
    program$state$traces,
    function(trace) identical(trace$trace_context, context),
    logical(1)
  )))
})

test_that("RLM trajectories retain trace context", {
  skip_if_not_installed("callr")
  context <- trace_context_fixture()
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  chat_factory <- local({
    state <- new.env(parent = emptyenv())
    state$calls <- 0L
    function() {
      turns <- list()
      chat <- new_test_chat(
        model = "trace-context-rlm",
        chat_structured = function(prompt, type, ...) {
          state$calls <- state$calls + 1L
          turns <<- c(
            turns,
            list(
              ellmer::UserTurn(as.character(prompt)),
              ellmer::AssistantTurn(
                contents = list(ellmer::ContentText("action")),
                tokens = c(1L, 1L, 0L),
                cost = 0,
                duration = 0
              )
            )
          )
          list(reasoning = "finish", code = "SUBMIT('ok')")
        }
      )
      override_test_chat_method(chat, "clone", function(deep = FALSE) {
        chat_factory()
      })
      override_test_chat_method(chat, "get_turns", function(...) turns)
      override_test_chat_method(chat, "set_turns", function(value) {
        turns <<- value
        invisible(NULL)
      })
      chat
    }
  })
  program <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 1L,
    max_llm_calls = 0L
  )

  result <- run(
    program,
    question = "test",
    .llm = chat_factory(),
    .cache = FALSE,
    .return_format = "structured",
    .trace_context = context
  )

  expect_identical(result$metadata$trace_context, context)
  expect_true(length(program$state$traces) > 0L)
  expect_identical(tail(program$state$traces, 1L)[[1L]]$trace_context, context)
})
