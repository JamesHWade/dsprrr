react_history_fixture <- function() {
  old_calls <- new.env(parent = emptyenv())
  old_calls$n <- 0L
  old_tool <- ellmer::tool(
    function(query) {
      old_calls$n <- old_calls$n + 1L
      paste0("old:", query)
    },
    name = "lookup",
    description = "Prior lookup tool",
    arguments = list(query = ellmer::type_string())
  )
  request <- ellmer::ContentToolRequest(
    id = "prior-request",
    name = "lookup",
    arguments = list(query = "old query"),
    tool = old_tool
  )
  turns <- list(
    ellmer::UserTurn(list(ellmer::ContentText("prior question"))),
    ellmer::AssistantTurn(list(request)),
    ellmer::UserTurn(list(ellmer::ContentToolResult(
      value = list(matches = c("one", "two")),
      request = request
    ))),
    ellmer::AssistantTurn(list(ellmer::ContentText("prior answer")))
  )
  list(
    history = conversation_history(turns),
    old_calls = old_calls
  )
}

react_history_test_chat <- function() {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  tool <- ellmer::tool(
    function(query) {
      calls$n <- calls$n + 1L
      paste0("current:", query)
    },
    name = "lookup",
    description = "Current lookup tool",
    arguments = list(query = ellmer::type_string())
  )
  chat <- ellmer::chat_openai(
    system_prompt = "Keep this current system prompt",
    credentials = function() "test-key",
    model = "gpt-test"
  )
  chat$register_tool(tool)
  chat$set_turns(list(ellmer::UserTurn(list(
    ellmer::ContentText("existing caller-owned turn")
  ))))
  list(chat = chat, calls = calls)
}

react_history_function_call <- function(name, arguments, id) {
  list(
    id = paste0("fc_", id),
    type = "function_call",
    status = "completed",
    call_id = id,
    name = name,
    arguments = as.character(jsonlite::toJSON(
      arguments,
      auto_unbox = TRUE,
      null = "null"
    ))
  )
}

react_history_message <- function(text, id) {
  list(
    id = paste0("msg_", id),
    type = "message",
    status = "completed",
    role = "assistant",
    content = list(list(
      type = "output_text",
      annotations = list(),
      logprobs = list(),
      text = text
    ))
  )
}

local_react_history_backend <- function(replies, calls, .env = parent.frame()) {
  calls$n <- 0L
  calls$bodies <- list()
  testthat::local_mocked_bindings(
    req_perform = function(req) {
      calls$n <- calls$n + 1L
      if (calls$n > length(replies)) {
        cli::cli_abort(
          "Unexpected provider request in explicit history test",
          class = "test_react_history_unexpected_request"
        )
      }
      calls$bodies[[calls$n]] <- req$body$data
      body <- list(
        id = paste0("resp_", calls$n),
        object = "response",
        created_at = 1L,
        status = "completed",
        model = "model-a",
        output = replies[[calls$n]],
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
}

react_history_real_chat <- function() {
  suppressWarnings(ellmer::chat_openai(
    credentials = function() "history-test-key",
    model = "model-a"
  ))
}

test_that("explicit ReAct history seeds an isolated current Chat", {
  fixture <- react_history_fixture()
  original_history <- fixture$history
  current <- react_history_test_chat()
  original_turns <- current$chat$get_turns()
  original_tools <- current$chat$get_tools()

  prepared <- react_history_prepare(
    current$chat,
    fixture$history
  )

  expect_false(identical(prepared, current$chat))
  expect_identical(
    prepared$get_system_prompt(),
    "Keep this current system prompt"
  )
  expect_identical(prepared$get_tools(), original_tools)
  expect_identical(prepared$get_turns(), history_turns(fixture$history))
  expect_identical(current$chat$get_turns(), original_turns)
  expect_identical(fixture$old_calls$n, 0L)
  expect_identical(current$calls$n, 0L)
  expect_null(prepared$get_turns()[[2L]]@contents[[1L]]@tool)

  changed_turns <- prepared$get_turns()
  changed_turns[[1L]]@contents[[1L]]@text <- "changed on clone"
  prepared$set_turns(changed_turns)
  expect_identical(fixture$history, original_history)
  expect_identical(
    history_turns(fixture$history)[[1L]]@contents[[1L]]@text,
    "prior question"
  )
})

test_that("completed explicit ReAct turns append to the supplied history", {
  fixture <- react_history_fixture()
  current <- react_history_test_chat()
  prepared <- react_history_prepare(current$chat, fixture$history)
  added <- list(
    ellmer::UserTurn(list(ellmer::ContentText("new question"))),
    ellmer::AssistantTurn(
      list(ellmer::ContentText("new answer")),
      tokens = c(12, 3, 15),
      cost = 0.02,
      duration = 1.25
    )
  )
  prepared$set_turns(c(prepared$get_turns(), added))

  completed <- react_history_finish(prepared, fixture$history)

  expect_identical(
    completed,
    conversation_history(c(history_turns(fixture$history), added))
  )
  expect_identical(length(history_turns(completed)), 6L)
  restored_answer <- history_turns(completed)[[6L]]
  expect_identical(restored_answer@tokens, rep(NA_real_, 3L))
  expect_identical(restored_answer@cost, NA_real_)
  expect_identical(restored_answer@duration, NA_real_)
  expect_identical(fixture$old_calls$n, 0L)
  other_history <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("different question"))),
    ellmer::AssistantTurn(list(ellmer::ContentText("different answer")))
  ))
  expect_error(
    react_history_finish(prepared, other_history),
    class = "dsprrr_history_error"
  )
})

test_that("empty history opts into a reset clone and rejects other kinds", {
  current <- react_history_test_chat()
  original_turns <- current$chat$get_turns()
  empty <- conversation_history()
  prepared <- react_history_prepare(current$chat, empty)

  expect_length(prepared$get_turns(), 0L)
  expect_identical(current$chat$get_turns(), original_turns)
  expect_identical(react_history_finish(prepared, empty), empty)

  other_kind <- conversation_history()
  other_kind$kind <- "rlm"
  expect_error(
    react_history_prepare(current$chat, other_kind),
    class = "dsprrr_history_error"
  )
})

test_that("get_history reads and validates structured result metadata", {
  fixture <- react_history_fixture()
  result <- list(
    output = list(answer = "prior answer"),
    metadata = list(conversation_history = fixture$history)
  )

  retained <- get_history(result)

  expect_identical(retained, fixture$history)
  retained$kind <- "changed"
  expect_identical(fixture$history$kind, "react")
  expect_null(get_history(list(metadata = list())))
  expect_null(get_history(list(output = list(answer = "no history"))))
})

test_that("run resumes saved ReAct history without mutating the supplied Chat", {
  calls <- new.env(parent = emptyenv())
  local_react_history_backend(
    list(
      list(
        react_history_function_call(
          "media",
          list(query = "look"),
          "call_media"
        ),
        react_history_function_call(
          "explode",
          list(query = "fail"),
          "call_explode"
        )
      ),
      list(react_history_message("I checked the evidence.", 2L)),
      list(react_history_message('{"answer":"first"}', 3L)),
      list(react_history_message("I remember the evidence.", 4L)),
      list(react_history_message('{"answer":"second"}', 5L)),
      list(react_history_message("This is separate.", 6L)),
      list(react_history_message('{"answer":"separate"}', 7L))
    ),
    calls
  )

  tool_calls <- new.env(parent = emptyenv())
  tool_calls$media <- 0L
  tool_calls$explode <- 0L
  image_parts <- list(
    ellmer::ContentText("Caption from the tool"),
    ellmer::content_image_url("https://example.com/history.png"),
    ellmer::ContentImageInline("image/png", "YWJj"),
    ellmer::ContentPDF("application/pdf", "ZGVm", "history.pdf")
  )
  tools <- list(
    ellmer::tool(
      function(query) {
        tool_calls$media <- tool_calls$media + 1L
        image_parts
      },
      name = "media",
      description = "Return mixed media",
      arguments = list(query = ellmer::type_string())
    ),
    ellmer::tool(
      function(query) {
        tool_calls$explode <- tool_calls$explode + 1L
        stop("deliberate tool failure: ", query)
      },
      name = "explode",
      description = "Return a tool error",
      arguments = list(query = ellmer::type_string())
    )
  )
  agent <- react("question -> answer", tools = tools)
  chat <- react_history_real_chat()
  original_turns <- chat$get_turns()
  original_tools <- chat$get_tools()

  first <- suppressWarnings(run(
    agent,
    question = "Inspect the evidence",
    .llm = chat,
    .history = conversation_history(),
    .return_format = "structured"
  ))
  first_history <- get_history(first)
  saved_path <- tempfile(fileext = ".json")
  save_history(first_history, saved_path)
  restored_history <- load_history(saved_path)
  original_history <- restored_history

  second <- run(
    agent,
    question = "What did you find?",
    .llm = chat,
    .history = restored_history,
    .return_format = "structured"
  )
  separate <- run(
    agent,
    question = "A different question",
    .llm = chat,
    .history = conversation_history(),
    .return_format = "structured"
  )

  expect_identical(first$output, list(answer = "first"))
  expect_identical(second$output, list(answer = "second"))
  expect_identical(separate$output, list(answer = "separate"))
  expect_identical(first$metadata$conversation_history, first_history)
  expect_identical(length(history_turns(get_history(second))), 10L)
  expect_identical(chat$get_turns(), original_turns)
  expect_identical(chat$get_tools(), original_tools)
  expect_identical(tool_calls$media, 1L)
  expect_identical(tool_calls$explode, 1L)
  expect_identical(restored_history, original_history)
  expect_identical(unname(second$metadata$input_tokens), 10)
  expect_identical(unname(second$metadata$output_tokens), 4)
  expect_identical(unname(second$metadata$total_tokens), 14)

  turns <- history_turns(first_history)
  requests <- unlist(
    lapply(turns, function(turn) {
      Filter(
        function(content) inherits(content, "ellmer::ContentToolRequest"),
        turn@contents
      )
    }),
    recursive = FALSE
  )
  results <- unlist(
    lapply(turns, function(turn) {
      Filter(
        function(content) inherits(content, "ellmer::ContentToolResult"),
        turn@contents
      )
    }),
    recursive = FALSE
  )
  expect_identical(
    vapply(requests, function(request) request@id, character(1)),
    c("fc_call_media", "fc_call_explode")
  )
  expect_identical(
    vapply(results, function(result) result@request@id, character(1)),
    c("fc_call_media", "fc_call_explode")
  )
  media_result <- results[[match(
    "fc_call_media",
    vapply(
      results,
      function(result) result@request@id,
      character(1)
    )
  )]]
  expect_identical(media_result@value, image_parts)
  error_result <- results[[match(
    "fc_call_explode",
    vapply(
      results,
      function(result) result@request@id,
      character(1)
    )
  )]]
  expect_match(error_result@error, "deliberate tool failure")

  continued_request <- jsonlite::toJSON(
    calls$bodies[[4L]],
    auto_unbox = TRUE,
    null = "null"
  )
  separate_request <- jsonlite::toJSON(
    calls$bodies[[6L]],
    auto_unbox = TRUE,
    null = "null"
  )
  expect_match(continued_request, "call_media", fixed = TRUE)
  expect_match(continued_request, "history.png", fixed = TRUE)
  expect_match(separate_request, "A different question", fixed = TRUE)
  expect_no_match(separate_request, "call_media")
  expect_identical(calls$n, 7L)
})

test_that("explicit history failures preserve caller history and Chat", {
  fixture <- react_history_fixture()
  history_before <- fixture$history
  chat <- react_history_real_chat()
  turns_before <- chat$get_turns()
  request_started <- 0L
  remove <- chat$on_request_start(function(turns) {
    request_started <<- request_started + 1L
    rlang::abort("Stop before transport", class = "test_history_boundary")
  })
  on.exit(remove(), add = TRUE)

  condition <- rlang::catch_cnd(run(
    react("question -> answer"),
    question = "This provider request will fail",
    .llm = chat,
    .history = fixture$history,
    .return_format = "structured"
  ))

  expect_s3_class(condition, "dsprrr_provider_error")
  expect_identical(request_started, 1L)
  expect_identical(fixture$history, history_before)
  expect_identical(chat$get_turns(), turns_before)
})

test_that("explicit history retains paired typed submit turns", {
  calls <- new.env(parent = emptyenv())
  local_react_history_backend(
    list(
      list(react_history_function_call(
        "submit",
        list(answer = "first"),
        "submit_first"
      )),
      list(react_history_function_call(
        "submit",
        list(answer = "second"),
        "submit_second"
      ))
    ),
    calls
  )
  chat <- react_history_real_chat()
  agent <- react(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_object(answer = ellmer::type_string())
    ),
    finalization = "submit"
  )

  first <- run(
    agent,
    question = "First submitted answer",
    .llm = chat,
    .history = conversation_history(),
    .return_format = "structured"
  )
  second <- run(
    agent,
    question = "Second submitted answer",
    .llm = chat,
    .history = get_history(first),
    .return_format = "structured"
  )

  expect_identical(first$output, list(answer = "first"))
  expect_identical(second$output, list(answer = "second"))
  expect_identical(first$metadata$termination_reason, "submitted")
  expect_identical(second$metadata$termination_reason, "submitted")
  expect_identical(length(history_turns(get_history(second))), 6L)
  expect_identical(calls$n, 2L)

  turns <- history_turns(get_history(second))
  requests <- unlist(
    lapply(turns, function(turn) {
      Filter(
        function(content) inherits(content, "ellmer::ContentToolRequest"),
        turn@contents
      )
    }),
    recursive = FALSE
  )
  results <- unlist(
    lapply(turns, function(turn) {
      Filter(
        function(content) inherits(content, "ellmer::ContentToolResult"),
        turn@contents
      )
    }),
    recursive = FALSE
  )
  expect_length(requests, 2L)
  expect_length(results, 2L)
  expect_identical(
    vapply(requests, function(request) request@id, character(1)),
    c("fc_submit_first", "fc_submit_second")
  )
  expect_identical(
    vapply(results, function(result) result@request@id, character(1)),
    c("fc_submit_first", "fc_submit_second")
  )
})

test_that("explicit history is structured, scalar, and root ReAct only", {
  calls <- new.env(parent = emptyenv())
  local_react_history_backend(list(), calls)
  history <- conversation_history()
  chat <- react_history_real_chat()
  react_module <- react("question -> answer")
  predict_module <- module(signature("question -> answer"))
  custom_module <- structure(list(), class = "unregistered_run_module")
  request_one <- ellmer::ContentToolRequest(
    id = "duplicate-check-one",
    name = "lookup",
    arguments = list(query = "one")
  )
  request_two <- ellmer::ContentToolRequest(
    id = "duplicate-check-two",
    name = "lookup",
    arguments = list(query = "two")
  )
  malformed <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("before"))),
    ellmer::AssistantTurn(list(request_one)),
    ellmer::UserTurn(list(ellmer::ContentToolResult(
      value = "one",
      request = request_one
    ))),
    ellmer::AssistantTurn(list(request_two)),
    ellmer::UserTurn(list(ellmer::ContentToolResult(
      value = "two",
      request = request_two
    )))
  ))
  malformed$turns[[4L]]$contents[[1L]]$id <- request_one@id
  malformed$turns[[5L]]$contents[[1L]]$request$id <- request_one@id

  expect_error(
    run(
      react_module,
      question = "no simple state",
      .llm = chat,
      .history = history
    ),
    class = "dsprrr_history_result_format_error"
  )
  expect_error(
    run(
      predict_module,
      question = "unsupported root",
      .llm = chat,
      .history = history,
      .return_format = "structured"
    ),
    class = "dsprrr_history_unsupported_module"
  )
  expect_error(
    run(custom_module, .history = history),
    class = "dsprrr_history_unsupported_module"
  )
  expect_error(
    run(
      react_module,
      question = "duplicate IDs",
      .llm = chat,
      .history = malformed,
      .return_format = "structured"
    ),
    class = "dsprrr_history_error"
  )
  expect_error(
    run(
      react_module,
      question = c("first", "second"),
      .llm = chat,
      .history = history,
      .return_format = "structured"
    ),
    class = "dsprrr_history_batch_error"
  )
  expect_error(
    react_module$run(
      question = c("first", "second"),
      .llm = chat,
      .history = history,
      .return_format = "structured"
    ),
    class = "dsprrr_history_batch_error"
  )
  expect_error(
    run_dataset(
      react_module,
      data.frame(question = character()),
      .llm = chat,
      .history = history
    ),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_error(
    run_async(react_module, question = "async", .history = history),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_error(
    react_module$run_async(question = "async", .history = history),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_error(
    stream_async(react_module, question = "stream", .history = history),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_error(
    react_module$stream_async(question = "stream", .history = history),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_error(
    react_module$stream(question = "stream", .history = history),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_error(
    run_stream(react_module, question = "stream", .history = history),
    class = "dsprrr_history_unsupported_entrypoint"
  )
  expect_identical(calls$n, 0L)
  expect_length(chat$get_turns(), 0L)
})

test_that("history is never implicit when ReAct keeps legacy Chat behavior", {
  calls <- new.env(parent = emptyenv())
  local_react_history_backend(
    list(
      list(react_history_message("No tools needed.", 1L)),
      list(react_history_message('{"answer":"legacy"}', 2L))
    ),
    calls
  )
  chat <- react_history_real_chat()

  result <- run(
    react("question -> answer"),
    question = "Use existing default behavior",
    .llm = chat,
    .return_format = "structured"
  )

  expect_identical(result$output, list(answer = "legacy"))
  expect_false("conversation_history" %in% names(result$metadata))
  expect_length(chat$get_turns(), 4L)
})

test_that("direct ReAct forward returns history without recording a trace", {
  calls <- new.env(parent = emptyenv())
  local_react_history_backend(
    list(
      list(react_history_message("Direct response.", 1L)),
      list(react_history_message('{"answer":"direct"}', 2L)),
      list(react_history_message("Module run response.", 3L)),
      list(react_history_message('{"answer":"continued"}', 4L))
    ),
    calls
  )
  chat <- react_history_real_chat()
  agent <- react("question -> answer")

  result <- agent$forward(
    list(question = "Direct forward"),
    .llm = chat,
    trace = FALSE,
    .history = conversation_history()
  )

  expect_identical(result$output[[1L]], list(answer = "direct"))
  expect_s3_class(result$metadata[[1L]]$conversation_history, "dsprrr_history")
  expect_length(agent$state$traces, 0L)

  continued <- agent$run(
    question = "Continue with module$run",
    .llm = chat,
    .history = result$metadata[[1L]]$conversation_history,
    .return_format = "structured"
  )

  expect_identical(continued$output, list(answer = "continued"))
  expect_identical(names(continued), c("output", "chat", "metadata"))
  expect_length(
    history_turns(continued$metadata$conversation_history),
    8L
  )
  expect_length(agent$state$traces, 1L)
  expect_length(chat$get_turns(), 0L)
  expect_identical(calls$n, 4L)
})
