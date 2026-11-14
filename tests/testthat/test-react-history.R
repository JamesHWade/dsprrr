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

test_that("explicit ReAct history seeds an isolated current Chat", {
  fixture <- react_history_fixture()
  original_history <- fixture$history
  current <- react_history_test_chat()
  original_turns <- current$chat$get_turns()
  original_tools <- current$chat$get_tools()

  prepared <- dsprrr:::react_history_prepare(
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
  prepared <- dsprrr:::react_history_prepare(current$chat, fixture$history)
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

  completed <- dsprrr:::react_history_finish(prepared, fixture$history)

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
    dsprrr:::react_history_finish(prepared, other_history),
    class = "dsprrr_history_error"
  )
})

test_that("empty history opts into a reset clone and rejects other kinds", {
  current <- react_history_test_chat()
  original_turns <- current$chat$get_turns()
  empty <- conversation_history()
  prepared <- dsprrr:::react_history_prepare(current$chat, empty)

  expect_length(prepared$get_turns(), 0L)
  expect_identical(current$chat$get_turns(), original_turns)
  expect_identical(dsprrr:::react_history_finish(prepared, empty), empty)

  other_kind <- conversation_history()
  other_kind$kind <- "rlm"
  expect_error(
    dsprrr:::react_history_prepare(current$chat, other_kind),
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
