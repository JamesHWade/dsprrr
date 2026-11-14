react_async_test_function_call <- function(name, arguments, id) {
  if (!is.character(arguments) || length(arguments) != 1L) {
    arguments <- as.character(jsonlite::toJSON(
      arguments,
      auto_unbox = TRUE,
      null = "null"
    ))
  }
  list(
    id = paste0("fc_", id),
    type = "function_call",
    status = "completed",
    call_id = paste0("call_", id),
    name = name,
    arguments = arguments
  )
}

react_async_test_message <- function(text) {
  list(
    id = "msg_1",
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

local_react_async_backend <- function(responses, state, control = NULL) {
  state$count <- 0L
  state$bodies <- list()
  withr::local_options(
    httr2_mock = function(req) {
      state$count <- state$count + 1L
      state$bodies[[state$count]] <- req$body$data
      if (!is.null(control) && identical(state$cancel_on_transport, TRUE)) {
        control$cancel("cancelled while provider request was pending")
      }
      body <- list(
        id = paste0("resp_", state$count),
        object = "response",
        created_at = 1L,
        status = "completed",
        model = "react-async-model",
        output = responses[[state$count]],
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
          body,
          auto_unbox = TRUE,
          null = "null"
        ))
      )
    },
    .local_envir = parent.frame()
  )
  invisible(state)
}

react_async_test_chat <- function() {
  suppressWarnings(ellmer::chat_openai(
    api_key = "mock-react-async-key",
    base_url = "http://127.0.0.1:9/v1",
    model = "react-async-model"
  ))
}

react_async_test_settle <- function(promise, timeout = 10) {
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
    stop("async ReAct test promise did not settle", call. = FALSE)
  }
  list(value = state$value, error = state$error)
}

react_async_test_run <- function(
  module,
  inputs,
  chat,
  return_format = "structured",
  control = NULL
) {
  args <- c(
    list(module),
    inputs,
    list(.llm = chat, .return_format = return_format, .control = control)
  )
  do.call(run_async, args)
}

test_that("ReAct async uses normalized media and awaits structured followup", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(
      list(react_async_test_message("I reviewed the image.")),
      list(react_async_test_message('{"description":"an image"}'))
    ),
    state
  )
  chat <- react_async_test_chat()
  image <- ellmer::ContentImageRemote("https://example.com/image.png")
  module <- react(
    signature(
      inputs = list(input("image")),
      output_type = ellmer::type_object(
        description = ellmer::type_string()
      )
    )
  )

  promise <- react_async_test_run(
    module,
    list(image = image),
    chat
  )
  outcome <- react_async_test_settle(promise)

  expect_null(outcome$error)
  expect_s3_class(outcome$value, "dsprrr_result")
  expect_identical(outcome$value$output, list(description = "an image"))
  expect_identical(outcome$value$metadata$backend, "async")
  expect_identical(outcome$value$metadata$cache, "bypass")
  expect_identical(outcome$value$metadata$finalization, "structured-followup")
  expect_identical(outcome$value$metadata$termination_reason, "structured")
  expect_identical(state$count, 2L)
  expect_identical(
    tail(.dsprrr_env$prompt_history, 1L)[[1L]]$source,
    "ReactModule"
  )
  first_input <- state$bodies[[1L]]$input
  expect_length(first_input, 2L)
  expect_identical(first_input[[1L]]$content[[1L]]$type, "input_text")
  expect_identical(first_input[[2L]]$content[[1L]]$type, "input_image")
  expect_identical(
    first_input[[2L]]$content[[1L]]$image_url,
    "https://example.com/image.png"
  )
  expect_false(any(vapply(
    first_input,
    function(item) {
      any(vapply(
        item$content,
        function(content) identical(content$text, "echo"),
        logical(1)
      ))
    },
    logical(1)
  )))
  expect_no_error(assert_async_chat_available(chat))
})

test_that("ReAct async executes sequential tools and emits ordered progress", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(
      list(
        react_async_test_function_call(
          "first_tool",
          list(value = "first"),
          1L
        ),
        react_async_test_function_call(
          "second_tool",
          list(value = "second"),
          2L
        )
      ),
      list(react_async_test_message("Both tools completed.")),
      list(react_async_test_message('{"answer":"done"}'))
    ),
    state
  )
  chat <- react_async_test_chat()
  tool_order <- character()
  events <- list()
  tools <- list(
    ellmer::tool(
      function(value) {
        tool_order <<- c(tool_order, paste0("first:", value))
        value
      },
      name = "first_tool",
      description = "Record the first ordered call.",
      arguments = list(value = ellmer::type_string())
    ),
    ellmer::tool(
      function(value) {
        tool_order <<- c(tool_order, paste0("second:", value))
        value
      },
      name = "second_tool",
      description = "Record the second ordered call.",
      arguments = list(value = ellmer::type_string())
    )
  )
  control <- react_async_control(function(event) {
    events[[length(events) + 1L]] <<- event
  })
  module <- react(
    "question -> answer",
    tools = tools,
    finalization = "structured"
  )

  promise <- react_async_test_run(
    module,
    list(question = "Run the tools"),
    chat,
    control = control
  )
  outcome <- react_async_test_settle(promise)

  expect_null(outcome$error)
  expect_identical(outcome$value$output, list(answer = "done"))
  expect_identical(tool_order, c("first:first", "second:second"))
  expect_identical(state$count, 3L)
  expect_identical(outcome$value$metadata$iterations, 1L)
  expect_length(outcome$value$metadata$tool_calls, 2L)
  expect_true(all(vapply(
    outcome$value$metadata$tool_calls,
    function(call) identical(call$status, "result") && !call$is_error,
    logical(1)
  )))
  expect_length(outcome$value$metadata$observed_tool_results, 2L)
  expect_identical(
    vapply(outcome$value$metadata$tool_calls, `[[`, character(1), "tool_name"),
    c("first_tool", "second_tool")
  )
  event_types <- vapply(events, `[[`, character(1), "type")
  expect_true(all(diff(vapply(events, `[[`, integer(1), "sequence")) > 0L))
  expect_true(all(
    c(
      "provider_request_start",
      "provider_request_end",
      "tool_start",
      "tool_end",
      "finalization_start",
      "finalization_end",
      "complete"
    ) %in%
      event_types
  ))
  expect_false(any(
    c("prompt", "arguments", "reasoning") %in% names(events[[1L]])
  ))
  expect_length(chat$get_tools(), 0L)
  expect_no_error(assert_async_chat_available(chat))
})

test_that("submit finalization returns directly and pairs its tool result", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(list(react_async_test_function_call(
      "submit",
      list(answer = "done"),
      1L
    ))),
    state
  )
  chat <- react_async_test_chat()
  module <- react("question -> answer", finalization = "submit")

  promise <- react_async_test_run(
    module,
    list(question = "Answer this"),
    chat
  )
  outcome <- react_async_test_settle(promise)

  expect_null(outcome$error)
  expect_identical(outcome$value$output, list(answer = "done"))
  expect_identical(outcome$value$metadata$finalization, "submit")
  expect_identical(outcome$value$metadata$termination_reason, "submitted")
  expect_identical(state$count, 1L)
  turns <- outcome$value$metadata$history
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
  expect_length(requests, 1L)
  expect_length(results, 1L)
  expect_identical(results[[1L]]@request@id, requests[[1L]]@id)
  expect_identical(results[[1L]]@request@name, "submit")
  expect_length(outcome$value$metadata$observed_tool_results, 1L)
  expect_identical(
    outcome$value$metadata$observed_tool_results[[1L]]$result,
    results[[1L]]
  )
  expect_length(chat$get_tools(), 0L)
})

test_that("invalid submit repairs within the async iteration limit", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(
      list(react_async_test_function_call(
        "submit",
        list(status = "invalid"),
        1L
      )),
      list(react_async_test_function_call(
        "submit",
        list(status = "valid"),
        2L
      ))
    ),
    state
  )
  chat <- react_async_test_chat()
  module <- react(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_object(
        status = ellmer::type_enum(c("valid", "unknown"))
      )
    ),
    finalization = "submit",
    max_iterations = 2L
  )

  expect_warning(
    outcome <- react_async_test_settle(react_async_test_run(
      module,
      list(question = "Submit a valid value"),
      chat
    )),
    "Failed to evaluate 1 tool call"
  )

  expect_null(outcome$error)
  expect_identical(outcome$value$output, list(status = "valid"))
  expect_identical(outcome$value$metadata$iterations, 2L)
  expect_identical(state$count, 2L)
  expect_true(outcome$value$metadata$tool_calls[[1L]]$is_error)
  expect_identical(outcome$value$metadata$tool_calls[[1L]]$status, "result")
  expect_false(outcome$value$metadata$tool_calls[[2L]]$is_error)
  expect_identical(outcome$value$metadata$tool_calls[[2L]]$status, "result")
  expect_length(outcome$value$metadata$observed_tool_results, 2L)
  expect_match(
    conditionMessage(
      outcome$value$metadata$observed_tool_results[[1L]]$result@error
    ),
    "does not match the declared schema"
  )
  repair <- Filter(
    function(item) identical(item$type, "function_call_output"),
    state$bodies[[2L]]$input
  )
  expect_length(repair, 1L)
  expect_match(repair[[1L]]$output, "must be one of")
})

test_that("async submit rejects raw array scalars and repairs with a singleton array", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(
      list(react_async_test_function_call(
        "submit",
        '{"value":"raw scalar"}',
        1L
      )),
      list(react_async_test_function_call(
        "submit",
        '{"value":["one item"]}',
        2L
      ))
    ),
    state
  )
  chat <- react_async_test_chat()
  module <- react(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_array(ellmer::type_string())
    ),
    finalization = "submit",
    max_iterations = 2L
  )

  expect_warning(
    outcome <- react_async_test_settle(react_async_test_run(
      module,
      list(question = "Submit an array"),
      chat
    )),
    "Failed to evaluate 1 tool call"
  )

  expect_null(outcome$error)
  expect_identical(outcome$value$output, list("one item"))
  expect_identical(outcome$value$metadata$iterations, 2L)
  expect_identical(outcome$value$metadata$finalization, "submit")
  expect_identical(outcome$value$metadata$termination_reason, "submitted")
  expect_identical(state$count, 2L)
  tool_calls <- outcome$value$metadata$tool_calls
  expect_identical(
    vapply(tool_calls, `[[`, character(1), "tool_id"),
    c("fc_1", "fc_2")
  )
  expect_identical(
    vapply(tool_calls, `[[`, character(1), "status"),
    c("result", "result")
  )
  expect_identical(
    vapply(tool_calls, `[[`, logical(1), "is_error"),
    c(TRUE, FALSE)
  )
  observed <- outcome$value$metadata$observed_tool_results
  expect_identical(
    vapply(observed, function(item) item$result@request@id, character(1)),
    c("fc_1", "fc_2")
  )
  expect_match(conditionMessage(observed[[1L]]$result@error), "array")
  expect_null(observed[[2L]]$result@error)
  repair <- Filter(
    function(item) identical(item$type, "function_call_output"),
    state$bodies[[2L]]$input
  )
  expect_length(repair, 1L)
  expect_identical(repair[[1L]]$call_id, "fc_1")
  expect_match(tolower(repair[[1L]]$output), "array")
  expect_length(chat$get_tools(), 0L)
  expect_no_error(assert_async_chat_available(chat))
})

test_that("async submit completes an empty object without another request", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(
      list(react_async_test_function_call("submit", "{}", 1L))
    ),
    state
  )
  chat <- react_async_test_chat()
  module <- react(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_object()
    ),
    finalization = "submit"
  )

  outcome <- react_async_test_settle(react_async_test_run(
    module,
    list(question = "Return an empty object"),
    chat
  ))

  expect_null(outcome$error)
  expect_identical(outcome$value$output, stats::setNames(list(), character()))
  expect_identical(outcome$value$metadata$finalization, "submit")
  expect_identical(outcome$value$metadata$termination_reason, "submitted")
  expect_identical(state$count, 1L)
  expect_identical(outcome$value$metadata$iterations, 1L)
  expect_length(outcome$value$metadata$tool_calls, 1L)
  expect_identical(outcome$value$metadata$tool_calls[[1L]]$tool_id, "fc_1")
  expect_identical(outcome$value$metadata$tool_calls[[1L]]$status, "result")
  expect_false(outcome$value$metadata$tool_calls[[1L]]$is_error)
  expect_length(outcome$value$metadata$observed_tool_results, 1L)
  expect_identical(
    outcome$value$metadata$observed_tool_results[[1L]]$result@request@id,
    "fc_1"
  )
  expect_length(chat$get_tools(), 0L)
  expect_no_error(assert_async_chat_available(chat))
})

test_that("async submit preserves numeric precision in the paired tool result", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  accepted_value <- 0.000000123456789012345
  local_react_async_backend(
    list(list(react_async_test_function_call(
      "submit",
      '{"value":0.000000123456789012345}',
      1L
    ))),
    state
  )
  chat <- react_async_test_chat()
  module <- react(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_number()
    ),
    finalization = "submit"
  )

  outcome <- react_async_test_settle(react_async_test_run(
    module,
    list(question = "Keep the accepted numeric value exact"),
    chat
  ))

  expect_null(outcome$error)
  expect_identical(outcome$value$output, accepted_value)
  expect_identical(state$count, 1L)
  expect_identical(outcome$value$metadata$termination_reason, "submitted")
  expect_length(outcome$value$metadata$observed_tool_results, 1L)
  observed <- outcome$value$metadata$observed_tool_results[[1L]]
  expect_identical(observed$tool_id, "fc_1")
  expect_identical(observed$result@request@id, "fc_1")
  observed_value <- jsonlite::fromJSON(
    observed$result@value,
    simplifyVector = FALSE
  )
  expect_identical(observed_value, accepted_value)

  turn_contents <- unlist(
    lapply(chat$get_turns(), function(turn) turn@contents),
    recursive = FALSE
  )
  retained_results <- Filter(
    function(content) inherits(content, "ellmer::ContentToolResult"),
    turn_contents
  )
  expect_length(retained_results, 1L)
  retained_result <- retained_results[[1L]]
  expect_identical(retained_result, observed$result)
  expect_identical(retained_result@request@id, observed$tool_id)
  retained_value <- jsonlite::fromJSON(
    retained_result@value,
    simplifyVector = FALSE
  )
  expect_identical(retained_value, observed_value)
  expect_identical(retained_value, outcome$value$output)
  expect_identical(retained_value, accepted_value)
  expect_length(chat$get_tools(), 0L)
  expect_no_error(assert_async_chat_available(chat))
})

test_that("mixed submit requests fail before async tool side effects", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(list(
      react_async_test_function_call("submit", list(value = "done"), 1L),
      react_async_test_function_call("side_effect", list(value = "run"), 2L)
    )),
    state
  )
  chat <- react_async_test_chat()
  side_effect <- FALSE
  side_tool <- ellmer::tool(
    function(value) {
      side_effect <<- TRUE
      value
    },
    name = "side_effect",
    description = "A side effect for this test.",
    arguments = list(value = ellmer::type_string())
  )
  module <- react(
    "question -> answer",
    tools = list(side_tool),
    finalization = "submit"
  )

  outcome <- react_async_test_settle(react_async_test_run(
    module,
    list(question = "Do not execute either tool"),
    chat
  ))

  expect_s3_class(outcome$error, "dsprrr_react_submit_mixed_tools")
  expect_false(side_effect)
  expect_identical(state$count, 1L)
  expect_length(chat$get_tools(), 0L)
})

test_that("duplicate tool request IDs fail before any tool side effects", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  first <- react_async_test_function_call(
    "side_effect",
    list(value = "one"),
    1L
  )
  second <- react_async_test_function_call(
    "side_effect",
    list(value = "two"),
    1L
  )
  local_react_async_backend(list(list(first, second)), state)
  chat <- react_async_test_chat()
  calls <- 0L
  tool <- ellmer::tool(
    function(value) {
      calls <<- calls + 1L
      value
    },
    name = "side_effect",
    description = "A tool that must not run on duplicate request IDs.",
    arguments = list(value = ellmer::type_string())
  )
  module <- react("question -> answer", tools = list(tool))

  outcome <- react_async_test_settle(react_async_test_run(
    module,
    list(question = "Reject duplicate IDs"),
    chat
  ))

  expect_s3_class(outcome$error, "dsprrr_react_tool_id_error")
  expect_identical(calls, 0L)
  expect_identical(state$count, 1L)
  expect_length(module$state$traces[[1L]]$metadata$tool_calls, 0L)
  expect_length(module$state$traces[[1L]]$metadata$observed_tool_results, 0L)
})

test_that("cooperative cancellation blocks tools and later provider requests", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  state$cancel_on_transport <- TRUE
  control <- react_async_control()
  local_react_async_backend(
    list(list(react_async_test_function_call(
      "side_effect",
      list(value = "run"),
      1L
    ))),
    state,
    control
  )
  chat <- react_async_test_chat()
  side_effect <- FALSE
  side_tool <- ellmer::tool(
    function(value) {
      side_effect <<- TRUE
      value
    },
    name = "side_effect",
    description = "A side effect for cancellation testing.",
    arguments = list(value = ellmer::type_string())
  )
  module <- react("question -> answer", tools = list(side_tool))

  promise <- react_async_test_run(
    module,
    list(question = "Cancel during the provider request"),
    chat,
    control = control
  )
  outcome <- react_async_test_settle(promise)

  expect_s3_class(outcome$error, "dsprrr_react_async_cancelled")
  expect_identical(
    outcome$error$cancellation_reason,
    "cancelled while provider request was pending"
  )
  expect_false(side_effect)
  expect_identical(state$count, 1L)
  expect_length(chat$get_tools(), 0L)
  expect_identical(
    module$state$traces[[1L]]$metadata$tool_calls[[1L]]$status,
    "requested"
  )
  expect_length(module$state$traces[[1L]]$metadata$observed_tool_results, 0L)
  expect_length(module$state$traces[[1L]]$metadata$tools_used, 0L)
  expect_no_error(assert_async_chat_available(chat))
})

test_that("cancellation after a tool blocks the next provider request", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(
      list(react_async_test_function_call(
        "side_effect",
        list(value = "run"),
        1L
      )),
      list(react_async_test_function_call("submit", list(value = "done"), 2L))
    ),
    state
  )
  chat <- react_async_test_chat()
  side_effect <- FALSE
  control <- NULL
  control <- react_async_control(function(event) {
    if (identical(event$type, "tool_end")) {
      control$cancel("cancel after the tool finished")
    }
  })
  side_tool <- ellmer::tool(
    function(value) {
      side_effect <<- TRUE
      value
    },
    name = "side_effect",
    description = "A side effect for cancellation testing.",
    arguments = list(value = ellmer::type_string())
  )
  module <- react(
    "question -> answer",
    tools = list(side_tool),
    finalization = "submit"
  )

  outcome <- react_async_test_settle(react_async_test_run(
    module,
    list(question = "Cancel after the tool"),
    chat,
    control = control
  ))

  expect_s3_class(outcome$error, "dsprrr_react_async_cancelled")
  expect_true(side_effect)
  expect_identical(state$count, 1L)
  expect_length(chat$get_tools(), 0L)
  expect_length(module$state$traces[[1L]]$metadata$tool_calls, 1L)
  expect_identical(
    module$state$traces[[1L]]$metadata$tool_calls[[1L]]$status,
    "result"
  )
  observed <- module$state$traces[[1L]]$metadata$observed_tool_results
  expect_length(observed, 1L)
  expect_identical(observed[[1L]]$tool_id, "fc_1")
  expect_s3_class(observed[[1L]]$result, "ellmer::ContentToolResult")
  expect_identical(observed[[1L]]$result@request@id, "fc_1")
  expect_identical(observed[[1L]]$result@value, "run")
  expect_identical(
    module$state$traces[[1L]]$metadata$tools_used,
    "side_effect"
  )
})

test_that("cancellation before structured followup prevents its request", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(list(react_async_test_message("I have an answer."))),
    state
  )
  chat <- react_async_test_chat()
  control <- NULL
  control <- react_async_control(function(event) {
    if (identical(event$type, "finalization_start")) {
      control$cancel("cancel before followup")
    }
  })
  module <- react("question -> answer")

  outcome <- react_async_test_settle(react_async_test_run(
    module,
    list(question = "Do not make the follow-up request"),
    chat,
    control = control
  ))

  expect_s3_class(outcome$error, "dsprrr_react_async_cancelled")
  expect_identical(state$count, 1L)
})

test_that("async control is single-use and progress errors are traced", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(
      list(react_async_test_message("A first answer.")),
      list(react_async_test_message('{"answer":"first answer"}'))
    ),
    state
  )
  chat <- react_async_test_chat()
  module <- react("question -> answer")
  control <- react_async_control()
  pending <- react_async_test_run(
    module,
    list(question = "First"),
    chat,
    control = control
  )
  reused <- rlang::catch_cnd(react_async_test_run(
    module,
    list(question = "Second"),
    chat,
    control = control
  ))

  expect_s3_class(reused, "dsprrr_react_async_control_reused")
  outcome <- react_async_test_settle(pending)
  expect_null(outcome$error)

  error_state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(list(react_async_test_message("A first answer."))),
    error_state
  )
  error_chat <- react_async_test_chat()
  error_module <- react("question -> answer")
  error_control <- react_async_control(function(event) {
    if (identical(event$type, "provider_request_start")) {
      stop("progress callback failed", call. = FALSE)
    }
  })
  error_promise <- react_async_test_run(
    error_module,
    list(question = "Progress error"),
    error_chat,
    control = error_control
  )
  error_outcome <- react_async_test_settle(error_promise)

  expect_match(
    conditionMessage(error_outcome$error),
    "progress callback failed"
  )
  expect_length(error_module$state$traces, 1L)
  expect_identical(
    error_module$state$traces[[1L]]$metadata$error_stage,
    "provider"
  )
  expect_no_error(assert_async_chat_available(error_chat))
})

test_that("Module$run_async forwards controls and cancellation returns a promise", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(list(react_async_test_message("Should not be requested."))),
    state
  )
  chat <- react_async_test_chat()
  module <- react("question -> answer")
  control <- react_async_control()
  control$cancel("cancel before provider launch")

  promise <- module$run_async(
    question = "Cancel before launch",
    .llm = chat,
    .control = control
  )
  outcome <- react_async_test_settle(promise)

  expect_s3_class(promise, "promise")
  expect_s3_class(outcome$error, "dsprrr_react_async_cancelled")
  expect_identical(state$count, 0L)
  expect_length(module$state$traces, 1L)
  expect_identical(module$state$traces[[1L]]$metadata$error_stage, "cancelled")
  expect_no_error(assert_async_chat_available(chat))
})

test_that("ReAct async rejects vector batches before provider work", {
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(list(react_async_test_message("Should not be requested."))),
    state
  )
  chat <- react_async_test_chat()
  react_module <- react("question -> answer")
  error <- rlang::catch_cnd(run_async(
    react_module,
    question = c("first", "second"),
    .llm = chat
  ))

  expect_s3_class(error, "dsprrr_react_async_batch_unsupported")
  expect_identical(state$count, 0L)
  expect_no_error(assert_async_chat_available(chat))

  predict_module <- module(signature("question -> answer"))
  unsupported <- rlang::catch_cnd(run_async(
    predict_module,
    question = "Do not accept ReAct control",
    .control = react_async_control()
  ))
  expect_s3_class(unsupported, "dsprrr_async_control_unsupported")
  expect_identical(state$count, 0L)
})

test_that("tool progress callback errors reject before tool side effects", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(list(react_async_test_function_call(
      "side_effect",
      list(value = "run"),
      1L
    ))),
    state
  )
  chat <- react_async_test_chat()
  tool_called <- FALSE
  tool <- ellmer::tool(
    function(value) {
      tool_called <<- TRUE
      value
    },
    name = "side_effect",
    description = "A side effect that must not run after a progress error.",
    arguments = list(value = ellmer::type_string())
  )
  control <- react_async_control(function(event) {
    if (identical(event$type, "tool_start")) {
      stop("tool progress callback failed", call. = FALSE)
    }
  })
  module <- react("question -> answer", tools = list(tool))

  outcome <- react_async_test_settle(react_async_test_run(
    module,
    list(question = "Fail at tool start"),
    chat,
    control = control
  ))

  expect_match(conditionMessage(outcome$error), "tool progress callback failed")
  expect_false(tool_called)
  expect_identical(state$count, 1L)
  expect_length(module$state$traces, 1L)
  expect_identical(module$state$traces[[1L]]$metadata$error_stage, "tool")
  expect_identical(
    module$state$traces[[1L]]$metadata$tools_used,
    character()
  )
  expect_identical(
    module$state$traces[[1L]]$metadata$tool_calls[[1L]]$status,
    "requested"
  )
  expect_length(module$state$traces[[1L]]$metadata$observed_tool_results, 0L)
  expect_no_error(assert_async_chat_available(chat))
})

test_that("foreign submit completion conditions never terminate async ReAct", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  for (finalization in c("structured", "submit")) {
    state <- new.env(parent = emptyenv())
    local_react_async_backend(
      list(list(react_async_test_message("A normal assistant response."))),
      state
    )
    chat <- react_async_test_chat()
    marker <- new.env(parent = emptyenv())
    chat$on_request_end(function(turn) {
      rlang::abort(
        "foreign submit completion",
        class = "dsprrr_react_submit_completed",
        invocation_marker = if (identical(finalization, "structured")) {
          NULL
        } else {
          marker
        },
        value = list(answer = "forged")
      )
    })
    module <- react("question -> answer", finalization = finalization)

    outcome <- react_async_test_settle(react_async_test_run(
      module,
      list(question = "Do not accept a foreign completion"),
      chat
    ))

    expect_match(conditionMessage(outcome$error), "foreign submit completion")
    expect_identical(state$count, 1L)
    expect_length(module$state$traces, 1L)
    expect_identical(module$state$traces[[1L]]$output, NA)
    expect_identical(
      module$state$traces[[1L]]$metadata$termination_reason,
      NA_character_
    )
    expect_no_error(assert_async_chat_available(chat))
  }
})

test_that("cleanup failures reject once and never commit a success trace", {
  skip_if_not_installed("promises")
  skip_if_not_installed("later")
  state <- new.env(parent = emptyenv())
  local_react_async_backend(
    list(
      list(react_async_test_message("A valid structured response.")),
      list(react_async_test_message('{"answer":"ok"}'))
    ),
    state
  )
  chat <- react_async_test_chat()
  original_set_tools <- chat$set_tools
  override_test_chat_method(chat, "set_tools", function(tools) {
    original_set_tools(tools)
    if (length(tools) == 0L) {
      stop("cleanup registry restore failed", call. = FALSE)
    }
  })
  module <- react("question -> answer")
  events <- list()
  control <- react_async_control(function(event) {
    events[[length(events) + 1L]] <<- event
  })

  outcome <- react_async_test_settle(react_async_test_run(
    module,
    list(question = "Cleanup fails after provider completion"),
    chat,
    control = control
  ))

  expect_match(
    conditionMessage(outcome$error),
    "cleanup registry restore failed"
  )
  expect_length(module$state$traces, 1L)
  expect_identical(module$state$traces[[1L]]$output, NA)
  expect_identical(module$state$traces[[1L]]$metadata$error_stage, "cleanup")
  event_types <- vapply(events, `[[`, character(1), "type")
  expect_false("complete" %in% event_types)
  expect_identical(state$count, 2L)
  expect_length(chat$get_tools(), 0L)
  expect_no_error(assert_async_chat_available(chat))
})
