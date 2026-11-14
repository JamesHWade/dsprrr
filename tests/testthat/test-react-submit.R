local_react_submit_backend <- function(replies, calls, .env = parent.frame()) {
  calls$n <- 0L
  calls$bodies <- list()
  testthat::local_mocked_bindings(
    req_perform = function(req) {
      calls$n <- calls$n + 1L
      if (calls$n > length(replies)) {
        cli::cli_abort(
          "Unexpected provider request in ReAct submit test",
          class = "test_react_unexpected_provider_request"
        )
      }
      calls$bodies[[calls$n]] <- req$body$data
      body <- list(
        id = paste0("resp_", calls$n),
        object = "response",
        created_at = 1L,
        status = "completed",
        model = "gpt-6-luna",
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
  invisible(calls)
}

react_submit_function_call <- function(name, arguments, id) {
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

react_submit_text <- function(text) {
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

react_submit_chat <- function() {
  suppressWarnings(ellmer::chat_openai(
    credentials = function() "test-key",
    model = "gpt-6-luna"
  ))
}

react_submit_output_signature <- function() {
  signature(
    inputs = list(input("question")),
    output_type = ellmer::type_object(
      status = ellmer::type_enum(c("supported", "unknown")),
      facts = ellmer::type_array(ellmer::type_object(
        label = ellmer::type_string(),
        score = ellmer::type_integer(),
        note = ellmer::type_string(required = FALSE)
      )),
      confidence = ellmer::type_number(required = FALSE)
    )
  )
}

test_that("submit finalization validates and returns nested typed output", {
  calls <- new.env(parent = emptyenv())
  output <- list(
    status = "supported",
    facts = list(list(label = "source", score = 3L))
  )
  local_react_submit_backend(
    list(list(react_submit_function_call("submit", output, 1L))),
    calls
  )
  chat <- react_submit_chat()
  prior_tool <- ellmer::tool(
    function(query) "kept",
    name = "existing",
    description = "An existing chat tool",
    arguments = list(query = ellmer::type_string())
  )
  chat$register_tool(prior_tool)
  tools_before <- chat$get_tools()
  mod <- react(
    react_submit_output_signature(),
    finalization = "submit"
  )

  result <- run(
    mod,
    question = "What is supported?",
    .llm = chat,
    .return_format = "structured"
  )

  expect_identical(result$output, output)
  expect_identical(result$metadata$finalization, "submit")
  expect_identical(result$metadata$termination_reason, "submitted")
  expect_identical(result$metadata$iterations, 1L)
  expect_identical(result$metadata$tools_used, "submit")
  expect_identical(calls$n, 1L)
  expect_identical(chat$get_tools(), tools_before)

  assistant_turns <- Filter(
    function(turn) identical(turn@role, "assistant"),
    result$metadata$history
  )
  requests <- unlist(
    lapply(assistant_turns, function(turn) {
      Filter(
        function(content) inherits(content, "ellmer::ContentToolRequest"),
        turn@contents
      )
    }),
    recursive = FALSE
  )
  tool_results <- unlist(
    lapply(result$metadata$history, function(turn) {
      Filter(
        function(content) inherits(content, "ellmer::ContentToolResult"),
        turn@contents
      )
    }),
    recursive = FALSE
  )
  expect_length(requests, 1L)
  expect_length(tool_results, 1L)
  expect_identical(tool_results[[1L]]@request@id, requests[[1L]]@id)
  expect_identical(tool_results[[1L]]@request@name, "submit")
  expect_identical(
    mod$state$traces[[1L]]$termination_reason,
    "submitted"
  )
})

test_that("invalid nested submit is returned as a repairable tool error", {
  calls <- new.env(parent = emptyenv())
  invalid <- list(
    status = "supported",
    facts = list(list(label = "source", score = 3L, extra = "unexpected"))
  )
  valid <- list(
    status = "supported",
    facts = list(list(label = "source", score = 3L))
  )
  local_react_submit_backend(
    list(
      list(react_submit_function_call("submit", invalid, 1L)),
      list(react_submit_function_call("submit", valid, 2L))
    ),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(
    react_submit_output_signature(),
    finalization = "submit",
    max_iterations = 2L
  )

  result <- suppressWarnings(run(
    mod,
    question = "What is supported?",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_identical(result$output, valid)
  expect_identical(result$metadata$iterations, 2L)
  expect_identical(calls$n, 2L)
  repair_results <- Filter(
    function(item) identical(item$type, "function_call_output"),
    calls$bodies[[2L]]$input
  )
  expect_length(repair_results, 1L)
  expect_match(repair_results[[1L]]$output, "included unknown fields")
  expect_length(
    Filter(
      function(content) {
        inherits(content, "ellmer::ContentToolResult") &&
          !is.null(content@error)
      },
      unlist(
        lapply(chat$get_turns(), function(turn) turn@contents),
        recursive = FALSE
      )
    ),
    1L
  )
})

test_that("submit attempts count toward max_iterations before acceptance", {
  calls <- new.env(parent = emptyenv())
  invalid <- list(status = "invalid", facts = list())
  valid <- list(status = "supported", facts = list())
  local_react_submit_backend(
    list(
      list(react_submit_function_call("submit", invalid, 1L)),
      list(react_submit_function_call("submit", valid, 2L))
    ),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(
    react_submit_output_signature(),
    finalization = "submit",
    max_iterations = 1L
  )

  condition <- tryCatch(
    withCallingHandlers(
      run(
        mod,
        question = "What is supported?",
        .llm = chat,
        .return_format = "structured"
      ),
      warning = function(warning) {
        if (inherits(warning, "ellmer_tool_failure")) {
          invokeRestart("muffleWarning")
        }
      }
    ),
    error = identity
  )

  expect_s3_class(condition, "dsprrr_react_iteration_limit")
  expect_identical(calls$n, 2L)
  expect_length(chat$get_tools(), 0L)
  results <- unlist(
    lapply(chat$get_turns(), function(turn) turn@contents),
    recursive = FALSE
  )
  expect_length(
    Filter(
      function(content) inherits(content, "ellmer::ContentToolResult"),
      results
    ),
    1L
  )
})

test_that("submit finalization fails when the model does not submit", {
  calls <- new.env(parent = emptyenv())
  local_react_submit_backend(
    list(list(react_submit_text("The answer is 42."))),
    calls
  )
  chat <- react_submit_chat()
  mod <- react("question -> answer", finalization = "submit")

  condition <- rlang::catch_cnd(run(
    mod,
    question = "What is the answer?",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_s3_class(condition, "dsprrr_react_submit_missing")
  expect_identical(calls$n, 1L)
  expect_length(chat$get_tools(), 0L)
})

test_that("mixed submit and user tools fail before tool side effects", {
  calls <- new.env(parent = emptyenv())
  side_effect <- FALSE
  local_react_submit_backend(
    list(list(
      react_submit_function_call(
        "submit",
        list(answer = "done"),
        1L
      ),
      react_submit_function_call(
        "side_effect",
        list(value = "run"),
        2L
      )
    )),
    calls
  )
  chat <- react_submit_chat()
  side_tool <- ellmer::tool(
    function(value) {
      side_effect <<- TRUE
      value
    },
    name = "side_effect",
    description = "A side effect for the test",
    arguments = list(value = ellmer::type_string())
  )
  mod <- react(
    "question -> answer",
    tools = list(side_tool),
    finalization = "submit"
  )

  condition <- rlang::catch_cnd(run(
    mod,
    question = "Run the task",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_s3_class(condition, "dsprrr_react_submit_mixed_tools")
  expect_false(side_effect)
  expect_identical(calls$n, 1L)
  expect_length(chat$get_tools(), 0L)
})

test_that("duplicate submit calls in one turn are rejected", {
  calls <- new.env(parent = emptyenv())
  local_react_submit_backend(
    list(list(
      react_submit_function_call("submit", list(value = "first"), 1L),
      react_submit_function_call("submit", list(value = "second"), 2L)
    )),
    calls
  )
  chat <- react_submit_chat()
  mod <- react("question -> answer", finalization = "submit")

  condition <- rlang::catch_cnd(run(
    mod,
    question = "Choose an answer",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_s3_class(condition, "dsprrr_react_submit_mixed_tools")
  expect_identical(calls$n, 1L)
  expect_length(chat$get_tools(), 0L)
  expect_length(
    Filter(
      function(content) inherits(content, "ellmer::ContentToolResult"),
      unlist(
        lapply(chat$get_turns(), function(turn) turn@contents),
        recursive = FALSE
      )
    ),
    0L
  )
})

test_that("submit tool name collisions are rejected", {
  submit_tool <- ellmer::tool(
    function(value) value,
    name = "submit",
    description = "A user-defined submit tool",
    arguments = list(value = ellmer::type_string())
  )
  module_condition <- rlang::catch_cnd(react(
    "question -> answer",
    tools = list(submit_tool),
    finalization = "submit"
  ))
  expect_s3_class(module_condition, "dsprrr_react_submit_tool_conflict")

  calls <- new.env(parent = emptyenv())
  local_react_submit_backend(list(), calls)
  chat <- react_submit_chat()
  chat$register_tool(submit_tool)
  tools_before <- chat$get_tools()
  condition <- rlang::catch_cnd(run(
    react("question -> answer", finalization = "submit"),
    question = "q",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_s3_class(condition, "dsprrr_react_submit_tool_conflict")
  expect_identical(calls$n, 0L)
  expect_identical(chat$get_tools(), tools_before)
})

test_that("bare output schemas submit through a value field", {
  calls <- new.env(parent = emptyenv())
  local_react_submit_backend(
    list(
      list(react_submit_function_call(
        "submit",
        '{"value":"no","unexpected":true}',
        1L
      )),
      list(react_submit_function_call(
        "submit",
        '{"value":"no","value":"yes"}',
        2L
      )),
      list(react_submit_function_call("submit", "{}", 3L)),
      list(react_submit_function_call(
        "submit",
        '{"value":"yes"}',
        4L
      ))
    ),
    calls
  )
  chat <- react_submit_chat()
  sig <- signature(
    inputs = list(input("question")),
    output_type = ellmer::type_enum(c("yes", "no"))
  )
  mod <- react(sig, finalization = "submit")

  result <- suppressWarnings(run(
    mod,
    question = "Is this correct?",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_identical(result$output, "yes")
  expect_identical(result$metadata$termination_reason, "submitted")
  expect_identical(result$metadata$iterations, 4L)
  expect_identical(calls$n, 4L)
  results <- unlist(
    lapply(chat$get_turns(), function(turn) turn@contents),
    recursive = FALSE
  )
  errors <- Filter(
    function(content) {
      inherits(content, "ellmer::ContentToolResult") &&
        !is.null(content@error)
    },
    results
  )
  expect_length(errors, 3L)
})

test_that("submit finalization persists through artifact and module copies", {
  mod <- react("question -> answer", finalization = "submit")
  restored <- restore_module_config(program_artifact(mod))

  expect_identical(restored$config$.react_finalization, "submit")
  expect_identical(mod$reset_copy()$config$.react_finalization, "submit")
  expect_identical(mod$deepcopy()$config$.react_finalization, "submit")
  expect_identical(
    react("question -> answer")$config$.react_finalization,
    "structured"
  )
})
