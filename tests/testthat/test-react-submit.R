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

test_that("duplicate submit object keys are repairable at every depth", {
  calls <- new.env(parent = emptyenv())
  sig <- signature(
    inputs = list(input("question")),
    output_type = ellmer::type_object(
      answer = ellmer::type_string(),
      details = ellmer::type_object(note = ellmer::type_string()),
      sources = ellmer::type_array(
        ellmer::type_object(label = ellmer::type_string())
      )
    )
  )
  local_react_submit_backend(
    list(
      list(react_submit_function_call(
        "submit",
        paste0(
          '{"answer":"first","answer":"second",',
          '"details":{"note":"ok"},',
          '"sources":[{"label":"source"}]}'
        ),
        1L
      )),
      list(react_submit_function_call(
        "submit",
        paste0(
          '{"answer":"ok",',
          '"details":{"note":"first","note":"second"},',
          '"sources":[{"label":"source"}]}'
        ),
        2L
      )),
      list(react_submit_function_call(
        "submit",
        paste0(
          '{"answer":"ok","details":{"note":"ok"},',
          '"sources":[{"label":"first","label":"second"}]}'
        ),
        3L
      )),
      list(react_submit_function_call(
        "submit",
        paste0(
          '{"answer":"ok","details":{"note":"ok"},',
          '"sources":[{"label":"source"}]}'
        ),
        4L
      ))
    ),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(sig, finalization = "submit", max_iterations = 4L)

  result <- suppressWarnings(run(
    mod,
    question = "Summarize this evidence",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_identical(
    result$output,
    list(
      answer = "ok",
      details = list(note = "ok"),
      sources = list(list(label = "source"))
    )
  )
  expect_identical(result$metadata$iterations, 4L)
  expect_identical(calls$n, 4L)
  repair_results <- lapply(calls$bodies[-1L], function(body) {
    Filter(
      function(item) identical(item$type, "function_call_output"),
      body$input
    )
  })
  expect_true(all(lengths(repair_results) >= 1L))
  expect_true(all(vapply(
    repair_results,
    function(items) {
      any(vapply(
        items,
        function(item) {
          grepl(
            "unique, non-empty field names",
            item$output
          )
        },
        logical(1)
      ))
    },
    logical(1)
  )))
})

test_that("bare array submit checks object item keys before stripping", {
  calls <- new.env(parent = emptyenv())
  sig <- signature(
    inputs = list(input("question")),
    output_type = ellmer::type_array(
      ellmer::type_object(label = ellmer::type_string())
    )
  )
  local_react_submit_backend(
    list(
      list(react_submit_function_call(
        "submit",
        '{"value":[{"label":"first","label":"second"}]}',
        1L
      )),
      list(react_submit_function_call(
        "submit",
        '{"value":[{"label":"source"}]}',
        2L
      ))
    ),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(sig, finalization = "submit", max_iterations = 2L)

  result <- suppressWarnings(run(
    mod,
    question = "List the source",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_identical(result$output, list(list(label = "source")))
  expect_identical(calls$n, 2L)
  tool_outputs <- Filter(
    function(item) identical(item$type, "function_call_output"),
    calls$bodies[[2L]]$input
  )
  expect_true(any(vapply(
    tool_outputs,
    function(item) grepl("unique, non-empty field names", item$output),
    logical(1)
  )))
})

test_that("bare array submit repairs JSON scalars before accepting arrays", {
  calls <- new.env(parent = emptyenv())
  sig <- signature(
    inputs = list(input("question")),
    output_type = ellmer::type_array(ellmer::type_string())
  )
  local_react_submit_backend(
    list(
      list(react_submit_function_call(
        "submit",
        '{"value":"answer"}',
        1L
      )),
      list(react_submit_function_call(
        "submit",
        '{"value":["answer"]}',
        2L
      ))
    ),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(sig, finalization = "submit", max_iterations = 2L)

  result <- suppressWarnings(run(
    mod,
    question = "Return an array",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_identical(result$output, list("answer"))
  expect_identical(calls$n, 2L)
  requests <- unlist(
    lapply(result$metadata$history, function(turn) {
      Filter(
        function(content) inherits(content, "ellmer::ContentToolRequest"),
        turn@contents
      )
    }),
    recursive = FALSE
  )
  expect_length(requests, 2L)
  expect_identical(requests[[1L]]@arguments$value, "answer")
  expect_identical(requests[[2L]]@arguments$value, list("answer"))
  tool_outputs <- Filter(
    function(item) identical(item$type, "function_call_output"),
    calls$bodies[[2L]]$input
  )
  expect_true(any(vapply(
    tool_outputs,
    function(item) grepl("must be a JSON array", item$output),
    logical(1)
  )))
})

test_that("bare array submit distinguishes empty arrays from objects", {
  calls <- new.env(parent = emptyenv())
  sig <- signature(
    inputs = list(input("question")),
    output_type = ellmer::type_array(ellmer::type_string())
  )
  local_react_submit_backend(
    list(
      list(react_submit_function_call("submit", '{"value":{}}', 1L)),
      list(react_submit_function_call("submit", '{"value":[]}', 2L))
    ),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(sig, finalization = "submit", max_iterations = 2L)

  result <- suppressWarnings(run(
    mod,
    question = "Return an empty array",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_identical(result$output, list())
  expect_identical(calls$n, 2L)
  requests <- unlist(
    lapply(result$metadata$history, function(turn) {
      Filter(
        function(content) inherits(content, "ellmer::ContentToolRequest"),
        turn@contents
      )
    }),
    recursive = FALSE
  )
  expect_length(requests, 2L)
  expect_identical(
    names(requests[[1L]]@arguments$value),
    character()
  )
  expect_null(names(requests[[2L]]@arguments$value))
  repair <- Filter(
    function(item) identical(item$type, "function_call_output"),
    calls$bodies[[2L]]$input
  )
  expect_true(any(vapply(
    repair,
    function(item) grepl("must be a JSON array", item$output),
    logical(1)
  )))
})

test_that("optional array submit still requires an object argument envelope", {
  calls <- new.env(parent = emptyenv())
  sig <- signature(
    inputs = list(input("question")),
    output_type = ellmer::type_array(
      ellmer::type_string(),
      required = FALSE
    )
  )
  local_react_submit_backend(
    list(
      list(react_submit_function_call("submit", "[]", 1L)),
      list(react_submit_function_call("submit", "{}", 2L))
    ),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(sig, finalization = "submit", max_iterations = 3L)

  result <- suppressWarnings(run(
    mod,
    question = "Omit the optional array output",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_null(result$output)
  expect_identical(calls$n, 2L)
  repair <- Filter(
    function(item) identical(item$type, "function_call_output"),
    calls$bodies[[2L]]$input
  )
  expect_true(any(vapply(
    repair,
    function(item) grepl("arguments must be a JSON object", item$output),
    logical(1)
  )))
})

test_that("ellmer rejects null tool arguments before ReAct submit callbacks", {
  calls <- new.env(parent = emptyenv())
  local_react_submit_backend(
    list(list(react_submit_function_call("submit", "null", 1L))),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_array(
        ellmer::type_string(),
        required = FALSE
      )
    ),
    finalization = "submit"
  )

  condition <- expect_error(
    run(
      mod,
      question = "Omit the optional array output",
      .llm = chat,
      .return_format = "structured"
    ),
    class = "dsprrr_provider_error"
  )

  expect_match(conditionMessage(condition), "@arguments must be <list>")
  expect_identical(calls$n, 1L)
})

test_that("nested array properties repair scalar JSON values", {
  calls <- new.env(parent = emptyenv())
  sig <- signature(
    inputs = list(input("question")),
    output_type = ellmer::type_object(
      items = ellmer::type_array(ellmer::type_string())
    )
  )
  local_react_submit_backend(
    list(
      list(react_submit_function_call(
        "submit",
        '{"items":"one"}',
        1L
      )),
      list(react_submit_function_call(
        "submit",
        '{"items":["one","two"]}',
        2L
      ))
    ),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(sig, finalization = "submit", max_iterations = 2L)

  result <- suppressWarnings(run(
    mod,
    question = "Return nested array data",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_identical(result$output, list(items = list("one", "two")))
  expect_identical(calls$n, 2L)
  repair <- Filter(
    function(item) identical(item$type, "function_call_output"),
    calls$bodies[[2L]]$input
  )
  expect_true(any(vapply(
    repair,
    function(item) grepl("items must be a JSON array", item$output),
    logical(1)
  )))
})

test_that("empty object and only-ignored output schemas still submit", {
  output_types <- list(
    ellmer::type_object(),
    ellmer::type_object(ignored = ellmer::type_ignore())
  )
  for (output_type in output_types) {
    calls <- new.env(parent = emptyenv())
    local_react_submit_backend(
      list(list(react_submit_function_call("submit", "{}", 1L))),
      calls,
      .env = environment()
    )
    chat <- react_submit_chat()
    mod <- react(
      signature(
        inputs = list(input("question")),
        output_type = output_type
      ),
      finalization = "submit"
    )

    result <- suppressWarnings(run(
      mod,
      question = "Return an empty object",
      .llm = chat,
      .return_format = "structured"
    ))

    expect_identical(result$output, stats::setNames(list(), character()))
    expect_identical(calls$n, 1L)
  }
  expect_identical(
    react_submit_tool(ellmer::type_object())(),
    "Submission accepted."
  )
  expect_identical(
    react_submit_tool(ellmer::type_object(ignored = ellmer::type_ignore()))(),
    "Submission accepted."
  )
})

test_that("nested empty objects and arrays keep their JSON shapes", {
  calls <- new.env(parent = emptyenv())
  sig <- signature(
    inputs = list(input("question")),
    output_type = ellmer::type_object(
      payload = ellmer::type_object(
        items = ellmer::type_array(ellmer::type_string())
      )
    )
  )
  local_react_submit_backend(
    list(
      list(react_submit_function_call("submit", '{"payload":[]}', 1L)),
      list(react_submit_function_call(
        "submit",
        '{"payload":{"items":{}}}',
        2L
      )),
      list(react_submit_function_call(
        "submit",
        '{"payload":{"items":[]}}',
        3L
      ))
    ),
    calls
  )
  chat <- react_submit_chat()
  mod <- react(sig, finalization = "submit", max_iterations = 3L)

  result <- suppressWarnings(run(
    mod,
    question = "Return nested empty data",
    .llm = chat,
    .return_format = "structured"
  ))

  expect_identical(
    result$output,
    list(payload = list(items = list()))
  )
  expect_identical(calls$n, 3L)
  repairs <- lapply(calls$bodies[-1L], function(body) {
    Filter(
      function(item) identical(item$type, "function_call_output"),
      body$input
    )
  })
  expect_true(any(vapply(
    repairs[[1L]],
    function(item) grepl("payload must be a JSON object", item$output),
    logical(1)
  )))
  expect_true(any(vapply(
    repairs[[2L]],
    function(item) grepl("payload\\$items must be a JSON array", item$output),
    logical(1)
  )))
})

test_that("duplicate checks preserve null, empty, and ignored field handling", {
  optional_type <- ellmer::type_object(
    maybe = ellmer::type_string(required = FALSE)
  )
  array_ignore_type <- ellmer::type_array(ellmer::type_object(
    answer = ellmer::type_string(),
    ignored = ellmer::type_ignore()
  ))
  ignored_type <- ellmer::type_object(
    answer = ellmer::type_string(),
    ignored = ellmer::type_ignore()
  )

  expect_identical(
    react_submit_validate(
      list(maybe = NULL),
      optional_type,
      bare_output = FALSE
    ),
    list(maybe = NULL)
  )
  expect_identical(
    react_submit_validate(
      stats::setNames(list(), character()),
      ellmer::type_object(),
      bare_output = FALSE
    ),
    stats::setNames(list(), character())
  )
  expect_identical(
    react_submit_validate(
      list(answer = "ready", ignored = NULL),
      ignored_type,
      bare_output = FALSE
    ),
    list(answer = "ready")
  )
  expect_identical(
    react_submit_validate(
      list(value = list(list(answer = "ready", ignored = NULL))),
      array_ignore_type,
      bare_output = TRUE
    ),
    list(list(answer = "ready"))
  )
  ignored_condition <- rlang::catch_cnd(react_submit_validate(
    list(answer = "ready", ignored = "not null"),
    ignored_type,
    bare_output = FALSE
  ))
  expect_s3_class(ignored_condition, "dsprrr_react_submit_validation_error")
  expect_match(conditionMessage(ignored_condition), "ignored and must be NULL")
  ignored_array_condition <- rlang::catch_cnd(react_submit_validate(
    list(
      value = list(list(
        answer = "ready",
        ignored = stats::setNames(list("first", "second"), c("key", "key"))
      ))
    ),
    array_ignore_type,
    bare_output = TRUE
  ))
  expect_s3_class(
    ignored_array_condition,
    "dsprrr_react_submit_validation_error"
  )
  expect_match(
    conditionMessage(ignored_array_condition),
    "ignored and must be NULL"
  )
  null_array_wrapper <- rlang::catch_cnd(react_submit_validate(
    NULL,
    ellmer::type_array(ellmer::type_string(), required = FALSE),
    bare_output = TRUE
  ))
  expect_s3_class(
    null_array_wrapper,
    "dsprrr_react_submit_validation_error"
  )
  expect_match(
    conditionMessage(null_array_wrapper),
    "arguments must be a JSON object"
  )
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

test_that("foreign submit signals never complete the current invocation", {
  for (mode in c("structured", "submit")) {
    for (marker in list(NULL, new.env(parent = emptyenv()))) {
      calls <- new.env(parent = emptyenv())
      local_react_submit_backend(
        list(list(react_submit_text("ordinary answer"))),
        calls,
        .env = environment()
      )
      chat <- react_submit_chat()
      request <- ellmer::ContentToolRequest(
        id = "foreign-submit",
        name = "submit",
        arguments = list(answer = "foreign output")
      )
      remove <- chat$on_request_end(function(turn) {
        react_submit_completed(request, list(answer = "foreign output"), marker)
      })
      agent <- react("question -> answer", finalization = mode)
      error <- rlang::catch_cnd(run(
        agent,
        question = "current question",
        .llm = chat,
        .return_format = "structured"
      ))
      remove()
      expect_s3_class(error, "error")
      cause <- error
      condition_classes <- character()
      while (inherits(cause, "condition")) {
        condition_classes <- c(condition_classes, class(cause))
        cause <- cause$parent
      }
      expect_true("dsprrr_react_submit_completed" %in% condition_classes)
      expect_identical(calls$n, 1L)
      contents <- unlist(
        lapply(chat$get_turns(), function(turn) turn@contents),
        recursive = FALSE
      )
      expect_length(
        Filter(
          function(content) {
            inherits(content, "ellmer::ContentToolResult")
          },
          contents
        ),
        0L
      )
    }
  }
})
