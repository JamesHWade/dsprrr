history_test_content_json <- function(data = NULL, string = NULL) {
  utils::getFromNamespace("ContentJson", "ellmer")(
    data = data,
    string = string
  )
}

test_that("empty conversation histories have a current versioned envelope", {
  history <- conversation_history()

  expect_s3_class(history, "dsprrr_history")
  expect_identical(history$format, "dsprrr_conversation_history")
  expect_identical(history$version, 1L)
  expect_null(history$kind)
  expect_identical(history$turns, list())
  expect_identical(history_turns(history), list())
})

test_that("history JSON round-trips supported ellmer content as fresh turns", {
  tool_calls <- 0L
  lookup <- function(query) {
    tool_calls <<- tool_calls + 1L
    query
  }
  tool <- ellmer::tool(
    lookup,
    name = "lookup",
    description = "Look up a value",
    arguments = list(query = ellmer::type_string())
  )
  request <- ellmer::ContentToolRequest(
    id = "call-1",
    name = "lookup",
    arguments = list(query = "alpha"),
    tool = tool,
    extra = list(provider = "test")
  )
  result <- ellmer::ContentToolResult(
    value = list(answer = "found"),
    extra = list(ok = TRUE),
    request = request
  )
  json_data <- list(
    logical = c(TRUE, NA),
    integer = c(-2L, NA_integer_),
    double = c(-0.125, NA_real_),
    character = c("café", NA_character_),
    empty = integer(),
    null = NULL,
    nested = list(flag = FALSE)
  )
  turns <- list(
    ellmer::UserTurn(list(
      ellmer::ContentText("Question"),
      history_test_content_json(data = json_data),
      ellmer::ContentThinking("reasoning", list(source = "model")),
      ellmer::ContentImageInline("image/png", "YWJj"),
      ellmer::ContentImageRemote("https://example.com/image.png", "high"),
      ellmer::ContentPDF(
        "application/pdf",
        "JVBERi0=",
        "document.pdf",
        url = "https://example.com/document.pdf"
      )
    )),
    ellmer::AssistantTurn(
      contents = list(request),
      tokens = c(11, 7, 18),
      cost = 0.25,
      duration = 2
    ),
    ellmer::UserTurn(list(result)),
    ellmer::AssistantTurn(
      contents = list(ellmer::ContentText("Finished")),
      json = list(answer = "Finished"),
      finish_reason = "stop"
    )
  )
  history <- conversation_history(turns)
  path <- tempfile(fileext = ".json")
  save_history(history, path)

  json <- readLines(path, warn = FALSE)
  restored <- load_history(path)
  restored_turns <- history_turns(restored)

  expect_true(startsWith(json[[1L]], "{"))
  expect_identical(length(restored_turns), length(turns))
  expect_identical(restored_turns[[1L]]@contents[[2L]]@data, json_data)
  expect_identical(
    restored_turns[[1L]]@contents[[3L]]@extra,
    list(source = "model")
  )
  expect_identical(restored_turns[[1L]]@contents[[4L]]@data, "YWJj")
  expect_identical(
    restored_turns[[1L]]@contents[[5L]]@url,
    "https://example.com/image.png"
  )
  expect_identical(restored_turns[[1L]]@contents[[6L]]@filename, "document.pdf")
  expect_identical(
    restored_turns[[1L]]@contents[[6L]]@url,
    "https://example.com/document.pdf"
  )
  expect_identical(restored_turns[[2L]]@contents[[1L]]@id, "call-1")
  expect_null(restored_turns[[2L]]@contents[[1L]]@tool)
  expect_identical(restored_turns[[3L]]@contents[[1L]]@request@id, "call-1")
  expect_null(restored_turns[[3L]]@contents[[1L]]@request@tool)
  expect_identical(restored_turns[[4L]]@json, list(answer = "Finished"))
  expect_identical(restored_turns[[4L]]@finish_reason, "stop")
  expect_true(all(is.na(restored_turns[[2L]]@tokens)))
  expect_true(is.na(restored_turns[[2L]]@cost))
  expect_true(is.na(restored_turns[[2L]]@duration))
  expect_identical(tool_calls, 0L)
})

test_that("history values are copied on construction and reconstruction", {
  original <- list(items = c(1L, 2L))
  turn <- ellmer::UserTurn(list(history_test_content_json(data = original)))
  history <- conversation_history(list(turn))
  original$items[[1L]] <- 99L

  first <- history_turns(history)
  first[[1L]]@contents[[1L]]@data$items[[1L]] <- 77L
  second <- history_turns(history)

  expect_identical(second[[1L]]@contents[[1L]]@data$items, c(1L, 2L))
})

test_that("bounded nested list values survive JSON round-trips", {
  nested <- list(value = "leaf")
  for (i in seq_len(40L)) {
    nested <- list(child = nested)
  }
  history <- conversation_history(list(
    ellmer::UserTurn(list(history_test_content_json(data = nested)))
  ))
  path <- tempfile(fileext = ".json")
  save_history(history, path)

  restored <- history_turns(load_history(path))
  expect_identical(restored[[1L]]@contents[[1L]]@data, nested)
})

test_that("large allowed vectors round-trip below the tagged-value limit", {
  values <- seq_len(40000L)
  history <- conversation_history(list(ellmer::UserTurn(list(
    history_test_content_json(data = values)
  ))))
  path <- tempfile(fileext = ".json")
  save_history(history, path)

  restored <- history_turns(load_history(path))
  expect_identical(restored[[1L]]@contents[[1L]]@data, values)
})

test_that("SystemTurn and uploaded content are rejected explicitly", {
  expect_error(
    conversation_history(list(ellmer::SystemTurn(list(ellmer::ContentText(
      "system"
    ))))),
    class = "dsprrr_history_error"
  )

  uploaded <- utils::getFromNamespace("ContentUploaded", "ellmer")(
    uri = "provider-upload-id",
    mime_type = "application/pdf"
  )
  expect_error(
    conversation_history(list(ellmer::UserTurn(list(uploaded)))),
    "Uploaded content is not portable",
    class = "dsprrr_history_error"
  )
})

test_that("history rejects executable, classed, attributed, and nonfinite data", {
  bad_values <- list(
    function() NULL,
    new.env(parent = emptyenv()),
    structure(list(value = 1), class = "custom_data"),
    structure(list(value = 1), metadata = "unexpected"),
    as.Date("2026-11-13"),
    NaN,
    Inf
  )

  for (value in bad_values) {
    turn <- ellmer::UserTurn(list(history_test_content_json(data = value)))
    expect_error(
      conversation_history(list(turn)),
      class = "dsprrr_history_error"
    )
  }

  deep_value <- NULL
  for (i in seq_len(70L)) {
    deep_value <- list(deep_value)
  }
  deep_turn <- ellmer::UserTurn(list(history_test_content_json(
    data = deep_value
  )))
  expect_error(
    conversation_history(list(deep_turn)),
    "maximum nesting depth",
    class = "dsprrr_history_error"
  )

  large_value <- strrep("x", 12L * 1024L^2 + 1L)
  large_turn <- ellmer::UserTurn(list(history_test_content_json(
    data = large_value
  )))
  expect_error(
    conversation_history(list(large_turn)),
    "maximum size",
    class = "dsprrr_history_error"
  )
})

test_that("tool requests require unique IDs and one matching result", {
  request <- ellmer::ContentToolRequest(
    id = "call-1",
    name = "lookup",
    arguments = list(query = "alpha")
  )
  duplicate <- ellmer::ContentToolRequest(
    id = "call-1",
    name = "lookup",
    arguments = list(query = "beta")
  )
  request_without_result <- list(
    ellmer::AssistantTurn(list(request))
  )
  duplicate_requests <- list(ellmer::AssistantTurn(list(request, duplicate)))
  result_without_request <- ellmer::ContentToolResult(value = list(ok = TRUE))
  orphan_result <- list(
    ellmer::UserTurn(list(ellmer::ContentToolResult(
      value = list(ok = TRUE),
      request = request
    )))
  )
  mismatched_result <- list(
    ellmer::AssistantTurn(list(request)),
    ellmer::UserTurn(list(ellmer::ContentToolResult(
      value = list(ok = TRUE),
      request = ellmer::ContentToolRequest(
        id = "call-1",
        name = "other",
        arguments = list(query = "alpha")
      )
    )))
  )

  expect_error(
    conversation_history(request_without_result),
    "unresolved tool request",
    class = "dsprrr_history_error"
  )
  expect_error(
    conversation_history(duplicate_requests),
    "IDs must be unique",
    class = "dsprrr_history_error"
  )
  expect_error(
    conversation_history(list(ellmer::UserTurn(list(result_without_request)))),
    "identify their originating request",
    class = "dsprrr_history_error"
  )
  expect_error(
    conversation_history(orphan_result),
    "no preceding matching request",
    class = "dsprrr_history_error"
  )
  expect_error(
    conversation_history(mismatched_result),
    "does not match its originating request",
    class = "dsprrr_history_error"
  )
})

test_that("tool result display content is copied in order and errors become messages", {
  request <- ellmer::ContentToolRequest(
    id = "call-1",
    name = "lookup",
    arguments = list(query = "alpha")
  )
  result <- ellmer::ContentToolResult(
    value = list(
      ellmer::ContentText("First"),
      history_test_content_json(data = list(order = 2L)),
      ellmer::ContentImageRemote("https://example.com/image.png"),
      ellmer::ContentImageInline("image/png", "YWJj"),
      ellmer::ContentPDF("application/pdf", "ZGVm", "report.pdf")
    ),
    error = simpleError("tool failed"),
    request = request
  )
  history <- conversation_history(list(
    ellmer::AssistantTurn(list(request)),
    ellmer::UserTurn(list(result))
  ))

  restored <- history_turns(history)
  value <- restored[[2L]]@contents[[1L]]@value

  expect_true(is.list(value))
  expect_s3_class(value[[1L]], "ellmer::ContentText")
  expect_s3_class(value[[2L]], "ellmer::ContentJson")
  expect_s3_class(value[[3L]], "ellmer::ContentImageRemote")
  expect_s3_class(value[[4L]], "ellmer::ContentImageInline")
  expect_s3_class(value[[5L]], "ellmer::ContentPDF")
  expect_identical(value[[1L]]@text, "First")
  expect_identical(value[[2L]]@data, list(order = 2L))
  expect_identical(value[[3L]]@url, "https://example.com/image.png")
  expect_identical(value[[4L]]@data, "YWJj")
  expect_identical(value[[5L]]@filename, "report.pdf")
  expect_identical(restored[[2L]]@contents[[1L]]@error, "tool failed")
  expect_false(inherits(restored[[2L]]@contents[[1L]]@error, "condition"))

  single_result <- ellmer::ContentToolResult(
    value = ellmer::ContentText("single"),
    request = request
  )
  single_history <- conversation_history(list(
    ellmer::AssistantTurn(list(request)),
    ellmer::UserTurn(list(single_result))
  ))
  expect_s3_class(
    history_turns(single_history)[[2L]]@contents[[1L]]@value,
    "ellmer::ContentText"
  )
})

test_that("tool requests and results must use their native turn roles", {
  request <- ellmer::ContentToolRequest(
    id = "call-1",
    name = "lookup",
    arguments = list()
  )
  result <- ellmer::ContentToolResult(value = "ok", request = request)

  expect_error(
    conversation_history(list(ellmer::UserTurn(list(request)))),
    "Tool requests must appear in assistant turns",
    class = "dsprrr_history_error"
  )
  expect_error(
    conversation_history(list(ellmer::AssistantTurn(list(result)))),
    "Tool results must appear in user turns",
    class = "dsprrr_history_error"
  )
})

test_that("pending tool requests settle before another assistant turn", {
  first <- ellmer::ContentToolRequest(
    id = "call-1",
    name = "first",
    arguments = list()
  )
  second <- ellmer::ContentToolRequest(
    id = "call-2",
    name = "second",
    arguments = list()
  )
  first_result <- ellmer::ContentToolResult(value = "one", request = first)
  second_result <- ellmer::ContentToolResult(value = "two", request = second)

  valid <- conversation_history(list(
    ellmer::AssistantTurn(list(first, second)),
    ellmer::UserTurn(list(second_result, first_result)),
    ellmer::AssistantTurn(list(ellmer::ContentText("done")))
  ))
  expect_s3_class(valid, "dsprrr_history")

  delayed <- list(
    ellmer::AssistantTurn(list(first)),
    ellmer::AssistantTurn(list(ellmer::ContentText("unexpected"))),
    ellmer::UserTurn(list(first_result))
  )
  expect_error(
    conversation_history(delayed),
    "cannot follow unresolved tool requests",
    class = "dsprrr_history_error"
  )

  ordinary_user_turns <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("first"))),
    ellmer::UserTurn(list(ellmer::ContentText("second")))
  ))
  expect_equal(length(history_turns(ordinary_user_turns)), 2L)
})

test_that("history loader rejects duplicate fields, unknown fields, tags, and versions", {
  history <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("hello")))
  ))
  path <- tempfile(fileext = ".json")
  save_history(history, path)
  json <- readLines(path, warn = FALSE)[[1L]]

  writeLines(
    sub(
      '"format":"dsprrr_conversation_history"',
      '"format":"dsprrr_conversation_history","format":"dsprrr_conversation_history"',
      json,
      fixed = TRUE
    ),
    path,
    useBytes = TRUE
  )
  expect_error(
    load_history(path),
    "duplicate fields",
    class = "dsprrr_history_error"
  )

  writeLines(
    sub('{"format":', '{"future":true,"format":', json, fixed = TRUE),
    path,
    useBytes = TRUE
  )
  expect_error(
    load_history(path),
    "unknown: future",
    class = "dsprrr_history_error"
  )

  writeLines(
    sub('"version":1', '"version":2', json, fixed = TRUE),
    path,
    useBytes = TRUE
  )
  expect_error(
    load_history(path),
    "Unsupported conversation history version",
    class = "dsprrr_history_error"
  )

  writeLines(
    sub('"tag":"vector"', '"tag":"future"', json, fixed = TRUE),
    path,
    useBytes = TRUE
  )
  expect_error(
    load_history(path),
    "Unknown history data tag",
    class = "dsprrr_history_error"
  )
})

test_that("mutated history envelopes fail closed", {
  history <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("hello")))
  ))
  history$version <- 2L
  expect_error(
    save_history(history, tempfile(fileext = ".json")),
    "Unsupported conversation history version",
    class = "dsprrr_history_error"
  )

  history <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("hello")))
  ))
  version_length_called <- FALSE
  version_method <- "length.dsprrr_history_version_probe"
  assign(
    version_method,
    function(x) {
      version_length_called <<- TRUE
      NextMethod("length")
    },
    envir = .GlobalEnv
  )
  on.exit(rm(list = version_method, envir = .GlobalEnv), add = TRUE)
  history$version <- structure(1L, class = "custom_version")
  class(history$version) <- "dsprrr_history_version_probe"
  expect_error(
    save_history(history, tempfile(fileext = ".json")),
    "Unsupported conversation history version",
    class = "dsprrr_history_error"
  )
  expect_false(version_length_called)

  history <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("hello")))
  ))
  history$turns[[1L]]$contents[[1L]]$future <- TRUE
  expect_error(
    history_turns(history),
    "unknown: future",
    class = "dsprrr_history_error"
  )

  history <- conversation_history(list(
    ellmer::UserTurn(list(history_test_content_json(data = TRUE)))
  ))
  history$turns[[1L]]$contents[[1L]]$data$values[[1L]]$value <- structure(
    TRUE,
    class = "custom_logical"
  )
  expect_error(
    save_history(history, tempfile(fileext = ".json")),
    "History logical value is invalid",
    class = "dsprrr_history_error"
  )

  history <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("hello")))
  ))
  history$turns[[1L]]$contents[[1L]] <- list(
    type = "tool_result",
    value = list(tag = "data", data = list(tag = "null")),
    error = list(tag = "null"),
    extra = list(tag = "null"),
    request = list(
      type = "tool_result",
      request = list(type = "tool_result")
    )
  )
  expect_error(
    history_turns(history),
    "must contain a tool_request record",
    class = "dsprrr_history_error"
  )
})

test_that("load and save validate ellmer field types before reconstruction", {
  request <- ellmer::ContentToolRequest(
    id = "call-1",
    name = "lookup",
    arguments = list(query = "alpha")
  )
  tool_result <- ellmer::ContentToolResult(value = "ok", request = request)
  bases <- list(
    thinking = conversation_history(list(ellmer::UserTurn(list(
      ellmer::ContentThinking("reasoning", list())
    )))),
    json = conversation_history(list(ellmer::UserTurn(list(
      history_test_content_json(data = list(ok = TRUE))
    )))),
    tools = conversation_history(list(
      ellmer::AssistantTurn(list(request)),
      ellmer::UserTurn(list(tool_result))
    ))
  )
  invalid_nodes <- list(
    thinking_extra = list(
      base = bases$thinking,
      modify = function(history) {
        history$turns[[1L]]$contents[[1L]]$extra <- list(
          tag = "vector",
          type = "character",
          names = NULL,
          values = list(list(tag = "character", value = "wrong"))
        )
        history
      },
      message = "Thinking extra must be list"
    ),
    json_string = list(
      base = bases$json,
      modify = function(history) {
        history$turns[[1L]]$contents[[1L]]$string <- list(
          tag = "vector",
          type = "logical",
          names = NULL,
          values = list(list(tag = "logical", value = TRUE))
        )
        history
      },
      message = "JSON content string must be character"
    ),
    tool_arguments = list(
      base = bases$tools,
      modify = function(history) {
        history$turns[[1L]]$contents[[1L]]$arguments <- list(
          tag = "vector",
          type = "integer",
          names = NULL,
          values = list(list(tag = "integer", value = "1"))
        )
        history
      },
      message = "Tool request arguments must be list"
    ),
    tool_error = list(
      base = bases$tools,
      modify = function(history) {
        history$turns[[2L]]$contents[[1L]]$error <- list(
          tag = "vector",
          type = "integer",
          names = NULL,
          values = list(list(tag = "integer", value = "1"))
        )
        history
      },
      message = "Tool result error must be character"
    )
  )

  path <- tempfile(fileext = ".json")
  for (case in invalid_nodes) {
    bad <- case$base
    bad <- case$modify(bad)
    json <- jsonlite::toJSON(
      unclass(bad),
      auto_unbox = TRUE,
      null = "null",
      na = "null"
    )
    writeLines(as.character(json), path, useBytes = TRUE)
    expect_error(
      load_history(path),
      case$message,
      class = "dsprrr_history_error"
    )
    expect_error(
      save_history(bad, tempfile(fileext = ".json")),
      case$message,
      class = "dsprrr_history_error"
    )
  }
})

test_that("classed mutated content is rejected before dollar dispatch", {
  history <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("hello")))
  ))
  content <- list(
    type = "text",
    text = history$turns[[1L]]$contents[[1L]]$text
  )
  class(content) <- "dsprrr_history_dispatch_probe"
  history$turns[[1L]]$contents[[1L]] <- content
  dispatched <- FALSE
  method_name <- "$.dsprrr_history_dispatch_probe"
  assign(
    method_name,
    function(x, name) {
      dispatched <<- TRUE
      NextMethod("$")
    },
    envir = .GlobalEnv
  )
  on.exit(rm(list = method_name, envir = .GlobalEnv), add = TRUE)

  expect_error(
    history_turns(history),
    "must be a plain JSON object",
    class = "dsprrr_history_error"
  )
  expect_false(dispatched)
})

test_that("classed value and name attributes are rejected before generics", {
  data <- list(value = 1L)
  attr(data, "names") <- structure(
    "value",
    class = "dsprrr_history_name_probe"
  )
  content <- history_test_content_json(data = list(value = 1L))
  content@data <- data

  request <- ellmer::ContentToolRequest(
    id = "call-1",
    name = "lookup",
    arguments = list()
  )
  result_value <- structure(
    list(ellmer::ContentText("unsafe classed result")),
    class = "dsprrr_history_name_probe"
  )
  result <- ellmer::ContentToolResult(value = result_value, request = request)

  length_called <- FALSE
  as_list_called <- FALSE
  length_method <- "length.dsprrr_history_name_probe"
  as_list_method <- "as.list.dsprrr_history_name_probe"
  assign(
    length_method,
    function(x) {
      length_called <<- TRUE
      NextMethod("length")
    },
    envir = .GlobalEnv
  )
  assign(
    as_list_method,
    function(x, ...) {
      as_list_called <<- TRUE
      NextMethod("as.list")
    },
    envir = .GlobalEnv
  )
  on.exit(
    rm(list = c(length_method, as_list_method), envir = .GlobalEnv),
    add = TRUE
  )

  expect_error(
    conversation_history(list(ellmer::UserTurn(list(content)))),
    "names must be plain character values",
    class = "dsprrr_history_error"
  )
  expect_false(length_called)
  expect_false(as_list_called)

  expect_error(
    conversation_history(list(
      ellmer::AssistantTurn(list(request)),
      ellmer::UserTurn(list(result))
    )),
    "attributes on tool result values",
    class = "dsprrr_history_error"
  )
  expect_false(length_called)
})
