# Tests for ReactModule (tool support)

test_that("ReactModule class exists", {
  expect_true(R6::is.R6Class(ReactModule))
})

test_that("ReactModule inherits from PredictModule", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "react")

  expect_s3_class(mod, "ReactModule")
  expect_s3_class(mod, "PredictModule")
  expect_s3_class(mod, "Module")
})

test_that("module() creates ReactModule for type='react'", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "react")

  expect_s3_class(mod, "ReactModule")
})

test_that("module() auto-upgrades to ReactModule when tools provided", {
  skip_if_not_installed("ellmer")

  test_fn <- function(x) x
  test_tool <- tryCatch(
    ellmer::tool(
      test_fn,
      name = "test",
      description = "Test tool",
      arguments = list(x = ellmer::type_string())
    ),
    error = function(e) NULL
  )

  skip_if(is.null(test_tool), "Could not create test tool")

  sig <- signature("question -> answer")
  # type="predict" but tools provided -> should upgrade to react
  mod <- module(sig, type = "predict", tools = list(test_tool))

  expect_s3_class(mod, "ReactModule")
})

test_that("ReactModule initializes with empty tools", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "react")

  expect_length(mod$tools, 0)
  expect_equal(mod$list_tools(), character(0))
})

test_that("ReactModule accepts tools in constructor", {
  skip_if_not_installed("ellmer")

  test_fn <- function(x) x
  test_tool <- tryCatch(
    ellmer::tool(
      test_fn,
      name = "my_tool",
      description = "Test tool",
      arguments = list(x = ellmer::type_string())
    ),
    error = function(e) NULL
  )

  skip_if(is.null(test_tool), "Could not create test tool")

  sig <- signature("question -> answer")
  mod <- module(sig, type = "react", tools = list(test_tool))

  expect_length(mod$tools, 1)
  expect_equal(mod$list_tools(), "my_tool")
})

test_that("ReactModule add_tool works", {
  skip_if_not_installed("ellmer")

  test_fn <- function(x) x
  test_tool <- tryCatch(
    ellmer::tool(
      test_fn,
      name = "added_tool",
      description = "Test tool",
      arguments = list(x = ellmer::type_string())
    ),
    error = function(e) NULL
  )

  skip_if(is.null(test_tool), "Could not create test tool")

  sig <- signature("question -> answer")
  mod <- module(sig, type = "react")

  expect_length(mod$tools, 0)

  mod$add_tool(test_tool)

  expect_length(mod$tools, 1)
  expect_equal(mod$list_tools(), "added_tool")
})

test_that("ReactModule remove_tool works", {
  skip_if_not_installed("ellmer")

  test_fn <- function(x) x
  test_tool <- tryCatch(
    ellmer::tool(
      test_fn,
      name = "removable_tool",
      description = "Test tool",
      arguments = list(x = ellmer::type_string())
    ),
    error = function(e) NULL
  )

  skip_if(is.null(test_tool), "Could not create test tool")

  sig <- signature("question -> answer")
  mod <- module(sig, type = "react", tools = list(test_tool))

  expect_length(mod$tools, 1)

  mod$remove_tool("removable_tool")

  expect_length(mod$tools, 0)
})

test_that("ReactModule remove_tool warns for non-existent tool", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "react")

  expect_warning(
    mod$remove_tool("nonexistent"),
    "not found"
  )
})

test_that("ReactModule max_iterations is configurable", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "react", max_iterations = 20L)

  expect_equal(mod$max_iterations, 20L)
  expect_error(
    module(sig, type = "react", max_iterations = 0L),
    "positive integer"
  )
})

test_that("ReactModule forward tracks tool calls from ellmer turns", {
  turns <- list()

  last_turn <- function(role = c("assistant", "user"), ...) {
    role <- match.arg(role)
    role_turns <- Filter(function(turn) identical(turn@role, role), turns)

    if (length(role_turns) == 0) {
      stop("no turns")
    }

    role_turns[[length(role_turns)]]
  }

  mock_llm <- structure(
    list(
      register_tool = function(tool) invisible(NULL),
      get_turns = function(...) turns,
      last_turn = last_turn,
      chat = function(prompt, echo = "none", ...) {
        user_turn <- ellmer::UserTurn(
          contents = list(ellmer::ContentText(prompt))
        )
        first_tool_turn <- ellmer::AssistantTurn(
          contents = list(ellmer::ContentToolRequest(
            id = "call_1",
            name = "lookup",
            arguments = list(query = "alpha")
          ))
        )
        second_tool_turn <- ellmer::AssistantTurn(
          contents = list(ellmer::ContentToolRequest(
            id = "call_2",
            name = "summarize",
            arguments = list(value = "beta")
          ))
        )

        turns <<- c(
          turns,
          list(user_turn, first_tool_turn, second_tool_turn)
        )
        invisible(NULL)
      },
      chat_structured = function(prompt, type, echo = "none", ...) {
        final_turn <- ellmer::AssistantTurn(
          contents = list(ellmer::ContentText("{\"answer\":\"done\"}"))
        )
        turns <<- c(turns, list(final_turn))
        list(answer = "done")
      },
      get_model = function() "mock-model"
    ),
    class = "Chat"
  )

  sig <- signature("question -> answer")
  mod <- module(sig, type = "react")

  result <- mod$forward(list(question = "test"), .llm = mock_llm, trace = TRUE)
  metadata <- result$metadata[[1]]

  expect_equal(metadata$iterations, 2L)
  expect_length(metadata$tool_calls, 2)
  expect_equal(
    vapply(metadata$tool_calls, `[[`, character(1), "tool_name"),
    c("lookup", "summarize")
  )
  expect_equal(metadata$tool_calls[[1]]$arguments$query, "alpha")
  expect_equal(metadata$tool_calls[[2]]$arguments$value, "beta")
  expect_equal(metadata$tools_used, c("lookup", "summarize"))
  expect_true(all(vapply(
    metadata$history,
    inherits,
    logical(1),
    "ellmer::Turn"
  )))
  expect_identical(metadata$finalization, "structured-followup")
  expect_true(is.na(metadata$cost))
  expect_equal(mod$state$traces[[1]]$iterations, 2L)
})

test_that("ReactModule enforces max_iterations before finalization", {
  turns <- list()
  mock_llm <- structure(
    list(
      register_tool = function(tool) invisible(NULL),
      get_turns = function(...) turns,
      chat = function(prompt, echo = "none", ...) {
        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(contents = list(ellmer::ContentText(prompt))),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentToolRequest(
                id = "call_1",
                name = "first",
                arguments = list()
              ))
            ),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentToolRequest(
                id = "call_2",
                name = "second",
                arguments = list()
              ))
            )
          )
        )
        invisible(NULL)
      },
      chat_structured = function(...) stop("finalization should not run")
    ),
    class = "Chat"
  )

  mod <- module(
    signature("question -> answer"),
    type = "react",
    max_iterations = 1L
  )
  expect_error(
    mod$forward(list(question = "test"), .llm = mock_llm),
    class = "dsprrr_react_iteration_limit"
  )
})

test_that("ReactModule iteration guard ignores tool turns from prior runs", {
  turns <- list(
    ellmer::AssistantTurn(
      contents = list(ellmer::ContentToolRequest(
        id = "old_call",
        name = "old_tool",
        arguments = list()
      ))
    )
  )
  iteration_guard <- NULL

  last_turn <- function(role = c("assistant", "user"), ...) {
    role <- match.arg(role)
    matching <- Filter(function(turn) identical(turn@role, role), turns)
    matching[[length(matching)]]
  }

  mock_llm <- structure(
    list(
      register_tool = function(tool) invisible(NULL),
      get_turns = function(...) turns,
      last_turn = last_turn,
      on_tool_request = function(callback) {
        iteration_guard <<- callback
        function() iteration_guard <<- NULL
      },
      chat = function(prompt, echo = "none", ...) {
        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(contents = list(ellmer::ContentText(prompt))),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentToolRequest(
                id = "new_call",
                name = "new_tool",
                arguments = list()
              ))
            )
          )
        )
        iteration_guard(list(id = "new_call"))
        invisible(NULL)
      },
      chat_structured = function(prompt, type, echo = "none", ...) {
        turns <<- c(
          turns,
          list(ellmer::AssistantTurn(
            contents = list(ellmer::ContentText("{\"answer\":\"done\"}"))
          ))
        )
        list(answer = "done")
      },
      get_model = function() "mock-model"
    ),
    class = "Chat"
  )

  mod <- module(
    signature("question -> answer"),
    type = "react",
    max_iterations = 1L
  )
  result <- mod$forward(list(question = "test"), .llm = mock_llm)

  expect_equal(result$metadata[[1]]$iterations, 1L)
  expect_identical(result$metadata[[1]]$tool_calls[[1]]$tool_id, "new_call")
  expect_null(iteration_guard)
})

test_that("ReactModule print includes tool info", {
  skip_if_not_installed("ellmer")

  test_fn <- function(x) x
  test_tool <- tryCatch(
    ellmer::tool(
      test_fn,
      name = "print_test_tool",
      description = "Test tool",
      arguments = list(x = ellmer::type_string())
    ),
    error = function(e) NULL
  )

  skip_if(is.null(test_tool), "Could not create test tool")

  sig <- signature("question -> answer")
  mod <- module(sig, type = "react", tools = list(test_tool))

  # Capture output including cli output
  output <- capture.output(print(mod), type = "message")
  if (length(output) == 0) {
    output <- capture.output(print(mod), type = "output")
  }
  output_text <- paste(output, collapse = "\n")

  # Check that list_tools returns the tool name
  expect_equal(mod$list_tools(), "print_test_tool")

  # Check module structure
  expect_s3_class(mod, "ReactModule")
  expect_length(mod$tools, 1)
})

test_that("ReactModule add_tool returns self invisibly", {
  skip_if_not_installed("ellmer")

  test_fn <- function(x) x
  test_tool <- tryCatch(
    ellmer::tool(
      test_fn,
      name = "chain_tool",
      description = "Test tool",
      arguments = list(x = ellmer::type_string())
    ),
    error = function(e) NULL
  )

  skip_if(is.null(test_tool), "Could not create test tool")

  sig <- signature("question -> answer")
  mod <- module(sig, type = "react")

  # Should be chainable
  result <- mod$add_tool(test_tool)
  expect_identical(result, mod)
})

test_that("ReactModule rejects non-ToolDef in constructor", {
  sig <- signature("question -> answer")

  expect_error(
    ReactModule$new(sig, tools = list("not a tool")),
    "ToolDef"
  )
})

test_that("ReactModule rejects non-ToolDef in add_tool", {
  sig <- signature("question -> answer")
  mod <- module(sig, type = "react")

  expect_error(
    mod$add_tool("not a tool"),
    "ToolDef"
  )
})
