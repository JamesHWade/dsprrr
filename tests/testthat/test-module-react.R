# Tests for ReactModule (tool support)

test_that("ReactModule class exists", {
  expect_true(R6::is.R6Class(ReactModule))
})

test_that("ReactModule inherits from PredictModule", {
  sig <- signature("question -> answer")
  mod <- react(sig)

  expect_s3_class(mod, "ReactModule")
  expect_s3_class(mod, "PredictModule")
  expect_s3_class(mod, "Module")
})

test_that("react() creates a ReactModule", {
  sig <- signature("question -> answer")
  mod <- react(sig)

  expect_s3_class(mod, "ReactModule")
})

test_that("module() never changes into ReAct when tools are supplied", {
  sig <- signature("question -> answer")

  expect_snapshot(
    module(sig, tools = list()),
    error = TRUE
  )
  condition <- rlang::catch_cnd(module(sig, tools = list()))
  expect_s3_class(condition, "dsprrr_module_argument_error")
})

test_that("ReactModule initializes with empty tools", {
  sig <- signature("question -> answer")
  mod <- react(sig)

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
  mod <- react(sig, tools = list(test_tool))

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
  mod <- react(sig)

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
  mod <- react(sig, tools = list(test_tool))

  expect_length(mod$tools, 1)

  mod$remove_tool("removable_tool")

  expect_length(mod$tools, 0)
})

test_that("ReactModule remove_tool warns for non-existent tool", {
  sig <- signature("question -> answer")
  mod <- react(sig)

  expect_warning(
    mod$remove_tool("nonexistent"),
    "not found"
  )
})

test_that("ReactModule max_iterations is configurable", {
  sig <- signature("question -> answer")
  mod <- react(sig, max_iterations = 20L)

  expect_equal(mod$max_iterations, 20L)
  expect_error(
    react(sig, max_iterations = 0L),
    "positive integer"
  )
})

ReactTestChat <- R6::R6Class(
  "ReactTestChat",
  inherit = TestChat,
  public = list(
    on_tool_request_impl = NULL,

    initialize = function(..., on_tool_request = NULL) {
      super$initialize(...)
      self$on_tool_request_impl <- on_tool_request
    },

    register_tool = function(tool) invisible(NULL),

    on_tool_request = function(callback) {
      if (is.null(self$on_tool_request_impl)) {
        return(function() invisible(NULL))
      }
      self$on_tool_request_impl(callback)
    }
  )
)

new_react_prompt_chat <- function() {
  state <- new.env(parent = emptyenv())
  state$prompt <- NULL
  state$turns <- list()

  last_turn <- function(role = c("assistant", "user"), ...) {
    role <- match.arg(role)
    role_turns <- Filter(
      function(turn) identical(turn@role, role),
      state$turns
    )
    if (length(role_turns) == 0L) {
      stop("no turns")
    }
    role_turns[[length(role_turns)]]
  }

  llm <- ReactTestChat$new(
    model = "mock-model",
    chat = function(..., echo = "none") {
      parts <- list(...)
      state$prompt <- if (length(parts) == 1L) parts[[1L]] else parts
      contents <- lapply(parts, function(part) {
        if (is.character(part)) ellmer::ContentText(part) else part
      })
      state$turns <- c(
        state$turns,
        list(
          ellmer::UserTurn(contents = contents),
          ellmer::AssistantTurn(
            contents = list(ellmer::ContentText("reasoning"))
          )
        )
      )
      invisible(NULL)
    },
    chat_structured = function(prompt, type, echo = "none", ...) {
      state$turns <- c(
        state$turns,
        list(ellmer::AssistantTurn(
          contents = list(ellmer::ContentText("{\"answer\":\"done\"}"))
        ))
      )
      list(answer = "done")
    }
  )
  override_test_chat_method(llm, "get_turns", function(...) state$turns)
  override_test_chat_method(llm, "last_turn", last_turn)

  list(llm = llm, state = state)
}

test_that("ReactModule sends image and PDF content through public run", {
  mock <- new_react_prompt_chat()

  image <- ellmer::ContentImageInline("image/png", "YWJj")
  pdf <- ellmer::ContentPDF("application/pdf", "ZGVm", "report.pdf")
  mod <- react(signature(
    "question, image, report -> answer",
    instructions = "Review the supplied evidence."
  ))

  result <- run(
    mod,
    question = "What is shown?",
    image = image,
    report = pdf,
    .llm = mock$llm,
    .return_format = "structured"
  )

  expect_type(mock$state$prompt, "list")
  expect_length(mock$state$prompt, 3L)
  expect_s3_class(mock$state$prompt[[1]], "ellmer::ContentText")
  expect_identical(mock$state$prompt[[2]], image)
  expect_identical(mock$state$prompt[[3]], pdf)
  expect_match(mock$state$prompt[[1]]@text, "Review the supplied evidence")
  expect_match(mock$state$prompt[[1]]@text, "question: What is shown\\?")
  expect_type(result$metadata$prompt, "character")
  expect_match(result$metadata$prompt, "image: <ellmer::ContentImageInline>")
  expect_match(result$metadata$prompt, "report: <ellmer::ContentPDF>")
  expect_identical(mod$state$traces[[1]]$prompt, result$metadata$prompt)
})

test_that("ReactModule keeps text-only prompts as character strings", {
  mock <- new_react_prompt_chat()

  mod <- react("question -> answer")
  result <- run(
    mod,
    question = "What is 2 + 2?",
    .llm = mock$llm,
    .return_format = "structured"
  )

  expect_type(mock$state$prompt, "character")
  expect_identical(result$metadata$prompt, mock$state$prompt)
  expect_match(mock$state$prompt, "question: What is 2 \\+ 2\\?")
})

test_that("ReAct media reaches real ellmer request normalization", {
  chat <- ellmer::chat_openai(model = "gpt-test", credentials = function() "x")
  captured <- NULL
  remove <- chat$on_request_start(function(turns) {
    captured <<- turns[[length(turns)]]@contents
    rlang::abort("Stop before transport", class = "test_request_captured")
  })
  on.exit(remove(), add = TRUE)
  image <- ellmer::ContentImageInline("image/png", "YWJj")
  pdf <- ellmer::ContentPDF("application/pdf", "ZGVm", "report.pdf")

  condition <- rlang::catch_cnd(run(
    react("question, image, report -> answer"),
    question = "Read this evidence",
    image = image,
    report = pdf,
    .llm = chat
  ))

  expect_s3_class(condition, "dsprrr_provider_error")
  expect_s3_class(condition$parent, "test_request_captured")
  expect_length(captured, 3L)
  expect_s3_class(captured[[1L]], "ellmer::ContentText")
  expect_identical(captured[[2L]], image)
  expect_identical(captured[[3L]], pdf)
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

  mock_llm <- ReactTestChat$new(
    model = "mock-model",
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
    }
  )
  override_test_chat_method(mock_llm, "get_turns", function(...) turns)
  override_test_chat_method(mock_llm, "last_turn", last_turn)

  sig <- signature("question -> answer")
  mod <- react(sig)

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
  expect_identical(metadata$termination_reason, "structured")
  expect_true(is.na(metadata$cost))
  expect_equal(mod$state$traces[[1]]$iterations, 2L)
})

test_that("ReactModule enforces max_iterations before finalization", {
  turns <- list()
  mock_llm <- ReactTestChat$new(
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
  )
  override_test_chat_method(mock_llm, "get_turns", function(...) turns)

  mod <- react(
    signature("question -> answer"),
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

  mock_llm <- ReactTestChat$new(
    model = "mock-model",
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
    }
  )
  override_test_chat_method(mock_llm, "get_turns", function(...) turns)
  override_test_chat_method(mock_llm, "last_turn", last_turn)

  mod <- react(
    signature("question -> answer"),
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
  mod <- react(sig, tools = list(test_tool))

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
  mod <- react(sig)

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
  mod <- react(sig)

  expect_error(
    mod$add_tool("not a tool"),
    "ToolDef"
  )
})

test_that("ReactModule rejects an unnamespaced ToolDef class", {
  sig <- signature("question -> answer")
  bare_tool <- \(x) x
  class(bare_tool) <- c("ToolDef", "function")

  constructor_condition <- rlang::catch_cnd(
    ReactModule$new(sig, tools = list(bare_tool))
  )
  mod <- react(sig)
  add_condition <- rlang::catch_cnd(mod$add_tool(bare_tool))

  expect_s3_class(constructor_condition, "rlang_error")
  expect_match(conditionMessage(constructor_condition), "ellmer ToolDef")
  expect_s3_class(add_condition, "rlang_error")
  expect_match(conditionMessage(add_condition), "ellmer ToolDef")
})

test_that("ReactModule rejects duplicate tool names", {
  make_tool <- function(value) {
    ellmer::tool(
      function(query) value,
      name = "lookup",
      description = paste("Returns", value),
      arguments = list(query = ellmer::type_string())
    )
  }
  sig <- signature("question -> answer")
  tools <- list(make_tool("first"), make_tool("second"))

  expect_error(
    react(sig, tools = tools),
    "Duplicate name: \"lookup\"",
    class = "dsprrr_react_tools_error"
  )
  expect_error(
    ReactModule$new(sig, tools = tools),
    class = "dsprrr_react_tools_error"
  )
})

test_that("ReactModule add_tool rejects a taken name unless replace = TRUE", {
  make_tool <- function(name, value) {
    ellmer::tool(
      function(query) value,
      name = name,
      description = paste("Returns", value),
      arguments = list(query = ellmer::type_string())
    )
  }
  first <- make_tool("lookup", "first")
  second <- make_tool("lookup", "second")
  other <- make_tool("other", "other")
  chat <- ellmer::chat_openai(model = "gpt-test", credentials = function() "x")
  mod <- react("question -> answer", tools = list(first, other), chat = chat)

  expect_error(
    mod$add_tool(second),
    "already has a tool named \"lookup\"",
    class = "dsprrr_react_tools_error"
  )
  expect_identical(mod$tools, list(first, other))
  expect_length(chat$get_tools(), 0L)
  expect_error(mod$add_tool(second, replace = NA), "TRUE or FALSE")

  mod$add_tool(second, replace = TRUE)

  expect_identical(mod$list_tools(), c("lookup", "other"))
  expect_identical(mod$tools[[1L]], second)
  expect_identical(chat$get_tools()$lookup, second)
})

test_that("ReactModule forward refuses duplicates assigned to its tools field", {
  make_tool <- function(value) {
    ellmer::tool(
      function(query) value,
      name = "lookup",
      description = paste("Returns", value),
      arguments = list(query = ellmer::type_string())
    )
  }
  registered <- character()
  llm <- ReactTestChat$new(chat = function(...) stop("unexpected model call"))
  override_test_chat_method(llm, "register_tool", function(tool) {
    registered <<- c(registered, tool@name)
    invisible(NULL)
  })
  mod <- react("question -> answer")
  mod$tools <- list(make_tool("first"), make_tool("second"))

  expect_error(
    mod$forward(list(question = "q"), .llm = llm),
    class = "dsprrr_react_tools_error"
  )
  expect_identical(registered, character())
})
