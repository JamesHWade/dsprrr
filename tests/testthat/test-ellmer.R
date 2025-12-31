# Tests for ellmer integration (as_ellmer_tool, register_dsprrr_tool)

test_that("as_ellmer_tool creates tool with correct function signature", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> sentiment"),
    type = "predict"
  )

  tool <- as_ellmer_tool(
    mod,
    name = "test_sentiment",
    description = "Analyze sentiment"
  )

  # Tool should be an ellmer ToolDef (which is also a function)
  expect_s3_class(tool, "ellmer::ToolDef")
  expect_true(is.function(tool))

  # Tool name and description should match
  expect_equal(tool@name, "test_sentiment")
  expect_equal(tool@description, "Analyze sentiment")

  # Function should have named parameter matching signature input
  # ToolDef IS the function, so formals() works directly
  fn_args <- names(formals(tool))
  expect_equal(fn_args, "text")
})

test_that("as_ellmer_tool creates tool with multiple inputs", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("context, question -> answer"),
    type = "predict"
  )

  tool <- as_ellmer_tool(
    mod,
    name = "qa_tool",
    description = "Answer questions"
  )

  # Function should have both parameters
  fn_args <- names(formals(tool))
  expect_equal(fn_args, c("context", "question"))
})

test_that("as_ellmer_tool function can be invoked with mock LLM", {
  skip_if_not_installed("ellmer")

  # Create mock LLM that returns predictable output
  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(sentiment = "positive")
      },
      last_turn = function(...) NULL
    ),
    class = "Chat"
  )

  mod <- module(
    signature("text -> sentiment"),
    type = "predict"
  )

  tool <- as_ellmer_tool(
    mod,
    name = "test_sentiment",
    description = "Analyze sentiment",
    .llm = mock_llm
  )

  # Actually invoke the tool function (ToolDef IS the function)
  result <- tool(text = "I love this!")

  # Should return JSON-formatted result
  expect_type(result, "character")
  parsed <- jsonlite::fromJSON(result)
  expect_equal(parsed$sentiment, "positive")
})

test_that("as_ellmer_tool generates description from signature if not provided", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> sentiment", instructions = "Analyze the sentiment"),
    type = "predict"
  )

  tool <- as_ellmer_tool(mod, name = "test_tool")

  # Should use instructions as description
  expect_equal(tool@description, "Analyze the sentiment")
})

test_that("as_ellmer_tool generates fallback description without instructions", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> sentiment"),
    type = "predict"
  )

  tool <- as_ellmer_tool(mod, name = "test_tool")

  # Should generate description from inputs
  expect_match(tool@description, "text")
})

test_that("as_ellmer_tool generates name from signature if not provided", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> sentiment"),
    type = "predict"
  )

  tool <- as_ellmer_tool(mod, description = "Test tool")

  # Should generate a name (implementation-dependent)
  expect_type(tool@name, "character")
  expect_true(nzchar(tool@name))
})

test_that("register_dsprrr_tool registers tool with chat", {
  skip_if_not_installed("ellmer")

  # Create a mock chat that tracks registered tools
  registered_tools <- list()
  mock_chat <- structure(
    list(
      register_tool = function(tool) {
        registered_tools <<- c(registered_tools, list(tool))
      }
    ),
    class = "Chat"
  )

  mod <- module(
    signature("text -> sentiment"),
    type = "predict"
  )

  result <- register_dsprrr_tool(
    mock_chat,
    mod,
    name = "sentiment_tool",
    description = "Analyze sentiment"
  )

  # Should return chat invisibly
  expect_identical(result, mock_chat)

  # Should have registered one tool
  expect_length(registered_tools, 1)
  expect_equal(registered_tools[[1]]@name, "sentiment_tool")
})

test_that("as_ellmer_tool function environment has access to run", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> answer"),
    type = "predict"
  )

  tool <- as_ellmer_tool(mod, name = "test", description = "test")

  # The function environment should have access to dsprrr functions
  fn_env <- environment(tool)

  # Should be able to find 'run' in the environment chain
  expect_true(
    exists("run", envir = fn_env, inherits = TRUE),
    info = "Tool function environment should have access to 'run'"
  )
})

test_that("as_ellmer_tool handles errors from module", {
  skip_if_not_installed("ellmer")

  # Create mock LLM that throws an error
  mock_llm <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        stop("API error")
      }
    ),
    class = "Chat"
  )

  mod <- module(
    signature("text -> sentiment"),
    type = "predict"
  )

  tool <- as_ellmer_tool(
    mod,
    name = "test_sentiment",
    description = "Analyze sentiment",
    .llm = mock_llm
  )

  # Errors should propagate (not be silently caught)
  expect_error(tool(text = "test"), "API error")
})
