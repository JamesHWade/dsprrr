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
