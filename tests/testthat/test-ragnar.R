test_that("format_search_results() numbers documents and keeps sources", {
  results <- data.frame(
    text = c("First document.", "Second document."),
    source = c("a.md", "b.md")
  )

  formatted <- format_search_results(results)

  expect_match(formatted, "[Result 1] (a.md)\nFirst document.", fixed = TRUE)
  expect_match(formatted, "[Result 2] (b.md)\nSecond document.", fixed = TRUE)
  expect_equal(format_search_results(results[0, ]), "No results found.")
  expect_equal(format_search_results(c("a", "b")), "a\n\nb")
})

test_that("ragnar_tool() returns an ellmer tool that react() accepts", {
  local_mocked_bindings(
    check_installed = function(...) invisible(TRUE),
    .package = "rlang"
  )

  tool <- ragnar_tool(store = NULL, k = 3L, name = "search_docs")

  expect_true(S7::S7_inherits(tool, ellmer::ToolDef))
  expect_equal(tool@name, "search_docs")
  expect_named(tool@arguments@properties, "query")
  expect_no_error(react(signature("question -> answer"), tools = list(tool)))
})
