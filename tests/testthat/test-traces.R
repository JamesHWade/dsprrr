test_that("export_traces renders turn content with tool fallbacks", {
  mod <- module(signature("question -> answer"), type = "predict")

  tool_request <- ellmer::ContentToolRequest(
    id = "tool-1",
    name = "lookup",
    arguments = list(question = "What is R?")
  )
  tool_result <- ellmer::ContentToolResult(
    value = list(answer = "A language"),
    request = tool_request
  )

  mod$state$traces <- list(
    list(
      timestamp = Sys.time(),
      user_turn = ellmer::UserTurn(
        contents = list(ellmer::ContentText("Question"), tool_request)
      ),
      assistant_turn = ellmer::AssistantTurn(
        contents = list(tool_result)
      ),
      turns = list(),
      output = list(answer = "A language"),
      model = "mock-model",
      latency_ms = 100,
      tokens = list(input_tokens = 10L, output_tokens = 5L, total_tokens = 15L),
      cost = 0.001
    )
  )

  traces <- export_traces(mod, include_prompts = TRUE, include_outputs = TRUE)

  expect_true(grepl("Question", traces$prompt[[1]], fixed = TRUE))
  expect_true(grepl(
    "tool result",
    traces$response_text[[1]],
    ignore.case = TRUE
  ))
  expect_true(grepl("lookup", traces$response_text[[1]], fixed = TRUE))
  expect_true(grepl("<p>", traces$prompt_html[[1]], fixed = TRUE))
})
