test_that("export_traces renders turn content with tool fallbacks", {
  mod <- module(signature("question -> answer"))

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

test_that("export_traces omits prompts and responses unless requested", {
  mod <- module(signature("question -> answer"))
  mod$state$traces <- list(
    list(
      timestamp = Sys.time(),
      prompt = "What is the secret?",
      output = list(answer = "hunter2"),
      model = "mock-model",
      latency_ms = 100,
      tokens = list(input_tokens = 10L, output_tokens = 5L, total_tokens = 15L),
      cost = 0.001
    )
  )

  metrics_only <- export_traces(mod)
  expect_false("prompt" %in% names(metrics_only))
  expect_false("response" %in% names(metrics_only))
  expect_identical(metrics_only$total_tokens, 15L)

  with_prompts <- export_traces(mod, include_prompts = TRUE)
  expect_true("prompt" %in% names(with_prompts))
  expect_false("response" %in% names(with_prompts))

  with_outputs <- export_traces(mod, include_outputs = TRUE)
  expect_false("prompt" %in% names(with_outputs))
  expect_true("response" %in% names(with_outputs))
})
