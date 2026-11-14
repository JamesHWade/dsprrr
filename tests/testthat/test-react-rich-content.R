# Exercise real ellmer tool dispatch and request serialization; only transport
# is mocked. This catches content being flattened before the provider boundary.
local_rich_tool_backend <- function(calls, .env = parent.frame()) {
  testthat::local_mocked_bindings(
    req_perform = function(req) {
      calls$requests <- c(calls$requests, list(req$body$data))
      index <- length(calls$requests)
      output <- if (index == 1L) {
        list(list(
          type = "function_call",
          id = "call_media",
          call_id = "call_media",
          name = "media",
          arguments = '{"query":"look"}',
          status = "completed"
        ))
      } else {
        list(list(
          id = paste0("msg_", index),
          type = "message",
          status = "completed",
          role = "assistant",
          content = list(list(
            type = "output_text",
            annotations = list(),
            logprobs = list(),
            text = if (index == 2L) {
              "The media are available."
            } else {
              '{"answer":"seen"}'
            }
          ))
        ))
      }
      body <- list(
        id = paste0("resp_", index),
        object = "response",
        created_at = 1L,
        status = "completed",
        model = "model-a",
        output = output,
        usage = list(
          input_tokens = 8L,
          input_tokens_details = list(cached_tokens = 0L),
          output_tokens = 2L,
          output_tokens_details = list(reasoning_tokens = 0L),
          total_tokens = 10L
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
}

for (route in c("native", "module_raw")) {
  test_that(paste("ReAct preserves rich tool content through", route), {
    calls <- new.env(parent = emptyenv())
    calls$requests <- list()
    calls$tools <- 0L
    local_rich_tool_backend(calls)
    parts <- list(
      ellmer::ContentText("Caption before media"),
      ellmer::content_image_url("https://example.com/image.png"),
      ellmer::ContentImageInline("image/png", "YWJj"),
      ellmer::ContentPDF("application/pdf", "ZGVm", "report.pdf")
    )
    get_media <- function(query) {
      calls$tools <- calls$tools + 1L
      expect_identical(query, "look")
      parts
    }
    tool <- if (route == "native") {
      ellmer::tool(
        get_media,
        "Get media",
        arguments = list(query = ellmer::type_string()),
        name = "media"
      )
    } else {
      media_module <- module_fn(
        signature(
          inputs = list(input("query")),
          output_type = ellmer::type_object()
        ),
        get_media
      )
      as_ellmer_tool(media_module, name = "media", output = "raw")
    }
    chat <- suppressWarnings(ellmer::chat_openai(
      api_key = "dummy-key",
      model = "model-a"
    ))
    result <- run(
      react("question -> answer", tools = list(tool)),
      question = "Inspect the media",
      .llm = chat,
      .return_format = "structured"
    )

    expect_identical(result$output, list(answer = "seen"))
    expect_identical(calls$tools, 1L)
    expect_length(calls$requests, 3L)
    tool_results <- Filter(
      function(item) identical(item$type, "function_call_output"),
      calls$requests[[2L]]$input
    )
    expect_length(tool_results, 1L)
    expect_identical(tool_results[[1L]]$call_id, "call_media")
    expect_match(
      tool_results[[1L]]$output,
      'call-id="call_media"',
      fixed = TRUE
    )
    content <- unlist(
      lapply(calls$requests[[2L]]$input, function(item) item$content),
      recursive = FALSE
    )
    selected <- Filter(
      function(item) {
        identical(item$text, "Caption before media") ||
          item$type %in% c("input_image", "input_file")
      },
      content
    )
    expect_identical(
      vapply(selected, `[[`, character(1), "type"),
      c("input_text", "input_image", "input_image", "input_file")
    )
    expect_identical(selected[[2L]]$image_url, "https://example.com/image.png")
    expect_identical(selected[[3L]]$image_url, "data:image/png;base64,YWJj")
    expect_identical(selected[[4L]]$filename, "report.pdf")
    expect_identical(
      selected[[4L]]$file_data,
      "data:application/pdf;base64,ZGVm"
    )
    history_result <- result$metadata$history[[3L]]@contents[[1L]]
    expect_s7_class(history_result, ellmer::ContentToolResult)
    expect_identical(history_result@value, parts)
    expect_null(history_result@error)
  })
}
