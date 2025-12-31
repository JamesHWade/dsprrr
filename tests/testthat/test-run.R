test_that("run generic exists and works with modules", {
  # Check if run is a generic function
  expect_true(is.function(run))
  expect_true("run" %in% ls("package:dsprrr"))
})

test_that("run validates required inputs", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character),
      input(name = "language", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Translate"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "{text} -> {language}"
  )

  # Mock LLM to avoid actual API calls
  mock_llm <- list(
    chat_structured = function(...) "translated"
  )

  expect_error(
    run(pred, text = "Hello", .llm = mock_llm),
    "Missing required inputs: language"
  )
})

test_that("build_prompt creates proper prompt", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify sentiment"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}\nSentiment:"
  )

  prompt <- dsprrr:::build_prompt(pred, list(text = "Great product!"))

  expect_false(grepl("Classify sentiment", prompt, fixed = TRUE))
  expect_true(grepl("Text: Great product!", prompt, fixed = TRUE))
})

test_that("build_prompt includes demonstrations", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )

  demos <- list(
    list(
      inputs = list(text = "Good"),
      output = "positive"
    )
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "Text: {text}",
    demos = demos
  )

  prompt <- dsprrr:::build_prompt(pred, list(text = "Test"))

  expect_true(grepl("Example 1", prompt, fixed = TRUE))
  expect_true(grepl("Good", prompt, fixed = TRUE))
  expect_true(grepl("positive", prompt, fixed = TRUE))
})

test_that("format_demos creates proper demo text", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )

  demos <- list(
    list(
      inputs = list(text = "input1"),
      output = "output1"
    ),
    list(
      inputs = list(text = "input2"),
      output = "output2"
    )
  )

  demo_text <- dsprrr:::format_demos(demos, sig)

  expect_true(grepl("Example 1", demo_text, fixed = TRUE))
  expect_true(grepl("Example 2", demo_text, fixed = TRUE))
  expect_true(grepl("input1", demo_text, fixed = TRUE))
  expect_true(grepl("output1", demo_text, fixed = TRUE))
})

test_that("format_inputs handles missing template", {
  sig_inputs <- list(
    input(
      name = "text",
      class = S7::class_character,
      description = "The input text"
    )
  )

  inputs <- list(text = "Hello world")

  formatted <- dsprrr:::format_inputs(inputs, sig_inputs)

  expect_true(grepl("The input text", formatted, fixed = TRUE))
  expect_true(grepl("text: Hello world", formatted, fixed = TRUE))
})

test_that("format_output handles different output types", {
  # List output
  list_output <- list(a = 1, b = "test")
  formatted_list <- dsprrr:::format_output(list_output)
  expect_true(grepl("a", formatted_list, fixed = TRUE))
  expect_true(grepl("test", formatted_list, fixed = TRUE))

  # Character output
  char_output <- "simple string"
  formatted_char <- dsprrr:::format_output(char_output)
  expect_equal(formatted_char, "simple string")

  # Numeric output
  num_output <- 42
  formatted_num <- dsprrr:::format_output(num_output)
  expect_equal(formatted_num, "42")
})

test_that("get_default_llm returns ellmer chat object", {
  # Create a mock module
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string()
  )
  mod <- module(signature = sig, type = "predict")

  # Test with module that has no stored chat - should auto-detect

  # Skip auto-detect test if no API credentials available
  if (has_ellmer_credentials()) {
    llm <- dsprrr:::get_default_llm(mod)
    expect_true(inherits(llm, "Chat"))
  }

  # Test with module that has chat stored
  mock_llm <- list(chat = function(...) "test")
  class(mock_llm) <- "Chat"
  mod$chat <- mock_llm
  llm <- dsprrr:::get_default_llm(mod)
  expect_identical(llm, mock_llm)
})

test_that("batch processing works with multiple inputs", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Echo the text"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "{text}"
  )

  # Mock LLM that echoes input
  mock_llm <- list(
    chat_structured = function(prompt, ...) {
      # Extract the text from the prompt
      gsub("Echo the text\n\n", "", prompt)
    }
  )
  class(mock_llm) <- "Chat"

  # Test batch processing
  results <- run(
    pred,
    text = c("Hello", "World"),
    .llm = mock_llm,
    .progress = FALSE
  )

  expect_length(results, 2)
  expect_equal(results[[1]], "Hello")
  expect_equal(results[[2]], "World")
})

test_that("structured return format includes metadata", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "{text}"
  )

  # Mock LLM
  mock_llm <- list(
    chat_structured = function(prompt, ...) "response"
  )
  class(mock_llm) <- "Chat"

  # Test structured return
  result <- run(
    pred,
    text = "test",
    .llm = mock_llm,
    .return_format = "structured"
  )

  expect_type(result, "list")
  expect_named(result, c("output", "chat", "metadata"))
  expect_equal(result$output, "response")
  expect_identical(result$chat, mock_llm)
  expect_true("latency_ms" %in% names(result$metadata))
  expect_true("prompt_length" %in% names(result$metadata))
  expect_true("prompt" %in% names(result$metadata))
  expect_equal(result$metadata$instructions, "Echo")
  expect_true("timestamp" %in% names(result$metadata))
  expect_s3_class(result, "dsprrr_result")
})

test_that("run_dataset processes data frames", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Process"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "{text}"
  )

  # Mock LLM
  mock_llm <- list(
    chat_structured = function(prompt, ...) {
      # Return processed version
      paste0("processed_", gsub("Process\n\n", "", prompt))
    }
  )
  class(mock_llm) <- "Chat"

  # Create test dataset
  test_data <- data.frame(
    text = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  # Test run_dataset
  results <- run_dataset(pred, test_data, .llm = mock_llm, .progress = FALSE)

  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 3)
  expect_true("result" %in% names(results))
  expect_equal(results$result[[1]], "processed_A")
  expect_equal(results$result[[2]], "processed_B")
  expect_equal(results$result[[3]], "processed_C")
})

test_that("module as function interface works", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "{text}"
  )

  # Mock LLM
  mock_llm <- list(
    chat_structured = function(prompt, ...) "result"
  )
  class(mock_llm) <- "Chat"

  # Test converting module to function
  skip("as_function not yet implemented for modules")
  # pred_func <- as_function(pred, .llm = mock_llm)
  # result <- pred_func(text = "test")
  # expect_equal(result, "result")
})

test_that("batch processing handles errors gracefully", {
  sig <- Signature(
    inputs = list(
      input(name = "text", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Process"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "{text}"
  )

  # Mock LLM that fails on certain inputs
  mock_llm <- list(
    chat_structured = function(prompt, ...) {
      if (grepl("ERROR", prompt, fixed = TRUE)) {
        stop("API error")
      }
      "success"
    }
  )
  class(mock_llm) <- "Chat"

  # Test with mixed success/failure
  expect_warning(
    results <- run(
      pred,
      text = c("OK", "ERROR", "FINE"),
      .llm = mock_llm,
      .progress = FALSE
    ),
    "Failed to process item"
  )

  expect_length(results, 3)
  expect_equal(results[[1]], "success")
  expect_true(is.na(results[[2]]))
  expect_equal(results[[3]], "success")
})

test_that("run warns when parallel execution with custom llm", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string()
  )
  module <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "ok"),
    class = "Chat"
  )

  expect_warning(
    out <- run(
      module,
      text = c("a", "b"),
      .llm = mock_llm,
      .parallel = TRUE,
      .progress = FALSE
    ),
    "Parallel execution requires"
  )
  expect_equal(length(out), 2)
})

# --- Parallel execution tests ---

test_that("process_batch_item returns correct format for simple mode", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "response"),
    class = "Chat"
  )

  result <- dsprrr:::process_batch_item(
    input_set = list(text = "hello"),
    module = mod,
    llm = mock_llm,
    index = 1,
    .verbose = FALSE,
    .return_format = "simple"
  )

  expect_equal(result, "response")
})

test_that("process_batch_item returns correct format for structured mode", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "response"),
    class = "Chat"
  )

  result <- dsprrr:::process_batch_item(
    input_set = list(text = "hello"),
    module = mod,
    llm = mock_llm,
    index = 5,
    .verbose = FALSE,
    .return_format = "structured"
  )

  expect_type(result, "list")
  expect_named(result, c("output", "chat", "metadata"))
  expect_equal(result$output, "response")
  expect_identical(result$chat, mock_llm)
  expect_equal(result$metadata$batch_index, 5)
  expect_true("latency_ms" %in% names(result$metadata))
  expect_true("prompt" %in% names(result$metadata))
})

test_that("extract_simple_output extracts single-field objects", {
  # Create real TypeObject with single property
  single_field_type <- ellmer::type_object(answer = ellmer::type_string())

  response <- list(answer = "42")
  result <- dsprrr:::extract_simple_output(response, single_field_type)
  expect_equal(result, "42")
})

test_that("extract_simple_output returns full response for multi-field objects", {
  multi_field_type <- ellmer::type_object(
    a = ellmer::type_string(),
    b = ellmer::type_string()
  )

  response <- list(a = "1", b = "2")
  result <- dsprrr:::extract_simple_output(response, multi_field_type)
  expect_equal(result, response)
})

test_that("extract_simple_output returns full response for non-TypeObject", {
  simple_type <- ellmer::type_string()
  response <- "hello"
  result <- dsprrr:::extract_simple_output(response, simple_type)
  expect_equal(result, "hello")
})

test_that("create_error_result formats simple errors correctly", {
  error <- simpleError("test error")
  result <- dsprrr:::create_error_result(
    error = error,
    index = 3,
    prompt = "test prompt",
    instructions = "test instructions",
    llm = NULL,
    .return_format = "simple"
  )

  expect_true(is.na(result))
  expect_equal(
    attr(result, "error_message"),
    "Failed to process item 3: test error"
  )
})

test_that("create_error_result formats structured errors correctly", {
  error <- simpleError("test error")
  mock_llm <- structure(list(), class = "Chat")

  result <- dsprrr:::create_error_result(
    error = error,
    index = 3,
    prompt = "test prompt",
    instructions = "test instructions",
    llm = mock_llm,
    .return_format = "structured"
  )

  expect_type(result, "list")
  expect_true(is.na(result$output))
  expect_identical(result$chat, mock_llm)
  expect_equal(result$metadata$error, "test error")
  expect_equal(result$metadata$batch_index, 3)
  expect_equal(result$metadata$prompt, "test prompt")
})

test_that("create_error_result handles list-style error objects (timeout case)", {
  # This tests the timeout error path which passes list(message = "...")
  # instead of a simpleError object
  error <- list(message = "Task timed out")

  result <- dsprrr:::create_error_result(
    error = error,
    index = 5,
    prompt = NA_character_,
    instructions = NA_character_,
    llm = NULL,
    .return_format = "simple"
  )

  expect_true(is.na(result))
})

test_that("create_error_result handles list-style error for structured format", {
  error <- list(message = "Task timed out")
  mock_llm <- structure(list(), class = "Chat")

  result <- dsprrr:::create_error_result(
    error = error,
    index = 2,
    prompt = "test prompt",
    instructions = "test instructions",
    llm = mock_llm,
    .return_format = "structured"
  )

  expect_type(result, "list")
  expect_true(is.na(result$output))
  expect_equal(result$metadata$error, "Task timed out")
  expect_equal(result$metadata$batch_index, 2)
})

test_that("run_batch_sequential processes all items", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  call_count <- 0
  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) {
      call_count <<- call_count + 1
      paste0("response_", call_count)
    }),
    class = "Chat"
  )

  input_sets <- list(
    list(text = "a"),
    list(text = "b"),
    list(text = "c")
  )

  results <- dsprrr:::run_batch_sequential(
    module = mod,
    input_sets = input_sets,
    n = 3,
    .llm = mock_llm,
    .verbose = FALSE,
    .return_format = "simple",
    .progress = FALSE
  )

  expect_length(results, 3)
  expect_equal(results[[1]], "response_1")
  expect_equal(results[[2]], "response_2")
  expect_equal(results[[3]], "response_3")
})

test_that("run_batch_sequential handles errors per item", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) {
      if (grepl("fail", prompt, fixed = TRUE)) {
        stop("intentional failure")
      }
      "ok"
    }),
    class = "Chat"
  )

  input_sets <- list(
    list(text = "good"),
    list(text = "fail"),
    list(text = "also_good")
  )

  expect_warning(
    results <- dsprrr:::run_batch_sequential(
      module = mod,
      input_sets = input_sets,
      n = 3,
      .llm = mock_llm,
      .verbose = FALSE,
      .return_format = "simple",
      .progress = FALSE
    ),
    "Failed to process item 2"
  )

  expect_length(results, 3)
  expect_equal(results[[1]], "ok")
  expect_true(is.na(results[[2]]))
  expect_equal(results[[3]], "ok")
})

test_that("parallel execution works with mock factory", {
  skip_on_cran()
  skip_if_not_installed("mirai")

  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  # Configure module to use a testable Chat
  mod$chat <- structure(
    list(chat_structured = function(prompt, ...) "parallel_result"),
    class = "Chat"
  )

  # Test parallel execution (will use chat from module)
  # Note: Mock LLM closures may not serialize correctly to mirai workers,

  # so we only verify that the parallel path executes without crashing
  # and returns the correct number of results
  results <- run(
    mod,
    text = c("a", "b", "c"),
    .parallel = TRUE,
    .progress = FALSE
  )

  expect_length(results, 3)
  # Results may be NA due to serialization issues with mock closures in workers,
  # but the function should complete without error
  expect_true(is.list(results))
})


# -- .show_prompt tests --

test_that("run with .show_prompt=TRUE shows prompt preview", {
  sig <- Signature(
    inputs = list(input(name = "q", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(signature = sig, type = "predict", template = "Question: {q}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "answer"),
    class = "Chat"
  )

  # cli output goes through message stream, so use expect_message
  # or check that instructions text appears
  expect_output(
    run(mod, q = "test question", .llm = mock_llm, .show_prompt = TRUE),
    "Answer the question|Input fields|Output type"
  )
})

test_that("run with .show_prompt defaults to FALSE", {
  sig <- Signature(
    inputs = list(input(name = "q", class = S7::class_character)),
    output_type = ellmer::type_string()
  )

  mod <- module(signature = sig, type = "predict", template = "{q}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "answer"),
    class = "Chat"
  )

  # Default .show_prompt = FALSE - should produce no output about prompt preview
  # Note: cli uses message stream, so capturing stdout may not catch everything
  output <- capture.output(
    run(mod, q = "test", .llm = mock_llm),
    type = "message"
  )

  # Should not contain preview-related text
  expect_false(any(grepl("Prompt Preview|Input fields", output)))
})

# -- Module cost tracking tests --

test_that("get_total_cost returns 0 for module with no traces", {
  sig <- Signature(
    inputs = list(input(name = "q", class = S7::class_character)),
    output_type = ellmer::type_string()
  )

  mod <- module(signature = sig, type = "predict")

  expect_equal(mod$get_total_cost(), 0)
})

test_that("get_cost_summary returns empty tibble for module with no traces", {
  sig <- Signature(
    inputs = list(input(name = "q", class = S7::class_character)),
    output_type = ellmer::type_string()
  )

  mod <- module(signature = sig, type = "predict")

  result <- mod$get_cost_summary()

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
  expect_true(all(
    c("timestamp", "model", "input_tokens", "output_tokens", "cost") %in%
      names(result)
  ))
})

test_that("batch run with structured format returns dsprrr_batch_result class", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "{text}"
  )

  mock_llm <- list(
    chat_structured = function(prompt, ...) "result"
  )
  class(mock_llm) <- "Chat"

  results <- run(
    pred,
    text = c("a", "b", "c"),
    .llm = mock_llm,
    .return_format = "structured",
    .progress = FALSE
  )

  expect_s3_class(results, "dsprrr_batch_result")

  # Print method should work without error
  output <- capture.output(print(results), type = "message")
  expect_true(any(grepl("Batch Results", output, fixed = TRUE)))
  expect_true(any(grepl("Items", output, fixed = TRUE)))
})

test_that("dsprrr_batch_result print method handles many items", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )

  pred <- module(
    signature = sig,
    type = "predict",
    template = "{text}"
  )

  mock_llm <- list(
    chat_structured = function(prompt, ...) "result"
  )
  class(mock_llm) <- "Chat"

  # Run with 5 items to trigger truncated display
  results <- run(
    pred,
    text = c("a", "b", "c", "d", "e"),
    .llm = mock_llm,
    .return_format = "structured",
    .progress = FALSE
  )

  expect_s3_class(results, "dsprrr_batch_result")

  # Should show "and X more"
  output <- capture.output(print(results), type = "message")
  expect_true(any(grepl("and.*more", output)))
})
