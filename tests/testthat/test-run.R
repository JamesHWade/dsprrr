test_that("run generic exists and works with modules", {
  # Check if run is a generic function
  expect_true(is.function(run))
  expect_true("run" %in% ls("package:dsprrr"))
})

test_that("run.PredictModule warns about input type mismatches", {
  sig <- signature(
    inputs = list(input_integer("count")),
    output_type = ellmer::type_object(answer = ellmer::type_string())
  )
  mod <- module(sig, type = "predict")
  mock_chat <- structure(
    list(
      chat_structured = function(...) list(answer = "ok"),
      get_model = function() "mock-model",
      get_turns = function() list(),
      last_turn = function(role = NULL) NULL
    ),
    class = "Chat"
  )

  old <- options(dsprrr.warn_on_type_mismatch = TRUE)
  on.exit(options(old))

  expect_warning(
    run(mod, count = "many", .llm = mock_chat, .cache = FALSE),
    "Type mismatch"
  )
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

test_that("run and Module$run preserve dotted signature input names", {
  DottedModule <- R6::R6Class(
    "DottedModule",
    inherit = dsprrr:::Module,
    public = list(
      initialize = function() {
        super$initialize(signature(".context -> answer"))
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        tibble::tibble(
          output = list(batch$.context),
          chat = list(NULL),
          metadata = list(list())
        )
      }
    )
  )

  mod <- DottedModule$new()

  expect_equal(run(mod, .context = "from-run"), "from-run")
  expect_equal(mod$run(.context = "from-module-run"), "from-module-run")
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

test_that("module config no longer synthesizes chats from provider fields", {
  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt), add = TRUE)
  clear_default_chat()

  old_env <- Sys.getenv(c(
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GOOGLE_API_KEY"
  ))
  on.exit(do.call(Sys.setenv, as.list(old_env)), add = TRUE)
  Sys.unsetenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))

  mod <- module(
    signature("text -> result"),
    type = "predict",
    config = list(provider = "openai", model = "gpt-4o-mini")
  )

  expect_error(
    run(mod, text = "hello"),
    "no longer creates Chat clients"
  )
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

test_that("structured datasets expose row-level LLM errors", {
  mod <- module(signature("text -> answer"), type = "predict")
  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) {
      if (grepl("explode", prompt, fixed = TRUE)) {
        stop("provider exploded")
      }
      "ok"
    }),
    class = "Chat"
  )

  expect_warning(
    result <- run_dataset(
      mod,
      data.frame(text = c("works", "explode")),
      .llm = mock_llm,
      .progress = FALSE,
      .return_format = "structured"
    ),
    "Failed to process item 2"
  )

  expect_named(result, c("text", "result", ".error", ".metadata", ".chat"))
  expect_true(is.na(result$.error[[1]]))
  expect_match(result$.error[[2]], "provider exploded")
  expect_true(is.na(result$result[[2]]))
  expect_identical(result$.metadata[[2]]$error_stage, "llm")
})

test_that("ellmer parallel preserves successes around a failed request", {
  observed_on_error <- NULL
  observed_usage_flags <- NULL
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(
      chat,
      prompts,
      type,
      include_tokens,
      include_cost,
      on_error,
      ...
    ) {
      observed_on_error <<- on_error
      observed_usage_flags <<- c(include_tokens, include_cost)
      tibble::tibble(
        answer = c("first", NA_character_, "third"),
        input_tokens = c(10L, 0L, 30L),
        output_tokens = c(2L, 0L, 4L),
        cached_input_tokens = c(1L, 0L, 3L),
        cost = c(0.01, 0, 0.03),
        .error = list(NULL, simpleError("parallel provider failure"), NULL)
      )
    },
    .package = "ellmer"
  )

  mod <- module(signature("text -> answer"), type = "predict")
  mock_llm <- structure(list(), class = "Chat")
  result <- dsprrr:::run_batch_ellmer_parallel(
    module = mod,
    input_sets = list(
      list(text = "a"),
      list(text = "b"),
      list(text = "c")
    ),
    n = 3,
    .llm = mock_llm,
    .verbose = FALSE,
    .return_format = "structured",
    .progress = FALSE
  )

  expect_identical(observed_on_error, "continue")
  expect_true(all(observed_usage_flags))
  expect_equal(result[[1]]$output$answer, "first")
  expect_equal(result[[3]]$output$answer, "third")
  expect_equal(result[[1]]$metadata$total_tokens, 12L)
  expect_equal(result[[1]]$metadata$cost, 0.01)
  expect_false("cost" %in% names(result[[1]]$output))
  expect_true(is.na(result[[2]]$output))
  expect_match(result[[2]]$metadata$error, "parallel provider failure")
  expect_identical(result[[2]]$metadata$error_stage, "llm")
})

test_that("ellmer parallel preserves outputs named like telemetry", {
  observed_usage_flags <- NULL
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(
      chat,
      prompts,
      type,
      include_tokens,
      include_cost,
      on_error,
      ...
    ) {
      observed_usage_flags <<- c(
        include_tokens = include_tokens,
        include_cost = include_cost
      )
      tibble::tibble(
        cost = c(1.25, 2.5),
        input_tokens = c(7L, 8L),
        .error = list(NULL, NULL)
      )
    },
    .package = "ellmer"
  )

  mod <- module(
    signature("text -> cost: number, input_tokens: integer"),
    type = "predict"
  )
  result <- dsprrr:::run_batch_ellmer_parallel(
    module = mod,
    input_sets = list(list(text = "a"), list(text = "b")),
    n = 2,
    .llm = structure(list(), class = "Chat"),
    .verbose = FALSE,
    .return_format = "structured",
    .progress = FALSE
  )

  expect_false(observed_usage_flags[["include_tokens"]])
  expect_false(observed_usage_flags[["include_cost"]])
  expect_equal(result[[1]]$output$cost, 1.25)
  expect_equal(result[[2]]$output$input_tokens, 8L)
  expect_false("cost" %in% names(result[[1]]$metadata))
  expect_false("input_tokens" %in% names(result[[1]]$metadata))
})

test_that("run warns when mirai parallel execution with custom llm", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string()
  )
  module <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "ok"),
    class = "Chat"
  )

  # mirai parallel method requires .llm = NULL
  expect_warning(
    out <- run(
      module,
      text = c("a", "b"),
      .llm = mock_llm,
      .parallel = TRUE,
      .parallel_method = "mirai",
      .progress = FALSE
    ),
    "mirai parallel execution requires"
  )
  expect_equal(length(out), 2)
})

test_that("run does NOT warn about mirai when ellmer parallel with custom llm", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string()
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "ok"),
    class = "Chat"
  )

  # ellmer parallel method should NOT warn about "mirai parallel execution"
  # when .llm is provided (ellmer needs .llm to work).
  # The call may still fail due to mock limitations (missing get_provider),
  # but it should NOT produce the mirai-specific warning.
  skip_if_not(
    exists("parallel_chat_structured", envir = asNamespace("ellmer")),
    "ellmer::parallel_chat_structured not available"
  )

  # Capture all conditions to check that mirai warning is NOT produced
  conditions <- list()
  withCallingHandlers(
    tryCatch(
      run(
        mod,
        text = c("a", "b"),
        .llm = mock_llm,
        .parallel = TRUE,
        .parallel_method = "ellmer",
        .progress = FALSE
      ),
      error = function(e) NULL
    ),
    warning = function(w) {
      conditions <<- c(conditions, list(w))
      invokeRestart("muffleWarning")
    }
  )

  # The key assertion: no "mirai parallel execution requires" warning
  mirai_warnings <- Filter(
    function(w) {
      grepl("mirai parallel execution", conditionMessage(w), fixed = TRUE)
    },
    conditions
  )
  expect_length(mirai_warnings, 0)
})

test_that("ellmer parallel falls back to sequential when unavailable with .llm", {
  skip_if(
    exists("parallel_chat_structured", envir = asNamespace("ellmer")),
    "Test only runs when ellmer::parallel_chat_structured is NOT available"
  )

  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string()
  )

  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(chat_structured = function(prompt, ...) "ok"),
    class = "Chat"
  )

  # When parallel_chat_structured is unavailable and .llm is provided,

  # should warn about falling back to sequential (not mirai)
  expect_warning(
    out <- run(
      mod,
      text = c("a", "b"),
      .llm = mock_llm,
      .parallel = TRUE,
      .parallel_method = "ellmer",
      .progress = FALSE
    ),
    "Falling back to sequential processing"
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

  turns <- list()
  mock_llm <- structure(
    list(
      chat_structured = function(prompt, ...) {
        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(
              contents = list(ellmer::ContentText(as.character(prompt)))
            ),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText("response")),
              tokens = c(10L, 2L, 1L),
              cost = 0.001
            )
          )
        )
        "response"
      },
      last_turn = function(role = "assistant") {
        ellmer::AssistantTurn(
          contents = list(ellmer::ContentText("response")),
          tokens = c(10L, 2L, 1L),
          cost = 0.001
        )
      },
      clone = function(...) mock_llm,
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      get_turns = function(...) turns
    ),
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
  # Chat is now a mock chat with turns, not the original llm
  expect_true(inherits(result$chat, "Chat"))
  expect_equal(result$metadata$batch_index, 5)
  expect_true("latency_ms" %in% names(result$metadata))
  expect_true("prompt" %in% names(result$metadata))
  expect_equal(result$metadata$total_tokens, 12L)
  expect_equal(result$metadata$cost, 0.001)
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
    "test error"
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
  expect_equal(results[[2]], "response_1")
  expect_equal(results[[3]], "response_1")
  expect_equal(call_count, 0L)
})

test_that("sequential batch rows branch from identical Chat state", {
  StatefulBatchChat <- R6::R6Class(
    "StatefulBatchChat",
    private = list(
      turns = NULL,
      system_prompt = NULL,
      tools = NULL,
      provider_config = NULL
    ),
    public = list(
      initialize = function(
        turns = list(),
        system_prompt = NULL,
        tools = list(),
        provider_config = list()
      ) {
        private$turns <- turns
        private$system_prompt <- system_prompt
        private$tools <- tools
        private$provider_config <- provider_config
      },
      get_turns = function(...) private$turns,
      get_system_prompt = function() private$system_prompt,
      get_tools = function() private$tools,
      set_turns = function(turns) {
        private$turns <- turns
        invisible(NULL)
      },
      chat_structured = function(prompt, type, echo = "none") {
        initial_length <- length(private$turns)
        private$turns <- c(
          private$turns,
          list(
            ellmer::UserTurn(
              contents = list(ellmer::ContentText(as.character(prompt)))
            ),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText("ok"))
            )
          )
        )
        list(
          answer = paste0(
            initial_length,
            ":",
            private$system_prompt,
            ":",
            length(private$tools),
            ":",
            private$provider_config$model,
            ":",
            prompt
          )
        )
      }
    )
  )

  initial_turns <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("prior"))),
    ellmer::AssistantTurn(contents = list(ellmer::ContentText("context")))
  )
  caller_chat <- StatefulBatchChat$new(
    turns = initial_turns,
    system_prompt = "system",
    tools = list("tool"),
    provider_config = list(model = "model-a")
  )
  mod <- module(
    signature("text -> answer"),
    type = "predict",
    template = "{text}"
  )

  results <- run(
    mod,
    text = c("a", "b"),
    .llm = caller_chat,
    .cache = FALSE,
    .return_format = "structured",
    .progress = FALSE
  )

  outputs <- vapply(
    results,
    function(result) result$output$answer,
    character(1)
  )
  expect_true(all(vapply(
    outputs,
    startsWith,
    logical(1),
    prefix = "2:system:1:model-a:"
  )))
  expect_true(endsWith(outputs[[1]], "a"))
  expect_true(endsWith(outputs[[2]], "b"))
  expect_length(results[[1]]$chat$get_turns(), 4)
  expect_length(results[[2]]$chat$get_turns(), 4)
  expect_identical(results[[1]]$chat$get_turns()[1:2], initial_turns)
  expect_identical(results[[2]]$chat$get_turns()[1:2], initial_turns)
  expect_true(endsWith(
    results[[1]]$chat$get_turns()[[3]]@contents[[1]]@text,
    "a"
  ))
  expect_true(endsWith(
    results[[2]]$chat$get_turns()[[3]]@contents[[1]]@text,
    "b"
  ))
  expect_length(caller_chat$get_turns(), 2)
  expect_identical(caller_chat$get_turns(), initial_turns)
})

test_that("structured batches synthesize history when a Chat records no delta", {
  NonRecordingChat <- R6::R6Class(
    "NonRecordingChat",
    private = list(turns = NULL),
    public = list(
      initialize = function(turns) {
        private$turns <- turns
        invisible(self)
      },
      get_turns = function(...) private$turns,
      set_turns = function(turns) {
        private$turns <- turns
        invisible(self)
      },
      chat_structured = function(...) list(answer = "ok")
    )
  )
  baseline <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("prior"))),
    ellmer::AssistantTurn(contents = list(ellmer::ContentText("context")))
  )
  chat <- NonRecordingChat$new(baseline)
  mod <- module(
    signature("text -> answer"),
    type = "predict",
    template = "{text}"
  )

  result <- dsprrr:::process_batch_item(
    input_set = list(text = "new"),
    module = mod,
    llm = chat,
    index = 1L,
    .verbose = FALSE,
    .return_format = "structured",
    .cache = FALSE
  )

  expect_identical(chat$get_turns(), baseline)
  expect_length(result$chat$get_turns(), 4)
  expect_identical(result$chat$get_turns()[1:2], baseline)
  expect_true(endsWith(
    result$chat$get_turns()[[3]]@contents[[1]]@text,
    "new"
  ))
})

test_that("non-cloneable closure Chats are copied before batch execution", {
  calls <- 0L
  chat <- structure(
    list(
      get_turns = function(...) list(),
      chat_structured = function(...) {
        calls <<- calls + 1L
        list(answer = "unexpected")
      }
    ),
    class = "Chat"
  )
  mod <- module(signature("text -> answer"), type = "predict")

  result <- run(
    mod,
    text = c("a", "b"),
    .llm = chat,
    .cache = FALSE,
    .progress = FALSE
  )
  expect_identical(
    unlist(result, use.names = FALSE),
    c("unexpected", "unexpected")
  )
  expect_equal(calls, 0L)
})

test_that("batch Chat isolation handles zero, one, and many rows", {
  EnvironmentStateChat <- R6::R6Class(
    "EnvironmentStateChat",
    private = list(state = NULL),
    public = list(
      initialize = function() {
        private$state <- new.env(parent = emptyenv())
        private$state$turns <- list()
      },
      get_turns = function(...) private$state$turns,
      set_turns = function(turns) {
        private$state$turns <- turns
        invisible(NULL)
      },
      chat_structured = function(...) list(answer = "ok")
    )
  )
  caller <- EnvironmentStateChat$new()

  expect_identical(dsprrr:::batch_chat_branches(caller, 0L), list())

  one <- dsprrr:::batch_chat_branches(caller, 1L)
  expect_length(one, 1L)
  one[[1]]$set_turns(list(ellmer::UserTurn("one")))
  expect_identical(caller$get_turns(), list())

  many <- dsprrr:::batch_chat_branches(caller, 3L)
  many[[1]]$set_turns(list(ellmer::UserTurn("first")))
  expect_identical(caller$get_turns(), list())
  expect_length(many[[1]]$get_turns(), 1L)
  expect_identical(many[[2]]$get_turns(), list())
  expect_identical(many[[3]]$get_turns(), list())
})

test_that("opaque closure-backed histories are copied, never shared", {
  turns <- list()
  chat <- structure(
    list(
      get_turns = function(...) turns,
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      chat_structured = function(...) list(answer = "ok")
    ),
    class = "Chat"
  )

  branches <- dsprrr:::batch_chat_branches(chat, 2L)
  branches[[1]]$set_turns(list(ellmer::UserTurn("branch one")))

  expect_identical(chat$get_turns(), list())
  expect_length(branches[[1]]$get_turns(), 1L)
  expect_identical(branches[[2]]$get_turns(), list())
})

test_that("batch isolation allows ordinary locked package functions", {
  chat <- local({
    structure(
      list(
        helper = stats::median,
        chat_structured = function(...) list(answer = "ok")
      ),
      class = "Chat"
    )
  })

  branches <- dsprrr:::batch_chat_branches(chat, 2L)

  expect_length(branches, 2L)
  expect_identical(branches[[1]]$helper(1:3), 2L)
  expect_identical(branches[[2]]$helper(2:4), 3L)
})

test_that("batch isolation normalizes canonical source metadata only", {
  initializations <- new.env(parent = emptyenv())
  initializations$lines <- 0L
  initializations$parse_data <- 0L
  srcfile <- srcfilecopy(
    "<batch-isolation-test>",
    "function(...) list(answer = 'ok')"
  )
  rm("lines", envir = srcfile)
  delayedAssign(
    "lines",
    {
      initializations$lines <- initializations$lines + 1L
      "function(...) list(answer = 'ok')"
    },
    eval.env = environment(),
    assign.env = srcfile
  )
  delayedAssign(
    "parseData",
    {
      initializations$parse_data <- initializations$parse_data + 1L
      structure(integer(), class = "parseData")
    },
    eval.env = environment(),
    assign.env = srcfile
  )
  reference <- structure(
    rep(1L, 8L),
    class = "srcref",
    srcfile = srcfile
  )
  chat_structured <- function(...) list(answer = "ok")
  attr(chat_structured, "srcref") <- reference
  chat <- structure(list(chat_structured = chat_structured), class = "Chat")

  branches <- dsprrr:::batch_chat_branches(chat, 2L)

  expect_length(branches, 2L)
  expect_identical(initializations$lines, 0L)
  expect_identical(initializations$parse_data, 0L)
  expect_true(rlang::env_binding_are_lazy(srcfile, "lines"))
  expect_true(rlang::env_binding_are_lazy(srcfile, "parseData"))
  for (branch in branches) {
    branch_reference <- attr(branch$chat_structured, "srcref", exact = TRUE)
    branch_source <- attr(branch_reference, "srcfile", exact = TRUE)
    expect_false(rlang::env_binding_are_lazy(branch_source, "lines"))
    expect_identical(branch_source$lines, character())
    expect_false(exists("parseData", envir = branch_source, inherits = FALSE))
  }

  unsafe <- function(...) list(answer = "unexpected")
  unsafe_source <- new.env(parent = emptyenv())
  delayedAssign("lines", "unsafe", assign.env = unsafe_source)
  attr(unsafe, "srcref") <- unsafe_source
  unsafe_chat <- structure(list(chat_structured = unsafe), class = "Chat")
  expect_error(
    dsprrr:::batch_chat_branches(unsafe_chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_true(rlang::env_binding_are_lazy(unsafe_source, "lines"))

  spoofed <- function(...) list(answer = "unexpected")
  spoofed_source <- new.env(parent = emptyenv())
  class(spoofed_source) <- "srcref"
  spoofed_source$shared <- globalenv()
  attr(spoofed, "srcref") <- spoofed_source
  spoofed_chat <- structure(list(chat_structured = spoofed), class = "Chat")
  expect_error(
    dsprrr:::batch_chat_branches(spoofed_chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )

  hidden <- function(...) list(answer = "unexpected")
  hidden_source <- srcfilecopy(
    "<spoofed-batch-isolation-test>",
    "function(...) list(answer = 'unexpected')"
  )
  hidden_source$shared <- globalenv()
  attr(hidden, "srcref") <- structure(
    rep(1L, 8L),
    class = "srcref",
    srcfile = hidden_source
  )
  hidden_chat <- structure(list(chat_structured = hidden), class = "Chat")
  expect_error(
    dsprrr:::batch_chat_branches(hidden_chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )

  attributed <- function(...) list(answer = "unexpected")
  attributed_source <- srcfilecopy(
    "<attributed-batch-isolation-test>",
    "function(...) list(answer = 'unexpected')"
  )
  attr(attributed_source, "shared") <- globalenv()
  attr(attributed, "srcref") <- structure(
    rep(1L, 8L),
    class = "srcref",
    srcfile = attributed_source
  )
  attributed_chat <- structure(
    list(chat_structured = attributed),
    class = "Chat"
  )
  expect_error(
    dsprrr:::batch_chat_branches(attributed_chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )

  locked <- function(...) list(answer = "unexpected")
  locked_source <- srcfilecopy(
    "<locked-batch-isolation-test>",
    "function(...) list(answer = 'unexpected')"
  )
  rm("lines", envir = locked_source)
  delayedAssign("lines", "locked", assign.env = locked_source)
  lockEnvironment(locked_source, bindings = FALSE)
  attr(locked, "srcref") <- structure(
    rep(1L, 8L),
    class = "srcref",
    srcfile = locked_source
  )
  locked_chat <- structure(list(chat_structured = locked), class = "Chat")
  expect_error(
    dsprrr:::batch_chat_branches(locked_chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_true(rlang::env_binding_are_lazy(locked_source, "lines"))

  dual_source <- srcfilecopy(
    "<dual-role-batch-isolation-test>",
    "function(...) list(answer = 'unexpected')"
  )
  dual <- function(...) list(answer = "unexpected")
  attr(dual, "srcref") <- structure(
    rep(1L, 8L),
    class = "srcref",
    srcfile = dual_source
  )
  source_first <- structure(
    list(chat_structured = dual, state_source = dual_source),
    class = "Chat"
  )
  runtime_first <- structure(
    list(state_source = dual_source, chat_structured = dual),
    class = "Chat"
  )
  expect_error(
    dsprrr:::batch_chat_branches(source_first, 1L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_error(
    dsprrr:::batch_chat_branches(runtime_first, 1L),
    class = "dsprrr_chat_isolation_error"
  )

  captured <- local({
    captured_reference <- reference
    function(...) {
      captured_source <- attr(
        captured_reference,
        "srcfile",
        exact = TRUE
      )
      list(answer = length(base::getSrcLines(captured_source, 1L, 1L)))
    }
  })
  captured_chat <- structure(
    list(chat_structured = captured),
    class = "Chat"
  )
  expect_error(
    dsprrr:::batch_chat_branches(captured_chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )

  counter <- ".dsprrr_source_metadata_initializations"
  assign(counter, 0L, envir = globalenv())
  withr::defer(rm(list = counter, envir = globalenv()))
  executable_source <- srcfilecopy(
    "<executable-batch-isolation-test>",
    "function(...) list(answer = 0L)"
  )
  rm("lines", envir = executable_source)
  delayedAssign(
    "lines",
    {
      .dsprrr_source_metadata_initializations <<-
        .dsprrr_source_metadata_initializations + 1L
      "shared source state"
    },
    eval.env = globalenv(),
    assign.env = executable_source
  )
  executable_reference <- structure(
    rep(1L, 8L),
    class = "srcref",
    srcfile = executable_source
  )
  self_referencing <- local({
    self <- function(...) {
      self_reference <- attr(self, "srcref", exact = TRUE)
      self_source <- attr(self_reference, "srcfile", exact = TRUE)
      list(answer = length(base::getSrcLines(self_source, 1L, 1L)))
    }
    attr(self, "srcref") <- executable_reference
    self
  })
  executable_chat <- structure(
    list(chat_structured = self_referencing),
    class = "Chat"
  )
  executable_branches <- dsprrr:::batch_chat_branches(executable_chat, 2L)
  expect_identical(executable_branches[[1]]$chat_structured()$answer, 0L)
  expect_identical(executable_branches[[2]]$chat_structured()$answer, 0L)
  expect_identical(
    get(counter, envir = globalenv(), inherits = FALSE),
    0L
  )
  expect_true(rlang::env_binding_are_lazy(executable_source, "lines"))

  alias_one <- srcfilealias("alias-one.R", srcfile)
  alias_two <- srcfilealias("alias-two.R", alias_one)
  alias_one$original <- alias_two
  cyclic_reference <- structure(
    rep(1L, 8L),
    class = "srcref",
    srcfile = alias_one
  )
  cyclic <- function(...) list(answer = "unexpected")
  attr(cyclic, "srcref") <- cyclic_reference
  cyclic_chat <- structure(list(chat_structured = cyclic), class = "Chat")
  expect_error(
    dsprrr:::batch_chat_branches(cyclic_chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
})

test_that("batch isolation aborts before rows when shared state remains", {
  binding <- ".dsprrr_opaque_batch_calls"
  assign(binding, 0L, envir = globalenv())
  withr::defer(rm(list = binding, envir = globalenv()))

  get_turns <- function(...) list()
  environment(get_turns) <- globalenv()
  chat_structured <- function(...) {
    .dsprrr_opaque_batch_calls <<- .dsprrr_opaque_batch_calls + 1L
    list(answer = "unexpected")
  }
  environment(chat_structured) <- globalenv()
  chat <- structure(
    list(
      get_turns = get_turns,
      chat_structured = chat_structured
    ),
    class = "Chat"
  )
  mod <- module(signature("text -> answer"), type = "predict")

  expect_error(
    run(
      mod,
      text = c("a", "b"),
      .llm = chat,
      .cache = FALSE,
      .progress = FALSE
    ),
    class = "dsprrr_chat_isolation_error"
  )
  expect_equal(get(binding, envir = globalenv()), 0L)
})

test_that("batch isolation scans deeply nested state without truncation", {
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  hidden <- globalenv()
  for (i in seq_len(12L)) {
    hidden <- list(hidden)
  }
  chat <- structure(
    list(
      hidden = hidden,
      chat_structured = function(...) {
        calls$n <- calls$n + 1L
        list(answer = "unexpected")
      }
    ),
    class = "Chat"
  )

  expect_error(
    dsprrr:::batch_chat_branches(chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_equal(calls$n, 0L)
})

test_that("batch isolation rejects opaque external state before rows", {
  calls <- 0L
  chat <- structure(
    list(
      hidden = methods::new("externalptr"),
      chat_structured = function(...) {
        calls <<- calls + 1L
        list(answer = "unexpected")
      }
    ),
    class = "Chat"
  )

  expect_error(
    dsprrr:::batch_chat_branches(chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_equal(calls, 0L)
})

test_that("batch isolation rejects namespace-held opaque closure state", {
  package_state <- asNamespace("dsprrr")$.dsprrr_env
  binding <- ".batch_namespace_probe"
  package_state[[binding]] <- 0L
  withr::defer(rm(list = binding, envir = package_state))

  closure_env <- new.env(parent = asNamespace("dsprrr"))
  chat_structured <- function(...) {
    .dsprrr_env$.batch_namespace_probe <-
      .dsprrr_env$.batch_namespace_probe + 1L
    list(answer = "unexpected")
  }
  environment(chat_structured) <- closure_env
  chat <- structure(list(chat_structured = chat_structured), class = "Chat")

  expect_error(
    dsprrr:::batch_chat_branches(chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_equal(package_state[[binding]], 0L)
})

test_that("batch isolation rejects reflective namespace state access", {
  package_state <- asNamespace("dsprrr")$.dsprrr_env
  binding <- ".batch_reflective_namespace_probe"
  package_state[[binding]] <- 0L
  withr::defer(rm(list = binding, envir = package_state))

  chat <- local({
    structure(
      list(chat_structured = function(...) {
        state <- getFromNamespace(".dsprrr_env", "dsprrr")
        state$.batch_reflective_namespace_probe <-
          state$.batch_reflective_namespace_probe + 1L
        list(answer = "unexpected")
      }),
      class = "Chat"
    )
  })

  expect_error(
    dsprrr:::batch_chat_branches(chat, 2L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_equal(package_state[[binding]], 0L)
})

test_that("batch isolation rejects triple-colon namespace access", {
  chat <- local({
    structure(
      list(chat_structured = function(...) {
        dsprrr:::.dsprrr_env
        list(answer = "unexpected")
      }),
      class = "Chat"
    )
  })

  expect_error(
    dsprrr:::batch_chat_branches(chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
})

test_that("batch isolation rejects parent-resolved global closure state", {
  binding <- ".dsprrr_parent_batch_state"
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  assign(binding, state, envir = globalenv())
  withr::defer(rm(list = binding, envir = globalenv()))

  closure_env <- new.env(parent = globalenv())
  chat_structured <- function(...) {
    .dsprrr_parent_batch_state$calls <-
      .dsprrr_parent_batch_state$calls + 1L
    list(answer = "unexpected")
  }
  environment(chat_structured) <- closure_env
  chat <- structure(list(chat_structured = chat_structured), class = "Chat")

  expect_error(
    dsprrr:::batch_chat_branches(chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_equal(state$calls, 0L)
})

test_that("batch isolation never forces delayed closure state", {
  initializations <- new.env(parent = emptyenv())
  initializations$n <- 0L
  closure_env <- new.env(parent = baseenv())
  closure_env$calls <- 0L
  delayedAssign(
    "state",
    {
      initializations$n <- initializations$n + 1L
      new.env(parent = emptyenv())
    },
    eval.env = environment(),
    assign.env = closure_env
  )
  chat_structured <- function(...) {
    state
    calls <<- calls + 1L
    list(answer = "unexpected")
  }
  environment(chat_structured) <- closure_env
  chat <- structure(list(chat_structured = chat_structured), class = "Chat")

  expect_error(
    dsprrr:::batch_chat_branches(chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_equal(initializations$n, 0L)
  expect_equal(closure_env$calls, 0L)
  expect_true(rlang::env_binding_are_lazy(closure_env, "state"))

  self_initializations <- new.env(parent = emptyenv())
  self_initializations$n <- 0L
  self_env <- new.env(parent = baseenv())
  delayedAssign(
    "self",
    {
      self_initializations$n <- self_initializations$n + 1L
      new.env(parent = emptyenv())
    },
    eval.env = environment(),
    assign.env = self_env
  )
  self_chat_structured <- function(...) list(answer = "unexpected")
  environment(self_chat_structured) <- self_env
  self_chat <- structure(
    list(chat_structured = self_chat_structured),
    class = "Chat"
  )

  expect_error(
    dsprrr:::batch_chat_branches(self_chat, 1L),
    class = "dsprrr_chat_isolation_error"
  )
  expect_equal(self_initializations$n, 0L)
  expect_true(rlang::env_binding_are_lazy(self_env, "self"))
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
  skip_if(nzchar(Sys.getenv("R_COVR")), "mirai workers interfere with covr")
  withr::local_options(dsprrr.parallel_timeout = 5)

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

  # Use mirai explicitly since the mock doesn't implement get_provider(),
  # which ellmer's parallel_chat_structured requires.
  results <- run(
    mod,
    text = c("a", "b", "c"),
    .parallel = TRUE,
    .parallel_method = "mirai",
    .progress = FALSE
  )

  expect_identical(results, rep(list("parallel_result"), 3L))
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

test_that("get_total_cost preserves unknown trace cost", {
  mod <- module(signature("text -> answer"), type = "predict")
  mod$state$traces <- list(list(cost = NA_real_))

  expect_true(is.na(mod$get_total_cost()))
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

# --- mock_batch_chat tests ---

test_that("mock_batch_chat creates chat with user and assistant turns", {
  # Create a minimal mock chat that supports clone and set_turns
  turns_stored <- NULL
  mock_chat <- structure(
    list(
      clone = function(...) {
        cloned <- structure(
          list(
            set_turns = function(turns) {
              turns_stored <<- turns
            },
            get_turns = function(...) turns_stored
          ),
          class = "Chat"
        )
        cloned
      }
    ),
    class = "Chat"
  )

  result <- dsprrr:::mock_batch_chat(
    prompt = "What is 2+2?",
    response = "4",
    chat = mock_chat
  )

  expect_true(inherits(result, "Chat"))
  stored_turns <- result$get_turns()
  expect_length(stored_turns, 2)
})

test_that("mock_batch_chat handles structured response (JSON serializes)", {
  turns_stored <- NULL
  mock_chat <- structure(
    list(
      clone = function(...) {
        cloned <- structure(
          list(
            set_turns = function(turns) {
              turns_stored <<- turns
            },
            get_turns = function(...) turns_stored
          ),
          class = "Chat"
        )
        cloned
      }
    ),
    class = "Chat"
  )

  # Pass a list response (structured output)
  result <- dsprrr:::mock_batch_chat(
    prompt = "Analyze sentiment",
    response = list(sentiment = "positive", confidence = 0.9),
    chat = mock_chat
  )

  expect_true(inherits(result, "Chat"))
})

test_that("mock_batch_chat handles character response directly", {
  turns_stored <- NULL
  mock_chat <- structure(
    list(
      clone = function(...) {
        cloned <- structure(
          list(
            set_turns = function(turns) {
              turns_stored <<- turns
            },
            get_turns = function(...) turns_stored
          ),
          class = "Chat"
        )
        cloned
      }
    ),
    class = "Chat"
  )

  result <- dsprrr:::mock_batch_chat(
    prompt = "Echo this",
    response = "echoed text",
    chat = mock_chat
  )

  expect_true(inherits(result, "Chat"))
})

test_that("print.dsprrr_batch_result counts failed items (dsprrr-8l0)", {
  # Regression: the error predicate read item$error / inherits(item$output,
  # "error"), but create_error_result() stores the message in metadata$error
  # with output = NA, so n_errors was always 0 and failures printed as success.
  result <- structure(
    list(
      list(output = "ok", chat = NULL, metadata = list()),
      list(
        output = NA,
        chat = NULL,
        metadata = list(error = "intentional failure", batch_index = 2L)
      )
    ),
    class = c("dsprrr_batch_result", "list")
  )

  # print.dsprrr_batch_result emits only via cli, so cli::cli_fmt() captures it
  # reliably. Do NOT switch to capture.output(type = "message"): cli writes to
  # stdout, so that pattern silently captures nothing.
  out <- cli::cli_fmt(print(result))
  expect_true(any(grepl("Errors", out, fixed = TRUE)))
  expect_true(any(grepl("1 of 2", out, fixed = TRUE)))
  expect_false(any(grepl(
    "All items completed successfully",
    out,
    fixed = TRUE
  )))
})

test_that("print.dsprrr_batch_result reports success when there are no errors", {
  result <- structure(
    list(
      list(output = "a", chat = NULL, metadata = list()),
      list(output = "b", chat = NULL, metadata = list())
    ),
    class = c("dsprrr_batch_result", "list")
  )
  out <- cli::cli_fmt(print(result))
  expect_true(any(grepl("All items completed successfully", out, fixed = TRUE)))
})
