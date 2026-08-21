test_that("run generic exists and works with modules", {
  # Check if run is a generic function
  expect_true(is.function(run))
  expect_true("run" %in% ls("package:dsprrr"))
})

test_that("run.PredictModule warns about input type mismatches", {
  sig <- signature(
    inputs = list(input("count", "integer")),
    output_type = ellmer::type_object(answer = ellmer::type_string())
  )
  mod <- module(sig, type = "predict")
  mock_chat <- new_test_chat(
    chat_structured = function(...) list(answer = "ok"),
    model = "mock-model",
    last_turn = function(role = NULL) NULL
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
      input(name = "text", type = "string"),
      input(name = "language", type = "string")
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
  mock_llm <- as_test_chat(mock_llm)

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
      input(name = "text", type = "string")
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
      input(name = "text", type = "string")
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
      type = "string",
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
    inputs = list(input(name = "text", type = "string")),
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
  mock_llm <- new_test_chat(chat = function(...) "test")
  mod$chat <- mock_llm
  llm <- dsprrr:::get_default_llm(mod)
  expect_identical(llm, mock_llm)
})

test_that("resolve_module_llm rejects class-tagged list adapters", {
  mod <- module(signature("text -> answer"))
  adapter <- structure(
    list(chat_structured = function(...) "answer"),
    class = "Chat"
  )

  expect_error(
    dsprrr:::resolve_module_llm(mod, .llm = adapter),
    class = "dsprrr_chat_type_error"
  )
})

test_that("runtime Chat parameters are isolated on an independent clone", {
  provider <- ellmer::Provider(
    name = "test",
    model = "test-model",
    base_url = "",
    extra_args = list(existing = TRUE)
  )
  chat <- new_test_chat(
    provider = provider,
    turns = list("prior turn")
  )

  configured <- dsprrr:::apply_chat_params(
    chat,
    list(temperature = 0.2)
  )

  expect_false(identical(configured, chat))
  expect_identical(configured$get_turns(), list("prior turn"))
  expect_identical(chat$get_provider()@extra_args, list(existing = TRUE))
  expect_identical(
    configured$get_provider()@extra_args,
    list(existing = TRUE, temperature = 0.2)
  )
})

test_that("runtime Chat parameters fail closed when cloning fails", {
  chat <- new_test_chat()
  override_test_chat_method(chat, "clone", function(...) stop("cannot clone"))

  expect_error(
    dsprrr:::apply_chat_params(chat, list(temperature = 0.2)),
    class = "dsprrr_chat_clone_error"
  )
})

test_that("runtime Chat parameters fail closed without a provider", {
  chat <- new_test_chat()
  chat$.__enclos_env__$private$provider <- NULL

  expect_error(
    dsprrr:::apply_chat_params(chat, list(temperature = 0.2)),
    class = "dsprrr_chat_params_error"
  )
})

test_that("module config rejects Chat fields", {
  expect_error(
    module(
      signature("text -> result"),
      type = "predict",
      config = list(provider = "openai", model = "gpt-4o-mini")
    ),
    class = "dsprrr_module_config_error"
  )
})

test_that("batch processing works with multiple inputs", {
  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string")
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
  mock_llm <- as_test_chat(mock_llm)

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
      input(name = "text", type = "string")
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
  mock_llm <- as_test_chat(mock_llm)

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
      input(name = "text", type = "string")
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
  mock_llm <- as_test_chat(mock_llm)

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
      input(name = "text", type = "string")
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
  mock_llm <- as_test_chat(mock_llm)

  # Test converting module to function
  skip("as_function not yet implemented for modules")
  # pred_func <- as_function(pred, .llm = mock_llm)
  # result <- pred_func(text = "test")
  # expect_equal(result, "result")
})

test_that("batch processing handles errors gracefully", {
  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string")
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
  mock_llm <- as_test_chat(mock_llm)

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
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, ...) {
      if (grepl("explode", prompt, fixed = TRUE)) {
        stop("provider exploded")
      }
      "ok"
    }
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
  mock_llm <- new_test_chat()
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
    .llm = new_test_chat(),
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

test_that("explicit mirai rejects a custom llm", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string()
  )
  module <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- new_test_chat(chat_structured = function(prompt, ...) "ok")

  expect_error(
    run(
      module,
      text = c("a", "b"),
      .llm = mock_llm,
      .concurrency = concurrency_control(
        backend = "mirai",
        max_active = 2L
      ),
      .progress = FALSE
    ),
    class = "dsprrr_concurrency_chat_error"
  )
})

test_that("run does NOT warn about mirai when ellmer parallel with custom llm", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string()
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- new_test_chat(chat_structured = function(prompt, ...) "ok")

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
        .concurrency = concurrency_control(
          backend = "ellmer",
          max_active = 2L
        ),
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

test_that("explicit ellmer fails closed when unavailable", {
  skip_if(
    exists("parallel_chat_structured", envir = asNamespace("ellmer")),
    "Test only runs when ellmer::parallel_chat_structured is NOT available"
  )

  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string()
  )

  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- new_test_chat(chat_structured = function(prompt, ...) "ok")

  expect_error(
    run(
      mod,
      text = c("a", "b"),
      .llm = mock_llm,
      .concurrency = concurrency_control(
        backend = "ellmer",
        max_active = 2L
      ),
      .progress = FALSE
    ),
    class = "dsprrr_concurrency_backend_unavailable"
  )
})

# --- Parallel execution tests ---

test_that("process_batch_item returns correct format for simple mode", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- new_test_chat(
    chat_structured = function(prompt, ...) "response"
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
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  turns <- list()
  mock_llm <- new_test_chat(
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
    set_turns = function(value) {
      turns <<- value
      invisible(NULL)
    },
    get_turns = function(...) turns
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

test_that("chat usage aggregates every verified assistant turn", {
  prior <- ellmer::UserTurn("prior")
  turns <- list(
    prior,
    ellmer::UserTurn("current"),
    ellmer::AssistantTurn(
      contents = list(ellmer::ContentText("tool request")),
      tokens = c(10L, 1L, 2L),
      cost = 0.11,
      duration = 0.4
    ),
    ellmer::AssistantTurn(
      contents = list(ellmer::ContentText("final")),
      tokens = c(20L, 2L, 3L),
      cost = 0.22,
      duration = 0.6
    )
  )
  chat <- new_test_chat(turns = turns)

  usage <- dsprrr:::chat_usage_metadata(chat, turns_before = list(prior))

  expect_identical(usage$provider_calls, 2L)
  expect_identical(usage$input_tokens, 30L)
  expect_identical(usage$output_tokens, 3L)
  expect_identical(usage$cached_input_tokens, 5L)
  expect_identical(usage$total_tokens, 33L)
  expect_equal(usage$cost, 0.33)
  expect_equal(usage$duration_s, 1)

  unknown <- dsprrr:::chat_usage_metadata(chat, turns_before = list("wrong"))
  expect_true(is.na(unknown$provider_calls))
  expect_true(is.na(unknown$total_tokens))
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
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- new_test_chat(
    chat_structured = function(prompt, ...) "response"
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
  expect_identical(
    vapply(results, identity, character(1)),
    rep("response", 3L)
  )
})

test_that("batch branches preserve exact ellmer history independently", {
  provider <- ellmer::Provider(
    name = "test",
    model = "batch-branch-test",
    base_url = ""
  )
  chat <- utils::getFromNamespace("Chat", "ellmer")$new(provider = provider)
  starting_history <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("prior"))),
    ellmer::AssistantTurn(contents = list(ellmer::ContentText("context")))
  )
  chat$set_turns(starting_history)

  expect_identical(dsprrr:::batch_chat_branches(chat, 0L), list())

  branches <- dsprrr:::batch_chat_branches(chat, 2L)
  ids <- vapply(c(list(chat), branches), rlang::obj_address, character(1))
  expect_length(unique(ids), 3L)
  expect_identical(branches[[1]]$get_turns(), starting_history)
  expect_identical(branches[[2]]$get_turns(), starting_history)

  branch_history <- c(
    starting_history,
    list(ellmer::UserTurn(contents = list(ellmer::ContentText("branch one"))))
  )
  branches[[1]]$set_turns(branch_history)

  expect_identical(chat$get_turns(), starting_history)
  expect_identical(branches[[1]]$get_turns(), branch_history)
  expect_identical(branches[[2]]$get_turns(), starting_history)
})

test_that("structured batches synthesize history when a Chat records no delta", {
  baseline <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("prior"))),
    ellmer::AssistantTurn(contents = list(ellmer::ContentText("context")))
  )
  chat <- new_test_chat(
    turns = baseline,
    chat_structured = function(...) list(answer = "ok")
  )
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
  expect_length(result$chat$get_turns(), 4L)
  expect_identical(result$chat$get_turns()[1:2], baseline)
  expect_true(endsWith(
    result$chat$get_turns()[[3]]@contents[[1]]@text,
    "new"
  ))
})

test_that("public batch run rejects class-tagged lists before work", {
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

  expect_error(
    run(
      mod,
      text = c("a", "b"),
      .llm = chat,
      .cache = FALSE,
      .progress = FALSE
    ),
    class = "dsprrr_chat_type_error"
  )
  expect_identical(calls, 0L)
})

test_that("public batch run reports typed clone failures before work", {
  calls <- 0L
  chat <- new_test_chat(
    chat_structured = function(...) {
      calls <<- calls + 1L
      list(answer = "unexpected")
    }
  )
  override_test_chat_method(
    chat,
    "clone",
    function(...) stop("cannot clone")
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
    class = "dsprrr_chat_clone_error"
  )
  expect_identical(calls, 0L)
})
test_that("run_batch_sequential handles errors per item", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- new_test_chat(
    chat_structured = function(prompt, ...) {
      if (grepl("fail", prompt, fixed = TRUE)) {
        stop("intentional failure")
      }
      "ok"
    }
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

  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  # Configure module to use a testable Chat
  mod$chat <- new_test_chat(
    chat_structured = function(prompt, ...) "parallel_result"
  )

  # Use mirai explicitly since the mock doesn't implement get_provider(),
  # which ellmer's parallel_chat_structured requires.
  results <- run(
    mod,
    text = c("a", "b", "c"),
    .concurrency = concurrency_control(
      backend = "mirai",
      max_active = 3L,
      total_timeout = 5
    ),
    .progress = FALSE
  )

  expect_identical(results, rep(list("parallel_result"), 3L))
})


# -- .show_prompt tests --

test_that("run with .show_prompt=TRUE shows prompt preview", {
  sig <- Signature(
    inputs = list(input(name = "q", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- module(signature = sig, type = "predict", template = "Question: {q}")

  mock_llm <- new_test_chat(
    chat_structured = function(prompt, ...) "answer"
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
    inputs = list(input(name = "q", type = "string")),
    output_type = ellmer::type_string()
  )

  mod <- module(signature = sig, type = "predict", template = "{q}")

  mock_llm <- new_test_chat(
    chat_structured = function(prompt, ...) "answer"
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
    inputs = list(input(name = "q", type = "string")),
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
    inputs = list(input(name = "q", type = "string")),
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
    inputs = list(input(name = "text", type = "string")),
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
  mock_llm <- as_test_chat(mock_llm)

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
    inputs = list(input(name = "text", type = "string")),
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
  mock_llm <- as_test_chat(mock_llm)

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
  mock_chat <- new_test_chat()

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
  mock_chat <- new_test_chat()

  # Pass a list response (structured output)
  result <- dsprrr:::mock_batch_chat(
    prompt = "Analyze sentiment",
    response = list(sentiment = "positive", confidence = 0.9),
    chat = mock_chat
  )

  expect_true(inherits(result, "Chat"))
})

test_that("mock_batch_chat handles character response directly", {
  mock_chat <- new_test_chat()

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


test_that("run_dataset accepts omitted and provided optional inputs", {
  seen <- list()
  program <- module_fn(
    signature(
      inputs = list(
        input("question"),
        input("context_note", type = ellmer::type_string(required = FALSE))
      ),
      output_type = ellmer::type_object(answer = ellmer::type_string())
    ),
    function(question, context_note = NULL, ...) {
      seen[[length(seen) + 1L]] <<- context_note %||% "<missing>"
      list(answer = paste(question, context_note %||% "none", sep = ":"))
    }
  )

  omitted <- run_dataset(
    program,
    data.frame(question = c("a", "b")),
    .progress = FALSE
  )
  provided <- run_dataset(
    program,
    data.frame(
      question = c("c", "d"),
      context_note = c("first", "second")
    ),
    .progress = FALSE
  )

  expect_identical(
    unlist(omitted$result, use.names = FALSE),
    c("a:none", "b:none")
  )
  expect_identical(
    unlist(provided$result, use.names = FALSE),
    c("c:first", "d:second")
  )
  expect_identical(seen, list("<missing>", "<missing>", "first", "second"))
})
