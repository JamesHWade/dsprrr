# Tests for dsp() and as_module() Chat-centric API

test_that("dsp generic exists", {
  expect_true(is.function(dsp))
  expect_true("dsp" %in% ls("package:dsprrr"))
})

test_that("as_module generic exists", {
  expect_true(is.function(as_module))
  expect_true("as_module" %in% ls("package:dsprrr"))
})

test_that("last_trace function exists", {
  expect_true(is.function(last_trace))
  expect_true("last_trace" %in% ls("package:dsprrr"))
})

# -- dsp.Chat tests --

test_that("dsp.Chat parses signature string", {
  mock_chat <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(answer = "42")
      }
    ),
    class = "Chat"
  )

  result <- dsp(mock_chat, "question -> answer", question = "What is 6 * 7?")
  expect_equal(result, "42")
})

test_that("dsp.Chat accepts Signature object", {
  mock_chat <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(answer = "test response")
      }
    ),
    class = "Chat"
  )

  sig <- signature("question -> answer")
  result <- dsp(mock_chat, sig, question = "Test?")
  expect_equal(result, "test response")
})

test_that("dsp.Chat validates inputs against signature", {
  mock_chat <- structure(
    list(chat_structured = function(...) list(answer = "x")),
    class = "Chat"
  )

  expect_error(
    dsp(mock_chat, "question -> answer"),
    "Missing required inputs"
  )
})

test_that("dsp.Chat warns about extra inputs", {
  mock_chat <- structure(
    list(chat_structured = function(...) list(answer = "x")),
    class = "Chat"
  )

  expect_warning(
    dsp(mock_chat, "question -> answer", question = "test", extra = "ignored"),
    "unknown input"
  )
})

test_that("dsp.Chat stores trace for last_trace()", {
  mock_chat <- structure(
    list(chat_structured = function(...) list(answer = "traced")),
    class = "Chat"
  )

  dsp(mock_chat, "q -> answer", q = "test")
  trace <- last_trace()

  expect_type(trace, "list")
  expect_true("signature" %in% names(trace))
  expect_true("inputs" %in% names(trace))
  expect_true("prompt" %in% names(trace))
  expect_true("output" %in% names(trace))
  expect_true("timestamp" %in% names(trace))
})

test_that("dsp.Chat includes .instructions", {
  prompt_received <- NULL
  mock_chat <- structure(
    list(
      chat_structured = function(prompt, ...) {
        prompt_received <<- prompt
        list(answer = "x")
      }
    ),
    class = "Chat"
  )

  dsp(mock_chat, "q -> answer", q = "test", .instructions = "Be very brief")

  expect_true(grepl("Be very brief", prompt_received))
})

test_that("dsp.Chat simplifies single-field output", {
  mock_chat <- structure(
    list(chat_structured = function(...) list(answer = "simplified")),
    class = "Chat"
  )

  result <- dsp(mock_chat, "q -> answer", q = "test")
  expect_equal(result, "simplified")
  expect_type(result, "character")
})

test_that("dsp.Chat returns full object for multi-field output", {
  mock_chat <- structure(
    list(
      chat_structured = function(...) {
        list(answer = "A", explanation = "B")
      }
    ),
    class = "Chat"
  )

  result <- dsp(mock_chat, "q -> answer, explanation", q = "test")
  expect_type(result, "list")
  expect_equal(result$answer, "A")
  expect_equal(result$explanation, "B")
})

# -- dsp.character tests --

test_that("dsp.character uses default Chat", {
  # Set up a mock default Chat
  mock_chat <- structure(
    list(chat_structured = function(...) list(answer = "from default")),
    class = "Chat"
  )
  old_opt <- options(dsprrr.default_chat = mock_chat)
  on.exit(options(old_opt))

  result <- dsp("q -> answer", q = "test")
  expect_equal(result, "from default")
})

test_that("dsp errors with helpful message when no default Chat", {
  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt))
  clear_default_chat()

  # Temporarily unset API keys
  old_env <- Sys.getenv(c(
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GOOGLE_API_KEY"
  ))
  on.exit(do.call(Sys.setenv, as.list(old_env)), add = TRUE)
  Sys.unsetenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))

  expect_error(
    dsp("q -> answer", q = "test"),
    "No default Chat available"
  )
})

# -- as_module tests --

test_that("as_module.Chat creates Module with Chat attached", {
  mock_chat <- structure(
    list(chat_structured = function(...) list(answer = "x")),
    class = "Chat"
  )

  mod <- as_module(mock_chat, "q -> answer")

  expect_s3_class(mod, "Module")
  expect_s3_class(mod, "PredictModule")
  expect_identical(mod$chat, mock_chat)
})

test_that("as_module.Chat accepts signature string", {
  mock_chat <- structure(list(), class = "Chat")
  mod <- as_module(mock_chat, "text -> sentiment")

  expect_s3_class(mod$signature, "dsprrr::Signature")
  expect_equal(length(mod$signature@inputs), 1)
})

test_that("as_module.Chat accepts Signature object", {
  mock_chat <- structure(list(), class = "Chat")
  sig <- signature("context, question -> answer")
  mod <- as_module(mock_chat, sig)

  expect_identical(mod$signature, sig)
})

test_that("as_module.Chat passes extra args to module()", {
  mock_chat <- structure(list(), class = "Chat")

  mod <- as_module(
    mock_chat,
    "text -> result",
    template = "Custom: {text}"
  )

  expect_equal(mod$template, "Custom: {text}")
})

test_that("as_module.character uses default Chat", {
  mock_chat <- structure(list(), class = "Chat")
  old_opt <- options(dsprrr.default_chat = mock_chat)
  on.exit(options(old_opt))

  mod <- as_module("q -> answer")

  expect_s3_class(mod, "Module")
  expect_identical(mod$chat, mock_chat)
})

# -- Module with stored Chat --

test_that("Module uses stored Chat when .llm not provided", {
  call_count <- 0
  mock_chat <- structure(
    list(
      chat_structured = function(...) {
        call_count <<- call_count + 1
        list(answer = paste0("response_", call_count))
      }
    ),
    class = "Chat"
  )

  mod <- signature("q -> answer") |>
    module(type = "predict", chat = mock_chat)

  # Run without providing .llm
  result <- run(mod, q = "test")
  expect_equal(result$answer, "response_1")

  # Run again - should still use stored Chat
  result2 <- run(mod, q = "test2")
  expect_equal(result2$answer, "response_2")
})

test_that("mod$predict() method works", {
  mock_chat <- structure(
    list(chat_structured = function(...) list(sentiment = "happy")),
    class = "Chat"
  )

  mod <- as_module(mock_chat, "text -> sentiment")
  result <- mod$predict(text = "Great day!")

  expect_equal(result$sentiment, "happy")
})

test_that("mod$predict() handles batch inputs", {
  call_count <- 0
  mock_chat <- structure(
    list(
      chat_structured = function(...) {
        call_count <<- call_count + 1
        list(sentiment = paste0("result_", call_count))
      }
    ),
    class = "Chat"
  )

  mod <- as_module(mock_chat, "text -> sentiment")
  results <- mod$predict(text = c("A", "B", "C"))

  expect_length(results, 3)
  expect_equal(results[[1]]$sentiment, "result_1")
  expect_equal(results[[2]]$sentiment, "result_2")
  expect_equal(results[[3]]$sentiment, "result_3")
})

test_that("Module prefers .llm over stored Chat", {
  stored_chat <- structure(
    list(chat_structured = function(...) list(answer = "from stored")),
    class = "Chat"
  )

  passed_chat <- structure(
    list(chat_structured = function(...) list(answer = "from passed")),
    class = "Chat"
  )

  mod <- signature("q -> answer") |>
    module(type = "predict", chat = stored_chat)

  result <- run(mod, q = "test", .llm = passed_chat)
  expect_equal(result$answer, "from passed")
})

# -- predict.Module tests --

test_that("predict.Module works with new_data", {
  mock_chat <- structure(
    list(
      chat_structured = function(prompt, ...) {
        # Extract text from prompt and return processed
        list(result = "processed")
      }
    ),
    class = "Chat"
  )

  mod <- signature("text -> result") |>
    module(type = "predict", chat = mock_chat)

  new_data <- data.frame(text = c("A", "B"), stringsAsFactors = FALSE)

  result <- predict(mod, new_data = new_data)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_true("result" %in% names(result))
})

test_that("predict.Module uses stored Chat", {
  mock_chat <- structure(
    list(
      chat_structured = function(...) list(result = "from stored")
    ),
    class = "Chat"
  )

  mod <- signature("text -> result") |>
    module(type = "predict", chat = mock_chat)

  new_data <- data.frame(text = "test", stringsAsFactors = FALSE)

  # Should work without providing any LLM
  result <- predict(mod, new_data = new_data)
  expect_s3_class(result, "tbl_df")
})

test_that("predict.Module errors clearly when no Chat available", {
  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt))
  clear_default_chat()

  old_env <- Sys.getenv(c(
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GOOGLE_API_KEY"
  ))

  on.exit(do.call(Sys.setenv, as.list(old_env)), add = TRUE)
  Sys.unsetenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))

  mod <- signature("text -> result") |>
    module(type = "predict")

  new_data <- data.frame(text = "test", stringsAsFactors = FALSE)

  expect_error(
    predict(mod, new_data = new_data),
    "No default Chat available|Chat"
  )
})

# -- wrap_llm_error tests --

test_that("wrap_llm_error detects rate limit errors", {
  e <- simpleError("Rate limit exceeded: 429 Too Many Requests")
  expect_error(
    dsprrr:::wrap_llm_error(e, "gpt-4o", "OpenAI", "test prompt"),
    "Rate limit exceeded"
  )
})

test_that("wrap_llm_error detects authentication errors", {
  e <- simpleError("Invalid API key: 401 Unauthorized")
  expect_error(
    dsprrr:::wrap_llm_error(e, "gpt-4o", "OpenAI", "test prompt"),
    "Authentication failed"
  )
})

test_that("wrap_llm_error detects timeout errors", {
  e <- simpleError("Connection timed out after 30 seconds")
  expect_error(
    dsprrr:::wrap_llm_error(e, "gpt-4o", "OpenAI", "test prompt"),
    "Request timed out"
  )
})

test_that("wrap_llm_error detects context length errors", {
  e <- simpleError("This model's maximum context length is 8192 tokens")
  long_prompt <- paste(rep("x", 10000), collapse = "")
  expect_error(
    dsprrr:::wrap_llm_error(e, "gpt-4o", "OpenAI", long_prompt),
    "Prompt too long"
  )
})

test_that("wrap_llm_error detects JSON parsing errors", {
  e <- simpleError("Failed to parse JSON response from API")
  expect_error(
    dsprrr:::wrap_llm_error(e, "gpt-4o", "OpenAI", "test prompt"),
    "Response parsing failed"
  )
})

test_that("wrap_llm_error detects content filter errors", {
  e <- simpleError("Content blocked by safety filter")
  expect_error(
    dsprrr:::wrap_llm_error(e, "gpt-4o", "OpenAI", "test prompt"),
    "Content was blocked"
  )
})

test_that("wrap_llm_error includes model and provider info", {
  e <- simpleError("Some error")
  expect_error(
    dsprrr:::wrap_llm_error(e, "gpt-4o-mini", "OpenAI", "test"),
    "gpt-4o-mini.*OpenAI"
  )
})

# -- Input validation suggestion tests --

test_that("dsp suggests corrections for typos in missing input names", {
  mock_chat <- structure(
    list(chat_structured = function(...) list(answer = "x")),
    class = "Chat"
  )

  # Typo: "questoin" instead of "question"
  expect_error(
    dsp(mock_chat, "question -> answer", questoin = "test"),
    "Did you mean.*question"
  )
})

test_that("dsp suggests corrections for extra inputs with typos", {
  mock_chat <- structure(
    list(chat_structured = function(...) list(answer = "x")),
    class = "Chat"
  )

  expect_warning(
    dsp(mock_chat, "question -> answer", question = "test", questoin = "extra"),
    "Did you mean.*question"
  )
})

# -- dsp() to inspect_history() integration test --

test_that("dsp populates global prompt history", {
  clear_prompt_history()
  on.exit(clear_prompt_history())

  mock_chat <- structure(
    list(chat_structured = function(...) list(answer = "42")),
    class = "Chat"
  )

  dsp(mock_chat, "q -> answer", q = "What is 6*7?")

  history <- inspect_history(n = 1)
  expect_equal(nrow(history), 1)
  expect_equal(history$source[1], "dsp()")
})
