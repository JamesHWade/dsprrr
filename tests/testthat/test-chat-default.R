# Tests for default Chat management

test_that("get_default_chat returns Chat from options", {
  mock_chat <- structure(list(), class = "Chat")
  old_opt <- options(dsprrr.default_chat = mock_chat)
  on.exit(options(old_opt))

  result <- get_default_chat()
  expect_identical(result, mock_chat)
})

test_that("get_default_chat validates options Chat", {
  old_opt <- options(dsprrr.default_chat = "not a Chat")
  on.exit(options(old_opt))

  expect_error(
    get_default_chat(),
    "must be an ellmer Chat object"
  )
})

test_that("get_default_chat returns NULL when create=FALSE and no default", {
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

  result <- get_default_chat(create = FALSE)
  expect_null(result)
})

test_that("get_default_chat errors when create=TRUE and no default", {
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

  expect_error(
    get_default_chat(create = TRUE),
    "No default Chat available"
  )
})

test_that("get_default_chat caches auto-detected Chat", {
  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt))
  clear_default_chat()

  # Ensure OPENAI_API_KEY is set for auto-detection
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", "OPENAI_API_KEY not set")

  chat1 <- get_default_chat()
  chat2 <- get_default_chat()

  # Should return the same cached object
  expect_identical(chat1, chat2)

  # Clean up
  clear_default_chat()
})

test_that("set_default_chat sets Chat in options", {
  mock_chat <- structure(list(), class = "Chat")

  old <- set_default_chat(mock_chat)
  on.exit(set_default_chat(old))

  result <- getOption("dsprrr.default_chat")
  expect_identical(result, mock_chat)
})

test_that("set_default_chat validates Chat object", {
  expect_error(
    set_default_chat("not a Chat"),
    "must be an ellmer Chat object or NULL"
  )
})

test_that("set_default_chat returns previous value", {
  mock_chat1 <- structure(list(id = 1), class = "Chat")
  mock_chat2 <- structure(list(id = 2), class = "Chat")

  old <- set_default_chat(mock_chat1)
  on.exit(set_default_chat(old))

  prev <- set_default_chat(mock_chat2)
  expect_identical(prev, mock_chat1)
})

test_that("set_default_chat overrides any cached value", {
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "",
    "OPENAI_API_KEY not set for auto-detection"
  )

  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt))

  # Get an auto-detected chat (will be cached)
  clear_default_chat()
  auto_chat <- get_default_chat()

  # Set a new default
  mock_chat <- structure(list(id = "mock"), class = "Chat")
  set_default_chat(mock_chat)

  # Should return the explicitly set chat, not the cached one
  result <- get_default_chat()
  expect_identical(result, mock_chat)
})

test_that("set_default_chat(NULL) clears default", {
  mock_chat <- structure(list(), class = "Chat")
  old <- set_default_chat(mock_chat)
  on.exit(set_default_chat(old))

  set_default_chat(NULL)

  expect_null(getOption("dsprrr.default_chat"))
})

test_that("clear_default_chat allows re-detection", {
  skip_if(
    Sys.getenv("OPENAI_API_KEY") == "",
    "OPENAI_API_KEY not set for auto-detection"
  )

  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt))
  clear_default_chat()

  # Get initial auto-detected chat
  chat1 <- get_default_chat()
  expect_s3_class(chat1, "Chat")

  # Clear and get again - should get new instance
  clear_default_chat()
  chat2 <- get_default_chat()
  expect_s3_class(chat2, "Chat")
})

test_that("clear_default_chat returns NULL invisibly", {
  result <- clear_default_chat()
  expect_null(result)
})

# -- Auto-detection tests --

test_that("auto_detect_chat returns chat_openai when OPENAI_API_KEY set", {
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", "OPENAI_API_KEY not set")

  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt))
  clear_default_chat()

  chat <- dsprrr:::auto_detect_chat()

  expect_s3_class(chat, "Chat")
})

test_that("auto_detect_chat returns NULL when no API keys", {
  old_env <- Sys.getenv(c(
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GOOGLE_API_KEY"
  ))
  on.exit(do.call(Sys.setenv, as.list(old_env)))
  Sys.unsetenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))

  result <- dsprrr:::auto_detect_chat()

  expect_null(result)
})

# -- Auto-detection messaging tests --

test_that("emit_auto_detection_message respects quiet option", {
  old_opt <- options(dsprrr.quiet = TRUE)
  on.exit(options(old_opt))

  mock_chat <- structure(
    list(get_model = function() "test-model"),
    class = "Chat"
  )

  # Should not emit any message when quiet
  expect_silent(
    dsprrr:::emit_auto_detection_message("OpenAI", mock_chat)
  )
})

test_that("emit_auto_detection_message shows once per session", {
  old_opt <- options(dsprrr.quiet = FALSE)
  on.exit(options(old_opt))
  .dsprrr_env$auto_detect_message_shown <- FALSE

  mock_chat <- structure(
    list(get_model = function() "test-model"),
    class = "Chat"
  )

  # First call should emit message
  expect_message(
    dsprrr:::emit_auto_detection_message("OpenAI", mock_chat),
    "Using OpenAI"
  )

  # Second call should be silent (already shown)
  expect_silent(
    dsprrr:::emit_auto_detection_message("OpenAI", mock_chat)
  )

  # Reset for other tests
  .dsprrr_env$auto_detect_message_shown <- FALSE
})

test_that("clear_default_chat resets auto_detect_message_shown", {
  .dsprrr_env$auto_detect_message_shown <- TRUE
  clear_default_chat()
  expect_false(isTRUE(.dsprrr_env$auto_detect_message_shown))
})

# -- dsp_configure tests --

test_that("dsp_configure validates provider", {
  expect_error(
    dsp_configure(provider = "invalid"),
    "Unknown provider"
  )
})

test_that("dsp_configure accepts valid providers", {
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", "OPENAI_API_KEY not set")

  old_opt <- options(dsprrr.default_chat = NULL, dsprrr.quiet = TRUE)
  on.exit(options(old_opt))
  clear_default_chat()

  chat <- dsp_configure(provider = "openai")
  expect_s3_class(chat, "Chat")

  # Clean up
  clear_default_chat()
})

test_that("dsp_configure sets default chat", {
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", "OPENAI_API_KEY not set")

  old_opt <- options(dsprrr.default_chat = NULL, dsprrr.quiet = TRUE)
  on.exit(options(old_opt))
  clear_default_chat()

  dsp_configure(provider = "openai")

  # Should be able to get it back
  chat <- get_default_chat()
  expect_s3_class(chat, "Chat")

  # Clean up
  clear_default_chat()
})

test_that("dsp_configure stores configuration metadata", {
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", "OPENAI_API_KEY not set")

  old_opt <- options(dsprrr.default_chat = NULL, dsprrr.quiet = TRUE)
  on.exit(options(old_opt))
  clear_default_chat()

  dsp_configure(provider = "openai", model = "gpt-4o-mini")

  config <- .dsprrr_env$config
  expect_equal(config$provider, "openai")
  expect_equal(config$model, "gpt-4o-mini")
  expect_true(inherits(config$configured_at, "POSIXct"))

  # Clean up
  clear_default_chat()
  .dsprrr_env$config <- NULL
})

test_that("dsp_configure errors without provider and no API keys", {
  old_env <- Sys.getenv(c(
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GOOGLE_API_KEY"
  ))
  on.exit(do.call(Sys.setenv, as.list(old_env)))
  Sys.unsetenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))

  old_opt <- options(dsprrr.default_chat = NULL, dsprrr.quiet = TRUE)
  on.exit(options(old_opt), add = TRUE)
  clear_default_chat()

  expect_error(
    dsp_configure(),
    "Could not auto-detect provider"
  )
})

test_that("dsp_configure shows confirmation message", {
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", "OPENAI_API_KEY not set")

  old_opt <- options(dsprrr.default_chat = NULL, dsprrr.quiet = FALSE)
  on.exit(options(old_opt))
  clear_default_chat()
  .dsprrr_env$auto_detect_message_shown <- TRUE # Suppress auto-detect msg

  expect_message(
    dsp_configure(provider = "openai"),
    "Configured dsprrr"
  )

  # Clean up
  clear_default_chat()
})

# -- detect_provider_name tests --

test_that("detect_provider_name identifies providers", {
  openai_chat <- structure(list(), class = c("ChatOpenAI", "Chat"))
  anthropic_chat <- structure(list(), class = c("ChatClaude", "Chat"))
  google_chat <- structure(list(), class = c("ChatGemini", "Chat"))
  unknown_chat <- structure(list(), class = c("ChatUnknown", "Chat"))

  expect_equal(dsprrr:::detect_provider_name(openai_chat), "OpenAI")
  expect_equal(dsprrr:::detect_provider_name(anthropic_chat), "Anthropic")
  expect_equal(dsprrr:::detect_provider_name(google_chat), "Google")
  expect_equal(dsprrr:::detect_provider_name(unknown_chat), "Unknown")
})

# -- dsprrr_sitrep tests --

test_that("dsprrr_sitrep returns invisible list", {
  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt))
  clear_default_chat()

  # Clear API keys temporarily
  old_env <- Sys.getenv(c(
    "OPENAI_API_KEY",
    "ANTHROPIC_API_KEY",
    "GOOGLE_API_KEY"
  ))
  on.exit(do.call(Sys.setenv, as.list(old_env)), add = TRUE)

  result <- dsprrr_sitrep()

  expect_type(result, "list")
  expect_true("has_default_chat" %in% names(result))
  expect_true("provider" %in% names(result))
  expect_true("api_keys" %in% names(result))
  expect_true("n_calls" %in% names(result))
})

test_that("dsprrr_sitrep shows configured provider", {
  skip_if(Sys.getenv("OPENAI_API_KEY") == "", "OPENAI_API_KEY not set")

  old_opt <- options(dsprrr.default_chat = NULL, dsprrr.quiet = TRUE)
  on.exit(options(old_opt))
  clear_default_chat()

  dsp_configure(provider = "openai")

  result <- dsprrr_sitrep()

  expect_true(result$has_default_chat)
  expect_equal(result$provider, "OpenAI")

  # Clean up
  clear_default_chat()
})

test_that("dsprrr_sitrep shows API key status", {
  result <- dsprrr_sitrep()

  expect_type(result$api_keys, "list")
  expect_true("OPENAI_API_KEY" %in% names(result$api_keys))
  expect_true("ANTHROPIC_API_KEY" %in% names(result$api_keys))
  expect_true("GOOGLE_API_KEY" %in% names(result$api_keys))
})

# -- session_cost tests --

test_that("session_cost returns empty summary when no calls made", {
  clear_prompt_history()
  on.exit(clear_prompt_history())

  result <- session_cost()

  expect_s3_class(result, "dsprrr_session_cost")
  expect_equal(result$n_calls, 0L)
  expect_equal(result$tokens_in, 0L)
  expect_equal(result$tokens_out, 0L)
  expect_equal(result$total_tokens, 0L)
  expect_equal(result$cost, 0)
  expect_s3_class(result$by_model, "tbl_df")
  expect_equal(nrow(result$by_model), 0)
})

test_that("session_cost aggregates from prompt history", {
  clear_prompt_history()
  on.exit(clear_prompt_history())

  # Simulate some prompt history entries
  .dsprrr_env <- dsprrr:::.dsprrr_env
  .dsprrr_env$prompt_history <- list(
    list(
      model = "gpt-4o-mini",
      tokens_in = 100L,
      tokens_out = 50L,
      cost = 0.001
    ),
    list(
      model = "gpt-4o-mini",
      tokens_in = 200L,
      tokens_out = 100L,
      cost = 0.002
    ),
    list(
      model = "claude-3-haiku",
      tokens_in = 150L,
      tokens_out = 75L,
      cost = 0.0015
    )
  )

  result <- session_cost()

  expect_equal(result$n_calls, 3)
  expect_equal(result$tokens_in, 450L)
  expect_equal(result$tokens_out, 225L)
  expect_equal(result$total_tokens, 675L)
  expect_equal(result$cost, 0.0045)

  # Check by_model breakdown
  expect_equal(nrow(result$by_model), 2)
  expect_true("gpt-4o-mini" %in% result$by_model$model)
  expect_true("claude-3-haiku" %in% result$by_model$model)
})

test_that("session_cost print method works", {
  clear_prompt_history()
  on.exit(clear_prompt_history())

  # Empty case - capture cli messages
  result <- session_cost()
  output <- capture.output(print(result), type = "message")
  expect_true(any(grepl("No LLM calls recorded", output, fixed = TRUE)))

  # With data
  .dsprrr_env <- dsprrr:::.dsprrr_env
  .dsprrr_env$prompt_history <- list(
    list(model = "gpt-4o", tokens_in = 100L, tokens_out = 50L, cost = 0.01)
  )

  result <- session_cost()
  output <- capture.output(print(result), type = "message")
  expect_true(any(grepl("LLM calls", output, fixed = TRUE)))
  expect_true(any(grepl("Tokens", output, fixed = TRUE)))
})
