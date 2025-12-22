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

  old_env <- Sys.getenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))
  on.exit(do.call(Sys.setenv, as.list(old_env)), add = TRUE)
  Sys.unsetenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))

  result <- get_default_chat(create = FALSE)
  expect_null(result)
})

test_that("get_default_chat errors when create=TRUE and no default", {
  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt))
  clear_default_chat()

  old_env <- Sys.getenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))
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
  skip_if(Sys.getenv("OPENAI_API_KEY") == "",
          "OPENAI_API_KEY not set")

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
  skip_if(Sys.getenv("OPENAI_API_KEY") == "",
          "OPENAI_API_KEY not set for auto-detection")

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
  skip_if(Sys.getenv("OPENAI_API_KEY") == "",
          "OPENAI_API_KEY not set for auto-detection")

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
  skip_if(Sys.getenv("OPENAI_API_KEY") == "",
          "OPENAI_API_KEY not set")

  old_opt <- options(dsprrr.default_chat = NULL)
  on.exit(options(old_opt))
  clear_default_chat()

  chat <- dsprrr:::auto_detect_chat()

  expect_s3_class(chat, "Chat")
})

test_that("auto_detect_chat returns NULL when no API keys", {
  old_env <- Sys.getenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))
  on.exit(do.call(Sys.setenv, as.list(old_env)))
  Sys.unsetenv(c("OPENAI_API_KEY", "ANTHROPIC_API_KEY", "GOOGLE_API_KEY"))

  result <- dsprrr:::auto_detect_chat()

  expect_null(result)
})
