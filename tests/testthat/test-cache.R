# Tests for LLM response caching
# Note: local_reset_cache() helper is defined in helper-cache.R

test_that("configure_cache sets default values", {
  local_reset_cache()

  configure_cache()

  config <- dsprrr:::get_cache_config()
  expect_true(config$enable)
  expect_true(config$enable_memory)
  expect_true(config$enable_disk)
  expect_equal(config$memory_max_entries, 1000L)
})

test_that("configure_cache can disable caching", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  config <- dsprrr:::get_cache_config()
  expect_false(config$enable)
})

test_that("configure_cache can disable disk cache", {
  local_reset_cache()

  configure_cache(enable_disk = FALSE)

  config <- dsprrr:::get_cache_config()
  expect_true(config$enable)
  expect_true(config$enable_memory)
  expect_false(config$enable_disk)
})

test_that("configure_cache returns previous config", {
  local_reset_cache()

  configure_cache(memory_max_entries = 500L)
  old <- configure_cache(memory_max_entries = 1000L)

  expect_equal(old$memory_max_entries, 500L)
})

test_that("cache_key produces consistent keys", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")

  expect_equal(key1, key2)
  expect_type(key1, "character")
  expect_equal(nchar(key1), 64)
})

test_that("cache_key differs with different prompts", {
  key1 <- dsprrr:::cache_key("prompt1", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt2", "gpt-4o", 0.7, "string")

  expect_false(key1 == key2)
})

test_that("cache_key differs with different models", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o-mini", 0.7, "string")

  expect_false(key1 == key2)
})

test_that("cache_key differs with different temperatures", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.5, "string")

  expect_false(key1 == key2)
})

test_that("cache_key differs with rollout_id", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string", rollout_id = 1)
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string", rollout_id = 2)

  expect_false(key1 == key2)
})

test_that("cache_key without rollout_id differs from with rollout_id", {
  key1 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string")
  key2 <- dsprrr:::cache_key("prompt", "gpt-4o", 0.7, "string", rollout_id = 1)

  expect_false(key1 == key2)
})

test_that("get_cache returns NULL when disabled", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  cache <- dsprrr:::get_cache()
  expect_null(cache)
})

test_that("get_cache creates memory-only cache", {
  local_reset_cache()

  # Use temp directory for disk to avoid pollution
  configure_cache(
    enable_memory = TRUE,
    enable_disk = FALSE
  )

  cache <- dsprrr:::get_cache()
  expect_false(is.null(cache))
})

test_that("clear_cache resets cache", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  cache <- dsprrr:::get_cache()
  cache$set("test_key", "test_value")

  clear_cache()

  # After clear, key should be missing
  result <- cache$get("test_key")
  expect_true(cachem::is.key_missing(result))
})

test_that("cache_stats returns statistics structure", {
  local_reset_cache()

  stats <- cache_stats()

  expect_s3_class(stats, "dsprrr_cache_stats")
  expect_true("enabled" %in% names(stats))
  expect_true("hits" %in% names(stats))
  expect_true("misses" %in% names(stats))
  expect_true("hit_rate" %in% names(stats))
})

test_that("cache_stats hit_rate is correct", {
  local_reset_cache()

  # Simulate some hits and misses
  dsprrr:::increment_cache_stats("hits")
  dsprrr:::increment_cache_stats("hits")
  dsprrr:::increment_cache_stats("misses")

  stats <- cache_stats()
  expect_equal(stats$hits, 2L)
  expect_equal(stats$misses, 1L)
  expect_equal(stats$hit_rate, 2 / 3)
})

test_that("cache_enabled returns correct state", {
  local_reset_cache()

  configure_cache(enable = TRUE)
  expect_true(dsprrr:::cache_enabled())

  configure_cache(enable = FALSE)
  expect_false(dsprrr:::cache_enabled())
})

test_that("print.dsprrr_cache_stats works", {
  local_reset_cache()

  stats <- cache_stats()

  # Should not error when printed
  expect_no_error(print(stats))

  # Verify print method is dispatched (check class is set)
  expect_s3_class(stats, "dsprrr_cache_stats")
})

test_that("cache stores and retrieves values", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  cache <- dsprrr:::get_cache()

  # Store a value
  cache$set("my_key", list(answer = "cached_result"))

  # Retrieve it
  result <- cache$get("my_key")

  expect_false(cachem::is.key_missing(result))
  expect_equal(result$answer, "cached_result")
})

test_that("clear_cache with 'memory' only clears memory", {
  local_reset_cache()

  # Memory only for simplicity
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)

  cache <- dsprrr:::get_cache()
  cache$set("test_key", "test_value")

  clear_cache("memory")

  result <- cache$get("test_key")
  expect_true(cachem::is.key_missing(result))
})

test_that("DSPRRR_CACHE_ENABLED=false disables cache", {
  local_reset_cache()

  # Reset config to force re-read of env var
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_config <- NULL

  withr::local_envvar(DSPRRR_CACHE_ENABLED = "false")

  config <- dsprrr:::get_cache_config()
  expect_false(config$enable)
})

test_that("DSPRRR_CACHE_ENABLED=0 disables cache", {
  local_reset_cache()

  # Reset config to force re-read of env var
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_config <- NULL

  withr::local_envvar(DSPRRR_CACHE_ENABLED = "0")

  config <- dsprrr:::get_cache_config()
  expect_false(config$enable)
})

test_that("DSPRRR_CACHE_ENABLED unset enables cache by default", {
  local_reset_cache()

  # Reset config to force re-read of env var
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_config <- NULL

  # Ensure env var is not set
  withr::local_envvar(DSPRRR_CACHE_ENABLED = NA)

  config <- dsprrr:::get_cache_config()
  expect_true(config$enable)
})

test_that("cached_chat_structured uses cache for repeated calls", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  # Create a mock LLM that tracks calls
  call_count <- 0
  mock_llm <- list(
    get_model = function() "mock-model",
    chat_structured = function(prompt, type, echo = "none") {
      call_count <<- call_count + 1
      list(answer = paste("response", call_count))
    },
    `.__enclos_env__` = list(private = list(api_args = list(temperature = 0.7)))
  )
  class(mock_llm) <- "Chat"

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call - cache miss
  result1 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type
  )

  expect_equal(call_count, 1)
  expect_equal(result1$answer, "response 1")

  # Second call with same params - cache hit
  result2 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type
  )

  # Should NOT have called LLM again

  expect_equal(call_count, 1)
  expect_equal(result2$answer, "response 1")

  # Verify cache stats
  stats <- cache_stats()
  expect_equal(stats$hits, 1L)
  expect_equal(stats$misses, 1L)
})

test_that("cached_chat_structured bypasses cache when disabled", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  # Create a mock LLM that tracks calls
  call_count <- 0
  mock_llm <- list(
    get_model = function() "mock-model",
    chat_structured = function(prompt, type, echo = "none") {
      call_count <<- call_count + 1
      list(answer = paste("response", call_count))
    }
  )
  class(mock_llm) <- "Chat"

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call
  result1 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type
  )

  expect_equal(call_count, 1)

  # Second call - should call LLM again (no caching)
  result2 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type
  )

  expect_equal(call_count, 2)
  expect_equal(result2$answer, "response 2")
})

# Disk cache integration tests

test_that("disk cache persists data across cache resets", {
  local_reset_cache()

  tmp_dir <- tempfile("cache_test_")
  withr::defer(unlink(tmp_dir, recursive = TRUE))

  # Configure with only disk cache
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = tmp_dir
  )

  cache <- dsprrr:::get_cache()
  expect_false(is.null(cache))

  # Store a value
  cache$set("persist_key", list(answer = "persisted value"))

  # Verify it's there
  result <- cache$get("persist_key")
  expect_false(cachem::is.key_missing(result))
  expect_equal(result$answer, "persisted value")

  # Reset memory references (simulating session refresh)
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache <- NULL
  pkg_env$cache_disk <- NULL

  # Reconfigure with same disk path
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = tmp_dir
  )

  # Get new cache handle
  cache2 <- dsprrr:::get_cache()

  # Data should still be there

  result2 <- cache2$get("persist_key")
  expect_false(cachem::is.key_missing(result2))
  expect_equal(result2$answer, "persisted value")
})

test_that("layered cache uses both memory and disk tiers", {
  local_reset_cache()

  tmp_dir <- tempfile("cache_test_layered_")
  withr::defer(unlink(tmp_dir, recursive = TRUE))

  # Configure with both tiers
  configure_cache(enable_memory = TRUE, enable_disk = TRUE, disk_path = tmp_dir)

  cache <- dsprrr:::get_cache()
  expect_false(is.null(cache))

  # Store a value
  cache$set("layered_key", list(answer = "layered value"))

  # Should be in memory cache
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  expect_false(is.null(pkg_env$cache_memory))
  mem_result <- pkg_env$cache_memory$get("layered_key")
  expect_false(cachem::is.key_missing(mem_result))

  # Should also be in disk cache
  expect_false(is.null(pkg_env$cache_disk))
  disk_result <- pkg_env$cache_disk$get("layered_key")
  expect_false(cachem::is.key_missing(disk_result))
})

test_that("disk cache creates directory when it doesn't exist", {
  local_reset_cache()

  # Use a nested path that doesn't exist
  tmp_dir <- file.path(tempdir(), "dsprrr_test", "nested", "cache")
  withr::defer(unlink(file.path(tempdir(), "dsprrr_test"), recursive = TRUE))

  # Directory should not exist yet
  expect_false(dir.exists(tmp_dir))

  # Configure disk cache - should create directory
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = tmp_dir
  )

  cache <- dsprrr:::get_cache()

  # If we got a cache, directory should exist now
  if (!is.null(cache)) {
    expect_true(dir.exists(tmp_dir))
  }
})

test_that("disk cache handles race condition when directory created by another process", {
  local_reset_cache()

  # Create a directory that will trigger the "already exists" warning
  # by creating it between the dir.exists() check and the dir.create() call
  tmp_dir <- file.path(tempdir(), "dsprrr_race_test", "cache")
  withr::defer(unlink(
    file.path(tempdir(), "dsprrr_race_test"),
    recursive = TRUE
  ))

  # Pre-create the directory to simulate race condition
  # When configure_cache runs, it will see dir doesn't exist, then
  # dir.create will warn "already exists" - this should NOT disable disk caching
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  # Now configure cache with that path
  # The internal logic checks !dir.exists() first, but we create the dir
  # to simulate the race. We need to manipulate this more carefully.

  # Actually, to properly test this, we need to:
  # 1. Have the dir not exist when check happens
  # 2. Have it exist when dir.create runs (creating the warning)
  # This is hard to simulate deterministically, so instead we'll verify
  # that if dir.create warns but directory exists, cache still works.

  # Reset and configure - the dir already exists, so no race happens in
  # this simple test. But we can at least verify the cache works with
  # an existing directory (which is the end state of a race).
  unlink(tmp_dir, recursive = TRUE)

  # Configure disk cache - should create directory normally
  configure_cache(
    enable_memory = FALSE,
    enable_disk = TRUE,
    disk_path = tmp_dir
  )

  cache <- dsprrr:::get_cache()

  # Cache should be available and directory should exist
  expect_false(is.null(cache))
  expect_true(dir.exists(tmp_dir))

  # Verify we can use the cache
  cache$set("race_test_key", "race_test_value")
  result <- cache$get("race_test_key")
  expect_false(cachem::is.key_missing(result))
  expect_equal(result, "race_test_value")
})

test_that("different mock LLMs don't share cache entries", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  # Create two mock LLMs with same "unknown" model behavior
  mock_llm1 <- list(
    get_model = function() stop("no model"),
    chat_structured = function(prompt, type, echo = "none") {
      list(answer = "response from LLM 1")
    }
  )
  class(mock_llm1) <- "Chat"

  mock_llm2 <- list(
    get_model = function() stop("no model"),
    chat_structured = function(prompt, type, echo = "none") {
      list(answer = "response from LLM 2")
    }
  )
  class(mock_llm2) <- "Chat"

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # Call with first LLM (expect warning about model extraction)
  result1 <- suppressWarnings(
    dsprrr:::cached_chat_structured(
      llm = mock_llm1,
      prompt = "test prompt",
      output_type = output_type
    )
  )

  expect_equal(result1$answer, "response from LLM 1")

  # Call with second LLM - should NOT get cached result from first
  result2 <- suppressWarnings(
    dsprrr:::cached_chat_structured(
      llm = mock_llm2,
      prompt = "test prompt",
      output_type = output_type
    )
  )

  # Different LLM objects should not share cache
  expect_equal(result2$answer, "response from LLM 2")
})

# New cache ergonomics features tests

test_that("first cache hit shows informative message", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  # Reset the first-hit flag
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_first_hit_shown <- FALSE

  # Create a mock LLM
  mock_llm <- list(
    get_model = function() "mock-model",
    chat_structured = function(prompt, type, echo = "none") {
      list(answer = "response")
    },
    `.__enclos_env__` = list(private = list(api_args = list(temperature = 0.7)))
  )
  class(mock_llm) <- "Chat"

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call - cache miss
  dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type
  )

  # Second call - cache hit, should show message
  expect_message(
    dsprrr:::cached_chat_structured(
      llm = mock_llm,
      prompt = "test prompt",
      output_type = output_type
    ),
    "Using cached LLM responses"
  )

  # Flag should now be set
  expect_true(pkg_env$cache_first_hit_shown)

  # Third call - should NOT show message again
  expect_no_message(
    dsprrr:::cached_chat_structured(
      llm = mock_llm,
      prompt = "test prompt",
      output_type = output_type
    )
  )
})

test_that("clear_cache resets first-hit flag", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  # Set the flag manually
  pkg_env <- asNamespace("dsprrr")$.dsprrr_env
  pkg_env$cache_first_hit_shown <- TRUE

  # Clear cache should reset it
  clear_cache()

  expect_false(isTRUE(pkg_env$cache_first_hit_shown))
})

test_that(".cache = FALSE bypasses cache for single call", {
  local_reset_cache()

  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()

  # Create a mock LLM that tracks calls
  call_count <- 0
  mock_llm <- list(
    get_model = function() "mock-model",
    chat_structured = function(prompt, type, echo = "none") {
      call_count <<- call_count + 1
      list(answer = paste("response", call_count))
    },
    `.__enclos_env__` = list(private = list(api_args = list(temperature = 0.7)))
  )
  class(mock_llm) <- "Chat"

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call - cache miss
  result1 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type
  )

  expect_equal(call_count, 1)
  expect_equal(result1$answer, "response 1")

  # Second call with .cache = FALSE - should bypass cache
  result2 <- suppressMessages(
    dsprrr:::cached_chat_structured(
      llm = mock_llm,
      prompt = "test prompt",
      output_type = output_type,
      .cache = FALSE
    )
  )

  # Should have called LLM again
  expect_equal(call_count, 2)
  expect_equal(result2$answer, "response 2")

  # Third call without .cache (uses global config) - cache hit
  result3 <- suppressMessages(
    dsprrr:::cached_chat_structured(
      llm = mock_llm,
      prompt = "test prompt",
      output_type = output_type
    )
  )

  # Should NOT have called LLM again (cache hit from first call)
  expect_equal(call_count, 2)
  expect_equal(result3$answer, "response 1")
})

test_that(".cache = TRUE has no effect when caching globally disabled", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  # Create a mock LLM that tracks calls
  call_count <- 0
  mock_llm <- list(
    get_model = function() "mock-model",
    chat_structured = function(prompt, type, echo = "none") {
      call_count <<- call_count + 1
      list(answer = paste("response", call_count))
    },
    `.__enclos_env__` = list(private = list(api_args = list(temperature = 0.7)))
  )
  class(mock_llm) <- "Chat"

  output_type <- ellmer::type_object(answer = ellmer::type_string())

  # First call with .cache = TRUE
  # With caching globally disabled, the cache object itself is NULL
  # So .cache = TRUE won't actually cache - it will still make the call
  result1 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type,
    .cache = TRUE
  )

  expect_equal(call_count, 1)

  # Second call with same prompt and .cache = TRUE
  # Should make a new call since cache is globally disabled
  result2 <- dsprrr:::cached_chat_structured(
    llm = mock_llm,
    prompt = "test prompt",
    output_type = output_type,
    .cache = TRUE
  )

  # Verify no caching occurred - second call was made
  expect_equal(call_count, 2)
})

# Integration tests for .cache parameter through run() API
test_that("run() respects .cache = FALSE parameter (single input)", {
  local_reset_cache()
  # Use memory-only cache to avoid disk pollution from previous runs
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  call_count <- 0
  mock_env <- new.env()
  mock_env$mock_llm <- structure(
    list(
      get_model = function() "mock-model",
      chat_structured = function(prompt, type, echo = "none") {
        call_count <<- call_count + 1
        list(sentiment = "positive")
      },
      `.__enclos_env__` = list(
        private = list(api_args = list(temperature = 0.7))
      ),
      clone = function(...) mock_env$mock_llm,
      set_turns = function(turns) invisible(NULL)
    ),
    class = "Chat"
  )
  mock_llm <- mock_env$mock_llm

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  # First call - cache miss
  result1 <- run(mod, text = "Great!", .llm = mock_llm)
  expect_equal(call_count, 1)

  # Second call with same input - should hit cache
  result2 <- run(mod, text = "Great!", .llm = mock_llm)
  expect_equal(call_count, 1) # No new call

  # Third call with .cache = FALSE - should bypass cache
  result3 <- run(mod, text = "Great!", .llm = mock_llm, .cache = FALSE)
  expect_equal(call_count, 2) # New call made

  # Fourth call with .cache = TRUE - should use cache
  result4 <- run(mod, text = "Great!", .llm = mock_llm, .cache = TRUE)
  expect_equal(call_count, 2) # No new call
})

# --- Cache-hit synthetic turn injection tests ---------------------------------

test_that("cache hit injects synthetic turns into the Chat object", {
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  turns <- list()
  mock_env <- new.env()
  mock_env$mock_llm <- structure(
    list(
      get_model = function() "mock-model",
      chat_structured = function(prompt, type, echo = "none") {
        user_turn <- ellmer::UserTurn(
          contents = list(ellmer::ContentText(prompt))
        )
        assistant_turn <- ellmer::AssistantTurn(
          contents = list(ellmer::ContentText("{\"sentiment\":\"positive\"}"))
        )
        turns <<- c(turns, list(user_turn, assistant_turn))
        list(sentiment = "positive")
      },
      `.__enclos_env__` = list(
        private = list(api_args = list(temperature = 0.7))
      ),
      clone = function(...) mock_env$mock_llm,
      set_turns = function(new_turns) {
        turns <<- new_turns
        invisible(NULL)
      },
      get_turns = function(...) turns,
      last_turn = function(role = c("assistant", "user"), ...) {
        role <- match.arg(role)
        class_name <- if (role == "assistant") {
          "ellmer::AssistantTurn"
        } else {
          "ellmer::UserTurn"
        }
        role_turns <- turns[vapply(
          turns,
          inherits,
          logical(1),
          what = class_name
        )]
        if (length(role_turns) == 0) {
          stop("no turns")
        }
        role_turns[[length(role_turns)]]
      }
    ),
    class = "Chat"
  )

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  # First call — cache miss, real LLM call adds turns

  invisible(run(mod, text = "Great!", .llm = mock_env$mock_llm))
  expect_equal(length(turns), 2) # user + assistant from real call

  # Second call — cache hit, synthetic turns should be injected
  mod$state$traces <- list()
  invisible(run(mod, text = "Great!", .llm = mock_env$mock_llm))
  expect_equal(length(turns), 4) # original 2 + synthetic 2

  # Verify the synthetic turns contain correct content
  last_user <- turns[[3]]
  last_assistant <- turns[[4]]
  expect_s3_class(last_user, "ellmer::UserTurn")
  expect_s3_class(last_assistant, "ellmer::AssistantTurn")
  expect_match(last_user@contents[[1]]@text, "Great!")
  expect_match(last_assistant@contents[[1]]@text, "positive")

  # Verify trace was recorded with turns (not NULL)
  trace <- mod$state$traces[[1]]
  expect_s3_class(trace$user_turn, "ellmer::UserTurn")
  expect_s3_class(trace$assistant_turn, "ellmer::AssistantTurn")
})

test_that("cache hit preserves pre-existing turn history", {
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  make_mock_chat <- function(initial_turns = list()) {
    turns <- initial_turns
    mock_env <- new.env()
    mock_env$mock_llm <- structure(
      list(
        get_model = function() "mock-model",
        chat_structured = function(prompt, type, echo = "none") {
          user_turn <- ellmer::UserTurn(
            contents = list(ellmer::ContentText(prompt))
          )
          assistant_turn <- ellmer::AssistantTurn(
            contents = list(ellmer::ContentText("{\"sentiment\":\"positive\"}"))
          )
          turns <<- c(turns, list(user_turn, assistant_turn))
          list(sentiment = "positive")
        },
        `.__enclos_env__` = list(
          private = list(api_args = list(temperature = 0.7))
        ),
        clone = function(...) mock_env$mock_llm,
        set_turns = function(new_turns) {
          turns <<- new_turns
          invisible(NULL)
        },
        get_turns = function(...) turns,
        last_turn = function(role = c("assistant", "user"), ...) {
          role <- match.arg(role)
          class_name <- if (role == "assistant") {
            "ellmer::AssistantTurn"
          } else {
            "ellmer::UserTurn"
          }
          role_turns <- turns[vapply(
            turns,
            inherits,
            logical(1),
            what = class_name
          )]
          if (length(role_turns) == 0) {
            stop("no turns")
          }
          role_turns[[length(role_turns)]]
        }
      ),
      class = "Chat"
    )
    mock_env$mock_llm
  }

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  # Warm cache
  llm_warm <- make_mock_chat()
  invisible(run(mod, text = "Great!", .llm = llm_warm))

  # Use a different chat with stale pre-existing history
  old_user <- ellmer::UserTurn(
    contents = list(ellmer::ContentText("OLD PROMPT"))
  )
  old_assistant <- ellmer::AssistantTurn(
    contents = list(ellmer::ContentText("OLD RESPONSE"))
  )
  llm_with_history <- make_mock_chat(
    initial_turns = list(old_user, old_assistant)
  )

  mod$state$traces <- list()
  invisible(run(mod, text = "Great!", .llm = llm_with_history))

  # Pre-existing turns preserved + new synthetic pair appended
  expect_equal(length(llm_with_history$get_turns()), 4)

  # Trace points to the new synthetic turns, not the old ones
  trace <- mod$state$traces[[1]]
  expect_match(trace$user_turn@contents[[1]]@text, "Great!")
  expect_false(
    identical(trace$user_turn@contents[[1]]@text, "OLD PROMPT")
  )
  expect_match(trace$assistant_turn@contents[[1]]@text, "positive")
})

test_that("cache hit works when Chat mock has no get_turns/set_turns", {
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  mock_env <- new.env()
  mock_env$mock_llm <- structure(
    list(
      get_model = function() "mock-model",
      chat_structured = function(prompt, type, echo = "none") {
        list(sentiment = "positive")
      },
      `.__enclos_env__` = list(
        private = list(api_args = list(temperature = 0.7))
      ),
      clone = function(...) mock_env$mock_llm
    ),
    class = "Chat"
  )

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  # Should not error even without get_turns/set_turns
  expect_no_error(run(mod, text = "Great!", .llm = mock_env$mock_llm))

  # Second call hits cache — still no error
  expect_no_error(run(mod, text = "Great!", .llm = mock_env$mock_llm))
})

test_that("synthetic turn tokens/cost/duration are NA (not 0)", {
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  turns <- list()
  mock_env <- new.env()
  mock_env$mock_llm <- structure(
    list(
      get_model = function() "mock-model",
      chat_structured = function(prompt, type, echo = "none") {
        user_turn <- ellmer::UserTurn(
          contents = list(ellmer::ContentText(prompt))
        )
        assistant_turn <- ellmer::AssistantTurn(
          contents = list(ellmer::ContentText("{\"answer\":\"42\"}")),
          tokens = c(10, 5, 0),
          cost = 0.001,
          duration = 1.5
        )
        turns <<- c(turns, list(user_turn, assistant_turn))
        list(answer = "42")
      },
      `.__enclos_env__` = list(
        private = list(api_args = list(temperature = 0.7))
      ),
      clone = function(...) mock_env$mock_llm,
      set_turns = function(new_turns) {
        turns <<- new_turns
        invisible(NULL)
      },
      get_turns = function(...) turns,
      last_turn = function(role = c("assistant", "user"), ...) {
        role <- match.arg(role)
        class_name <- if (role == "assistant") {
          "ellmer::AssistantTurn"
        } else {
          "ellmer::UserTurn"
        }
        role_turns <- turns[vapply(
          turns,
          inherits,
          logical(1),
          what = class_name
        )]
        if (length(role_turns) == 0) {
          stop("no turns")
        }
        role_turns[[length(role_turns)]]
      }
    ),
    class = "Chat"
  )

  sig <- signature("question -> answer: string")
  mod <- module(sig, type = "predict")

  # Cache miss — real call
  invisible(run(mod, question = "What?", .llm = mock_env$mock_llm))

  # Cache hit — synthetic turn
  mod$state$traces <- list()
  invisible(run(mod, question = "What?", .llm = mock_env$mock_llm))

  # Synthetic assistant turn should have NA tokens/cost/duration (not 0)
  synthetic_assistant <- turns[[4]]
  expect_true(all(is.na(synthetic_assistant@tokens)))
  expect_true(is.na(synthetic_assistant@cost))
  expect_true(is.na(synthetic_assistant@duration))
})

test_that("run() respects .cache = FALSE in batch processing", {
  local_reset_cache()
  # Use memory-only cache to avoid disk pollution from previous runs
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  call_count <- 0
  mock_env <- new.env()
  mock_env$mock_llm <- structure(
    list(
      get_model = function() "mock-model",
      chat_structured = function(prompt, type, echo = "none") {
        call_count <<- call_count + 1
        list(sentiment = "positive")
      },
      `.__enclos_env__` = list(
        private = list(api_args = list(temperature = 0.7))
      ),
      clone = function(...) mock_env$mock_llm,
      set_turns = function(turns) invisible(NULL)
    ),
    class = "Chat"
  )
  mock_llm <- mock_env$mock_llm

  sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
  mod <- module(sig, type = "predict")

  # First batch call - cache miss for both
  results1 <- run(
    mod,
    text = c("Great!", "Awesome!"),
    .llm = mock_llm,
    .progress = FALSE
  )
  expect_equal(call_count, 2)

  # Second batch call with same inputs - should hit cache
  results2 <- run(
    mod,
    text = c("Great!", "Awesome!"),
    .llm = mock_llm,
    .progress = FALSE
  )
  expect_equal(call_count, 2) # No new calls

  # Third batch call with .cache = FALSE - should bypass cache
  results3 <- run(
    mod,
    text = c("Great!", "Awesome!"),
    .llm = mock_llm,
    .cache = FALSE,
    .progress = FALSE
  )
  expect_equal(call_count, 4) # Two new calls
})

test_that("run() validates invalid .cache parameter values", {
  sig <- signature("text -> answer")
  mod <- module(sig, type = "predict")

  # Invalid: string instead of logical
  expect_error(
    run(mod, text = "test", .cache = "false"),
    class = "rlang_error"
  )

  # Invalid: NA value
  expect_error(
    run(mod, text = "test", .cache = NA),
    class = "rlang_error"
  )

  # Invalid: numeric instead of logical
  expect_error(
    run(mod, text = "test", .cache = 1),
    class = "rlang_error"
  )

  # Invalid: vector of length > 1
  expect_error(
    run(mod, text = "test", .cache = c(TRUE, FALSE)),
    class = "rlang_error"
  )
})

test_that("dsprrr_sitrep shows cache configuration", {
  local_reset_cache()

  configure_cache(enable = TRUE)

  # Should not error and output cache section
  result <- expect_no_error(dsprrr_sitrep())

  # Should return invisibly a list with cache info
  expect_type(result, "list")
  expect_true("cache_enabled" %in% names(result))
  expect_true(result$cache_enabled)

  # Should have cache_stats if there's cache activity
  if (!is.null(result$cache_stats)) {
    expect_s3_class(result$cache_stats, "dsprrr_cache_stats")
  }
})

test_that("dsprrr_sitrep shows disabled cache", {
  local_reset_cache()

  configure_cache(enable = FALSE)

  result <- expect_no_error(suppressMessages(dsprrr_sitrep()))

  expect_false(result$cache_enabled)
})

test_that(".cache is accepted and validated for non-Predict modules (dsprrr-jup)", {
  # Regression: only run.PredictModule declared .cache; for every other module
  # type and pipelines it fell into ... -> treated as an unknown signature input
  # (spurious warning) and dropped.
  mod <- module_fn("text -> answer", function(text) paste("Echo:", text))

  expect_no_warning(res <- run(mod, text = "hi", .cache = FALSE))
  expect_equal(res$answer, "Echo: hi")

  # Invalid .cache values are rejected the same way as for PredictModule.
  expect_error(run(mod, text = "hi", .cache = "yes"), "must be")
  expect_error(run(mod, text = "hi", .cache = NA), "must be")
})

test_that("rollout_id threads from forward() into the cache key (dsprrr-pcd)", {
  # Regression: rollout_id was implemented in the cache layer but no caller
  # passed it, so BestOfN/Refine retries with caching enabled were served
  # identical cached responses (effective N = 1).
  local_reset_cache()
  configure_cache(enable = TRUE, enable_memory = TRUE, enable_disk = FALSE)

  call_count <- 0
  mock_llm <- structure(
    list(chat_structured = function(prompt, type, ...) {
      call_count <<- call_count + 1
      list(answer = paste0("resp-", call_count))
    }),
    class = "Chat"
  )
  sig <- Signature(
    inputs = list(input(name = "q", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = ""
  )
  mod <- module(signature = sig, type = "predict", template = "{q}")

  # Same prompt, different rollout_id -> both miss the cache -> 2 real calls.
  mod$forward(list(q = "x"), .llm = mock_llm, rollout_id = 1)
  mod$forward(list(q = "x"), .llm = mock_llm, rollout_id = 2)
  expect_equal(call_count, 2)

  # Repeating a rollout_id hits the cache -> no new call.
  mod$forward(list(q = "x"), .llm = mock_llm, rollout_id = 1)
  expect_equal(call_count, 2)
})
