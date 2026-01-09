# Tests for LLM response caching

# Helper to reset cache state
local_reset_cache <- function(.env = parent.frame()) {
  withr::defer(
    {
      # Reset to defaults
      dsprrr:::configure_cache()
      dsprrr:::clear_cache()
    },
    envir = .env
  )
  dsprrr:::clear_cache()
}

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
