# Tests for prompt visibility features

test_that("last_prompt returns NULL when no history", {
  # Clear any existing history
  clear_prompt_history()

  result <- get_last_prompt()
  expect_null(result)
})

test_that("inspect_history returns empty tibble when no history", {
  clear_prompt_history()

  result <- inspect_history()
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 0)
})

test_that("clear_prompt_history clears history", {
  # Add a fake entry to history
  .dsprrr_env$prompt_history <- list(
    list(
      timestamp = Sys.time(),
      source = "test",
      prompt = "test prompt",
      response = "test response"
    )
  )

  n_cleared <- clear_prompt_history()
  expect_equal(n_cleared, 1)
  expect_equal(length(.dsprrr_env$prompt_history), 0)
  expect_identical(dsprrr:::prompt_history_generation(), 0)
})

test_that("add_to_global_history adds entries", {
  clear_prompt_history()

  # Add an entry
  trace <- list(
    timestamp = Sys.time(),
    prompt = "Test prompt",
    output = "Test response",
    model = "test-model"
  )
  add_to_global_history(trace, source = "test")

  expect_equal(length(.dsprrr_env$prompt_history), 1)
  expect_identical(dsprrr:::prompt_history_generation(), 1)
  expect_equal(.dsprrr_env$prompt_history[[1]]$source, "test")
  expect_equal(.dsprrr_env$prompt_history[[1]]$prompt, "Test prompt")
})

test_that("prompt history generation wraps without losing append counts", {
  clear_prompt_history()
  .dsprrr_env$prompt_history_generation <-
    dsprrr:::prompt_history_generation_max
  before <- dsprrr:::prompt_history_generation()

  add_to_global_history(
    list(prompt = "wrapped", output = "wrapped"),
    source = "test"
  )

  after <- dsprrr:::prompt_history_generation()
  expect_identical(after, 0)
  expect_identical(
    dsprrr:::prompt_history_generation_delta(before, after),
    1
  )
  clear_prompt_history()
})

test_that("add_to_global_history respects max history limit", {
  clear_prompt_history()

  # Set a small max
  old_max <- getOption("dsprrr.prompt_history_max")
  options(dsprrr.prompt_history_max = 3)

  # Add 5 entries
  for (i in 1:5) {
    trace <- list(
      timestamp = Sys.time(),
      prompt = paste("Prompt", i),
      output = paste("Response", i),
      model = "test-model"
    )
    add_to_global_history(trace, source = paste("test", i))
  }

  # Should only have last 3
  expect_equal(length(.dsprrr_env$prompt_history), 3)

  # Check it's the last 3 (3, 4, 5)
  expect_equal(.dsprrr_env$prompt_history[[1]]$prompt, "Prompt 3")
  expect_equal(.dsprrr_env$prompt_history[[3]]$prompt, "Prompt 5")

  # Restore
  options(dsprrr.prompt_history_max = old_max)
  clear_prompt_history()
})

test_that("last_prompt returns dsprrr_prompt_inspection object", {
  clear_prompt_history()

  # Add an entry
  trace <- list(
    timestamp = Sys.time(),
    prompt = "Test prompt",
    output = list(answer = "Test response"),
    model = "test-model",
    metadata = list(
      input_tokens = 10L,
      output_tokens = 5L,
      cost = 0.001
    )
  )
  add_to_global_history(trace, source = "test")

  result <- get_last_prompt()
  expect_s3_class(result, "dsprrr_prompt_inspection")
  expect_equal(result$prompt, "Test prompt")
  expect_equal(result$model, "test-model")
  expect_equal(result$source, "test")

  clear_prompt_history()
})

test_that("inspect_history returns correct tibble structure", {
  clear_prompt_history()

  # Add entries
  for (i in 1:3) {
    trace <- list(
      timestamp = Sys.time(),
      prompt = paste("Prompt", i),
      output = paste("Response", i),
      model = "test-model",
      metadata = list(
        input_tokens = 10L,
        output_tokens = 5L,
        cost = 0.001,
        duration_s = 0.5
      )
    )
    add_to_global_history(trace, source = paste("source", i))
  }

  result <- inspect_history(n = 10)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 3)

  # Check columns exist
  expect_true("timestamp" %in% names(result))
  expect_true("source" %in% names(result))
  expect_true("model" %in% names(result))
  expect_true("tokens_in" %in% names(result))
  expect_true("tokens_out" %in% names(result))
  expect_true("cost" %in% names(result))
  expect_true("prompt" %in% names(result))
  expect_true("response" %in% names(result))

  clear_prompt_history()
})

test_that("inspect_history respects n parameter", {
  clear_prompt_history()

  # Add 5 entries
  for (i in 1:5) {
    trace <- list(
      timestamp = Sys.time(),
      prompt = paste("Prompt", i),
      output = paste("Response", i),
      model = "test-model"
    )
    add_to_global_history(trace, source = paste("source", i))
  }

  result <- inspect_history(n = 2)
  expect_equal(nrow(result), 2)

  # Should be the last 2 entries (4 and 5)
  expect_equal(result$prompt[1], "Prompt 4")
  expect_equal(result$prompt[2], "Prompt 5")

  clear_prompt_history()
})

test_that("inspect_history respects include_prompts parameter", {
  clear_prompt_history()

  trace <- list(
    timestamp = Sys.time(),
    prompt = "Test prompt",
    output = "Test response",
    model = "test-model"
  )
  add_to_global_history(trace, source = "test")

  # With prompts
  result_with <- inspect_history(n = 1, include_prompts = TRUE)
  expect_true("prompt" %in% names(result_with))

  # Without prompts
  result_without <- inspect_history(n = 1, include_prompts = FALSE)
  expect_false("prompt" %in% names(result_without))

  clear_prompt_history()
})

test_that("inspect_history writes a plain transcript to file", {
  clear_prompt_history()
  on.exit(clear_prompt_history())

  trace <- list(
    timestamp = as.POSIXct("2026-04-21 12:00:00", tz = "UTC"),
    prompt = "Test prompt",
    output = "Test response",
    model = "test-model",
    metadata = list(
      input_tokens = 10L,
      output_tokens = 5L,
      cost = 0.001
    )
  )
  add_to_global_history(trace, source = "test")

  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)

  result <- inspect_history(n = 1, file = tmp)
  transcript <- paste(readLines(tmp), collapse = "\n")

  expect_s3_class(result, "tbl_df")
  expect_match(transcript, "Prompt:")
  expect_match(transcript, "Test prompt")
  expect_match(transcript, "Response:")
  expect_match(transcript, "Test response")
  expect_match(transcript, "Model: test-model")
})

test_that("print.dsprrr_prompt_inspection works", {
  clear_prompt_history()

  trace <- list(
    timestamp = Sys.time(),
    prompt = "Test prompt text",
    output = "Test response text",
    model = "gpt-4o-mini",
    metadata = list(
      input_tokens = 50L,
      output_tokens = 20L,
      cost = 0.0012,
      duration_s = 0.8
    )
  )
  add_to_global_history(trace, source = "test")

  result <- get_last_prompt()

  # Check that print returns invisible(x) and doesn't error
  expect_invisible(print(result))

  # Check structure of the object
  expect_equal(result$prompt, "Test prompt text")
  expect_equal(result$model, "gpt-4o-mini")
  expect_equal(result$tokens_in, 50L)

  clear_prompt_history()
})

test_that("Module$inspect() method exists and works", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Process the text"
  )
  mod <- module(signature = sig, type = "predict")

  # Check that inspect returns invisible self and doesn't error
  expect_invisible(mod$inspect())

  # Check that the method exists
  expect_true("inspect" %in% names(mod))
})
