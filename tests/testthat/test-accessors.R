# Tests for accessor functions

test_that("get_output extracts output from structured result", {
  # Simple list with output
  result <- list(output = "hello", metadata = list())
  expect_equal(get_output(result), "hello")

  # List with predictions (evaluation style)
  result <- list(predictions = list("a", "b"), mean_score = 0.5)
  expect_equal(get_output(result), list("a", "b"))

  # Plain value
  expect_equal(get_output("value"), "value")
})

test_that("get_output works with dsprrr_batch_result", {
  batch <- structure(
    list(
      list(output = "a", metadata = list()),
      list(output = "b", metadata = list())
    ),
    class = c("dsprrr_batch_result", "list")
  )

  outputs <- get_output(batch)
  expect_equal(outputs, list("a", "b"))
})

test_that("get_output works with dsprrr_evaluation", {
  eval_result <- structure(
    list(
      mean_score = 0.5,
      predictions = list("a", "b"),
      scores = c(0, 1),
      n_evaluated = 2,
      n_errors = 0
    ),
    class = "dsprrr_evaluation"
  )

  expect_equal(get_output(eval_result), list("a", "b"))
})

test_that("get_metadata extracts metadata from structured result", {
  result <- list(
    output = "hello",
    metadata = list(model = "test", tokens = 100)
  )
  expect_equal(get_metadata(result), list(model = "test", tokens = 100))

  # Missing metadata
  expect_equal(get_metadata(list(output = "hello")), list())
})

test_that("get_metadata works with dsprrr_batch_result", {
  batch <- structure(
    list(
      list(output = "a", metadata = list(model = "m1")),
      list(output = "b", metadata = list(model = "m2"))
    ),
    class = c("dsprrr_batch_result", "list")
  )

  meta <- get_metadata(batch)
  expect_length(meta, 2)
  expect_equal(meta[[1]]$model, "m1")
  expect_equal(meta[[2]]$model, "m2")
})

test_that("get_tokens extracts token counts", {
  result <- list(
    output = "hello",
    metadata = list(input_tokens = 10L, output_tokens = 5L, total_tokens = 15L)
  )

  tokens <- get_tokens(result)
  expect_equal(tokens$input_tokens, 10L)
  expect_equal(tokens$output_tokens, 5L)
  expect_equal(tokens$total_tokens, 15L)

  # Missing metadata
  tokens_empty <- get_tokens(list(output = "hello"))
  expect_true(is.na(tokens_empty$input_tokens))
})

test_that("get_tokens works with dsprrr_batch_result", {
  batch <- structure(
    list(
      list(
        output = "a",
        metadata = list(input_tokens = 10L, output_tokens = 5L, total_tokens = 15L)
      ),
      list(
        output = "b",
        metadata = list(input_tokens = 20L, output_tokens = 10L, total_tokens = 30L)
      )
    ),
    class = c("dsprrr_batch_result", "list")
  )

  tokens <- get_tokens(batch)
  expect_s3_class(tokens, "tbl_df")
  expect_equal(tokens$input_tokens, c(10L, 20L))
  expect_equal(tokens$output_tokens, c(5L, 10L))
})

test_that("get_cost extracts cost", {
  result <- list(
    output = "hello",
    metadata = list(cost = 0.001)
  )
  expect_equal(get_cost(result), 0.001)

  # Missing cost
  expect_true(is.na(get_cost(list(output = "hello"))))
})

test_that("get_cost works with dsprrr_batch_result", {
  batch <- structure(
    list(
      list(output = "a", metadata = list(cost = 0.001)),
      list(output = "b", metadata = list(cost = 0.002))
    ),
    class = c("dsprrr_batch_result", "list")
  )

  result <- get_cost(batch)
  expect_s3_class(result, "dsprrr_cost_summary")
  expect_s3_class(result$costs, "tbl_df")
  expect_equal(result$costs$cost, c(0.001, 0.002))
  expect_equal(result$total, 0.003)
})

test_that("get_cost works with dsprrr_evaluation", {
  eval_result <- structure(
    list(
      mean_score = 0.5,
      predictions = list("a", "b"),
      scores = c(0, 1),
      metadata = list(
        list(cost = 0.001),
        list(cost = 0.002)
      ),
      n_evaluated = 2,
      n_errors = 0
    ),
    class = "dsprrr_evaluation"
  )

  result <- get_cost(eval_result)
  expect_s3_class(result, "dsprrr_cost_summary")
  expect_s3_class(result$costs, "tbl_df")
  expect_equal(result$costs$cost, c(0.001, 0.002))
  expect_equal(result$total, 0.003)
})

test_that("get_tokens works with dsprrr_evaluation", {
  eval_result <- structure(
    list(
      mean_score = 0.5,
      predictions = list("a", "b"),
      scores = c(0, 1),
      metadata = list(
        list(input_tokens = 10L, output_tokens = 5L, total_tokens = 15L),
        list(input_tokens = 20L, output_tokens = 10L, total_tokens = 30L)
      ),
      n_evaluated = 2,
      n_errors = 0
    ),
    class = "dsprrr_evaluation"
  )

  tokens <- get_tokens(eval_result)
  expect_s3_class(tokens, "tbl_df")
  expect_equal(tokens$input_tokens, c(10L, 20L))
  expect_equal(sum(tokens$total_tokens), 45L)
})

test_that("get_tokens handles empty dsprrr_evaluation", {
  eval_result <- structure(
    list(
      mean_score = NA_real_,
      predictions = list(),
      scores = numeric(0),
      metadata = list(),
      n_evaluated = 0,
      n_errors = 0
    ),
    class = "dsprrr_evaluation"
  )

  tokens <- get_tokens(eval_result)
  expect_s3_class(tokens, "tbl_df")
  expect_equal(nrow(tokens), 0)
})

test_that("get_cost handles empty dsprrr_evaluation", {
  eval_result <- structure(
    list(
      mean_score = NA_real_,
      predictions = list(),
      scores = numeric(0),
      metadata = list(),
      n_evaluated = 0,
      n_errors = 0
    ),
    class = "dsprrr_evaluation"
  )

  result <- get_cost(eval_result)
  expect_s3_class(result, "dsprrr_cost_summary")
  expect_s3_class(result$costs, "tbl_df")
  expect_equal(nrow(result$costs), 0)
  expect_equal(result$total, 0)
})
