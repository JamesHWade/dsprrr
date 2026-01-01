# Tests for MIPROv2 teleprompter

test_that("MIPROv2 can be created with defaults", {
  tp <- MIPROv2()
  expect_s3_class(tp, "dsprrr::MIPROv2")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(tp@auto, "light")
  expect_equal(tp@max_bootstrapped_demos, 4L)
  expect_equal(tp@max_labeled_demos, 4L)
  expect_equal(tp@seed, 9L)
  expect_true(tp@track_stats)
})

test_that("MIPROv2 validates properties", {
  expect_error(
    MIPROv2(auto = "fast"),
    "auto must be NULL"
  )

  expect_error(
    MIPROv2(num_candidates = 0),
    "positive integer"
  )

  expect_error(
    MIPROv2(init_temperature = 0),
    "positive numeric"
  )
})

test_that("MIPROv2 runs end-to-end with auto=light", {
  MockMIPROModule <- R6::R6Class(
    "MockMIPROModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        input_val <- batch[[1]][[1]]
        output_val <- switch(
          input_val,
          "What is 2+2?" = "4",
          "What is 3+3?" = "6",
          "What is 4+4?" = "8",
          "What is 5+5?" = "10",
          "unknown"
        )
        tibble::tibble(
          output = list(output_val),
          chat = list(NULL),
          metadata = list(list())
        )
      }
    )
  )

  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )

  mod <- MockMIPROModule$new(signature = sig)

  trainset <- data.frame(
    question = c(
      "What is 2+2?",
      "What is 3+3?",
      "What is 4+4?",
      "What is 5+5?"
    ),
    answer = c("4", "6", "8", "10")
  )

  log_dir <- tempfile("mipro-log-")
  dir.create(log_dir)

  tp <- MIPROv2(
    metric = metric_exact_match(field = "answer"),
    auto = "light",
    max_bootstrapped_demos = 2L,
    max_labeled_demos = 2L,
    seed = 1L,
    log_dir = log_dir
  )

  compiled <- compile(tp, mod, trainset, valset = trainset)

  expect_true(compiled$config$compiled)
  expect_equal(compiled$config$teleprompter, "MIPROv2")
  expect_true(length(compiled$demos) > 0)

  optimizer <- compiled$config$optimizer
  expect_true(length(optimizer$demo_candidates) > 0)
  expect_true(length(optimizer$instruction_candidates) > 0)
  expect_s3_class(optimizer$trial_history, "tbl_df")
  expect_true(any(optimizer$trial_history$eval_type == "full"))

  expect_true(file.exists(file.path(log_dir, "trials.jsonl")))
})
