test_that("module_fn creates callable modules that run and evaluate", {
  mod <- module_fn(
    "text -> answer",
    function(text, ...) {
      list(answer = paste("Echo:", text))
    },
    name = "echoer"
  )

  expect_s3_class(mod, "FnModule")
  expect_equal(mod$config$name, "echoer")
  expect_equal(run(mod, text = "hello")$answer, "Echo: hello")

  eval <- evaluate(
    mod,
    data = data.frame(text = c("a", "b"), answer = c("Echo: a", "Echo: b")),
    metric = function(prediction, expected_row) {
      identical(prediction$answer, expected_row$answer[[1]])
    }
  )

  expect_s3_class(eval, "dsprrr_evaluation")
  expect_equal(eval$mean_score, 1)
})

test_that("module_fn accepts scalar returns for single-field signatures", {
  mod <- module_fn("text -> answer", function(text) paste("Echo:", text))

  expect_equal(run(mod, text = "hi"), list(answer = "Echo: hi"))
})

test_that("module_fn captures metadata from .metadata", {
  mod <- module_fn(
    "text -> answer",
    function(text, ...) {
      list(answer = text, .metadata = list(source = "test"))
    }
  )

  result <- run(mod, text = "hi", .return_format = "structured")

  expect_equal(result$output$answer, "hi")
  expect_equal(result$metadata$source, "test")
  expect_true(is.numeric(result$metadata$latency_ms))
})

test_that("module_fn validates missing and extra output fields", {
  missing <- module_fn("text -> answer, confidence: number", function(text) {
    list(answer = text)
  })
  expect_error(
    run(missing, text = "hi"),
    "missing required output fields"
  )

  extra <- module_fn("text -> answer", function(text) {
    list(answer = text, extra = TRUE)
  })
  expect_error(
    run(extra, text = "hi"),
    "unknown output fields"
  )
})

test_that("module_fn validates basic output types", {
  mod <- module_fn("text -> confidence: number", function(text) {
    list(confidence = "high")
  })

  expect_error(
    run(mod, text = "hi"),
    "wrong type"
  )
})

test_that("module_fn objects do not support optimization yet", {
  mod <- module_fn("text -> answer", function(text) list(answer = text))

  expect_error(
    compile(
      LabeledFewShot(),
      mod,
      data.frame(text = "hi", answer = "hi")
    ),
    "do not support optimization"
  )
})
