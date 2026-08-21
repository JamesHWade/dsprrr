test_that("llm_predict exposes only current engine parameters", {
  skip_if_not_installed("parsnip")

  expect_named(
    formals(llm_predict),
    c("mode", "signature", "temperature", "top_p")
  )
  expect_error(llm_predict(model = "gpt-4o-mini"), "unused argument")
  expect_error(llm_predict(provider = "openai"), "unused argument")
})

test_that("fit_llm_predict stores only runtime parameters", {
  fitted <- fit_llm_predict(
    x = data.frame(text = "example"),
    y = factor("positive"),
    signature = "text -> sentiment: enum('positive')",
    temperature = 0.2,
    top_p = 0.9
  )

  expect_s3_class(fitted, "Module")
  expect_equal(fitted$config$params$temperature, 0.2)
  expect_equal(fitted$config$params$top_p, 0.9)
  expect_false(any(c("model", "provider") %in% names(fitted$config)))
  expect_error(
    fit_llm_predict(
      x = data.frame(text = "example"),
      y = factor("positive"),
      model = "gpt-4o-mini"
    ),
    "unused argument"
  )
})
