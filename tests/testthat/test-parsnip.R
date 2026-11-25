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

test_that("llm_predict fits and predicts via parsnip::fit() and fit_xy()", {
  skip_if_not_installed("parsnip")

  register_dsprrr_engine()

  train <- tibble::tibble(
    text = c("Great value", "Arrived broken", "Works fine"),
    sentiment = factor(
      c("positive", "negative", "neutral"),
      levels = c("positive", "negative", "neutral")
    )
  )

  spec <- llm_predict(
    mode = "classification",
    signature = "text -> sentiment: enum('positive', 'negative', 'neutral')"
  ) |>
    parsnip::set_engine("dsprrr")

  fitted <- parsnip::fit(spec, sentiment ~ text, data = train)
  expect_s3_class(fitted, "model_fit")
  expect_s3_class(fitted$fit, "Module")

  fitted_xy <- parsnip::fit_xy(spec, x = train["text"], y = train$sentiment)
  expect_s3_class(fitted_xy, "model_fit")

  mock_chat <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      list(sentiment = if (grepl("broken", prompt)) "negative" else "positive")
    }
  )
  local_lm(mock_chat)

  preds <- stats::predict(
    fitted_xy,
    new_data = tibble::tibble(text = c("Love it", "Arrived broken"))
  )
  expect_s3_class(preds, "tbl_df")
  expect_named(preds, ".pred_class")
  expect_equal(as.character(preds$.pred_class), c("positive", "negative"))
  expect_equal(levels(preds$.pred_class), levels(train$sentiment))
})
