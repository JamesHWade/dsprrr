test_that("as_vitals_solver returns vitals-compatible results", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Repeat"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  mock_llm <- structure(
    list(
      chat_structured = function(prompt, ...) {
        lines <- strsplit(prompt, "\n")[[1]]
        tail(lines, 1L)
      }
    ),
    class = "Chat"
  )

  solver <- as_vitals_solver(mod, .llm = mock_llm)

  inputs <- data.frame(text = c("foo", "bar"), stringsAsFactors = FALSE)
  result <- solver(inputs)

  expect_equal(result$result, list("foo", "bar"))
  expect_equal(length(result$solver_chat), 2)
  expect_true(all(vapply(result$solver_chat, inherits, logical(1), what = "Chat")))
  expect_equal(length(result$metadata), 2)
  expect_true(all(vapply(result$metadata, function(x) "prompt" %in% names(x), logical(1))))

  simple_solver <- as_vitals_solver(mod, .llm = mock_llm, .return_format = "simple")
  simple_result <- simple_solver(inputs)
  expect_equal(simple_result$result, list("foo", "bar"))
  expect_equal(simple_result$solver_chat, list(NULL, NULL))
  expect_equal(simple_result$metadata, list(list(), list()))
})

test_that("as_dsprrr_metric wraps vitals scorers", {
  vitals_scorer <- function(samples) {
    tibble::tibble(
      score = ifelse(
        vapply(samples$result, `[[`, character(1), 1) ==
          vapply(samples$target, `[[`, character(1), 1),
        "C",
        "I"
      )
    )
  }

  metric <- as_dsprrr_metric(vitals_scorer)

  expected <- data.frame(target = "answer", stringsAsFactors = FALSE)
  expect_equal(metric("answer", expected), 1)
  expect_equal(metric("wrong", expected), 0)

  # Numeric scorer
  numeric_scorer <- function(samples) tibble::tibble(score = 0.4)
  metric_num <- as_dsprrr_metric(numeric_scorer)
  expect_equal(metric_num("any", expected), 0.4)
})
