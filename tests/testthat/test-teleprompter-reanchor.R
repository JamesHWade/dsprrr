flag_signature <- function(description = "Should this item be flagged?") {
  signature(
    inputs = list(input("text")),
    output_type = ellmer::type_object(
      flag = ellmer::type_boolean(description)
    )
  )
}

flag_data <- function() {
  p <- seq(0.05, 0.95, by = 0.1)
  data.frame(
    text = paste0("item", seq_along(p)),
    flag = p >= 0.72,
    p = p
  )
}

# Evidence follows `p`; native generation always answers `native`.
flag_chat <- function(data, native = TRUE) {
  new_decision_chat(function(input, field, field_type) {
    row <- data[data$text == input, , drop = FALSE]
    if (is_evidence_type(field_type)) {
      list(probability = row$p)
    } else if (identical(native, "truth")) {
      row$flag
    } else {
      native
    }
  })
}

flag_metric <- function(prediction, expected) {
  as.numeric(identical(prediction$flag, expected$flag[[1]]))
}

test_that("ReAnchor validates its settings", {
  expect_s7_class(ReAnchor(metric = flag_metric), ReAnchor)
  expect_error(ReAnchor(metric = flag_metric, folds = 1L), "folds")
  expect_error(
    ReAnchor(metric = flag_metric, max_candidates = 2L),
    "max_candidates"
  )
  expect_error(ReAnchor(metric = flag_metric, fields = character()), "fields")
})

test_that("ReAnchor fits a Boolean threshold from recorded evidence", {
  data <- flag_data()
  mock <- flag_chat(data)
  mod <- module(flag_signature()) |> with_decisions(flag = decision_bool())

  tuned <- compile(mod, ReAnchor(metric = flag_metric), data, .llm = mock$chat)

  threshold <- decision_settings(tuned)$threshold
  expect_gt(threshold, 0.65)
  expect_lt(threshold, 0.75)
  expect_equal(decision_settings(mod)$threshold, 0.5)

  # The configured module already requested evidence, so one pass sufficed.
  expect_length(mock$calls$types, nrow(data))

  result <- optimization_result(tuned)
  expect_identical(result$optimizer, "ReAnchor")
  expect_equal(result$baseline_score, 0.8)
  expect_equal(result$best_score, 1)
  report <- result$extensions$re_anchor
  expect_true(report$accepted)
  expect_identical(report$fitted[[1]]$parameter, "threshold")
  expect_identical(report$fitted[[1]]$observed$calls, nrow(data))
  expect_identical(report$fitted[[1]]$fold_check$passed, 1L)
})

test_that("ReAnchor promotes described native outputs to evidence decoding", {
  data <- flag_data()
  mock <- flag_chat(data, native = TRUE)
  mod <- module(flag_signature())

  tuned <- compile(mod, ReAnchor(metric = flag_metric), data, .llm = mock$chat)

  expect_identical(decision_settings(tuned)$field, "flag")
  expect_null(mod$config$decisions)
  kinds <- vapply(
    mock$calls$types,
    function(type) {
      is_evidence_type(type@properties$flag)
    },
    logical(1)
  )
  expect_identical(kinds, rep(c(FALSE, TRUE), each = nrow(data)))
  expect_equal(optimization_result(tuned)$baseline_score, 0.3)
  expect_equal(optimization_result(tuned)$best_score, 1)
})

test_that("ReAnchor restores the original behavior when fitting cannot beat it", {
  data <- flag_data()
  mock <- flag_chat(data, native = "truth")

  tuned <- compile(
    module(flag_signature()),
    ReAnchor(metric = flag_metric),
    data,
    .llm = mock$chat
  )

  expect_null(tuned$config$decisions)
  report <- optimization_result(tuned)$extensions$re_anchor
  expect_false(report$accepted)
  expect_match(report$reason, "did not beat")
  expect_identical(optimization_result(tuned)$stop_reason, "no_improvement")
})

test_that("ReAnchor fits Choice weights", {
  data <- data.frame(
    text = paste0("t", 1:8),
    team = rep(c("billing", "technical"), each = 4)
  )
  mock <- new_decision_chat(function(input, field, field_type) {
    technical <- data$team[data$text == input] == "technical"
    list(
      probabilities = if (technical) {
        list(billing = 0.55, technical = 0.45)
      } else {
        list(billing = 0.9, technical = 0.1)
      },
      confidence = 0.5
    )
  })
  mod <- module(signature(
    inputs = list(input("text")),
    output_type = ellmer::type_object(
      team = ellmer::type_enum(c("billing", "technical"), "Which team?")
    )
  )) |>
    with_decisions(team = decision_choice())
  metric <- function(prediction, expected) {
    as.numeric(prediction$team == expected$team[[1]])
  }

  tuned <- compile(mod, ReAnchor(metric = metric), data, .llm = mock$chat)

  weights <- decision_settings(tuned)$weights[[1]]
  ratio <- weights[["technical"]] / weights[["billing"]]
  expect_gt(ratio, 0.55 / 0.45)
  expect_lt(ratio, 9)
  expect_equal(optimization_result(tuned)$best_score, 1)
})

test_that("ReAnchor fits Score cuts", {
  means <- c(0.2, 0.2, 0.2, 0.4, 0.4, 0.4, 1.2, 1.2, 1.8, 1.8)
  data <- data.frame(
    text = paste0("s", seq_along(means)),
    level = c(rep("low", 3), rep("mid", 5), rep("high", 2)),
    mean = means
  )
  mock <- new_decision_chat(function(input, field, field_type) {
    m <- data$mean[data$text == input]
    p <- if (m < 1) c(1 - m, m, 0) else c(0, 2 - m, m - 1)
    list(
      probabilities = stats::setNames(as.list(p), c("0", "1", "2")),
      confidence = 0.9
    )
  })
  mod <- module(signature(
    inputs = list(input("text")),
    output_type = ellmer::type_object(
      level = ellmer::type_enum(c("low", "mid", "high"), "Rate the level.")
    )
  )) |>
    with_decisions(level = decision_score())
  metric <- function(prediction, expected) {
    as.numeric(prediction$level == expected$level[[1]])
  }

  tuned <- compile(mod, ReAnchor(metric = metric), data, .llm = mock$chat)

  cuts <- decision_settings(tuned)$cuts[[1]]
  expect_gt(cuts[[1]], 0.2)
  expect_lt(cuts[[1]], 0.4)
  expect_equal(cuts[[2]], 1.5)
  expect_equal(optimization_result(tuned)$best_score, 1)
})

test_that("ReAnchor scores a validation set without fitting on it", {
  data <- flag_data()
  mock <- flag_chat(data)
  mod <- module(flag_signature()) |> with_decisions(flag = decision_bool())

  tuned <- compile(
    mod,
    ReAnchor(metric = flag_metric),
    data,
    valset = data[1:4, ],
    .llm = mock$chat
  )
  report <- optimization_result(tuned)$extensions$re_anchor
  expect_equal(report$val_score_before, 1)
  expect_equal(report$val_score, 1)
})

test_that("ReAnchor rejects programs it cannot calibrate", {
  data <- flag_data()
  mock <- flag_chat(data)

  expect_error(
    compile(
      module(flag_signature(description = NULL)),
      ReAnchor(metric = flag_metric),
      data,
      .llm = mock$chat
    ),
    class = "dsprrr_reanchor_error"
  )
  expect_error(
    compile(
      module(flag_signature()),
      ReAnchor(metric = flag_metric, fields = "missing"),
      data,
      .llm = mock$chat
    ),
    class = "dsprrr_reanchor_error"
  )
  expect_error(
    compile(
      module(flag_signature()),
      ReAnchor(metric = flag_metric),
      data[0, ],
      .llm = mock$chat
    ),
    class = "dsprrr_reanchor_error"
  )
  expect_error(
    compile(
      module(flag_signature()),
      ReAnchor(metric = flag_metric),
      data[1, ],
      .llm = mock$chat
    ),
    class = "dsprrr_reanchor_error",
    regexp = "at least two rows"
  )
  expect_error(
    compile(
      module(flag_signature()),
      ReAnchor(),
      data,
      .llm = mock$chat
    ),
    class = "dsprrr_reanchor_error"
  )
  expect_error(
    compile(
      pipeline(module(flag_signature())),
      ReAnchor(metric = flag_metric),
      data,
      .llm = mock$chat
    ),
    class = "dsprrr_reanchor_error"
  )
})

test_that("ReAnchor gap candidates cover every distinct decision", {
  gaps <- reanchor_gaps(c(0.98, 1), 0, 1, max_candidates = 40L)
  expect_equal(vapply(gaps, `[[`, numeric(1), "value"), c(0.5, 0.99))

  many <- reanchor_gaps(seq(0.001, 0.999, length.out = 200), 0, 1, 10L)
  expect_lte(length(many), 10L)

  geometric <- reanchor_gaps(c(1), 0.25, 4, 40L, geometric = TRUE)
  expect_equal(vapply(geometric, `[[`, numeric(1), "value"), c(0.5, 2))
})

test_that("ReAnchor folds are deterministic and cover every row", {
  folds <- reanchor_folds(7L, 5L)
  expect_length(folds, 5L)
  expect_setequal(unlist(folds), 1:7)
  expect_identical(folds, reanchor_folds(7L, 5L))
})

test_that("ReAnchor fits only the requested fields", {
  data <- flag_data()
  mock <- new_decision_chat(function(input, field, field_type) {
    list(probability = data$p[data$text == input])
  })
  mod <- module(signature(
    inputs = list(input("text")),
    output_type = ellmer::type_object(
      flag = ellmer::type_boolean("Should this item be flagged?"),
      other = ellmer::type_boolean("Is this item archived?")
    )
  )) |>
    with_decisions(
      flag = decision_bool(),
      other = decision_bool(threshold = 0.4)
    )
  metric <- function(prediction, expected) {
    as.numeric(identical(prediction$flag, expected$flag[[1]])) +
      as.numeric(identical(prediction$other, expected$flag[[1]]))
  }

  tuned <- compile(
    mod,
    ReAnchor(metric = metric, fields = "flag"),
    data,
    .llm = mock$chat
  )

  settings <- decision_settings(tuned)
  expect_gt(settings$threshold[settings$field == "flag"], 0.65)
  expect_identical(settings$threshold[settings$field == "other"], 0.4)
  fitted <- optimization_result(tuned)$extensions$re_anchor$fitted
  expect_identical(vapply(fitted, `[[`, character(1), "field"), "flag")
})

test_that("ReAnchor reaches Choice weights beyond fixed bounds", {
  data <- data.frame(
    text = paste0("t", 1:8),
    team = rep(c("billing", "technical"), each = 4)
  )
  mock <- new_decision_chat(function(input, field, field_type) {
    technical <- data$team[data$text == input] == "technical"
    list(
      probabilities = if (technical) {
        list(billing = 0.9999, technical = 0.0001)
      } else {
        list(billing = 1, technical = 0)
      },
      confidence = 0.5
    )
  })
  mod <- module(signature(
    inputs = list(input("text")),
    output_type = ellmer::type_object(
      team = ellmer::type_enum(c("billing", "technical"), "Which team?")
    )
  )) |>
    with_decisions(team = decision_choice())
  metric <- function(prediction, expected) {
    as.numeric(prediction$team == expected$team[[1]])
  }

  tuned <- compile(mod, ReAnchor(metric = metric), data, .llm = mock$chat)

  weights <- decision_settings(tuned)$weights[[1]]
  expect_gt(weights[["technical"]] / weights[["billing"]], 9999)
  expect_equal(optimization_result(tuned)$best_score, 1)
})

test_that("ReAnchor gives trace metrics the evaluate() trace and current decisions", {
  data <- flag_data()
  mock <- flag_chat(data)
  mod <- module(flag_signature()) |> with_decisions(flag = decision_bool())
  seen <- new.env(parent = emptyenv())
  seen$mismatch <- 0L
  seen$events <- integer()
  metric <- metric_with_trace(function(prediction, expected, program_trace) {
    seen$events <- c(seen$events, length(program_trace$events))
    record <- program_trace$metadata$decisions$flag
    if (!identical(record$value, prediction$flag)) {
      seen$mismatch <- seen$mismatch + 1L
    }
    as.numeric(identical(prediction$flag, expected$flag[[1]]))
  })

  evaluate(mod, data, metric, .llm = mock$chat, .progress = FALSE)
  evaluate_events <- seen$events
  seen$events <- integer()

  tuned <- compile(mod, ReAnchor(metric = metric), data, .llm = mock$chat)

  expect_identical(seen$mismatch, 0L)
  expect_true(all(evaluate_events > 0L))
  expect_true(all(seen$events == evaluate_events[[1]]))
  expect_equal(optimization_result(tuned)$best_score, 1)
})
