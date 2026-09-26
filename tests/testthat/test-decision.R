triage_signature <- function() {
  signature(
    inputs = list(input("text", description = "Customer report")),
    output_type = ellmer::type_object(
      urgent = ellmer::type_boolean("Is the service blocked?"),
      severity = ellmer::type_enum(
        c("minor", "disruptive", "blocking"),
        "How severe is the impact?"
      ),
      category = ellmer::type_enum(
        c("billing", "technical"),
        "Which team owns the issue?"
      ),
      summary = ellmer::type_string("One-line summary")
    )
  )
}

triage_module <- function() {
  module(triage_signature()) |>
    with_decisions(
      urgent = decision_bool(),
      severity = decision_score(),
      category = decision_choice()
    )
}

triage_evidence <- function(input, field, field_type) {
  switch(
    field,
    urgent = list(probability = 0.6),
    severity = list(
      probabilities = list(`0` = 0.1, `1` = 0.3, `2` = 0.6),
      confidence = 0.8
    ),
    category = list(
      probabilities = list(billing = 0.55, technical = 0.45),
      confidence = 0.7
    ),
    summary = paste("Summary of", input)
  )
}

test_that("decision helpers build validated specifications", {
  spec <- decision_bool(threshold = 0.7, criteria = c(true = "Blocked"))
  expect_s3_class(spec, "dsprrr_decision_spec")
  expect_identical(spec$kind, "bool")
  expect_identical(spec$threshold, 0.7)
  expect_output(print(spec), "threshold")

  expect_identical(decision_score()$kind, "score")
  expect_identical(decision_choice(weights = c(a = 2))$weights, c(a = 2))
  expect_error(
    decision_bool(description = ""),
    class = "dsprrr_decision_config_error"
  )
})

test_that("with_decisions validates fields against the signature", {
  mod <- module(triage_signature())

  expect_error(
    with_decisions(mod, missing = decision_bool()),
    class = "dsprrr_decision_config_error"
  )
  expect_error(
    with_decisions(mod, summary = decision_bool()),
    class = "dsprrr_decision_config_error"
  )
  expect_error(
    with_decisions(mod, urgent = decision_choice()),
    class = "dsprrr_decision_config_error"
  )
  expect_error(
    with_decisions(mod, urgent = decision_bool(threshold = 2)),
    class = "dsprrr_decision_config_error"
  )
  expect_error(
    with_decisions(mod, severity = decision_score(cuts = c(1.5, 0.5))),
    class = "dsprrr_decision_config_error"
  )
  expect_error(
    with_decisions(mod, category = decision_choice(weights = c(other = 1))),
    class = "dsprrr_decision_config_error"
  )
  expect_error(
    with_decisions(
      mod,
      category = decision_choice(weights = c(billing = 0, technical = 0))
    ),
    class = "dsprrr_decision_config_error"
  )
  expect_error(
    with_decisions(mod, decision_bool()),
    class = "dsprrr_decision_config_error"
  )
  expect_error(
    with_decisions(mod, urgent = list(kind = "bool")),
    class = "dsprrr_decision_config_error"
  )
})

test_that("decision fields require a question", {
  mod <- module(signature("text -> flagged: boolean"))
  expect_error(
    with_decisions(mod, flagged = decision_bool()),
    class = "dsprrr_decision_config_error",
    regexp = "needs a question"
  )
  configured <- with_decisions(
    mod,
    flagged = decision_bool(description = "Should this be flagged?")
  )
  expect_identical(
    configured$config$decisions$flagged$description,
    "Should this be flagged?"
  )
})

test_that("with_decisions returns a configured copy with defaults", {
  mod <- module(triage_signature())
  configured <- with_decisions(
    mod,
    urgent = decision_bool(threshold = 0.7),
    severity = decision_score(),
    category = decision_choice(weights = c(technical = 2))
  )

  expect_null(mod$config$decisions)
  settings <- decision_settings(configured)
  expect_identical(settings$field, c("urgent", "severity", "category"))
  expect_identical(settings$kind, c("bool", "score", "choice"))
  expect_equal(settings$threshold, c(0.7, NA, NA))
  expect_equal(settings$cuts[[2]], c(0.5, 1.5))
  expect_equal(settings$weights[[3]], c(billing = 1, technical = 2))

  removed <- with_decisions(configured, urgent = NULL)
  expect_identical(decision_settings(removed)$field, c("severity", "category"))
  expect_null(
    with_decisions(removed, severity = NULL, category = NULL)$config$decisions
  )
})

test_that("with_decisions rejects non-Predict modules", {
  expect_error(
    with_decisions(list(), urgent = decision_bool()),
    class = "dsprrr_decision_config_error"
  )
})

test_that("request types carry evidence schemas but no numeric settings", {
  a <- with_decisions(
    module(triage_signature()),
    urgent = decision_bool(threshold = 0.2),
    category = decision_choice(weights = c(billing = 3))
  )
  b <- with_decisions(
    module(triage_signature()),
    urgent = decision_bool(threshold = 0.9),
    category = decision_choice()
  )
  type_a <- decision_request_type(a$signature@output_type, module_decisions(a))
  type_b <- decision_request_type(b$signature@output_type, module_decisions(b))

  expect_identical(
    names(type_a@properties),
    names(triage_signature()@output_type@properties)
  )
  expect_true(is_evidence_type(type_a@properties$urgent))
  expect_true(is_evidence_type(type_a@properties$category))
  expect_identical(
    names(type_a@properties$category@properties$probabilities@properties),
    c("billing", "technical")
  )
  expect_true(inherits(type_a@properties$severity, "ellmer::TypeEnum"))
  expect_identical(
    cache_output_schema(type_a),
    cache_output_schema(type_b)
  )
})

test_that("run() decodes decision evidence into native values", {
  mock <- new_decision_chat(triage_evidence)
  mod <- triage_module()

  result <- run(mod, text = "Checkout fails", .llm = mock$chat)
  expect_identical(result$urgent, TRUE)
  # Mean level index = 0.3 + 1.2 = 1.5, which reaches the second cut.
  expect_identical(result$severity, "blocking")
  expect_identical(result$category, "billing")
  expect_identical(result$summary, "Summary of Checkout fails")

  stricter <- with_decisions(
    mod,
    urgent = decision_bool(threshold = 0.7),
    severity = decision_score(cuts = c(0.5, 1.6)),
    category = decision_choice(weights = c(technical = 2))
  )
  result <- run(stricter, text = "Checkout fails", .llm = mock$chat)
  expect_identical(result$urgent, FALSE)
  expect_identical(result$severity, "disruptive")
  expect_identical(result$category, "technical")
})

test_that("structured results expose decision evidence", {
  mock <- new_decision_chat(triage_evidence)
  result <- run(
    triage_module(),
    text = "Checkout fails",
    .llm = mock$chat,
    .return_format = "structured"
  )
  evidence <- decision_evidence(result)

  expect_s3_class(evidence, "tbl_df")
  expect_identical(evidence$field, c("urgent", "severity", "category"))
  expect_equal(evidence$probability[[1]], 0.6)
  expect_equal(evidence$confidence[[1]], 0.1 / 0.5)
  expect_equal(evidence$score[[2]], 1.5)
  expect_identical(evidence$level[[2]], 2L)
  expect_equal(evidence$confidence[[2]], 0.8)
  expect_equal(
    evidence$probabilities[[3]],
    c(billing = 0.55, technical = 0.45)
  )
  expect_identical(evidence$value[[3]], "billing")
})

test_that("batch execution decodes every row", {
  mock <- new_decision_chat(function(input, field, field_type) {
    if (field == "urgent") {
      list(probability = if (input == "down") 0.9 else 0.1)
    } else {
      triage_evidence(input, field, field_type)
    }
  })
  data <- data.frame(text = c("down", "slow"))
  results <- run_dataset(
    triage_module(),
    data,
    .llm = mock$chat,
    .progress = FALSE,
    .return_format = "structured"
  )

  expect_identical(
    vapply(results$result, `[[`, logical(1), "urgent"),
    c(TRUE, FALSE)
  )
  evidence <- decision_evidence(results)
  expect_identical(evidence$row, rep(1:2, each = 3))
  expect_equal(
    evidence$probability[evidence$field == "urgent"],
    c(0.9, 0.1)
  )
})

test_that("module forward() decodes decision evidence", {
  mock <- new_decision_chat(triage_evidence)
  mod <- triage_module()
  out <- mod$forward(list(text = "x"), .llm = mock$chat)
  expect_identical(out$output[[1]]$urgent, TRUE)
  expect_identical(
    names(out$metadata[[1]]$decisions),
    c("urgent", "severity", "category")
  )
})

test_that("invalid evidence fails with a typed error", {
  mock <- new_decision_chat(function(input, field, field_type) {
    if (field == "severity") {
      list(probabilities = list(`0` = 0.5, `1` = 0.5), confidence = 0.5)
    } else {
      triage_evidence(input, field, field_type)
    }
  })
  expect_error(
    run(triage_module(), text = "x", .llm = mock$chat),
    class = "dsprrr_decision_evidence_error"
  )

  out_of_range <- new_decision_chat(function(input, field, field_type) {
    if (field == "urgent") {
      list(probability = 1.5)
    } else {
      triage_evidence(input, field, field_type)
    }
  })
  expect_error(
    run(triage_module(), text = "x", .llm = out_of_range$chat),
    class = "dsprrr_decision_evidence_error"
  )
})

test_that("choice weights break ties toward the raw winner", {
  decision <- list(
    kind = "choice",
    values = c("a", "b", "c"),
    weights = list(a = 1, b = 2, c = 1)
  )
  evidence <- list(
    probabilities = c(a = 0.5, b = 0.25, c = 0.25),
    confidence = 1
  )
  expect_identical(decision_decode_field(evidence, decision, "x")$value, "a")

  decision$weights <- list(a = 0, b = 0, c = 1)
  evidence$probabilities <- c(a = 0.5, b = 0.5, c = 0)
  expect_error(
    decision_decode_field(evidence, decision, "x"),
    class = "dsprrr_decision_evidence_error"
  )
})

test_that("concurrent batch backends reject decision modules", {
  mock <- new_decision_chat(triage_evidence)
  expect_error(
    run(
      triage_module(),
      text = c("a", "b"),
      .llm = mock$chat,
      .concurrency = concurrency_control(backend = "ellmer", max_active = 2L)
    ),
    class = "dsprrr_decision_unsupported_error"
  )
})

test_that("decision settings survive program artifacts", {
  mod <- with_decisions(
    module(triage_signature()),
    urgent = decision_bool(threshold = 0.73, criteria = c(true = "Blocked")),
    severity = decision_score(cuts = c(0.4, 1.2)),
    category = decision_choice(weights = c(technical = 1.5))
  )
  restored <- restore_module_config(program_artifact(mod))

  expect_identical(
    decision_settings(restored),
    decision_settings(mod)
  )
  expect_identical(
    restored$config$decisions$urgent$criteria,
    list(true = "Blocked")
  )
})

test_that("decision_evidence validates its input", {
  expect_error(decision_evidence(data.frame(x = 1)), "metadata")
  expect_error(decision_evidence(1), "Cannot find decision evidence")
  empty <- decision_evidence(list(output = list(), metadata = list()))
  expect_identical(nrow(empty), 0L)
})

test_that("optional decision outputs stay optional", {
  sig <- signature(
    inputs = list(input("text")),
    output_type = ellmer::type_object(
      flag = ellmer::type_boolean("Should this be flagged?", required = FALSE),
      label = ellmer::type_enum(c("a", "b"), "Which label?")
    )
  )
  mod <- with_decisions(
    module(sig),
    flag = decision_bool(),
    label = decision_choice()
  )
  request_type <- decision_request_type(
    mod$signature@output_type,
    module_decisions(mod)
  )
  expect_false(request_type@properties$flag@required)
  expect_true(request_type@properties$label@required)

  mock <- new_decision_chat(function(input, field, field_type) {
    if (field == "flag") {
      NULL
    } else {
      list(probabilities = list(a = 0.2, b = 0.8), confidence = 0.6)
    }
  })
  result <- run(
    mod,
    text = "x",
    .llm = mock$chat,
    .return_format = "structured"
  )
  expect_null(result$output$flag)
  expect_identical(result$output$label, "b")
  expect_identical(decision_evidence(result)$field, "label")

  required <- new_decision_chat(function(input, field, field_type) NULL)
  expect_error(
    run(
      with_decisions(module(sig), label = decision_choice()),
      text = "x",
      .llm = required$chat
    ),
    class = "dsprrr_decision_evidence_error"
  )
})

test_that("choice options may match type_object() argument names", {
  sig <- signature(
    inputs = list(input("text")),
    output_type = ellmer::type_object(
      pick = ellmer::type_enum(
        c(".description", ".required", "plain"),
        "Which option?"
      )
    )
  )
  mod <- with_decisions(module(sig), pick = decision_choice())
  request_type <- decision_request_type(
    mod$signature@output_type,
    module_decisions(mod)
  )
  expect_identical(
    names(request_type@properties$pick@properties$probabilities@properties),
    c(".description", ".required", "plain")
  )

  mock <- new_decision_chat(function(input, field, field_type) {
    list(
      probabilities = list(
        `.description` = 0.7,
        `.required` = 0.2,
        plain = 0.1
      ),
      confidence = 0.8
    )
  })
  expect_identical(run(mod, text = "x", .llm = mock$chat)$pick, ".description")
})
