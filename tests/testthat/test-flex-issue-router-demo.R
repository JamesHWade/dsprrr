test_that("executable Flex issue router preserves quality with fewer predictors", {
  skip_if_not_installed("callr")

  route_signature <- signature(
    "ticket -> queue: enum('database', 'security', 'payments', 'identity')",
    instructions = "Route each support ticket to its owning queue."
  )
  holdout <- data.frame(
    ticket = c(
      "[INC-PAY-4] capture queue is growing",
      "[INC-DB-17] replicas unavailable",
      "[INC-SEC-9] login spray detected",
      "Reset email arrives after its token expires",
      "Renewal created two invoices",
      "Read traffic fails after failover"
    ),
    queue = c(
      "payments",
      "database",
      "security",
      "identity",
      "payments",
      "database"
    )
  )

  lookup_incident <- local({
    catalog <- c(
      "INC-DB-17" = "database",
      "INC-SEC-9" = "security",
      "INC-PAY-4" = "payments"
    )

    function(ticket) {
      hit <- names(catalog)[vapply(
        names(catalog),
        grepl,
        logical(1),
        x = ticket,
        fixed = TRUE
      )]
      if (!length(hit)) {
        return(list(found = FALSE))
      }
      list(found = TRUE, queue = unname(catalog[[hit[[1L]]]]))
    }
  })

  route_chat <- function() {
    turns <- list()
    calls <- 0L

    last_turn <- function(role = c("assistant", "user"), ...) {
      role <- match.arg(role)
      matching <- Filter(function(turn) identical(turn@role, role), turns)
      if (!length(matching)) {
        return(NULL)
      }
      matching[[length(matching)]]
    }

    chat <- new_test_chat(
      model = "issue-router-test",
      get_turns = function(...) turns,
      last_turn = last_turn,
      chat_structured = function(prompt, type, ...) {
        prompt <- paste(as.character(prompt), collapse = "\n")
        queue <- if (grepl("INC-PAY-4", prompt, fixed = TRUE)) {
          "payments"
        } else if (grepl("INC-DB-17", prompt, fixed = TRUE)) {
          "database"
        } else if (grepl("INC-SEC-9", prompt, fixed = TRUE)) {
          "security"
        } else if (
          grepl(
            "Reset email arrives after its token expires",
            prompt,
            fixed = TRUE
          )
        ) {
          "identity"
        } else if (
          grepl("Renewal created two invoices", prompt, fixed = TRUE)
        ) {
          "payments"
        } else if (
          grepl("Read traffic fails after failover", prompt, fixed = TRUE)
        ) {
          "database"
        } else {
          stop("unexpected issue-router prompt")
        }

        calls <<- calls + 1L
        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(
              contents = list(ellmer::ContentText(prompt))
            ),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText(queue)),
              tokens = c(2L, 1L, 0L),
              cost = 0,
              duration = 0.001
            )
          )
        )
        list(queue = queue)
      }
    )
    chat$calls <- function() calls
    chat
  }

  # Fixed reviewed fixtures are the only source run without a sandbox here.
  baseline_source <- paste(
    "router <- Predict(\"$outer\", instructions = \"Choose the owning queue.\")",
    "forward <- function(ticket) router(ticket = ticket)",
    sep = "\n"
  )
  winner_source <- paste(
    "fallback <- Predict(\"$outer\", instructions = \"Route tickets without a known incident code.\")",
    "forward <- function(ticket) {",
    "  if (grepl(\"INC-\", ticket, fixed = TRUE)) {",
    "    hit <- lookup_incident(ticket = ticket)",
    "    if (isTRUE(hit$found)) return(Prediction(queue = hit$queue))",
    "  }",
    "  fallback(ticket = ticket)",
    "}",
    sep = "\n"
  )
  make_program <- function(module_src) {
    suppressWarnings(flex(
      route_signature,
      module_src = module_src,
      tools = list(lookup_incident = lookup_incident),
      interpreter_factory = r_code_runner,
      source_format = "r",
      require_sandbox = FALSE,
      max_predictor_calls = 1L,
      max_tool_calls = 1L
    ))
  }
  run_ticket <- function(program, ticket, chat) {
    run(
      program,
      ticket = ticket,
      .llm = chat,
      .return_format = "structured",
      .cache = FALSE
    )
  }

  winner <- make_program(winner_source)
  known_chat <- route_chat()
  known <- run_ticket(winner, holdout$ticket[[1L]], known_chat)

  expect_identical(known$output, list(queue = "payments"))
  expect_identical(known$metadata$predictor_calls, 0L)
  expect_identical(known$metadata$tool_calls, 1L)
  expect_identical(known$metadata$total_tokens, 0L)
  expect_identical(known_chat$calls(), 0L)

  ambiguous_chat <- route_chat()
  ambiguous <- run_ticket(winner, holdout$ticket[[4L]], ambiguous_chat)

  expect_identical(ambiguous$output, list(queue = "identity"))
  expect_identical(ambiguous$metadata$predictor_calls, 1L)
  expect_identical(ambiguous$metadata$tool_calls, 0L)
  expect_identical(ambiguous_chat$calls(), 1L)

  run_holdout <- function(program) {
    chat <- route_chat()
    results <- lapply(holdout$ticket, function(ticket) {
      run_ticket(program, ticket, chat)
    })
    list(results = results, chat = chat)
  }
  summarize_holdout <- function(execution) {
    outputs <- vapply(
      execution$results,
      function(result) result$output$queue,
      character(1)
    )
    metadata <- lapply(execution$results, `[[`, "metadata")
    list(
      accuracy = mean(outputs == holdout$queue),
      predictor_calls = sum(vapply(
        metadata,
        `[[`,
        integer(1),
        "predictor_calls"
      )),
      tool_calls = sum(vapply(metadata, `[[`, integer(1), "tool_calls")),
      chat_calls = execution$chat$calls()
    )
  }

  after <- summarize_holdout(run_holdout(winner))
  before <- summarize_holdout(run_holdout(make_program(baseline_source)))

  expect_identical(after$accuracy, 1)
  expect_identical(after$predictor_calls, 3L)
  expect_identical(after$tool_calls, 3L)
  expect_identical(after$chat_calls, 3L)
  expect_identical(before$accuracy, 1)
  expect_identical(before$predictor_calls, 6L)
  expect_identical(before$tool_calls, 0L)
  expect_identical(before$chat_calls, 6L)
})

test_that("GEPA selects the executable Flex issue-router hybrid", {
  skip_if_not_installed("callr")

  route_signature <- signature(
    "ticket -> queue: enum('database', 'security', 'payments', 'identity')",
    instructions = "Route each support ticket to its owning queue."
  )
  trainset <- data.frame(
    ticket = c(
      "[INC-SEC-9] login spray detected",
      "Reset email arrives after its token expires"
    ),
    queue = c("security", "identity")
  )
  valset <- data.frame(
    ticket = c(
      "[INC-PAY-4] capture queue is growing",
      "Renewal created two invoices"
    ),
    queue = c("payments", "payments")
  )
  expect_length(intersect(trainset$ticket, valset$ticket), 0L)

  lookup_incident <- local({
    catalog <- c(
      "INC-SEC-9" = "security",
      "INC-PAY-4" = "payments"
    )

    function(ticket) {
      hit <- names(catalog)[vapply(
        names(catalog),
        grepl,
        logical(1),
        x = ticket,
        fixed = TRUE
      )]
      if (!length(hit)) {
        return(list(found = FALSE))
      }
      list(found = TRUE, queue = unname(catalog[[hit[[1L]]]]))
    }
  })

  baseline_source <- paste(
    "router <- Predict(\"$outer\", instructions = \"Choose the owning queue.\")",
    "forward <- function(ticket) router(ticket = ticket)",
    sep = "\n"
  )
  winner_source <- paste(
    "fallback <- Predict(\"$outer\", instructions = \"Route tickets without a known incident code.\")",
    "forward <- function(ticket) {",
    "  if (grepl(\"INC-\", ticket, fixed = TRUE)) {",
    "    hit <- lookup_incident(ticket = ticket)",
    "    if (isTRUE(hit$found)) return(Prediction(queue = hit$queue))",
    "  }",
    "  fallback(ticket = ticket)",
    "}",
    sep = "\n"
  )

  issue_router_chat <- function(winner_source) {
    force(winner_source)
    turns <- list()
    proposal_calls <- 0L

    last_turn <- function(role = c("assistant", "user"), ...) {
      role <- match.arg(role)
      matching <- Filter(function(turn) identical(turn@role, role), turns)
      if (!length(matching)) {
        return(NULL)
      }
      matching[[length(matching)]]
    }

    chat <- new_test_chat(
      model = "issue-router-gepa-test",
      get_turns = function(...) turns,
      last_turn = last_turn,
      chat_structured = function(prompt, type, ...) {
        fields <- names(type@properties)
        if (identical(fields, "module_src")) {
          proposal_calls <<- proposal_calls + 1L
          return(list(module_src = winner_source))
        }
        if (!identical(fields, "queue")) {
          stop("unexpected issue-router structured output")
        }

        prompt <- paste(as.character(prompt), collapse = "\n")
        queue <- if (grepl("INC-SEC-9", prompt, fixed = TRUE)) {
          "security"
        } else if (grepl("INC-PAY-4", prompt, fixed = TRUE)) {
          "payments"
        } else if (
          grepl(
            "Reset email arrives after its token expires",
            prompt,
            fixed = TRUE
          )
        ) {
          "identity"
        } else if (
          grepl("Renewal created two invoices", prompt, fixed = TRUE)
        ) {
          "payments"
        } else {
          stop("unexpected issue-router prompt")
        }

        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(
              contents = list(ellmer::ContentText(prompt))
            ),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText(queue)),
              tokens = c(2L, 1L, 0L),
              cost = 0,
              duration = 0.001
            )
          )
        )
        list(queue = queue)
      }
    )
    chat$proposal_calls <- function() proposal_calls
    chat
  }

  make_program <- function(module_src) {
    # Both candidate sources are fixed, reviewed test fixtures.
    suppressWarnings(flex(
      route_signature,
      module_src = module_src,
      tools = list(lookup_incident = lookup_incident),
      interpreter_factory = r_code_runner,
      source_format = "r",
      require_sandbox = FALSE,
      max_predictor_calls = 1L,
      max_tool_calls = 1L
    ))
  }
  route_metric <- metric_with_trace(
    function(prediction, expected, program_trace) {
      correct <- identical(prediction$queue, expected$queue[[1L]])
      predictor_calls <- program_trace$metadata$predictor_calls
      if (is.null(predictor_calls)) {
        predictor_calls <- 0L
      }
      list(
        score = if (!correct) {
          0
        } else if (identical(as.integer(predictor_calls), 0L)) {
          1
        } else {
          0.5
        },
        feedback = if (correct) {
          "Correct route; prefer deterministic routing when available."
        } else {
          "Route does not match the owning queue."
        }
      )
    },
    field = "queue"
  )

  optimizer_chat <- issue_router_chat(winner_source)
  optimized <- compile(
    make_program(baseline_source),
    GEPA(
      metric = route_metric,
      population_size = 2L,
      generations = 1L,
      selection = "current_best",
      track_best_outputs = TRUE,
      verbose = FALSE
    ),
    trainset,
    valset = valset,
    .llm = optimizer_chat,
    control = dsprrr:::optimizer_control(num_threads = 1L)
  )
  optimizer_metadata <- gepa_test_metadata(optimized)

  validate <- function(program) {
    result <- evaluate(
      program,
      valset,
      route_metric,
      .llm = issue_router_chat(winner_source),
      .progress = FALSE
    )
    predicted <- vapply(result$predictions, `[[`, character(1), "queue")
    list(
      accuracy = mean(predicted == valset$queue),
      score = result$mean_score,
      predictor_calls = sum(vapply(
        result$metadata,
        `[[`,
        integer(1),
        "predictor_calls"
      )),
      tool_calls = sum(vapply(
        result$metadata,
        `[[`,
        integer(1),
        "tool_calls"
      ))
    )
  }
  before <- validate(make_program(baseline_source))
  after <- validate(optimized)

  expect_identical(optimizer_chat$proposal_calls(), 1L)
  expect_identical(optimized$module_src, winner_source)
  expect_identical(optimizer_metadata$best_scores, c(quality = 0.75))
  expect_setequal(
    unname(optimizer_metadata$val_aggregate_scores),
    c(0.5, 0.75)
  )
  expect_identical(
    unname(optimizer_metadata$discovery_eval_counts),
    c(2L, 2L)
  )
  expect_identical(optimizer_metadata$track_best_outputs, TRUE)
  expect_length(optimizer_metadata$best_outputs_valset, nrow(valset))
  expect_identical(
    optimizer_metadata$per_val_instance_best_candidates[["1"]],
    optimizer_metadata$best_candidate_id
  )
  expect_identical(
    optimizer_metadata$best_outputs_valset[["1"]][[1L]]$output,
    list(queue = "payments")
  )
  expect_identical(before$accuracy, 1)
  expect_identical(after$accuracy, 1)
  expect_identical(before$score, 0.5)
  expect_identical(after$score, 0.75)
  expect_identical(before$predictor_calls, 2L)
  expect_identical(after$predictor_calls, 1L)
  expect_identical(before$tool_calls, 0L)
  expect_identical(after$tool_calls, 1L)
})
