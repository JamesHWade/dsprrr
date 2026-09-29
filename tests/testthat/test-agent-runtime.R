# A chat in the style of a deputy Agent: its structured-request methods take a
# `run_context`, and `last_run()` returns the identifiers of its latest run.
AgentRuntimeTestChat <- R6::R6Class(
  "AgentRuntimeTestChat",
  inherit = TestChat,
  public = list(
    runs = 0L,
    run_contexts = list(),
    fixed_run_id = NULL,
    chat_structured = function(..., type, echo = "none", run_context = list()) {
      self$runs <- self$runs + 1L
      self$run_contexts <- c(self$run_contexts, list(run_context))
      list(answer = "4")
    },
    chat_structured_async = function(
      ...,
      type,
      echo = "none",
      run_context = list()
    ) {
      self$runs <- self$runs + 1L
      self$run_contexts <- c(self$run_contexts, list(run_context))
      promises::promise_resolve(list(answer = "4"))
    },
    last_run = function() {
      if (self$runs == 0L) {
        return(NULL)
      }
      list(
        run_id = self$fixed_run_id %||% paste0("run_", self$runs),
        agent_id = "agent_1",
        session_id = "session_1",
        stop_reason = "complete"
      )
    }
  ),
  lock_objects = FALSE
)

test_that("an agent runtime receives dsprrr's trace fields and returns a receipt", {
  chat <- AgentRuntimeTestChat$new()
  program <- module(signature("question -> answer"))

  result <- run(
    program,
    question = "What is 2 + 2?",
    .llm = chat,
    .trace_context = list(request_id = "abc"),
    .return_format = "structured"
  )

  context <- chat$run_contexts[[1]]$dsprrr
  expect_identical(context$trace_context, list(request_id = "abc"))
  expect_identical(context$program_artifact_id, program_artifact_id(program))
  expect_identical(
    result$metadata$agent_run,
    list(
      run_id = "run_1",
      agent_id = "agent_1",
      session_id = "session_1",
      stop_reason = "complete"
    )
  )
  trace <- program$state$traces[[length(program$state$traces)]]
  expect_identical(trace$metadata$agent_run$run_id, "run_1")
})

test_that("forward() records the receipt in its metadata and trace", {
  chat <- AgentRuntimeTestChat$new()
  program <- module(signature("question -> answer"))

  result <- program$forward(list(question = "What is 2 + 2?"), .llm = chat)

  expect_identical(result$metadata[[1]]$agent_run$run_id, "run_1")
  trace <- program$state$traces[[length(program$state$traces)]]
  expect_identical(trace$agent_run$run_id, "run_1")
})

test_that("each batch row records the run that answered it", {
  chat <- AgentRuntimeTestChat$new()
  program <- module(signature("question -> answer"))

  results <- run(
    program,
    question = c("2 + 2?", "3 + 3?"),
    .llm = chat,
    .return_format = "structured",
    .progress = FALSE
  )

  run_ids <- vapply(
    results,
    function(row) row$metadata$agent_run$run_id %||% NA_character_,
    character(1)
  )
  expect_false(anyNA(run_ids))
})

test_that("asynchronous calls pass the trace fields too", {
  chat <- AgentRuntimeTestChat$new()
  program <- module(signature("question -> answer"))

  program$run_async(
    question = "What is 2 + 2?",
    .llm = chat,
    .trace_context = list(request_id = "async")
  )

  expect_identical(
    chat$run_contexts[[1]]$dsprrr$trace_context,
    list(request_id = "async")
  )
})

test_that("a call that made no new run has no receipt", {
  chat <- AgentRuntimeTestChat$new()
  chat$fixed_run_id <- "run_before"
  chat$runs <- 1L
  program <- module(signature("question -> answer"))

  result <- run(
    program,
    question = "What is 2 + 2?",
    .llm = chat,
    .return_format = "structured"
  )

  expect_null(result$metadata$agent_run)
})

test_that("a plain ellmer Chat gets no run context and no receipt", {
  arguments <- NULL
  chat <- new_test_chat(
    chat_structured = function(...) {
      arguments <<- names(list(...))
      list(answer = "4")
    }
  )
  program <- module(signature("question -> answer"))

  result <- run(
    program,
    question = "What is 2 + 2?",
    .llm = chat,
    .trace_context = list(request_id = "abc"),
    .return_format = "structured"
  )

  expect_false("run_context" %in% arguments)
  expect_null(result$metadata$agent_run)
})
