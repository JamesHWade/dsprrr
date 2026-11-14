gepa_resume_test_chat <- function(model, system_prompt = NULL, params = NULL) {
  suppressWarnings(ellmer::chat_openai(
    model = model,
    api_key = "checkpoint-test-key",
    system_prompt = system_prompt,
    params = params
  ))
}

gepa_resume_test_program <- function(instructions = "Baseline") {
  module(signature(
    "question -> answer",
    instructions = instructions
  ))
}

gepa_resume_test_metrics <- function() {
  list(quality = function(prediction, expected) {
    as.numeric(identical(prediction, expected$answer))
  })
}

gepa_resume_test_eval_result <- function(program, dataset) {
  instructions <- program$signature@instructions
  prediction <- if (grepl("Mutated", instructions, fixed = TRUE)) {
    as.character(dataset$answer[[1L]])
  } else {
    "incorrect"
  }
  score <- as.numeric(identical(prediction, as.character(dataset$answer[[1L]])))
  EvalResult(
    examples = data.frame(
      score = score,
      error = NA_character_,
      predicted = prediction,
      feedback = NA_character_
    ),
    mean_score = score,
    n_evaluated = 1L,
    n_errors = 0L,
    provider_calls = 0L,
    metric_calls = 1L,
    provider_usage_unknown = FALSE,
    token_usage_unknown = FALSE
  )
}

gepa_resume_test_mocks <- function() {
  eval_calls <- new.env(parent = emptyenv())
  eval_calls$n <- 0L
  proposal_calls <- new.env(parent = emptyenv())
  proposal_calls$n <- 0L
  list(
    eval_calls = eval_calls,
    proposal_calls = proposal_calls,
    eval_program = function(program, dataset, metric, .llm = NULL, ...) {
      eval_calls$n <- eval_calls$n + 1L
      if (!is.null(.llm)) {
        turns <- .llm$get_turns()
        .llm$set_turns(c(
          turns,
          list(ellmer::UserTurn("evaluation"))
        ))
      }
      gepa_resume_test_eval_result(program, dataset)
    },
    propose = function(
      instruction,
      failed_examples,
      .llm,
      verbose,
      budget,
      unit_id
    ) {
      proposal_chat <- gepa_clone_reflection_chat(.llm)
      request <- optimizer_budgeted_provider_call(
        budget = budget,
        model = proposal_chat,
        stage = "gepa_reflection",
        unit_id = unit_id,
        call = function() {
          proposal_calls$n <- proposal_calls$n + 1L
          turns <- proposal_chat$get_turns()
          proposal_chat$set_turns(c(
            turns,
            list(ellmer::UserTurn("proposal"))
          ))
          "Mutated instructions"
        },
        success = function(value, condition) {
          is.null(condition) && is.character(value)
        }
      )
      if (isTRUE(request$started)) request$value else instruction
    }
  )
}

gepa_resume_test_compile <- function(
  path,
  control_args = list(),
  program = gepa_resume_test_program(),
  trainset = data.frame(
    question = c("q1", "q2"),
    answer = c("a1", "a2")
  ),
  valset = NULL,
  teleprompter = GEPA(
    metrics = gepa_resume_test_metrics(),
    population_size = 2L,
    generations = 1L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "current_best",
    seed = 21L,
    verbose = FALSE
  ),
  resume = FALSE,
  task_model = "gepa-task-resume",
  reflection_model = "gepa-reflection-resume",
  reflection_system_prompt = NULL,
  task_chat = NULL,
  reflection_chat = NULL
) {
  control <- do.call(
    optimizer_control,
    c(
      list(
        checkpoint_path = path,
        resume = resume,
        progress = FALSE
      ),
      control_args
    )
  )
  compile(
    program,
    teleprompter,
    trainset,
    valset = valset,
    .llm = task_chat %||% gepa_resume_test_chat(task_model),
    .reflection_llm = reflection_chat %||%
      gepa_resume_test_chat(
        reflection_model,
        system_prompt = reflection_system_prompt
      ),
    control = control
  )
}

gepa_resume_test_http_response <- function(req, output_field = "answer") {
  body <- list(
    id = "resp-test",
    object = "response",
    created_at = 1L,
    status = "completed",
    output = list(list(
      id = "msg-test",
      type = "message",
      status = "completed",
      role = "assistant",
      content = list(list(
        type = "output_text",
        annotations = list(),
        logprobs = list(),
        text = jsonlite::toJSON(
          stats::setNames(list("a1"), output_field),
          auto_unbox = TRUE
        )
      ))
    )),
    usage = list(input_tokens = 10L, output_tokens = 3L, total_tokens = 13L)
  )
  httr2::response(
    status_code = 200L,
    url = req$url,
    method = "POST",
    headers = list("content-type" = "application/json"),
    body = charToRaw(jsonlite::toJSON(
      body,
      auto_unbox = TRUE,
      null = "null"
    ))
  )
}

gepa_resume_test_disable_cache <- function() {
  previous <- get_cache_config()
  withr::defer(
    {
      do.call(configure_cache, previous)
      clear_cache()
    },
    envir = parent.frame()
  )
  configure_cache(enable = FALSE)
  clear_cache()
  invisible(NULL)
}

test_that("GEPA checkpoint writes retain nullable schema fields", {
  testthat::local_mocked_bindings(
    optimizer_checkpoint_write = function(...) invisible(NULL),
    .package = "dsprrr"
  )
  program <- gepa_resume_test_program()
  state <- gepa_checkpoint_initial_state(
    program,
    gepa_checkpoint_supported_program(program)
  )
  expect_no_error(gepa_checkpoint_write(list(), state))
  missing_best_key <- state
  missing_best_key$best_record_key <- NULL
  expect_error(
    gepa_checkpoint_write(list(), missing_best_key),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )
})

test_that("checkpointed GEPA completes and terminal resume makes no calls", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  task_chat <- gepa_resume_test_chat("gepa-task-resume")
  reflection_chat <- gepa_resume_test_chat("gepa-reflection-resume")

  result <- gepa_resume_test_compile(
    path,
    task_chat = task_chat,
    reflection_chat = reflection_chat
  )
  expect_identical(gepa_test_metadata(result)$resume_supported, TRUE)
  expect_identical(mocks$eval_calls$n, 4L)
  expect_identical(mocks$proposal_calls$n, 1L)
  checkpoint <- optimizer_checkpoint_read(path)
  expect_identical(checkpoint$progress$phase, "complete")
  expect_identical(checkpoint$budget$provider_calls, 1L)
  expect_identical(checkpoint$budget$metric_calls, 4L)
  expect_true(all(vapply(
    checkpoint$progress$search_state$all_records,
    function(record) isTRUE(record$complete),
    logical(1)
  )))

  resumed <- gepa_resume_test_compile(
    path,
    resume = TRUE,
    task_chat = task_chat,
    reflection_chat = reflection_chat
  )
  expect_identical(mocks$eval_calls$n, 4L)
  expect_identical(mocks$proposal_calls$n, 1L)
  expect_length(task_chat$get_turns(), 0L)
  expect_length(reflection_chat$get_turns(), 0L)
  expect_identical(
    gepa_test_metadata(resumed)$best_scores,
    gepa_test_metadata(result)$best_scores
  )
})

test_that("checkpointed GEPA isolates a configured default Chat during real eval", {
  gepa_resume_test_disable_cache()
  proposal_calls <- new.env(parent = emptyenv())
  proposal_calls$n <- 0L
  task_requests <- new.env(parent = emptyenv())
  task_requests$n <- 0L
  task_requests$input_lengths <- integer()
  task_requests$models <- character()
  testthat::local_mocked_bindings(
    gepa_propose_component_instructions = function(
      instruction,
      failed_examples,
      .llm,
      verbose,
      budget,
      unit_id
    ) {
      proposal_chat <- gepa_clone_reflection_chat(.llm)
      request <- optimizer_budgeted_provider_call(
        budget = budget,
        model = proposal_chat,
        stage = "gepa_reflection",
        unit_id = unit_id,
        call = function() {
          proposal_calls$n <- proposal_calls$n + 1L
          "Mutated instructions"
        },
        success = function(value, condition) {
          is.null(condition) && is.character(value)
        }
      )
      if (isTRUE(request$started)) request$value else instruction
    },
    .package = "dsprrr"
  )
  httr2::local_mocked_responses(function(req) {
    task_requests$n <- task_requests$n + 1L
    task_requests$input_lengths <- c(
      task_requests$input_lengths,
      length(req$body$data$input)
    )
    task_requests$models <- c(task_requests$models, req$body$data$model)
    gepa_resume_test_http_response(req)
  })
  default_chat <- gepa_resume_test_chat("gepa-default-task")
  withr::local_options(dsprrr.default_chat = default_chat)
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  teleprompter <- GEPA(
    metrics = gepa_resume_test_metrics(),
    population_size = 2L,
    generations = 1L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "current_best",
    seed = 21L,
    verbose = FALSE
  )
  run_checkpoint <- function(resume, max_metric_calls) {
    compile(
      gepa_resume_test_program(),
      teleprompter,
      data.frame(
        question = c("q1", "q2"),
        answer = c("a1", "a2")
      ),
      .llm = NULL,
      .reflection_llm = gepa_resume_test_chat("gepa-reflection-resume"),
      control = optimizer_control(
        checkpoint_path = path,
        resume = resume,
        max_metric_calls = max_metric_calls,
        progress = FALSE
      )
    )
  }

  partial <- run_checkpoint(resume = FALSE, max_metric_calls = 1L)

  expect_true(gepa_test_metadata(partial)$partial)
  expect_identical(task_requests$n, 1L)
  expect_length(default_chat$get_turns(), 0L)

  resumed <- run_checkpoint(resume = TRUE, max_metric_calls = 100L)

  expect_false(gepa_test_metadata(resumed)$partial)
  expect_identical(task_requests$n, 4L)
  expect_identical(task_requests$input_lengths, rep(1L, 4L))
  expect_identical(task_requests$models, rep("gepa-default-task", 4L))
  expect_length(default_chat$get_turns(), 0L)
})

test_that("checkpointed GEPA isolates attached Chats for every evaluation row", {
  gepa_resume_test_disable_cache()
  mocks <- gepa_resume_test_mocks()
  task_requests <- new.env(parent = emptyenv())
  task_requests$n <- 0L
  task_requests$input_lengths <- integer()
  task_requests$models <- character()
  testthat::local_mocked_bindings(
    get_default_chat = function(create = TRUE) NULL,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  httr2::local_mocked_responses(function(req) {
    task_requests$n <- task_requests$n + 1L
    task_requests$input_lengths <- c(
      task_requests$input_lengths,
      length(req$body$data$input)
    )
    task_requests$models <- c(task_requests$models, req$body$data$model)
    gepa_resume_test_http_response(req)
  })
  attached_chat <- gepa_resume_test_chat("attached-task-chat")
  program <- gepa_resume_test_program()
  program$chat <- attached_chat
  teleprompter <- GEPA(
    metrics = gepa_resume_test_metrics(),
    population_size = 2L,
    generations = 1L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "current_best",
    seed = 21L,
    verbose = FALSE
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  dataset <- data.frame(
    question = c("q1", "q2"),
    answer = c("a1", "a2")
  )
  run_checkpoint <- function(resume, max_metric_calls) {
    compile(
      program,
      teleprompter,
      dataset,
      .llm = NULL,
      .reflection_llm = gepa_resume_test_chat("gepa-reflection-attached"),
      control = optimizer_control(
        checkpoint_path = path,
        resume = resume,
        max_metric_calls = max_metric_calls,
        progress = FALSE
      )
    )
  }

  partial <- run_checkpoint(resume = FALSE, max_metric_calls = 1L)

  expect_true(gepa_test_metadata(partial)$partial)
  expect_identical(task_requests$n, 1L)
  expect_length(attached_chat$get_turns(), 0L)

  resumed <- run_checkpoint(resume = TRUE, max_metric_calls = 100L)

  expect_false(gepa_test_metadata(resumed)$partial)
  expect_identical(task_requests$n, 4L)
  expect_identical(task_requests$input_lengths, rep(1L, 4L))
  expect_identical(task_requests$models, rep("attached-task-chat", 4L))
  expect_length(attached_chat$get_turns(), 0L)
})

test_that("checkpointed GEPA isolates mixed pipeline Chats per row", {
  gepa_resume_test_disable_cache()
  mocks <- gepa_resume_test_mocks()
  task_requests <- new.env(parent = emptyenv())
  task_requests$n <- 0L
  task_requests$input_lengths <- integer()
  task_requests$models <- character()
  testthat::local_mocked_bindings(
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  httr2::local_mocked_responses(function(req) {
    task_requests$n <- task_requests$n + 1L
    task_requests$input_lengths <- c(
      task_requests$input_lengths,
      length(req$body$data$input)
    )
    task_requests$models <- c(task_requests$models, req$body$data$model)
    output_field <- if (task_requests$n %% 2L == 1L) "middle" else "answer"
    gepa_resume_test_http_response(req, output_field = output_field)
  })
  attached_chat <- gepa_resume_test_chat("attached-pipeline-task")
  default_chat <- gepa_resume_test_chat("default-pipeline-task")
  withr::local_options(dsprrr.default_chat = default_chat)
  first <- module(signature("question -> middle"))
  first$chat <- attached_chat
  second <- module(signature("middle -> answer"))
  program <- pipeline(first, second)
  teleprompter <- GEPA(
    metric = function(prediction, expected) 1,
    population_size = 2L,
    generations = 1L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "current_best",
    seed = 21L,
    verbose = FALSE
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  dataset <- data.frame(
    question = c("q1", "q2"),
    answer = c("a1", "a2")
  )
  run_checkpoint <- function(resume, max_metric_calls) {
    compile(
      program,
      teleprompter,
      dataset,
      .llm = NULL,
      .reflection_llm = gepa_resume_test_chat("gepa-reflection-mixed"),
      control = optimizer_control(
        checkpoint_path = path,
        resume = resume,
        max_metric_calls = max_metric_calls,
        progress = FALSE
      )
    )
  }

  partial <- run_checkpoint(resume = FALSE, max_metric_calls = 1L)

  expect_true(gepa_test_metadata(partial)$partial)
  expect_identical(task_requests$n, 2L)
  expect_length(attached_chat$get_turns(), 0L)
  expect_length(default_chat$get_turns(), 0L)

  resumed <- run_checkpoint(resume = TRUE, max_metric_calls = 100L)

  expect_false(gepa_test_metadata(resumed)$partial)
  expect_identical(task_requests$n, 8L)
  expect_identical(task_requests$input_lengths, rep(1L, 8L))
  expect_identical(
    task_requests$models,
    rep(c("attached-pipeline-task", "default-pipeline-task"), 4L)
  )
  expect_length(attached_chat$get_turns(), 0L)
  expect_length(default_chat$get_turns(), 0L)
})

test_that("checkpointed GEPA preserves attached Chat precedence", {
  mocks <- gepa_resume_test_mocks()
  defaults <- new.env(parent = emptyenv())
  defaults$chat <- NULL
  observed_models <- new.env(parent = emptyenv())
  observed_models$by_eval <- list()
  testthat::local_mocked_bindings(
    get_default_chat = function(create = TRUE) defaults$chat,
    eval_program = function(program, dataset, metric, .llm = NULL, ...) {
      graph <- gepa_checkpoint_leaf_modules(program)
      observed_models$by_eval[[length(observed_models$by_eval) + 1L]] <-
        vapply(
          graph$module,
          function(module) gepa_chat_model_name(module$chat),
          character(1)
        )
      n <- nrow(dataset)
      EvalResult(
        examples = data.frame(
          score = rep(1, n),
          error = rep(NA_character_, n),
          predicted = rep("output", n),
          feedback = rep(NA_character_, n)
        ),
        mean_score = 1,
        n_evaluated = as.integer(n),
        n_errors = 0L,
        provider_calls = 0L,
        metric_calls = as.integer(n),
        provider_usage_unknown = FALSE,
        token_usage_unknown = FALSE
      )
    },
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  attached_chat <- gepa_resume_test_chat("attached-task-chat")
  configured_default <- gepa_resume_test_chat("configured-default-chat")
  program <- gepa_resume_test_program()
  program$chat <- attached_chat
  teleprompter <- GEPA(
    metrics = gepa_resume_test_metrics(),
    population_size = 2L,
    generations = 1L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "current_best",
    seed = 21L,
    verbose = FALSE
  )
  run_checkpoint <- function(path) {
    compile(
      program,
      teleprompter,
      data.frame(question = "q1", answer = "a1"),
      .llm = NULL,
      .reflection_llm = gepa_resume_test_chat("gepa-reflection-attached"),
      control = optimizer_control(
        checkpoint_path = path,
        progress = FALSE
      )
    )
  }

  absent_default_path <- withr::local_tempfile(fileext = ".rds")
  unlink(absent_default_path)
  no_default_result <- run_checkpoint(absent_default_path)
  expect_false(gepa_test_metadata(no_default_result)$partial)
  expect_true(all(vapply(
    observed_models$by_eval,
    function(models) all(models == "attached-task-chat"),
    logical(1)
  )))

  defaults$chat <- configured_default
  precedence_path <- withr::local_tempfile(fileext = ".rds")
  unlink(precedence_path)
  precedence_result <- run_checkpoint(precedence_path)
  expect_false(gepa_test_metadata(precedence_result)$partial)
  expect_true(all(vapply(
    observed_models$by_eval,
    function(models) all(models == "attached-task-chat"),
    logical(1)
  )))

  first <- gepa_resume_test_program("First")
  second <- module(signature("question -> answer", instructions = "Second"))
  first$chat <- attached_chat
  mixed <- pipeline(first, second)
  mixed_with_default <- gepa_checkpoint_attach_default_chats(mixed)
  mixed_leaves <- gepa_checkpoint_leaf_modules(mixed_with_default)
  mixed_models <- vapply(
    mixed_leaves$module,
    function(module) gepa_chat_model_name(module$chat),
    character(1)
  )
  expect_identical(
    mixed_models,
    c("attached-task-chat", "configured-default-chat")
  )
  expect_identical(mixed_leaves$module[[1L]]$chat, attached_chat)
  expect_length(attached_chat$get_turns(), 0L)
})

test_that("checkpointed reflection evidence respects metric_threshold", {
  capture <- new.env(parent = emptyenv())
  capture$examples <- list()
  testthat::local_mocked_bindings(
    eval_program = function(program, dataset, metric, .llm = NULL, ...) {
      n <- nrow(dataset)
      EvalResult(
        examples = data.frame(
          score = rep(0.75, n),
          error = rep(NA_character_, n),
          predicted = rep("prediction", n),
          feedback = rep(NA_character_, n)
        ),
        mean_score = 0.75,
        n_evaluated = as.integer(n),
        n_errors = 0L,
        provider_calls = 0L,
        metric_calls = as.integer(n),
        provider_usage_unknown = FALSE,
        token_usage_unknown = FALSE
      )
    },
    gepa_propose_component_instructions = function(
      instruction,
      failed_examples,
      .llm,
      verbose,
      budget,
      unit_id
    ) {
      capture$examples[[length(capture$examples) + 1L]] <- failed_examples
      proposal_chat <- gepa_clone_reflection_chat(.llm)
      request <- optimizer_budgeted_provider_call(
        budget = budget,
        model = proposal_chat,
        stage = "gepa_reflection",
        unit_id = unit_id,
        call = function() "Mutated instructions",
        success = function(value, condition) {
          is.null(condition) && is.character(value)
        }
      )
      if (isTRUE(request$started)) request$value else instruction
    },
    .package = "dsprrr"
  )
  teleprompter <- GEPA(
    metrics = gepa_resume_test_metrics(),
    population_size = 2L,
    generations = 2L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "current_best",
    metric_threshold = 0.5,
    seed = 21L,
    verbose = FALSE
  )
  run_compile <- function(control) {
    compile(
      gepa_resume_test_program(),
      teleprompter,
      data.frame(question = "q1", answer = "a1"),
      .llm = gepa_resume_test_chat("gepa-task-threshold"),
      .reflection_llm = gepa_resume_test_chat("gepa-reflection-threshold"),
      control = control
    )
  }

  checkpoint_path <- withr::local_tempfile(fileext = ".rds")
  unlink(checkpoint_path)
  run_compile(optimizer_control(
    checkpoint_path = checkpoint_path,
    progress = FALSE
  ))
  checkpoint_examples <- capture$examples
  capture$examples <- list()

  run_compile(optimizer_control(progress = FALSE))
  ordinary_examples <- capture$examples

  expect_length(checkpoint_examples, 3L)
  expect_identical(checkpoint_examples, ordinary_examples)
  expect_true(all(
    lengths(checkpoint_examples) == 0L
  ))
})

test_that("checkpointed GEPA resumes a partial second metric", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  full_path <- withr::local_tempfile(fileext = ".rds")
  unlink(c(path, full_path))
  teleprompter <- GEPA(
    metrics = list(
      quality = function(prediction, expected) {
        as.numeric(identical(prediction, expected$answer))
      },
      safety = function(prediction, expected) {
        as.numeric(nzchar(prediction))
      }
    ),
    population_size = 2L,
    generations = 1L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "pareto",
    seed = 44L,
    verbose = FALSE
  )
  trainset <- data.frame(
    question = c("train1", "train2"),
    answer = c("a1", "a2")
  )
  valset <- data.frame(
    question = c("valid1", "valid2"),
    answer = c("a1", "a2")
  )

  partial <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 3L),
    teleprompter = teleprompter,
    trainset = trainset,
    valset = valset
  )
  expect_true(gepa_test_metadata(partial)$partial)
  expect_identical(mocks$eval_calls$n, 3L)
  checkpoint <- optimizer_checkpoint_read(path)
  state <- checkpoint$progress$search_state
  expect_identical(state$active_evaluation$role, "metric")
  expect_identical(state$active_evaluation$metric_index, 2L)
  expect_length(state$active_evaluation$rows, 1L)
  expect_identical(state$active_candidate$completed_metrics, 1L)
  expect_identical(
    lengths(state$active_candidate$metric_rows),
    c(2L, 0L)
  )

  resumed <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 100L),
    teleprompter = teleprompter,
    trainset = trainset,
    valset = valset,
    resume = TRUE
  )
  expect_identical(mocks$eval_calls$n, 12L)
  expect_identical(mocks$proposal_calls$n, 1L)
  resumed_checkpoint <- optimizer_checkpoint_read(path)

  full <- gepa_resume_test_compile(
    full_path,
    teleprompter = teleprompter,
    trainset = trainset,
    valset = valset
  )
  full_checkpoint <- optimizer_checkpoint_read(full_path)
  expect_identical(
    names(resumed_checkpoint$progress$search_state$all_records),
    names(full_checkpoint$progress$search_state$all_records)
  )
  expect_identical(
    lapply(
      resumed_checkpoint$progress$search_state$all_records,
      function(record) {
        list(
          candidate_id = record$candidate_id,
          candidate = record$candidate,
          scores = record$scores,
          metric_rows = record$metric_rows,
          discovery_rows = record$discovery_rows
        )
      }
    ),
    lapply(
      full_checkpoint$progress$search_state$all_records,
      function(record) {
        list(
          candidate_id = record$candidate_id,
          candidate = record$candidate,
          scores = record$scores,
          metric_rows = record$metric_rows,
          discovery_rows = record$discovery_rows
        )
      }
    )
  )
  expect_identical(
    gepa_test_metadata(resumed)$best_scores,
    gepa_test_metadata(full)$best_scores
  )
  expect_identical(
    resumed_checkpoint$budget[c(
      "attempts",
      "successes",
      "total_errors",
      "metric_calls",
      "provider_calls",
      "trials"
    )],
    full_checkpoint$budget[c(
      "attempts",
      "successes",
      "total_errors",
      "metric_calls",
      "provider_calls",
      "trials"
    )]
  )
})

test_that("checkpointed GEPA resumes partial discovery rows", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  trainset <- data.frame(
    question = c("train1", "train2"),
    answer = c("a1", "a2")
  )
  valset <- data.frame(
    question = c("valid1", "valid2"),
    answer = c("a1", "a2")
  )

  partial <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 3L),
    trainset = trainset,
    valset = valset
  )
  expect_true(gepa_test_metadata(partial)$partial)
  expect_identical(mocks$eval_calls$n, 3L)
  state <- optimizer_checkpoint_read(path)$progress$search_state
  expect_identical(state$active_candidate$completed_metrics, 1L)
  expect_identical(state$active_evaluation$role, "discovery")
  expect_identical(state$active_evaluation$rows[[1L]]$row_index, 1L)

  resumed <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 100L),
    trainset = trainset,
    valset = valset,
    resume = TRUE
  )
  expect_false(gepa_test_metadata(resumed)$partial)
  expect_identical(mocks$eval_calls$n, 8L)
  expect_identical(mocks$proposal_calls$n, 1L)
  expect_identical(
    optimizer_checkpoint_read(path)$budget$metric_calls,
    8L
  )
})

test_that("checkpointed GEPA resumes an already committed proposal", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  original_writer <- optimizer_checkpoint_write
  interrupted <- new.env(parent = emptyenv())
  interrupted$done <- FALSE
  testthat::local_mocked_bindings(
    optimizer_checkpoint_write = function(
      context,
      phase,
      search_state,
      ...
    ) {
      result <- original_writer(context, phase, search_state, ...)
      if (
        !interrupted$done &&
          identical(phase, "evaluating") &&
          length(search_state$population) == 2L &&
          length(search_state$all_records) == 0L &&
          is.null(search_state$active_proposal)
      ) {
        interrupted$done <- TRUE
        stop("injected interruption after proposal commit")
      }
      result
    },
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)

  expect_error(
    gepa_resume_test_compile(path),
    "injected interruption after proposal commit"
  )
  expect_identical(mocks$proposal_calls$n, 1L)
  committed <- optimizer_checkpoint_read(path)
  expect_identical(committed$progress$phase, "evaluating")
  expect_length(committed$progress$search_state$population, 2L)
  expect_null(committed$progress$search_state$active_proposal)

  completed <- gepa_resume_test_compile(path, resume = TRUE)
  expect_identical(mocks$proposal_calls$n, 1L)
  expect_identical(mocks$eval_calls$n, 4L)
  expect_false(gepa_test_metadata(completed)$partial)
})

test_that("checkpointed GEPA retains planned generation decisions and RNG", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  original_writer <- optimizer_checkpoint_write
  interrupted <- new.env(parent = emptyenv())
  interrupted$done <- FALSE
  testthat::local_mocked_bindings(
    optimizer_checkpoint_write = function(
      context,
      phase,
      search_state,
      ...
    ) {
      result <- original_writer(context, phase, search_state, ...)
      plan <- search_state$active_proposal
      if (
        !interrupted$done &&
          identical(phase, "proposing") &&
          !is.null(plan) &&
          isTRUE(plan$mutation)
      ) {
        interrupted$done <- TRUE
        stop("injected interruption after generation plan commit")
      }
      result
    },
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  full_path <- withr::local_tempfile(fileext = ".rds")
  unlink(c(path, full_path))
  teleprompter <- GEPA(
    metric = function(prediction, expected) {
      as.numeric(identical(prediction, expected$answer))
    },
    population_size = 2L,
    generations = 2L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "current_best",
    seed = 86L,
    verbose = FALSE
  )
  dataset <- data.frame(
    question = c("q1", "q2"),
    answer = c("a1", "a2")
  )
  withr::local_seed(911L)
  caller_rng <- get(".Random.seed", envir = globalenv())

  expect_error(
    gepa_resume_test_compile(
      path,
      teleprompter = teleprompter,
      trainset = dataset
    ),
    "injected interruption after generation plan commit"
  )
  expect_identical(get(".Random.seed", envir = globalenv()), caller_rng)
  planned <- optimizer_checkpoint_read(path)
  plan <- planned$progress$search_state$active_proposal
  expect_identical(planned$progress$phase, "proposing")
  expect_identical(plan$kind, "generation")
  expect_true(plan$mutation)
  expect_identical(mocks$proposal_calls$n, 1L)

  resumed <- gepa_resume_test_compile(
    path,
    teleprompter = teleprompter,
    trainset = dataset,
    resume = TRUE
  )
  resumed_checkpoint <- optimizer_checkpoint_read(path)
  expect_identical(get(".Random.seed", envir = globalenv()), caller_rng)
  expect_identical(mocks$proposal_calls$n, 3L)
  expect_identical(mocks$eval_calls$n, 8L)

  full_mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = full_mocks$eval_program,
    gepa_propose_component_instructions = full_mocks$propose,
    .package = "dsprrr"
  )
  full <- gepa_resume_test_compile(
    full_path,
    teleprompter = teleprompter,
    trainset = dataset
  )
  full_checkpoint <- optimizer_checkpoint_read(full_path)
  expect_identical(full_mocks$proposal_calls$n, 3L)
  expect_identical(full_mocks$eval_calls$n, 8L)
  expect_identical(
    resumed_checkpoint$progress$search_state$all_records,
    full_checkpoint$progress$search_state$all_records
  )
  expect_identical(
    resumed_checkpoint$rng,
    full_checkpoint$rng
  )
  expect_identical(
    gepa_test_metadata(resumed)$best_scores,
    gepa_test_metadata(full)$best_scores
  )
  expect_identical(
    resumed_checkpoint$budget[c(
      "attempts",
      "successes",
      "total_errors",
      "metric_calls",
      "provider_calls",
      "trials"
    )],
    full_checkpoint$budget[c(
      "attempts",
      "successes",
      "total_errors",
      "metric_calls",
      "provider_calls",
      "trials"
    )]
  )
})

test_that("checkpointed GEPA resumes after a durable validation row", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)

  partial <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 1L)
  )
  expect_true(gepa_test_metadata(partial)$partial)
  expect_identical(mocks$eval_calls$n, 1L)
  before <- optimizer_checkpoint_read(path)
  before_state <- before$progress$search_state
  expect_identical(before_state$phase, "evaluating")
  expect_length(before_state$active_evaluation$rows, 1L)
  expect_identical(before_state$active_evaluation$rows[[1L]]$row_index, 1L)
  stopped <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 1L),
    resume = TRUE
  )
  expect_true(gepa_test_metadata(stopped)$partial)
  expect_identical(mocks$eval_calls$n, 1L)
  expect_identical(mocks$proposal_calls$n, 1L)
  lowered <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 0L),
    resume = TRUE
  )
  expect_true(gepa_test_metadata(lowered)$partial)
  expect_identical(mocks$eval_calls$n, 1L)
  expect_identical(mocks$proposal_calls$n, 1L)

  completed <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 100L),
    resume = TRUE
  )
  expect_identical(mocks$eval_calls$n, 4L)
  expect_identical(mocks$proposal_calls$n, 1L)
  expect_false(gepa_test_metadata(completed)$partial)
  expect_identical(
    optimizer_checkpoint_read(path)$budget$metric_calls,
    4L
  )
})

test_that("checkpointed GEPA preserves NULL predictions across resume", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = function(program, dataset, metric, .llm = NULL, ...) {
      n <- nrow(dataset)
      EvalResult(
        examples = data.frame(
          score = rep(1, n),
          error = rep(NA_character_, n),
          predicted = I(rep(list(NULL), n)),
          feedback = rep(NA_character_, n)
        ),
        mean_score = 1,
        n_evaluated = as.integer(n),
        n_errors = 0L,
        provider_calls = 0L,
        metric_calls = as.integer(n),
        provider_usage_unknown = FALSE,
        token_usage_unknown = FALSE
      )
    },
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  dataset <- data.frame(question = c("q1", "q2"), answer = c("a1", "a2"))
  partial <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 1L),
    trainset = dataset
  )
  expect_true(gepa_test_metadata(partial)$partial)
  checkpoint <- optimizer_checkpoint_read(path)
  saved_row <- checkpoint$progress$search_state$active_evaluation$rows[[1L]]
  expect_true("predicted" %in% names(saved_row))
  expect_null(saved_row$predicted)

  completed <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 100L),
    trainset = dataset,
    resume = TRUE
  )
  expect_false(gepa_test_metadata(completed)$partial)
  state <- optimizer_checkpoint_read(path)$progress$search_state
  expect_true(all(vapply(
    state$all_records,
    function(record) {
      rows <- record$metric_rows[[1L]]
      length(rows) == nrow(dataset) &&
        all(vapply(
          rows,
          function(row) {
            "predicted" %in% names(row) && is.null(row$predicted)
          },
          logical(1)
        ))
    },
    logical(1)
  )))
})

test_that("checkpointed GEPA rejects unsupported selectors before calls", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  teleprompter <- GEPA(
    metrics = gepa_resume_test_metrics(),
    population_size = 2L,
    generations = 1L,
    component_selector = "all",
    verbose = FALSE
  )

  expect_error(
    gepa_resume_test_compile(path, teleprompter = teleprompter),
    class = "dsprrr_optimizer_checkpoint_unsupported_optimizer"
  )
  expect_identical(mocks$eval_calls$n, 0L)
  expect_identical(mocks$proposal_calls$n, 0L)
  expect_false(file.exists(path))
})

test_that("checkpointed GEPA validates identity and closed state before work", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  dataset <- data.frame(
    question = c("q1", "q2"),
    answer = c("a1", "a2")
  )
  gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 1L),
    trainset = dataset
  )
  call_counts <- c(mocks$eval_calls$n, mocks$proposal_calls$n)

  expect_error(
    gepa_resume_test_compile(
      path,
      control_args = list(max_metric_calls = 100L),
      trainset = transform(dataset, answer = c("changed", "a2")),
      resume = TRUE
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible"
  )
  expect_error(
    gepa_resume_test_compile(
      path,
      control_args = list(max_metric_calls = 100L),
      trainset = dataset,
      task_model = "changed-task-model",
      resume = TRUE
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible"
  )
  expect_error(
    gepa_resume_test_compile(
      path,
      control_args = list(max_metric_calls = 100L),
      trainset = dataset,
      reflection_system_prompt = "Changed reflection semantics",
      resume = TRUE
    ),
    class = "dsprrr_optimizer_checkpoint_incompatible"
  )
  expect_identical(c(mocks$eval_calls$n, mocks$proposal_calls$n), call_counts)

  checkpoint <- optimizer_checkpoint_read(path)
  checkpoint$progress$search_state$candidate_index <- 0L
  checkpoint$integrity <- optimizer_checkpoint_integrity(checkpoint)
  saveRDS(checkpoint, path, version = 3L)
  Sys.chmod(path, mode = "0600")
  expect_error(
    gepa_resume_test_compile(
      path,
      control_args = list(max_metric_calls = 100L),
      trainset = dataset,
      resume = TRUE
    ),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )
  expect_identical(c(mocks$eval_calls$n, mocks$proposal_calls$n), call_counts)
})

test_that("checkpointed GEPA rejects tampered scores and winners before work", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  path <- withr::local_tempfile(fileext = ".rds")
  unlink(path)
  gepa_resume_test_compile(path)
  checkpoint <- optimizer_checkpoint_read(path)
  state <- checkpoint$progress$search_state
  record_keys <- names(state$all_records)
  expect_length(record_keys, 2L)
  expect_false(identical(state$best_record_key, record_keys[[1L]]))
  call_counts <- c(mocks$eval_calls$n, mocks$proposal_calls$n)

  bad_score <- checkpoint
  bad_score$progress$search_state$all_records[[record_keys[[1L]]]]$scores[[
    1L
  ]] <- 999
  bad_score$integrity <- optimizer_checkpoint_integrity(bad_score)
  saveRDS(bad_score, path, version = 3L)
  Sys.chmod(path, mode = "0600")
  expect_error(
    gepa_resume_test_compile(path, resume = TRUE),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )

  bad_discovery_count <- checkpoint
  bad_discovery_count$progress$search_state$all_records[[record_keys[[
    1L
  ]]]]$discovery_metric_calls <-
    bad_discovery_count$progress$search_state$all_records[[record_keys[[
      1L
    ]]]]$discovery_metric_calls +
    1L
  bad_discovery_count$integrity <- optimizer_checkpoint_integrity(
    bad_discovery_count
  )
  saveRDS(bad_discovery_count, path, version = 3L)
  Sys.chmod(path, mode = "0600")
  expect_error(
    gepa_resume_test_compile(path, resume = TRUE),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )

  bad_summary <- checkpoint
  bad_summary$progress$search_state$all_generations[["1"]]$n_selectable <- 99L
  bad_summary$integrity <- optimizer_checkpoint_integrity(bad_summary)
  saveRDS(bad_summary, path, version = 3L)
  Sys.chmod(path, mode = "0600")
  expect_error(
    gepa_resume_test_compile(path, resume = TRUE),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )

  removed_summaries <- checkpoint
  removed_summaries$progress$search_state$all_generations <- list()
  removed_summaries$integrity <- optimizer_checkpoint_integrity(
    removed_summaries
  )
  saveRDS(removed_summaries, path, version = 3L)
  Sys.chmod(path, mode = "0600")
  expect_error(
    gepa_resume_test_compile(path, resume = TRUE),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )

  bad_winner <- checkpoint
  bad_winner$progress$search_state$best_record_key <- record_keys[[1L]]
  bad_winner$integrity <- optimizer_checkpoint_integrity(bad_winner)
  saveRDS(bad_winner, path, version = 3L)
  Sys.chmod(path, mode = "0600")
  expect_error(
    gepa_resume_test_compile(path, resume = TRUE),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )

  missing_field <- checkpoint
  missing_field$progress$search_state$best_record_key <- NULL
  missing_field$integrity <- optimizer_checkpoint_integrity(
    missing_field
  )
  saveRDS(missing_field, path, version = 3L)
  Sys.chmod(path, mode = "0600")
  expect_error(
    gepa_resume_test_compile(path, resume = TRUE),
    class = "dsprrr_optimizer_checkpoint_malformed"
  )
  expect_identical(c(mocks$eval_calls$n, mocks$proposal_calls$n), call_counts)
})

test_that("checkpointed GEPA rejects opaque and stateful Chats before work", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  custom_path <- withr::local_tempfile(fileext = ".rds")
  turns_path <- withr::local_tempfile(fileext = ".rds")
  unlink(c(custom_path, turns_path))

  expect_error(
    gepa_resume_test_compile(
      custom_path,
      task_chat = new_test_chat()
    ),
    class = "dsprrr_optimizer_checkpoint_unsupported_optimizer"
  )
  reflection_chat <- gepa_resume_test_chat("stateful-reflection-chat")
  reflection_chat$set_turns(list(ellmer::UserTurn("prior session")))
  expect_error(
    gepa_resume_test_compile(
      turns_path,
      reflection_chat = reflection_chat
    ),
    class = "dsprrr_optimizer_checkpoint_unsupported_optimizer"
  )
  expect_identical(mocks$eval_calls$n, 0L)
  expect_identical(mocks$proposal_calls$n, 0L)
  expect_false(file.exists(custom_path))
  expect_false(file.exists(turns_path))
})

test_that("checkpointed GEPA refreshes partial summaries at record commits", {
  mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = mocks$eval_program,
    gepa_propose_component_instructions = mocks$propose,
    .package = "dsprrr"
  )
  program <- gepa_resume_test_program()
  dataset <- data.frame(
    question = c("q1", "q2"),
    answer = c("a1", "a2")
  )
  teleprompter <- GEPA(
    metrics = gepa_resume_test_metrics(),
    population_size = 2L,
    generations = 2L,
    mutation_rate = 1,
    crossover_rate = 0,
    selection = "current_best",
    seed = 86L,
    verbose = FALSE,
    track_stats = TRUE
  )
  component_specs <- gepa_checkpoint_supported_program(program)
  interrupt <- new.env(parent = emptyenv())
  interrupt$path <- NULL
  interrupt$record_count <- NA_integer_
  interrupt$generation <- NA_integer_
  writer_observations <- new.env(parent = emptyenv())
  writer_observations$n <- 0L
  original_writer <- optimizer_checkpoint_write
  testthat::local_mocked_bindings(
    optimizer_checkpoint_write = function(
      context,
      phase,
      search_state,
      ...
    ) {
      result <- original_writer(context, phase, search_state, ...)
      checkpoint <- optimizer_checkpoint_read(context$path)
      gepa_checkpoint_state_validate(
        checkpoint$progress$search_state,
        program,
        component_specs,
        names(gepa_resume_test_metrics()),
        dataset,
        dataset,
        teleprompter,
        context$budget
      )
      writer_observations$n <- writer_observations$n + 1L
      if (
        identical(context$path, interrupt$path) &&
          identical(phase, "evaluating") &&
          identical(search_state$generation, interrupt$generation) &&
          identical(
            length(search_state$all_records),
            interrupt$record_count
          ) &&
          is.null(search_state$active_candidate)
      ) {
        interrupt$record_count <- NA_integer_
        stop("injected interruption after durable candidate commit")
      }
      result
    },
    .package = "dsprrr"
  )

  path <- withr::local_tempfile(fileext = ".rds")
  full_path <- withr::local_tempfile(fileext = ".rds")
  unlink(c(path, full_path))
  withr::local_seed(917L)
  caller_rng <- get(".Random.seed", envir = globalenv())

  partial <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 5L),
    program = program,
    trainset = dataset,
    teleprompter = teleprompter
  )
  expect_true(gepa_test_metadata(partial)$partial)
  expect_identical(get(".Random.seed", envir = globalenv()), caller_rng)
  partial_state <- optimizer_checkpoint_read(path)$progress$search_state
  expect_length(partial_state$all_generations[["1"]]$record_keys, 2L)
  expect_length(partial_state$all_generations[["2"]]$record_keys, 0L)
  generation_one_summary <- partial_state$all_generations[["1"]]

  interrupt$path <- path
  interrupt$record_count <- 3L
  interrupt$generation <- 2L
  expect_error(
    gepa_resume_test_compile(
      path,
      control_args = list(max_metric_calls = 100L),
      program = program,
      trainset = dataset,
      teleprompter = teleprompter,
      resume = TRUE
    ),
    "injected interruption after durable candidate commit"
  )
  expect_identical(get(".Random.seed", envir = globalenv()), caller_rng)
  first_commit <- optimizer_checkpoint_read(path)
  first_commit_state <- first_commit$progress$search_state
  expect_length(first_commit_state$all_records, 3L)
  expect_length(first_commit_state$all_generations[["2"]]$record_keys, 1L)
  expect_identical(
    first_commit_state$all_generations[["1"]],
    generation_one_summary
  )

  calls_before_same_limit <- c(mocks$eval_calls$n, mocks$proposal_calls$n)
  stopped <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 6L),
    program = program,
    trainset = dataset,
    teleprompter = teleprompter,
    resume = TRUE
  )
  expect_true(gepa_test_metadata(stopped)$partial)
  expect_identical(
    c(mocks$eval_calls$n, mocks$proposal_calls$n),
    calls_before_same_limit
  )
  second_partial_state <- optimizer_checkpoint_read(path)$progress$search_state
  expect_length(second_partial_state$all_generations[["2"]]$record_keys, 1L)
  expect_identical(
    second_partial_state$all_generations[["1"]],
    generation_one_summary
  )

  interrupt$record_count <- 4L
  expect_error(
    gepa_resume_test_compile(
      path,
      control_args = list(max_metric_calls = 100L),
      program = program,
      trainset = dataset,
      teleprompter = teleprompter,
      resume = TRUE
    ),
    "injected interruption after durable candidate commit"
  )
  expect_identical(get(".Random.seed", envir = globalenv()), caller_rng)
  second_commit_state <- optimizer_checkpoint_read(path)$progress$search_state
  expect_length(second_commit_state$all_records, 4L)
  expect_length(second_commit_state$all_generations[["2"]]$record_keys, 2L)
  expect_identical(
    second_commit_state$all_generations[["1"]],
    generation_one_summary
  )

  resumed <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 100L),
    program = program,
    trainset = dataset,
    teleprompter = teleprompter,
    resume = TRUE
  )
  resumed_checkpoint <- optimizer_checkpoint_read(path)
  calls_after_completion <- c(mocks$eval_calls$n, mocks$proposal_calls$n)
  terminal <- gepa_resume_test_compile(
    path,
    control_args = list(max_metric_calls = 100L),
    program = program,
    trainset = dataset,
    teleprompter = teleprompter,
    resume = TRUE
  )
  expect_identical(
    c(mocks$eval_calls$n, mocks$proposal_calls$n),
    calls_after_completion
  )
  expect_false(gepa_test_metadata(terminal)$partial)
  expect_identical(get(".Random.seed", envir = globalenv()), caller_rng)

  full_mocks <- gepa_resume_test_mocks()
  testthat::local_mocked_bindings(
    eval_program = full_mocks$eval_program,
    gepa_propose_component_instructions = full_mocks$propose,
    .package = "dsprrr"
  )
  full <- gepa_resume_test_compile(
    full_path,
    program = program,
    trainset = dataset,
    teleprompter = teleprompter
  )
  full_checkpoint <- optimizer_checkpoint_read(full_path)

  expect_gt(writer_observations$n, 0L)
  expect_identical(c(mocks$eval_calls$n, mocks$proposal_calls$n), c(8L, 3L))
  expect_identical(
    c(full_mocks$eval_calls$n, full_mocks$proposal_calls$n),
    c(8L, 3L)
  )
  expect_identical(
    resumed_checkpoint$progress$search_state$all_records,
    full_checkpoint$progress$search_state$all_records
  )
  expect_identical(
    resumed_checkpoint$progress$search_state$all_generations,
    full_checkpoint$progress$search_state$all_generations
  )
  expect_identical(resumed_checkpoint$rng, full_checkpoint$rng)
  expect_identical(
    gepa_test_metadata(resumed)$all_generations,
    gepa_test_metadata(full)$all_generations
  )
  expect_identical(
    gepa_test_metadata(resumed)$best_scores,
    gepa_test_metadata(full)$best_scores
  )
  expect_identical(
    resumed_checkpoint$budget[c(
      "attempts",
      "successes",
      "total_errors",
      "metric_calls",
      "provider_calls",
      "trials"
    )],
    full_checkpoint$budget[c(
      "attempts",
      "successes",
      "total_errors",
      "metric_calls",
      "provider_calls",
      "trials"
    )]
  )
})
