batch_contract_chat <- function(fail_on = NULL, initial_turns = list()) {
  force(fail_on)
  force(initial_turns)
  turns <- initial_turns
  calls <- 0L

  last_turn <- function(role = c("assistant", "user"), ...) {
    role <- match.arg(role)
    matching <- Filter(function(turn) identical(turn@role, role), turns)
    if (length(matching) == 0L) {
      stop("no matching turn")
    }
    matching[[length(matching)]]
  }

  structure(
    list(
      calls = function() calls,
      get_turns = function(...) turns,
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      last_turn = last_turn,
      get_model = function() "batch-contract-model",
      chat_structured = function(prompt, ...) {
        calls <<- calls + 1L
        prompt <- as.character(prompt)
        if (!is.null(fail_on) && grepl(fail_on, prompt, fixed = TRUE)) {
          error <- simpleError("provider row failed")
          class(error) <- c("batch_contract_provider_error", class(error))
          stop(error)
        }
        response <- list(answer = paste0("ok:", prompt))
        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(
              contents = list(ellmer::ContentText(prompt))
            ),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText("ok")),
              tokens = c(4L, 2L, 0L),
              cost = 0.001,
              duration = 0.01
            )
          )
        )
        response
      }
    ),
    class = "Chat"
  )
}

batch_shape_chat <- function(responses) {
  force(responses)
  turns <- list()
  calls <- 0L

  structure(
    list(
      calls = function() calls,
      get_turns = function(...) turns,
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      get_model = function() "batch-shape-model",
      chat_structured = function(prompt, ...) {
        calls <<- calls + 1L
        prompt <- as.character(prompt)
        matches <- names(responses)[vapply(
          names(responses),
          function(name) grepl(name, prompt, fixed = TRUE),
          logical(1)
        )]
        if (length(matches) != 1L) {
          stop("could not select one batch-shape response")
        }
        response <- responses[[matches]]
        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(
              contents = list(ellmer::ContentText(prompt))
            ),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText("ok"))
            )
          )
        )
        response
      }
    ),
    class = "Chat"
  )
}

batch_contract_metadata_names <- c(
  "usage",
  "error",
  "error_class",
  "error_stage",
  "cache",
  "backend",
  "batch_index"
)

test_that("zero-length Predict inputs return without runtime side effects", {
  local_reset_cache()
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()
  clear_prompt_history()

  mod <- module(signature("text -> answer"), type = "predict")
  chat <- batch_contract_chat()
  stats_before <- cache_stats()

  simple <- run(mod, text = character(), .llm = chat, .cache = TRUE)
  structured <- run(
    mod,
    text = character(),
    .llm = chat,
    .cache = TRUE,
    .return_format = "structured"
  )

  expect_identical(simple, list())
  expect_s3_class(structured, "dsprrr_batch_result")
  expect_length(structured, 0L)
  expect_equal(chat$calls(), 0L)
  expect_length(mod$state$traces, 0L)
  expect_length(dsprrr:::.dsprrr_env$prompt_history, 0L)
  expect_identical(cache_stats()$hits, stats_before$hits)
  expect_identical(cache_stats()$misses, stats_before$misses)
})

test_that("zero-row datasets preserve simple and structured shapes", {
  clear_prompt_history()
  mod <- module(signature("text -> answer"), type = "predict")
  chat <- batch_contract_chat()
  data <- data.frame(text = character(), group = integer())

  simple <- run_dataset(mod, data, .llm = chat)
  structured <- run_dataset(
    mod,
    data,
    .llm = chat,
    .return_format = "structured"
  )

  expect_named(simple, c("text", "group", "result"))
  expect_named(
    structured,
    c("text", "group", "result", ".error", ".metadata", ".chat")
  )
  expect_equal(nrow(simple), 0L)
  expect_equal(nrow(structured), 0L)
  expect_type(simple$result, "list")
  expect_type(structured$.metadata, "list")
  expect_type(structured$.chat, "list")
  expect_equal(chat$calls(), 0L)
  expect_length(mod$state$traces, 0L)
  expect_length(dsprrr:::.dsprrr_env$prompt_history, 0L)
})

test_that("positive-row zero-input datasets execute every isolated row", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_object(answer = ellmer::type_string()),
    instructions = "Return a constant answer"
  )
  data <- data.frame(row.names = seq_len(3L))

  simple_mod <- module(sig, type = "predict")
  simple_chat <- batch_contract_chat()
  simple <- run_dataset(
    simple_mod,
    data,
    .llm = simple_chat,
    .progress = FALSE,
    .cache = FALSE
  )

  expect_equal(nrow(simple), 3L)
  expect_named(simple, "result")
  expect_length(simple$result, 3L)
  expect_true(all(vapply(simple$result, is.character, logical(1))))
  expect_length(simple_mod$state$traces, 3L)
  expect_equal(simple_chat$calls(), 0L)
  expect_length(simple_chat$get_turns(), 0L)

  clear_prompt_history()
  structured_mod <- module(sig, type = "predict")
  structured_chat <- batch_contract_chat()
  control <- concurrency_control(backend = "sequential", max_active = 4L)
  structured <- run_dataset(
    structured_mod,
    data,
    .llm = structured_chat,
    .concurrency = control,
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_equal(nrow(structured), 3L)
  expect_named(structured, c("result", ".error", ".metadata", ".chat"))
  expect_length(structured$result, 3L)
  expect_true(all(is.na(structured$.error)))
  expect_identical(
    vapply(structured$.metadata, `[[`, integer(1), "batch_index"),
    1:3
  )
  expect_identical(
    vapply(structured$.metadata, `[[`, integer(1), "requested_workers"),
    rep(4L, 3L)
  )
  expect_identical(
    vapply(structured$.metadata, `[[`, integer(1), "effective_workers"),
    rep(1L, 3L)
  )
  expect_true(all(vapply(
    structured$.chat,
    function(row_chat) {
      length(row_chat$get_turns()) == 2L
    },
    logical(1)
  )))
  expect_length(structured_mod$state$traces, 3L)
  expect_identical(
    vapply(
      structured_mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:3
  )
  expect_identical(
    vapply(
      structured_mod$state$traces,
      function(trace) trace$metadata$requested_workers,
      integer(1)
    ),
    rep(4L, 3L)
  )
  expect_identical(
    vapply(
      structured_mod$state$traces,
      function(trace) trace$metadata$effective_workers,
      integer(1)
    ),
    rep(1L, 3L)
  )
  history <- dsprrr:::.dsprrr_env$prompt_history
  expect_length(history, 3L)
  expect_identical(
    vapply(history, function(entry) entry$metadata$batch_index, integer(1)),
    1:3
  )
  expect_identical(
    vapply(
      history,
      function(entry) entry$metadata$requested_workers,
      integer(1)
    ),
    rep(4L, 3L)
  )
  expect_identical(
    vapply(
      history,
      function(entry) entry$metadata$effective_workers,
      integer(1)
    ),
    rep(1L, 3L)
  )
  expect_equal(structured_chat$calls(), 0L)
  expect_length(structured_chat$get_turns(), 0L)
})

test_that("one-row datasets keep a simple result list-column", {
  mod <- module(signature("text -> answer"), type = "predict")
  result <- run_dataset(
    mod,
    data.frame(text = "one"),
    .llm = batch_contract_chat(),
    .cache = FALSE
  )

  expect_type(result$result, "list")
  expect_length(result$result, 1L)
  expect_type(result$result[[1]], "character")
  expect_match(result$result[[1]], "one", fixed = TRUE)
})

test_that("no-input signatures preserve zero-row dataset shape", {
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_object(answer = ellmer::type_string()),
    instructions = "Produce an answer without inputs"
  )
  mod <- module(sig, type = "predict")
  chat <- batch_contract_chat()

  result <- run_dataset(
    mod,
    data.frame(row.names = integer()),
    .llm = chat,
    .return_format = "structured"
  )

  expect_equal(nrow(result), 0L)
  expect_named(result, c("result", ".error", ".metadata", ".chat"))
  expect_equal(chat$calls(), 0L)
  expect_length(mod$state$traces, 0L)
})

test_that("mixed zero and incompatible lengths fail before execution", {
  sig <- signature("left, right -> answer")
  mod <- module(sig, type = "predict")
  chat <- batch_contract_chat()

  expect_error(
    run(mod, left = character(), right = "scalar", .llm = chat),
    class = "dsprrr_batch_length_error"
  )
  expect_error(
    run(mod, left = c("a", "b"), right = c("x", "y", "z"), .llm = chat),
    class = "dsprrr_batch_length_error"
  )
  expect_equal(chat$calls(), 0L)
  expect_length(mod$state$traces, 0L)
})

test_that("scalar runtime objects recycle by identity in both public paths", {
  output_type <- ellmer::type_object(answer = ellmer::type_string())
  sig <- signature(
    inputs = list(
      input("text", ellmer::type_string()),
      input("content", ellmer::type_string())
    ),
    output_type = output_type
  )
  content <- ellmer::ContentText("shared multimodal input")
  observed <- list()
  testthat::local_mocked_bindings(
    run_batch = function(module, inputs, n, ...) {
      observed[[length(observed) + 1L]] <<- inputs
      rep(list("ok"), n)
    },
    .package = "dsprrr"
  )

  generic <- module(sig, type = "predict")
  direct <- module(sig, type = "predict")
  suppressWarnings(run(
    generic,
    text = c("one", "two"),
    content = content,
    .progress = FALSE
  ))
  suppressWarnings(direct$run(
    text = c("one", "two"),
    content = content,
    .progress = FALSE
  ))

  expect_length(observed, 2L)
  for (inputs in observed) {
    expect_type(inputs$content, "list")
    expect_length(inputs$content, 2L)
    expect_identical(inputs$content[[1L]], content)
    expect_identical(inputs$content[[2L]], content)
  }

  nonreplicable <- new.env(parent = emptyenv())
  recycled <- dsprrr:::batch_recycle_input(nonreplicable, 2L)
  expect_type(recycled, "list")
  expect_identical(recycled, list(nonreplicable, nonreplicable))
})

test_that("scalar and sequential batch traces share one metadata contract", {
  clear_prompt_history()
  scalar_mod <- module(signature("text -> answer"), type = "predict")
  batch_mod <- module(signature("text -> answer"), type = "predict")

  scalar <- run(
    scalar_mod,
    text = "one",
    .llm = batch_contract_chat(),
    .cache = FALSE,
    .return_format = "structured"
  )
  batch <- run(
    batch_mod,
    text = c("one", "two"),
    .llm = batch_contract_chat(),
    .cache = FALSE,
    .return_format = "structured",
    .progress = FALSE
  )

  expect_true(all(batch_contract_metadata_names %in% names(scalar$metadata)))
  expect_true(all(vapply(
    batch,
    function(row) {
      all(batch_contract_metadata_names %in% names(row$metadata))
    },
    logical(1)
  )))
  expect_length(scalar_mod$state$traces, 1L)
  expect_length(batch_mod$state$traces, 2L)
  expect_identical(scalar_mod$state$traces[[1]]$metadata$backend, "sequential")
  expect_identical(scalar_mod$state$traces[[1]]$metadata$batch_index, 1L)
  expect_identical(scalar_mod$state$traces[[1]]$metadata$cache, "bypass")
  expect_identical(
    vapply(
      batch_mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:2
  )
  expect_true(all(vapply(
    batch_mod$state$traces,
    function(trace) identical(trace$metadata$backend, "sequential"),
    logical(1)
  )))
  expect_equal(length(dsprrr:::.dsprrr_env$prompt_history), 3L)
})

test_that("direct Predict batches use isolated canonical row execution", {
  clear_prompt_history()
  baseline <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("prior question"))),
    ellmer::AssistantTurn(contents = list(ellmer::ContentText("prior answer")))
  )
  caller <- batch_contract_chat(initial_turns = baseline)
  mod <- module(signature("text -> answer"), type = "predict")

  result <- mod$run(
    text = c("one", "two"),
    .llm = caller,
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_s3_class(result, "dsprrr_batch_result")
  expect_identical(caller$get_turns(), baseline)
  expect_equal(caller$calls(), 0L)
  expect_length(mod$state$traces, 2L)
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:2
  )
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$backend,
      character(1)
    ),
    rep("sequential", 2L)
  )
  expect_length(dsprrr:::.dsprrr_env$prompt_history, 2L)
  row_chat_ids <- vapply(
    result,
    function(row) rlang::obj_address(row$chat),
    character(1)
  )
  expect_length(unique(row_chat_ids), 2L)
})

test_that("direct custom Module batches reject before forward work", {
  DirectBatchProbe <- R6::R6Class(
    "DirectBatchContractProbe",
    inherit = dsprrr:::Module,
    public = list(
      calls = 0L,
      initialize = function() {
        super$initialize(signature("text -> answer"))
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$calls <- self$calls + 1L
        tibble::tibble(
          output = list(list(answer = "unexpected")),
          chat = list(NULL),
          metadata = list(list())
        )
      }
    )
  )
  mod <- DirectBatchProbe$new()

  error <- rlang::catch_cnd(mod$run(text = c("one", "two")))

  expect_s3_class(error, "dsprrr_batch_unsupported_module")
  expect_equal(mod$calls, 0L)
  expect_length(mod$state$traces, 0L)
})

test_that("scalar output stays named while batch rows stay simplified", {
  scalar_mod <- module(signature("text -> answer"), type = "predict")
  batch_mod <- module(signature("text -> answer"), type = "predict")

  scalar <- run(
    scalar_mod,
    text = "one",
    .llm = batch_contract_chat(),
    .cache = FALSE
  )
  batch <- run(
    batch_mod,
    text = c("one", "two"),
    .llm = batch_contract_chat(),
    .cache = FALSE,
    .progress = FALSE
  )

  expect_named(scalar, "answer")
  expect_length(scalar, 1L)
  expect_type(batch[[1]], "character")
  expect_identical(scalar$answer, batch[[1]])
})

test_that("Module predict preserves named records without changing run batches", {
  responses <- list(
    ROW_ONE = list(sentiment = "first"),
    ROW_TWO = list(sentiment = "second")
  )
  make_module <- function() {
    module(signature("text -> sentiment"), type = "predict")
  }

  scalar <- make_module()$predict(
    text = "ROW_ONE",
    .llm = batch_shape_chat(responses)
  )
  predicted <- make_module()$predict(
    text = names(responses),
    .llm = batch_shape_chat(responses)
  )
  run_result <- run(
    make_module(),
    text = names(responses),
    .llm = batch_shape_chat(responses),
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(scalar, responses[[1L]])
  expect_identical(predicted, unname(responses))
  expect_identical(run_result, list("first", "second"))
  expect_named(predicted[[1L]], "sentiment")
  expect_type(run_result[[1L]], "character")
})

test_that("sequential failures still commit one ordered trace per row", {
  clear_prompt_history()
  mod <- module(signature("text -> answer"), type = "predict")

  expect_warning(
    result <- run(
      mod,
      text = c("first", "FAIL", "third"),
      .llm = batch_contract_chat(fail_on = "FAIL"),
      .cache = FALSE,
      .return_format = "structured",
      .progress = FALSE
    ),
    "Failed to process item 2"
  )

  expect_length(result, 3L)
  expect_length(mod$state$traces, 3L)
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$inputs$text,
      character(1)
    ),
    c("first", "FAIL", "third")
  )
  expect_true(is.na(mod$state$traces[[1]]$metadata$error))
  expect_match(mod$state$traces[[2]]$metadata$error, "provider row failed")
  expect_identical(
    mod$state$traces[[2]]$metadata$error_class,
    "batch_contract_provider_error"
  )
  expect_true(is.na(mod$state$traces[[3]]$metadata$error))
  history <- dsprrr:::.dsprrr_env$prompt_history
  expect_equal(length(history), 3L)
  expect_true(grepl("first", history[[1]]$prompt, fixed = TRUE))
  expect_true(grepl("FAIL", history[[2]]$prompt, fixed = TRUE))
  expect_true(grepl("third", history[[3]]$prompt, fixed = TRUE))
})

test_that("simple failures warn once and return no internal attributes", {
  mod <- module(signature("text -> answer"), type = "predict")
  warnings <- character()
  result <- withCallingHandlers(
    run(
      mod,
      text = c("first", "FAIL", "third"),
      .llm = batch_contract_chat(fail_on = "FAIL"),
      .cache = FALSE,
      .progress = FALSE
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(warnings, 1L)
  expect_match(warnings, "Failed to process item 2: provider row failed")
  expect_false(grepl("Failed to process item 2: Failed", warnings))
  expect_true(is.na(result[[2]]))
  expect_null(attributes(result[[2]]))
  expect_null(attributes(result))
})

test_that("usage comes only from a verified current-call assistant delta", {
  baseline <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("old question"))),
    ellmer::AssistantTurn(
      contents = list(ellmer::ContentText("old answer")),
      tokens = c(100L, 50L, 0L),
      cost = 9.99,
      duration = 8
    )
  )
  turns <- baseline
  opaque_success <- structure(
    list(
      get_turns = function(...) turns,
      set_turns = function(value) turns <<- value,
      last_turn = function(...) baseline[[2]],
      chat_structured = function(...) list(answer = "fresh")
    ),
    class = "Chat"
  )
  mod <- module(signature("text -> answer"), type = "predict")
  success <- dsprrr:::process_batch_item(
    list(text = "new"),
    mod,
    opaque_success,
    index = 1L,
    .verbose = FALSE,
    .return_format = "structured",
    .cache = FALSE
  )
  expect_true(all(is.na(success$metadata$usage)))

  seeded_failure <- batch_contract_chat(
    fail_on = "FAIL",
    initial_turns = baseline
  )
  failure <- dsprrr:::process_batch_item(
    list(text = "FAIL"),
    mod,
    seeded_failure,
    index = 1L,
    .verbose = FALSE,
    .return_format = "structured",
    .cache = FALSE
  )
  expect_match(failure$metadata$error, "provider row failed")
  expect_true(all(is.na(failure$metadata$usage)))
})

test_that("scalar provider errors are traced then re-signalled unchanged", {
  mod <- module(signature("text -> answer"), type = "predict")
  error <- rlang::catch_cnd(run(
    mod,
    text = "FAIL",
    .llm = batch_contract_chat(fail_on = "FAIL"),
    .cache = FALSE
  ))

  expect_s3_class(error, "batch_contract_provider_error")
  expect_match(conditionMessage(error), "provider row failed")
  expect_length(mod$state$traces, 1L)
  expect_match(mod$state$traces[[1]]$metadata$error, "provider row failed")
})

test_that("a genuine cache hit is recorded on the matching canonical trace", {
  local_reset_cache()
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()
  calls <- new.env(parent = emptyenv())
  calls$n <- 0L
  testthat::local_mocked_bindings(
    req_perform = function(req) {
      calls$n <- calls$n + 1L
      body <- list(
        id = paste0("response_", calls$n),
        object = "response",
        created_at = 1L,
        status = "completed",
        model = "batch-cache-model",
        output = list(list(
          id = paste0("message_", calls$n),
          type = "message",
          status = "completed",
          role = "assistant",
          content = list(list(
            type = "output_text",
            annotations = list(),
            logprobs = list(),
            text = "{\"answer\":\"cached answer\"}"
          ))
        )),
        usage = list(
          input_tokens = 4L,
          input_tokens_details = list(cached_tokens = 0L),
          output_tokens = 2L,
          output_tokens_details = list(reasoning_tokens = 0L),
          total_tokens = 6L
        ),
        service_tier = "default",
        metadata = list()
      )
      getFromNamespace("response", "httr2")(
        headers = list(`content-type` = "application/json"),
        body = charToRaw(jsonlite::toJSON(
          body,
          auto_unbox = TRUE,
          null = "null"
        ))
      )
    },
    .package = "ellmer"
  )
  base <- suppressWarnings(ellmer::chat_openai(
    api_key = "dummy-key",
    model = "batch-cache-model"
  ))
  first_chat <- base$clone(deep = TRUE)
  second_chat <- base$clone(deep = TRUE)
  mod <- module(signature("text -> answer"), type = "predict")

  first <- run(
    mod,
    text = "same request",
    .llm = first_chat,
    .cache = TRUE,
    .return_format = "structured"
  )
  second <- suppressMessages(run(
    mod,
    text = "same request",
    .llm = second_chat,
    .cache = TRUE,
    .return_format = "structured"
  ))

  expect_equal(first$output$answer, "cached answer")
  expect_equal(second$output$answer, "cached answer")
  expect_equal(calls$n, 1L)
  expect_length(mod$state$traces, 2L)
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$cache,
      character(1)
    ),
    c("miss", "hit")
  )
  expect_true(all(is.na(second$metadata$usage[c(
    "input_tokens",
    "output_tokens",
    "cost",
    "duration_s"
  )])))
  miss_turns <- mod$state$traces[[1]]$turns
  hit_turns <- mod$state$traces[[2]]$turns
  expect_length(miss_turns, 2L)
  expect_length(hit_turns, 2L)
  expect_identical(
    vapply(miss_turns, function(turn) turn@role, character(1)),
    vapply(hit_turns, function(turn) turn@role, character(1))
  )
  expect_identical(miss_turns[[1]]@contents, hit_turns[[1]]@contents)
  expect_identical(miss_turns[[2]]@contents, hit_turns[[2]]@contents)
})

test_that("native ellmer parallel traces successes and character errors", {
  clear_prompt_history()
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      tibble::tibble(
        answer = c("first", NA_character_, "third"),
        input_tokens = c(3L, 0L, 5L),
        output_tokens = c(1L, 0L, 2L),
        cached_input_tokens = c(0L, 0L, 0L),
        cost = c(0.01, 0, 0.02),
        .error = c(NA_character_, "native row failed", NA_character_)
      )
    },
    .package = "ellmer"
  )
  mod <- module(signature("text -> answer"), type = "predict")

  result <- run(
    mod,
    text = c("a", "b", "c"),
    .llm = batch_contract_chat(),
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_equal(result[[1]]$output$answer, "first")
  expect_match(result[[2]]$metadata$error, "native row failed")
  expect_equal(result[[3]]$output$answer, "third")
  expect_length(mod$state$traces, 3L)
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:3
  )
  expect_true(all(vapply(
    mod$state$traces,
    function(trace) identical(trace$metadata$backend, "ellmer"),
    logical(1)
  )))
  history <- dsprrr:::.dsprrr_env$prompt_history
  expect_equal(length(history), 3L)
  expect_true(grepl("text: a", history[[1]]$prompt, fixed = TRUE))
  expect_true(grepl("text: b", history[[2]]$prompt, fixed = TRUE))
  expect_true(grepl("text: c", history[[3]]$prompt, fixed = TRUE))
  expect_true(all(vapply(
    mod$state$traces,
    function(trace) identical(trace$metadata$cache, "bypass"),
    logical(1)
  )))
})

test_that("native ellmer rows reconstruct nested and array output types", {
  output_type <- ellmer::type_object(
    summary = ellmer::type_string(),
    details = ellmer::type_object(
      score = ellmer::type_number(),
      tags = ellmer::type_array(ellmer::type_string())
    ),
    choices = ellmer::type_array(ellmer::type_object(
      label = ellmer::type_string(),
      confidence = ellmer::type_number()
    ))
  )
  sig <- signature(
    inputs = list(input("text", ellmer::type_string())),
    output_type = output_type
  )
  choices <- list(
    tibble::tibble(
      label = c("alpha", "beta"),
      confidence = c(0.7, 0.3)
    ),
    tibble::tibble(label = "gamma", confidence = 1)
  )
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      tibble::tibble(
        summary = c("first", "second"),
        details = tibble::tibble(
          score = c(0.9, 0.8),
          tags = list(c("red", "blue"), "green")
        ),
        choices = choices,
        .error = list(NULL, NULL)
      )
    },
    .package = "ellmer"
  )
  mod <- module(sig, type = "predict")

  result <- run(
    mod,
    text = c("one", "two"),
    .llm = batch_contract_chat(),
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(
    result[[1L]]$output,
    list(
      summary = "first",
      details = list(score = 0.9, tags = c("red", "blue")),
      choices = choices[[1L]]
    )
  )
  expect_identical(
    result[[2L]]$output,
    list(
      summary = "second",
      details = list(score = 0.8, tags = "green"),
      choices = choices[[2L]]
    )
  )
  expect_type(result[[1L]]$output$details, "list")
  expect_s3_class(result[[1L]]$output$choices, "tbl_df")
})

test_that("native ellmer ignores ambiguous child probes for parent presence", {
  output_type <- ellmer::type_object(
    label = ellmer::type_string(),
    details = ellmer::type_object(
      score = ellmer::type_number(),
      meta = ellmer::type_object(
        note = ellmer::type_string(required = FALSE)
      ),
      .required = FALSE
    )
  )
  raw_rows <- list(
    list(
      label = "present",
      details = list(score = 0.8, meta = list())
    ),
    list(label = "absent")
  )
  convert_from_type <- get("convert_from_type", asNamespace("ellmer"))
  scalar_rows <- lapply(raw_rows, convert_from_type, type = output_type)
  parallel_rows <- convert_from_type(
    raw_rows,
    ellmer::type_array(output_type)
  )
  parallel_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      parallel_calls <<- parallel_calls + 1L
      parallel_rows
    },
    .package = "ellmer"
  )
  mod <- module(
    signature(
      inputs = list(input("text", ellmer::type_string())),
      output_type = output_type
    ),
    type = "predict"
  )

  result <- run(
    mod,
    text = c("one", "two"),
    .llm = batch_contract_chat(),
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )
  batch_rows <- lapply(result, `[[`, "output")

  expect_identical(batch_rows, scalar_rows)
  expect_null(batch_rows[[2L]]$details)
  expect_identical(parallel_calls, 1L)
})

test_that("native ellmer preserves non-object row failures through a wrapper", {
  output_type <- ellmer::type_string()
  observed_type <- NULL
  parallel_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(type, ...) {
      parallel_calls <<- parallel_calls + 1L
      observed_type <<- type
      tibble::tibble(
        value = c("ok", NA_character_),
        .error = c(NA_character_, "native string row failed")
      )
    },
    .package = "ellmer"
  )
  mod <- module(
    signature(
      inputs = list(input("text", ellmer::type_string())),
      output_type = output_type
    ),
    type = "predict"
  )

  result <- run(
    mod,
    text = c("one", "two"),
    .llm = batch_contract_chat(),
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_s3_class(observed_type, "ellmer::TypeObject")
  expect_identical(observed_type@properties$value, output_type)
  expect_identical(parallel_calls, 1L)
  expect_identical(result[[1L]]$output, "ok")
  expect_true(is.na(result[[2L]]$output))
  expect_match(result[[2L]]$metadata$error, "native string row failed")
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:2
  )
})

test_that("simple batches preserve valid optional NULL rows and traces", {
  output_type <- ellmer::type_string(required = FALSE)
  responses <- list(ROW_NULL = NULL, ROW_OK = "ok")
  make_module <- function() {
    module(
      signature(
        inputs = list(input("text", ellmer::type_string())),
        output_type = output_type
      ),
      type = "predict"
    )
  }

  sequential_module <- make_module()
  sequential <- run(
    sequential_module,
    text = names(responses),
    .llm = batch_shape_chat(responses),
    .concurrency = concurrency_control(backend = "sequential"),
    .progress = FALSE,
    .cache = FALSE
  )

  parallel_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      parallel_calls <<- parallel_calls + 1L
      tibble::tibble(
        value = c(NA_character_, "ok"),
        .error = list(NULL, NULL)
      )
    },
    .package = "ellmer"
  )
  native_module <- make_module()
  native <- run(
    native_module,
    text = names(responses),
    .llm = batch_contract_chat(),
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(sequential, unname(responses))
  expect_identical(native, unname(responses))
  expect_identical(parallel_calls, 1L)
  expect_length(sequential_module$state$traces, 2L)
  expect_length(native_module$state$traces, 2L)
  expect_true(all(vapply(
    c(sequential_module$state$traces, native_module$state$traces),
    function(trace) is.na(trace$metadata$error),
    logical(1)
  )))
  expect_identical(
    vapply(
      native_module$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:2
  )
})

test_that("native ellmer distinguishes empty arrays from failed array rows", {
  output_type <- ellmer::type_array(ellmer::type_string())
  observed_type <- NULL
  parallel_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(type, ...) {
      parallel_calls <<- parallel_calls + 1L
      observed_type <<- type
      tibble::tibble(
        value = list(character(), character()),
        .error = list(NULL, simpleError("native array row failed"))
      )
    },
    .package = "ellmer"
  )
  mod <- module(
    signature(
      inputs = list(input("text", ellmer::type_string())),
      output_type = output_type
    ),
    type = "predict"
  )

  result <- run(
    mod,
    text = c("one", "two"),
    .llm = batch_contract_chat(),
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_s3_class(observed_type, "ellmer::TypeObject")
  expect_identical(observed_type@properties$value, output_type)
  expect_identical(parallel_calls, 1L)
  expect_identical(result[[1L]]$output, character())
  expect_true(is.na(result[[2L]]$output))
  expect_match(result[[2L]]$metadata$error, "native array row failed")
  expect_identical(
    dsprrr:::ellmer_parallel_schema_supported(
      ellmer::type_array(ellmer::type_string(), required = FALSE)
    ),
    TRUE
  )
})

test_that("ambiguous required-array presence uses isolated scalar rows", {
  output_type <- ellmer::type_object(
    label = ellmer::type_string(),
    details = ellmer::type_object(
      tags = ellmer::type_array(ellmer::type_string()),
      .required = FALSE
    )
  )
  raw_rows <- list(
    ROW_ARRAY_PRESENT = list(
      label = "present",
      details = list(tags = character())
    ),
    ROW_ARRAY_ABSENT = list(label = "absent")
  )
  convert_from_type <- get("convert_from_type", asNamespace("ellmer"))
  scalar_rows <- lapply(raw_rows, convert_from_type, type = output_type)
  parallel_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      parallel_calls <<- parallel_calls + 1L
      stop("native path must not run")
    },
    .package = "ellmer"
  )
  chat <- batch_shape_chat(scalar_rows)
  mod <- module(
    signature(
      inputs = list(input("text", ellmer::type_string())),
      output_type = output_type
    ),
    type = "predict"
  )

  result <- run(
    mod,
    text = names(raw_rows),
    .llm = chat,
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )
  batch_rows <- lapply(result, `[[`, "output")

  expect_identical(batch_rows, unname(scalar_rows))
  expect_identical(batch_rows[[1L]]$details$tags, character())
  expect_null(batch_rows[[2L]]$details)
  expect_identical(parallel_calls, 0L)
  expect_identical(
    sum(vapply(result, function(row) row$chat$calls(), integer(1))),
    2L
  )
  expect_identical(
    vapply(result, function(row) row$metadata$requested_backend, character(1)),
    rep("ellmer", 2L)
  )
  expect_identical(
    vapply(result, function(row) row$metadata$effective_backend, character(1)),
    rep("sequential", 2L)
  )
  expect_true(all(vapply(
    result,
    function(row) grepl("present-empty", row$metadata$fallback_reason),
    logical(1)
  )))
})

test_that("ambiguous object without required evidence preserves scalar shape", {
  output_type <- ellmer::type_object(
    label = ellmer::type_string(),
    details = ellmer::type_object(
      note = ellmer::type_string(required = FALSE),
      .required = FALSE
    )
  )
  raw_rows <- list(
    ROW_SPARSE_PRESENT = list(label = "present", details = list()),
    ROW_SPARSE_ABSENT = list(label = "absent")
  )
  convert_from_type <- get("convert_from_type", asNamespace("ellmer"))
  scalar_rows <- lapply(raw_rows, convert_from_type, type = output_type)
  parallel_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      parallel_calls <<- parallel_calls + 1L
      stop("native path must not run")
    },
    .package = "ellmer"
  )
  result <- run(
    module(
      signature(
        inputs = list(input("text", ellmer::type_string())),
        output_type = output_type
      ),
      type = "predict"
    ),
    text = names(raw_rows),
    .llm = batch_shape_chat(scalar_rows),
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )
  batch_rows <- lapply(result, `[[`, "output")

  expect_identical(batch_rows, unname(scalar_rows))
  expect_identical(batch_rows[[1L]]$details, list(note = NULL))
  expect_null(batch_rows[[2L]]$details)
  expect_identical(parallel_calls, 0L)
  expect_identical(
    sum(vapply(result, function(row) row$chat$calls(), integer(1))),
    2L
  )
})

test_that("reserved top-level error fields use isolated scalar rows", {
  output_type <- ellmer::type_object(.error = ellmer::type_string())
  raw_rows <- list(
    ROW_ERROR_ONE = list(.error = "model-one"),
    ROW_ERROR_TWO = list(.error = "model-two")
  )
  convert_from_type <- get("convert_from_type", asNamespace("ellmer"))
  scalar_rows <- lapply(raw_rows, convert_from_type, type = output_type)
  parallel_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      parallel_calls <<- parallel_calls + 1L
      stop("native path must not run")
    },
    .package = "ellmer"
  )

  result <- run(
    module(
      signature(
        inputs = list(input("text", ellmer::type_string())),
        output_type = output_type
      ),
      type = "predict"
    ),
    text = names(raw_rows),
    .llm = batch_shape_chat(scalar_rows),
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(lapply(result, `[[`, "output"), unname(scalar_rows))
  expect_identical(parallel_calls, 0L)
  expect_true(all(vapply(
    result,
    function(row) is.na(row$metadata$error),
    logical(1)
  )))
  expect_identical(
    vapply(result, function(row) row$metadata$effective_backend, character(1)),
    rep("sequential", 2L)
  )
})

test_that("empty object schemas preserve row count through scalar fallback", {
  output_type <- ellmer::type_object()
  scalar_rows <- list(ROW_EMPTY_ONE = list(), ROW_EMPTY_TWO = list())
  parallel_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      parallel_calls <<- parallel_calls + 1L
      stop("native path must not run")
    },
    .package = "ellmer"
  )

  result <- run(
    module(
      signature(
        inputs = list(input("text", ellmer::type_string())),
        output_type = output_type
      ),
      type = "predict"
    ),
    text = names(scalar_rows),
    .llm = batch_shape_chat(scalar_rows),
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(lapply(result, `[[`, "output"), unname(scalar_rows))
  expect_identical(length(result), 2L)
  expect_identical(parallel_calls, 0L)
})

test_that("explicit ellmer rejects ambiguous schemas before provider work", {
  output_type <- ellmer::type_object(
    details = ellmer::type_object(
      tags = ellmer::type_array(ellmer::type_string()),
      .required = FALSE
    )
  )
  parallel_calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      parallel_calls <<- parallel_calls + 1L
      stop("native path must not run")
    },
    .package = "ellmer"
  )
  chat <- batch_shape_chat(list(ROW_ONE = list(details = NULL)))
  condition <- tryCatch(
    run(
      module(
        signature(
          inputs = list(input("text", ellmer::type_string())),
          output_type = output_type
        ),
        type = "predict"
      ),
      text = c("ROW_ONE", "ROW_TWO"),
      .llm = chat,
      .concurrency = concurrency_control(
        backend = "ellmer",
        max_active = 2L
      ),
      .progress = FALSE,
      .cache = FALSE
    ),
    error = function(error) error
  )

  expect_s3_class(condition, "dsprrr_parallel_schema_unsupported_error")
  expect_identical(parallel_calls, 0L)
  expect_identical(chat$calls(), 0L)
  json_type <- ellmer::type_from_schema(
    text = '{"type":"object","additionalProperties":true}'
  )
  expect_identical(
    dsprrr:::ellmer_parallel_schema_supported(json_type),
    FALSE
  )
})

test_that("native ellmer row chats preserve baseline while traces keep deltas", {
  baseline <- list(
    ellmer::UserTurn(contents = list(ellmer::ContentText("prior question"))),
    ellmer::AssistantTurn(contents = list(ellmer::ContentText("prior answer")))
  )
  caller <- batch_contract_chat(initial_turns = baseline)
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      tibble::tibble(
        answer = c("first", "second"),
        .error = list(NULL, NULL)
      )
    },
    .package = "ellmer"
  )
  mod <- module(signature("text -> answer"), type = "predict")
  result <- run(
    mod,
    text = c("a", "b"),
    .llm = caller,
    .parallel = TRUE,
    .parallel_method = "ellmer",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(caller$get_turns(), baseline)
  expect_true(all(vapply(
    result,
    function(row) {
      identical(row$chat$get_turns()[1:2], baseline)
    },
    logical(1)
  )))
  expect_true(all(vapply(
    result,
    function(row) {
      length(row$chat$get_turns()) == 4L
    },
    logical(1)
  )))
  expect_identical(
    vapply(mod$state$traces, function(trace) length(trace$turns), integer(1)),
    c(2L, 2L)
  )
})

test_that("mirai workers return records committed by the parent in row order", {
  skip_if_not_installed("mirai")
  current <- mirai::daemons(NULL)
  if (is.null(current) || current == 0L) {
    mirai::daemons(n = 1L)
    withr::defer(mirai::daemons(0L))
  }

  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- structure(
    list(chat_structured = function(prompt, ...) {
      list(answer = paste0("worker:", as.character(prompt)))
    }),
    class = "Chat"
  )
  result <- run(
    mod,
    text = c("one", "two"),
    .parallel = TRUE,
    .parallel_method = "mirai",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_true(grepl("one", result[[1]]$output$answer, fixed = TRUE))
  expect_true(grepl("two", result[[2]]$output$answer, fixed = TRUE))
  expect_length(mod$state$traces, 2L)
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:2
  )
  expect_true(all(vapply(
    mod$state$traces,
    function(trace) identical(trace$metadata$backend, "mirai"),
    logical(1)
  )))
})

test_that("mirai row failures retain ordered parent traces and error metadata", {
  clear_prompt_history()
  skip_if_not_installed("mirai")
  current <- mirai::daemons(NULL)
  if (is.null(current) || current == 0L) {
    mirai::daemons(n = 1L)
    withr::defer(mirai::daemons(0L))
  }

  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- structure(
    list(chat_structured = function(prompt, ...) {
      if (grepl("FAIL", as.character(prompt), fixed = TRUE)) {
        error <- simpleError("mirai provider row failed")
        class(error) <- c("mirai_provider_error", class(error))
        stop(error)
      }
      list(answer = paste0("worker:", as.character(prompt)))
    }),
    class = "Chat"
  )
  result <- run(
    mod,
    text = c("first", "FAIL", "third"),
    .parallel = TRUE,
    .parallel_method = "mirai",
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_true(grepl("first", result[[1]]$output$answer, fixed = TRUE))
  expect_true(is.na(result[[2]]$output))
  expect_match(result[[2]]$metadata$error, "mirai provider row failed")
  expect_identical(result[[2]]$metadata$error_class, "mirai_provider_error")
  expect_true(grepl("third", result[[3]]$output$answer, fixed = TRUE))
  expect_length(mod$state$traces, 3L)
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$inputs$text,
      character(1)
    ),
    c("first", "FAIL", "third")
  )
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:3
  )
  expect_match(
    mod$state$traces[[2]]$metadata$error,
    "mirai provider row failed"
  )
  history <- dsprrr:::.dsprrr_env$prompt_history
  expect_equal(length(history), 3L)
  expect_true(grepl("first", history[[1]]$prompt, fixed = TRUE))
  expect_true(grepl("FAIL", history[[2]]$prompt, fixed = TRUE))
  expect_true(grepl("third", history[[3]]$prompt, fixed = TRUE))
})

test_that("malformed mirai records become typed traced failure rows", {
  mod <- module(signature("text -> answer"), type = "predict")
  request <- dsprrr:::build_module_request(mod, list(text = "row"))
  chat <- batch_contract_chat()
  started_at <- Sys.time() - 1
  ended_at <- Sys.time()
  incomplete_success <- list(
    ok = TRUE,
    started_at = started_at,
    ended_at = ended_at,
    usage = list(),
    model = NA_character_,
    turns_before = NULL
  )
  serialization_failure <- list(
    ok = FALSE,
    error_message = "cannot serialize Chat",
    error_class = "simpleError",
    error_kind = "serialization",
    started_at = started_at,
    ended_at = ended_at,
    usage = list(),
    model = NA_character_,
    turns_before = NULL
  )
  vector_usage <- list(
    ok = TRUE,
    response = list(answer = "invalid"),
    usage_verified = TRUE,
    started_at = started_at,
    ended_at = ended_at,
    usage = list(input_tokens = 1:2),
    model = NA_character_,
    turns_before = NULL
  )
  unknown_field <- list(
    ok = TRUE,
    response = list(answer = "invalid"),
    usage_verified = TRUE,
    started_at = started_at,
    ended_at = ended_at,
    usage = list(),
    model = NA_character_,
    turns_before = NULL,
    unexpected = "field"
  )
  records <- list(
    "atomic",
    list(),
    incomplete_success,
    vector_usage,
    unknown_field,
    structure(
      "remote worker crashed",
      class = "miraiError",
      message = "remote worker crashed"
    ),
    serialization_failure
  )
  results <- lapply(records, function(record) {
    dsprrr:::mirai_worker_result(
      record = record,
      module = mod,
      input_set = list(text = "row"),
      request = request,
      chat = chat,
      index = 1L,
      .return_format = "structured",
      fallback_started_at = started_at,
      fallback_ended_at = ended_at
    )
  })

  expect_true(all(vapply(
    results,
    function(result) {
      is.na(result$output) &&
        identical(result$metadata$backend, "mirai") &&
        identical(result$metadata$cache, "bypass") &&
        nzchar(result$metadata$error) &&
        !is.null(attr(result, "dsprrr_trace", exact = TRUE))
    },
    logical(1)
  )))
  expect_true(all(vapply(
    results[1:5],
    function(result) {
      inherits(
        attr(result, "dsprrr_error_condition", exact = TRUE),
        "dsprrr_mirai_record_error"
      )
    },
    logical(1)
  )))
  expect_s3_class(
    attr(results[[6]], "dsprrr_error_condition", exact = TRUE),
    "dsprrr_mirai_worker_error"
  )
  expect_s3_class(
    attr(results[[7]], "dsprrr_error_condition", exact = TRUE),
    "dsprrr_mirai_serialization_error"
  )
})

test_that("mirai accepts valid optional NULL responses at its boundary", {
  mod <- module(
    signature(
      inputs = list(input("text", ellmer::type_string())),
      output_type = ellmer::type_string(required = FALSE)
    ),
    type = "predict"
  )
  request <- dsprrr:::build_module_request(mod, list(text = "row"))
  now <- Sys.time()
  record <- list(
    ok = TRUE,
    response = NULL,
    usage_verified = TRUE,
    started_at = now,
    ended_at = now,
    usage = list(),
    model = "mirai-model",
    turns_before = list()
  )

  result <- dsprrr:::mirai_worker_result(
    record = record,
    module = mod,
    input_set = list(text = "row"),
    request = request,
    chat = batch_contract_chat(),
    index = 1L,
    .return_format = "simple"
  )

  expect_s3_class(result, "dsprrr_internal_null_batch_result")
  expect_null(attr(result, "dsprrr_error_condition", exact = TRUE))
  collected <- dsprrr:::collect_backend_traces(list(result))
  traces <- attr(collected, "dsprrr_traces", exact = TRUE)
  public <- dsprrr:::strip_backend_traces(collected)
  expect_identical(public, list(NULL))
  expect_length(traces, 1L)
  expect_null(traces[[1L]]$output)
  expect_true(is.na(traces[[1L]]$metadata$error))

  json_type <- ellmer::type_from_schema(
    text = '{"type":["string","null"]}'
  )
  json_module <- module(
    signature(
      inputs = list(input("text", ellmer::type_string())),
      output_type = json_type
    ),
    type = "predict"
  )
  json_result <- dsprrr:::mirai_worker_result(
    record = record,
    module = json_module,
    input_set = list(text = "row"),
    request = dsprrr:::build_module_request(
      json_module,
      list(text = "row")
    ),
    chat = batch_contract_chat(),
    index = 1L,
    .return_format = "simple"
  )
  expect_s3_class(json_result, "dsprrr_internal_null_batch_result")
  expect_identical(dsprrr:::mirai_output_allows_null(json_type), TRUE)
})

test_that("mirai timeouts stop tasks and commit typed elapsed failures", {
  skip_if_not_installed("mirai")
  current <- mirai::daemons(NULL)
  if (is.null(current) || current == 0L) {
    mirai::daemons(n = 1L)
    withr::defer(mirai::daemons(0L))
  }
  withr::local_options(list(dsprrr.parallel_timeout = 0.03))
  clear_prompt_history()

  stops <- new.env(parent = emptyenv())
  stops$n <- 0L
  progress <- new.env(parent = emptyenv())
  progress$done <- 0L
  original_stop <- mirai::stop_mirai
  testthat::local_mocked_bindings(
    stop_mirai = function(x) {
      stops$n <- stops$n + 1L
      original_stop(x)
    },
    .package = "mirai"
  )
  testthat::local_mocked_bindings(
    cli_progress_bar = function(...) "batch-contract-progress",
    cli_progress_update = function(...) invisible(NULL),
    cli_progress_done = function(...) {
      progress$done <- progress$done + 1L
      invisible(NULL)
    },
    .package = "cli"
  )

  mod <- module(signature("text -> answer"), type = "predict")
  mod$chat <- structure(
    list(chat_structured = function(...) {
      Sys.sleep(0.25)
      list(answer = "too late")
    }),
    class = "Chat"
  )
  expect_warning(
    result <- run(
      mod,
      text = c("first", "second"),
      .parallel = TRUE,
      .parallel_method = "mirai",
      .return_format = "structured",
      .progress = TRUE,
      .cache = FALSE
    ),
    "timed out"
  )

  expect_gte(stops$n, 1L)
  expect_equal(progress$done, 1L)
  expect_length(result, 2L)
  expect_length(mod$state$traces, 2L)
  expect_true(all(vapply(
    result,
    function(row) {
      identical(row$metadata$error_class, "dsprrr_mirai_timeout_error") &&
        identical(row$metadata$backend, "mirai") &&
        identical(row$metadata$cache, "bypass") &&
        row$metadata$latency_ms >= 20 &&
        is.null(attr(row, "dsprrr_trace", exact = TRUE)) &&
        is.null(attr(row, "dsprrr_error_condition", exact = TRUE))
    },
    logical(1)
  )))
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:2
  )
  expect_equal(length(dsprrr:::.dsprrr_env$prompt_history), 2L)
})

test_that("ReAct scalar run preserves its specialized forward path", {
  local_reset_cache()
  configure_cache(enable_memory = TRUE, enable_disk = FALSE)
  clear_cache()
  clear_prompt_history()
  turns <- list()
  chat_calls <- 0L
  last_turn <- function(role = c("assistant", "user"), ...) {
    role <- match.arg(role)
    matching <- Filter(function(turn) identical(turn@role, role), turns)
    matching[[length(matching)]]
  }
  chat <- structure(
    list(
      register_tool = function(tool) invisible(NULL),
      get_turns = function(...) turns,
      last_turn = last_turn,
      chat = function(prompt, ...) {
        chat_calls <<- chat_calls + 1L
        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(contents = list(ellmer::ContentText(prompt))),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText("reasoning"))
            )
          )
        )
        invisible(NULL)
      },
      chat_structured = function(...) {
        turns <<- c(
          turns,
          list(ellmer::AssistantTurn(
            contents = list(ellmer::ContentText("{\"answer\":\"done\"}"))
          ))
        )
        list(answer = "done")
      },
      get_model = function() "react-model"
    ),
    class = "Chat"
  )
  mod <- module(signature("question -> answer"), type = "react")

  scalar <- run(
    mod,
    question = "solve",
    .llm = chat,
    .cache = FALSE,
    .return_format = "structured"
  )

  expect_equal(chat_calls, 1L)
  expect_identical(scalar$metadata$finalization, "structured-followup")
  expect_length(mod$state$traces, 1L)
  traces_before <- mod$state$traces
  history_before <- dsprrr:::.dsprrr_env$prompt_history
  cache_before <- cache_stats()[c("hits", "misses")]
  turns_before <- chat$get_turns()
  expect_error(
    run(mod, question = c("a", "b"), .llm = chat, .cache = TRUE),
    class = "dsprrr_batch_unsupported_module"
  )
  expect_equal(chat_calls, 1L)
  expect_identical(mod$state$traces, traces_before)
  expect_identical(dsprrr:::.dsprrr_env$prompt_history, history_before)
  expect_identical(cache_stats()[c("hits", "misses")], cache_before)
  expect_identical(chat$get_turns(), turns_before)
})

test_that("direct specialized Predict batches reject before forward work", {
  SpecializedPredict <- R6::R6Class(
    "BatchContractSpecializedPredict",
    inherit = dsprrr:::PredictModule,
    public = list(
      calls = 0L,
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$calls <- self$calls + 1L
        tibble::tibble(
          output = list("unexpected"),
          chat = list(NULL),
          metadata = list(list())
        )
      }
    )
  )
  mod <- SpecializedPredict$new(signature("text -> answer"))

  expect_error(
    mod$run(text = c("a", "b"), .progress = FALSE),
    class = "dsprrr_batch_unsupported_module"
  )
  expect_identical(mod$calls, 0L)
  expect_length(mod$state$traces, 0L)
})

specialized_dataset_chat <- function() {
  turns <- list()
  structure(
    list(
      get_turns = function(...) turns,
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      record = function(value) {
        turns <<- append(turns, list(value))
        invisible(NULL)
      }
    ),
    class = "Chat"
  )
}

specialized_dataset_module <- function() {
  SpecializedDatasetPredict <- R6::R6Class(
    "BatchContractSpecializedDatasetPredict",
    inherit = dsprrr:::PredictModule,
    public = list(
      calls = list(),
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        self$calls <- append(self$calls, list(batch))
        if (identical(batch$text, "bad")) {
          stop("specialized row failed")
        }
        history_before <- length(.llm$get_turns())
        .llm$record(batch$text)
        tibble::tibble(
          output = list(paste0("special:", batch$text)),
          chat = list(.llm),
          metadata = list(list(history_before = history_before))
        )
      }
    )
  )
  SpecializedDatasetPredict$new(signature("text -> answer"))
}

test_that("datasets run specialized Predict modules through isolated scalar rows", {
  mod <- specialized_dataset_module()
  chat <- specialized_dataset_chat()

  result <- run_dataset(
    mod,
    data.frame(text = c("one", "two")),
    .llm = chat,
    .return_format = "structured",
    .progress = FALSE
  )

  expect_identical(result$result, list("special:one", "special:two"))
  expect_identical(
    vapply(result$.metadata, `[[`, integer(1), "history_before"),
    c(0L, 0L)
  )
  expect_identical(
    vapply(mod$calls, `[[`, character(1), "text"),
    c("one", "two")
  )
  expect_true(all(lengths(mod$calls) == 1L))
  expect_length(chat$get_turns(), 0L)
  expect_true(all(vapply(
    result$.chat,
    function(row_chat) {
      length(row_chat$get_turns()) == 1L
    },
    logical(1)
  )))
  expect_false(identical(result$.chat[[1]], result$.chat[[2]]))
})

test_that("scalar dataset adapters preserve valid NULL row positions", {
  NullDatasetPredict <- R6::R6Class(
    "BatchContractNullDatasetPredict",
    inherit = dsprrr:::PredictModule,
    public = list(
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        value <- if (identical(batch$text, "null")) NULL else "ok"
        tibble::tibble(
          output = list(value),
          chat = list(.llm),
          metadata = list(list())
        )
      }
    )
  )
  mod <- NullDatasetPredict$new(signature(
    inputs = list(input("text", ellmer::type_string())),
    output_type = ellmer::type_string(required = FALSE)
  ))

  result <- run_dataset(
    mod,
    data.frame(text = c("null", "value")),
    .llm = specialized_dataset_chat(),
    .progress = FALSE
  )

  expect_equal(nrow(result), 2L)
  expect_identical(result$text, c("null", "value"))
  expect_identical(result$result, list(NULL, "ok"))
})

test_that("specialized row traces and history inherit dataset metadata", {
  TracingSpecializedPredict <- R6::R6Class(
    "BatchContractTracingSpecializedPredict",
    inherit = dsprrr:::PredictModule,
    public = list(
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        super$forward(batch, .llm = .llm, trace = trace, ...)
      }
    )
  )
  clear_prompt_history()
  mod <- TracingSpecializedPredict$new(signature("text -> answer"))
  chat <- batch_contract_chat()
  control <- concurrency_control(backend = "sequential", max_active = 4L)

  result <- run_dataset(
    mod,
    data.frame(text = c("one", "two")),
    .llm = chat,
    .concurrency = control,
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(
    vapply(result$.metadata, `[[`, integer(1), "batch_index"),
    1:2
  )
  expect_identical(
    vapply(result$.metadata, `[[`, integer(1), "requested_workers"),
    rep(4L, 2L)
  )
  expect_length(mod$state$traces, 2L)
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$batch_index,
      integer(1)
    ),
    1:2
  )
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$requested_workers,
      integer(1)
    ),
    rep(4L, 2L)
  )
  expect_identical(
    vapply(
      mod$state$traces,
      function(trace) trace$metadata$effective_workers,
      integer(1)
    ),
    rep(1L, 2L)
  )
  history <- dsprrr:::.dsprrr_env$prompt_history
  expect_length(history, 2L)
  expect_identical(
    vapply(history, function(entry) entry$metadata$batch_index, integer(1)),
    1:2
  )
  expect_identical(
    vapply(
      history,
      function(entry) entry$metadata$requested_workers,
      integer(1)
    ),
    rep(4L, 2L)
  )
  expect_identical(
    vapply(
      history,
      function(entry) entry$metadata$effective_workers,
      integer(1)
    ),
    rep(1L, 2L)
  )
  expect_equal(chat$calls(), 0L)
  expect_length(chat$get_turns(), 0L)
})

test_that("history generation disambiguates identical ring replacements", {
  RingTracingPredict <- R6::R6Class(
    "BatchContractRingTracingPredict",
    inherit = dsprrr:::PredictModule,
    public = list(
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        super$forward(batch, .llm = .llm, trace = trace, ...)
      }
    )
  )
  withr::local_options(dsprrr.prompt_history_max = 1L)
  clear_prompt_history()
  identical_entry <- list(
    timestamp = as.POSIXct("2026-01-01", tz = "UTC"),
    source = "PredictModule",
    prompt = "identical",
    response = "identical",
    model = "identical"
  )
  testthat::local_mocked_bindings(
    extract_history_entry = function(trace, source) {
      rlang::duplicate(identical_entry, shallow = FALSE)
    },
    .package = "dsprrr"
  )
  add_to_global_history(list(), source = "PredictModule")
  expect_identical(dsprrr:::prompt_history_generation(), 1)
  expect_identical(dsprrr:::.dsprrr_env$prompt_history[[1]], identical_entry)

  mod <- RingTracingPredict$new(signature("text -> answer"))
  result <- run_dataset(
    mod,
    data.frame(text = "one"),
    .llm = batch_contract_chat(),
    .concurrency = concurrency_control(
      backend = "sequential",
      max_active = 4L
    ),
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )

  expect_length(result$result, 1L)
  expect_length(mod$state$traces, 1L)
  expect_length(dsprrr:::.dsprrr_env$prompt_history, 1L)
  expect_identical(dsprrr:::prompt_history_generation(), 2)
  expect_identical(
    dsprrr:::.dsprrr_env$prompt_history[[1]]$metadata$batch_index,
    1L
  )
  expect_identical(
    dsprrr:::.dsprrr_env$prompt_history[[1]]$metadata$requested_workers,
    4L
  )
  expect_identical(
    dsprrr:::.dsprrr_env$prompt_history[[1]]$metadata$effective_workers,
    1L
  )
})

test_that("failed history capture never patches a prior entry", {
  FailedHistoryTracingPredict <- R6::R6Class(
    "BatchContractFailedHistoryTracingPredict",
    inherit = dsprrr:::PredictModule,
    public = list(
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        super$forward(batch, .llm = .llm, trace = trace, ...)
      }
    )
  )
  clear_prompt_history()
  add_to_global_history(
    list(prompt = "prior", output = "prior", model = "prior"),
    source = "prior"
  )
  .dsprrr_env$prompt_history[[1]]$metadata <- list(
    batch_index = 99L,
    requested_workers = 99L,
    effective_workers = 99L
  )
  prior <- rlang::duplicate(
    dsprrr:::.dsprrr_env$prompt_history[[1]],
    shallow = FALSE
  )
  generation_before <- dsprrr:::prompt_history_generation()
  testthat::local_mocked_bindings(
    extract_history_entry = function(trace, source) {
      stop("history capture probe failed")
    },
    .package = "dsprrr"
  )

  mod <- FailedHistoryTracingPredict$new(signature("text -> answer"))
  expect_warning(
    run_dataset(
      mod,
      data.frame(text = "one"),
      .llm = batch_contract_chat(),
      .concurrency = concurrency_control(
        backend = "sequential",
        max_active = 4L
      ),
      .return_format = "structured",
      .progress = FALSE,
      .cache = FALSE
    ),
    "Failed to capture prompt history"
  )

  expect_length(mod$state$traces, 1L)
  expect_identical(mod$state$traces[[1]]$metadata$batch_index, 1L)
  expect_identical(dsprrr:::prompt_history_generation(), generation_before)
  expect_identical(dsprrr:::.dsprrr_env$prompt_history, list(prior))
})

test_that("specialized simple dataset rows have one stable output shape", {
  NamedDatasetPredict <- R6::R6Class(
    "BatchContractNamedDatasetPredict",
    inherit = dsprrr:::PredictModule,
    public = list(
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        tibble::tibble(
          output = list(list(answer = paste0("named:", batch$text))),
          chat = list(.llm),
          metadata = list(list())
        )
      }
    )
  )
  mod <- NamedDatasetPredict$new(signature("text -> answer"))
  chat <- specialized_dataset_chat()

  one <- run_dataset(
    mod,
    data.frame(text = "one"),
    .llm = chat,
    .progress = FALSE
  )
  many <- run_dataset(
    mod,
    data.frame(text = c("one", "two")),
    .llm = chat,
    .progress = FALSE
  )

  expect_identical(one$result, list("named:one"))
  expect_identical(many$result, list("named:one", "named:two"))
  expect_identical(one$result[[1]], many$result[[1]])
})

test_that("specialized dataset concurrency intent rejects before row work", {
  mod <- specialized_dataset_module()
  chat <- specialized_dataset_chat()

  expect_error(
    run_dataset(
      mod,
      data.frame(text = c("one", "two")),
      .llm = chat,
      .concurrency = concurrency_control(backend = "auto", max_active = 2L),
      .progress = FALSE
    ),
    class = "dsprrr_batch_unsupported_module"
  )
  expect_length(mod$calls, 0L)
  expect_length(chat$get_turns(), 0L)
})

test_that("specialized row adapters reject unsupported controls before work", {
  controls <- list(
    concurrency_control(backend = "sequential", max_errors = 1L),
    concurrency_control(backend = "sequential", task_timeout = 1),
    concurrency_control(backend = "sequential", total_timeout = 1),
    concurrency_control(backend = "sequential", cancel = FALSE)
  )

  for (control in controls) {
    mod <- specialized_dataset_module()
    chat <- specialized_dataset_chat()
    expect_error(
      run_dataset(
        mod,
        data.frame(text = c("one", "two")),
        .llm = chat,
        .concurrency = control,
        .progress = FALSE
      ),
      class = "dsprrr_concurrency_unsupported_error"
    )
    expect_length(mod$calls, 0L)
    expect_length(chat$get_turns(), 0L)
  }
})

test_that("row adapters auto-resolve once and do not force local providers", {
  auto_calls <- logical()
  detected_chat <- specialized_dataset_chat()
  testthat::local_mocked_bindings(
    get_default_chat = function(create = TRUE) {
      auto_calls <<- c(auto_calls, create)
      if (create) detected_chat else NULL
    },
    .package = "dsprrr"
  )
  withr::local_envvar(c(
    OPENAI_API_KEY = "mock-key",
    ANTHROPIC_API_KEY = "",
    GOOGLE_API_KEY = ""
  ))

  auto_mod <- specialized_dataset_module()
  auto <- run_dataset(
    auto_mod,
    data.frame(text = c("one", "two")),
    .return_format = "structured",
    .progress = FALSE
  )

  expect_identical(auto_calls, TRUE)
  expect_identical(auto$result, list("special:one", "special:two"))
  expect_length(detected_chat$get_turns(), 0L)
  expect_false(identical(auto$.chat[[1]], auto$.chat[[2]]))

  LocalDatasetPredict <- R6::R6Class(
    "BatchContractLocalDatasetPredict",
    inherit = dsprrr:::PredictModule,
    public = list(
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        tibble::tibble(
          output = list(paste0("local:", batch$text)),
          chat = list(.llm),
          metadata = list(list())
        )
      }
    )
  )
  local_calls <- logical()
  testthat::local_mocked_bindings(
    get_default_chat = function(create = TRUE) {
      local_calls <<- c(local_calls, create)
      if (create) {
        stop("provider creation must not be forced")
      }
      NULL
    },
    .package = "dsprrr"
  )
  withr::local_envvar(c(
    OPENAI_API_KEY = "",
    ANTHROPIC_API_KEY = "",
    GOOGLE_API_KEY = ""
  ))

  local <- run_dataset(
    LocalDatasetPredict$new(signature("text -> answer")),
    data.frame(text = c("one", "two")),
    .progress = FALSE
  )
  expect_identical(local_calls, FALSE)
  expect_identical(local$result, list("local:one", "local:two"))
})

test_that("specialized dataset row failures preserve structured shape", {
  mod <- specialized_dataset_module()
  chat <- specialized_dataset_chat()

  expect_warning(
    result <- run_dataset(
      mod,
      data.frame(text = c("one", "bad", "three")),
      .llm = chat,
      .return_format = "structured",
      .progress = FALSE
    ),
    "Failed to process item 2: specialized row failed"
  )

  expect_identical(result$result[[1]], "special:one")
  expect_true(is.na(result$result[[2]]))
  expect_identical(result$result[[3]], "special:three")
  expect_true(is.na(result$.error[[1]]))
  expect_match(result$.error[[2]], "specialized row failed")
  expect_true(is.na(result$.error[[3]]))
  expect_length(result$.metadata, 3L)
  expect_length(result$.chat, 3L)
  expect_length(mod$calls, 3L)
  expect_length(chat$get_turns(), 0L)

  simple_mod <- specialized_dataset_module()
  expect_warning(
    simple <- run_dataset(
      simple_mod,
      data.frame(text = c("one", "bad", "three")),
      .llm = specialized_dataset_chat(),
      .progress = FALSE
    ),
    "Failed to process item 2: specialized row failed"
  )
  expect_true(is.na(simple$result[[2]]))
  expect_null(attributes(simple$result[[2]]))
})

test_that("cache observer failures never change model results", {
  calls <- 0L
  chat <- structure(
    list(chat_structured = function(...) {
      calls <<- calls + 1L
      list(answer = "ok")
    }),
    class = "Chat"
  )

  result <- dsprrr:::cached_chat_structured(
    chat,
    "prompt",
    ellmer::type_object(answer = ellmer::type_string()),
    .cache = FALSE,
    .observer = function(...) stop("observer failure")
  )

  expect_equal(result$answer, "ok")
  expect_equal(calls, 1L)
})

test_that("cache observer fires exactly once for every outcome", {
  stored <- cachem::key_missing()
  fake_cache <- list(
    get = function(key) stored,
    set = function(key, value) {
      stored <<- value
      invisible(NULL)
    }
  )
  testthat::local_mocked_bindings(
    get_cache = function() fake_cache,
    cache_request_fingerprint = function(...) list(version = "test"),
    cache_key = function(...) "observer-key",
    cache_turn_snapshot = function(...) list(mode = "stateless"),
    cache_turn_delta = function(...) list(mode = "stateless", turns = list()),
    cache_replay_turn_delta = function(...) TRUE,
    increment_cache_stats = function(...) invisible(NULL),
    .package = "dsprrr"
  )
  package_state <- getFromNamespace(".dsprrr_env", "dsprrr")
  old_first_hit <- package_state$cache_first_hit_shown
  package_state$cache_first_hit_shown <- TRUE
  withr::defer({
    package_state$cache_first_hit_shown <- old_first_hit
  })
  provider_calls <- 0L
  chat <- structure(
    list(chat_structured = function(...) {
      provider_calls <<- provider_calls + 1L
      list(answer = "ok")
    }),
    class = "Chat"
  )
  output_type <- ellmer::type_object(answer = ellmer::type_string())
  observe <- function() {
    events <- character()
    list(
      callback = function(status, reason) {
        events <<- c(events, paste(status, reason, sep = ":"))
      },
      events = function() events
    )
  }

  miss <- observe()
  dsprrr:::cached_chat_structured(
    chat,
    "prompt",
    output_type,
    .cache = TRUE,
    .observer = miss$callback
  )
  expect_identical(miss$events(), "miss:cache_miss")

  hit <- observe()
  dsprrr:::cached_chat_structured(
    chat,
    "prompt",
    output_type,
    .cache = TRUE,
    .observer = hit$callback
  )
  expect_identical(hit$events(), "hit:cache_hit")
  expect_equal(provider_calls, 1L)

  bypass <- observe()
  dsprrr:::cached_chat_structured(
    chat,
    "bypass",
    output_type,
    .cache = FALSE,
    .observer = bypass$callback
  )
  expect_identical(bypass$events(), "bypass:disabled")

  stored <- cachem::key_missing()
  failure <- observe()
  failing_chat <- structure(
    list(chat_structured = function(...) stop("provider failed")),
    class = "Chat"
  )
  expect_error(
    dsprrr:::cached_chat_structured(
      failing_chat,
      "failure",
      output_type,
      .cache = TRUE,
      .observer = failure$callback
    ),
    "provider failed"
  )
  expect_identical(failure$events(), "miss:cache_miss")

  observer_calls <- 0L
  result <- dsprrr:::cached_chat_structured(
    chat,
    "observer failure",
    output_type,
    .cache = FALSE,
    .observer = function(...) {
      observer_calls <<- observer_calls + 1L
      stop("observer failed")
    }
  )
  expect_equal(result$answer, "ok")
  expect_equal(observer_calls, 1L)
})

test_that("batch printing ignores missing and empty success errors", {
  result <- structure(
    list(
      list(output = "a", chat = NULL, metadata = list(error = NA_character_)),
      list(output = "b", chat = NULL, metadata = list(error = ""))
    ),
    class = c("dsprrr_batch_result", "list")
  )
  output <- cli::cli_fmt(print(result))

  expect_true(any(grepl(
    "All items completed successfully",
    output,
    fixed = TRUE
  )))
  expect_false(any(grepl("Errors", output, fixed = TRUE)))
})
