capture_rlm_tools_error <- function(expr) {
  tryCatch(force(expr), error = function(error) error)
}


eval_rlm_guest <- function(
  code,
  replay = list(),
  nonce = "rlm-tools-test-nonce",
  has_sub_lm = TRUE,
  custom_tools = list()
) {
  replay <- lapply(replay, function(record) {
    if (!is.list(record)) {
      return(record)
    }
    record$kind <- record$kind %||% "query"
    record[c("kind", setdiff(names(record), "kind"))]
  })
  context <- list()
  context[[rlm_control_replay_field()]] <- replay
  env <- new.env(parent = baseenv())
  env$.context <- context
  prelude <- create_rlm_prelude(
    has_sub_lm = has_sub_lm,
    custom_tools = custom_tools,
    control_nonce = nonce
  )
  eval(parse(text = paste(prelude, code, sep = "\n\n")), envir = env)
}


query_request_from_error <- function(error, nonce = "rlm-tools-test-nonce") {
  decode_rlm_control(conditionMessage(error), nonce)
}


encode_rlm_tools_frame <- function(payload, nonce) {
  envelope <- list(
    version = 1L,
    nonce = nonce,
    kind = "query",
    payload = payload
  )
  json <- jsonlite::toJSON(
    envelope,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
  paste0(
    rlm_control_prefix(),
    gsub(
      "[[:space:]]",
      "",
      jsonlite::base64_enc(charToRaw(as.character(json)))
    )
  )
}


test_that("recursive queries replay as ordinary R values", {
  code <- paste(
    "answer <- llm_query('What is the answer?', 'relevant context')",
    "paste0('answer=', toupper(answer))",
    sep = "\n"
  )
  first <- capture_rlm_tools_error(eval_rlm_guest(code))
  request <- query_request_from_error(first)

  expect_s3_class(first, "error")
  expect_s3_class(request, "rlm_query_request")
  expect_identical(request$index, 1L)
  expect_identical(request$query, "What is the answer?")
  expect_identical(request$context, "relevant context")
  expect_identical(request$batch, FALSE)

  replay <- list(list(
    request = unclass(request),
    success = TRUE,
    value = "forty-two",
    error = NULL
  ))

  expect_identical(eval_rlm_guest(code, replay), "answer=FORTY-TWO")
})


test_that("recursive query indices advance across replayed calls", {
  code <- paste(
    "first <- llm_query('first')",
    "second <- llm_query('second')",
    "paste(first, second, sep = ':')",
    sep = "\n"
  )
  first_error <- capture_rlm_tools_error(eval_rlm_guest(code))
  first_request <- query_request_from_error(first_error)
  first_replay <- list(list(
    request = unclass(first_request),
    success = TRUE,
    value = "one",
    error = NULL
  ))

  second_error <- capture_rlm_tools_error(eval_rlm_guest(code, first_replay))
  second_request <- query_request_from_error(second_error)

  expect_identical(first_request$index, 1L)
  expect_identical(second_request$index, 2L)
  expect_identical(second_request$query, "second")

  replay <- append(
    first_replay,
    list(list(
      request = unclass(second_request),
      success = TRUE,
      value = "two",
      error = NULL
    ))
  )
  expect_identical(eval_rlm_guest(code, replay), "one:two")
})


test_that("batched recursive queries preserve singleton array shape", {
  code <- paste(
    "answers <- llm_query_batched('only', slices = 'context')",
    "list(length = length(answers), value = answers)",
    sep = "\n"
  )
  error <- capture_rlm_tools_error(eval_rlm_guest(code))
  request <- query_request_from_error(error)

  expect_s3_class(request, "rlm_query_request")
  expect_identical(request$index, 1L)
  expect_identical(request$batch, TRUE)
  expect_identical(request$queries, "only")
  expect_identical(request$slices, "context")

  replay <- list(list(
    request = unclass(request),
    success = TRUE,
    value = "response",
    error = NULL
  ))
  expect_identical(
    eval_rlm_guest(code, replay),
    list(length = 1L, value = "response")
  )
})


test_that("recursive query replay rejects divergence and malformed records", {
  code <- "llm_query('original')"
  request_error <- capture_rlm_tools_error(eval_rlm_guest(code))
  request <- query_request_from_error(request_error)

  divergent_request <- unclass(request)
  divergent_request$query <- "different"
  divergent <- capture_rlm_tools_error(eval_rlm_guest(
    code,
    list(list(
      request = divergent_request,
      success = TRUE,
      value = "answer",
      error = NULL
    ))
  ))
  expect_s3_class(divergent, "error")
  expect_match(
    conditionMessage(divergent),
    "control replay diverged from its recorded request",
    fixed = TRUE
  )

  malformed <- capture_rlm_tools_error(eval_rlm_guest(
    code,
    list(list(
      request = unclass(request),
      success = "yes",
      value = "answer"
    ))
  ))
  expect_s3_class(malformed, "error")
  expect_identical(
    conditionMessage(malformed),
    "Malformed RLM control replay state"
  )

  missing_value <- capture_rlm_tools_error(eval_rlm_guest(
    code,
    list(list(request = unclass(request), success = TRUE))
  ))
  expect_identical(
    conditionMessage(missing_value),
    "Malformed RLM control replay state"
  )

  malformed_container <- capture_rlm_tools_error(eval_rlm_guest(
    code,
    replay = "not a list"
  ))
  expect_match(
    conditionMessage(malformed_container),
    "Malformed RLM control replay state",
    fixed = TRUE
  )
})


test_that("recursive query replay reproduces host failures", {
  code <- "llm_query('fails')"
  request_error <- capture_rlm_tools_error(eval_rlm_guest(code))
  request <- query_request_from_error(request_error)
  replay <- list(list(
    request = unclass(request),
    success = FALSE,
    value = NULL,
    error = "sub-LM unavailable"
  ))

  replay_error <- capture_rlm_tools_error(eval_rlm_guest(code, replay))

  expect_s3_class(replay_error, "error")
  expect_identical(conditionMessage(replay_error), "sub-LM unavailable")
})


test_that("control requests reject lossy non-JSON values", {
  invalid <- list(
    "SUBMIT(NA_character_)",
    "SUBMIT(NaN)",
    "SUBMIT(Inf)",
    "SUBMIT(as.Date('2026-08-11'))"
  )

  for (code in invalid) {
    error <- capture_rlm_tools_error(eval_rlm_guest(
      code,
      has_sub_lm = FALSE
    ))
    expect_s3_class(error, "error")
    expect_match(
      conditionMessage(error),
      "JSON-compatible|missing values|NaN or infinite",
      perl = TRUE
    )
    expect_null(query_request_from_error(error))
  }

  tool_error <- capture_rlm_tools_error(eval_rlm_guest(
    "classify_missing(NA_character_)",
    has_sub_lm = FALSE,
    custom_tools = list(classify_missing = function(value) value)
  ))
  expect_match(conditionMessage(tool_error), "missing values", fixed = TRUE)
  expect_null(query_request_from_error(tool_error))
})


test_that("query control frames authenticate and validate their index", {
  nonce <- "current-query-frame"
  request_error <- capture_rlm_tools_error(eval_rlm_guest(
    "llm_query('current')",
    nonce = nonce
  ))
  frame <- conditionMessage(request_error)

  expect_null(decode_rlm_control(frame, "stale-query-frame"))
  expect_identical(decode_rlm_control(frame, nonce)$index, 1L)

  invalid_payloads <- list(
    list(query = "missing", context = NULL, batch = FALSE),
    list(index = 0L, query = "zero", context = NULL, batch = FALSE),
    list(index = 1.5, query = "fractional", context = NULL, batch = FALSE),
    list(
      index = 1L,
      query = "bad context",
      context = I(c("one", "two")),
      batch = FALSE
    ),
    list(
      index = .Machine$integer.max + 1,
      query = "overflow",
      context = NULL,
      batch = FALSE
    )
  )
  for (payload in invalid_payloads) {
    malformed <- encode_rlm_tools_frame(payload, nonce)
    error <- capture_rlm_tools_error(decode_rlm_control(malformed, nonce))
    expect_s3_class(error, "dsprrr_rlm_control_error")
    expect_match(
      conditionMessage(error),
      "Malformed RLM query control frame",
      fixed = TRUE
    )
  }
})


test_that("peek handles scalar and vector boundaries without reverse slices", {
  env <- new.env(parent = baseenv())
  env$.context <- list()
  eval(
    parse(
      text = create_rlm_prelude(
        has_sub_lm = FALSE,
        control_nonce = "peek-test"
      )
    ),
    envir = env
  )

  expect_identical(env$peek(c("a", "b", "c"), 2, 3), c("b", "c"))
  expect_identical(env$peek(c("a", "b", "c"), 4, 7), character())
  expect_identical(env$peek(c("a", "b", "c"), 3, 2), character())
  expect_identical(env$peek(c("a", "b", "c"), -2, 2), c("a", "b"))
  expect_identical(env$peek("abcdef", 2, 4), "bcd")
  expect_identical(env$peek("abcdef", 8, 10), "")
  expect_identical(env$peek("abcdef", 4, 2), "")
  expect_identical(env$peek(character(), 1, 2), character())
  expect_identical(env$peek(NA_character_, 1, 2), NA_character_)

  invalid_bounds <- list(
    function() env$peek("abcdef", NA_real_, 2),
    function() env$peek("abcdef", 1.5, 2),
    function() env$peek("abcdef", c(1, 2), 3),
    function() env$peek("abcdef", 1, Inf)
  )
  for (call in invalid_bounds) {
    error <- capture_rlm_tools_error(call())
    expect_s3_class(error, "error")
    expect_match(conditionMessage(error), "finite whole number", fixed = TRUE)
  }
})


test_that("query replay uses a reserved context field", {
  expect_identical(
    rlm_control_replay_field(),
    ".dsprrr_rlm_control_replay"
  )
})
