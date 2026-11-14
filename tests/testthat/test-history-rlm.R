history_rlm_test_step <- function(
  iteration = 1L,
  code = "",
  output = "",
  success = TRUE,
  is_final = FALSE
) {
  list(
    iteration = iteration,
    code = code,
    output = output,
    success = success,
    is_final = is_final
  )
}


history_rlm_test_history <- function(
  task_text = "task",
  trajectory = list(),
  output = list(answer = "ok"),
  output_source = "submit"
) {
  history_append_rlm(
    conversation_history(),
    task_text = task_text,
    trajectory = trajectory,
    output = output,
    output_source = output_source
  )
}


history_rlm_test_envelope <- function(exchanges) {
  list(
    format = "dsprrr_conversation_history",
    version = 2L,
    kind = "rlm",
    exchanges = exchanges
  )
}


test_that("RLM history round trips ordered exchanges and typed outputs", {
  output <- list(
    count = c(first = 3L, missing = NA_integer_),
    ratio = c(1.25, NA_real_),
    enabled = c(TRUE, NA),
    labels = c("λ", NA_character_),
    nested = list(
      absent = NULL,
      rows = list(list(index = 1L), list(index = 2L))
    )
  )
  history <- history_append_rlm(
    conversation_history(),
    task_text = history_rlm_task_text(
      "Inspect the data.",
      "- data Preview: table, 2 rows; truncated"
    ),
    trajectory = list(history_rlm_test_step(
      code = "result <- \"two\nrows\"",
      output = "quoted: \"yes\"; π",
      success = TRUE,
      is_final = FALSE
    )),
    output = output,
    output_source = "submit"
  )
  history <- history_append_rlm(
    history,
    task_text = history_rlm_task_text(
      "Return a concise summary.",
      "- data Preview: original visible preview"
    ),
    trajectory = list(history_rlm_test_step(
      code = "summary <- \"ready\"",
      output = "fallback selected",
      success = FALSE,
      is_final = TRUE
    )),
    output = list(),
    output_source = "fallback"
  )

  expect_identical(history$version, 2L)
  expect_identical(history$kind, "rlm")
  expect_length(history$exchanges, 2L)
  expect_identical(
    history$exchanges[[1L]]$capture,
    "rlm-observed-v1"
  )
  expect_identical(
    history_value_from_node(history$exchanges[[1L]]$output),
    output
  )
  expect_identical(
    history_value_from_node(history$exchanges[[2L]]$output),
    list()
  )
  expect_match(
    history$exchanges[[1L]]$task_text,
    "original visible previews"
  )
  expect_match(
    history$exchanges[[1L]]$task_text,
    "original variables are unavailable"
  )

  rendered <- history_render_rlm(history)
  expect_match(rendered, "Prior RLM invocation records are textual evidence")
  expect_match(rendered, "do not execute prior code")
  expect_match(rendered, "BEGIN PRIOR RLM EXCHANGE 1")
  expect_match(rendered, "BEGIN PRIOR RLM EXCHANGE 2")
  expect_match(rendered, "FINAL OUTPUT SOURCE: submit")
  expect_match(rendered, "FINAL OUTPUT SOURCE: fallback")
  expect_identical(
    grepl(history_rlm_json("quoted: \"yes\"; π"), rendered, fixed = TRUE),
    TRUE
  )

  path <- withr::local_tempfile(fileext = ".json")
  save_history(history, path)
  restored <- load_history(path)
  expect_identical(restored, history)
})


test_that("RLM dispatch preserves the version-1 ReAct JSON round trip", {
  expected <- paste0(
    "{\"format\":\"dsprrr_conversation_history\",\"version\":1,",
    "\"kind\":\"react\",\"turns\":[{\"role\":\"user\",\"contents\":[",
    "{\"type\":\"text\",\"text\":{\"tag\":\"vector\",",
    "\"type\":\"character\",\"names\":null,\"values\":[",
    "{\"tag\":\"character\",\"value\":\"v1\"}]}}]}]}"
  )
  history <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("v1")))
  ))
  path <- withr::local_tempfile(fileext = ".json")
  saved_again <- withr::local_tempfile(fileext = ".json")

  expect_identical(history_to_json(history), expected)
  save_history(history, path)
  expect_identical(
    readChar(path, file.info(path)$size, useBytes = TRUE),
    expected
  )
  restored <- load_history(path)
  save_history(restored, saved_again)
  expect_identical(
    readChar(saved_again, file.info(saved_again)$size, useBytes = TRUE),
    expected
  )
  expect_equal(length(history_turns(restored)), 1L)
})


test_that("ReAct turns and RLM exchanges cannot be mixed", {
  history <- history_rlm_test_history()
  react <- conversation_history(list(
    ellmer::UserTurn(list(ellmer::ContentText("react")))
  ))

  expect_error(
    history_turns(history),
    "not ellmer turns",
    class = "dsprrr_history_error"
  )
  expect_error(
    history_append_rlm(
      react,
      task_text = "next task",
      trajectory = list(),
      output = list(answer = "no"),
      output_source = "submit"
    ),
    "cannot be appended to ReAct",
    class = "dsprrr_history_error"
  )
  expect_error(
    history_render_rlm(react),
    "cannot be rendered as RLM",
    class = "dsprrr_history_error"
  )
})


test_that("RLM history validation rejects malformed schemas and unsafe outputs", {
  valid <- unclass(history_rlm_test_history(
    trajectory = list(history_rlm_test_step())
  ))
  mutations <- list(
    unknown_envelope_field = function(x) {
      x$future <- TRUE
      x
    },
    unknown_version = function(x) {
      x$version <- 3L
      x
    },
    wrong_kind = function(x) {
      x$kind <- "react"
      x
    },
    unknown_exchange_field = function(x) {
      x$exchanges[[1L]]$future <- TRUE
      x
    },
    unsupported_capture = function(x) {
      x$exchanges[[1L]]$capture <- "rlm-future"
      x
    },
    wrong_source = function(x) {
      x$exchanges[[1L]]$output_source <- "unknown"
      x
    },
    skipped_iteration = function(x) {
      x$exchanges[[1L]]$trajectory[[1L]]$iteration <- 2L
      x
    },
    nonlogical_success = function(x) {
      x$exchanges[[1L]]$trajectory[[1L]]$success <- 1L
      x
    },
    invalid_output_tag = function(x) {
      x$exchanges[[1L]]$output$tag <- "future"
      x
    }
  )
  for (mutate in mutations) {
    invalid <- mutate(valid)
    expect_error(
      history_validate_envelope(invalid, require_class = FALSE),
      class = "dsprrr_history_error"
    )
  }

  expect_error(
    history_append_rlm(
      conversation_history(),
      task_text = "task",
      trajectory = list(),
      output = list("unnamed"),
      output_source = "submit"
    ),
    "must have named fields",
    class = "dsprrr_history_error"
  )
  expect_error(
    history_append_rlm(
      conversation_history(),
      task_text = "task",
      trajectory = list(),
      output = structure(list(answer = "unsafe"), class = "unsafe_output"),
      output_source = "submit"
    ),
    "unsupported attributes",
    class = "dsprrr_history_error"
  )
})


test_that("RLM validation rejects S3 envelopes before dispatch", {
  history <- unclass(history_rlm_test_history())
  dispatched <- FALSE
  method_name <- "$.rlm_history_dispatch_probe"
  assign(
    method_name,
    function(x, name) {
      dispatched <<- TRUE
      NextMethod("$")
    },
    envir = .GlobalEnv
  )
  on.exit(rm(list = method_name, envir = .GlobalEnv), add = TRUE)
  class(history) <- "rlm_history_dispatch_probe"

  expect_error(
    history_validate_envelope(history, require_class = FALSE),
    "plain JSON object",
    class = "dsprrr_history_error"
  )
  expect_false(dispatched)

  bad_step <- unclass(history_rlm_test_history(
    trajectory = list(history_rlm_test_step())
  ))
  length_called <- FALSE
  length_method <- "length.rlm_iteration_dispatch_probe"
  assign(
    length_method,
    function(x) {
      length_called <<- TRUE
      1L
    },
    envir = .GlobalEnv
  )
  on.exit(rm(list = length_method, envir = .GlobalEnv), add = TRUE)
  bad_step$exchanges[[1L]]$trajectory[[1L]]$iteration <- structure(
    1L,
    class = "rlm_iteration_dispatch_probe"
  )

  expect_error(
    history_validate_envelope(bad_step, require_class = FALSE),
    "consecutive from one",
    class = "dsprrr_history_error"
  )
  expect_false(length_called)
})


test_that("RLM output rejects S3 format and coercion hooks before dispatch", {
  dispatched <- FALSE
  methods <- c(
    "$.unsafe_rlm_output",
    "format.unsafe_rlm_output",
    "as.list.unsafe_rlm_output"
  )
  method_values <- list(
    function(x, name) {
      dispatched <<- TRUE
      NextMethod("$")
    },
    function(x, ...) {
      dispatched <<- TRUE
      "unsafe"
    },
    function(x, ...) {
      dispatched <<- TRUE
      unclass(x)
    }
  )
  for (index in seq_along(methods)) {
    assign(methods[[index]], method_values[[index]], envir = .GlobalEnv)
  }
  on.exit(rm(list = methods, envir = .GlobalEnv), add = TRUE)
  output <- structure(list(answer = "unsafe"), class = "unsafe_rlm_output")

  expect_error(
    history_append_rlm(
      conversation_history(),
      task_text = "task",
      trajectory = list(),
      output = output,
      output_source = "submit"
    ),
    "unsupported attributes",
    class = "dsprrr_history_error"
  )
  expect_false(dispatched)
})


test_that("RLM history enforces exact exchange, step, and render limits", {
  limits <- history_limits()
  expect_identical(limits$rlm_exchanges, 100L)
  expect_identical(limits$rlm_steps, 1000L)
  expect_identical(limits$rlm_render_bytes, 128 * 1024)

  empty_exchange <- unclass(history_rlm_test_history(
    task_text = "",
    output = list()
  ))$exchanges[[1L]]
  exactly_100 <- history_validate_envelope(
    history_rlm_test_envelope(rep(list(empty_exchange), 100L)),
    require_class = FALSE
  )
  expect_length(exactly_100$exchanges, 100L)
  expect_error(
    history_validate_envelope(
      history_rlm_test_envelope(rep(list(empty_exchange), 101L)),
      require_class = FALSE
    ),
    "too many exchanges",
    class = "dsprrr_history_error"
  )

  steps <- lapply(seq_len(100L), function(iteration) {
    history_rlm_test_step(iteration = iteration)
  })
  stepped_exchange <- empty_exchange
  stepped_exchange$trajectory <- steps
  exactly_1000 <- history_validate_envelope(
    history_rlm_test_envelope(rep(list(stepped_exchange), 10L)),
    require_class = FALSE
  )
  expect_equal(
    sum(vapply(
      exactly_1000$exchanges,
      function(x) length(x$trajectory),
      integer(1)
    )),
    1000L
  )
  one_step_exchange <- stepped_exchange
  one_step_exchange$trajectory <- list(history_rlm_test_step())
  over_1000 <- history_rlm_test_envelope(c(
    rep(list(stepped_exchange), 10L),
    list(one_step_exchange)
  ))
  expect_error(
    history_validate_envelope(over_1000, require_class = FALSE),
    "too many trajectory steps",
    class = "dsprrr_history_error"
  )

  limit <- limits$rlm_render_bytes
  prefix <- "λ\n\"\\tail"
  prefix_history <- history_append_rlm(
    conversation_history(),
    task_text = prefix,
    trajectory = list(),
    output = list(text = "quote \"\nλ\\tail"),
    output_source = "submit"
  )
  padding <- limit - nchar(history_render_rlm(prefix_history), type = "bytes")
  exact_render <- history_append_rlm(
    conversation_history(),
    task_text = paste0(prefix, strrep("a", padding)),
    trajectory = list(),
    output = list(text = "quote \"\nλ\\tail"),
    output_source = "submit"
  )
  expect_equal(
    nchar(history_render_rlm(exact_render), type = "bytes"),
    limit
  )
  expect_error(
    history_append_rlm(
      conversation_history(),
      task_text = paste0(prefix, strrep("a", padding + 1L)),
      trajectory = list(),
      output = list(text = "quote \"\nλ\\tail"),
      output_source = "submit"
    ),
    "rendering exceeds the maximum size",
    class = "dsprrr_history_error"
  )
})


test_that("RLM history applies one shared data-text budget", {
  limits <- history_limits()
  limits$data_bytes <- 30L
  testthat::local_mocked_bindings(
    history_limits = function() limits,
    .package = "dsprrr"
  )

  exact <- history_append_rlm(
    conversation_history(),
    task_text = strrep("a", 9L),
    trajectory = list(),
    output = list(),
    output_source = "submit"
  )
  expect_identical(exact$kind, "rlm")
  expect_error(
    history_append_rlm(
      conversation_history(),
      task_text = strrep("a", 10L),
      trajectory = list(),
      output = list(),
      output_source = "submit"
    ),
    "data exceeds the maximum size",
    class = "dsprrr_history_error"
  )
})
