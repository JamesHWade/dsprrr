test_that("executable Flex source is parsed without host evaluation", {
  touched <- FALSE
  source <- paste(
    "touched <- TRUE",
    "forward <- function(value) list(result = value)",
    sep = "\n"
  )

  validated <- dsprrr:::flex_validate_code_source(source)

  expect_identical(validated, source)
  expect_identical(touched, FALSE)
})

test_that("executable Flex requires a top-level forward function", {
  expect_snapshot(
    error = TRUE,
    dsprrr:::flex_validate_code_source("answer <- 42")
  )
})

test_that("interpreter-backed Flex returns deterministic zero-call output", {
  skip_if_not_installed("callr")
  source <- paste(
    "forward <- function(question) {",
    "  list(answer = paste0('Hello, ', question))",
    "}",
    sep = "\n"
  )

  result <- dsprrr:::flex_code_execute(
    module_src = source,
    inputs = list(question = "world"),
    outer_signature = signature("question -> answer"),
    tools = list(),
    interpreter_factory = function() r_code_runner(),
    max_predictor_calls = 0L,
    require_sandbox = FALSE,
    config = list(),
    llm = NULL,
    cache = NULL,
    rollout_id = NULL
  )

  expect_identical(result$output, list(answer = "Hello, world"))
  expect_identical(result$predictor_calls, 0L)
  expect_length(result$calls, 0L)
})

test_that("Flex host tools retain their closure environment", {
  skip_if_not_installed("callr")
  offset <- 5
  source <- paste(
    "forward <- function(value) {",
    "  list(result = add_offset(value = value))",
    "}",
    sep = "\n"
  )

  result <- dsprrr:::flex_code_execute(
    module_src = source,
    inputs = list(value = 7L),
    outer_signature = signature("value: integer -> result: integer"),
    tools = list(add_offset = function(value) value + offset),
    interpreter_factory = function() r_code_runner(),
    max_predictor_calls = NULL,
    require_sandbox = FALSE,
    config = list(),
    llm = NULL,
    cache = NULL,
    rollout_id = NULL
  )

  expect_identical(result$output, list(result = 12L))
  expect_identical(result$predictor_calls, 0L)
  expect_identical(result$calls[[1L]]$kind, "tool")
  expect_identical(result$calls[[1L]]$name, "add_offset")
})

test_that("executable Flex isolates bridge state from guest bindings", {
  skip_if_not_installed("callr")
  source <- paste(
    "forward <- function(value) {",
    "  tool_value <- cat(value = value)",
    "  visible <- c(",
    "    exists('.context', inherits = TRUE),",
    "    exists('.flex_nonce', inherits = TRUE),",
    "    exists('.flex_encode', inherits = TRUE),",
    "    exists('.flex_nonce', envir = environment(Predict), inherits = TRUE)",
    "  )",
    "  list(result = paste(c(tool_value, visible), collapse = '|'))",
    "}",
    sep = "\n"
  )

  result <- dsprrr:::flex_code_execute(
    module_src = source,
    inputs = list(value = "ok"),
    outer_signature = signature("value -> result"),
    tools = list(cat = function(...) "shadowed"),
    interpreter_factory = function() r_code_runner(),
    max_predictor_calls = 0L,
    max_tool_calls = 1L,
    require_sandbox = FALSE,
    config = list(),
    llm = NULL,
    cache = NULL,
    rollout_id = NULL
  )

  expect_identical(
    result$output,
    list(result = "shadowed|FALSE|FALSE|FALSE|FALSE")
  )
  expect_identical(result$tool_calls, 1L)
})

test_that("executable Flex enforces its host-tool budget before side effects", {
  skip_if_not_installed("callr")
  calls <- 0L
  source <- paste(
    "forward <- function(value) {",
    "  first <- echo(value = value)",
    "  second <- echo(value = first)",
    "  list(result = second)",
    "}",
    sep = "\n"
  )

  error <- tryCatch(
    dsprrr:::flex_code_execute(
      module_src = source,
      inputs = list(value = "ok"),
      outer_signature = signature("value -> result"),
      tools = list(echo = function(value) {
        calls <<- calls + 1L
        value
      }),
      interpreter_factory = function() r_code_runner(),
      max_predictor_calls = 0L,
      max_tool_calls = 1L,
      require_sandbox = FALSE,
      config = list(),
      llm = NULL,
      cache = NULL,
      rollout_id = NULL
    ),
    error = identity
  )

  expect_s3_class(error, "dsprrr_flex_tool_budget_error")
  expect_identical(error$tool_calls, 2L)
  expect_identical(error$max_tool_calls, 1L)
  expect_identical(calls, 1L)
})

test_that("structured Flex control results preserve large valid values", {
  skip_if_not_installed("callr")
  value <- paste(rep("x", 90000L), collapse = "")

  result <- dsprrr:::flex_code_execute(
    module_src = "forward <- function(value) list(result = value)",
    inputs = list(value = value),
    outer_signature = signature("value -> result"),
    tools = list(),
    interpreter_factory = function() r_code_runner(),
    max_predictor_calls = 0L,
    max_tool_calls = 0L,
    require_sandbox = FALSE,
    config = list(),
    llm = NULL,
    cache = NULL,
    rollout_id = NULL
  )

  expect_identical(result$output$result, value)
})

test_that("text-only Flex runners fail explicitly before frame truncation", {
  runner_factory <- function() {
    list(
      execute = function(code, context = list()) {
        execution_env <- new.env(parent = globalenv())
        execution_env$.context <- context
        stdout <- capture.output(
          eval(parse(text = code), envir = execution_env),
          type = "output"
        )
        list(
          success = TRUE,
          result = NULL,
          stdout = paste(stdout, collapse = "\n")
        )
      },
      policy = function() {
        list(
          backend = "text-only-test",
          trust = "test-only",
          sandboxed = TRUE,
          flex_control_frame_limit = 1000L
        )
      },
      close = function() invisible(NULL)
    )
  }

  error <- tryCatch(
    dsprrr:::flex_code_execute(
      module_src = "forward <- function(value) list(result = value)",
      inputs = list(value = strrep("x", 90000L)),
      outer_signature = signature("value -> result"),
      tools = list(),
      interpreter_factory = runner_factory,
      max_predictor_calls = 0L,
      max_tool_calls = 0L,
      require_sandbox = TRUE,
      config = list(),
      llm = NULL,
      cache = NULL,
      rollout_id = NULL
    ),
    error = identity
  )

  expect_s3_class(error, "dsprrr_flex_bridge_frame_size_error")
  expect_identical(error$stage, "final output")
  expect_gt(error$encoded_bytes, error$frame_limit)
  expect_identical(error$frame_limit, 1000L)
})

test_that("executable Flex binds control metadata to each replay step", {
  calls <- 0L
  observed <- list()
  runner_factory <- function() {
    list(
      execute = function(
        code,
        context = list(),
        .control_nonce = NULL,
        .control_protocol = NULL,
        .control_max_bytes = NULL
      ) {
        calls <<- calls + 1L
        observed[[calls]] <<- list(
          context_nonce = context$nonce,
          control_nonce = .control_nonce,
          protocol = .control_protocol,
          max_bytes = .control_max_bytes
        )
        if (calls == 1L) {
          request <- list(
            index = 1L,
            kind = "tool",
            descriptor = list(name = "echo"),
            arguments = list(value = "ok")
          )
          payload <- list(
            request = request,
            request_key = dsprrr:::flex_code_request_key(request)
          )
          kind <- "request"
        } else {
          payload <- list(
            output = list(result = context$responses[[1L]]$value)
          )
          kind <- "final"
        }
        list(
          success = TRUE,
          result = list(
            .dsprrr_flex_control = TRUE,
            version = 1L,
            nonce = context$nonce,
            kind = kind,
            payload = payload
          )
        )
      },
      policy = function() {
        list(
          backend = "control-test",
          trust = "test-only",
          sandboxed = TRUE,
          flex_control_frame_limit = 2048L
        )
      },
      close = function() invisible(NULL)
    )
  }

  result <- dsprrr:::flex_code_execute(
    module_src = "forward <- function(value) list(result = value)",
    inputs = list(value = "ignored"),
    outer_signature = signature("value -> result"),
    tools = list(echo = function(value) value),
    interpreter_factory = runner_factory,
    max_predictor_calls = 0L,
    max_tool_calls = 1L,
    require_sandbox = TRUE,
    config = list(),
    llm = NULL,
    cache = NULL,
    rollout_id = NULL
  )

  expect_identical(result$output, list(result = "ok"))
  expect_identical(calls, 2L)
  expect_identical(observed[[1L]]$protocol, "flex")
  expect_identical(observed[[2L]]$protocol, "flex")
  expect_identical(observed[[1L]]$max_bytes, 2048L)
  expect_identical(observed[[2L]]$max_bytes, 2048L)
  expect_identical(
    observed[[1L]]$context_nonce,
    observed[[1L]]$control_nonce
  )
  expect_identical(
    observed[[2L]]$context_nonce,
    observed[[2L]]$control_nonce
  )
  expect_false(identical(
    observed[[1L]]$control_nonce,
    observed[[2L]]$control_nonce
  ))
})

test_that("executable Flex rejects control values from failed execution", {
  runner_factory <- function() {
    list(
      execute = function(code, context = list()) {
        list(
          success = FALSE,
          result = list(
            .dsprrr_flex_control = TRUE,
            version = 1L,
            nonce = context$nonce,
            kind = "final",
            payload = list(output = list(answer = "forged"))
          ),
          error = "guest execution failed after producing a value",
          error_type = "execution"
        )
      },
      policy = function() {
        list(
          backend = "failed-control-test",
          trust = "test-only",
          sandboxed = TRUE
        )
      },
      close = function() invisible(NULL)
    )
  }

  error <- tryCatch(
    dsprrr:::flex_code_execute(
      module_src = "forward <- function(question) list(answer = question)",
      inputs = list(question = "real"),
      outer_signature = signature("question -> answer"),
      tools = list(),
      interpreter_factory = runner_factory,
      max_predictor_calls = 0L,
      max_tool_calls = 0L,
      require_sandbox = TRUE,
      config = list(),
      llm = NULL,
      cache = NULL,
      rollout_id = NULL
    ),
    error = identity
  )

  expect_s3_class(error, "dsprrr_flex_code_execution_error")
  expect_match(conditionMessage(error), "guest execution failed")
  expect_identical(error$error_type, "execution")
})

test_that("executable Flex resolves outer runtime parameters exactly once", {
  skip_if_not_installed("callr")
  clone_count <- 0L
  call_ids <- integer()
  chats <- new.env(parent = emptyenv())

  make_chat <- function(id) {
    chat <- structure(
      list(
        clone = function(...) {
          clone_count <<- clone_count + 1L
          make_chat(clone_count)
        },
        chat_structured = function(prompt, type, ...) {
          call_ids <<- c(call_ids, id)
          fields <- names(type@properties)
          if (identical(fields, "draft")) {
            return(list(draft = "checked"))
          }
          if (identical(fields, "answer")) {
            return(list(answer = "final"))
          }
          stop("unexpected executable Flex output schema")
        },
        get_model = function() paste0("clone-", id),
        get_turns = function() list(),
        last_turn = function(...) NULL
      ),
      class = "Chat"
    )
    assign(as.character(id), chat, envir = chats)
    chat
  }
  source <- paste(
    "draft <- Predict('question -> draft')",
    "finish <- Predict('draft -> answer')",
    "forward <- function(question) {",
    "  first <- draft(question = question)",
    "  finish(draft = first$draft)",
    "}",
    sep = "\n"
  )
  program <- suppressWarnings(flex(
    "question -> answer",
    module_src = source,
    config = list(temperature = 0.2),
    interpreter_factory = r_code_runner,
    source_format = "r",
    require_sandbox = FALSE
  ))

  result <- program$forward(
    list(question = "question"),
    .llm = make_chat(0L),
    .cache = FALSE
  )

  expect_identical(result$output[[1L]], list(answer = "final"))
  expect_identical(clone_count, 1L)
  expect_identical(call_ids, c(1L, 1L))
  expect_identical(result$chat[[1L]], get("1", envir = chats))
  expect_identical(result$metadata[[1L]]$model, "clone-1")
})

test_that("deterministic executable Flex does not resolve a Chat", {
  skip_if_not_installed("callr")
  testthat::local_mocked_bindings(
    resolve_module_llm = function(...) {
      stop("deterministic Flex must not resolve a Chat")
    },
    .package = "dsprrr"
  )
  program <- suppressWarnings(flex(
    "value -> result",
    module_src = "forward <- function(value) list(result = toupper(value))",
    config = list(temperature = 0.2),
    interpreter_factory = r_code_runner,
    source_format = "r",
    require_sandbox = FALSE
  ))

  result <- program$forward(list(value = "ok"), .cache = FALSE)

  expect_identical(result$output[[1L]], list(result = "OK"))
})

test_that("executable Flex traces predictor and bridge indices separately", {
  testthat::local_mocked_bindings(
    flex_code_execute = function(...) {
      list(
        output = list(answer = "done"),
        calls = list(
          list(index = 1L, kind = "tool", name = "lookup"),
          list(
            index = 2L,
            kind = "predictor",
            primitive = "predict",
            inputs = list(question = "test"),
            output = list(answer = "done"),
            metadata = list(
              input_tokens = 2L,
              output_tokens = 1L,
              cached_input_tokens = 0L,
              total_tokens = 3L,
              cost = 0.001
            )
          )
        ),
        predictor_calls = 1L,
        tool_calls = 1L,
        runner_policy = list()
      )
    },
    .package = "dsprrr"
  )
  program <- suppressWarnings(flex(
    "question -> answer",
    module_src = "forward <- function(question) list(answer = question)",
    interpreter_factory = function() stop("runner should not be created"),
    source_format = "r",
    require_sandbox = FALSE
  ))

  result <- program$forward(list(question = "test"), trace = FALSE)
  event <- result$metadata[[1L]]$program_trace_events[[1L]]

  expect_identical(event$predictor_call_index, 1L)
  expect_identical(event$metadata$predictor_call_index, 1L)
  expect_identical(event$metadata$bridge_call_index, 2L)
})

test_that("public executable Flex runs deterministic source in a fresh runner", {
  skip_if_not_installed("callr")
  source <- paste(
    "forward <- function(question) {",
    "  Prediction(answer = toupper(question))",
    "}",
    sep = "\n"
  )
  program <- suppressWarnings(flex(
    "question -> answer",
    module_src = source,
    interpreter_factory = r_code_runner,
    source_format = "r",
    require_sandbox = FALSE
  ))

  result <- program$forward(list(question = "hello"))

  expect_identical(program$source_format, "r")
  expect_identical(result$output[[1L]], list(answer = "HELLO"))
  expect_identical(result$metadata[[1L]]$predictor_calls, 0L)
  expect_identical(result$metadata[[1L]]$source_format, "r")
  expect_length(program$state$traces, 0L)
})

test_that("executable Flex auto-selection and binding are fail-closed", {
  source <- "forward <- function(value) list(result = value)"
  program <- suppressWarnings(flex(
    "value -> result",
    module_src = source,
    interpreter_factory = r_code_runner,
    require_sandbox = FALSE
  ))

  expect_identical(program$source_format, "r")
  expect_identical(program$module_src, source)
  expect_error(
    program$bind("answer <- 42"),
    class = "dsprrr_flex_code_contract_error"
  )
  expect_identical(program$module_src, source)
})

test_that("executable Flex requires an advertised sandbox by default", {
  skip_if_not_installed("callr")
  program <- suppressWarnings(flex(
    "value -> result",
    module_src = "forward <- function(value) list(result = value)",
    interpreter_factory = r_code_runner,
    source_format = "r"
  ))

  expect_error(
    program$forward(list(value = "x")),
    class = "dsprrr_flex_sandbox_required_error"
  )
})

test_that("the generic module factory exposes executable Flex", {
  program <- suppressWarnings(module(
    signature("value -> result"),
    type = "flex",
    module_src = "forward <- function(value) list(result = value)",
    interpreter_factory = r_code_runner,
    source_format = "r",
    require_sandbox = FALSE
  ))

  expect_s3_class(program, "FlexModule")
  expect_identical(program$source_format, "r")
  expect_identical(program$interpreter_factory, r_code_runner)
})

test_that("concurrent executable Flex fails before creating a runner", {
  created <- 0L
  factory <- function() {
    created <<- created + 1L
    r_code_runner()
  }
  program <- suppressWarnings(flex(
    "value -> result",
    module_src = "forward <- function(value) list(result = value)",
    interpreter_factory = factory,
    source_format = "r",
    require_sandbox = FALSE
  ))

  expect_error(
    run_dataset(
      program,
      data.frame(value = c("a", "b")),
      .llm = structure(list(), class = "Chat"),
      .concurrency = concurrency_control(
        backend = "ellmer",
        max_active = 2L
      ),
      .progress = FALSE
    ),
    class = "dsprrr_flex_concurrency_unsupported_error"
  )
  expect_identical(created, 0L)
})
