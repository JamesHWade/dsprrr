factory_test_log <- function() {
  log <- new.env(parent = emptyenv())
  log$created <- integer()
  log$executed <- integer()
  log$closed <- integer()
  log
}

factory_test_runner <- function(
  id,
  log,
  result = list(
    success = TRUE,
    result = 42,
    stdout = "",
    stderr = "",
    messages = "",
    warnings = "",
    error = NULL,
    duration_ms = 1
  ),
  close_error = NULL
) {
  closed <- FALSE
  list(
    execute = function(code, context = list(), ...) {
      if (closed) {
        cli::cli_abort(
          "test runner is closed",
          class = "dsprrr_interpreter_closed_error"
        )
      }
      log$executed <- c(log$executed, id)
      result
    },
    policy = function() {
      list(
        backend = paste0("test-", id),
        trust = "test-only",
        sandboxed = TRUE,
        persistent = FALSE,
        secret = "must-not-enter-metadata"
      )
    },
    close = function() {
      log$closed <- c(log$closed, id)
      if (!is.null(close_error)) {
        stop(close_error)
      }
      closed <<- TRUE
      invisible(NULL)
    }
  )
}

factory_test_factory <- function(log, result = NULL, close_error = NULL) {
  force(log)
  force(result)
  force(close_error)
  function() {
    id <- length(log$created) + 1L
    log$created <- c(log$created, id)
    args <- list(id = id, log = log, close_error = close_error)
    if (!is.null(result)) {
      args$result <- result
    }
    do.call(factory_test_runner, args)
  }
}

factory_test_pot_chat <- function(error = NULL) {
  chat <- NULL
  chat <- list(
    clone = function() chat,
    chat_structured = function(prompt, type, ...) {
      if (!is.null(error)) {
        stop(error)
      }
      list(code = "42", explanation = "constant")
    },
    chat = function(prompt, ...) "42"
  )
  chat
}

test_that("interpreter factories are validated without being invoked", {
  log <- factory_test_log()
  factory <- factory_test_factory(log)

  modules <- list(
    program_of_thought(
      "question -> answer",
      interpreter_factory = factory
    ),
    code_act(
      "question -> answer",
      interpreter_factory = factory
    ),
    rlm_module(
      "question -> answer",
      interpreter_factory = factory
    )
  )

  expect_length(log$created, 0L)
  invisible(lapply(modules, print))
  copies <- lapply(modules, function(module) module$reset_copy())
  deep_copies <- lapply(modules, function(module) module$deepcopy())
  expect_length(log$created, 0L)
  expect_true(all(vapply(
    copies,
    function(module) identical(module$interpreter_factory, factory),
    logical(1)
  )))
  expect_true(all(vapply(
    deep_copies,
    function(module) identical(module$interpreter_factory, factory),
    logical(1)
  )))

  expect_error(
    program_of_thought(
      "question -> answer",
      runner = factory_test_runner(1L, log),
      interpreter_factory = factory
    ),
    class = "dsprrr_interpreter_binding_error"
  )
  expect_error(
    code_act("question -> answer", interpreter_factory = function(x) x),
    class = "dsprrr_interpreter_factory_error"
  )
  expect_error(
    rlm_module("question -> answer", interpreter_factory = "not a factory"),
    class = "dsprrr_interpreter_factory_error"
  )
})

test_that("module factory exposes Flex and invocation-owned runtimes", {
  log <- factory_test_log()
  factory <- factory_test_factory(log)
  sig <- signature("question -> answer")

  runtime_modules <- list(
    module(
      sig,
      type = "program_of_thought",
      interpreter_factory = factory
    ),
    module(sig, type = "codeact", interpreter_factory = factory),
    module(sig, type = "rlm", interpreter_factory = factory)
  )
  flex_module <- suppressWarnings(
    module(sig, type = "flex", max_predictor_calls = 7L)
  )

  expect_true(all(vapply(
    runtime_modules,
    function(module) identical(module$interpreter_factory, factory),
    logical(1)
  )))
  expect_s3_class(flex_module, "FlexModule")
  expect_identical(flex_module$max_predictor_calls, 7L)
  expect_identical(flex_module$config$.module_kind, "flex")
  expect_length(log$created, 0L)

  expect_error(
    suppressWarnings(
      module(sig, type = "flex", max_predictor_call = 7L)
    ),
    "`...` must be empty"
  )
})

test_that("module factory rejects type-specific arguments for other types", {
  sig <- signature("question -> answer")

  expect_error(
    module(sig, type = "predict", interpreter_factory = function() NULL),
    class = "dsprrr_module_type_argument_error"
  )
  expect_error(
    module(sig, type = "predict", module_src = "{}"),
    class = "dsprrr_module_type_argument_error"
  )
  expect_error(
    module(sig, type = "predict", max_predictor_calls = 100L),
    class = "dsprrr_module_type_argument_error"
  )

  # Preserve the pre-existing generic-factory behavior for runner.
  expect_s3_class(
    module(sig, type = "predict", runner = factory_test_runner(1L)),
    "PredictModule"
  )
})

test_that("new factory arguments preserve existing positional APIs", {
  log <- factory_test_log()
  runner <- factory_test_runner(1L, log)

  pot <- program_of_thought(
    "question -> answer",
    runner,
    5L,
    FALSE
  )
  codeact <- code_act(
    "question -> answer",
    list(),
    runner,
    6L
  )
  rlm <- rlm_module(
    "question -> answer",
    runner,
    7L,
    8L,
    900L,
    NULL,
    TRUE,
    list()
  )

  expect_identical(pot$runner, runner)
  expect_identical(pot$max_iters, 5L)
  expect_false(pot$extract_answer)
  expect_identical(codeact$runner, runner)
  expect_identical(codeact$max_iterations, 6L)
  expect_identical(rlm$runner, runner)
  expect_identical(rlm$max_iterations, 7L)
  expect_identical(rlm$max_llm_calls, 8L)
  expect_identical(rlm$max_output_chars, 900L)
  expect_true(rlm$verbose)

  expect_identical(
    names(formals(program_of_thought))[seq_len(5L)],
    c("signature", "runner", "max_iters", "extract_answer", "...")
  )
  expect_identical(
    names(formals(code_act))[seq_len(5L)],
    c("signature", "tools", "runner", "max_iterations", "...")
  )
  expect_identical(
    names(formals(rlm_module))[seq_len(10L)],
    c(
      "signature",
      "runner",
      "max_iterations",
      "max_llm_calls",
      "max_output_chars",
      "sub_lm",
      "verbose",
      "tools",
      "max_iters",
      "..."
    )
  )
  expect_identical(
    names(formals(module))[seq_len(15L)],
    c(
      "signature",
      "type",
      "tools",
      "max_iterations",
      "M",
      "temperature",
      "runner",
      "max_iters",
      "extract_answer",
      "template",
      "demos",
      "config",
      "chat",
      "...",
      "interpreter_factory"
    )
  )
})

test_that("factory-created runners are fresh and closed per invocation", {
  log <- factory_test_log()
  factory <- factory_test_factory(log)
  module <- program_of_thought(
    "question -> answer",
    extract_answer = FALSE,
    interpreter_factory = factory
  )
  chat <- factory_test_pot_chat()

  first <- module$forward(list(question = "first"), .llm = chat)
  second <- module$forward(list(question = "second"), .llm = chat)

  expect_identical(log$created, c(1L, 2L))
  expect_identical(log$executed, c(1L, 2L))
  expect_identical(log$closed, c(1L, 2L))
  expect_identical(first$output[[1L]]$answer, "42")
  expect_identical(second$output[[1L]]$answer, "42")
  policy <- first$metadata[[1L]]$runner_policy
  expect_named(
    policy,
    c("backend", "trust", "sandboxed", "persistent"),
    ignore.order = FALSE
  )
  expect_false("secret" %in% names(policy))
  expect_identical(
    first$metadata[[1L]]$runner_lifecycle,
    "invocation-owned"
  )
})

test_that("runtime mutation cannot bypass the runner/factory XOR", {
  log <- factory_test_log()
  factory <- factory_test_factory(log)
  module <- program_of_thought(
    "question -> answer",
    interpreter_factory = factory
  )

  module$runner <- factory_test_runner(99L, log)
  expect_error(
    module$forward(
      list(question = "test"),
      .llm = factory_test_pot_chat()
    ),
    class = "dsprrr_interpreter_binding_error"
  )
  expect_length(log$created, 0L)

  module$runner <- NULL
  module$interpreter_factory <- NULL
  expect_error(
    module$forward(
      list(question = "test"),
      .llm = factory_test_pot_chat()
    ),
    class = "dsprrr_interpreter_binding_error"
  )
  expect_length(log$created, 0L)
})

test_that("caller-owned runners are reused and never automatically closed", {
  log <- factory_test_log()
  runner <- factory_test_runner(7L, log)
  module <- program_of_thought(
    "question -> answer",
    runner = runner,
    extract_answer = FALSE
  )
  chat <- factory_test_pot_chat()

  module$forward(list(question = "first"), .llm = chat)
  module$forward(list(question = "second"), .llm = chat)

  expect_identical(log$executed, c(7L, 7L))
  expect_length(log$closed, 0L)
  expect_identical(module$deepcopy()$runner, runner)
})

test_that("leases close on module and runner protocol errors", {
  module_errors <- list(
    program_of_thought = function(factory) {
      program_of_thought(
        "question -> answer",
        interpreter_factory = factory
      )$forward(
        list(question = "test"),
        .llm = factory_test_pot_chat("POT failed")
      )
    },
    code_act = function(factory) {
      chat <- NULL
      chat <- list(
        clone = function() chat,
        register_tool = function(tool) stop("CodeAct failed")
      )
      code_act(
        "question -> answer",
        interpreter_factory = factory
      )$forward(list(question = "test"), .llm = chat)
    },
    rlm = function(factory) {
      chat <- NULL
      chat <- list(
        clone = function() chat,
        chat_structured = function(...) stop("RLM failed")
      )
      rlm_module(
        "question -> answer",
        interpreter_factory = factory
      )$forward(list(question = "test"), .llm = chat)
    }
  )

  for (name in names(module_errors)) {
    log <- factory_test_log()
    expect_error(
      module_errors[[name]](factory_test_factory(log))
    )
    expect_identical(log$created, 1L, info = name)
    expect_identical(log$closed, 1L, info = name)
  }

  log <- factory_test_log()
  malformed <- list(success = TRUE, result = 42, stdout = character())
  module <- program_of_thought(
    "question -> answer",
    interpreter_factory = factory_test_factory(log, result = malformed)
  )
  expect_error(
    module$forward(
      list(question = "test"),
      .llm = factory_test_pot_chat()
    ),
    class = "dsprrr_code_runner_protocol_error"
  )
  expect_identical(log$closed, 1L)
})

test_that("leases reject an unusable close contract before execution", {
  log <- factory_test_log()
  factory <- function() {
    log$created <- c(log$created, 1L)
    runner <- factory_test_runner(1L, log)
    runner$close <- function(force) {
      if (isTRUE(force)) {
        log$closed <- c(log$closed, 1L)
      }
    }
    runner
  }
  module <- program_of_thought(
    "question -> answer",
    interpreter_factory = factory,
    extract_answer = FALSE
  )

  expect_error(
    module$forward(
      list(question = "test"),
      .llm = factory_test_pot_chat()
    ),
    class = "dsprrr_interpreter_factory_error"
  )
  expect_identical(log$created, 1L)
  expect_length(log$executed, 0L)
  expect_length(log$closed, 0L)
})

test_that("lease error precedence preserves cleanup diagnostics", {
  log <- factory_test_log()
  factory <- factory_test_factory(log, close_error = "close failed")
  condition <- tryCatch(
    dsprrr:::with_code_runner_lease(
      runner = NULL,
      interpreter_factory = factory,
      module_name = "test module",
      code = function(runner, lease) stop("body failed")
    ),
    error = identity
  )

  expect_match(conditionMessage(condition), "body failed", fixed = TRUE)
  close_condition <- attr(condition, "dsprrr_interpreter_close_error")
  expect_s3_class(close_condition, "error")
  expect_match(conditionMessage(close_condition), "close failed", fixed = TRUE)

  expect_error(
    dsprrr:::with_code_runner_lease(
      runner = NULL,
      interpreter_factory = factory,
      module_name = "test module",
      code = function(runner, lease) "success"
    ),
    class = "dsprrr_interpreter_close_error"
  )
})

test_that("lease cleanup runs for interrupts without changing the condition", {
  log <- factory_test_log()
  interrupt <- structure(
    list(message = "test interrupt", call = NULL),
    class = c("test_interrupt", "interrupt", "condition")
  )
  condition <- tryCatch(
    dsprrr:::with_code_runner_lease(
      runner = NULL,
      interpreter_factory = factory_test_factory(log),
      module_name = "test module",
      code = function(runner, lease) stop(interrupt)
    ),
    interrupt = identity
  )

  expect_s3_class(condition, "test_interrupt")
  expect_identical(conditionMessage(condition), "test interrupt")
  expect_identical(log$closed, 1L)
})

test_that("invalid factory runners are closed when possible", {
  closes <- 0L
  invalid_factory <- function() {
    list(
      execute = function(code, context = list()) NULL,
      close = function() closes <<- closes + 1L
    )
  }

  expect_error(
    dsprrr:::acquire_code_runner(
      runner = NULL,
      interpreter_factory = invalid_factory,
      module_name = "test module"
    ),
    class = "dsprrr_interpreter_factory_error"
  )
  expect_identical(closes, 1L)
})

test_that("factory runner cleanup preserves policy interrupts", {
  closes <- 0L
  interrupt <- structure(
    list(message = "policy interrupted", call = NULL),
    class = c("factory_policy_interrupt", "interrupt", "condition")
  )
  factory <- function() {
    list(
      execute = function(code, context = list()) NULL,
      policy = function() stop(interrupt),
      close = function() closes <<- closes + 1L
    )
  }

  condition <- tryCatch(
    dsprrr:::acquire_code_runner(
      runner = NULL,
      interpreter_factory = factory,
      module_name = "test module"
    ),
    interrupt = identity
  )
  expect_s3_class(condition, "factory_policy_interrupt")
  expect_identical(closes, 1L)
})

test_that("runner results are normalized and malformed results fail closed", {
  normalized <- dsprrr:::validate_code_runner_result(list(
    success = TRUE,
    result = NULL
  ))
  expect_identical(normalized$stdout, "")
  expect_identical(normalized$stderr, "")
  expect_identical(normalized$messages, "")
  expect_identical(normalized$warnings, "")
  expect_null(normalized$error)
  expect_true(is.na(normalized$duration_ms))

  invalid <- list(
    unnamed = unname(list(success = TRUE, result = 1)),
    empty_name = stats::setNames(
      list(TRUE, 1),
      c("success", "")
    ),
    duplicate_name = stats::setNames(
      list(TRUE, 1),
      c("success", "success")
    ),
    invalid_success = list(success = NA, result = 1),
    failed_without_error = list(success = FALSE, result = NULL),
    invalid_duration = list(
      success = TRUE,
      result = 1,
      duration_ms = -1
    )
  )
  for (name in names(invalid)) {
    expect_error(
      dsprrr:::validate_code_runner_result(invalid[[name]]),
      class = "dsprrr_code_runner_protocol_error",
      info = name
    )
  }
})

test_that("runner policies reject ambiguous or malformed metadata", {
  execute <- function(code, context = list()) {
    list(success = TRUE, result = NULL)
  }
  invalid <- list(
    unnamed = list(
      execute = execute,
      policy = function() unname(list("test", "test", TRUE))
    ),
    duplicate = list(
      execute = execute,
      policy = function() {
        stats::setNames(
          list("test", "shadow", "test", TRUE),
          c("backend", "backend", "trust", "sandboxed")
        )
      }
    ),
    empty = list(
      execute = execute,
      policy = function() {
        stats::setNames(
          list("test", "test", TRUE),
          c("backend", "", "sandboxed")
        )
      }
    )
  )

  for (name in names(invalid)) {
    expect_error(
      dsprrr:::validate_code_runner(invalid[[name]]),
      class = "dsprrr_code_runner_protocol_error",
      info = name
    )
  }
})

test_that("CodeAct retained tools cannot outlive a factory lease", {
  log <- factory_test_log()
  registered <- list()
  chat <- NULL
  chat <- list(
    clone = function() chat,
    register_tool = function(tool) {
      registered[[as.character(tool@name)]] <<- tool
      invisible(NULL)
    },
    chat = function(prompt, ...) "done",
    get_turns = function() {
      list(list(
        role = "assistant",
        contents = list("done")
      ))
    }
  )
  module <- code_act(
    "question -> answer",
    interpreter_factory = factory_test_factory(log)
  )

  result <- module$forward(list(question = "test"), .llm = chat)

  expect_identical(result$output[[1L]]$answer, "done")
  expect_identical(log$closed, 1L)
  expect_error(
    registered$execute_r_code(code = "1 + 1"),
    class = "dsprrr_interpreter_closed_error"
  )
})

test_that("specialized direct async rejects while run_stream falls back", {
  log <- factory_test_log()
  module <- program_of_thought(
    "question -> answer",
    interpreter_factory = factory_test_factory(log)
  )
  flex_module <- suppressWarnings(flex("question -> answer"))

  expect_error(
    run_async(module, question = "test"),
    class = "dsprrr_specialized_async_unsupported"
  )
  expect_error(
    stream_async(module, question = "test"),
    class = "dsprrr_specialized_async_unsupported"
  )
  expect_error(
    module$stream(question = "test"),
    class = "dsprrr_specialized_async_unsupported"
  )
  streamed <- run_stream(
    module,
    question = "test",
    .llm = factory_test_pot_chat()
  )
  expect_identical(streamed$answer, "42")
  expect_error(
    run_async(flex_module, question = "test"),
    class = "dsprrr_specialized_async_unsupported"
  )
  expect_identical(log$created, 1L)
  expect_identical(log$closed, 1L)
})

test_that("built-in runners become terminal after close", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 1)
  runner$close()
  expect_invisible(runner$close())
  expect_error(
    runner$execute("1 + 1"),
    class = "dsprrr_interpreter_closed_error"
  )

  closes <- 0L
  mcp <- dsprrr:::McpReplRunner$new(
    repl = function(input, timeout_ms) "ok",
    timeout = 1,
    max_output_chars = 100L,
    sandbox = "workspace-write",
    sandbox_verified = TRUE,
    oversized_output = "files",
    close_connection = function() closes <<- closes + 1L,
    connection_owned = TRUE
  )
  mcp$close()
  mcp$close()
  expect_identical(closes, 1L)
  expect_error(
    mcp$execute("1 + 1"),
    class = "dsprrr_interpreter_closed_error"
  )
  expect_error(
    mcp$reset(),
    class = "dsprrr_interpreter_closed_error"
  )
})
