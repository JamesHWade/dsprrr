# Tests for RLMModule (Recursive Language Model)

# Helper: Create a mock LLM for RLM testing
create_mock_rlm_llm <- function(
  code_responses = list(),
  fallback_chat = "fallback answer",
  fallback_structured = list(answer = fallback_chat),
  .state = NULL
) {
  if (is.null(.state)) {
    .state <- new.env(parent = emptyenv())
    .state$call_count <- 0L
  }
  new_test_chat(
    chat_structured = function(prompt, type, ...) {
      type_fields <- if (
        inherits(type, "ellmer::TypeObject") &&
          methods::.hasSlot(type, "properties")
      ) {
        names(type@properties)
      } else {
        character()
      }

      is_code_gen <- all(c("reasoning", "code") %in% type_fields)

      if (is_code_gen) {
        .state$call_count <- .state$call_count + 1L
        if (.state$call_count <= length(code_responses)) {
          return(code_responses[[.state$call_count]])
        }

        # Default: simple SUBMIT
        return(list(
          reasoning = "Returning default answer",
          code = "SUBMIT('default answer')"
        ))
      }

      if (!is.null(fallback_structured)) {
        fallback_structured
      } else {
        stop("structured fallback unavailable")
      }
    },
    chat = function(prompt, ...) {
      fallback_chat
    }
  )
}

as_test_rlm_chat <- function(chat) {
  if (inherits(chat, "Chat") && R6::is.R6(chat)) {
    return(chat)
  }
  chat_fn <- chat[["chat"]]
  structured_fn <- chat[["chat_structured"]]
  converted <- new_test_chat(
    chat = if (is.function(chat_fn)) chat_fn else function(...) "unused",
    chat_structured = if (is.function(structured_fn)) {
      structured_fn
    } else {
      function(...) list(answer = "unused")
    }
  )
  for (name in c("get_turns", "set_turns", "last_turn", "get_model")) {
    method <- chat[[name]]
    if (is.function(method)) {
      override_test_chat_method(converted, name, method)
    }
  }
  clone <- chat[["clone"]]
  if (is.function(clone)) {
    override_test_chat_method(converted, "clone", function(deep = TRUE) {
      args <- names(formals(clone))
      cloned <- if (any(c("deep", "...") %in% args)) {
        clone(deep = deep)
      } else {
        clone()
      }
      as_test_rlm_chat(cloned)
    })
  }
  converted
}

# ============================================================================
# Factory Function Tests
# ============================================================================

test_that("rlm_module requires an ellmer Chat for sub_lm", {
  expect_error(
    rlm_module(
      "question -> answer",
      interpreter_factory = function() stop("not used"),
      sub_lm = list(chat = function(prompt) "duck typed")
    ),
    class = "dsprrr_rlm_sub_lm_error"
  )
})

test_that("rlm forwards structured one-shot results with an explicit runner", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  chat <- create_mock_rlm_llm(list(list(
    reasoning = "Return the result",
    code = "SUBMIT(42L)"
  )))

  result <- rlm(
    "question -> answer: integer",
    question = "What is the answer?",
    .llm = chat,
    .runner = runner,
    .max_llm_calls = 0L,
    .return_format = "structured"
  )

  expect_s3_class(result, "dsprrr_result")
  expect_named(result, c("output", "chat", "metadata"))
  expect_identical(result$output$answer, 42L)
  expect_identical(result$metadata$runner_lifecycle, "caller-owned")
})

test_that("run treats every RLM input as one rich context object", {
  skip_if_not_installed("callr")
  # Instrumented coverage runs can spend more than ten seconds starting the
  # persistent callr session before this rich context is staged.
  runner <- r_code_runner(timeout = 60, persistent = TRUE)
  withr::defer(runner$shutdown())
  chat <- create_mock_rlm_llm(list(list(
    reasoning = "Verify the staged R objects",
    code = paste(
      "stopifnot(",
      "identical(.context$values, c(2L, 4L, 8L)),",
      "identical(.context$unnamed, list('alpha', 2L)),",
      "identical(.context$named, list(alpha = 1L, beta = 'two')),",
      "is.matrix(.context$grid),",
      "identical(dim(.context$grid), c(2L, 2L)),",
      "inherits(.context$model, 'lm')",
      ") ; SUBMIT(42L)"
    )
  )))

  result <- run(
    rlm_module(
      "values, unnamed, named, grid, model -> answer: integer",
      runner = runner,
      max_llm_calls = 0L
    ),
    values = c(2L, 4L, 8L),
    unnamed = list("alpha", 2L),
    named = list(alpha = 1L, beta = "two"),
    grid = matrix(1:4, nrow = 2L),
    model = stats::lm(mpg ~ wt, data = mtcars),
    .llm = chat
  )

  expect_identical(result, list(answer = 42L))
})

test_that("run_dataset batches rich contexts with one owned runner per row", {
  skip_if_not_installed("callr")
  lifecycle <- new.env(parent = emptyenv())
  lifecycle$created <- 0L
  lifecycle$closed <- 0L
  factory <- function() {
    lifecycle$created <- lifecycle$created + 1L
    inner <- r_code_runner(timeout = 10, persistent = TRUE)
    list(
      execute = function(code, context = list(), .control_nonce = NULL) {
        inner$execute(code, context = context)
      },
      prepare_context = function(...) inner$prepare_context(...),
      policy = function() inner$policy(),
      shutdown = function() {
        lifecycle$closed <- lifecycle$closed + 1L
        inner$shutdown()
      }
    )
  }
  chat <- create_mock_rlm_llm(list(
    list(
      reasoning = "Read the first rich-context value",
      code = "SUBMIT(.context$records$value[[1L]])"
    ),
    list(
      reasoning = "Read the first rich-context value",
      code = "SUBMIT(.context$records$value[[1L]])"
    )
  ))
  records <- list(
    data.frame(value = 10L, label = "first"),
    data.frame(value = 20L, label = "second")
  )

  module <- rlm_module(
    "records, question -> answer: integer",
    interpreter_factory = factory,
    max_llm_calls = 0L
  )
  result <- run_dataset(
    module,
    tibble::tibble(
      records = records,
      question = c("first", "second")
    ),
    .llm = chat,
    .progress = FALSE,
    .return_format = "structured"
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(is.na(result$.error)))
  expect_identical(
    result$result,
    list(list(answer = 10L), list(answer = 20L))
  )
  expect_identical(lifecycle$created, 2L)
  expect_identical(lifecycle$closed, 2L)
})

test_that("one-row factory RLM datasets keep the named result record", {
  skip_if_not_installed("callr")
  module <- rlm_module(
    "question -> answer: integer",
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    max_llm_calls = 0L
  )
  chat <- create_mock_rlm_llm(list(list(
    reasoning = "Return one value",
    code = "SUBMIT(7L)"
  )))

  result <- run_dataset(
    module,
    data.frame(question = "one"),
    .llm = chat,
    .progress = FALSE
  )

  expect_identical(result$result, list(list(answer = 7L)))
})

test_that("run_dataset reuses a caller-owned RLM runner sequentially by row", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  chat <- new_test_chat(
    chat_structured = function(...) {
      list(
        reasoning = "Return this row",
        code = "SUBMIT(.context$question)"
      )
    },
    chat = function(...) {
      stop("recursive queries are disabled", call. = FALSE)
    },
    model = "caller-owned-dataset-test"
  )
  module <- rlm_module(
    "question -> answer: string",
    runner = runner,
    max_llm_calls = 0L
  )

  result <- run_dataset(
    module,
    data.frame(question = c("first", "second")),
    .llm = chat,
    .progress = FALSE
  )

  expect_identical(
    result$result,
    list(list(answer = "first"), list(answer = "second"))
  )
  expect_error(
    run_dataset(
      module,
      data.frame(question = c("first", "second")),
      .llm = chat,
      .concurrency = concurrency_control(backend = "mirai", max_active = 2L),
      .progress = FALSE
    ),
    class = "dsprrr_batch_unsupported_module"
  )
})

test_that("rlm rejects conflicting execution ownership", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())

  expect_error(
    rlm(
      "question -> answer",
      question = "unused",
      .runner = runner,
      .interpreter_factory = function() r_code_runner(persistent = TRUE)
    ),
    class = "dsprrr_interpreter_binding_error"
  )
})

test_that("rlm constructs its managed default lazily and forwards run controls", {
  captured <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(
    rlm_module = function(...) {
      captured$module <- list(...)
      structure(list(), class = "mock_rlm_module")
    },
    run = function(module, ...) {
      captured$run <- list(module = module, arguments = list(...))
      "result"
    },
    mcp_repl_runner = function(...) {
      captured$mcp <- list(...)
      "managed runner"
    },
    .package = "dsprrr"
  )

  result <- rlm(
    "question -> answer",
    question = "inspect",
    .llm = "chat",
    .timeout = 17,
    .max_output_chars = 321L,
    .return_format = "structured"
  )

  expect_identical(result, "result")
  expect_null(captured$module$runner)
  expect_null(captured$mcp)
  expect_identical(captured$run$arguments$question, "inspect")
  expect_identical(captured$run$arguments$.return_format, "structured")
  expect_identical(captured$module$interpreter_factory(), "managed runner")
  expect_identical(captured$mcp, list(timeout = 17, max_output_chars = 321L))
})

test_that("rlm_module requires runner", {
  expect_error(
    rlm_module("question -> answer"),
    "RLM requires an explicit runner"
  )
})

test_that("rlm_module validates runner type", {
  expect_error(
    rlm_module("question -> answer", runner = "not a runner"),
    "runner must implement the dsprrr code-runner protocol"
  )
})

test_that("rlm_module creates RLMModule", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module("question -> answer", runner = runner)

  expect_s3_class(rlm, "RLMModule")
  expect_s3_class(rlm, "Module")
})

test_that("rlm_module accepts string signature", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module("document, question -> answer", runner = runner)

  expect_equal(length(rlm$signature@inputs), 2)
})

test_that("rlm_module rejects opaque JSON Schema outputs before execution", {
  schema_type <- ellmer::TypeJsonSchema(
    json = list(
      type = "object",
      properties = list(n = list(type = "integer")),
      required = list("n"),
      additionalProperties = FALSE
    )
  )
  sig <- signature(
    inputs = list(input("question")),
    output_type = schema_type
  )
  runner <- list(
    execute = function(...) stop("must not execute"),
    policy = function() {
      list(
        backend = "test",
        trust = "test-only",
        sandboxed = TRUE,
        persistent = TRUE
      )
    }
  )

  expect_error(
    rlm_module(sig, runner = runner),
    class = "dsprrr_rlm_json_schema_unsupported"
  )
})

test_that("rlm_module accepts Signature object", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  sig <- signature("question -> answer")
  rlm <- rlm_module(sig, runner = runner)

  expect_s3_class(rlm, "RLMModule")
})

test_that("rlm_module respects max_iterations", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer: integer",
    runner = runner,
    max_iterations = 10
  )

  expect_equal(rlm$max_iterations, 10L)
})

test_that("rlm_module respects max_llm_calls", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_llm_calls = 25
  )

  expect_equal(rlm$max_llm_calls, 25L)
})

test_that("rlm_module validates tools parameter", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())

  # Must be a list
  expect_error(
    rlm_module("question -> answer", runner = runner, tools = "not a list"),
    "tools must be a named list"
  )

  # If not empty, must be named
  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(function() {})
    ),
    "tools must be a named list"
  )
})

test_that("rlm_module validates all tools are functions", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())

  # Non-function in tools list
  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(good = function() {}, bad = "not a function")
    ),
    "All tools must be functions"
  )

  # Multiple non-functions
  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(a = 1, b = "string", c = function() {})
    ),
    "All tools must be functions"
  )
})

test_that("rlm_module validates tool names", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())

  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(`bad-name` = function() 1)
    ),
    "valid R identifiers"
  )

  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(SUBMIT = function() 1)
    ),
    "conflict with built-in RLM tools"
  )

  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(llm_query = function() "x")
    ),
    "conflict with built-in RLM tools"
  )

  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = list(print = function(...) NULL)
    ),
    "conflict with built-in RLM tools"
  )

  duplicate_tools <- structure(
    list(function() 1, function() 2),
    names = c("duplicate", "duplicate")
  )
  expect_error(
    rlm_module(
      "question -> answer",
      runner = runner,
      tools = duplicate_tools
    ),
    "must be unique"
  )

  for (invalid_name in c(NA_character_, "...", "..1")) {
    invalid_tools <- structure(list(function() 1), names = invalid_name)
    expect_error(
      rlm_module(
        "question -> answer",
        runner = runner,
        tools = invalid_tools
      ),
      class = "dsprrr_rlm_tools_error",
      info = paste("tool name", invalid_name)
    )
  }

  for (reserved_name in c(
    ".context",
    ".rlm_output_fields",
    "list",
    "length",
    "names"
  )) {
    reserved_tools <- stats::setNames(list(function() 1), reserved_name)
    expect_error(
      rlm_module(
        "question -> answer",
        runner = runner,
        tools = reserved_tools
      ),
      class = "dsprrr_rlm_tools_error",
      info = paste("reserved tool", reserved_name)
    )
  }
})

test_that("RLMModule constructor also enforces tool and signature contracts", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())

  expect_error(
    signature("question, question -> answer"),
    "input field names must be unique"
  )
  expect_error(
    RLMModule$new(
      signature = signature("question -> answer"),
      runner = runner,
      tools = structure(
        list(function() 1, function() 2),
        names = c("duplicate", "duplicate")
      )
    ),
    class = "dsprrr_rlm_tools_error"
  )
})

test_that("rlm_module validates max_iterations bounds", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())

  expect_error(
    rlm_module("question -> answer", runner = runner, max_iterations = 0),
    "max_iterations must be at least 1"
  )

  expect_error(
    rlm_module("question -> answer", runner = runner, max_iterations = -5),
    "max_iterations must be at least 1"
  )

  for (value in list(1.5, NA_real_, Inf, c(1, 2), .Machine$integer.max + 1)) {
    expect_error(
      rlm_module("question -> answer", runner = runner, max_iterations = value),
      class = "dsprrr_rlm_bounds_error",
      info = paste("max_iterations value", paste(value, collapse = ", "))
    )
  }
})

test_that("rlm_module validates max_llm_calls bounds", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())

  expect_error(
    rlm_module("question -> answer", runner = runner, max_llm_calls = -1),
    "max_llm_calls must be non-negative"
  )

  # Zero is allowed (disables recursive calls)
  expect_no_error(
    rlm_module("question -> answer", runner = runner, max_llm_calls = 0)
  )

  expect_error(
    rlm_module("question -> answer", runner = runner, max_llm_calls = 2.5),
    class = "dsprrr_rlm_bounds_error"
  )
})

test_that("rlm_module validates output bounds at both construction layers", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())

  expect_error(
    rlm_module("question -> answer", runner = runner, max_output_chars = 0),
    class = "dsprrr_rlm_bounds_error"
  )
  expect_error(
    RLMModule$new(
      signature = signature("question -> answer"),
      runner = runner,
      max_output_chars = 1.5
    ),
    class = "dsprrr_rlm_bounds_error"
  )
})

# ============================================================================
# Module Structure Tests
# ============================================================================

test_that("RLMModule has correct fields", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module("question -> answer", runner = runner)

  expect_true(!is.null(rlm$runner))
  expect_true(!is.null(rlm$max_iterations))
  expect_true(!is.null(rlm$max_llm_calls))
  expect_true(!is.null(rlm$max_output_chars))
  expect_true(!is.null(rlm$signature))
  expect_false(rlm$verbose)
  expect_null(rlm$sub_lm)
})

test_that("RLMModule get_repl_history returns list", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module("question -> answer", runner = runner)

  history <- rlm$get_repl_history()

  expect_type(history, "list")
  expect_length(history, 0) # No executions yet
})

test_that("RLMModule reset_copy creates fresh module", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 15
  )

  copy <- rlm$reset_copy()

  expect_s3_class(copy, "RLMModule")
  expect_equal(copy$max_iterations, 15L)
  expect_length(copy$state$repl_history, 0)
})

test_that("RLMModule print works", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module("question -> answer", runner = runner)

  expect_invisible(print(rlm))
})

# ============================================================================
# SUBMIT Termination Tests
# ============================================================================

test_that("RLMModule terminates on SUBMIT call", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer: integer",
    runner = runner
  )

  # Mock LLM that immediately calls SUBMIT
  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Computing the answer",
      code = "SUBMIT(42)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is 6 * 7?"),
    .llm = mock_llm
  )

  expect_s3_class(result, "tbl_df")
  expect_true("output" %in% names(result))
  expect_true("metadata" %in% names(result))
  expect_equal(result$metadata[[1]]$iterations, 1)
})

test_that("RLMModule rejects unexpected invocation inputs", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module("question -> answer: integer", runner = runner)

  expect_error(
    rlm$forward(list(question = "q", ignored = "unsafe")),
    "Unexpected: ignored",
    class = "dsprrr_rlm_input_error"
  )
})

test_that("RLMModule accepts an instruction-only zero-input signature", {
  skip_if_not_installed("callr")
  sig <- Signature(
    inputs = list(),
    output_type = ellmer::type_object(answer = ellmer::type_string()),
    instructions = "Return a greeting."
  )
  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(sig, runner = runner)
  result <- rlm$forward(
    list(),
    .llm = create_mock_rlm_llm(list(list(
      reasoning = "Done",
      code = "SUBMIT('hello')"
    )))
  )

  expect_identical(result$output[[1L]]$answer, "hello")
})

test_that("RLMModule SUBMIT returns correct value", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Simple string answer",
      code = "SUBMIT('the answer is 42')"
    )
  ))

  result <- rlm$forward(
    list(question = "What is the meaning of life?"),
    .llm = mock_llm
  )

  # The output should contain the submitted answer
  expect_equal(result$output[[1]]$answer, "the answer is 42")
})

test_that("RLMModule SUBMIT works with complex values", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_object(
        answer = ellmer::type_object(
          value = ellmer::type_number(),
          unit = ellmer::type_string()
        )
      )
    ),
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Returning a list",
      code = "SUBMIT(list(value = 42, unit = 'answer'))"
    )
  ))

  result <- rlm$forward(
    list(question = "Give me structured data"),
    .llm = mock_llm
  )

  expect_type(result$output[[1]]$answer, "list")
  expect_equal(result$output[[1]]$answer$value, 42)
})

test_that("RLMModule strips markdown code fences before execution", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module("question -> answer: integer", runner = runner)

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Return fenced code block",
      code = "```r\nx <- 40 + 2\nSUBMIT(x)\n```"
    )
  ))

  result <- rlm$forward(
    list(question = "What is 40 + 2?"),
    .llm = mock_llm
  )

  expect_equal(result$output[[1]]$answer, 42)
})

test_that("RLMModule supports multi-output SUBMIT with named arguments", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer, confidence: number",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Return named outputs",
      code = "SUBMIT(answer = 'Paris', confidence = '0.95')"
    )
  ))

  result <- rlm$forward(
    list(question = "What is the capital of France?"),
    .llm = mock_llm
  )

  expect_equal(result$output[[1]]$answer, "Paris")
  expect_equal(result$output[[1]]$confidence, 0.95)
})

test_that("RLMModule supports multi-output SUBMIT with positional arguments", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer, confidence: number",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Return positional outputs",
      code = "SUBMIT('Paris', 0.8)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is the capital of France?"),
    .llm = mock_llm
  )

  expect_equal(result$output[[1]]$answer, "Paris")
  expect_equal(result$output[[1]]$confidence, 0.8)
})

test_that("RLMModule retries after invalid multi-output SUBMIT payload", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer, confidence: number",
    runner = runner,
    max_iterations = 3
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Missing one required output field",
      code = "SUBMIT(answer = 'Paris')"
    ),
    list(
      reasoning = "Now provide both fields",
      code = "SUBMIT(answer = 'Paris', confidence = 0.9)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is the capital of France?"),
    .llm = mock_llm
  )

  expect_equal(result$metadata[[1]]$iterations, 2)
  expect_equal(result$output[[1]]$answer, "Paris")
  expect_equal(result$output[[1]]$confidence, 0.9)
})

# ============================================================================
# REPL Tools Tests
# ============================================================================

test_that("RLM peek function works in REPL", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "document -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Peek at document",
      code = "first_100 <- peek(.context$document, 1, 100)\nSUBMIT(first_100)"
    )
  ))

  long_doc <- paste(rep("Hello world. ", 100), collapse = "")

  result <- rlm$forward(
    list(document = long_doc),
    .llm = mock_llm
  )

  # Should get first 100 characters
  expect_equal(nchar(result$output[[1]]$answer), 100)
  expect_true(startsWith(result$output[[1]]$answer, "Hello world"))
})

test_that("RLM search function works in REPL", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "document -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Search for emails",
      code = "matches <- search(.context$document, '[a-z]+@[a-z]+\\\\.[a-z]+')\nSUBMIT(paste(matches, collapse=', '))"
    )
  ))

  doc <- "Contact us at test@example.com or info@example.org for more info."

  result <- rlm$forward(
    list(document = doc),
    .llm = mock_llm
  )

  expect_true(grepl("test@example.com", result$output[[1]]$answer))
  expect_true(grepl("info@example.org", result$output[[1]]$answer))
})

# ============================================================================
# Multiple Iterations Tests
# ============================================================================

test_that("RLMModule supports multiple iterations", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer: integer",
    runner = runner
  )

  # First iteration explores, second submits
  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "First, let me compute something",
      code = "x <- 10 + 32"
    ),
    list(
      reasoning = "Now submit the answer",
      code = "SUBMIT(42)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is 10 + 32?"),
    .llm = mock_llm
  )

  expect_equal(result$metadata[[1]]$iterations, 2)
})

test_that("RLMModule respects max_iterations", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 2
  )

  # LLM never calls SUBMIT
  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Step 1", code = "x <- 1"),
    list(reasoning = "Step 2", code = "y <- 2"),
    list(reasoning = "Step 3", code = "z <- 3") # Won't be reached
  ))

  result <- expect_test_warnings(
    rlm$forward(
      list(question = "test"),
      .llm = mock_llm
    ),
    "reached max_iterations"
  )

  # Should use fallback after max_iterations
  expect_equal(result$metadata[[1]]$iterations, 2)
  expect_equal(result$metadata[[1]]$max_iterations, 2)
})

test_that("RLM control nonces are cross-platform and preserve R RNG state", {
  set.seed(20260804)
  original_seed <- .Random.seed

  nonces <- replicate(3L, dsprrr:::rlm_control_nonce())

  expect_identical(nchar(nonces), rep.int(64L, 3L))
  expect_length(unique(nonces), 3L)
  expect_identical(.Random.seed, original_seed)
})

test_that("RLMModule uses fallback when no SUBMIT", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 1
  )

  # LLM never calls SUBMIT
  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Just computing", code = "1 + 1")
  ))

  result <- expect_test_warnings(
    rlm$forward(
      list(question = "test"),
      .llm = mock_llm
    ),
    "reached max_iterations"
  )

  # Should have used fallback extraction
  expect_equal(result$output[[1]]$answer, "fallback answer")
})

test_that("RLMModule normalizes structured fallback to signature fields and types", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> score: number, label: enum('positive', 'negative'), tags: array(string)",
    runner = runner,
    max_iterations = 1
  )

  mock_llm <- create_mock_rlm_llm(
    code_responses = list(
      list(reasoning = "No final output yet", code = "x <- 1")
    ),
    fallback_structured = list(
      score = "0.75",
      label = "POSITIVE",
      tags = c(1, 2, "ok")
    )
  )

  result <- expect_test_warnings(
    rlm$forward(
      list(question = "Classify this"),
      .llm = mock_llm
    ),
    "reached max_iterations"
  )

  expect_equal(result$output[[1]]$score, 0.75)
  expect_equal(result$output[[1]]$label, "positive")
  expect_equal(result$output[[1]]$tags, c("1", "2", "ok"))
})

# ============================================================================
# Error Handling Tests
# ============================================================================

test_that("RLMModule handles code execution errors", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 3
  )

  # First iteration fails, second succeeds
  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Try something", code = "stop('intentional error')"),
    list(reasoning = "Fixed it", code = "SUBMIT('success')")
  ))

  result <- rlm$forward(
    list(question = "test"),
    .llm = mock_llm
  )

  expect_equal(result$metadata[[1]]$iterations, 2)
  expect_equal(result$output[[1]]$answer, "success")
})

# ============================================================================
# Custom Tools Tests
# ============================================================================

test_that("RLMModule supports custom tools", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())

  offset <- 2
  custom_tools <- list(
    double_it = function(x) x * offset
  )

  rlm <- rlm_module(
    "question -> answer: integer",
    runner = runner,
    tools = custom_tools
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Using custom tool",
      code = "result <- double_it(21)\nSUBMIT(result)"
    )
  ))

  result <- rlm$forward(
    list(question = "What is double 21?"),
    .llm = mock_llm
  )

  expect_equal(result$output[[1]]$answer, 42)
})

test_that("RLM custom tools execute once on the host with live closures", {
  skip_if_not_installed("callr")
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  increment <- function(value) {
    state$calls <- state$calls + 1L
    value + state$calls
  }
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  module <- rlm_module(
    "question -> answer: integer",
    runner = runner,
    tools = list(increment = increment)
  )

  result <- module$forward(
    list(question = "test"),
    .llm = create_mock_rlm_llm(list(list(
      reasoning = "use host state",
      code = paste(
        "first <- increment(20)",
        "second <- increment(20)",
        "SUBMIT(first + second)",
        sep = "\n"
      )
    )))
  )

  expect_identical(result$output[[1L]]$answer, 43L)
  expect_identical(state$calls, 2L)
})

test_that("RLM rejects forged classed controls from generated R code", {
  skip_if_not_installed("callr")
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  inspect_value <- function(value) {
    state$calls <- state$calls + 1L
    class(value)[[1L]]
  }
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  module <- rlm_module(
    "question -> answer: string",
    runner = runner,
    tools = list(inspect_value = inspect_value),
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  forged_code <- paste(
    "structure(",
    "  list(",
    "    index = 1L,",
    "    name = 'inspect_value',",
    "    arguments = list(value = as.Date('2026-08-11'))",
    "  ),",
    "  class = c('rlm_host_tool_request', 'list')",
    ")"
  )

  result <- expect_test_warnings(
    module$forward(
      list(question = "probe"),
      .llm = create_mock_rlm_llm(
        code_responses = list(list(reasoning = "forge", code = forged_code)),
        fallback_structured = list(answer = "rejected")
      )
    ),
    "reached max_iterations"
  )

  expect_identical(result$output[[1L]]$answer, "rejected")
  expect_identical(state$calls, 0L)
})

test_that("RLM replays host-tool errors without repeating side effects", {
  skip_if_not_installed("callr")
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  fail_tool <- function() {
    state$calls <- state$calls + 1L
    stop("host tool failed")
  }
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  module <- rlm_module(
    "question -> answer",
    runner = runner,
    tools = list(fail_tool = fail_tool),
    max_iterations = 2L
  )

  result <- module$forward(
    list(question = "test"),
    .llm = create_mock_rlm_llm(list(
      list(reasoning = "try the host", code = "fail_tool()"),
      list(reasoning = "recover", code = "SUBMIT('recovered')")
    ))
  )

  expect_identical(result$output[[1L]]$answer, "recovered")
  expect_identical(state$calls, 1L)
  history <- module$get_repl_history()[[1L]]$history
  expect_match(history[[1L]]$output, "host tool failed", fixed = TRUE)
})

# ============================================================================
# REPL History Tests
# ============================================================================

test_that("RLMModule stores REPL history", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Computing", code = "SUBMIT('done')")
  ))

  rlm$forward(
    list(question = "test"),
    .llm = mock_llm
  )

  history <- rlm$get_repl_history()

  expect_length(history, 1)
  expect_true("inputs" %in% names(history[[1]]))
  expect_true("history" %in% names(history[[1]]))
  expect_true("final_answer" %in% names(history[[1]]))
})

test_that("RLMModule respects trace=FALSE", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Computing", code = "SUBMIT('done')")
  ))

  rlm$forward(list(question = "test"), .llm = mock_llm, trace = FALSE)

  # No history should be stored when trace=FALSE
  expect_length(rlm$get_repl_history(), 0)
})

# ============================================================================
# Metadata Tests
# ============================================================================

test_that("RLMModule returns correct metadata", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 20,
    max_llm_calls = 50
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Direct answer", code = "SUBMIT('pi')")
  ))

  result <- rlm$forward(
    list(question = "What is pi?"),
    .llm = mock_llm
  )

  metadata <- result$metadata[[1]]

  expect_equal(metadata$model, "rlm")
  expect_true("iterations" %in% names(metadata))
  expect_true("max_iterations" %in% names(metadata))
  expect_true("llm_calls" %in% names(metadata))
  expect_true("max_llm_calls" %in% names(metadata))
  expect_true("duration_ms" %in% names(metadata))
  expect_true("repl_history" %in% names(metadata))

  expect_equal(metadata$max_iterations, 20)
  expect_equal(metadata$max_llm_calls, 50)
})

# ============================================================================
# Integration with module() factory
# ============================================================================

test_that("module() factory works with type='rlm'", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 5, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- module(
    signature("question -> answer"),
    type = "rlm",
    runner = runner
  )

  expect_s3_class(rlm, "RLMModule")
  expect_identical(rlm$max_iterations, 20L)
  expect_error(
    module(
      signature("question -> answer"),
      type = "rlm",
      runner = runner,
      max_iters = 6L
    ),
    class = "dsprrr_module_type_argument_error"
  )
})

test_that("module() factory requires runner for rlm", {
  expect_error(
    module(
      signature("question -> answer"),
      type = "rlm"
    ),
    "rlm requires a runner"
  )
})

# ============================================================================
# Context Description Tests
# ============================================================================

test_that("RLMModule handles various context types", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "text, numbers, df -> answer",
    runner = runner
  )

  mock_llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Process inputs",
      code = "SUBMIT(paste('text:', nchar(.context$text), 'numbers:', length(.context$numbers), 'df rows:', nrow(.context$df)))"
    )
  ))

  result <- rlm$forward(
    list(
      text = "Hello world",
      numbers = c(1, 2, 3, 4, 5),
      df = data.frame(a = 1:3, b = 4:6)
    ),
    .llm = mock_llm
  )

  expect_true(grepl("text: 11", result$output[[1]]$answer, fixed = TRUE))
  expect_true(grepl("numbers: 5", result$output[[1]]$answer, fixed = TRUE))
  expect_true(grepl("df rows: 3", result$output[[1]]$answer, fixed = TRUE))
})

# ============================================================================
# RLM Tools Unit Tests
# ============================================================================

test_that("create_rlm_prelude generates valid R code", {
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  expect_type(prelude, "character")
  # Parse should succeed
  expect_no_error(parse(text = prelude))
})

test_that("create_rlm_prelude includes SUBMIT function", {
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  expect_true(grepl("SUBMIT <- base::local", prelude, fixed = TRUE))
})

test_that("create_rlm_prelude includes peek function", {
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  expect_true(grepl("peek <- base::local", prelude, fixed = TRUE))
})

test_that("create_rlm_prelude includes search function", {
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  expect_true(grepl("search <- function", prelude, fixed = TRUE))
})

test_that("create_rlm_prelude includes llm_query when sub_lm enabled", {
  prelude_with <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list()
  )

  prelude_without <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list()
  )

  # With sub_lm: should have working llm_query
  expect_true(grepl("llm_query <- base::local", prelude_with, fixed = TRUE))

  # Without sub_lm: should have disabled llm_query
  expect_true(grepl(
    "Recursive LLM queries are disabled",
    prelude_without,
    fixed = TRUE
  ))
})

test_that("create_rlm_prelude requests host tools without deparsing them", {
  custom_tools <- list(
    my_tool = function(x) x * 2,
    another_tool = function(a, b) a + b
  )

  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = custom_tools
  )

  expect_match(prelude, "Non-local host-tool replay bridge", fixed = TRUE)
  expect_match(prelude, ".name <- \"my_tool\"", fixed = TRUE)
  expect_match(prelude, ".name <- \"another_tool\"", fixed = TRUE)
  expect_false(grepl("function(x) x * 2", prelude, fixed = TRUE))
})

test_that("create_rlm_prelude enforces multi-output SUBMIT shape", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  control_nonce <- "submit-shape-test"
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = FALSE,
    custom_tools = list(),
    output_fields = c("answer", "confidence"),
    control_nonce = control_nonce
  )

  ok <- runner$execute(
    paste0(prelude, "\nSUBMIT('ok', 0.9)"),
    context = list()
  )
  expect_false(ok$success)
  decoded <- dsprrr:::decode_rlm_control(ok$error, control_nonce)
  expect_true(dsprrr:::is_rlm_final(decoded))
  expect_equal(
    names(dsprrr:::extract_rlm_final(decoded)),
    c("answer", "confidence")
  )

  bad <- runner$execute(
    paste0(prelude, "\nSUBMIT(answer = 'ok')"),
    context = list()
  )
  expect_false(bad$success)
  expect_true(grepl("missing outputs", bad$error, fixed = TRUE))

  duplicate <- runner$execute(
    paste0(
      prelude,
      "\nSUBMIT(answer = 'first', answer = 'second', confidence = 0.9)"
    ),
    context = list()
  )
  expect_false(duplicate$success)
  expect_true(grepl("names must be unique", duplicate$error, fixed = TRUE))
})

test_that("long authenticated control frames round-trip without whitespace", {
  skip_if_not_installed("callr")

  nonce <- "long-control-frame"
  prelude <- dsprrr:::create_rlm_prelude(
    control_nonce = nonce,
    control_frame_limit = 20000
  )
  result <- r_code_runner(timeout = 10, persistent = TRUE)$execute(
    paste0(prelude, "\nSUBMIT(base::strrep('x', 5000L))")
  )
  control <- dsprrr:::decode_rlm_control(result$error, nonce)

  expect_false(result$success)
  expect_true(dsprrr:::is_rlm_final(control))
  expect_identical(
    dsprrr:::extract_rlm_final(control)$answer,
    strrep("x", 5000L)
  )
})

test_that("RLM control frames preserve long multiline unicode payloads", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  control_nonce <- "long-payload-test"
  prelude <- dsprrr:::create_rlm_prelude(
    has_sub_lm = FALSE,
    output_fields = "answer",
    control_nonce = control_nonce
  )
  answer <- paste(
    rep("A long line with unicode snow 雪\nand a newline", 12L),
    collapse = " | "
  )
  result <- runner$execute(
    paste0(prelude, "\nSUBMIT(", encodeString(answer, quote = "\""), ")"),
    context = list()
  )
  decoded <- dsprrr:::decode_rlm_control(result$error, control_nonce)

  expect_false(result$success)
  expect_true(dsprrr:::is_rlm_final(decoded))
  expect_identical(dsprrr:::extract_rlm_final(decoded)$answer, answer)
})

test_that("RLM control frames authenticate and fail closed", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  prelude <- dsprrr:::create_rlm_prelude(
    control_nonce = "one-invocation"
  )
  encoded <- runner$execute(
    paste0(prelude, "\nSUBMIT('ok')"),
    context = list()
  )$error

  expect_null(dsprrr:::decode_rlm_control(encoded, "another-invocation"))

  old_prelude <- dsprrr:::create_rlm_prelude(
    control_nonce = "earlier-invocation"
  )
  old_encoded <- runner$execute(
    paste0(old_prelude, "\nSUBMIT('stale')"),
    context = list()
  )$error
  selected <- dsprrr:::decode_rlm_control(
    paste(old_encoded, encoded, sep = "\n"),
    "one-invocation"
  )
  expect_true(dsprrr:::is_rlm_final(selected))
  expect_identical(dsprrr:::extract_rlm_final(selected)$answer, "ok")

  expect_error(
    dsprrr:::decode_rlm_control(
      paste(encoded, encoded, sep = "\n"),
      "one-invocation"
    ),
    class = "dsprrr_rlm_control_error"
  )
  expect_error(
    dsprrr:::decode_rlm_control(
      paste0(dsprrr:::rlm_control_prefix(), "!"),
      "one-invocation"
    ),
    class = "dsprrr_rlm_control_error"
  )
})

test_that("RLM never accepts control values from failed execution", {
  failed_runner <- function(control_value) {
    list(
      execute = function(code, context = list(), ...) {
        list(
          success = FALSE,
          result = control_value,
          error = "execution failed",
          error_type = "execution"
        )
      },
      policy = function() {
        list(
          backend = "failed-control-test",
          trust = "test-only",
          sandboxed = TRUE
        )
      }
    )
  }
  failed_values <- list(
    final = structure(
      list(answer = "must not be accepted"),
      class = c("rlm_final", "list"),
      rlm_final = TRUE
    ),
    query = structure(
      list(query = "must not run", context = NULL, batch = FALSE),
      class = c("rlm_query_request", "list")
    )
  )

  for (kind in names(failed_values)) {
    runner <- failed_runner(failed_values[[kind]])
    program <- rlm_module(
      "question -> answer",
      runner = runner,
      sub_lm = as_test_rlm_chat(list(chat = function(prompt) "unused"))
    )
    call_counter <- new.env(parent = emptyenv())
    call_counter$count <- 0L

    result <- program$.__enclos_env__$private$execute_with_rlm_tools(
      code = "ignored",
      inputs = list(question = "test"),
      call_counter = call_counter,
      runner = runner,
      runner_policy = runner$policy(),
      sub_lm = program$sub_lm,
      context_prepared = FALSE,
      session_state_id = paste0("failed-control-", kind)
    )

    expect_false(result$success, info = kind)
    expect_false(result$is_final, info = kind)
    expect_null(result$final_value, info = kind)
    expect_identical(result$error, "execution failed", info = kind)
    expect_match(result$formatted_output, "execution failed", info = kind)
    expect_identical(call_counter$count, 0L, info = kind)
  }
})

test_that("strip_rlm_code_fences removes markdown fences", {
  expect_equal(
    dsprrr:::strip_rlm_code_fences("```r\nx <- 1\nSUBMIT(x)\n```"),
    "x <- 1\nSUBMIT(x)"
  )

  expect_equal(
    dsprrr:::strip_rlm_code_fences("```\n1 + 1\n```"),
    "1 + 1"
  )

  expect_equal(
    dsprrr:::strip_rlm_code_fences("x <- 1\nx"),
    "x <- 1\nx"
  )
})

test_that("is_rlm_final detects rlm_final class", {
  # Regular value
  expect_false(dsprrr:::is_rlm_final(42))
  expect_false(dsprrr:::is_rlm_final("hello"))

  # rlm_final value
  final <- structure("answer", class = c("rlm_final", "character"))
  expect_true(dsprrr:::is_rlm_final(final))

  # Via attribute
  final2 <- "answer"
  attr(final2, "rlm_final") <- TRUE
  expect_true(dsprrr:::is_rlm_final(final2))
})

test_that("extract_rlm_final removes rlm_final class", {
  final <- structure("answer", class = c("rlm_final", "character"))
  attr(final, "rlm_final") <- TRUE

  extracted <- dsprrr:::extract_rlm_final(final)

  expect_false(dsprrr:::is_rlm_final(extracted))
  expect_equal(extracted, "answer")
  expect_null(attr(extracted, "rlm_final"))
})

# ============================================================================
# Error Handling and Edge Case Tests
# ============================================================================

test_that("RLMModule warns when max_iterations reached without SUBMIT", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    max_iterations = 2
  )

  # LLM never calls SUBMIT
  mock_llm <- create_mock_rlm_llm(list(
    list(reasoning = "Step 1", code = "x <- 1"),
    list(reasoning = "Step 2", code = "y <- 2")
  ))

  expect_warning(
    rlm$forward(list(question = "test"), .llm = mock_llm),
    "reached max_iterations"
  )
})

test_that("RLMModule handles LLM response with missing code", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  # Mock LLM that returns invalid response (missing code)
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      list(reasoning = "Thinking")
      # Missing 'code' field
    },
    chat = function(prompt, ...) "fallback"
  )

  expect_error(
    rlm$forward(list(question = "test"), .llm = mock_llm),
    "invalid"
  )
})

test_that("RLMModule handles LLM response with non-string code", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )

  # Mock LLM that returns code as number
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      list(reasoning = "Thinking", code = 123)
    },
    chat = function(prompt, ...) "fallback"
  )

  expect_error(
    rlm$forward(list(question = "test"), .llm = mock_llm),
    "invalid"
  )
})

# ============================================================================
# rlm_query Batch Tests
# ============================================================================

with_mock_parallel_chat <- function(mock_fn, code) {
  ns <- asNamespace("ellmer")
  if (!exists("parallel_chat", envir = ns, inherits = FALSE)) {
    skip("ellmer::parallel_chat not available in this ellmer version")
  }
  old_fn <- get("parallel_chat", envir = ns, inherits = FALSE)

  unlockBinding("parallel_chat", ns)
  assign("parallel_chat", mock_fn, envir = ns)
  lockBinding("parallel_chat", ns)

  on.exit(
    {
      unlockBinding("parallel_chat", ns)
      assign("parallel_chat", old_fn, envir = ns)
      lockBinding("parallel_chat", ns)
    },
    add = TRUE
  )

  force(code)
}

test_that("is_rlm_query_request detects query requests", {
  regular <- 42
  expect_false(dsprrr:::is_rlm_query_request(regular))

  request <- structure(
    list(query = "test", context = NULL, batch = FALSE),
    class = "rlm_query_request"
  )
  expect_true(dsprrr:::is_rlm_query_request(request))
})

test_that("llm_query_batched generates batch request marker", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  control_nonce <- "query-batch-test"

  # Execute prelude in subprocess to test llm_query_batched
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list(),
    control_nonce = control_nonce
  )

  result <- runner$execute(
    paste0(prelude, "\nllm_query_batched(c('q1', 'q2'))"),
    context = list()
  )

  expect_false(result$success)
  request <- dsprrr:::decode_rlm_control(result$error, control_nonce)
  expect_s3_class(request, "rlm_query_request")
  expect_true(request$batch)
  expect_equal(request$queries, c("q1", "q2"))
})

test_that("llm_query_batched preserves one-query arrays", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  control_nonce <- "query-singleton-test"
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list(),
    control_nonce = control_nonce
  )

  result <- runner$execute(
    paste0(
      prelude,
      "\nllm_query_batched('q1', slices = 'context')"
    ),
    context = list()
  )

  expect_identical(result$success, FALSE)
  request <- dsprrr:::decode_rlm_control(result$error, control_nonce)
  expect_s3_class(request, "rlm_query_request")
  expect_identical(request$queries, "q1")
  expect_identical(request$slices, "context")

  captured <- new.env(parent = emptyenv())
  sub_lm <- as_test_rlm_chat(list(chat = function(prompt) {
    captured$prompt <- prompt
    "answer"
  }))
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    sub_lm = sub_lm
  )
  call_counter <- new.env(parent = emptyenv())
  call_counter$count <- 0L
  processed <- rlm$.__enclos_env__$private$process_rlm_query_batch(
    request,
    call_counter,
    sub_lm
  )

  expect_identical(
    captured$prompt,
    "Context:\ncontext\n\nQuestion: q1"
  )
  expect_identical(processed$success, TRUE)
})

test_that("llm_query_batched rejects non-scalar context slices", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  control_nonce <- "query-slice-shape-test"
  prelude <- dsprrr:::create_rlm_prelude(
    has_sub_lm = TRUE,
    control_nonce = control_nonce
  )
  result <- runner$execute(
    paste0(
      prelude,
      "\nllm_query_batched('q1', slices = list(c('a', 'b')))"
    ),
    context = list()
  )

  expect_identical(result$success, FALSE)
  expect_match(
    result$error,
    "slices must contain one non-missing character string per query",
    fixed = TRUE
  )

  malformed_json <- jsonlite::toJSON(
    list(
      version = 1L,
      nonce = control_nonce,
      kind = "query",
      payload = list(
        queries = base::I("q1"),
        slices = list(c("a", "b")),
        batch = TRUE
      )
    ),
    auto_unbox = TRUE,
    null = "null"
  )
  malformed <- paste0(
    dsprrr:::rlm_control_prefix(),
    gsub(
      "[\r\n]",
      "",
      jsonlite::base64_enc(charToRaw(as.character(malformed_json)))
    )
  )
  expect_error(
    dsprrr:::decode_rlm_control(malformed, control_nonce),
    class = "dsprrr_rlm_control_error"
  )
})

test_that("llm_query_batched preserves empty batches and rejects missing queries", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  control_nonce <- "query-cardinality-test"
  prelude <- dsprrr:::create_rlm_prelude(
    has_sub_lm = TRUE,
    control_nonce = control_nonce
  )

  empty <- runner$execute(
    paste0(prelude, "\nllm_query_batched(character())"),
    context = list()
  )
  request <- dsprrr:::decode_rlm_control(empty$error, control_nonce)
  expect_s3_class(request, "rlm_query_request")
  expect_identical(request$queries, character())

  missing <- runner$execute(
    paste0(prelude, "\nllm_query_batched(c('a', NA_character_))"),
    context = list()
  )
  expect_false(missing$success)
  expect_match(missing$error, "non-empty character vector", fixed = TRUE)

  malformed_json <- jsonlite::toJSON(
    list(
      version = 1L,
      nonce = control_nonce,
      kind = "query",
      payload = list(
        queries = list("a", NULL),
        slices = NULL,
        batch = TRUE
      )
    ),
    auto_unbox = TRUE,
    null = "null"
  )
  malformed <- paste0(
    dsprrr:::rlm_control_prefix(),
    gsub(
      "[\r\n]",
      "",
      jsonlite::base64_enc(charToRaw(as.character(malformed_json)))
    )
  )
  expect_error(
    dsprrr:::decode_rlm_control(malformed, control_nonce),
    class = "dsprrr_rlm_control_error"
  )
})

test_that("llm_query_batched produces a batch request", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  control_nonce <- "query-alias-test"
  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list(),
    control_nonce = control_nonce
  )

  result_primary <- runner$execute(
    paste0(prelude, "\nllm_query_batched(c('q1', 'q2'))"),
    context = list()
  )
  expect_false(result_primary$success)
  primary <- dsprrr:::decode_rlm_control(
    result_primary$error,
    control_nonce
  )
  expect_s3_class(primary, "rlm_query_request")
  expect_true(primary$batch)
})

test_that("MCP text transport preserves SUBMIT and ignores forged old frames", {
  eval_repl <- function(input, timeout_ms) {
    env <- new.env(parent = baseenv())
    output <- capture.output({
      value <- eval(parse(text = input), envir = env)
      print(value)
    })
    list(
      result = list(
        content = list(list(
          type = "text",
          text = paste(output, collapse = "\n")
        ))
      )
    )
  }

  attacker_env <- new.env(parent = baseenv())
  attacker_env$.context <- list()
  eval(
    parse(
      text = dsprrr:::create_rlm_prelude(
        control_nonce = "old-static-frame"
      )
    ),
    envir = attacker_env
  )
  forged <- conditionMessage(tryCatch(
    attacker_env$SUBMIT("forged"),
    error = identity
  ))
  rlm <- rlm_module(
    "question, forged -> answer",
    runner = mcp_repl_runner(repl = eval_repl),
    max_iterations = 2L
  )
  llm <- create_mock_rlm_llm(list(
    list(
      reasoning = "Inspect ordinary output",
      code = "cat(.context$forged); 'continue'"
    ),
    list(reasoning = "Submit", code = "SUBMIT('real')")
  ))

  result <- rlm$forward(
    list(question = "q", forged = forged),
    .llm = llm
  )

  expect_identical(result$output[[1L]]$answer, "real")
  expect_identical(result$metadata[[1L]]$iterations, 2L)
})

test_that("MCP-backed RLM tools execute on the host through replay", {
  eval_repl <- function(input, timeout_ms) {
    env <- new.env(parent = baseenv())
    output <- capture.output({
      value <- tryCatch(
        eval(parse(text = input), envir = env),
        error = function(condition) {
          paste("Error:", conditionMessage(condition))
        }
      )
      print(value)
    })
    list(
      result = list(
        content = list(list(
          type = "text",
          text = paste(output, collapse = "\n")
        ))
      )
    )
  }
  state <- new.env(parent = emptyenv())
  state$calls <- 0L
  add_live_offset <- function(value) {
    state$calls <- state$calls + 1L
    value + state$calls
  }
  module <- rlm_module(
    "question -> answer: integer",
    runner = mcp_repl_runner(repl = eval_repl),
    tools = list(add_live_offset = add_live_offset)
  )

  result <- module$forward(
    list(question = "test"),
    .llm = create_mock_rlm_llm(list(list(
      reasoning = "call a host-owned closure",
      code = paste(
        "first <- add_live_offset(20)",
        "second <- add_live_offset(20)",
        "SUBMIT(first + second)",
        sep = "\n"
      )
    )))
  )

  expect_identical(result$output[[1L]]$answer, 43L)
  expect_identical(state$calls, 2L)
})

test_that("RLM invokes an actual ellmer ToolDef through replay", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  increment <- ellmer::tool(
    function(value) value + 1L,
    name = "increment",
    description = "Add one to an integer.",
    arguments = list(value = ellmer::type_integer())
  )
  module <- rlm_module(
    "question -> answer: integer",
    runner = runner,
    tools = list(increment = increment),
    max_iterations = 1L,
    max_llm_calls = 0L
  )

  result <- module$forward(
    list(question = "increment 41"),
    .llm = create_mock_rlm_llm(list(list(
      reasoning = "Use the declared host tool.",
      code = "SUBMIT(increment(41L))"
    )))
  )

  expect_identical(result$output[[1L]]$answer, 42L)
})

test_that("captured SUBMIT helpers resist user-code base masking", {
  skip_if_not_installed("callr")
  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner
  )
  llm <- create_mock_rlm_llm(list(list(
    reasoning = "Mask common names after prelude creation",
    code = paste(
      "list <- function(...) 'shadow'",
      "length <- function(...) 999",
      "names <- function(...) 'shadow'",
      "SUBMIT('ok')",
      sep = "; "
    )
  )))

  result <- rlm$forward(list(question = "q"), .llm = llm)

  expect_identical(result$output[[1L]]$answer, "ok")
})

test_that("llm_query_batched validates queries is character", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())

  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list()
  )

  result <- runner$execute(
    paste0(prelude, "\nllm_query_batched(123)"),
    context = list()
  )

  expect_false(result$success)
  expect_true(grepl("character vector", result$error, fixed = TRUE))
})

test_that("llm_query_batched validates slices length", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())

  prelude <- dsprrr:::create_rlm_prelude(
    max_llm_calls = 50,
    has_sub_lm = TRUE,
    custom_tools = list()
  )

  result <- runner$execute(
    paste0(prelude, "\nllm_query_batched(c('q1', 'q2'), slices = c('s1'))"),
    context = list()
  )

  expect_false(result$success)
  expect_true(grepl("same length", result$error, fixed = TRUE))
})

test_that("recursive queries count every provider assistant turn", {
  make_chat <- function(initial_turns = list()) {
    turns <- initial_turns
    chat <- NULL
    chat <- list(
      clone = function(...) make_chat(turns),
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      get_turns = function(...) turns,
      chat = function(prompt, ...) {
        turns <<- c(
          turns,
          list(
            ellmer::UserTurn(prompt),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText("tool request")),
              tokens = c(10L, 1L, 0L),
              cost = 0.11,
              duration = 0.4
            ),
            ellmer::AssistantTurn(
              contents = list(ellmer::ContentText("final")),
              tokens = c(20L, 2L, 0L),
              cost = 0.22,
              duration = 0.6
            )
          )
        )
        "final"
      }
    )
    chat
  }
  sub_lm <- as_test_rlm_chat(
    make_chat(list(ellmer::UserTurn("private prior turn")))
  )
  rlm <- rlm_module(
    "question -> answer",
    interpreter_factory = function() stop("not used"),
    sub_lm = sub_lm
  )
  call_counter <- new.env(parent = emptyenv())
  call_counter$count <- 0L

  result <- rlm$.__enclos_env__$private$process_rlm_query(
    list(query = "question", context = NULL, batch = FALSE),
    call_counter,
    sub_lm
  )

  expect_identical(result$value, "final")
  expect_identical(call_counter$count, 1L)
  expect_identical(call_counter$provider_calls, 2L)
  expect_identical(call_counter$provider_calls_known, TRUE)
  expect_identical(call_counter$usage[[1L]]$input_tokens, 30L)
  expect_identical(call_counter$usage[[1L]]$output_tokens, 3L)
  expect_identical(call_counter$usage[[1L]]$total_tokens, 33L)
  expect_equal(call_counter$usage[[1L]]$cost, 0.33)
})

test_that("recursive provider errors leave call and usage accounting unknown", {
  make_failed <- function(turns = list()) {
    as_test_rlm_chat(list(
      clone = function(...) make_failed(turns),
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      get_turns = function(...) turns,
      chat = function(prompt, ...) {
        turns <<- c(
          turns,
          list(ellmer::AssistantTurn(
            contents = list(ellmer::ContentText("partial")),
            tokens = c(10L, 1L, 0L),
            cost = 0.1
          ))
        )
        error <- simpleError("provider stopped")
        class(error) <- c("dsprrr_rlm_provider_error", class(error))
        stop(error)
      }
    ))
  }
  failed <- make_failed()
  rlm <- rlm_module(
    "question -> answer",
    interpreter_factory = function() stop("not used"),
    sub_lm = failed
  )
  call_counter <- new.env(parent = emptyenv())
  call_counter$count <- 0L

  result <- suppressWarnings(
    rlm$.__enclos_env__$private$process_rlm_query(
      list(query = "question", context = NULL, batch = FALSE),
      call_counter,
      failed
    )
  )

  expect_false(result$success)
  expect_identical(call_counter$count, 1L)
  expect_identical(call_counter$provider_calls, 0L)
  expect_identical(call_counter$provider_calls_known, FALSE)
  expect_true(is.na(call_counter$usage[[1L]]$provider_calls))
  expect_true(is.na(call_counter$usage[[1L]]$total_tokens))
  expect_true(is.na(call_counter$usage[[1L]]$cost))
})

test_that("parallel recursive accounting uses the cleared batch baseline", {
  original_turns <- list(ellmer::UserTurn("private prior turn"))
  make_template <- function(turns = list()) {
    chat <- NULL
    chat <- list(
      clone = function(...) make_template(turns),
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      get_turns = function(...) turns,
      chat = function(prompt, ...) "unused"
    )
    chat
  }
  sub_lm <- as_test_rlm_chat(make_template(original_turns))
  make_result <- function(text, usage) {
    turns <- c(
      list(ellmer::UserTurn("current")),
      lapply(seq_along(usage), function(i) {
        item <- usage[[i]]
        ellmer::AssistantTurn(
          contents = list(ellmer::ContentText(
            if (i == length(usage)) text else "tool request"
          )),
          tokens = item$tokens,
          cost = item$cost,
          duration = item$duration
        )
      })
    )
    new_test_chat(turns = turns)
  }
  rlm <- rlm_module(
    "question -> answer",
    interpreter_factory = function() stop("not used"),
    sub_lm = sub_lm
  )
  call_counter <- new.env(parent = emptyenv())
  call_counter$count <- 0L
  captured <- new.env(parent = emptyenv())

  result <- with_mock_parallel_chat(
    function(
      chat,
      prompts,
      max_active = 10,
      rpm = 500,
      on_error = c("return", "continue", "stop")
    ) {
      captured$baseline <- chat$get_turns()
      list(
        make_result(
          "first",
          list(
            list(tokens = c(10L, 1L, 0L), cost = 0.11, duration = 0.4),
            list(tokens = c(20L, 2L, 0L), cost = 0.22, duration = 0.6)
          )
        ),
        make_result(
          "second",
          list(
            list(tokens = c(5L, 1L, 0L), cost = 0.05, duration = 0.2)
          )
        )
      )
    },
    rlm$.__enclos_env__$private$process_rlm_query_batch(
      list(
        queries = c("q1", "q2"),
        slices = NULL,
        batch = TRUE
      ),
      call_counter,
      sub_lm
    )
  )

  expect_identical(captured$baseline, list())
  expect_identical(result$value, c("first", "second"))
  expect_identical(call_counter$count, 2L)
  expect_identical(call_counter$provider_calls, 3L)
  expect_identical(call_counter$provider_calls_known, TRUE)
  expect_identical(
    vapply(call_counter$usage, `[[`, integer(1), "provider_calls"),
    c(2L, 1L)
  )
  expect_identical(
    vapply(call_counter$usage, `[[`, integer(1), "total_tokens"),
    c(33L, 6L)
  )
})

test_that("process_rlm_query_batch uses bounded parallelism and preserves order", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    sub_lm = as_test_rlm_chat(list(chat = function(prompt) "unused"))
  )

  call_counter <- new.env(parent = emptyenv())
  call_counter$count <- 0L
  request <- list(queries = c("q1", "q2", "q3"), slices = NULL, batch = TRUE)
  captured <- new.env(parent = emptyenv())

  make_fake_chat <- function(text) {
    list(last_turn = function() list(text = text))
  }
  provider_error <- structure(
    simpleError("boom"),
    class = c("dsprrr_rlm_provider_error", class(simpleError("boom")))
  )

  withr::local_options(list(dsprrr.rlm_batch_max_active = 2L))
  warning_message <- NULL
  result <- withCallingHandlers(
    with_mock_parallel_chat(
      function(
        chat,
        prompts,
        max_active = 10,
        rpm = 500,
        on_error = c("return", "continue", "stop")
      ) {
        captured$prompts <- prompts
        captured$max_active <- max_active
        captured$on_error <- on_error
        list(
          make_fake_chat("first"),
          provider_error,
          make_fake_chat("third")
        )
      },
      rlm$.__enclos_env__$private$process_rlm_query_batch(
        request,
        call_counter,
        rlm$sub_lm
      )
    ),
    warning = function(w) {
      warning_message <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  expect_equal(call_counter$count, 3L)
  expect_equal(call_counter$provider_calls, 0L)
  expect_false(call_counter$provider_calls_known)
  expect_equal(captured$max_active, 2L)
  expect_equal(captured$prompts, as.list(c("q1", "q2", "q3")))
  expect_equal(captured$on_error, "continue")
  expect_match(warning_message, "Some batch queries failed")

  expect_true(result$success)
  expect_null(result$error)
  expect_identical(result$value, c("first", "[ERROR] boom", "third"))
  expect_match(result$errors, "Query 2: boom")
  expect_match(result$formatted_output, "Query 1 result: first")
  expect_match(
    result$formatted_output,
    "Query 2 result: [ERROR] boom",
    fixed = TRUE
  )
  expect_match(result$formatted_output, "Query 3 result: third")

  pos1 <- regexpr("Query 1 result: first", result$formatted_output)[1]
  pos2 <- regexpr(
    "Query 2 result: [ERROR] boom",
    result$formatted_output,
    fixed = TRUE
  )[1]
  pos3 <- regexpr("Query 3 result: third", result$formatted_output)[1]
  expect_true(pos1 < pos2 && pos2 < pos3)
})

test_that("process_rlm_query_batch propagates parallel infrastructure failure", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  chat_calls <- 0L
  sub_lm <- as_test_rlm_chat(list(
    chat = function(prompt, ...) {
      chat_calls <<- chat_calls + 1L
      paste0("ok: ", prompt)
    }
  ))

  rlm <- rlm_module(
    "question -> answer",
    runner = runner,
    sub_lm = sub_lm
  )

  call_counter <- new.env(parent = emptyenv())
  call_counter$count <- 0L
  request <- list(
    queries = c("good", "better", "fine"),
    slices = NULL,
    batch = TRUE
  )

  expect_error(
    with_mock_parallel_chat(
      function(
        chat,
        prompts,
        max_active = 10,
        rpm = 500,
        on_error = c("return", "continue", "stop")
      ) {
        stop("parallel unavailable")
      },
      rlm$.__enclos_env__$private$process_rlm_query_batch(
        request,
        call_counter,
        sub_lm
      )
    ),
    class = "dsprrr_rlm_batch_transport_error"
  )

  expect_equal(call_counter$count, 3L)
  expect_equal(call_counter$provider_calls, 0L)
  expect_true(call_counter$provider_calls_known)
  expect_equal(chat_calls, 0L)
})

test_that("RLM sub-LM responses must contain one non-empty text value", {
  expect_identical(dsprrr:::normalize_rlm_sub_lm_text("answer"), "answer")
  expect_identical(
    dsprrr:::normalize_rlm_sub_lm_text(list(text = "answer")),
    "answer"
  )

  invalid <- list(
    list(answer = "silently stringified before"),
    "",
    character(),
    c("first", "second")
  )
  for (response in invalid) {
    expect_error(
      dsprrr:::normalize_rlm_sub_lm_text(response),
      class = "dsprrr_rlm_sub_lm_response_error"
    )
  }
})
