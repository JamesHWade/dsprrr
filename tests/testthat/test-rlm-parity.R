# Deterministic end-to-end regressions for the public RLM contract.

new_rlm_parity_chat <- function(
  actions,
  query_values = list(),
  fallback = list(answer = "fallback"),
  fallback_error = NULL,
  .state = NULL
) {
  if (is.null(.state)) {
    .state <- new.env(parent = emptyenv())
    .state$action_index <- 0L
    .state$query_index <- 0L
    .state$fallback_calls <- 0L
    .state$action_prompts <- character()
    .state$query_prompts <- character()
    .state$fallback_prompts <- character()
  }
  turns <- list()

  record_turns <- function(prompt, response) {
    turns <<- c(
      turns,
      list(
        ellmer::UserTurn(as.character(prompt)),
        ellmer::AssistantTurn(
          contents = list(ellmer::ContentText(as.character(response))),
          tokens = c(1L, 1L, 0L),
          cost = 0,
          duration = 0
        )
      )
    )
    invisible(NULL)
  }

  chat <- new_test_chat(
    clone = function(deep = TRUE) {
      new_rlm_parity_chat(
        actions = actions,
        query_values = query_values,
        fallback = fallback,
        fallback_error = fallback_error,
        .state = .state
      )
    },
    chat_structured = function(prompt, type, ...) {
      fields <- if (
        inherits(type, "ellmer::TypeObject") &&
          methods::.hasSlot(type, "properties")
      ) {
        names(type@properties)
      } else {
        character()
      }

      if (all(c("reasoning", "code") %in% fields)) {
        .state$action_index <- .state$action_index + 1L
        .state$action_prompts <- c(.state$action_prompts, prompt)
        if (.state$action_index > length(actions)) {
          stop("No scripted RLM action remains", call. = FALSE)
        }
        value <- actions[[.state$action_index]]
        record_turns(prompt, "action")
        return(value)
      }

      .state$fallback_calls <- .state$fallback_calls + 1L
      .state$fallback_prompts <- c(.state$fallback_prompts, prompt)
      if (!is.null(fallback_error)) {
        stop(fallback_error, call. = FALSE)
      }
      if (is.function(fallback)) {
        value <- fallback(prompt, type)
        record_turns(prompt, "fallback")
        return(value)
      }
      record_turns(prompt, "fallback")
      fallback
    },
    chat = function(prompt, ...) {
      .state$query_index <- .state$query_index + 1L
      .state$query_prompts <- c(.state$query_prompts, prompt)
      if (.state$query_index > length(query_values)) {
        stop("No scripted recursive-query response remains", call. = FALSE)
      }
      value <- query_values[[.state$query_index]]
      if (inherits(value, "condition")) {
        stop(value)
      }
      record_turns(prompt, value)
      value
    },
    get_turns = function(...) turns,
    set_turns = function(value) {
      turns <<- value
      invisible(NULL)
    },
    last_turn = function(role = c("assistant", "user"), ...) {
      role <- match.arg(role)
      matching <- Filter(function(turn) identical(turn@role, role), turns)
      if (length(matching) == 0L) NULL else matching[[length(matching)]]
    },
    get_model = function() "rlm-parity-test"
  )
  chat$parity_state <- .state
  chat
}

capture_rlm_parity_error <- function(expr) {
  captured <- NULL
  withCallingHandlers(
    tryCatch(
      force(expr),
      error = function(error) {
        captured <<- error
        NULL
      }
    ),
    warning = function(warning) invokeRestart("muffleWarning")
  )
  captured
}

test_that("persistent R runners preserve variables across RLM iterations", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  chat <- new_rlm_parity_chat(list(
    list(reasoning = "Create a reusable value", code = "seed <- 40L; seed"),
    list(reasoning = "Use the prior value", code = "SUBMIT(seed + 2L)")
  ))
  module <- rlm_module(
    "question -> answer: integer",
    runner = runner,
    max_iterations = 2L,
    max_llm_calls = 0L
  )

  result <- module$forward(list(question = "What is the answer?"), .llm = chat)

  expect_identical(result$output[[1L]]$answer, 42L)
  expect_identical(result$metadata[[1L]]$iterations, 2L)
  expect_match(result$metadata[[1L]]$repl_history[[1L]]$output, "40")
  expect_true(result$metadata[[1L]]$repl_history[[2L]]$is_final)
})

test_that("RLM rejects runners without an explicit persistent capability", {
  skip_if_not_installed("callr")

  module <- rlm_module(
    "question -> answer",
    runner = r_code_runner(persistent = FALSE),
    max_llm_calls = 0L
  )
  chat <- new_rlm_parity_chat(list(list(
    reasoning = "Should not run",
    code = "SUBMIT('no')"
  )))

  expect_error(
    module$forward(list(question = "test"), .llm = chat),
    class = "dsprrr_rlm_persistent_runner_error"
  )
  expect_identical(chat$parity_state$action_index, 0L)
})

test_that("caller-owned persistent runners release context and bridge state", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  preexisting <- runner$execute(paste(
    "search <- 'PREEXISTING-SEARCH'",
    "domain_lookup <- 'PREEXISTING-TOOL'",
    sep = "\n"
  ))
  expect_true(preexisting$success)
  secret <- paste0("INVOCATION-SECRET-", strrep("x", 100L))
  module <- rlm_module(
    "document -> answer",
    runner = runner,
    max_iterations = 1L,
    max_llm_calls = 0L,
    tools = list(domain_lookup = function(value) value)
  )
  chat <- new_rlm_parity_chat(list(list(
    reasoning = "Read the staged value",
    code = "SUBMIT(nchar(.context$document))"
  )))

  result <- module$forward(list(document = secret), .llm = chat)
  audit <- runner$execute(paste(
    "list(",
    "  state = ls(pattern = '^\\\\.dsprrr_rlm_state_', all.names = TRUE),",
    "  context = .context,",
    "  bridge = intersect(",
    "    c('SUBMIT', 'peek', 'llm_query', 'llm_query_batched',",
    "      '.rlm_host_tool_call'),",
    "    ls(all.names = TRUE)",
    "  ),",
    "  transient = ls(pattern = '^\\\\.rlm_', all.names = TRUE),",
    "  preserved = c(search, domain_lookup)",
    ")",
    sep = "\n"
  ))

  expect_identical(result$output[[1L]]$answer, as.character(nchar(secret)))
  expect_true(audit$success)
  expect_length(audit$result$state, 0L)
  expect_length(audit$result$context, 0L)
  expect_length(audit$result$bridge, 0L)
  expect_length(audit$result$transient, 0L)
  expect_identical(
    unname(audit$result$preserved),
    c("PREEXISTING-SEARCH", "PREEXISTING-TOOL")
  )
  expect_false(grepl(
    secret,
    paste(capture.output(str(audit$result)), collapse = "\n"),
    fixed = TRUE
  ))
})

test_that("the outer LM supplies composable recursive-query values", {
  skip_if_not_installed("callr")

  chat <- new_rlm_parity_chat(
    actions = list(list(
      reasoning = "Ask two focused questions and combine their answers",
      code = paste(
        "first <- llm_query('first question')",
        "second <- llm_query('second question')",
        "combined <- paste(first, second, sep = ' + ')",
        "SUBMIT(combined)",
        sep = "\n"
      )
    )),
    query_values = list("alpha", "beta")
  )
  module <- rlm_module(
    "question -> answer: string",
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    sub_lm = NULL,
    max_iterations = 1L,
    max_llm_calls = 2L
  )

  result <- module$forward(list(question = "Combine two answers"), .llm = chat)

  expect_identical(result$output[[1L]]$answer, "alpha + beta")
  expect_identical(result$metadata[[1L]]$llm_calls, 2L)
  expect_identical(result$metadata[[1L]]$provider_calls, 3L)
  expect_identical(chat$parity_state$query_index, 2L)
  expect_identical(
    chat$parity_state$query_prompts,
    c("first question", "second question")
  )
  expect_identical(chat$parity_state$action_index, 1L)
})

test_that("recursive values stay complete while displayed evidence is bounded", {
  skip_if_not_installed("callr")

  recursive_value <- paste0("HEAD-", strrep("x", 200L), "-TAIL")
  chat <- new_rlm_parity_chat(
    actions = list(list(
      reasoning = "Use the complete recursive result",
      code = paste(
        "value <- llm_query('return the evidence')",
        "SUBMIT(nchar(value))",
        sep = "\n"
      )
    )),
    query_values = list(recursive_value)
  )
  module <- rlm_module(
    "question -> answer: integer",
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    max_iterations = 1L,
    max_llm_calls = 1L,
    max_output_chars = 40L
  )

  result <- module$forward(list(question = "measure"), .llm = chat)

  expect_identical(result$output[[1L]]$answer, nchar(recursive_value))
  expect_identical(result$metadata[[1L]]$output_source, "submit")
})

test_that("typed SUBMIT failures are repairable on the next turn", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())
  cases <- list(
    integer = list(
      type = ellmer::type_integer(),
      invalid = "SUBMIT(value = 1.5)",
      valid = "SUBMIT(value = 2L)",
      expected = 2L
    ),
    structural_integer = list(
      type = ellmer::type_integer(),
      invalid = "SUBMIT(value = list('2'))",
      valid = "SUBMIT(value = 2L)",
      expected = 2L
    ),
    boolean = list(
      type = ellmer::type_boolean(),
      invalid = "SUBMIT(value = 'maybe')",
      valid = "SUBMIT(value = TRUE)",
      expected = TRUE
    ),
    enum = list(
      type = ellmer::type_enum(c("yes", "no")),
      invalid = "SUBMIT(value = 'unknown')",
      valid = "SUBMIT(value = 'yes')",
      expected = "yes"
    ),
    array = list(
      type = ellmer::type_array(ellmer::type_integer()),
      invalid = "SUBMIT(value = list(1L, 'bad'))",
      valid = "SUBMIT(value = c(1L, 2L))",
      expected = c(1L, 2L)
    ),
    object = list(
      type = ellmer::type_object(
        code = ellmer::type_string(),
        score = ellmer::type_number()
      ),
      invalid = "SUBMIT(value = list(code = 'ok'))",
      valid = "SUBMIT(value = list(code = 'ok', score = 0.5))",
      expected = list(code = "ok", score = 0.5)
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    sig <- signature(
      inputs = list(input("question")),
      output_type = ellmer::type_object(value = case$type),
      instructions = "Return one typed value."
    )
    chat <- new_rlm_parity_chat(list(
      list(reasoning = "Try an invalid value", code = case$invalid),
      list(reasoning = "Repair the value", code = case$valid)
    ))
    module <- rlm_module(
      sig,
      runner = runner,
      max_iterations = 2L,
      max_llm_calls = 0L
    )

    result <- module$forward(list(question = case_name), .llm = chat)
    history <- result$metadata[[1L]]$repl_history

    expect_identical(length(history), 2L, info = case_name)
    expect_false(history[[1L]]$success, info = case_name)
    expect_false(history[[1L]]$is_final, info = case_name)
    expect_match(history[[1L]]$output, "wrong type", info = case_name)
    expect_match(
      chat$parity_state$action_prompts[[2L]],
      "wrong type",
      info = case_name
    )
    expect_true(history[[2L]]$success, info = case_name)
    expect_true(history[[2L]]$is_final, info = case_name)
    expect_identical(
      result$output[[1L]]$value,
      case$expected,
      info = case_name
    )
  }
})

test_that("optional inputs, outputs, and ignored fields follow the signature", {
  skip_if_not_installed("callr")

  sig <- signature(
    inputs = list(
      input("question"),
      input("context_note", type = ellmer::type_string(required = FALSE))
    ),
    output_type = ellmer::type_object(
      answer = ellmer::type_string(),
      note = ellmer::type_string(required = FALSE),
      hidden = ellmer::type_ignore()
    )
  )
  module <- rlm_module(
    sig,
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  chat <- new_rlm_parity_chat(list(list(
    reasoning = "Only the required result is needed",
    code = "SUBMIT(answer = 'ok')"
  )))

  result <- run(
    module,
    question = "test",
    .llm = chat,
    .return_format = "structured"
  )

  expect_identical(result$output, list(answer = "ok"))
  prompt <- chat$parity_state$action_prompts[[1L]]
  expect_match(prompt, "note.*optional")
  expect_false(grepl("hidden", prompt, fixed = TRUE))
})

test_that("all-optional and nested ignored outputs may be omitted", {
  skip_if_not_installed("callr")

  optional <- rlm_module(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_object(
        note = ellmer::type_string(required = FALSE)
      )
    ),
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  empty_result <- optional$forward(
    list(question = "none"),
    .llm = new_rlm_parity_chat(list(list(
      reasoning = "No output is needed",
      code = "SUBMIT()"
    )))
  )
  expect_identical(empty_result$output[[1L]], list())

  nested <- rlm_module(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_object(
        answer = ellmer::type_object(
          value = ellmer::type_string(),
          hidden = ellmer::type_ignore()
        )
      )
    ),
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  nested_result <- nested$forward(
    list(question = "nested"),
    .llm = new_rlm_parity_chat(list(list(
      reasoning = "Return the visible field",
      code = "SUBMIT(answer = list(value = 'ok'))"
    )))
  )
  expect_identical(nested_result$output[[1L]]$answer, list(value = "ok"))
})

test_that("SUBMIT prevents later guest and host-tool effects", {
  skip_if_not_installed("callr")

  calls <- 0L
  increment_counter <- function() {
    calls <<- calls + 1L
    calls
  }
  module <- rlm_module(
    "question -> answer",
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    tools = list(increment_counter = increment_counter),
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  result <- module$forward(
    list(question = "stop"),
    .llm = new_rlm_parity_chat(list(list(
      reasoning = "Submit immediately",
      code = paste(
        "frame <- try(SUBMIT('ok'), silent = TRUE)",
        "increment_counter()",
        "frame",
        sep = "; "
      )
    )))
  )

  expect_identical(result$output[[1L]]$answer, "ok")
  expect_identical(calls, 0L)
})

test_that("one ordered replay ledger rejects cross-kind divergence", {
  skip_if_not_installed("callr")

  host_calls <- 0L
  query_calls <- 0L
  ping <- function() {
    host_calls <<- host_calls + 1L
    "pong"
  }
  sub_lm <- new_test_chat(
    chat = function(prompt) {
      query_calls <<- query_calls + 1L
      "unexpected"
    }
  )
  branch_option <- paste0("dsprrr.rlm.replay.", sample.int(1e8, 1L))
  code <- paste0(
    "if (is.null(getOption('",
    branch_option,
    "'))) { options(",
    branch_option,
    " = TRUE); ping() } else { llm_query('must not run') }"
  )
  module <- rlm_module(
    "question -> answer",
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    sub_lm = sub_lm,
    tools = list(ping = ping),
    max_iterations = 2L,
    max_llm_calls = 1L
  )
  chat <- new_rlm_parity_chat(list(
    list(reasoning = "Take a replay-unstable branch", code = code),
    list(reasoning = "Recover", code = "SUBMIT('recovered')")
  ))

  result <- module$forward(list(question = "test"), .llm = chat)

  expect_identical(result$output[[1L]]$answer, "recovered")
  expect_identical(host_calls, 1L)
  expect_identical(query_calls, 0L)
  expect_match(
    result$metadata[[1L]]$repl_history[[1L]]$output,
    "replay state"
  )
})

test_that("tool contracts never reveal function default expressions", {
  skip_if_not_installed("callr")

  secret <- "TOOL-DEFAULT-SECRET"
  lookup <- eval(bquote(function(key, token = .(secret)) key))
  module <- rlm_module(
    "question -> answer",
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    tools = list(lookup = lookup),
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  chat <- new_rlm_parity_chat(list(list(
    reasoning = "No tool needed",
    code = "SUBMIT('ok')"
  )))
  module$forward(list(question = "test"), .llm = chat)

  prompt <- chat$parity_state$action_prompts[[1L]]
  expect_false(grepl(secret, prompt, fixed = TRUE))
  expect_match(prompt, "token = <default>", fixed = TRUE)
})

test_that("typed SUBMIT accepts an explicitly empty array", {
  skip_if_not_installed("callr")

  module <- rlm_module(
    signature(
      inputs = list(input("question")),
      output_type = ellmer::type_object(
        values = ellmer::type_array(ellmer::type_integer())
      )
    ),
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  chat <- new_rlm_parity_chat(list(list(
    reasoning = "The collection is empty",
    code = "SUBMIT(values = integer())"
  )))

  result <- module$forward(list(question = "none"), .llm = chat)

  expect_length(result$output[[1L]]$values, 0L)
  expect_identical(result$metadata[[1L]]$output_source, "submit")
})

test_that("fallback provider and output failures are terminal", {
  skip_if_not_installed("callr")

  runner <- r_code_runner(timeout = 10, persistent = TRUE)
  withr::defer(runner$shutdown())

  provider_chat <- new_rlm_parity_chat(
    actions = list(list(reasoning = "Inspect", code = "1 + 1")),
    fallback_error = "structured extraction unavailable"
  )
  provider_module <- rlm_module(
    "question -> answer: integer",
    runner = runner,
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  provider_error <- capture_rlm_parity_error(
    provider_module$forward(list(question = "test"), .llm = provider_chat)
  )

  expect_s3_class(provider_error, "dsprrr_rlm_fallback_error")
  expect_match(conditionMessage(provider_error), "fallback extraction failed")
  expect_identical(provider_chat$parity_state$action_index, 1L)
  expect_identical(provider_chat$parity_state$fallback_calls, 1L)

  invalid_chat <- new_rlm_parity_chat(
    actions = list(list(reasoning = "Inspect", code = "1 + 1")),
    fallback = list(answer = "not an integer")
  )
  invalid_module <- rlm_module(
    "question -> answer: integer",
    runner = runner,
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  invalid_error <- capture_rlm_parity_error(
    invalid_module$forward(list(question = "test"), .llm = invalid_chat)
  )

  expect_s3_class(invalid_error, "dsprrr_rlm_output_validation_error")
  expect_match(conditionMessage(invalid_error), "wrong type")
  expect_identical(invalid_chat$parity_state$action_index, 1L)
  expect_identical(invalid_chat$parity_state$fallback_calls, 1L)
})

test_that("direct forward rejects multi-row data frames before leasing a runner", {
  factory_calls <- 0L
  module <- rlm_module(
    "question -> answer",
    interpreter_factory = function() {
      factory_calls <<- factory_calls + 1L
      stop("factory should not run", call. = FALSE)
    }
  )

  error <- capture_rlm_parity_error(module$forward(data.frame(
    question = c("first", "second")
  )))

  expect_s3_class(error, "dsprrr_rlm_batch_error")
  expect_match(conditionMessage(error), "exactly one data-frame row")
  expect_identical(factory_calls, 0L)
})

test_that("RLM predictors are visible through named_modules", {
  module <- rlm_module(
    "question -> answer",
    interpreter_factory = function() r_code_runner(persistent = TRUE)
  )

  children <- named_modules(module, include_root = FALSE)

  expect_setequal(
    names(children),
    c("$/generate_action", "$/extract")
  )
  expect_identical(children[["$/generate_action"]], module$generate_action)
  expect_identical(children[["$/extract"]], module$extract)
})

test_that("RLM prompts carry the task, output contract, and bounded head and tail", {
  skip_if_not_installed("callr")

  sig <- signature(
    "question -> answer: string, confidence: number",
    instructions = "PARITY_TASK_INSTRUCTION: preserve exact evidence."
  )
  chat <- new_rlm_parity_chat(list(
    list(
      reasoning = "Produce deliberately long output",
      code = paste0(
        "cat(paste0('HEAD-SIGNAL-', strrep('m', 800), ",
        " '-TAIL-SIGNAL'))"
      )
    ),
    list(
      reasoning = "Submit the bounded result",
      code = "SUBMIT(answer = 'ok', confidence = 1)"
    )
  ))
  module <- rlm_module(
    sig,
    interpreter_factory = function() {
      r_code_runner(
        timeout = 10,
        max_output_chars = 5000L,
        persistent = TRUE
      )
    },
    max_iterations = 2L,
    max_llm_calls = 0L,
    max_output_chars = 240L
  )

  result <- module$forward(list(question = "Inspect output"), .llm = chat)
  first_prompt <- chat$parity_state$action_prompts[[1L]]
  second_prompt <- chat$parity_state$action_prompts[[2L]]

  expect_identical(result$output[[1L]]$answer, "ok")
  expect_true(grepl("PARITY_TASK_INSTRUCTION", first_prompt, fixed = TRUE))
  expect_true(grepl("`answer`: string", first_prompt, fixed = TRUE))
  expect_true(grepl("`confidence`: number", first_prompt, fixed = TRUE))
  expect_true(grepl("HEAD-SIGNAL-", second_prompt, fixed = TRUE))
  expect_true(grepl("-TAIL-SIGNAL", second_prompt, fixed = TRUE))
  expect_true(grepl("characters omitted; total", second_prompt, fixed = TRUE))
})


test_that("RLM fallback preserves task instructions and the configured output bound", {
  skip_if_not_installed("callr")

  decisive_tail <- "DECISIVE-TAIL-EVIDENCE"
  chat <- new_rlm_parity_chat(
    actions = list(list(
      reasoning = "Produce evidence for fallback extraction",
      code = paste0(
        "marker <- paste0('DECISIVE', '-TAIL-EVIDENCE'); ",
        "cat(paste0('HEAD-', strrep('x', 3000L), '-', marker))"
      )
    )),
    fallback = function(prompt, type) {
      list(
        answer = if (
          grepl(decisive_tail, prompt, fixed = TRUE) &&
            grepl("FALLBACK-TASK-MARKER", prompt, fixed = TRUE)
        ) {
          "evidence retained"
        } else {
          "evidence missing"
        }
      )
    }
  )
  module <- rlm_module(
    signature(
      "question, records -> answer: string",
      instructions = "FALLBACK-TASK-MARKER: use the decisive tail."
    ),
    interpreter_factory = function() {
      r_code_runner(
        timeout = 10,
        max_output_chars = 5000L,
        persistent = TRUE
      )
    },
    max_iterations = 1L,
    max_llm_calls = 0L,
    max_output_chars = 4000L
  )

  result <- suppressWarnings(module$forward(
    list(
      question = "Inspect the evidence",
      records = data.frame(
        ordinary = 1L,
        decisive_column = "available"
      )
    ),
    .llm = chat
  ))
  fallback_prompt <- chat$parity_state$fallback_prompts[[1L]]

  expect_identical(result$output[[1L]]$answer, "evidence retained")
  expect_match(fallback_prompt, "FALLBACK-TASK-MARKER", fixed = TRUE)
  expect_match(fallback_prompt, decisive_tail, fixed = TRUE)
  expect_match(fallback_prompt, "decisive_column", fixed = TRUE)
  expect_gt(regexpr(decisive_tail, fallback_prompt, fixed = TRUE)[[1L]], 2000L)
})


test_that("RLM excerpts honor very small output bounds", {
  module <- rlm_module(
    "question -> answer",
    runner = r_code_runner(),
    max_output_chars = 40L
  )
  excerpt <- module$.__enclos_env__$private$format_excerpt(strrep("x", 100000L))

  expect_lte(nchar(excerpt), 40L)
  expect_identical(excerpt, strrep("x", 40L))
})

test_that("RLM bounds rendered data frames before retaining trajectory output", {
  module <- rlm_module(
    "question -> answer",
    interpreter_factory = function() stop("not used"),
    max_output_chars = 300L
  )
  large <- data.frame(
    id = seq_len(40000L),
    value = sprintf("row-%05d", seq_len(40000L))
  )
  formatted <- module$.__enclos_env__$private$format_execution_output(list(
    stdout = "",
    messages = "",
    warnings = "",
    result = large
  ))

  expect_lte(nchar(formatted), 300L)
  expect_match(formatted, "39980 data-frame rows omitted", fixed = TRUE)
  expect_false(grepl("row-20000", formatted, fixed = TRUE))
})

test_that("RLM usage totals include recursive calls and fail closed on gaps", {
  module <- rlm_module(
    "question -> answer",
    interpreter_factory = function() stop("not used")
  )
  action <- list(
    input_tokens = 2L,
    cached_input_tokens = 0L,
    output_tokens = 3L,
    total_tokens = 5L,
    cost = 0.01
  )
  recursive <- list(
    input_tokens = 7L,
    cached_input_tokens = 1L,
    output_tokens = 11L,
    total_tokens = 18L,
    cost = 0.02
  )
  known <- module$.__enclos_env__$private$summarize_action_usage(
    list(list(action_metadata = action)),
    recursive_metadata = list(recursive)
  )
  unknown <- module$.__enclos_env__$private$summarize_action_usage(
    list(list(action_metadata = action)),
    recursive_metadata = list(canonical_usage_metadata())
  )

  expect_identical(known$total_tokens, 23L)
  expect_equal(known$cost, 0.03)
  expect_true(is.na(unknown$total_tokens))
  expect_true(is.na(unknown$cost))
})

test_that("RLM chat clones clear pre-existing conversation turns", {
  chat <- new_test_chat(turns = list("private prior turn"))

  result <- dsprrr:::clone_rlm_chat(chat)

  expect_s3_class(result, "Chat")
  expect_false(identical(result, chat))
  expect_identical(result$get_turns(), list())
  expect_identical(chat$get_turns(), list("private prior turn"))
})

test_that("RLM traces retain input hashes and sizes, not source values", {
  skip_if_not_installed("callr")

  secret <- paste0("TRACE-SECRET-", strrep("do-not-retain-", 200L))
  chat <- new_rlm_parity_chat(list(list(
    reasoning = "Return a fixed answer",
    code = "SUBMIT('done')"
  )))
  module <- rlm_module(
    "document -> answer: string",
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    max_iterations = 1L,
    max_llm_calls = 0L
  )

  module$forward(list(document = secret), .llm = chat, trace = TRUE)
  history_summary <- module$get_repl_history()[[1L]]$inputs$document
  trace_summary <- module$state$traces[[1L]]$inputs$document
  retained <- paste(
    utils::capture.output(str(list(history_summary, trace_summary))),
    collapse = "\n"
  )

  expect_identical(
    history_summary$sha256,
    digest::digest(secret, algo = "sha256")
  )
  expect_identical(history_summary$length, 1L)
  expect_gt(history_summary$bytes, 0)
  expect_identical(trace_summary, history_summary)
  expect_false("value" %in% names(history_summary))
  expect_false(grepl("TRACE-SECRET", retained, fixed = TRUE))
})

test_that("bounded RLM traces still expose newly appended events", {
  skip_if_not_installed("callr")
  withr::local_options(list(dsprrr.rlm_trace_limit = 2L))

  module <- rlm_module(
    "question -> answer: string",
    interpreter_factory = function() {
      r_code_runner(timeout = 10, persistent = TRUE)
    },
    max_iterations = 1L,
    max_llm_calls = 0L
  )
  run_once <- function(answer) {
    chat <- new_rlm_parity_chat(list(list(
      reasoning = "Submit",
      code = paste0("SUBMIT('", answer, "')")
    )))
    module$forward(list(question = answer), .llm = chat, trace = TRUE)
  }

  run_once("first")
  run_once("second")
  cursor <- dsprrr:::evaluation_trace_cursor(module)
  run_once("third")
  events <- dsprrr:::new_evaluation_trace_events(module, cursor)

  expect_length(module$state$traces, 2L)
  expect_identical(module$state$trace_sequence, 3)
  expect_length(events, 1L)
  expect_identical(events[[1L]]$output$answer, "third")
})

test_that("the 40k release investigation is exact and keeps the table external", {
  skip_if_not_installed("callr")

  n <- 5000L
  sessions <- expand.grid(
    release = c("2.3.9", "2.4.0"),
    platform = c("desktop", "mobile"),
    plan = c("free", "pro"),
    within_group = seq_len(n),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  base_rate <- c(
    "desktop.free" = 0.88,
    "mobile.free" = 0.84,
    "desktop.pro" = 0.94,
    "mobile.pro" = 0.92
  )
  cohort_key <- paste(sessions$platform, sessions$plan, sep = ".")
  conversion_rate <- unname(base_rate[cohort_key])
  conversion_rate[
    sessions$release == "2.4.0" & cohort_key == "mobile.pro"
  ] <- 0.61
  sessions$converted <- sessions$within_group <= n * conversion_rate
  sessions$audit_marker <- ""
  full_table_sentinel <- "FULL-TABLE-SENTINEL-ROW-40000"
  sessions$audit_marker[[nrow(sessions)]] <- full_table_sentinel

  changes <- data.frame(
    change_id = sprintf("CHG-%04d", 1701:1900),
    release = rep(c("2.3.9", "2.4.0"), each = 100),
    component = "miscellaneous",
    note = "Routine maintenance with no expected checkout impact."
  )
  target <- which(changes$change_id == "CHG-1842")
  changes$component[[target]] <- "checkout-auth"
  changes$note[[target]] <- paste(
    "Mobile Pro token refresh:",
    "retry budget changed from 3 to 0 and timeout from 8 s to 800 ms."
  )
  question <- paste(
    "Checkout conversion fell after release 2.4.0.",
    "Find the finest low-cardinality categorical cohort with the largest",
    "before/after drop; do not stop at a marginal roll-up that dilutes the",
    "change. Identify its dimensions and values, quantify the rates, and cite",
    "the change-log record that is the strongest candidate explanation."
  )
  sig <- signature(
    paste(
      "sessions, changes, question ->",
      "release: string, cohort: string,",
      "before_rate: number, after_rate: number,",
      "drop_pp: number, change_id: string, evidence: string"
    ),
    instructions = paste(
      "Inspect the schema and low-cardinality categorical fields; exclude the",
      "release, outcome, and row-index fields from candidate cohort dimensions.",
      "Find the finest low-cardinality cohort with the largest before/after",
      "drop; do not stop at a marginal roll-up that dilutes the change.",
      "Quantify it and cite the strongest matching change record.",
      "Format cohort in source-column order as '<dimension>=<value> / ...'.",
      "Copy the selected change note verbatim into evidence. Treat that record",
      "as evidence, not proof of causation."
    )
  )
  chat <- new_rlm_parity_chat(list(
    list(
      reasoning = "Inspect shape and schema without printing rows",
      code = paste(
        "schema <- list(",
        "  session_dim = dim(.context$sessions),",
        "  session_fields = names(.context$sessions),",
        "  releases = table(.context$sessions$release),",
        "  change_fields = names(.context$changes)",
        ")",
        "schema",
        sep = "\n"
      )
    ),
    list(
      reasoning = "Calculate each cohort's before and after rates",
      code = paste(
        "rates <- aggregate(",
        "  converted ~ release + platform + plan,",
        "  data = .context$sessions,",
        "  FUN = mean",
        ")",
        "before <- subset(rates, release == '2.3.9')",
        "after <- subset(rates, release == '2.4.0')",
        "deltas <- merge(",
        "  before, after,",
        "  by = c('platform', 'plan'),",
        "  suffixes = c('_before', '_after')",
        ")",
        "deltas$drop_pp <- 100 * (",
        "  deltas$converted_before - deltas$converted_after",
        ")",
        "deltas[order(-deltas$drop_pp), ]",
        sep = "\n"
      )
    ),
    list(
      reasoning = "Inspect only the matching change record",
      code = paste(
        "candidate <- subset(",
        "  .context$changes,",
        "  release == '2.4.0' &",
        "    grepl('token|retry', paste(component, note), ignore.case = TRUE)",
        ")",
        "candidate",
        sep = "\n"
      )
    ),
    list(
      reasoning = "Submit the computed incident evidence",
      code = paste(
        "winner <- deltas[which.max(deltas$drop_pp), ]",
        "SUBMIT(",
        "  release = after$release[[1L]],",
        paste0(
          "  cohort = paste0('platform=', winner$platform, ",
          " ' / plan=', winner$plan),"
        ),
        "  before_rate = winner$converted_before,",
        "  after_rate = winner$converted_after,",
        "  drop_pp = winner$drop_pp,",
        "  change_id = candidate$change_id[[1L]],",
        "  evidence = candidate$note[[1L]]",
        ")",
        sep = "\n"
      )
    )
  ))
  investigator <- rlm_module(
    sig,
    interpreter_factory = function() {
      r_code_runner(timeout = 20, persistent = TRUE)
    },
    max_iterations = 4L,
    max_llm_calls = 0L,
    max_output_chars = 10000L
  )

  result <- run(
    investigator,
    sessions = sessions,
    changes = changes,
    question = question,
    .llm = chat,
    .return_format = "structured"
  )
  output <- result$output
  prompts <- chat$parity_state$action_prompts

  expect_identical(nrow(sessions), 40000L)
  expect_identical(output$release, "2.4.0")
  expect_identical(output$cohort, "platform=mobile / plan=pro")
  expect_equal(output$before_rate, 0.92)
  expect_equal(output$after_rate, 0.61)
  expect_equal(output$drop_pp, 31)
  expect_identical(output$change_id, "CHG-1842")
  expect_identical(output$evidence, changes$note[[target]])
  expect_identical(result$metadata$iterations, 4L)
  expect_identical(length(prompts), 4L)
  expect_true(grepl("40000 rows x 6 cols", prompts[[1L]], fixed = TRUE))
  expect_false(any(grepl(full_table_sentinel, prompts, fixed = TRUE)))
  expect_lt(max(nchar(prompts)), as.numeric(utils::object.size(sessions)))
})
