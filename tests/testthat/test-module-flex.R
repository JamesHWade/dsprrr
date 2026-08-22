flex_test_json <- function(steps, outputs, ...) {
  fields <- list(...)
  source <- c(
    list(schema_version = 1L, steps = steps, outputs = outputs),
    fields
  )
  as.character(jsonlite::toJSON(
    source,
    auto_unbox = TRUE,
    null = "null",
    digits = NA,
    pretty = FALSE
  ))
}

flex_test_step <- function(
  name = "predict",
  primitive = "predict",
  signature = "$outer",
  inputs = list(question = "$input.question"),
  instructions = NULL
) {
  step <- list(
    name = name,
    primitive = primitive,
    signature = signature,
    inputs = inputs
  )
  if (!is.null(instructions)) {
    step$instructions <- instructions
  }
  step
}

flex_test_program <- function(...) suppressWarnings(flex(...))

flex_two_step_source <- function() {
  flex_test_json(
    steps = list(
      flex_test_step(
        name = "draft",
        signature = "question -> draft"
      ),
      flex_test_step(
        name = "finish",
        primitive = "chain_of_thought",
        signature = "draft -> answer",
        inputs = list(draft = "$step.draft.draft"),
        instructions = "Use the draft to produce the final answer."
      )
    ),
    outputs = list(answer = "$step.finish.answer")
  )
}

flex_test_chat <- function() {
  chat <- NULL
  chat <- new_test_chat(
    model = "flex-test-model",
    chat_structured = function(prompt, type, ...) {
      chat$parity_state$calls <- chat$parity_state$calls + 1L
      prompt <- as.character(prompt)
      chat$parity_state$prompts <- c(chat$parity_state$prompts, prompt)
      fields <- names(type@properties)
      response <- if (identical(fields, "draft")) {
        list(draft = "a checked draft")
      } else if (identical(fields, c("reasoning", "answer"))) {
        list(reasoning = "checked", answer = "the final answer")
      } else {
        stop("unexpected Flex output schema")
      }
      chat$turns <- c(
        chat$turns,
        list(
          ellmer::UserTurn(
            contents = list(ellmer::ContentText(prompt))
          ),
          ellmer::AssistantTurn(
            contents = list(ellmer::ContentText("ok")),
            tokens = c(2L, 1L, 0L),
            cost = 0.001,
            duration = 0.01
          )
        )
      )
      response
    }
  )
  chat$parity_state <- list(calls = 0L, prompts = character())
  chat$calls <- function() chat$parity_state$calls
  chat$prompts <- function() chat$parity_state$prompts
  chat
}

test_that("flex gives one experimental lifecycle warning per session", {
  # pkgload can replace the active namespace while an already-sourced test
  # environment still resolves `flex()` from the previous package environment.
  # Reset the state used by the exact function under test, not whichever
  # namespace `asNamespace()` currently reports.
  lifecycle <- get(
    ".flex_lifecycle",
    envir = environment(flex),
    inherits = FALSE
  )
  old <- lifecycle$warned
  withr::defer({
    lifecycle$warned <- old
  })
  lifecycle$warned <- FALSE

  expect_warning(
    first <- flex("question -> answer"),
    class = "dsprrr_flex_experimental_warning"
  )
  second <- expect_no_warning(flex("question -> answer"))
  expect_s3_class(first, "FlexModule")
  expect_s3_class(second, "FlexModule")
  expect_s3_class(first, "PredictModule")
})

test_that("flex baseline is canonical, bounded, and read-only", {
  program <- flex_test_program("question -> answer")
  expected <- paste0(
    '{"schema_version":1,"steps":[{"name":"predict",',
    '"primitive":"predict","signature":"$outer",',
    '"inputs":{"question":"$input.question"}}],',
    '"outputs":{"answer":"$step.predict.answer"}}'
  )

  expect_identical(program$module_src, expected)
  expect_identical(program$max_predictor_calls, 100L)
  expect_true(program$graph_is_parameter())
  expect_identical(named_parameters(program)[["$"]], program)
  expect_error(
    program$module_src <- "{}",
    class = "dsprrr_flex_read_only_error"
  )
  expect_error(
    program$max_predictor_calls <- 2L,
    class = "dsprrr_flex_read_only_error"
  )
})

test_that("flex supports unlimited call budgets and deterministic plans", {
  default_creations <- 0L
  testthat::local_mocked_bindings(
    get_default_chat = function(create = TRUE) {
      if (isTRUE(create)) {
        default_creations <<- default_creations + 1L
      }
      NULL
    },
    .package = "dsprrr"
  )
  deterministic <- flex_test_json(
    steps = list(),
    outputs = list(result = "$input.value")
  )
  program <- flex_test_program(
    "value: string -> result: string",
    module_src = deterministic,
    max_predictor_calls = NULL
  )

  result <- program$forward(list(value = "unchanged"))

  expect_null(program$max_predictor_calls)
  expect_identical(result$output[[1L]], list(result = "unchanged"))
  expect_identical(result$metadata[[1L]]$predictor_calls, 0L)
  expect_identical(result$metadata[[1L]]$total_tokens, 0L)
  expect_length(result$metadata[[1L]]$program_trace_events, 0L)
  expect_length(program$state$traces, 0L)
  dataset_result <- run_dataset(
    program,
    data.frame(value = c("first", "second")),
    .progress = FALSE
  )
  expect_identical(
    dataset_result$result,
    list(list(result = "first"), list(result = "second"))
  )
  expect_identical(default_creations, 0L)

  zero_budget <- flex_test_program(
    "value: string -> result: string",
    module_src = deterministic,
    max_predictor_calls = 0L
  )
  expect_identical(zero_budget$max_predictor_calls, 0L)
  expect_identical(
    zero_budget$forward(list(value = "zero"))$output[[1L]],
    list(result = "zero")
  )

  unrestricted <- flex_test_program(
    "question -> answer",
    module_src = flex_two_step_source(),
    max_predictor_calls = NULL
  )
  expect_null(unrestricted$max_predictor_calls)
  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_two_step_source(),
      max_predictor_calls = 1L
    ),
    class = "dsprrr_flex_budget_error"
  )
})

test_that("flex canonicalizes field order and harmless JSON whitespace", {
  source <- list(
    outputs = list(answer = "$step.draft.draft"),
    steps = list(list(
      inputs = list(question = "$input.question"),
      signature = "  question -> draft  ",
      primitive = "predict",
      name = "draft"
    )),
    schema_version = 1L
  )
  pretty <- jsonlite::toJSON(
    source,
    auto_unbox = TRUE,
    pretty = TRUE
  )
  program <- flex_test_program("question -> answer", module_src = pretty)

  expect_false(grepl("\n", program$module_src, fixed = TRUE))
  expect_identical(
    program$module_src,
    paste0(
      '{"schema_version":1,"steps":[{"name":"draft",',
      '"primitive":"predict","signature":"question -> draft",',
      '"inputs":{"question":"$input.question"}}],',
      '"outputs":{"answer":"$step.draft.draft"}}'
    )
  )
  canonical <- program$module_src
  program$bind(jsonlite::prettify(canonical))
  expect_identical(program$module_src, canonical)
})

test_that("flex requires a steps array and supports a zero-input baseline", {
  object_steps <- paste0(
    '{"schema_version":1,"steps":{"predict":',
    '{"name":"predict","primitive":"predict","signature":"$outer",',
    '"inputs":{"question":"$input.question"}}},',
    '"outputs":{"answer":"$step.predict.answer"}}'
  )
  expect_error(
    flex_test_program("question -> answer", module_src = object_steps),
    class = "dsprrr_flex_schema_error"
  )

  program <- flex_test_program("-> answer")
  expect_match(program$module_src, '"inputs":{}', fixed = TRUE)
  calls <- 0L
  chat <- new_test_chat(
    model = "zero-input-test",
    chat_structured = function(prompt, type, ...) {
      calls <<- calls + 1L
      list(answer = "ready")
    }
  )
  result <- program$forward(list(), .llm = chat)

  expect_identical(result$output[[1L]], list(answer = "ready"))
  expect_identical(calls, 1L)
  expect_error(
    program$forward(list(extra = "closed"), .llm = chat),
    class = "dsprrr_extra_input_error"
  )
  expect_error(
    run(program, extra = "closed", .llm = chat),
    class = "dsprrr_extra_input_error"
  )
  expect_identical(calls, 1L)
})

test_that("flex rejects unknown schema fields and unsafe step definitions", {
  unknown_top <- flex_test_json(
    list(flex_test_step()),
    list(answer = "$step.predict.answer"),
    surprise = TRUE
  )
  expect_error(
    flex_test_program("question -> answer", module_src = unknown_top),
    class = "dsprrr_flex_schema_error"
  )

  unknown_step <- flex_test_step()
  unknown_step$code <- "system('false')"
  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_test_json(
        list(unknown_step),
        list(answer = "$step.predict.answer")
      )
    ),
    class = "dsprrr_flex_schema_error"
  )

  null_source <- paste0(
    '{"schema_version":1,"steps":[{"name":"predict",',
    '"primitive":"predict","signature":"$outer",',
    '"inputs":{"question":"$input.question"},"instructions":null}],',
    '"outputs":{"answer":"$step.predict.answer"}}'
  )
  expect_error(
    flex_test_program("question -> answer", module_src = null_source),
    class = "dsprrr_flex_schema_error"
  )

  for (primitive in c("react", "program_of_thought", "R")) {
    expect_error(
      flex_test_program(
        "question -> answer",
        module_src = flex_test_json(
          list(flex_test_step(primitive = primitive)),
          list(answer = "$step.predict.answer")
        )
      ),
      class = "dsprrr_flex_primitive_error"
    )
  }

  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_test_json(
        list(flex_test_step(name = "bad.name")),
        list(answer = "$step.bad.name.answer")
      )
    ),
    class = "dsprrr_flex_step_name_error"
  )
})

test_that("flex requires unique steps and exact step interfaces", {
  duplicate <- flex_test_json(
    list(
      flex_test_step(name = "same"),
      flex_test_step(name = "same")
    ),
    list(answer = "$step.same.answer")
  )
  expect_error(
    flex_test_program("question -> answer", module_src = duplicate),
    class = "dsprrr_flex_duplicate_step_error"
  )

  missing <- flex_test_step(inputs = list())
  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_test_json(
        list(missing),
        list(answer = "$step.predict.answer")
      )
    ),
    class = "dsprrr_flex_interface_error"
  )

  extra <- flex_test_step(
    inputs = list(
      question = "$input.question",
      extra = "$input.question"
    )
  )
  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_test_json(
        list(extra),
        list(answer = "$step.predict.answer")
      )
    ),
    class = "dsprrr_flex_interface_error"
  )
})

test_that("flex requires exact outer outputs and valid references", {
  step <- flex_test_step()
  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_test_json(list(step), list())
    ),
    class = "dsprrr_flex_interface_error"
  )
  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_test_json(
        list(step),
        list(
          answer = "$step.predict.answer",
          extra = "$step.predict.answer"
        )
      )
    ),
    class = "dsprrr_flex_interface_error"
  )
  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_test_json(
        list(step),
        list(answer = "$step.missing.answer")
      )
    ),
    class = "dsprrr_flex_reference_error"
  )
  bad_input <- flex_test_step(
    inputs = list(question = "$input.missing")
  )
  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_test_json(
        list(bad_input),
        list(answer = "$step.predict.answer")
      )
    ),
    class = "dsprrr_flex_reference_error"
  )
})

test_that("flex rejects cycles separately from forward references", {
  first <- flex_test_step(
    name = "first",
    signature = "seed -> answer",
    inputs = list(seed = "$step.second.answer")
  )
  second <- flex_test_step(
    name = "second",
    signature = "seed -> answer",
    inputs = list(seed = "$input.question")
  )
  forward <- flex_test_json(
    list(first, second),
    list(answer = "$step.first.answer")
  )
  expect_error(
    flex_test_program("question -> answer", module_src = forward),
    class = "dsprrr_flex_forward_reference_error"
  )

  second$inputs <- list(seed = "$step.first.answer")
  cyclic <- flex_test_json(
    list(first, second),
    list(answer = "$step.second.answer")
  )
  expect_error(
    flex_test_program("question -> answer", module_src = cyclic),
    class = "dsprrr_flex_cycle_error"
  )
})

test_that("flex enforces source and target type compatibility", {
  numeric_step <- flex_test_step(
    signature = "value: number -> score: number",
    inputs = list(value = "$input.question")
  )
  expect_error(
    flex_test_program(
      "question: string -> answer: string",
      module_src = flex_test_json(
        list(numeric_step),
        list(answer = "$step.predict.score")
      )
    ),
    class = "dsprrr_flex_type_error"
  )

  output_mismatch <- flex_test_step(
    signature = "question -> score: number"
  )
  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_test_json(
        list(output_mismatch),
        list(answer = "$step.predict.score")
      )
    ),
    class = "dsprrr_flex_type_error"
  )

  optional_output <- flex_test_step(
    signature = "question -> answer: Optional[string]"
  )
  expect_error(
    flex_test_program(
      "question -> answer: string",
      module_src = flex_test_json(
        list(optional_output),
        list(answer = "$step.predict.answer")
      )
    ),
    class = "dsprrr_flex_type_error"
  )

  json_schema_type <- ellmer::type_from_schema(
    text = paste0(
      '{"type":"object","properties":',
      '{"answer":{"type":"string"}}}'
    )
  )
  opaque_signature <- Signature(
    inputs = list(input("question")),
    output_type = json_schema_type
  )
  expect_error(
    flex_test_program(opaque_signature),
    class = "dsprrr_flex_type_error"
  )

  empty_object_signature <- Signature(
    inputs = list(input("question")),
    output_type = ellmer::type_object()
  )
  expect_error(
    flex_test_program(empty_object_signature),
    class = "dsprrr_flex_type_error"
  )
})

test_that("flex rejects invalid, oversized, and over-budget sources", {
  expect_error(
    flex_test_program("question -> answer", module_src = "{not json"),
    class = "dsprrr_flex_json_error"
  )

  oversized <- paste0(
    '{"schema_version":1,"steps":[],"outputs":{},"padding":"',
    strrep(
      "x",
      get(".flex_max_source_bytes", asNamespace("dsprrr"))
    ),
    '"}'
  )
  expect_error(
    flex_test_program("question -> answer", module_src = oversized),
    class = "dsprrr_flex_source_size_error"
  )

  expect_error(
    flex_test_program(
      "question -> answer",
      module_src = flex_two_step_source(),
      max_predictor_calls = 1L
    ),
    class = "dsprrr_flex_budget_error"
  )
})

test_that("flex never evaluates module source as R", {
  marker <- tempfile("flex-must-not-evaluate-")
  instruction <- paste0("file.create(", encodeString(marker, quote = '"'), ")")
  source <- flex_test_json(
    list(flex_test_step(instructions = instruction)),
    list(answer = "$step.predict.answer")
  )

  program <- flex_test_program("question -> answer", module_src = source)
  expect_s3_class(program, "FlexModule")
  expect_false(file.exists(marker))
})

test_that("flex binding and optimizer updates are transactional", {
  program <- flex_test_program("question -> answer")
  baseline <- program$module_src

  expect_error(
    program$bind("{bad json"),
    class = "dsprrr_flex_json_error"
  )
  expect_identical(program$module_src, baseline)

  candidate <- flex_two_step_source()
  expect_identical(program$bind(candidate), program)
  canonical_candidate <- program$module_src
  expect_false(identical(canonical_candidate, baseline))

  expect_error(
    program$apply_optimization_params(module_src = "{}"),
    class = "dsprrr_flex_schema_error"
  )
  expect_identical(program$module_src, canonical_candidate)

  program$apply_optimization_params(list(module_src = baseline))
  expect_identical(program$module_src, baseline)

  program$apply_optimization_params(list(instructions = "New outer rules."))
  expect_identical(program$signature@instructions, "New outer rules.")
  expect_identical(program$module_src, baseline)

  expect_error(
    program$apply_optimization_params(list(instructions = c("one", "two"))),
    class = "dsprrr_flex_config_error"
  )
  expect_identical(program$signature@instructions, "New outer rules.")
  expect_identical(program$module_src, baseline)

  expect_error(
    program$apply_optimization_params(list(instructions_extra = "oops")),
    class = "dsprrr_flex_config_error"
  )
  expect_error(
    program$apply_optimization_params(list(module_src_typo = candidate)),
    class = "dsprrr_flex_config_error"
  )
  expect_identical(program$signature@instructions, "New outer rules.")
  expect_identical(program$module_src, baseline)
})

test_that("flex rejects extra and incompatible runtime inputs exactly", {
  program <- flex_test_program("question: string -> answer")
  chat <- flex_test_chat()

  expect_error(
    program$forward(
      list(question = "q", unexpected = "not declared"),
      .llm = chat
    ),
    class = "dsprrr_extra_input_error"
  )
  expect_error(
    run(
      program,
      question = "q",
      unexpected = "not declared",
      .llm = chat
    ),
    class = "dsprrr_extra_input_error"
  )
  expect_error(
    program$forward(list(question = 42), .llm = chat),
    class = "dsprrr_type_mismatch_error"
  )
  expect_error(
    program$forward(list(question = c("one", "two")), .llm = chat),
    class = "dsprrr_flex_runtime_input_error"
  )
  expect_error(
    run(program, question = 42, .llm = chat),
    class = "dsprrr_type_mismatch_error"
  )
  expect_error(
    program$forward(data.frame(question = character()), .llm = chat),
    class = "dsprrr_flex_runtime_input_error"
  )
  expect_error(
    program$forward(
      data.frame(question = c("one", "two")),
      .llm = chat
    ),
    class = "dsprrr_flex_runtime_input_error"
  )

  nested_signature <- Signature(
    inputs = list(input(
      "payload",
      type = ellmer::type_object(
        label = ellmer::type_string(),
        counts = ellmer::type_array(ellmer::type_integer())
      )
    )),
    output_type = ellmer::type_object(answer = ellmer::type_string())
  )
  nested <- flex_test_program(nested_signature)
  expect_error(
    nested$forward(
      list(payload = list(label = 1, counts = c(1L, 2L))),
      .llm = chat
    ),
    class = "dsprrr_flex_runtime_input_error"
  )
  expect_error(
    nested$forward(
      list(payload = list(label = "ok", counts = c(1L, 1.5))),
      .llm = chat
    ),
    class = "dsprrr_flex_runtime_input_error"
  )

  valid_chat <- new_test_chat(
    model = "nested-input-test",
    chat_structured = function(...) list(answer = "accepted")
  )
  nested_row <- tibble::tibble(
    payload = list(list(label = "ok", counts = c(1L, 2L)))
  )
  nested_result <- nested$forward(nested_row, .llm = valid_chat)
  expect_identical(nested_result$output[[1L]], list(answer = "accepted"))
  expect_error(
    nested$forward(
      tibble::tibble(payload = tibble::tibble(other = "wrong field")),
      .llm = valid_chat
    ),
    class = "dsprrr_flex_runtime_input_error"
  )

  optional_signature <- Signature(
    inputs = list(
      input("question", type = ellmer::type_string()),
      input("context", type = ellmer::type_string(required = FALSE))
    ),
    output_type = ellmer::type_object(answer = ellmer::type_string())
  )
  optional <- flex_test_program(optional_signature)
  expect_identical(
    optional$forward(list(question = "q"), .llm = valid_chat)$output[[1L]],
    list(answer = "accepted")
  )
  expect_identical(
    run(optional, question = "q", .llm = valid_chat),
    list(answer = "accepted")
  )
  expect_identical(
    optional$forward(
      list(question = "q", context = "provided"),
      .llm = valid_chat
    )$output[[1L]],
    list(answer = "accepted")
  )
  expect_error(
    optional$forward(list(context = "missing question"), .llm = valid_chat),
    class = "dsprrr_flex_runtime_input_error"
  )
  expect_error(
    run(optional, context = "missing question", .llm = valid_chat),
    class = "dsprrr_flex_runtime_input_error"
  )
  expect_identical(chat$calls(), 0L)
})

test_that("flex validates every model output recursively and exactly", {
  malformed_chat <- function(response) {
    new_test_chat(
      model = "malformed-output-test",
      chat_structured = function(...) response
    )
  }
  program <- flex_test_program("question -> answer: string")
  malformed <- list(
    list(answer = 42),
    list(answer = NULL),
    list(answer = "ok", extra = "unknown"),
    list(wrong = "ok"),
    list(answer = c("one", "two"))
  )
  for (response in malformed) {
    expect_error(
      program$forward(
        list(question = "q"),
        .llm = malformed_chat(response)
      ),
      class = "dsprrr_flex_step_output_error"
    )
  }

  nested_signature <- Signature(
    inputs = list(input("question")),
    output_type = ellmer::type_object(
      payload = ellmer::type_object(
        label = ellmer::type_string(),
        scores = ellmer::type_array(ellmer::type_integer())
      )
    )
  )
  nested <- flex_test_program(nested_signature)
  nested_bad <- list(
    payload = list(label = "ok", scores = list(1L, "not an integer"))
  )
  expect_error(
    nested$forward(
      list(question = "q"),
      .llm = malformed_chat(nested_bad)
    ),
    class = "dsprrr_flex_step_output_error"
  )

  table_signature <- Signature(
    inputs = list(input("question")),
    output_type = ellmer::type_object(
      items = ellmer::type_array(ellmer::type_object(
        name = ellmer::type_string(),
        score = ellmer::type_integer(),
        tags = ellmer::type_array(ellmer::type_string()),
        meta = ellmer::type_object(
          active = ellmer::type_boolean(),
          .required = FALSE
        ),
        ignored = ellmer::type_ignore()
      ))
    )
  )
  table_program <- flex_test_program(table_signature)
  table <- tibble::tibble(
    name = c("a", "b"),
    score = c(1L, 2L),
    tags = list(c("x", "y"), "z"),
    meta = tibble::tibble(active = c(NA, TRUE)),
    ignored = list(NULL, NULL)
  )
  table_result <- table_program$forward(
    list(question = "q"),
    .llm = malformed_chat(list(items = table))
  )
  expect_identical(
    table_result$output[[1L]]$items,
    table[setdiff(names(table), "ignored")]
  )
  expect_error(
    table_program$forward(
      list(question = "q"),
      .llm = malformed_chat(list(
        items = tibble::tibble(
          name = "a",
          score = 1.5,
          tags = list("x"),
          meta = tibble::tibble(active = TRUE)
        )
      ))
    ),
    class = "dsprrr_flex_step_output_error"
  )

  scalar_array_signature <- Signature(
    inputs = list(input("question")),
    output_type = ellmer::type_array(ellmer::type_object(
      name = ellmer::type_string(),
      tags = ellmer::type_array(ellmer::type_string())
    ))
  )
  scalar_array <- flex_test_program(scalar_array_signature)
  scalar_table <- tibble::tibble(
    name = c("a", "b"),
    tags = list("x", c("y", "z"))
  )
  scalar_result <- scalar_array$forward(
    list(question = "q"),
    .llm = malformed_chat(scalar_table)
  )
  expect_identical(scalar_result$output[[1L]]$answer, scalar_table)
})

test_that("flex consistently omits ignored output properties", {
  output_type <- ellmer::type_object(
    answer = ellmer::type_string(),
    ignored = ellmer::type_ignore(),
    nested = ellmer::type_object(
      visible = ellmer::type_string(),
      ignored = ellmer::type_ignore()
    )
  )
  sig <- Signature(
    inputs = list(input("question")),
    output_type = output_type
  )
  program <- flex_test_program(sig)
  expect_false(grepl("ignored", program$module_src, fixed = TRUE))
  chat <- new_test_chat(
    model = "ignore-output-test",
    chat_structured = function(...) {
      list(
        answer = "ok",
        ignored = NULL,
        nested = list(visible = "kept", ignored = NULL)
      )
    }
  )

  result <- program$forward(list(question = "q"), .llm = chat)

  expect_identical(
    result$output[[1L]],
    list(answer = "ok", nested = list(visible = "kept"))
  )

  only_ignored <- flex_test_program(Signature(
    inputs = list(input("question")),
    output_type = ellmer::type_object(ignored = ellmer::type_ignore())
  ))
  ignored_result <- only_ignored$forward(
    list(question = "q"),
    .llm = new_test_chat(
      model = "all-ignored-output-test",
      chat_structured = function(...) list(ignored = NULL)
    )
  )
  expect_identical(ignored_result$output[[1L]], setNames(list(), character()))
})

test_that("flex records one ordered trace event per predictor call", {
  chat <- flex_test_chat()
  program <- flex_test_program(
    "question -> answer",
    module_src = flex_two_step_source()
  )

  result <- program$forward(
    list(question = "the question"),
    .llm = chat
  )

  expect_identical(result$output[[1L]], list(answer = "the final answer"))
  expect_identical(chat$calls(), 2L)
  expect_length(chat$prompts(), 2L)
  expect_match(chat$prompts()[[2L]], "a checked draft", fixed = TRUE)

  metadata <- result$metadata[[1L]]
  expect_identical(metadata$predictor_calls, 2L)
  expect_named(metadata$steps, c("draft", "finish"))
  expect_identical(metadata$input_tokens, 4L)
  expect_identical(metadata$output_tokens, 2L)
  expect_identical(metadata$total_tokens, 6L)
  expect_equal(metadata$cost, 0.002)

  expect_length(metadata$program_trace_events, 2L)
  expect_named(metadata$program_trace_events, c("draft", "finish"))
  expect_length(program$state$traces, 2L)
  expect_identical(
    unname(vapply(
      program$state$traces,
      `[[`,
      integer(1),
      "predictor_call_index"
    )),
    1:2
  )
  expect_identical(
    unname(vapply(
      program$state$traces,
      `[[`,
      character(1),
      "step_name"
    )),
    c("draft", "finish")
  )
  expect_identical(
    program$state$traces[[1L]]$inputs,
    list(question = "the question")
  )
  expect_identical(
    program$state$traces[[2L]]$inputs,
    list(draft = "a checked draft")
  )
  expect_identical(
    program$state$traces[[2L]]$output$answer,
    "the final answer"
  )
  expect_equal(sum(program$get_traces()$total_tokens), 6L)
})

test_that("trace-aware metrics receive ordered Flex predictor calls once", {
  chat <- flex_test_chat()
  program <- flex_test_program(
    "question -> answer",
    module_src = flex_two_step_source()
  )
  seen <- NULL
  metric <- metric_with_trace(
    function(prediction, expected, program_trace) {
      seen <<- program_trace
      1
    }
  )

  evaluated <- evaluate(
    program,
    data = data.frame(question = "the question", answer = "the final answer"),
    metric = metric,
    .llm = chat,
    .progress = FALSE
  )

  expect_s3_class(seen, "dsprrr_program_trace")
  expect_identical(seen$status, "ok")
  expect_length(seen$events, 2L)
  expect_identical(
    unname(vapply(seen$events, `[[`, character(1), "step_name")),
    c("draft", "finish")
  )
  expect_identical(evaluated$metadata[[1L]]$predictor_calls, 2L)
  expect_equal(
    sum(vapply(
      seen$events,
      function(event) event$tokens$total_tokens,
      integer(1)
    )),
    evaluated$metadata[[1L]]$total_tokens
  )
})

test_that("flex resolves runtime Chat parameters exactly once", {
  clones <- new.env(parent = emptyenv())
  clone_count <- 0L
  call_ids <- integer()

  make_chat <- function(id) {
    chat <- new_test_chat(
      model = paste0("clone-", id),
      chat_structured = function(prompt, type, ...) {
        call_ids <<- c(call_ids, id)
        fields <- names(type@properties)
        if (identical(fields, "draft")) {
          return(list(draft = "a checked draft"))
        }
        if (identical(fields, c("reasoning", "answer"))) {
          return(list(reasoning = "checked", answer = "final"))
        }
        stop("unexpected Flex output schema")
      }
    )
    override_test_chat_method(chat, "clone", function(...) {
      clone_count <<- clone_count + 1L
      make_chat(clone_count)
    })
    assign(as.character(id), chat, envir = clones)
    chat
  }

  program <- flex_test_program(
    "question -> answer",
    module_src = flex_two_step_source(),
    config = list(temperature = 0.2, custom = "preserved")
  )
  result <- program$forward(
    list(question = "the question"),
    .llm = make_chat(0L)
  )

  expect_identical(clone_count, 1L)
  expect_identical(call_ids, c(1L, 1L))
  expect_identical(result$chat[[1L]], get("1", envir = clones))
  expect_identical(result$metadata[[1L]]$model, "clone-1")
  expect_identical(program$config$custom, "preserved")
})

test_that("flex reset and copy retain the plan without sharing bindings", {
  program <- flex_test_program(
    "question -> answer",
    module_src = flex_two_step_source(),
    config = list(temperature = 0.2)
  )
  program$state$traces <- list(list(test = TRUE))
  original_source <- program$module_src

  reset <- program$reset_copy()
  copied <- program$copy()
  deep <- program$deepcopy()
  expect_false(identical(reset, program))
  expect_false(identical(copied, program))
  expect_false(identical(deep, program))
  expect_identical(reset$module_src, original_source)
  expect_identical(copied$module_src, original_source)
  expect_identical(deep$module_src, original_source)
  expect_s3_class(deep, "FlexModule")
  expect_equal(reset$config$temperature, 0.2)
  expect_length(reset$state$traces, 0L)
  expect_length(copied$state$traces, 0L)

  copied$bind(NULL)
  expect_false(identical(copied$module_src, original_source))
  expect_identical(program$module_src, original_source)
})

test_that("flex uses the specialized scalar dataset adapter", {
  chat <- flex_test_chat()
  program <- flex_test_program(
    "question -> answer",
    module_src = flex_two_step_source()
  )
  dataset <- data.frame(
    question = c("one", "two"),
    answer = c("the final answer", "the final answer")
  )

  evaluated <- run_dataset(
    program,
    dataset,
    .llm = chat,
    .return_format = "structured",
    .progress = FALSE
  )

  expect_equal(nrow(evaluated), 2L)
  expect_length(evaluated$result, 2L)
  expect_true(all(vapply(
    evaluated$result,
    identical,
    logical(1),
    list(answer = "the final answer")
  )))
  expect_true(all(vapply(
    evaluated$.metadata,
    function(metadata) identical(metadata$predictor_calls, 2L),
    logical(1)
  )))
  expect_length(program$state$traces, 4L)
  expect_identical(
    unname(vapply(
      program$state$traces,
      function(event) event$metadata$batch_index,
      integer(1)
    )),
    c(1L, 1L, 2L, 2L)
  )
})

test_that("one-predictor Flex datasets use isolated native concurrency", {
  calls <- 0L
  observed_prompts <- list()
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(
      prompts,
      max_active,
      ...
    ) {
      calls <<- calls + 1L
      observed_prompts[[calls]] <<- prompts
      expect_identical(max_active, 3L)
      tibble::tibble(
        answer = paste0("answer-", seq_along(prompts)),
        input_tokens = rep(2L, length(prompts)),
        output_tokens = rep(1L, length(prompts)),
        cached_input_tokens = rep(0L, length(prompts)),
        cost = rep(0.001, length(prompts)),
        .error = rep(list(NULL), length(prompts))
      )
    },
    .package = "ellmer"
  )
  program <- flex_test_program("question -> answer")
  dataset <- data.frame(question = c("one", "two", "three"))

  result <- run_dataset(
    program,
    dataset,
    .llm = new_test_chat(),
    .concurrency = concurrency_control(
      backend = "ellmer",
      max_active = 3L
    ),
    .return_format = "structured",
    .progress = FALSE,
    .cache = FALSE
  )
  simple_program <- flex_test_program("question -> answer")
  simple <- run_dataset(
    simple_program,
    dataset,
    .llm = new_test_chat(),
    .concurrency = concurrency_control(
      backend = "ellmer",
      max_active = 3L
    ),
    .progress = FALSE,
    .cache = FALSE
  )

  expect_identical(calls, 2L)
  expect_length(observed_prompts[[1L]], 3L)
  expect_identical(
    simple$result,
    lapply(paste0("answer-", 1:3), \(answer) list(answer = answer))
  )
  expect_identical(
    vapply(result$result, `[[`, character(1), "answer"),
    paste0("answer-", 1:3)
  )
  expect_true(all(vapply(
    result$.metadata,
    function(metadata) identical(metadata$effective_backend, "ellmer"),
    logical(1)
  )))
  expect_true(all(vapply(
    result$.metadata,
    function(metadata) identical(metadata$predictor_calls, 1L),
    logical(1)
  )))
  expect_length(program$state$traces, 3L)
  expect_identical(
    vapply(
      program$state$traces,
      function(event) event$metadata$batch_index,
      integer(1)
    ),
    1:3
  )
})

test_that("concurrent Flex preserves provider error conditions", {
  provider_cause <- rlang::error_cnd(
    class = "flex_test_provider_offline",
    message = "provider offline"
  )
  provider_error <- rlang::error_cnd(
    class = "dsprrr_provider_error",
    message = "LLM call failed: provider offline",
    parent = provider_cause
  )
  predictor_results <- tibble::tibble(
    question = "q",
    result = list(NA),
    .error = "LLM call failed: provider offline",
    .metadata = list(list(
      error = "LLM call failed: provider offline",
      error_class = "dsprrr_provider_error"
    )),
    .chat = list(NULL)
  )
  attr(predictor_results, "dsprrr_error_conditions") <- list(provider_error)
  testthat::local_mocked_bindings(
    run_dataset = function(...) predictor_results,
    .package = "dsprrr"
  )
  program <- flex_test_program("question -> answer")

  result <- dsprrr:::run_flex_dataset_batch(
    program = program,
    data = data.frame(question = "q"),
    .llm = new_test_chat(),
    .verbose = FALSE,
    .progress = FALSE,
    .return_format = "structured",
    .concurrency = concurrency_control(
      backend = "ellmer",
      max_active = 2L
    ),
    .concurrency_runtime = list(effective_backend = "ellmer"),
    dots = list()
  )

  conditions <- attr(result, "dsprrr_error_conditions", exact = TRUE)
  expect_length(conditions, 1L)
  expect_identical(conditions[[1L]], provider_error)
  expect_s3_class(
    dsprrr:::run_provider_error_condition(conditions[[1L]]),
    "flex_test_provider_offline"
  )
})

test_that("multi-predictor Flex concurrency remains fail-closed", {
  calls <- 0L
  testthat::local_mocked_bindings(
    parallel_chat_structured = function(...) {
      calls <<- calls + 1L
      stop("provider should not run")
    },
    .package = "ellmer"
  )
  program <- flex_test_program(
    "question -> answer",
    module_src = flex_two_step_source()
  )

  error <- tryCatch(
    run_dataset(
      program,
      data.frame(question = c("one", "two")),
      .llm = new_test_chat(),
      .concurrency = concurrency_control(
        backend = "ellmer",
        max_active = 2L
      ),
      .progress = FALSE
    ),
    error = identity
  )

  expect_s3_class(error, "dsprrr_flex_concurrency_unsupported_error")
  expect_identical(error$predictor_calls, 2L)
  expect_identical(calls, 0L)
})

test_that("run preserves Flex array and object inputs as scalar values", {
  calls <- 0L
  chat <- new_test_chat(
    model = "flex-schema-input-test",
    chat_structured = function(...) {
      calls <<- calls + 1L
      list(answer = "accepted")
    }
  )

  array_program <- flex_test_program(Signature(
    inputs = list(input(
      "items",
      type = ellmer::type_array(ellmer::type_integer())
    )),
    output_type = ellmer::type_object(answer = ellmer::type_string())
  ))

  expect_identical(
    run(array_program, items = c(1L, 2L), .llm = chat),
    list(answer = "accepted")
  )
  expect_identical(
    array_program$run(items = integer(), .llm = chat),
    list(answer = "accepted")
  )

  object_program <- flex_test_program(Signature(
    inputs = list(input(
      "payload",
      type = ellmer::type_object(
        label = ellmer::type_string(),
        counts = ellmer::type_array(ellmer::type_integer())
      )
    )),
    output_type = ellmer::type_object(answer = ellmer::type_string())
  ))

  expect_identical(
    run(
      object_program,
      payload = list(label = "ok", counts = c(1L, 2L)),
      .llm = chat
    ),
    list(answer = "accepted")
  )
  expect_identical(calls, 3L)

  scalar_program <- flex_test_program("question: string -> answer")
  expect_error(
    run(scalar_program, question = c("one", "two"), .llm = chat),
    class = "dsprrr_flex_runtime_input_error"
  )
  expect_identical(calls, 3L)
})
