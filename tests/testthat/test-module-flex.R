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
  turns <- list()
  prompts <- character()
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
      prompts = function() prompts,
      get_turns = function(...) turns,
      set_turns = function(value) {
        turns <<- value
        invisible(NULL)
      },
      last_turn = last_turn,
      get_model = function() "flex-test-model",
      chat_structured = function(prompt, type, ...) {
        calls <<- calls + 1L
        prompt <- as.character(prompt)
        prompts <<- c(prompts, prompt)
        fields <- names(type@properties)
        response <- if (identical(fields, "draft")) {
          list(draft = "a checked draft")
        } else if (identical(fields, c("reasoning", "answer"))) {
          list(reasoning = "checked", answer = "the final answer")
        } else {
          stop("unexpected Flex output schema")
        }
        turns <<- c(
          turns,
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
    ),
    class = "Chat"
  )
}

test_that("flex gives one experimental lifecycle warning per session", {
  lifecycle <- get(".flex_lifecycle", asNamespace("dsprrr"))
  old <- lifecycle$warned
  withr::defer(lifecycle$warned <- old)
  lifecycle$warned <- FALSE

  expect_warning(
    first <- flex("question -> answer"),
    class = "dsprrr_flex_experimental_warning"
  )
  expect_no_warning(second <- flex("question -> answer"))
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
  chat <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        calls <<- calls + 1L
        list(answer = "ready")
      },
      get_model = function() "zero-input-test"
    ),
    class = "Chat"
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

  valid_chat <- structure(
    list(
      chat_structured = function(...) list(answer = "accepted"),
      get_model = function() "nested-input-test"
    ),
    class = "Chat"
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
    structure(
      list(
        chat_structured = function(...) response,
        get_model = function() "malformed-output-test"
      ),
      class = "Chat"
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
  chat <- structure(
    list(
      chat_structured = function(...) {
        list(
          answer = "ok",
          ignored = NULL,
          nested = list(visible = "kept", ignored = NULL)
        )
      },
      get_model = function() "ignore-output-test"
    ),
    class = "Chat"
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
    .llm = structure(
      list(
        chat_structured = function(...) list(ignored = NULL),
        get_model = function() "all-ignored-output-test"
      ),
      class = "Chat"
    )
  )
  expect_identical(ignored_result$output[[1L]], setNames(list(), character()))
})

test_that("flex executes two fresh predictors with one aggregate trace", {
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

  expect_length(program$state$traces, 1L)
  trace <- program$state$traces[[1L]]
  expect_identical(trace$predictor_calls, 2L)
  expect_named(trace$steps, c("draft", "finish"))
  expect_identical(program$get_traces()$total_tokens, 6L)
})

test_that("flex resolves runtime Chat parameters exactly once", {
  clones <- new.env(parent = emptyenv())
  clone_count <- 0L
  call_ids <- integer()

  make_chat <- function(id) {
    chat <- NULL
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
            return(list(draft = "a checked draft"))
          }
          if (identical(fields, c("reasoning", "answer"))) {
            return(list(reasoning = "checked", answer = "final"))
          }
          stop("unexpected Flex output schema")
        },
        get_model = function() paste0("clone-", id)
      ),
      class = "Chat"
    )
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
  expect_length(program$state$traces, 2L)
})

test_that("run preserves Flex array and object inputs as scalar values", {
  calls <- 0L
  chat <- structure(
    list(
      chat_structured = function(...) {
        calls <<- calls + 1L
        list(answer = "accepted")
      },
      get_model = function() "flex-schema-input-test"
    ),
    class = "Chat"
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
