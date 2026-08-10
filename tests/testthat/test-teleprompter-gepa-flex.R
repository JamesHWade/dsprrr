gepa_flex_test_source <- function(input = "question", output = "answer") {
  first_output <- paste0(output, "_draft")
  source <- list(
    schema_version = 1L,
    steps = list(
      list(
        name = "draft",
        primitive = "predict",
        signature = paste0(input, " -> ", first_output),
        inputs = setNames(
          list(paste0("$input.", input)),
          input
        )
      ),
      list(
        name = "finish",
        primitive = "chain_of_thought",
        signature = paste0(first_output, " -> ", output),
        instructions = "Check the draft before answering.",
        inputs = setNames(
          list(paste0("$step.draft.", first_output)),
          first_output
        )
      )
    ),
    outputs = setNames(
      list(paste0("$step.finish.", output)),
      output
    )
  )
  as.character(jsonlite::toJSON(
    source,
    auto_unbox = TRUE,
    null = "null",
    digits = NA,
    pretty = FALSE
  ))
}

gepa_flex_test_chat <- function(response = NULL, error = NULL) {
  prompts <- character()
  output_fields <- list()
  structure(
    list(
      prompts = function() prompts,
      output_fields = function() output_fields,
      chat_structured = function(prompt, type, ...) {
        prompts <<- c(prompts, as.character(prompt))
        output_fields[[length(output_fields) + 1L]] <<-
          names(type@properties)
        if (!is.null(error)) {
          stop(error)
        }
        response
      }
    ),
    class = "Chat"
  )
}

gepa_flex_test_program <- function() {
  ordinary <- module(
    signature("question -> draft", instructions = "Ordinary instructions."),
    type = "predict"
  )
  flexible <- suppressWarnings(flex(
    signature("draft -> answer", instructions = "Flex instructions.")
  ))
  pipeline(ordinary = ordinary, flexible = flexible)
}

GepaRefreshTestModule <- R6::R6Class(
  "GepaRefreshTestModule",
  inherit = dsprrr:::Module,
  public = list(
    children = NULL,
    child_address = NULL,
    refreshes = 0L,
    initialize = function(child) {
      super$initialize(child$signature)
      self$children <- list(leaf = child)
      self$child_address <- rlang::obj_address(child)
    },
    graph_children = function() self$children,
    set_graph_children = function(children) {
      self$children <- children
      invisible(self)
    },
    graph_children_changed = function() {
      self$child_address <- rlang::obj_address(self$children$leaf)
      self$signature <- self$children$leaf$signature
      self$refreshes <- self$refreshes + 1L
      invisible(self)
    },
    forward = function(...) cli::cli_abort("refresh-test only")
  )
)

test_that("GEPA discovers direct and nested Flex leaves by stable graph path", {
  direct <- suppressWarnings(flex("question -> answer"))
  expect_identical(dsprrr:::gepa_flex_paths(direct), "$")

  program <- gepa_flex_test_program()
  expect_identical(
    dsprrr:::gepa_flex_paths(program),
    "$/steps/flexible"
  )

  candidate <- dsprrr:::gepa_component_candidate(program)
  expect_s3_class(candidate, "dsprrr_gepa_component_candidate")
  expect_identical(
    names(candidate$components),
    c(
      "instructions::$/steps/ordinary",
      "module_src::$/steps/flexible"
    )
  )
  expect_identical(
    unname(vapply(candidate$components, `[[`, character(1), "kind")),
    c("instructions", "module_src")
  )
})

test_that("GEPA omits frozen Flex parameters without losing discovery", {
  program <- suppressWarnings(flex("question -> answer"))
  freeze_modules(program, "$")

  expect_length(dsprrr:::gepa_flex_paths(program), 0L)
  expect_identical(
    dsprrr:::gepa_flex_paths(program, mutable_only = FALSE),
    "$"
  )
  candidate <- dsprrr:::gepa_component_candidate(program)
  expect_length(candidate$components, 0L)
  expect_no_error(dsprrr:::gepa_validate_component_candidate(
    candidate,
    program
  ))
  expect_true(
    dsprrr:::gepa_materialize_component_candidate(
      program,
      candidate
    )$ok
  )
})

test_that("GEPA skips all work when a Flex graph has no mutable components", {
  program <- suppressWarnings(flex("question -> answer"))
  freeze_modules(program, "$")
  provider_calls <- 0L
  evaluation_calls <- 0L
  chat <- structure(
    list(chat_structured = function(...) {
      provider_calls <<- provider_calls + 1L
      stop("provider must not be called")
    }),
    class = "Chat"
  )
  testthat::local_mocked_bindings(
    eval_program = function(...) {
      evaluation_calls <<- evaluation_calls + 1L
      stop("evaluation must not be called")
    },
    .package = "dsprrr"
  )

  optimized <- compile(
    GEPA(
      metric = function(...) 1,
      population_size = 4L,
      generations = 3L,
      verbose = FALSE
    ),
    program,
    data.frame(question = "q", answer = "a"),
    .llm = chat
  )
  metadata <- optimized$config$optimizer

  expect_false(identical(optimized, program))
  expect_identical(optimized$module_src, program$module_src)
  expect_identical(provider_calls, 0L)
  expect_identical(evaluation_calls, 0L)
  expect_true(metadata$optimization_skipped)
  expect_identical(metadata$skip_reason, "no_mutable_components")
  expect_identical(metadata$flex_paths, "$")
  expect_length(metadata$mutable_flex_paths, 0L)
  expect_length(metadata$component_ids, 0L)
  expect_null(metadata$best_candidate)
  expect_length(metadata$all_generations, 0L)
  expect_identical(metadata$budget_summary$attempts, 0L)
  expect_identical(metadata$budget_summary$provider_calls, 0L)
  expect_identical(metadata$budget_summary$trials, 0L)
  expect_false(metadata$partial)
})

test_that("GEPA materialization supports composite roots and shared aliases", {
  source <- gepa_flex_test_source()
  constructors <- list(
    ensemble = function(module) ensemble(list(module)),
    best_of_n = function(module) best_of_n(module, N = 2L),
    refine = function(module) refine(module, N = 2L)
  )

  for (constructor in constructors) {
    leaf <- suppressWarnings(flex("question -> answer"))
    baseline <- leaf$module_src
    program <- constructor(leaf)
    candidate <- dsprrr:::gepa_component_candidate(program)
    source_id <- names(candidate$components)[vapply(
      candidate$components,
      function(component) identical(component$kind, "module_src"),
      logical(1)
    )]
    candidate$components[[source_id]]$value <- source

    materialized <- dsprrr:::gepa_materialize_component_candidate(
      program,
      candidate
    )

    expect_true(materialized$ok)
    copied_leaf <- named_modules(materialized$program)[[
      candidate$components[[source_id]]$path
    ]]
    expect_false(identical(copied_leaf, leaf))
    expect_false(identical(copied_leaf$module_src, baseline))
    expect_identical(leaf$module_src, baseline)
  }

  shared <- suppressWarnings(flex("question -> answer"))
  shared_program <- pipeline(first = shared, second = shared)
  shared_candidate <- dsprrr:::gepa_component_candidate(shared_program)
  shared_candidate$components[["module_src::$/steps/first"]]$value <- source
  materialized <- dsprrr:::gepa_materialize_component_candidate(
    shared_program,
    shared_candidate
  )
  copied <- named_modules(materialized$program, aliases = TRUE)

  expect_true(materialized$ok)
  expect_identical(copied[["$/steps/first"]], copied[["$/steps/second"]])
  expect_false(identical(copied[["$/steps/first"]], shared))
  expect_false(identical(
    copied[["$/steps/first"]]$module_src,
    shared$module_src
  ))
})

test_that("GEPA component cloning refreshes custom graph-derived state", {
  leaf <- suppressWarnings(flex("question -> answer"))
  program <- GepaRefreshTestModule$new(leaf)

  cloned <- dsprrr:::gepa_clone_component_program(program)

  expect_false(identical(cloned, program))
  expect_false(identical(cloned$children$leaf, leaf))
  expect_identical(
    cloned$child_address,
    rlang::obj_address(cloned$children$leaf)
  )
  expect_identical(cloned$refreshes, 1L)
  expect_identical(program$child_address, rlang::obj_address(leaf))
  expect_identical(program$refreshes, 0L)
})

test_that("GEPA materializes complete mixed candidates on a deep copy", {
  program <- gepa_flex_test_program()
  candidate <- dsprrr:::gepa_component_candidate(program)
  candidate$components[["instructions::$/steps/ordinary"]]$value <-
    "Improved ordinary instructions."
  candidate$components[["module_src::$/steps/flexible"]]$value <-
    gepa_flex_test_source("draft", "answer")

  materialized <- dsprrr:::gepa_materialize_component_candidate(
    program,
    candidate,
    failure_score = -1
  )

  expect_true(materialized$ok)
  expect_true(materialized$selectable)
  expect_false(identical(materialized$program, program))
  copied <- named_modules(materialized$program)
  original <- named_modules(program)
  expect_identical(
    copied[["$/steps/ordinary"]]$signature@instructions,
    "Improved ordinary instructions."
  )
  expect_false(identical(
    copied[["$/steps/flexible"]]$module_src,
    original[["$/steps/flexible"]]$module_src
  ))
  expect_identical(
    original[["$/steps/ordinary"]]$signature@instructions,
    "Ordinary instructions."
  )
})

test_that("GEPA source binding failure is transactional and non-selectable", {
  program <- suppressWarnings(flex("question -> answer"))
  baseline <- program$module_src
  candidate <- dsprrr:::gepa_component_candidate(program)
  candidate$components[["module_src::$"]]$value <- "{}"

  materialized <- dsprrr:::gepa_materialize_component_candidate(
    program,
    candidate,
    failure_score = -3
  )

  expect_false(materialized$ok)
  expect_false(materialized$selectable)
  expect_identical(materialized$failure_score, -3)
  expect_identical(materialized$failure_kind, "candidate")
  expect_s3_class(
    materialized$condition,
    "dsprrr_gepa_invalid_candidate"
  )
  expect_null(materialized$program)
  expect_identical(program$module_src, baseline)
})

test_that("Flex proposer is structured, schema-grounded, and row-aligned", {
  task_signature <- signature(
    inputs = list(input(
      "question",
      description = "Arithmetic question supplied by the user."
    )),
    output_type = ellmer::type_object(
      answer = ellmer::type_string(
        description = "The exact arithmetic answer."
      )
    ),
    instructions = "Solve the arithmetic task exactly."
  )
  program <- suppressWarnings(flex(task_signature))
  proposed <- gepa_flex_test_source()
  chat <- gepa_flex_test_chat(list(module_src = proposed))
  failures <- list(list(
    row_id = 17L,
    inputs = data.frame(question = "What is 2 + 2?"),
    expected = "4",
    predicted = "5",
    feedback = "Arithmetic was incorrect."
  ))

  result <- dsprrr:::gepa_propose_flex_source(
    program,
    failures,
    .llm = chat,
    unit_id = "proposal:17"
  )

  expect_identical(result$status, "proposed")
  expect_true(result$valid)
  expect_true(jsonlite::validate(result$module_src))
  expect_identical(result$unit_id, "proposal:17")
  expect_identical(chat$output_fields()[[1L]], "module_src")
  prompt <- chat$prompts()[[1L]]
  expect_match(prompt, "complete declarative Flex module source", fixed = TRUE)
  expect_match(prompt, "schema_version", fixed = TRUE)
  expect_match(prompt, program$module_src, fixed = TRUE)
  expect_match(prompt, "Solve the arithmetic task exactly.", fixed = TRUE)
  expect_match(
    prompt,
    "Arithmetic question supplied by the user.",
    fixed = TRUE
  )
  expect_match(prompt, "The exact arithmetic answer.", fixed = TRUE)
  expect_match(prompt, '"row_id":17', fixed = TRUE)
  expect_match(prompt, "What is 2 + 2?", fixed = TRUE)
  expect_match(prompt, '"expected_output":"4"', fixed = TRUE)
  expect_match(prompt, '"predicted_output":"5"', fixed = TRUE)
  expect_match(prompt, "Arithmetic was incorrect.", fixed = TRUE)
  expect_false(grepl("```", prompt, fixed = TRUE))
})

test_that("Flex proposer describes the executable R DSL and host tools", {
  source <- paste(
    "forward <- function(question) {",
    "  Prediction(answer = lookup(query = question))",
    "}",
    sep = "\n"
  )
  program <- suppressWarnings(flex(
    "question -> answer",
    module_src = source,
    tools = list(lookup = function(query) query),
    interpreter_factory = r_code_runner,
    source_format = "r",
    require_sandbox = FALSE,
    max_tool_calls = 3L
  ))
  proposed <- sub("query = question", "query = toupper(question)", source)
  chat <- gepa_flex_test_chat(list(module_src = proposed))

  result <- dsprrr:::gepa_propose_flex_source(
    program,
    list(list(
      row_id = 1L,
      inputs = list(question = "q"),
      expected = "A",
      predicted = "B",
      feedback = "Normalize before lookup."
    )),
    .llm = chat
  )

  expect_identical(result$status, "proposed")
  expect_identical(result$module_src, proposed)
  prompt <- chat$prompts()[[1L]]
  expect_match(prompt, "complete executable R Flex", fixed = TRUE)
  expect_match(prompt, '"source_format":"r"', fixed = TRUE)
  expect_match(prompt, '"name":"lookup"', fixed = TRUE)
  expect_match(prompt, '"arguments":"query"', fixed = TRUE)
  expect_match(prompt, '"required":"query"', fixed = TRUE)
  expect_match(prompt, "ProgramOfThought", fixed = TRUE)
  expect_match(prompt, "versioned bridge", fixed = TRUE)
  expect_match(prompt, "max_predictor_calls", fixed = TRUE)
  expect_match(prompt, '"max_tool_calls":3', fixed = TRUE)
  expect_match(
    prompt,
    "host-tool calls count against max_tool_calls",
    fixed = TRUE
  )
})

test_that("GEPA Flex tool contracts distinguish ToolDefs from functions", {
  ordinary <- dsprrr:::gepa_flex_tool_contract(
    function(query, limit = 5L) query,
    "ordinary"
  )
  tool <- ellmer::tool(
    function(query, limit = 5L) query,
    name = "lookup",
    description = "Look up records.",
    arguments = list(
      query = ellmer::type_string(description = "Search terms."),
      limit = ellmer::type_integer(
        description = "Maximum records.",
        required = FALSE
      )
    )
  )
  contract <- dsprrr:::gepa_flex_tool_contract(tool, "lookup")

  expect_identical(ordinary$kind, "function")
  expect_identical(ordinary$arguments, c("query", "limit"))
  expect_identical(ordinary$required, "query")
  expect_identical(ordinary$defaults, list(limit = "5L"))
  expect_identical(contract$name, "lookup")
  expect_identical(contract$kind, "tool_def")
  expect_identical(contract$description, "Look up records.")
  expect_identical(contract$arguments_schema$type, "object")
  expect_identical(
    contract$arguments_schema$properties$query,
    list(type = "string", description = "Search terms.")
  )
  expect_identical(
    contract$arguments_schema$properties$limit,
    list(type = "integer", description = "Maximum records.")
  )
  expect_identical(contract$arguments_schema$required, "query")
  expect_identical(contract$arguments_schema$additionalProperties, FALSE)
})

test_that("Flex proposer includes ToolDef metadata in its prompt", {
  source <- paste(
    "forward <- function(question) {",
    "  Prediction(answer = lookup(query = question))",
    "}",
    sep = "\n"
  )
  tool <- ellmer::tool(
    function(query) query,
    name = "lookup",
    description = "Look up records by query.",
    arguments = list(
      query = ellmer::type_string(description = "Search terms to look up.")
    )
  )
  program <- suppressWarnings(flex(
    "question -> answer",
    module_src = source,
    tools = list(lookup = tool),
    interpreter_factory = r_code_runner,
    source_format = "r",
    require_sandbox = FALSE
  ))
  chat <- gepa_flex_test_chat(list(module_src = source))

  result <- dsprrr:::gepa_propose_flex_source(
    program,
    list(),
    .llm = chat
  )

  expect_identical(result$status, "proposed")
  prompt <- chat$prompts()[[1L]]
  expect_match(prompt, '"kind":"tool_def"', fixed = TRUE)
  expect_match(
    prompt,
    '"description":"Look up records by query."',
    fixed = TRUE
  )
  expect_match(prompt, '"arguments_schema":{"type":"object"', fixed = TRUE)
  expect_match(
    prompt,
    '"description":"Search terms to look up."',
    fixed = TRUE
  )
  expect_match(prompt, '"required":"query"', fixed = TRUE)
  expect_match(prompt, '"additionalProperties":false', fixed = TRUE)
})

test_that("nested Flex proposals label component and root-program evidence", {
  program <- gepa_flex_test_program()
  target <- named_modules(program)[["$/steps/flexible"]]
  trace <- list(
    events = list(list(
      step_inputs = list(
        list(question = "root question"),
        list(draft = "observed draft")
      ),
      step_outputs = list(
        list(draft = "observed draft"),
        list(answer = "observed answer")
      )
    ))
  )
  failure <- list(list(
    row_id = 8L,
    inputs = data.frame(question = "root question"),
    expected = "expected answer",
    predicted = "observed answer",
    feedback = "improve the final step",
    program_trace = trace
  ))

  prompt <- dsprrr:::gepa_flex_reflection_prompt(
    target,
    failure,
    program = program,
    component_path = "$/steps/flexible"
  )

  expect_match(prompt, '"evidence_scope":"pipeline_step"', fixed = TRUE)
  expect_match(
    prompt,
    '"component_inputs":{"draft":"observed draft"}',
    fixed = TRUE
  )
  expect_match(
    prompt,
    '"component_observed_output":{"answer":"observed answer"}',
    fixed = TRUE
  )
  expect_match(
    prompt,
    '"root_inputs":{"question":"root question"}',
    fixed = TRUE
  )
  expect_match(
    prompt,
    '"expected_program_output":"expected answer"',
    fixed = TRUE
  )
})

test_that("Flex proposer rejects incomplete or invalid source responses", {
  program <- suppressWarnings(flex("question -> answer"))

  malformed <- gepa_flex_test_chat(list(not_module_src = "{}"))
  malformed_result <- dsprrr:::gepa_propose_flex_source(
    program,
    list(),
    .llm = malformed
  )
  expect_identical(malformed_result$status, "invalid_proposal")
  expect_false(malformed_result$valid)
  expect_s3_class(
    malformed_result$condition,
    "dsprrr_gepa_invalid_candidate"
  )

  invalid <- gepa_flex_test_chat(list(module_src = "{not json"))
  invalid_result <- dsprrr:::gepa_propose_flex_source(
    program,
    list(),
    .llm = invalid
  )
  expect_identical(invalid_result$status, "invalid_proposal")
  expect_false(invalid_result$valid)
  expect_identical(program$module_src, invalid_result$module_src)
})

test_that("Flex proposer propagates provider failure and keeps explicit fallback", {
  program <- suppressWarnings(flex("question -> answer"))
  provider_error <- simpleError("provider unavailable")
  class(provider_error) <- c("gepa_test_provider_error", class(provider_error))
  chat <- gepa_flex_test_chat(error = provider_error)

  expect_error(
    dsprrr:::gepa_propose_flex_source(program, list(), .llm = chat),
    class = "gepa_test_provider_error"
  )

  no_provider <- dsprrr:::gepa_propose_flex_source(
    program,
    list(),
    .llm = NULL
  )
  expect_identical(no_provider$status, "unchanged_no_proposer")
  expect_true(no_provider$valid)
  expect_identical(no_provider$module_src, program$module_src)
})

test_that("Flex proposer uses the shared provider budget ledger", {
  program <- suppressWarnings(flex("question -> answer"))
  chat <- gepa_flex_test_chat(list(module_src = gepa_flex_test_source()))
  budget <- dsprrr:::new_optimizer_budget(dsprrr:::optimizer_control())

  result <- dsprrr:::gepa_propose_flex_source(
    program,
    list(),
    .llm = chat,
    budget = budget,
    unit_id = "gepa:flex:budgeted"
  )
  summary <- dsprrr:::optimizer_budget_summary(budget)

  expect_identical(result$status, "proposed")
  expect_identical(summary$provider_calls, 1L)
  expect_identical(summary$attempts, 1L)
  expect_identical(summary$successes, 1L)
  expect_true("gepa:flex:budgeted" %in% summary$completed_units)
})

test_that("budgeted Flex proposer records and propagates provider failure", {
  program <- suppressWarnings(flex("question -> answer"))
  provider_error <- simpleError("provider unavailable")
  class(provider_error) <- c("gepa_test_provider_error", class(provider_error))
  chat <- gepa_flex_test_chat(error = provider_error)
  budget <- dsprrr:::new_optimizer_budget(
    dsprrr:::optimizer_control(max_errors = 2L)
  )

  expect_error(
    dsprrr:::gepa_propose_flex_source(
      program,
      list(),
      .llm = chat,
      budget = budget,
      unit_id = "gepa:flex:provider-failure"
    ),
    class = "gepa_test_provider_error"
  )
  summary <- dsprrr:::optimizer_budget_summary(budget)

  expect_identical(summary$provider_calls, 1L)
  expect_identical(summary$total_errors, 1L)
  expect_true("gepa:flex:provider-failure" %in% summary$completed_units)
})

test_that("component instruction proposer propagates provider failure", {
  program <- gepa_flex_test_program()
  candidate <- dsprrr:::gepa_component_candidate(program)
  provider_error <- simpleError("instruction provider unavailable")
  class(provider_error) <- c("gepa_test_provider_error", class(provider_error))
  chat <- gepa_flex_test_chat(error = provider_error)

  expect_error(
    dsprrr:::gepa_mutate_component_candidate(
      candidate,
      program,
      failed_examples = list(),
      .llm = chat,
      component_id = "instructions::$/steps/ordinary"
    ),
    class = "gepa_test_provider_error"
  )

  malformed <- dsprrr:::gepa_mutate_component_candidate(
    candidate,
    program,
    failed_examples = list(),
    .llm = gepa_flex_test_chat(response = "not structured"),
    component_id = "instructions::$/steps/ordinary"
  )
  expect_false(malformed$valid)
  expect_identical(
    malformed$failure$error_class,
    "dsprrr_gepa_invalid_candidate"
  )
})

test_that("Flex evaluation preflights and records exact predictor calls", {
  program <- suppressWarnings(flex(
    "question -> answer",
    module_src = gepa_flex_test_source()
  ))
  chat <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        fields <- names(type@properties)
        if (identical(fields, "answer_draft")) {
          return(list(answer_draft = "draft"))
        }
        if (identical(fields, c("reasoning", "answer"))) {
          return(list(reasoning = "checked", answer = "final"))
        }
        stop("unexpected Flex runtime schema")
      },
      get_model = function() "flex-budget-test"
    ),
    class = "Chat"
  )
  blocked_chat <- structure(
    list(chat_structured = function(...) {
      stop("provider must not be reached")
    }),
    class = "Chat"
  )
  data <- data.frame(
    question = c("q1", "q2"),
    answer = c("final", "final")
  )

  too_small <- dsprrr:::new_optimizer_budget(
    dsprrr:::optimizer_control(max_provider_calls = 1L)
  )
  blocked <- dsprrr:::optimizer_eval_program(
    program,
    data,
    metric = function(...) 1,
    .llm = blocked_chat,
    budget = too_small,
    stage = "flex_budget",
    unit_id = "flex:blocked",
    .cache = FALSE
  )
  expect_identical(blocked@n_evaluated, 0L)
  expect_identical(
    dsprrr:::optimizer_budget_summary(too_small)$stop_reason$code,
    "max_provider_calls"
  )

  exact <- dsprrr:::new_optimizer_budget(
    dsprrr:::optimizer_control(max_provider_calls = 2L)
  )
  evaluated <- dsprrr:::optimizer_eval_program(
    program,
    data,
    metric = function(...) 1,
    .llm = chat,
    budget = exact,
    stage = "flex_budget",
    unit_id = "flex:exact",
    .cache = FALSE
  )
  summary <- dsprrr:::optimizer_budget_summary(exact)

  expect_identical(evaluated@n_evaluated, 1L)
  expect_false(evaluated@provider_usage_unknown)
  expect_identical(evaluated@provider_calls, 2L)
  expect_identical(
    evaluated@examples$predicted[[1L]],
    list(answer = "final")
  )
  expect_identical(summary$provider_calls, 2L)
  expect_identical(summary$stop_reason$code, "max_provider_calls")
})

test_that("invalid proposed sources become failed candidates", {
  program <- suppressWarnings(flex("question -> answer"))
  candidate <- dsprrr:::gepa_component_candidate(program)
  chat <- gepa_flex_test_chat(list(module_src = "{}"))

  mutated <- dsprrr:::gepa_mutate_component_candidate(
    candidate,
    program,
    failed_examples = list(),
    .llm = chat,
    component_id = "module_src::$",
    failure_score = -2
  )
  expect_false(mutated$valid)
  expect_identical(
    mutated$history[[1L]]$status,
    "invalid_proposal"
  )

  materialized <- dsprrr:::gepa_materialize_component_candidate(
    program,
    mutated,
    failure_score = -2
  )
  expect_false(materialized$ok)
  expect_false(materialized$selectable)
  expect_identical(materialized$failure_score, -2)
})

test_that("invalid candidates get aligned failure scores and ledger outcomes", {
  program <- suppressWarnings(flex("question -> answer"))
  candidate <- dsprrr:::gepa_component_candidate(program)
  candidate$components[["module_src::$"]]$value <- "{}"
  dataset <- data.frame(
    question = c("q1", "q2", "q3"),
    answer = c("a1", "a2", "a3")
  )
  metric_called <- FALSE
  metric <- function(...) {
    metric_called <<- TRUE
    1
  }
  control <- dsprrr:::optimizer_control(max_errors = 10L)
  budget <- dsprrr:::new_optimizer_budget(control)

  evaluated <- dsprrr:::gepa_evaluate_component_candidate(
    program,
    candidate,
    dataset,
    metric,
    control = control,
    budget = budget,
    stage = "gepa_flex_metric_1",
    unit_id = "candidate:invalid:metric:1",
    failure_score = -1
  )
  result <- evaluated$eval_result
  summary <- dsprrr:::optimizer_budget_summary(budget)

  expect_false(metric_called)
  expect_false(evaluated$selectable)
  expect_identical(evaluated$failure_kind, "candidate")
  expect_identical(result@examples$row_id, 1:3)
  expect_identical(result@examples$input_question, dataset$question)
  expect_identical(result@examples$score, rep(-1, 3L))
  expect_true(all(nzchar(result@examples$error)))
  expect_true(all(
    result@examples$error_class == "dsprrr_gepa_invalid_candidate"
  ))
  expect_identical(result@mean_score, -1)
  expect_identical(result@n_errors, 3L)
  expect_identical(result@metric_calls, 0L)
  expect_identical(result@provider_calls, 0L)
  expect_identical(summary$attempts, 1L)
  expect_identical(summary$total_errors, 1L)
  expect_identical(summary$trials, 1L)
  expect_true("candidate:invalid:metric:1" %in% summary$completed_units)

  record <- list(
    complete = TRUE,
    candidate_valid = FALSE,
    scores = c(quality = -1)
  )
  expect_false(dsprrr:::gepa_component_record_selectable(record))

  invalid_candidate <- dsprrr:::gepa_component_candidate(program)
  invalid_candidate$valid <- FALSE
  invalid_candidate$failure <- list(
    error_class = "invalid_test_candidate",
    error_message = "invalid"
  )
  expect_false(dsprrr:::gepa_component_record_selectable(list(
    complete = TRUE,
    candidate = invalid_candidate,
    scores = c(quality = 100)
  )))
})

test_that("one invalid proposal counts once while retaining row-aligned audit", {
  program <- suppressWarnings(flex("question -> answer"))
  candidate <- dsprrr:::gepa_component_candidate(program)
  candidate$components[["module_src::$"]]$value <- "{}"
  dataset <- data.frame(
    question = paste0("q", 1:6),
    answer = paste0("a", 1:6)
  )
  control <- dsprrr:::optimizer_control(max_errors = 5L)
  budget <- dsprrr:::new_optimizer_budget(control)

  evaluated <- dsprrr:::gepa_evaluate_component_candidate(
    program,
    candidate,
    dataset,
    metric = function(...) 1,
    control = control,
    budget = budget,
    stage = "gepa_flex_metric_1",
    unit_id = "candidate:one-invalid-proposal"
  )
  summary <- dsprrr:::optimizer_budget_summary(budget)

  expect_identical(nrow(evaluated$eval_result@examples), 6L)
  expect_identical(evaluated$eval_result@n_errors, 6L)
  expect_identical(summary$attempts, 1L)
  expect_identical(summary$total_errors, 1L)
  expect_false(summary$stopped)
})

test_that("valid candidate evaluation preserves row and error classification", {
  program <- suppressWarnings(flex("question -> answer"))
  candidate <- dsprrr:::gepa_component_candidate(program)
  dataset <- data.frame(
    question = c("q1", "q2"),
    answer = c("a1", "a2")
  )
  aligned <- dsprrr:::EvalResult(
    examples = tibble::tibble(
      row_id = 1:2,
      score = c(0.5, NA_real_),
      error = c(NA_character_, "provider row failed"),
      predicted = list("a1", NA),
      feedback = c("ok", NA_character_)
    ),
    mean_score = 0.25,
    n_evaluated = 1L,
    n_errors = 1L
  )
  testthat::local_mocked_bindings(
    optimizer_eval_candidate = function(...) aligned,
    .package = "dsprrr"
  )

  result <- dsprrr:::gepa_evaluate_component_candidate(
    program,
    candidate,
    dataset,
    metric = function(...) 1,
    stage = "gepa_flex_metric_1",
    unit_id = "candidate:valid:metric:1"
  )
  expect_true(result$selectable)
  expect_identical(result$eval_result@examples$row_id, 1:2)
  expect_identical(
    result$eval_result@examples$error[[2L]],
    "provider row failed"
  )

  infrastructure <- simpleError("provider infrastructure failed")
  class(infrastructure) <- c(
    "gepa_test_provider_infrastructure_error",
    class(infrastructure)
  )
  testthat::local_mocked_bindings(
    optimizer_eval_candidate = function(...) stop(infrastructure),
    .package = "dsprrr"
  )
  expect_error(
    dsprrr:::gepa_evaluate_component_candidate(
      program,
      candidate,
      dataset,
      metric = function(...) 1,
      stage = "gepa_flex_metric_1",
      unit_id = "candidate:provider-error:metric:1"
    ),
    class = "gepa_test_provider_infrastructure_error"
  )
})

test_that("component population mutates complete instruction and source values", {
  program <- gepa_flex_test_program()
  proposed <- gepa_flex_test_source("draft", "answer")
  chat <- local({
    structure(
      list(
        chat_structured = function(prompt, type, ...) {
          if (grepl("improving system instructions", prompt, fixed = TRUE)) {
            return(list(instructions = "Improved instructions."))
          }
          list(module_src = proposed)
        }
      ),
      class = "Chat"
    )
  })

  population <- dsprrr:::gepa_initial_component_population(
    program,
    population_size = 3L,
    .llm = chat
  )
  expect_length(population, 3L)
  expect_true(all(vapply(
    population,
    inherits,
    logical(1),
    "dsprrr_gepa_component_candidate"
  )))
  expect_identical(
    population[[2L]]$components[["instructions::$/steps/ordinary"]]$value,
    "Improved instructions."
  )
  expect_false(identical(
    population[[3L]]$components[["module_src::$/steps/flexible"]]$value,
    population[[1L]]$components[["module_src::$/steps/flexible"]]$value
  ))
  expect_true(jsonlite::validate(
    population[[3L]]$components[["module_src::$/steps/flexible"]]$value
  ))

  singleton <- dsprrr:::gepa_initial_component_population(
    program,
    population_size = 1L,
    .llm = chat
  )
  expect_length(singleton, 1L)
  expect_identical(
    singleton[[1L]]$components,
    dsprrr:::gepa_component_candidate(program)$components
  )
})

test_that("component selectors support round-robin, all, and custom policies", {
  candidate <- dsprrr:::gepa_component_candidate(gepa_flex_test_program())
  ids <- names(candidate$components)

  expect_identical(
    dsprrr:::gepa_component_selector_ids("round_robin", candidate),
    ids[[1L]]
  )
  candidate$history <- list(list(component_id = ids[[1L]]))
  expect_identical(
    dsprrr:::gepa_component_selector_ids("round_robin", candidate),
    ids[[2L]]
  )
  expect_identical(
    dsprrr:::gepa_component_selector_ids("all", candidate),
    ids
  )
  custom <- function(component_ids, candidate, failed_examples, context) {
    expect_identical(context$generation, 3L)
    component_ids[[2L]]
  }
  expect_identical(
    dsprrr:::gepa_component_selector_ids(
      custom,
      candidate,
      context = list(generation = 3L)
    ),
    ids[[2L]]
  )
})

test_that("all-component mutation is atomic, budgeted, and lineage-tracked", {
  program <- gepa_flex_test_program()
  baseline <- dsprrr:::gepa_component_candidate(program)
  proposed <- gepa_flex_test_source("draft", "answer")
  chat <- structure(
    list(chat_structured = function(prompt, type, ...) {
      fields <- names(type@properties)
      if (identical(fields, "instructions")) {
        return(list(instructions = "Improved ordinary instructions."))
      }
      list(module_src = proposed)
    }),
    class = "Chat"
  )
  budget <- dsprrr:::new_optimizer_budget(dsprrr:::optimizer_control())

  mutated <- dsprrr:::gepa_mutate_component_candidate(
    baseline,
    program,
    failed_examples = list(),
    .llm = chat,
    budget = budget,
    unit_id = "gepa:all",
    component_selector = "all"
  )
  summary <- dsprrr:::optimizer_budget_summary(budget)

  expect_identical(
    mutated$components[["instructions::$/steps/ordinary"]]$value,
    "Improved ordinary instructions."
  )
  expect_false(identical(
    mutated$components[["module_src::$/steps/flexible"]]$value,
    baseline$components[["module_src::$/steps/flexible"]]$value
  ))
  expect_length(mutated$history, 2L)
  expect_identical(
    dsprrr:::gepa_component_candidate_lineage(mutated)$parents,
    dsprrr:::gepa_component_candidate_id(baseline)
  )
  expect_identical(summary$provider_calls, 2L)
  expect_setequal(
    summary$completed_units,
    c("gepa:all:component:1", "gepa:all:component:2")
  )
})

test_that("component crossover selects whole values and handles one parent", {
  program <- suppressWarnings(flex("question -> answer"))
  parent1 <- dsprrr:::gepa_component_candidate(program)
  parent2 <- parent1
  parent2$components[["module_src::$"]]$value <- gepa_flex_test_source()

  set.seed(11)
  child <- dsprrr:::gepa_crossover_component_candidates(parent1, parent2)
  for (id in names(child$components)) {
    expect_true(
      child$components[[id]]$value %in%
        c(
          parent1$components[[id]]$value,
          parent2$components[[id]]$value
        )
    )
  }
  expect_true(jsonlite::validate(
    child$components[["module_src::$"]]$value
  ))
  expect_true(
    dsprrr:::gepa_materialize_component_candidate(
      program,
      child
    )$ok
  )

  records <- list(list(
    candidate = parent1,
    scores = c(quality = 0.8),
    failed_examples = list(),
    complete = TRUE,
    candidate_valid = TRUE
  ))
  next_population <- dsprrr:::gepa_next_component_generation(
    records,
    program = program,
    population_size = 2L,
    mutation_rate = 0,
    crossover_rate = 0,
    selection = "current_best",
    .llm = NULL
  )
  expect_length(next_population, 2L)
  expect_true(all(vapply(
    next_population,
    inherits,
    logical(1),
    "dsprrr_gepa_component_candidate"
  )))
})

test_that("lineage merge combines compatible sibling improvements", {
  program <- gepa_flex_test_program()
  ancestor <- dsprrr:::gepa_component_candidate(program)
  ancestor_id <- dsprrr:::gepa_component_candidate_id(ancestor)
  left <- ancestor
  left$components[["instructions::$/steps/ordinary"]]$value <-
    "Left instruction improvement."
  left <- dsprrr:::gepa_set_component_candidate_lineage(
    left,
    parents = ancestor_id,
    tag = "reflective_mutation"
  )
  right <- ancestor
  right$components[["module_src::$/steps/flexible"]]$value <-
    gepa_flex_test_source("draft", "answer")
  right <- dsprrr:::gepa_set_component_candidate_lineage(
    right,
    parents = ancestor_id,
    tag = "reflective_mutation"
  )
  registry <- dsprrr:::gepa_component_candidate_registry(list(list(
    candidate = ancestor,
    scores = c(quality = 0.2),
    generation = 1L,
    index = 1L
  )))

  merged <- dsprrr:::gepa_merge_component_candidates(
    left,
    right,
    registry,
    parent1_score = 0.8,
    parent2_score = 0.9
  )

  expect_identical(
    merged$components[["instructions::$/steps/ordinary"]]$value,
    "Left instruction improvement."
  )
  expect_identical(
    merged$components[["module_src::$/steps/flexible"]]$value,
    right$components[["module_src::$/steps/flexible"]]$value
  )
  expect_identical(
    dsprrr:::gepa_component_candidate_lineage(merged)$ancestor,
    ancestor_id
  )
  expect_identical(tail(merged$history, 1L)[[1L]]$status, "lineage_merge")
  expect_true(
    dsprrr:::gepa_materialize_component_candidate(program, merged)$ok
  )
})

test_that("validation frontier retains per-example winners and outputs", {
  program <- suppressWarnings(flex("question -> answer"))
  baseline <- dsprrr:::gepa_component_candidate(program)
  alternative <- baseline
  alternative$components[["module_src::$"]]$value <- gepa_flex_test_source()
  dominated <- alternative
  dominated$components[["module_src::$"]]$value <- sub(
    "Check the draft before answering.",
    "Review the draft before answering.",
    dominated$components[["module_src::$"]]$value,
    fixed = TRUE
  )
  make_evaluation <- function(scores, predictions) {
    dsprrr:::EvalResult(
      examples = tibble::tibble(
        row_id = c(10L, 20L),
        score = scores,
        error = NA_character_,
        predicted = as.list(predictions),
        feedback = NA_character_
      ),
      mean_score = mean(scores),
      n_evaluated = 2L,
      n_errors = 0L
    )
  }
  records <- list(
    list(
      candidate = baseline,
      candidate_id = dsprrr:::gepa_component_candidate_id(baseline),
      parents = character(),
      candidate_valid = TRUE,
      complete = TRUE,
      scores = c(quality = 0.5),
      primary_eval = make_evaluation(c(0.9, 0.1), c("base-10", "base-20")),
      discovery_metric_calls = 2L
    ),
    list(
      candidate = alternative,
      candidate_id = dsprrr:::gepa_component_candidate_id(alternative),
      parents = dsprrr:::gepa_component_candidate_id(baseline),
      candidate_valid = TRUE,
      complete = TRUE,
      scores = c(quality = 0.6),
      primary_eval = make_evaluation(c(0.4, 0.8), c("alt-10", "alt-20")),
      discovery_metric_calls = 2L
    ),
    list(
      candidate = dominated,
      candidate_id = dsprrr:::gepa_component_candidate_id(dominated),
      parents = dsprrr:::gepa_component_candidate_id(baseline),
      candidate_valid = TRUE,
      complete = TRUE,
      scores = c(quality = 0.2),
      primary_eval = make_evaluation(c(0.2, 0.2), c("low-10", "low-20")),
      discovery_metric_calls = 2L
    )
  )

  result <- dsprrr:::gepa_component_validation_result(records)
  baseline_id <- records[[1L]]$candidate_id
  alternative_id <- records[[2L]]$candidate_id

  expect_identical(
    result$per_val_instance_best_candidates[["10"]],
    baseline_id
  )
  expect_identical(
    result$per_val_instance_best_candidates[["20"]],
    alternative_id
  )
  expect_identical(
    result$best_outputs_valset[["20"]][[1L]]$output,
    "alt-20"
  )
  expect_identical(result$validation_frontier_scores, c(`10` = 0.9, `20` = 0.8))
  parent_records <- dsprrr:::gepa_component_parent_records(records, "pareto")
  expect_setequal(
    vapply(parent_records, `[[`, character(1), "candidate_id"),
    c(baseline_id, alternative_id)
  )
})

test_that("GEPA Flex audit data and semantic limits are explicit", {
  program <- suppressWarnings(flex("question -> answer"))
  candidate <- dsprrr:::gepa_component_candidate(program)
  params <- dsprrr:::gepa_component_candidate_params(candidate)
  semantics <- dsprrr:::gepa_component_semantics()

  expect_named(
    params$component_values,
    "module_src::$"
  )
  expect_true(params$candidate_valid)
  expect_true(semantics$complete_program_candidates)
  expect_true(semantics$flex_source_is_single_component)
  expect_true(semantics$transactional_flex_binding)
  expect_true(semantics$validation_instance_frontier)
  expect_true(semantics$lineage_aware_merge)
  expect_false(semantics$inference_time_candidate_selection)
  expect_match(semantics$note, "validation example", fixed = TRUE)
})

test_that("failed example bundles preserve evaluation row identity", {
  evaluated <- dsprrr:::EvalResult(
    examples = tibble::tibble(
      row_id = c(11L, 29L),
      score = c(0, 1),
      error = NA_character_,
      predicted = list("wrong", "right"),
      feedback = c("fix row 11", "ok")
    ),
    mean_score = 0.5,
    n_evaluated = 2L,
    n_errors = 0L
  )
  dataset <- data.frame(
    question = c("q11", "q29"),
    answer = c("a11", "a29")
  )

  failures <- dsprrr:::gepa_failed_examples(
    evaluated,
    dataset,
    signature("question -> answer"),
    threshold = 0.5,
    output_col = "answer"
  )

  expect_length(failures, 1L)
  expect_identical(failures[[1L]]$row_id, 11L)
  expect_identical(failures[[1L]]$inputs$question, "q11")
  expect_identical(failures[[1L]]$expected, "a11")
  expect_identical(failures[[1L]]$predicted, "wrong")
  expect_identical(failures[[1L]]$feedback, "fix row 11")
})

test_that("compile GEPA selects a valid structural Flex candidate", {
  proposed <- gepa_flex_test_source()
  program <- suppressWarnings(flex("question -> answer"))
  canonical <- suppressWarnings(
    flex("question -> answer", module_src = proposed)
  )$module_src
  proposal_calls <- 0L
  chat <- structure(
    list(chat_structured = function(prompt, type, ...) {
      fields <- names(type@properties)
      if (identical(fields, "instructions")) {
        return(list(instructions = "Improved instruction candidate."))
      }
      if (identical(fields, "module_src")) {
        proposal_calls <<- proposal_calls + 1L
        return(list(module_src = proposed))
      }
      stop("unexpected structured call")
    }),
    class = "Chat"
  )
  evaluated_sources <- character()
  testthat::local_mocked_bindings(
    eval_program = function(program, dataset, metric, ...) {
      evaluated_sources <<- c(evaluated_sources, program$module_src)
      score <- if (identical(program$module_src, canonical)) 1 else 0.2
      dsprrr:::EvalResult(
        examples = tibble::tibble(
          row_id = 7L,
          score = score,
          error = NA_character_,
          predicted = list(if (score == 1) "right" else "wrong"),
          feedback = if (score == 1) "ok" else "try structure"
        ),
        mean_score = score,
        n_evaluated = 1L,
        n_errors = 0L,
        metric_calls = 1L
      )
    },
    .package = "dsprrr"
  )
  teleprompter <- GEPA(
    metric = function(...) 1,
    population_size = 3L,
    generations = 1L,
    selection = "current_best",
    track_best_outputs = TRUE,
    verbose = FALSE
  )

  optimized <- compile(
    teleprompter,
    program,
    data.frame(question = "q", answer = "right"),
    .llm = chat
  )
  metadata <- optimized$config$optimizer

  expect_identical(proposal_calls, 2L)
  expect_true(canonical %in% evaluated_sources)
  expect_identical(optimized$module_src, canonical)
  expect_identical(metadata$optimization_mode, "component_candidates")
  expect_identical(metadata$flex_paths, "$")
  expect_named(
    metadata$best_candidate$component_values,
    "module_src::$"
  )
  expect_identical(metadata$best_scores, c(quality = 1))
  expect_length(metadata$all_generations[[1L]]$population, 3L)
  expect_true(metadata$component_semantics$validation_instance_frontier)
  expect_identical(
    metadata$per_val_instance_best_candidates[["7"]],
    metadata$best_candidate_id
  )
  expect_length(metadata$best_outputs_valset[["7"]], 1L)
})

test_that("compile GEPA audits but never selects an invalid Flex source", {
  program <- suppressWarnings(flex(
    signature("question -> answer", instructions = "Baseline instructions.")
  ))
  baseline_source <- program$module_src
  evaluated_sources <- character()
  chat <- structure(
    list(chat_structured = function(prompt, type, ...) {
      fields <- names(type@properties)
      if (identical(fields, "module_src")) {
        return(list(module_src = "{}"))
      }
      stop("unexpected structured call")
    }),
    class = "Chat"
  )
  testthat::local_mocked_bindings(
    eval_program = function(program, dataset, metric, ...) {
      evaluated_sources <<- c(evaluated_sources, program$module_src)
      score <- -1
      dsprrr:::EvalResult(
        examples = tibble::tibble(
          row_id = 1L,
          score = score,
          error = NA_character_,
          predicted = list("answer"),
          feedback = "runtime candidate"
        ),
        mean_score = score,
        n_evaluated = 1L,
        n_errors = 0L,
        metric_calls = 1L
      )
    },
    .package = "dsprrr"
  )
  teleprompter <- GEPA(
    metric = function(...) 1,
    population_size = 3L,
    generations = 1L,
    selection = "current_best",
    max_errors = 5L,
    verbose = FALSE
  )

  optimized <- compile(
    teleprompter,
    program,
    data.frame(question = "q", answer = "a"),
    .llm = chat
  )
  metadata <- optimized$config$optimizer
  population <- metadata$all_generations[[1L]]$population
  invalid <- Filter(
    function(record) !isTRUE(record$candidate_valid),
    population
  )

  expect_length(evaluated_sources, 1L)
  expect_length(invalid, 2L)
  expect_true(all(vapply(
    invalid,
    function(record) identical(record$scores, c(quality = 0)),
    logical(1)
  )))
  expect_true(all(!vapply(invalid, `[[`, logical(1), "selectable")))
  expect_gt(invalid[[1L]]$scores[[1L]], metadata$best_scores[[1L]])
  expect_identical(metadata$invalid_candidate_count, 2L)
  expect_identical(metadata$budget_summary$total_errors, 2L)
  expect_identical(optimized$module_src, baseline_source)
  expect_true(all(vapply(
    metadata$pareto_frontier,
    function(entry) isTRUE(entry$candidate$candidate_valid),
    logical(1)
  )))
})

test_that("compile GEPA returns a safe composite clone on an early budget stop", {
  leaf <- suppressWarnings(flex("question -> answer"))
  program <- ensemble(list(leaf))
  optimized <- compile(
    GEPA(
      metric = function(...) 1,
      population_size = 2L,
      generations = 1L,
      verbose = FALSE
    ),
    program,
    data.frame(question = "q", answer = "a"),
    .llm = gepa_flex_test_chat(list(module_src = gepa_flex_test_source())),
    control = dsprrr:::optimizer_control(max_provider_calls = 0L)
  )

  expect_s3_class(optimized, "EnsembleModule")
  expect_false(identical(optimized, program))
  expect_false(identical(optimized$modules[[1L]], leaf))
  expect_identical(optimized$modules[[1L]]$module_src, leaf$module_src)
  expect_true(optimized$config$optimizer$partial)
  expect_identical(
    optimized$config$optimizer$stop_reason$code,
    "max_provider_calls"
  )
})

test_that("ordinary programs use complete component candidates and result metadata", {
  program <- module(
    signature("question -> answer", instructions = "Baseline."),
    type = "predict"
  )
  selector_calls <- 0L
  selector <- function(component_ids, candidate, failed_examples, context) {
    selector_calls <<- selector_calls + 1L
    component_ids[[1L]]
  }
  testthat::local_mocked_bindings(
    eval_program = function(program, dataset, metric, ...) {
      dsprrr:::EvalResult(
        examples = tibble::tibble(
          row_id = 1L,
          score = 1,
          error = NA_character_,
          predicted = list("answer"),
          feedback = NA_character_
        ),
        mean_score = 1,
        n_evaluated = 1L,
        n_errors = 0L,
        metric_calls = 1L
      )
    },
    .package = "dsprrr"
  )

  optimized <- compile(
    GEPA(
      metric = function(...) 1,
      population_size = 2L,
      generations = 1L,
      component_selector = selector,
      track_best_outputs = TRUE,
      verbose = FALSE
    ),
    program,
    data.frame(question = "q", answer = "a")
  )
  metadata <- optimized$config$optimizer

  expect_gt(selector_calls, 0L)
  expect_identical(metadata$optimization_mode, "component_candidates")
  expect_identical(metadata$component_selector, "custom")
  expect_named(metadata$best_candidate$component_values, "instructions::$")
  expect_true(metadata$component_semantics$validation_instance_frontier)
  expect_true(metadata$component_semantics$retained_best_outputs)
  expect_length(metadata$best_outputs_valset[["1"]], 2L)
})

test_that("GEPA separates discovery trainset from validation frontier data", {
  program <- suppressWarnings(flex("question -> answer"))
  observed_splits <- character()
  testthat::local_mocked_bindings(
    eval_program = function(program, dataset, metric, ...) {
      observed_splits <<- c(observed_splits, as.character(dataset$split[[1L]]))
      score <- if (identical(dataset$split[[1L]], "validation")) 0.8 else 0.2
      dsprrr:::EvalResult(
        examples = tibble::tibble(
          row_id = 1L,
          score = score,
          error = NA_character_,
          predicted = list(dataset$split[[1L]]),
          feedback = paste0("feedback:", dataset$split[[1L]])
        ),
        mean_score = score,
        n_evaluated = 1L,
        n_errors = 0L,
        metric_calls = 1L
      )
    },
    .package = "dsprrr"
  )

  optimized <- compile(
    GEPA(
      metric = function(...) 1,
      population_size = 2L,
      generations = 1L,
      verbose = FALSE
    ),
    program,
    data.frame(question = "train", answer = "t", split = "discovery"),
    valset = data.frame(
      question = "validation",
      answer = "v",
      split = "validation"
    )
  )
  metadata <- optimized$config$optimizer

  expect_setequal(unique(observed_splits), c("discovery", "validation"))
  expect_true(all(metadata$validation_frontier_scores == 0.8))
  expect_true(all(metadata$discovery_eval_counts == 1L))
  records <- metadata$all_generations[[1L]]$population
  expect_true(all(vapply(
    records,
    function(record) {
      identical(record$discovery_eval@examples$predicted[[1L]], "discovery")
    },
    logical(1)
  )))
})

test_that("GEPA propagates candidate runtime provider conditions", {
  program <- suppressWarnings(flex("question -> answer"))
  candidate <- dsprrr:::gepa_component_candidate(program)
  chat <- structure(
    list(
      chat_structured = function(...) {
        rlang::abort("provider offline", class = "gepa_provider_outage")
      },
      get_model = function() "broken-provider"
    ),
    class = "Chat"
  )
  control <- dsprrr:::optimizer_control(max_errors = 10L, num_threads = 1L)
  budget <- dsprrr:::new_optimizer_budget(control)

  expect_error(
    suppressWarnings(dsprrr:::gepa_evaluate_component_candidate(
      program,
      candidate,
      data.frame(question = "q", answer = "a"),
      metric = function(...) 1,
      .llm = chat,
      control = control,
      budget = budget,
      stage = "gepa_provider_test",
      unit_id = "gepa:provider:test",
      .cache = FALSE,
      .propagate_provider_errors = TRUE
    )),
    class = "gepa_provider_outage"
  )
})

test_that("all-component mutation rolls back at a provider budget boundary", {
  program <- gepa_flex_test_program()
  baseline <- dsprrr:::gepa_component_candidate(program)
  calls <- 0L
  chat <- structure(
    list(chat_structured = function(prompt, type, ...) {
      calls <<- calls + 1L
      list(instructions = "Changed instructions.")
    }),
    class = "Chat"
  )
  budget <- dsprrr:::new_optimizer_budget(
    dsprrr:::optimizer_control(max_provider_calls = 1L)
  )

  mutated <- dsprrr:::gepa_mutate_component_candidate(
    baseline,
    program,
    failed_examples = list(),
    .llm = chat,
    budget = budget,
    component_selector = "all"
  )

  expect_identical(mutated, baseline)
  expect_identical(calls, 1L)
  expect_identical(
    dsprrr:::optimizer_budget_summary(budget)$stop_reason$code,
    "max_provider_calls"
  )
})

test_that("merge invocation cap counts attempted rather than successful merges", {
  program <- suppressWarnings(flex("question -> answer"))
  candidate <- dsprrr:::gepa_component_candidate(program)
  evaluation <- dsprrr:::EvalResult(
    examples = tibble::tibble(
      row_id = 1L,
      score = 1,
      error = NA_character_,
      predicted = list("a"),
      feedback = NA_character_
    ),
    mean_score = 1,
    n_evaluated = 1L,
    n_errors = 0L
  )
  record <- list(
    candidate = candidate,
    candidate_id = dsprrr:::gepa_component_candidate_id(candidate),
    scores = c(quality = 1),
    complete = TRUE,
    candidate_valid = TRUE,
    failed_examples = list(),
    generation = 1L,
    index = 1L,
    primary_eval = evaluation
  )
  attempts <- 0L
  testthat::local_mocked_bindings(
    gepa_merge_component_candidates = function(...) {
      attempts <<- attempts + 1L
      NULL
    },
    .package = "dsprrr"
  )

  population <- dsprrr:::gepa_next_component_generation(
    list(record),
    program,
    population_size = 4L,
    mutation_rate = 0,
    crossover_rate = 1,
    selection = "current_best",
    .llm = NULL,
    use_merge = TRUE,
    max_merges = 1L,
    all_records = list(record)
  )

  expect_identical(attempts, 1L)
  expect_identical(attr(population, "merge_invocations"), 1L)
  expect_identical(attr(population, "lineage_merges"), 0L)
})

test_that("validation winners do not exclude objective Pareto parents", {
  program <- suppressWarnings(flex("question -> answer"))
  left <- dsprrr:::gepa_component_candidate(program)
  right <- left
  right$components[[1L]]$value <- paste0(
    right$components[[1L]]$value,
    " "
  )
  make_record <- function(candidate, quality, safety) {
    list(
      candidate = candidate,
      candidate_id = dsprrr:::gepa_component_candidate_id(candidate),
      scores = c(quality = quality, safety = safety),
      complete = TRUE,
      candidate_valid = TRUE,
      primary_eval = dsprrr:::EvalResult(
        examples = tibble::tibble(
          row_id = 1L,
          score = quality,
          error = NA_character_,
          predicted = list("a"),
          feedback = NA_character_
        ),
        mean_score = quality,
        n_evaluated = 1L,
        n_errors = 0L
      )
    )
  }
  records <- list(
    make_record(left, 1, 0),
    make_record(right, 0.9, 1)
  )

  parents <- dsprrr:::gepa_component_parent_records(records, "pareto")

  expect_setequal(
    vapply(parents, `[[`, character(1), "candidate_id"),
    vapply(records, `[[`, character(1), "candidate_id")
  )
})

test_that("mutation metadata distinguishes immediate parents from ancestors", {
  program <- gepa_flex_test_program()
  seed <- dsprrr:::gepa_component_candidate(program)
  seed_id <- dsprrr:::gepa_component_candidate_id(seed)
  parent <- seed
  parent$components[[1L]]$value <- "Parent instructions."
  parent <- dsprrr:::gepa_set_component_candidate_lineage(
    parent,
    parents = seed_id,
    tag = "test_parent"
  )
  parent_id <- dsprrr:::gepa_component_candidate_id(parent)

  child <- dsprrr:::gepa_mutate_component_candidate(
    parent,
    program,
    failed_examples = list(list(feedback = "Improve it.")),
    component_id = names(parent$components)[[1L]]
  )
  lineage <- dsprrr:::gepa_component_candidate_lineage(child)

  expect_identical(lineage$parents, parent_id)
  expect_identical(lineage$ancestors, seed_id)
})

test_that("best-output semantics reflect the tracking option", {
  expect_false(dsprrr:::gepa_component_semantics(FALSE)$retained_best_outputs)
  expect_true(dsprrr:::gepa_component_semantics(TRUE)$retained_best_outputs)
  expect_true(
    dsprrr:::gepa_component_semantics(FALSE)$supports_retained_best_outputs
  )
})

test_that("GEPA proposes, executes, and selects executable Flex source", {
  skip_if_not_installed("callr")
  baseline <- paste(
    "forward <- function(question)",
    "Prediction(answer = question)"
  )
  improved <- paste(
    "forward <- function(question)",
    "Prediction(answer = toupper(question))"
  )
  program <- suppressWarnings(flex(
    "question -> answer",
    module_src = baseline,
    interpreter_factory = r_code_runner,
    source_format = "r",
    require_sandbox = FALSE
  ))
  chat <- structure(
    list(
      chat_structured = function(prompt, type, ...) {
        list(
          module_src = paste(
            "forward <- function(question)",
            "Prediction(answer = toupper(question))"
          )
        )
      },
      get_model = function() "gepa-executable-test"
    ),
    class = "Chat"
  )

  optimized <- compile(
    GEPA(
      metric = metric_exact_match(field = "answer"),
      population_size = 2L,
      generations = 1L,
      selection = "current_best",
      track_best_outputs = TRUE,
      verbose = FALSE
    ),
    program,
    data.frame(question = "hello", answer = "HELLO"),
    .llm = chat,
    control = dsprrr:::optimizer_control(num_threads = 1L)
  )
  metadata <- optimized$config$optimizer

  expect_identical(optimized$module_src, improved)
  expect_identical(metadata$best_scores, c(quality = 1))
  expect_identical(
    metadata$per_val_instance_best_candidates[["1"]],
    metadata$best_candidate_id
  )
  expect_identical(
    optimized$forward(list(question = "hello"))$output[[1L]],
    list(answer = "HELLO")
  )
})
