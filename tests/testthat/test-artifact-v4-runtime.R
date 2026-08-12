artifact_v4_runner <- function() {
  list(
    execute = function(code, context = list()) {
      list(
        success = TRUE,
        result = NULL,
        stdout = "",
        stderr = "",
        error = "",
        duration_ms = 0
      )
    },
    policy = function() {
      list(
        backend = "artifact-test",
        trust = "test-only",
        sandboxed = TRUE
      )
    },
    close = function() invisible(NULL)
  )
}

artifact_v4_modules <- function(interpreter_factory) {
  list(
    program_of_thought = dsprrr:::ProgramOfThoughtModule$new(
      signature = signature("question -> answer"),
      interpreter_factory = interpreter_factory
    ),
    codeact = dsprrr:::CodeActModule$new(
      signature = signature("question -> answer"),
      interpreter_factory = interpreter_factory
    ),
    rlm = dsprrr:::RLMModule$new(
      signature = signature("question -> answer"),
      interpreter_factory = interpreter_factory,
      max_llm_calls = 0L
    )
  )
}

artifact_v4_rehash <- function(artifact) {
  artifact$integrity <- dsprrr:::artifact_integrity(artifact)
  artifact
}

artifact_v4_strip_rlm_children <- function(artifact, node_id = "$") {
  child_ids <- vapply(
    artifact$graph$nodes[[node_id]]$children,
    `[[`,
    character(1),
    ".node"
  )
  artifact$graph$nodes[[node_id]]$children <- list()
  artifact$graph$nodes <- artifact$graph$nodes[
    !names(artifact$graph$nodes) %in% child_ids
  ]
  artifact$graph$edges <- Filter(
    function(edge) {
      !edge$from %in% child_ids && !edge$to %in% child_ids
    },
    artifact$graph$edges
  )
  artifact
}

test_that("v5 interpreter factories round-trip without being invoked", {
  calls <- 0L
  interpreter_factory <- function(.unused = NULL) {
    calls <<- calls + 1L
    artifact_v4_runner()
  }
  registry <- list(interpreter_factory = interpreter_factory)

  for (program in artifact_v4_modules(interpreter_factory)) {
    artifact <- program_artifact(program, registry = registry)
    expect_identical(artifact$format_version, 5L)
    fields <- artifact$graph$nodes[["$"]]$fields
    expect_null(fields$runner)
    expect_identical(
      fields$interpreter_factory$.dsprrr$payload$id,
      "interpreter_factory"
    )
    expect_no_error(dsprrr:::artifact_validate_manifest(artifact))
    expect_identical(calls, 0L)

    expect_error(
      restore_module_config(artifact),
      class = "dsprrr_artifact_registry_error"
    )
    expect_identical(calls, 0L)

    restored <- restore_module_config(artifact, registry = registry)
    expect_null(restored$runner)
    expect_identical(restored$interpreter_factory, interpreter_factory)
    expect_no_error(print(restored))
    expect_identical(calls, 0L)

    expect_error(
      export_module_code(program, registry = registry),
      class = "dsprrr_artifact_code_export_unsupported"
    )
    expect_identical(calls, 0L)
  }
})

test_that("v5 RLM predictor children preserve tuned state", {
  calls <- 0L
  interpreter_factory <- function(.unused = NULL) {
    calls <<- calls + 1L
    artifact_v4_runner()
  }
  action <- dsprrr:::new_rlm_action_module()
  extract <- dsprrr:::new_rlm_extract_module(
    signature("question -> answer")@output_type
  )
  action$signature@instructions <- "Use the tuned action policy."
  action$demos <- list(list(
    state = "trajectory",
    reasoning = "inspect the evidence",
    code = "summary(.context$data)"
  ))
  action$state$compiled <- TRUE
  action$state$best_score <- 0.8
  action$state$best_trial <- 2L
  action$state$best_params <- list(temperature = 0.2)
  extract$signature@instructions <- "Use the tuned extraction policy."
  extract$demos <- list(list(state = "trajectory", answer = "supported"))
  program <- dsprrr:::RLMModule$new(
    signature = signature("question -> answer"),
    interpreter_factory = interpreter_factory,
    max_llm_calls = 0L,
    generate_action = action,
    extract = extract
  )
  registry <- list(interpreter_factory = interpreter_factory)

  artifact <- program_artifact(program, registry = registry)
  restored <- restore_module_config(artifact, registry = registry)

  expect_named(
    artifact$graph$nodes[["$"]]$children,
    c("generate_action", "extract")
  )
  expect_named(
    artifact$graph$nodes,
    c("$", "$/generate_action", "$/extract")
  )
  expect_identical(
    restored$generate_action$signature@instructions,
    "Use the tuned action policy."
  )
  expect_identical(restored$generate_action$demos, action$demos)
  expect_identical(restored$generate_action$state$compiled, TRUE)
  expect_identical(restored$generate_action$state$best_score, 0.8)
  expect_identical(restored$generate_action$state$best_trial, 2L)
  expect_identical(
    restored$generate_action$state$best_params,
    list(temperature = 0.2)
  )
  expect_identical(
    restored$extract$signature@instructions,
    "Use the tuned extraction policy."
  )
  expect_identical(restored$extract$demos, extract$demos)
  expect_identical(restored$interpreter_factory, interpreter_factory)
  expect_identical(calls, 0L)
  expect_identical(
    program_artifact(restored, registry = registry)$graph,
    artifact$graph
  )
})

test_that("childless v4 RLM artifacts restore with default predictors", {
  calls <- 0L
  interpreter_factory <- function(.unused = NULL) {
    calls <<- calls + 1L
    artifact_v4_runner()
  }
  registry <- list(interpreter_factory = interpreter_factory)
  program <- dsprrr:::RLMModule$new(
    signature = signature("question -> answer"),
    interpreter_factory = interpreter_factory,
    max_llm_calls = 0L
  )
  artifact <- program_artifact(program, registry = registry) |>
    artifact_v4_strip_rlm_children()
  artifact$format_version <- 4L
  artifact <- artifact_v4_rehash(artifact)

  expect_no_error(dsprrr:::artifact_validate_manifest(artifact))
  restored <- restore_module_config(artifact, registry = registry)

  expect_named(module_children(restored), c("generate_action", "extract"))
  expect_s3_class(restored$generate_action, "PredictModule")
  expect_s3_class(restored$extract, "PredictModule")
  expect_identical(restored$interpreter_factory, interpreter_factory)
  expect_identical(calls, 0L)
  upgraded <- program_artifact(restored, registry = registry)
  expect_named(
    upgraded$graph$nodes[["$"]]$children,
    c("generate_action", "extract")
  )
})

test_that("v5 RLM artifacts reject partial predictor child schemas", {
  interpreter_factory <- function(.unused = NULL) artifact_v4_runner()
  registry <- list(interpreter_factory = interpreter_factory)
  program <- dsprrr:::RLMModule$new(
    signature = signature("question -> answer"),
    interpreter_factory = interpreter_factory,
    max_llm_calls = 0L
  )
  artifact <- program_artifact(program, registry = registry)
  artifact$graph$nodes[["$"]]$children$extract <- NULL
  artifact <- artifact_v4_rehash(artifact)

  expect_error(
    dsprrr:::artifact_validate_manifest(artifact),
    class = "dsprrr_artifact_malformed"
  )
})

test_that("v5 interpreter factories support explicit trusted embedding", {
  calls <- 0L
  interpreter_factory <- function(.unused = NULL) {
    calls <<- calls + 1L
    artifact_v4_runner()
  }
  program <- dsprrr:::ProgramOfThoughtModule$new(
    signature = signature("question -> answer"),
    interpreter_factory = interpreter_factory
  )

  artifact <- program_artifact(program, trusted = TRUE)
  expect_error(
    restore_module_config(artifact),
    class = "dsprrr_artifact_unsafe_value"
  )
  restored <- restore_module_config(artifact, trusted = TRUE)

  expect_true(is.function(restored$interpreter_factory))
  expect_identical(calls, 0L)
})

test_that("v5 runtime bindings enforce exact XOR", {
  runner <- artifact_v4_runner()
  factory <- function(.unused = NULL) artifact_v4_runner()
  registry <- list(runner = runner, factory = factory)
  runner_artifact <- program_artifact(
    dsprrr:::ProgramOfThoughtModule$new(
      signature = signature("question -> answer"),
      runner = runner
    ),
    registry = registry
  )
  factory_artifact <- program_artifact(
    dsprrr:::ProgramOfThoughtModule$new(
      signature = signature("question -> answer"),
      interpreter_factory = factory
    ),
    registry = registry
  )

  both <- factory_artifact
  both$graph$nodes[["$"]]$fields$runner <-
    runner_artifact$graph$nodes[["$"]]$fields$runner
  both <- artifact_v4_rehash(both)
  expect_error(
    restore_module_config(both, registry = registry),
    class = "dsprrr_artifact_malformed"
  )

  neither <- factory_artifact
  neither$graph$nodes[["$"]]$fields$interpreter_factory <- NULL
  neither <- artifact_v4_rehash(neither)
  expect_error(
    restore_module_config(neither, registry = registry),
    class = "dsprrr_artifact_malformed"
  )
})

test_that("validated v3 runner artifacts upgrade before restoration", {
  runner <- artifact_v4_runner()
  registry <- list(runner = runner)
  program <- dsprrr:::RLMModule$new(
    signature = signature("question -> answer"),
    runner = runner,
    max_llm_calls = 1L
  )
  artifact <- program_artifact(program, registry = registry)
  artifact <- artifact_v4_strip_rlm_children(artifact)
  fields <- artifact$graph$nodes[["$"]]$fields
  fields$interpreter_factory <- NULL
  fields <- fields[setdiff(names(fields), "interpreter_factory")]
  artifact$graph$nodes[["$"]]$fields <- fields
  artifact$format_version <- 3L
  artifact <- artifact_v4_rehash(artifact)

  expect_no_error(dsprrr:::artifact_validate_manifest(artifact))
  restored <- restore_module_config(artifact, registry = registry)

  expect_identical(restored$runner, runner)
  expect_null(restored$interpreter_factory)
  expect_identical(restored$max_llm_calls, 1L)

  zero_calls <- artifact
  zero_calls$graph$nodes[["$"]]$fields$max_llm_calls <- 0L
  zero_calls <- artifact_v4_rehash(zero_calls)
  expect_error(
    dsprrr:::artifact_validate_manifest(zero_calls),
    class = "dsprrr_artifact_malformed"
  )

  corrupt <- artifact
  corrupt$graph$nodes[["$"]]$fields$max_iterations <- 999L
  expect_error(
    restore_module_config(corrupt, registry = registry),
    class = "dsprrr_artifact_integrity_error"
  )
})

test_that("Flex modules keep their resource-free codec in v5", {
  program <- suppressWarnings(
    dsprrr:::FlexModule$new(
      signature = signature("question -> answer"),
      max_predictor_calls = 7L,
      config = list(label = "persisted")
    )
  )
  program$state$traces <- list(list(active_lease = new.env()))

  artifact <- program_artifact(program)
  fields <- artifact$graph$nodes[["$"]]$fields
  restored <- restore_module_config(artifact)

  expect_identical(artifact$format_version, 5L)
  expect_named(
    fields,
    c(
      "module_src",
      "max_predictor_calls",
      "max_tool_calls",
      "source_format",
      "tools",
      "interpreter_factory",
      "require_sandbox"
    )
  )
  expect_identical(fields$module_src, program$module_src)
  expect_identical(fields$max_predictor_calls, 7L)
  expect_identical(fields$max_tool_calls, 100L)
  expect_identical(fields$source_format, "json")
  expect_length(fields$tools, 0L)
  expect_null(fields$interpreter_factory)
  expect_true(fields$require_sandbox)
  expect_false(any(grepl("active_lease", capture.output(dput(artifact)))))
  expect_s3_class(restored, "FlexModule")
  expect_identical(restored$module_src, program$module_src)
  expect_identical(restored$max_predictor_calls, 7L)
  expect_identical(restored$max_tool_calls, 100L)
  expect_identical(restored$source_format, "json")
  expect_identical(restored$config$label, "persisted")
  expect_length(restored$state$traces, 0L)

  exported <- export_module_code(program, name = "restored_flex")
  environment <- new.env(parent = globalenv())
  expect_no_error(eval(parse(text = exported), envir = environment))
  expect_s3_class(environment$restored_flex, "FlexModule")
  expect_identical(environment$restored_flex$module_src, program$module_src)
})

test_that("Flex artifacts preserve an unlimited runtime predictor budget", {
  program <- suppressWarnings(
    dsprrr:::FlexModule$new(
      signature = signature("question -> answer"),
      max_predictor_calls = NULL
    )
  )

  artifact <- program_artifact(program)
  restored <- restore_module_config(artifact)

  expect_null(artifact$graph$nodes[["$"]]$fields$max_predictor_calls)
  expect_null(restored$max_predictor_calls)
})

test_that("Flex artifacts preserve an unlimited host-tool budget", {
  program <- suppressWarnings(
    dsprrr:::FlexModule$new(
      signature = signature("question -> answer"),
      max_tool_calls = NULL
    )
  )

  artifact <- program_artifact(program)
  restored <- restore_module_config(artifact)

  expect_null(artifact$graph$nodes[["$"]]$fields$max_tool_calls)
  expect_null(restored$max_tool_calls)
})

test_that("Flex artifacts preserve a zero predictor budget", {
  source <- jsonlite::toJSON(
    list(
      schema_version = 1L,
      steps = list(),
      outputs = list(result = "$input.value")
    ),
    auto_unbox = TRUE
  )
  program <- suppressWarnings(flex(
    "value -> result",
    module_src = source,
    max_predictor_calls = 0L
  ))

  artifact <- program_artifact(program)
  restored <- restore_module_config(artifact)

  expect_identical(restored$max_predictor_calls, 0L)
})

test_that("executable Flex runtime dependencies round-trip through a registry", {
  calls <- 0L
  offset <- 3L
  interpreter_factory <- function(.unused = NULL) {
    calls <<- calls + 1L
    artifact_v4_runner()
  }
  add_offset <- function(value) value + offset
  registry <- list(
    interpreter_factory = interpreter_factory,
    add_offset = add_offset
  )
  program <- suppressWarnings(flex(
    "value: integer -> result: integer",
    module_src = paste(
      "forward <- function(value) {",
      "  list(result = add_offset(value = value))",
      "}",
      sep = "\n"
    ),
    tools = list(add_offset = add_offset),
    interpreter_factory = interpreter_factory,
    source_format = "r",
    require_sandbox = FALSE,
    max_tool_calls = 4L
  ))

  artifact <- program_artifact(program, registry = registry)
  fields <- artifact$graph$nodes[["$"]]$fields
  restored <- restore_module_config(artifact, registry = registry)

  expect_identical(fields$source_format, "r")
  expect_identical(fields$max_tool_calls, 4L)
  expect_identical(
    fields$interpreter_factory$.dsprrr$payload$id,
    "interpreter_factory"
  )
  expect_identical(fields$tools[[1L]]$.dsprrr$payload$id, "add_offset")
  expect_false(fields$require_sandbox)
  expect_identical(restored$source_format, "r")
  expect_identical(restored$max_tool_calls, 4L)
  expect_identical(restored$interpreter_factory, interpreter_factory)
  expect_identical(restored$tools$add_offset, add_offset)
  expect_false(restored$require_sandbox)
  expect_identical(calls, 0L)
})

test_that("legacy two-field v4 Flex artifacts remain readable", {
  program <- suppressWarnings(flex("question -> answer"))
  artifact <- program_artifact(program)
  artifact$graph$nodes[["$"]]$fields <- artifact$graph$nodes[["$"]]$fields[
    c("module_src", "max_predictor_calls")
  ]
  artifact <- artifact_v4_rehash(artifact)

  expect_no_error(dsprrr:::artifact_validate_manifest(artifact))
  restored <- restore_module_config(artifact)
  expect_identical(restored$source_format, "json")
  expect_identical(restored$module_src, program$module_src)
  expect_identical(restored$max_tool_calls, 100L)
})

test_that("earlier runtime-aware v4 Flex artifacts remain readable", {
  program <- suppressWarnings(flex("question -> answer"))
  artifact <- program_artifact(program)
  fields <- artifact$graph$nodes[["$"]]$fields
  artifact$graph$nodes[["$"]]$fields <- fields[
    setdiff(names(fields), "max_tool_calls")
  ]
  artifact <- artifact_v4_rehash(artifact)

  expect_no_error(dsprrr:::artifact_validate_manifest(artifact))
  restored <- restore_module_config(artifact)
  expect_identical(restored$max_tool_calls, 100L)
})

test_that("Flex artifact validation rejects invalid host-tool maps", {
  interpreter_factory <- function() artifact_v4_runner()
  identity_tool <- function(value) value
  registry <- list(
    interpreter_factory = interpreter_factory,
    identity_tool = identity_tool
  )
  program <- suppressWarnings(flex(
    "value -> result",
    module_src = "forward <- function(value) list(result = identity_tool(value = value))",
    tools = list(identity_tool = identity_tool),
    interpreter_factory = interpreter_factory,
    source_format = "r"
  ))
  artifact <- program_artifact(program, registry = registry)
  names(artifact$graph$nodes[["$"]]$fields$tools) <- "bad name"
  artifact <- artifact_v4_rehash(artifact)

  error <- tryCatch(
    dsprrr:::artifact_validate_manifest(artifact),
    error = identity
  )

  expect_s3_class(error, "dsprrr_artifact_malformed")
})
