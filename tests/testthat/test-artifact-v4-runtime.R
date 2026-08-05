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

test_that("v4 interpreter factories round-trip without being invoked", {
  calls <- 0L
  interpreter_factory <- function(.unused = NULL) {
    calls <<- calls + 1L
    artifact_v4_runner()
  }
  registry <- list(interpreter_factory = interpreter_factory)

  for (program in artifact_v4_modules(interpreter_factory)) {
    artifact <- program_artifact(program, registry = registry)
    expect_identical(artifact$format_version, 4L)
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

test_that("v4 interpreter factories support explicit trusted embedding", {
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

test_that("v4 runtime bindings enforce exact XOR", {
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

test_that("Flex modules have a resource-free v4 artifact codec", {
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

  expect_identical(artifact$format_version, 4L)
  expect_named(fields, c("module_src", "max_predictor_calls"))
  expect_identical(fields$module_src, program$module_src)
  expect_identical(fields$max_predictor_calls, 7L)
  expect_false(any(grepl("active_lease", capture.output(dput(artifact)))))
  expect_s3_class(restored, "FlexModule")
  expect_identical(restored$module_src, program$module_src)
  expect_identical(restored$max_predictor_calls, 7L)
  expect_identical(restored$config$label, "persisted")
  expect_length(restored$state$traces, 0L)

  exported <- export_module_code(program, name = "restored_flex")
  environment <- new.env(parent = globalenv())
  expect_no_error(eval(parse(text = exported), envir = environment))
  expect_s3_class(environment$restored_flex, "FlexModule")
  expect_identical(environment$restored_flex$module_src, program$module_src)
})
