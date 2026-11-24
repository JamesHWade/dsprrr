test_that("program artifact phases preserve the canonical payload", {
  program <- module(signature("text -> answer"))

  plan <- dsprrr:::artifact_plan_program(program)

  expect_identical(plan$registry, list())
  expect_identical(plan$trusted, FALSE)
  expect_length(plan$node_ids, 1L)
  expect_identical(unname(plan$node_ids), "$")

  payload <- dsprrr:::artifact_serialize_program_plan(plan)
  expect_null(attr(payload, "class"))
  expect_false("integrity" %in% names(payload))

  artifact <- dsprrr:::artifact_finalize_program(payload)
  direct <- program_artifact(program)
  expect_identical(artifact$format, direct$format)
  expect_identical(artifact$format_version, direct$format_version)
  expect_identical(artifact$root, direct$root)
  expect_identical(artifact$graph, direct$graph)
  expect_identical(artifact$exclusions, direct$exclusions)
  expect_identical(artifact$integrity, dsprrr:::artifact_integrity(artifact))
})

test_that("restore phases carry explicit admission and execution state", {
  artifact <- program_artifact(module(signature("text -> answer")))

  admission <- dsprrr:::artifact_admit_restore(artifact)
  expect_identical(admission$artifact, artifact)
  expect_identical(admission$registry, list())
  expect_identical(admission$trusted, FALSE)

  execution <- dsprrr:::artifact_execute_restore(admission)
  expect_s3_class(execution$program, "Module")
  expect_true(is.environment(execution$cache))

  restored <- dsprrr:::artifact_finalize_restore(execution, admission)
  expect_identical(
    program_artifact(restored)$graph,
    artifact$graph
  )
})

test_that("restore admission rejects malformed manifests before construction", {
  artifact <- program_artifact(module(signature("text -> answer")))
  malformed <- artifact
  malformed$graph$nodes <- list()
  malformed$integrity <- dsprrr:::artifact_integrity(malformed)
  constructor_called <- FALSE

  testthat::local_mocked_bindings(
    artifact_build_node = function(...) {
      constructor_called <<- TRUE
      cli::cli_abort("unexpected artifact construction")
    },
    .package = "dsprrr"
  )

  condition <- rlang::catch_cnd(
    restore_module_config(malformed)
  )

  expect_s3_class(condition, "dsprrr_artifact_malformed")
  expect_false(constructor_called)
})
