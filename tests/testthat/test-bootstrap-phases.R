# Contract tests for the BootstrapFewShot orchestration phases.

test_that("bootstrap validation phase retains guard ordering", {
  program <- module(signature("x -> y"))

  empty <- BootstrapFewShot()
  empty_result <- NULL
  expect_warning(
    empty_result <- dsprrr:::bootstrap_validate_inputs(
      empty,
      program,
      data.frame(x = character(), y = character())
    ),
    "Empty trainset"
  )
  expect_true(empty_result$empty)

  expect_error(
    dsprrr:::bootstrap_validate_inputs(
      BootstrapFewShot(metric = function(...) 1),
      program,
      list(x = "a", y = "b")
    ),
    "must be a data frame"
  )
  expect_error(
    dsprrr:::bootstrap_validate_inputs(
      BootstrapFewShot(),
      program,
      data.frame(x = "a", y = "b")
    ),
    "requires a metric"
  )
})

test_that("module planning is explicit and does not execute candidates", {
  calls <- 0L
  program <- module(signature("x -> y"), template = "{x}")
  teleprompter <- BootstrapFewShot(
    metric = function(...) {
      calls <<- calls + 1L
      1
    },
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 1L,
    seed = 42L
  )

  plan <- dsprrr:::bootstrap_module_plan(
    teleprompter,
    program,
    data.frame(x = c("a", "b"), y = c("A", "B")),
    control = dsprrr:::optimizer_control(progress = FALSE)
  )

  expect_false(plan$complete)
  expect_equal(plan$bootstrap_indices, 2L)
  expect_length(plan$labeled_demos, 1L)
  expect_equal(plan$labeled_demos[[1]]$source, "labeled")
  expect_length(plan$bootstrapped_demos, 0L)
  expect_identical(calls, 0L)
  expect_false(identical(plan$student, program))
  expect_false(identical(plan$teacher, program))
})

test_that("labeled-only compilation does not force an unused llm", {
  program <- module(signature("x -> y"))
  teleprompter <- BootstrapFewShot(
    metric = function(...) 1,
    max_labeled_demos = 1L,
    max_bootstrapped_demos = 0L
  )

  compiled <- dsprrr:::compile_bootstrap(
    teleprompter,
    program,
    data.frame(x = "a", y = "A"),
    .llm = stop("the unused llm was forced"),
    control = dsprrr:::optimizer_control(progress = FALSE)
  )

  expect_length(compiled$demos, 1L)
  expect_identical(compiled$demos[[1L]]$source, "labeled")
})

test_that("pipeline planning identifies demo steps and final-step labels", {
  first <- module(signature("x -> draft"))
  second <- module(signature("draft -> y"))
  program <- first %>>% second
  teleprompter <- BootstrapFewShot(
    metric = function(...) 1,
    max_labeled_demos = 2L,
    max_bootstrapped_demos = 1L
  )

  plan <- dsprrr:::bootstrap_pipeline_plan(
    teleprompter,
    program,
    data.frame(x = c("a", "b"), y = c("A", "B")),
    control = dsprrr:::optimizer_control(progress = FALSE)
  )

  expect_false(plan$complete)
  expect_identical(plan$demo_steps, c(1L, 2L))
  expect_identical(plan$n_labeled, 0L)
  expect_identical(plan$bootstrap_indices, c(1L, 2L))
  expect_named(plan$step_demos, c("1", "2"))
  expect_identical(unname(lengths(plan$step_demos)), c(0L, 0L))
})

test_that("checkpoint phase receives a value-oriented state", {
  program <- module(signature("x -> y"))
  student <- dsprrr:::copy_module(program)
  budget <- dsprrr:::new_optimizer_budget(
    dsprrr:::optimizer_control(progress = FALSE)
  )
  observed <- new.env(parent = emptyenv())

  dsprrr:::bootstrap_write_checkpoint(
    kind = "module",
    student = student,
    labeled_demos = list(list(source = "labeled")),
    bootstrapped_demos = list(list(source = "bootstrapped")),
    bootstrap_lineage = list(list(child = "demo:1")),
    checkpoint_context = NULL,
    checkpoint_callback = function(state, phase, best_program, budget) {
      observed$state <- state
      observed$phase <- phase
      observed$best_program <- best_program
      observed$budget <- budget
    },
    budget = budget
  )

  expect_identical(observed$phase, "bootstrap")
  expect_identical(observed$state$kind, "module")
  expect_length(observed$state$bootstrapped_demos, 1L)
  expect_length(observed$best_program$demos, 2L)
  expect_identical(observed$budget, budget)
  expect_length(student$demos, 0L)
})

test_that("execution and finalization phases compose through the central result", {
  program <- module(signature("x -> y"), template = "{x}")
  chat <- new_test_chat(
    chat_structured = function(...) list(y = "ok")
  )
  teleprompter <- BootstrapFewShot(
    metric = function(...) 1,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 1L
  )
  plan <- dsprrr:::bootstrap_module_plan(
    teleprompter,
    program,
    data.frame(x = "a", y = "ok"),
    .llm = chat,
    control = dsprrr:::optimizer_control(progress = FALSE)
  )
  state <- dsprrr:::bootstrap_module_execute(plan)
  compiled <- dsprrr:::bootstrap_finalize_module(plan, state)

  expect_identical(state$budget$attempts, 1L)
  expect_length(compiled$demos, 1L)
  expect_identical(
    optimization_result(compiled)$best_params$n_bootstrapped_demos,
    1L
  )
  expect_true(compiled$config$compiled)
})
