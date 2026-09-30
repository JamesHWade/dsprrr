# Tests for joint pipeline compilation with BootstrapFewShot

# R6 mock step module: deterministic forward via a supplied function,
# inherits PredictModule so it supports demos like a real step
MockStepModule <- R6::R6Class(
  "MockStepModule",
  inherit = dsprrr:::PredictModule,
  public = list(
    fn = NULL,
    initialize = function(signature, fn) {
      super$initialize(
        signature,
        template = "",
        demos = list(),
        config = list()
      )
      self$fn <- fn
    },
    forward = function(batch, .llm = NULL, trace = TRUE, .cache = NULL, ...) {
      if (is.data.frame(batch)) {
        batch <- as.list(batch[1, , drop = FALSE])
      }
      tibble::tibble(
        output = list(self$fn(batch)),
        chat = list(NULL),
        metadata = list(list(total_tokens = 10, cost = 0.001, model = "mock"))
      )
    },
    deepcopy = function() {
      new_mod <- MockStepModule$new(self$signature, self$fn)
      new_mod$demos <- lapply(self$demos, function(x) x)
      new_mod
    }
  )
)

make_qa_pipeline <- function() {
  answers <- c(
    "What is 2+2?" = "4",
    "What is 3+3?" = "6",
    "What is 4+4?" = "8",
    "What is 5+5?" = "10"
  )

  sig1 <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_object(draft = ellmer::type_string()),
    instructions = "Draft an answer"
  )
  sig2 <- Signature(
    inputs = list(input(name = "draft", type = "string")),
    output_type = ellmer::type_object(answer = ellmer::type_string()),
    instructions = "Finalize the answer"
  )

  mod1 <- MockStepModule$new(sig1, function(b) {
    list(draft = as.character(b$question))
  })
  mod2 <- MockStepModule$new(sig2, function(b) {
    val <- unname(answers[as.character(b$draft)])
    list(answer = if (is.na(val)) "unknown" else val)
  })

  mod1 %>>% mod2
}

qa_metric <- function() {
  # Field-aware metrics receive the full trainset row as `expected`
  metric <- function(prediction, expected) {
    pred <- if (is.list(prediction)) prediction$answer else prediction
    exp <- if (is.list(expected)) expected$answer else expected
    as.numeric(identical(as.character(pred), as.character(exp)))
  }
  attr(metric, "field") <- "answer"
  metric
}

qa_trainset <- function() {
  data.frame(
    question = c(
      "What is 2+2?",
      "What is 3+3?",
      "What is 4+4?",
      "What is 5+5?"
    ),
    answer = c("4", "6", "8", "10"),
    stringsAsFactors = FALSE
  )
}

test_that("pipeline forward records per-step inputs in traces", {
  p <- make_qa_pipeline()

  result <- p$forward(list(question = "What is 2+2?"), trace = TRUE)

  expect_equal(result$output[[1]]$answer, "4")
  expect_length(p$state$traces, 1)

  trace <- p$state$traces[[1]]
  expect_true("step_inputs" %in% names(trace))
  expect_length(trace$step_inputs, 2)
  expect_equal(trace$step_inputs[[1]]$question, "What is 2+2?")
  expect_equal(trace$step_inputs[[2]]$draft, "What is 2+2?")
  expect_equal(trace$step_outputs[[2]]$answer, "4")
})

test_that("PipelineModule deepcopy creates independent step modules", {
  p <- make_qa_pipeline()
  p2 <- p$deepcopy()

  mod2_copy <- p2$steps[[2]]@module
  mod2_copy$demos <- list(list(inputs = list(draft = "x"), output = "y"))

  expect_length(p$steps[[2]]@module$demos, 0)
  expect_length(p2$steps[[2]]@module$demos, 1)
})

test_that("BootstrapFewShot compiles pipelines jointly with per-step demos", {
  p <- make_qa_pipeline()
  trainset <- qa_trainset()

  tp <- BootstrapFewShot(
    metric = qa_metric(),
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 2L,
    seed = 42L
  )

  compiled <- compile(p, tp, trainset)

  expect_true(inherits(compiled, "PipelineModule"))
  expect_true(compiled$config$compiled)
  expect_equal(optimization_result(compiled)$optimizer, "BootstrapFewShot")
  expect_true(
    optimization_result(compiled)$extensions$bootstrap_few_shot$joint_pipeline
  )

  # Every demo-capable step received bootstrapped demos from passing traces
  demos1 <- compiled$steps[[1]]@module$demos
  demos2 <- compiled$steps[[2]]@module$demos
  expect_length(demos1, 2)
  expect_length(demos2, 2)

  # Step demos reflect that step's own inputs/outputs from the trace
  expect_true("question" %in% names(demos1[[1]]$inputs))
  expect_equal(names(demos1[[1]]$output), "draft")
  expect_true("draft" %in% names(demos2[[1]]$inputs))
  expect_equal(names(demos2[[1]]$output), "answer")
  expect_equal(demos1[[1]]$source, "bootstrapped")
  expect_equal(demos1[[1]]$score, 1)

  # The original pipeline is untouched
  expect_length(p$steps[[1]]@module$demos, 0)
  expect_length(p$steps[[2]]@module$demos, 0)
})

test_that("BootstrapFewShot preserves registry-backed pipeline identity", {
  runtime_hook <- function(value) value
  first <- module(
    signature("question -> draft"),
    config = list(runtime_hook = runtime_hook)
  )
  second <- module(signature("draft -> answer"))
  program <- pipeline(first, second)
  program_artifact_id(
    program,
    registry = list(runtime_hook_v1 = runtime_hook)
  )
  optimizer <- BootstrapFewShot(
    metric = function(...) 1,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 0L
  )

  compiled <- dsprrr:::compile_bootstrap_pipeline(
    optimizer,
    program,
    data.frame(question = "q", answer = "a"),
    control = dsprrr:::optimizer_control(progress = FALSE)
  )

  expect_match(program_artifact_id(compiled), "^sha256:[0-9a-f]{64}$")
  expect_named(
    dsprrr:::artifact_detached_runtime(compiled)$registry,
    "runtime_hook_v1"
  )
})

test_that("joint pipeline compilation respects metric threshold", {
  p <- make_qa_pipeline()
  trainset <- qa_trainset()

  # Metric that always fails: no traces pass, no demos harvested
  failing_metric <- function(prediction, expected) 0
  attr(failing_metric, "field") <- "answer"

  tp <- BootstrapFewShot(
    metric = failing_metric,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 2L,
    seed = 42L
  )

  compiled <- compile(p, tp, trainset)

  expect_true(compiled$config$compiled)
  expect_length(compiled$steps[[1]]@module$demos, 0)
  expect_length(compiled$steps[[2]]@module$demos, 0)
  expect_equal(
    optimization_result(compiled)$best_params$n_bootstrapped_demos,
    0L
  )
  expect_equal(optimization_result(compiled)$budget$attempts, 4L)
  expect_equal(optimization_result(compiled)$budget$total_errors, 0L)
  expect_equal(optimization_result(compiled)$budget$successes, 4L)
  expect_false(optimization_result(compiled)$budget$stopped)
})

test_that("joint pipeline compilation skips labeled demos when fields absent", {
  p <- make_qa_pipeline()
  trainset <- qa_trainset()

  # Final step needs 'draft', which is not a trainset column, so labeled
  # demos cannot be applied even though max_labeled_demos > 0
  tp <- BootstrapFewShot(
    metric = qa_metric(),
    max_labeled_demos = 2L,
    max_bootstrapped_demos = 1L,
    seed = 42L
  )

  compiled <- compile(p, tp, trainset)

  expect_equal(optimization_result(compiled)$best_params$n_labeled_demos, 0L)
  # All trainset rows remained available for bootstrapping
  expect_length(compiled$steps[[2]]@module$demos, 1)
  expect_equal(compiled$steps[[2]]@module$demos[[1]]$source, "bootstrapped")
})

test_that("joint pipeline compilation works with built-in field-aware metrics", {
  p <- make_qa_pipeline()
  trainset <- qa_trainset()

  tp <- BootstrapFewShot(
    metric = metric_exact_match(field = "answer"),
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 2L,
    seed = 42L
  )

  compiled <- compile(p, tp, trainset)

  expect_length(compiled$steps[[1]]@module$demos, 2)
  expect_length(compiled$steps[[2]]@module$demos, 2)
  expect_equal(compiled$steps[[1]]@module$demos[[1]]$source, "bootstrapped")
})

test_that("joint pipeline compilation does not accumulate teacher traces", {
  p <- make_qa_pipeline()
  trainset <- qa_trainset()

  tp <- BootstrapFewShot(
    metric = qa_metric(),
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 4L,
    seed = 42L
  )

  compiled <- compile(p, tp, trainset)

  expect_equal(optimization_result(compiled)$budget$attempts, 4L)
  # Traces are cleared after each bootstrap attempt, so neither the original
  # pipeline nor the compiled student retains them
  expect_length(p$state$traces, 0)
  expect_length(compiled$state$traces, 0)
})

test_that("joint pipeline compilation works with feedback metrics", {
  p <- make_qa_pipeline()
  trainset <- qa_trainset()

  metric <- metric_with_feedback(
    function(prediction, expected) {
      pred <- if (is.list(prediction)) prediction$answer else prediction
      exp <- if (is.list(expected)) expected$answer else expected
      ok <- identical(as.character(pred), as.character(exp))
      list(
        score = as.numeric(ok),
        feedback = if (ok) "Correct." else "Wrong."
      )
    },
    field = "answer"
  )

  tp <- BootstrapFewShot(
    metric = metric,
    max_labeled_demos = 0L,
    max_bootstrapped_demos = 2L,
    seed = 42L
  )

  compiled <- compile(p, tp, trainset)

  expect_length(compiled$steps[[1]]@module$demos, 2)
  expect_equal(compiled$steps[[1]]@module$demos[[1]]$score, 1)
})
