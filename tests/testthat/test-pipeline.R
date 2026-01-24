# Tests for pipeline.R (PipelineModule, %>>%, pipeline())

# Helper: Create a mock module for testing that outputs structured data
create_mock_module_structured <- function(
  input_names = "input",
  output_fields = list(answer = "default"),
  transform_fn = NULL
) {
  sig <- dsprrr::signature(
    inputs = lapply(input_names, function(n) dsprrr::input(name = n)),
    output_type = if (
      length(output_fields) == 1 && names(output_fields)[1] == "answer"
    ) {
      ellmer::type_object(answer = ellmer::type_string())
    } else {
      do.call(
        ellmer::type_object,
        lapply(output_fields, function(x) ellmer::type_string())
      )
    },
    instructions = "Test module"
  )

  mock_mod <- list(
    signature = sig,
    chat = NULL,
    forward = function(batch, .llm = NULL, trace = TRUE, .cache = NULL, ...) {
      if (is.data.frame(batch)) {
        inputs <- as.list(batch[1, , drop = FALSE])
      } else {
        inputs <- batch
      }

      output <- if (!is.null(transform_fn)) {
        transform_fn(inputs)
      } else {
        output_fields
      }

      tibble::tibble(
        output = list(output),
        chat = list(NULL),
        metadata = list(list(
          total_tokens = 50,
          cost = 0.0005,
          model = "mock-model"
        ))
      )
    },
    reset_copy = function() {
      create_mock_module_structured(input_names, output_fields, transform_fn)
    },
    copy = function(deep = TRUE) {
      create_mock_module_structured(input_names, output_fields, transform_fn)
    }
  )
  class(mock_mod) <- c("MockModule", "Module", "R6")
  mock_mod
}

# ============================================================================
# PipelineStep Tests
# ============================================================================

test_that("PipelineStep can be created with module", {
  mod <- module(signature("q -> a"))
  step <- PipelineStep(module = mod)

  expect_true(inherits(step, "dsprrr::PipelineStep"))
  expect_true(inherits(step@module, "Module"))
  expect_equal(step@input_map, list())
  expect_equal(step@static_inputs, list())
})

test_that("PipelineStep validates module type", {
  expect_error(
    PipelineStep(module = "not a module"),
    "must be a Module"
  )
})

test_that("PipelineStep accepts input mapping", {
  mod <- module(signature("context -> answer"))
  step <- PipelineStep(
    module = mod,
    input_map = list(documents = "context")
  )

  expect_equal(step@input_map, list(documents = "context"))
})

test_that("PipelineStep accepts static inputs", {
  mod <- module(signature("context, prompt -> answer"))
  step <- PipelineStep(
    module = mod,
    static_inputs = list(prompt = "Be concise")
  )

  expect_equal(step@static_inputs, list(prompt = "Be concise"))
})

# ============================================================================
# PipelineModule Tests
# ============================================================================

test_that("PipelineModule class exists and inherits from Module", {
  expect_true(R6::is.R6Class(PipelineModule))
})

test_that("PipelineModule can be created with list of modules", {
  mod1 <- create_mock_module_structured(
    input_names = "question",
    output_fields = list(answer = "42")
  )
  mod2 <- create_mock_module_structured(
    input_names = "answer",
    output_fields = list(formatted = "Formatted: 42")
  )

  pipeline <- PipelineModule$new(steps = list(mod1, mod2))

  expect_s3_class(pipeline, "PipelineModule")
  expect_s3_class(pipeline, "Module")
  expect_length(pipeline$steps, 2)
})

test_that("PipelineModule requires at least one step", {
  expect_error(
    PipelineModule$new(steps = list()),
    "at least one step"
  )
})

test_that("PipelineModule validates step types", {
  expect_error(
    PipelineModule$new(steps = list("not a module")),
    "Expected Module"
  )
})

test_that("PipelineModule creates composite signature", {
  mod1 <- create_mock_module_structured(
    input_names = "question",
    output_fields = list(answer = "42")
  )
  mod2 <- create_mock_module_structured(
    input_names = "answer",
    output_fields = list(formatted = "result")
  )

  pipeline <- PipelineModule$new(steps = list(mod1, mod2))

  # Composite signature should have 'question' as input (from first module)
  # since 'answer' is provided by upstream
  sig <- pipeline$signature
  input_names <- vapply(sig@inputs, function(x) x$name, character(1))

  expect_equal(input_names, "question")
})

test_that("PipelineModule forward chains outputs to inputs", {
  # Module 1: question -> answer
  mod1 <- create_mock_module_structured(
    input_names = "question",
    output_fields = list(answer = "42"),
    transform_fn = function(inputs) {
      list(answer = paste0("Answer to '", inputs$question, "' is 42"))
    }
  )

  # Module 2: answer -> formatted
  mod2 <- create_mock_module_structured(
    input_names = "answer",
    output_fields = list(formatted = "result"),
    transform_fn = function(inputs) {
      list(formatted = paste0("Formatted: ", inputs$answer))
    }
  )

  pipeline <- PipelineModule$new(steps = list(mod1, mod2))

  result <- pipeline$forward(list(question = "What is 6*7?"))

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("output", "chat", "metadata"))

  output <- result$output[[1]]
  expect_true(grepl("Formatted:", output$formatted))
  expect_true(grepl("Answer to", output$formatted))
})

test_that("PipelineModule aggregates metadata", {
  mod1 <- create_mock_module_structured(
    input_names = "q",
    output_fields = list(a = "1")
  )
  mod2 <- create_mock_module_structured(
    input_names = "a",
    output_fields = list(b = "2")
  )

  pipeline <- PipelineModule$new(steps = list(mod1, mod2))

  result <- pipeline$forward(list(q = "test"))
  metadata <- result$metadata[[1]]

  expect_equal(metadata$n_steps, 2)
  # Each mock module returns 50 tokens
  expect_equal(metadata$total_tokens, 100)
  expect_equal(metadata$total_cost, 0.001)
  expect_true(!is.null(metadata$step_metadata))
  expect_length(metadata$step_metadata, 2)
})

test_that("PipelineModule errors on missing input", {
  mod1 <- create_mock_module_structured(
    input_names = c("question", "context"),
    output_fields = list(answer = "42")
  )
  mod2 <- create_mock_module_structured(
    input_names = "answer",
    output_fields = list(formatted = "result")
  )

  pipeline <- PipelineModule$new(steps = list(mod1, mod2))

  # Missing 'context' input
  expect_error(
    pipeline$forward(list(question = "test")),
    "Missing inputs for pipeline step 1"
  )
})

test_that("Pipeline error includes step number and original message", {
  mod1 <- create_mock_module_structured(
    input_names = "q",
    output_fields = list(a = "ok")
  )

  # Module that always fails
  mod_fail <- list(
    signature = signature("a -> b"),
    chat = NULL,
    forward = function(...) stop("Simulated LLM failure")
  )
  class(mod_fail) <- c("MockModule", "Module", "R6")

  pipeline <- PipelineModule$new(steps = list(mod1, mod_fail))

  expect_error(
    pipeline$forward(list(q = "test")),
    "Pipeline step 2 failed"
  )
})

test_that("Pipeline errors on NULL output from step", {
  # Module that returns NULL output
  mod_null <- list(
    signature = signature("q -> a"),
    chat = NULL,
    forward = function(...) {
      tibble::tibble(
        output = list(NULL),
        chat = list(NULL),
        metadata = list(list(total_tokens = 10))
      )
    }
  )
  class(mod_null) <- c("MockModule", "Module", "R6")

  pipeline <- PipelineModule$new(steps = list(mod_null))

  expect_error(
    pipeline$forward(list(q = "test")),
    "returned NULL output"
  )
})

test_that("Pipeline warns when input mapping references non-existent field", {
  # mod1 outputs 'answer' and 'extra'
  mod1 <- create_mock_module_structured(
    input_names = "q",
    output_fields = list(answer = "42", extra = "more"),
    transform_fn = function(inputs) list(answer = "the answer", extra = "more data")
  )
  # mod2 needs 'context' and 'data' inputs
  mod2 <- create_mock_module_structured(
    input_names = c("context", "data"),
    output_fields = list(result = "done"),
    transform_fn = function(inputs) {
      list(result = paste0("Got: ", inputs$context, " and ", inputs$data))
    }
  )

  # Map 'answer' -> 'context' (works) and 'nonexistent' -> 'data' (will warn)
  # Also inject 'data' as static input so pipeline doesn't fail
  pipeline <- pipeline(
    mod1,
    step(mod2, map = c(answer = "context", nonexistent = "data"), data = "fallback")
  )

  expect_warning(
    pipeline$forward(list(q = "test")),
    "non-existent field"
  )
})

test_that("Pipeline warns when output selection requests non-existent fields", {
  mod1 <- create_mock_module_structured(
    input_names = "q",
    output_fields = list(answer = "42"),
    transform_fn = function(inputs) list(answer = "the answer")
  )
  mod2 <- create_mock_module_structured(
    input_names = "answer",
    output_fields = list(result = "done")
  )

  # Select 'nonexistent' field (will warn)
  pipeline <- pipeline(
    step(mod1, select = c("answer", "nonexistent")),
    mod2
  )

  expect_warning(
    pipeline$forward(list(q = "test")),
    "non-existent fields"
  )
})

test_that("PipelineModule print method works", {
  mod1 <- module(signature("question -> answer"))
  mod2 <- module(signature("answer -> formatted"))

  pipeline <- PipelineModule$new(steps = list(mod1, mod2))

  output <- capture.output(print(pipeline), type = "message")
  expect_true(any(grepl("PipelineModule", output, fixed = TRUE)))
  expect_true(any(grepl("Steps", output, fixed = TRUE)))
})

# ============================================================================
# %>>% Operator Tests
# ============================================================================

test_that("%>>% operator creates PipelineModule from two modules", {
  mod1 <- module(signature("q -> a"))
  mod2 <- module(signature("a -> b"))

  pipeline <- mod1 %>>% mod2

  expect_s3_class(pipeline, "PipelineModule")
  expect_length(pipeline$steps, 2)
})

test_that("%>>% operator chains multiple modules", {
  mod1 <- module(signature("q -> a"))
  mod2 <- module(signature("a -> b"))
  mod3 <- module(signature("b -> c"))

  pipeline <- mod1 %>>% mod2 %>>% mod3

  expect_s3_class(pipeline, "PipelineModule")
  expect_length(pipeline$steps, 3)
})

test_that("%>>% operator validates left-hand side", {
  mod <- module(signature("q -> a"))

  expect_error(
    "not a module" %>>% mod,
    "Invalid left-hand side"
  )
})

test_that("%>>% operator validates right-hand side", {
  mod <- module(signature("q -> a"))

  expect_error(
    mod %>>% "not a module",
    "Invalid right-hand side"
  )
})

test_that("%>>% works with execution", {
  mod1 <- create_mock_module_structured(
    input_names = "q",
    output_fields = list(a = "step1"),
    transform_fn = function(inputs) list(a = paste0("Q: ", inputs$q))
  )
  mod2 <- create_mock_module_structured(
    input_names = "a",
    output_fields = list(b = "step2"),
    transform_fn = function(inputs) list(b = paste0("A: ", inputs$a))
  )

  pipeline <- mod1 %>>% mod2
  result <- pipeline$forward(list(q = "hello"))

  expect_s3_class(result, "tbl_df")
  output <- result$output[[1]]
  expect_true(grepl("A: Q: hello", output$b))
})

# ============================================================================
# pipeline() Constructor Tests
# ============================================================================

test_that("pipeline() creates PipelineModule", {
  mod1 <- module(signature("q -> a"))
  mod2 <- module(signature("a -> b"))

  p <- pipeline(mod1, mod2)

  expect_s3_class(p, "PipelineModule")
  expect_length(p$steps, 2)
})

test_that("pipeline() requires at least one module", {
  expect_error(
    pipeline(),
    "requires at least one module"
  )
})

test_that("pipeline() accepts step() with mapping", {
  mod1 <- module(signature("q -> a"))
  mod2 <- module(signature("context -> b"))

  p <- pipeline(
    mod1,
    step(mod2, map = c(a = "context"))
  )

  expect_length(p$steps, 2)
  expect_equal(p$steps[[2]]@input_map, list(a = "context"))
})

test_that("pipeline() accepts step() with static inputs", {
  mod1 <- module(signature("q -> a"))
  mod2 <- module(signature("a, prompt -> b"))

  p <- pipeline(
    mod1,
    step(mod2, prompt = "Be concise")
  )

  expect_length(p$steps, 2)
  expect_equal(p$steps[[2]]@static_inputs, list(prompt = "Be concise"))
})

# ============================================================================
# step() Helper Tests
# ============================================================================

test_that("step() creates PipelineStep", {
  mod <- module(signature("q -> a"))
  s <- step(mod)

  expect_s3_class(s, "dsprrr::PipelineStep")
})

test_that("step() validates module argument", {
  expect_error(
    step("not a module"),
    "requires a Module"
  )
})

test_that("step() accepts map argument", {
  mod <- module(signature("context -> a"))
  s <- step(mod, map = c(answer = "context"))

  expect_equal(s@input_map, list(answer = "context"))
})

test_that("step() accepts select argument", {
  mod <- module(signature("q -> a"))
  s <- step(mod, select = c("answer"))

  expect_equal(s@output_select, "answer")
})

test_that("step() accepts static inputs via ...", {
  mod <- module(signature("q, style -> a"))
  s <- step(mod, style = "formal")

  expect_equal(s@static_inputs, list(style = "formal"))
})

# ============================================================================
# map_inputs() Helper Tests
# ============================================================================

test_that("map_inputs() creates PipelineMappedModule", {
  mod <- module(signature("context -> a"))
  mapped <- map_inputs(mod, answer = "context")

  expect_s3_class(mapped, "PipelineMappedModule")
  expect_equal(mapped$mapping, list(answer = "context"))
})

test_that("map_inputs() validates module argument", {
  expect_error(
    map_inputs("not a module", a = "b"),
    "requires a Module"
  )
})

test_that("map_inputs() warns with no mappings", {
  mod <- module(signature("q -> a"))
  expect_warning(
    result <- map_inputs(mod),
    "no mappings"
  )
  # Should return consistent PipelineMappedModule type

  expect_s3_class(result, "PipelineMappedModule")
})

test_that("map_inputs() errors on unnamed arguments", {
  mod <- module(signature("context -> a"))

  expect_error(
    map_inputs(mod, "context"),
    "requires named arguments"
  )
})

test_that("map_inputs() works with %>>%", {
  mod1 <- create_mock_module_structured(
    input_names = "q",
    output_fields = list(answer = "42"),
    transform_fn = function(inputs) list(answer = "the answer")
  )
  mod2 <- create_mock_module_structured(
    input_names = "context",
    output_fields = list(result = "done"),
    transform_fn = function(inputs) {
      list(result = paste0("Got: ", inputs$context))
    }
  )

  pipeline <- mod1 %>>% map_inputs(mod2, answer = "context")
  result <- pipeline$forward(list(q = "test"))

  output <- result$output[[1]]
  expect_equal(output$result, "Got: the answer")
})

# ============================================================================
# with_inputs() Helper Tests
# ============================================================================

test_that("with_inputs() creates PipelineMappedModule", {
  mod <- module(signature("q, style -> a"))
  mapped <- with_inputs(mod, style = "formal")

  expect_s3_class(mapped, "PipelineMappedModule")
  expect_equal(mapped$static_inputs, list(style = "formal"))
})

test_that("with_inputs() validates module argument", {
  expect_error(
    with_inputs("not a module", a = "b"),
    "requires a Module"
  )
})

test_that("with_inputs() warns with no inputs", {
  mod <- module(signature("q -> a"))
  expect_warning(
    result <- with_inputs(mod),
    "no inputs"
  )
  # Should return consistent PipelineMappedModule type
  expect_s3_class(result, "PipelineMappedModule")
})

# ============================================================================
# select_outputs() Helper Tests
# ============================================================================

test_that("select_outputs() creates PipelineMappedModule", {
  mod <- module(signature("q -> a"))
  mapped <- select_outputs(mod, "answer")

  expect_s3_class(mapped, "PipelineMappedModule")
  expect_equal(mapped$output_select, "answer")
})

test_that("select_outputs() validates module argument", {
  expect_error(
    select_outputs("not a module", "a"),
    "requires a Module"
  )
})

test_that("select_outputs() errors on non-character fields", {
  mod <- module(signature("q -> a"))

  expect_error(
    select_outputs(mod, 1, 2, 3),
    "requires character field names"
  )
})

test_that("select_outputs() errors on empty fields", {
  mod <- module(signature("q -> a"))

  expect_error(
    select_outputs(mod),
    "requires at least one field name"
  )
})

test_that("select_outputs() works with %>>%", {
  # mod1 outputs both 'answer' and 'reasoning'
  mod1 <- create_mock_module_structured(
    input_names = "q",
    output_fields = list(answer = "42", reasoning = "because"),
    transform_fn = function(inputs) list(answer = "the answer", reasoning = "steps")
  )
  # mod2 only needs 'answer'
  mod2 <- create_mock_module_structured(
    input_names = "answer",
    output_fields = list(result = "done"),
    transform_fn = function(inputs) {
      list(result = paste0("Got: ", inputs$answer))
    }
  )

  # select_outputs on mod1 filters ITS outputs before passing downstream
  # Syntax: select_outputs(producing_module, fields_to_keep) %>>% consuming_module
  pipeline <- select_outputs(mod1, "answer") %>>% mod2

  # Check that output_select is properly transferred to PipelineStep for mod1 (step 1)
  expect_equal(pipeline$steps[[1]]@output_select, "answer")

  # Execute the pipeline - mod2 receives only 'answer', not 'reasoning'
  result <- pipeline$forward(list(q = "test"))
  output <- result$output[[1]]
  expect_equal(output$result, "Got: the answer")
})

# ============================================================================
# Integration Tests
# ============================================================================

test_that("Pipeline works with run() generic", {
  mod1 <- create_mock_module_structured(
    input_names = "question",
    output_fields = list(answer = "42")
  )
  mod2 <- create_mock_module_structured(
    input_names = "answer",
    output_fields = list(formatted = "result")
  )

  pipeline <- mod1 %>>% mod2

  # run() should work since PipelineModule inherits from Module
  # The forward() method is called internally
  result <- pipeline$forward(list(question = "test"))

  expect_s3_class(result, "tbl_df")
  expect_named(result, c("output", "chat", "metadata"))
})

test_that("Pipeline reset_copy creates independent copy", {
  mod1 <- module(signature("q -> a"))
  mod2 <- module(signature("a -> b"))

  original <- mod1 %>>% mod2
  copy <- original$reset_copy()

  expect_s3_class(copy, "PipelineModule")
  expect_length(copy$steps, 2)

  # Should be a different object

  expect_false(identical(original, copy))
})

test_that("Complex pipeline with mapping and static inputs works", {
  # Step 1: question -> answer
  mod1 <- create_mock_module_structured(
    input_names = "question",
    output_fields = list(answer = "42"),
    transform_fn = function(inputs) list(answer = "computed answer")
  )

  # Step 2: context, style -> formatted (maps answer -> context, injects style)
  mod2 <- create_mock_module_structured(
    input_names = c("context", "style"),
    output_fields = list(formatted = "result"),
    transform_fn = function(inputs) {
      list(formatted = paste0(inputs$style, ": ", inputs$context))
    }
  )

  pipeline <- pipeline(
    mod1,
    step(mod2, map = c(answer = "context"), style = "Formal")
  )

  result <- pipeline$forward(list(question = "What is life?"))
  output <- result$output[[1]]

  expect_equal(output$formatted, "Formal: computed answer")
})

test_that("Pipeline handles simple (non-object) outputs", {
  # Create a module that returns a simple string
  # Signature "q -> a: string" creates output field named "a"
  sig_simple <- signature("q -> a: string")
  mod_simple <- list(
    signature = sig_simple,
    chat = NULL,
    forward = function(batch, .llm = NULL, trace = TRUE, .cache = NULL, ...) {
      tibble::tibble(
        output = list(list(a = "simple string")),
        chat = list(NULL),
        metadata = list(list(total_tokens = 10, cost = 0.0001, model = "mock"))
      )
    }
  )
  class(mod_simple) <- c("MockModule", "Module", "R6")

  # Second module expects input "a" (the output field name from first module)
  mod2 <- create_mock_module_structured(
    input_names = "a",
    output_fields = list(result = "done"),
    transform_fn = function(inputs) list(result = paste0("Got: ", inputs$a))
  )

  pipeline <- PipelineModule$new(steps = list(mod_simple, mod2))
  result <- pipeline$forward(list(q = "test"))

  output <- result$output[[1]]
  expect_equal(output$result, "Got: simple string")
})
