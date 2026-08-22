test_that("Teleprompter base class can be created", {
  tp <- Teleprompter()
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_null(tp@metric)
  expect_null(tp@metric_threshold)
  expect_equal(tp@max_errors, 5L)

  # With metric
  metric_fn <- function(pred, exp) pred == exp
  tp_metric <- Teleprompter(metric = metric_fn, metric_threshold = 0.8)
  expect_identical(tp_metric@metric, metric_fn)
  expect_equal(tp_metric@metric_threshold, 0.8)
})

test_that("Teleprompter validates properties", {
  # Invalid metric
  expect_error(
    Teleprompter(metric = "not a function"),
    "metric must be a function"
  )

  # Invalid threshold
  expect_error(
    Teleprompter(metric_threshold = 1.5),
    "between 0 and 1"
  )
  expect_error(
    Teleprompter(metric_threshold = c(0.5, 0.8)),
    "single numeric value"
  )

  # Invalid max_errors
  expect_error(
    Teleprompter(max_errors = -1L),
    "non-negative"
  )
})

test_that("LabeledFewShot can be created and validated", {
  tp <- LabeledFewShot()
  expect_s3_class(tp, "dsprrr::LabeledFewShot")
  expect_s3_class(tp, "dsprrr::Teleprompter") # Inherits from Teleprompter
  expect_equal(tp@k, 4L)
  expect_true(tp@sample)
  expect_equal(tp@seed, 123L)

  # Custom parameters
  tp_custom <- LabeledFewShot(k = 8L, sample = FALSE, seed = 42L)
  expect_equal(tp_custom@k, 8L)
  expect_false(tp_custom@sample)
  expect_equal(tp_custom@seed, 42L)

  # Validation
  expect_error(LabeledFewShot(k = -1L), "non-negative")
  expect_error(LabeledFewShot(sample = c(TRUE, FALSE)), "single logical")
})

test_that("LabeledFewShot compile works", {
  # Create a simple module
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Classify text"
  )
  mod <- module(signature = sig, type = "predict")

  # Create training data
  trainset <- data.frame(
    text = c("hello", "world", "test", "data"),
    label = c("greeting", "noun", "noun", "noun"),
    stringsAsFactors = FALSE
  )

  # Compile with LabeledFewShot
  tp <- LabeledFewShot(k = 2L, seed = 42L)
  optimized <- compile(tp, mod, trainset)

  expect_true(inherits(optimized, "Module"))
  expect_length(optimized$demos, 2)
  expect_true(optimized$config$compiled)
  expect_equal(optimized$config$teleprompter, "LabeledFewShot")
  expect_equal(optimized$config$compilation_k, 2L)

  # Check demos structure
  demo1 <- optimized$demos[[1]]
  expect_true(is.list(demo1))
  expect_true("inputs" %in% names(demo1))
  expect_true("output" %in% names(demo1))
  expect_equal(
    demo1$inputs$text,
    trainset$text[demo1$inputs$text %in% trainset$text][1]
  )

  # Empty trainset
  empty_trainset <- data.frame(text = character(), label = character())
  optimized_empty <- expect_warning(
    compile(tp, mod, empty_trainset),
    "Empty trainset provided"
  )
  expect_length(optimized_empty$demos, 0)

  # No sampling
  tp_no_sample <- LabeledFewShot(k = 2L, sample = FALSE)
  optimized_no_sample <- compile(tp_no_sample, mod, trainset)
  expect_equal(optimized_no_sample$demos[[1]]$inputs$text, "hello")
  expect_equal(optimized_no_sample$demos[[2]]$inputs$text, "world")
})

test_that("LabeledFewShot rejects root labels for nested predictors", {
  runner <- list(
    execute = function(code, context = list(), ...) {
      list(success = TRUE, result = NULL)
    },
    policy = function() {
      list(backend = "test", trust = "test-only", sandboxed = TRUE)
    }
  )
  program <- rlm_module("question -> answer", runner = runner)
  trainset <- data.frame(question = "What is 2 + 2?", answer = "4")

  expect_error(
    compile(LabeledFewShot(k = 1L), program, trainset),
    class = "dsprrr_labeled_graph_unsupported"
  )
  expect_length(program$generate_action$demos, 0L)
  expect_length(program$extract$demos, 0L)
})

test_that("GridSearchTeleprompter can be created", {
  variants <- data.frame(
    id = c("v1", "v2"),
    instructions = c("Be brief", "Be detailed"),
    stringsAsFactors = FALSE
  )

  tp <- GridSearchTeleprompter(
    variants = variants,
    metric = metric_exact_match()
  )

  expect_s3_class(tp, "dsprrr::GridSearchTeleprompter")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(nrow(tp@variants), 2)
  expect_equal(tp@k, 2L)
  expect_equal(tp@eval_sample_size, 50L)
  expect_true(tp@verbose)

  # Custom parameters
  tp_custom <- GridSearchTeleprompter(
    variants = variants,
    metric = metric_f1(),
    k = 3L,
    eval_sample_size = 100L,
    verbose = FALSE
  )
  expect_equal(tp_custom@k, 3L)
  expect_equal(tp_custom@eval_sample_size, 100L)
  expect_false(tp_custom@verbose)
})

test_that("GridSearchTeleprompter validates variants", {
  # Missing id column
  bad_variants <- data.frame(
    name = c("v1", "v2"),
    instructions = c("a", "b")
  )
  expect_error(
    GridSearchTeleprompter(variants = bad_variants),
    "must have an 'id' column"
  )

  # Empty variants
  empty_variants <- data.frame(id = character())
  expect_error(
    GridSearchTeleprompter(variants = empty_variants),
    "at least one row"
  )

  # Not a data frame
  expect_error(
    GridSearchTeleprompter(variants = list(id = "v1")),
    "must be S3<data.frame>"
  )
})

test_that("GridSearchTeleprompter delegates to optimize_grid", {
  MockVariantModule <- R6::R6Class(
    "MockVariantModule",
    inherit = dsprrr:::PredictModule,
    public = list(
      initialize = function(
        signature,
        template = "",
        demos = list(),
        config = list()
      ) {
        super$initialize(
          signature,
          template = template,
          demos = demos,
          config = config
        )
        self$config$prompt_style <- self$config$prompt_style %||% "baseline"
      },
      forward = function(batch, .llm = NULL, trace = TRUE, ...) {
        style <- self$config$prompt_style %||% "baseline"
        prediction <- if (style == "energetic") "energetic" else "baseline"
        tibble::tibble(
          output = list(prediction),
          chat = list(NULL),
          metadata = list(list())
        )
      },
      apply_optimization_params = function(params) {
        super$apply_optimization_params(params)
        if (!is.null(params$prompt_style) && !is.na(params$prompt_style)) {
          self$config$prompt_style <- params$prompt_style
        }
        invisible(self)
      },
      deepcopy = function() {
        new_signature <- Signature(
          inputs = self$signature@inputs,
          output_type = self$signature@output_type,
          instructions = self$signature@instructions
        )

        new_module <- MockVariantModule$new(
          signature = new_signature,
          template = self$template,
          demos = lapply(self$demos, function(x) x),
          config = lapply(self$config, function(x) x)
        )

        new_module$state <- lapply(self$state, function(x) x)
        new_module
      }
    )
  )

  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Classify style"
  )

  mod <- MockVariantModule$new(signature = sig, template = "Text: {text}")

  trainset <- tibble::tibble(
    text = c("a", "b", "c", "d"),
    label = rep("energetic", 4)
  )

  variants <- tibble::tibble(
    id = c("baseline", "energetic"),
    prompt_style = c("baseline", "energetic"),
    instructions = sig@instructions
  )

  metric <- function(prediction, expected_row) {
    pred_value <- if (is.list(prediction)) prediction[[1]] else prediction
    as.numeric(pred_value == expected_row$label)
  }

  mock_llm <- new_test_chat(chat_structured = function(...) list())

  tp <- GridSearchTeleprompter(
    variants = variants,
    metric = metric,
    k = 0L,
    eval_sample_size = 2L,
    verbose = FALSE
  )

  optimized <- compile(tp, mod, trainset, .llm = mock_llm)

  expect_true(inherits(optimized, "Module"))
  expect_true(inherits(optimized, "PredictModule"))
  expect_equal(optimized$config$teleprompter, "GridSearchTeleprompter")
  expect_equal(nrow(optimized$state$trials), nrow(variants))
  expect_named(optimized$config$all_scores, variants$id)
  expect_false(anyNA(optimized$config$all_scores))
})

test_that("Module state management methods work", {
  # Create a compiled module with demos
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )
  module <- module(
    signature = sig,
    template = "{text}",
    demos = list(list(inputs = list(text = "demo"), output = "result")),
    config = list(compiled = TRUE, teleprompter = "test"),
    type = "predict"
  )
  # Manually set compiled state for testing
  module$state$compiled <- TRUE

  # Test reset_copy
  reset <- module$reset_copy()
  expect_true(inherits(reset, "Module"))
  expect_length(reset$demos, 0)
  expect_length(reset$config, 0)
  expect_equal(reset$template, module$template)
  expect_equal(reset$signature@instructions, module$signature@instructions)

  # Test deepcopy
  copy <- module$deepcopy()
  expect_true(inherits(copy, "Module"))
  expect_equal(copy$demos, module$demos)
  expect_equal(copy$config, module$config)
  expect_equal(copy$template, module$template)

  # Verify deep copy is independent
  copy$demos[[1]]$output <- "modified"
  expect_equal(module$demos[[1]]$output, "result") # Original unchanged

  # Test is_compiled
  expect_true(module$is_compiled())
  expect_false(reset$is_compiled())
  expect_true(copy$is_compiled())
})

test_that("compile generic dispatches correctly", {
  sig <- Signature(
    inputs = list(input(name = "x", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )
  mod <- module(signature = sig, type = "predict")
  trainset <- data.frame(x = "test", y = "result")

  # Base Teleprompter should error
  tp_base <- Teleprompter()
  expect_error(
    compile(tp_base, mod, trainset),
    "compile\\(\\) method not implemented"
  )

  # LabeledFewShot should work
  tp_labeled <- LabeledFewShot(k = 1L)
  result <- compile(tp_labeled, mod, trainset)
  expect_true(inherits(result, "Module"))

  # GridSearchTeleprompter requires metric
  variants <- data.frame(id = "v1", instructions = "test")
  tp_grid <- GridSearchTeleprompter(variants = variants)
  expect_error(
    compile(tp_grid, mod, trainset),
    "requires a metric"
  )
})

test_that("format_trainset_as_demos handles various formats", {
  sig <- Signature(
    inputs = list(
      input(name = "text", type = "string"),
      input(name = "context", type = "string")
    ),
    output_type = ellmer::type_string(),
    instructions = ""
  )

  # Standard format with matching columns
  trainset <- data.frame(
    text = c("q1", "q2"),
    context = c("c1", "c2"),
    answer = c("a1", "a2")
  )
  demos <- dsprrr:::format_trainset_as_demos(trainset, sig)
  expect_length(demos, 2)
  expect_equal(demos[[1]]$inputs$text, "q1")
  expect_equal(demos[[1]]$inputs$context, "c1")
  expect_equal(demos[[1]]$output, "a1")

  # Missing input column
  trainset_missing <- data.frame(
    text = c("q1", "q2"),
    output = c("a1", "a2")
  )
  demos_missing <- dsprrr:::format_trainset_as_demos(trainset_missing, sig)
  expect_equal(demos_missing[[1]]$inputs$text, "q1")
  expect_null(demos_missing[[1]]$inputs$context)

  # Multiple potential output columns (use non-standard column names)
  trainset_multi <- data.frame(
    text = c("q1"),
    context = c("c1"),
    prediction = c("p1"),
    other = c("o1")
  )
  expect_warning(
    demos_multi <- dsprrr:::format_trainset_as_demos(trainset_multi, sig),
    "Multiple potential output columns"
  )
  expect_equal(demos_multi[[1]]$output, "p1")
})

test_that("get_metric_field extracts field attribute from metrics", {
  # metric_exact_match stores field as attribute
  metric_with_field <- metric_exact_match(field = "sentiment")
  expect_equal(dsprrr:::get_metric_field(metric_with_field), "sentiment")

  # metric without field returns NULL
  metric_no_field <- metric_exact_match()
  expect_null(dsprrr:::get_metric_field(metric_no_field))

  # NULL metric returns NULL
  expect_null(dsprrr:::get_metric_field(NULL))

  # metric_f1 also stores field attribute
  metric_f1_with_field <- metric_f1(field = "answer")
  expect_equal(dsprrr:::get_metric_field(metric_f1_with_field), "answer")

  # metric_contains also stores field attribute
  metric_contains_with_field <- metric_contains("test", field = "response")
  expect_equal(
    dsprrr:::get_metric_field(metric_contains_with_field),
    "response"
  )
})

test_that("format_trainset_as_demos uses explicit output_col parameter", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = ""
  )

  # Trainset with non-standard output column name
  trainset <- data.frame(
    id = 1:2,
    text = c("hello", "world"),
    classification = c("positive", "negative"),
    stringsAsFactors = FALSE
  )

  # Without output_col, would warn about multiple columns
  # With explicit output_col, should use that column
  demos <- dsprrr:::format_trainset_as_demos(
    trainset,
    sig,
    output_col = "classification"
  )
  expect_length(demos, 2)
  expect_equal(demos[[1]]$output, "positive")
  expect_equal(demos[[2]]$output, "negative")
})

test_that("LabeledFewShot uses metric field for output column", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Classify text"
  )
  mod <- module(signature = sig, type = "predict")

  # Trainset with non-standard column name "classification"
  trainset <- data.frame(
    id = 1:4,
    text = c("great", "terrible", "ok", "amazing"),
    classification = c("positive", "negative", "neutral", "positive"),
    stringsAsFactors = FALSE
  )

  # Without metric field, would warn about multiple columns or use wrong one
  # With metric field = "classification", should use that column
  tp <- LabeledFewShot(
    k = 2L,
    seed = 42L,
    metric = metric_exact_match(field = "classification")
  )

  # Should not warn about multiple output columns
  optimized <- compile(tp, mod, trainset)

  expect_true(inherits(optimized, "Module"))
  expect_length(optimized$demos, 2)

  # Verify demos use the classification column
  demo_outputs <- vapply(optimized$demos, function(d) d$output, character(1))
  expect_true(all(demo_outputs %in% trainset$classification))
})

test_that("format_trainset_as_demos extracts nested field from list column", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = ""
  )

  # Trainset with nested output in list column
  trainset <- tibble::tibble(
    text = c("hello", "world"),
    output = list(
      list(classification = "positive", confidence = 0.9),
      list(classification = "negative", confidence = 0.8)
    )
  )

  # Extract the nested "classification" field
  demos <- dsprrr:::format_trainset_as_demos(
    trainset,
    sig,
    output_col = "classification"
  )

  expect_length(demos, 2)
  expect_equal(demos[[1]]$output, "positive")
  expect_equal(demos[[2]]$output, "negative")
})

test_that("format_trainset_as_demos unwraps list column when field is column name", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = ""
  )

  # Trainset with nested output in list column
  trainset <- tibble::tibble(
    text = c("hello", "world"),
    output = list(
      list(classification = "positive", confidence = 0.9),
      list(classification = "negative", confidence = 0.8)
    )
  )

  # When field = "output" (the column name), should return unwrapped list
  demos <- dsprrr:::format_trainset_as_demos(
    trainset,
    sig,
    output_col = "output"
  )

  expect_length(demos, 2)
  # Should be unwrapped - direct access to classification, not output[[1]]$classification
  expect_equal(demos[[1]]$output$classification, "positive")
  expect_equal(demos[[1]]$output$confidence, 0.9)
  expect_equal(demos[[2]]$output$classification, "negative")
  expect_equal(demos[[2]]$output$confidence, 0.8)
})

test_that("format_trainset_as_demos handles multiple fields", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = ""
  )

  # Trainset with nested output containing multiple fields
  trainset <- tibble::tibble(
    text = c("hello", "world"),
    output = list(
      list(classification = "positive", confidence = 0.9, extra = "foo"),
      list(classification = "negative", confidence = 0.8, extra = "bar")
    )
  )

  # Extract multiple specific fields
  demos <- dsprrr:::format_trainset_as_demos(
    trainset,
    sig,
    output_col = c("classification", "confidence")
  )

  expect_length(demos, 2)
  # Should return named list with only the specified fields
  expect_equal(demos[[1]]$output$classification, "positive")
  expect_equal(demos[[1]]$output$confidence, 0.9)
  expect_null(demos[[1]]$output$extra) # extra should not be included
  expect_equal(demos[[2]]$output$classification, "negative")
  expect_equal(demos[[2]]$output$confidence, 0.8)
  expect_null(demos[[2]]$output$extra)
})

test_that("detect_output_source handles various trainset formats", {
  input_names <- "text"

  # Case 1: Direct column match
  trainset1 <- data.frame(text = "a", classification = "pos")
  result1 <- dsprrr:::detect_output_source(
    trainset1,
    "classification",
    input_names
  )
  expect_equal(result1$type, "column")
  expect_equal(result1$name, "classification")

  # Case 2: Nested field in list column
  trainset2 <- tibble::tibble(
    text = "a",
    output = list(list(sentiment = "positive"))
  )
  result2 <- dsprrr:::detect_output_source(trainset2, "sentiment", input_names)
  expect_equal(result2$type, "nested")
  expect_equal(result2$column, "output")
  expect_equal(result2$field, "sentiment")

  # Case 3: Field not found
  trainset3 <- data.frame(text = "a", other = "b")
  result3 <- expect_test_warnings(
    dsprrr:::detect_output_source(
      trainset3,
      "nonexistent",
      input_names
    ),
    "Could not find output field"
  )
  expect_equal(result3$type, "not_found")

  # Case 4: No field specified, falls back to common names
  trainset4 <- data.frame(text = "a", label = "pos")
  result4 <- dsprrr:::detect_output_source(trainset4, NULL, input_names)
  expect_equal(result4$type, "column")
  expect_equal(result4$name, "label")

  # Case 5: No field specified, no common names, uses first non-input
  trainset5 <- data.frame(text = "a", foo = "bar")
  result5 <- dsprrr:::detect_output_source(trainset5, NULL, input_names)
  expect_equal(result5$type, "column")
  expect_equal(result5$name, "foo")

  # Case 6: Multiple fields in nested list column
  trainset6 <- tibble::tibble(
    text = "a",
    output = list(list(classification = "pos", confidence = 0.9))
  )
  result6 <- dsprrr:::detect_output_source(
    trainset6,
    c("classification", "confidence"),
    input_names
  )
  expect_equal(result6$type, "multi")
  expect_equal(result6$column, "output")
  expect_equal(result6$fields, c("classification", "confidence"))

  # Case 7: Multiple fields not found (one missing)
  trainset7 <- tibble::tibble(
    text = "a",
    output = list(list(classification = "pos")) # missing confidence
  )
  result7 <- expect_test_warnings(
    dsprrr:::detect_output_source(
      trainset7,
      c("classification", "confidence"),
      input_names
    ),
    "Could not find all requested fields"
  )
  expect_equal(result7$type, "not_found")
})

test_that("get_metric_field warns for non-function input", {
  # Passing wrong type should warn and return NULL
  expect_warning(
    result <- dsprrr:::get_metric_field("not a function"),
    "Expected metric to be a function"
  )
  expect_null(result)

  # List should also warn
  expect_warning(
    result2 <- dsprrr:::get_metric_field(list(field = "test")),
    "Expected metric to be a function"
  )
  expect_null(result2)
})

test_that("format_trainset_as_demos validates output_col type", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = ""
  )

  trainset <- data.frame(text = "hello", output = "positive")

  # Passing numeric should error

  expect_error(
    dsprrr:::format_trainset_as_demos(trainset, sig, output_col = 1),
    "must be a character vector or NULL"
  )

  # Passing list should error
  expect_error(
    dsprrr:::format_trainset_as_demos(
      trainset,
      sig,
      output_col = list("output")
    ),
    "must be a character vector or NULL"
  )
})

test_that("detect_output_source warns when field not found", {
  input_names <- "text"

  # Single field not found should warn
  trainset <- data.frame(text = "a", other = "b")
  expect_warning(
    result <- dsprrr:::detect_output_source(
      trainset,
      "nonexistent",
      input_names
    ),
    "Could not find output field"
  )
  expect_equal(result$type, "not_found")
})

test_that("detect_output_source warns when multiple fields not found", {
  input_names <- "text"

  # Multiple fields not all found should warn
  trainset <- tibble::tibble(
    text = "a",
    output = list(list(classification = "pos"))
  )
  expect_warning(
    result <- dsprrr:::detect_output_source(
      trainset,
      c("classification", "missing_field"),
      input_names
    ),
    "Could not find all requested fields"
  )
  expect_equal(result$type, "not_found")
})

test_that("detect_output_source warns when no output column found", {
  # Trainset with only input column
  trainset <- data.frame(text = "a")
  expect_warning(
    result <- dsprrr:::detect_output_source(trainset, NULL, "text"),
    "No output column found"
  )
  expect_equal(result$type, "none")
})
