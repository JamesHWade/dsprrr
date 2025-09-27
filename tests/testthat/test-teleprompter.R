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
  expect_s3_class(tp, "dsprrr::Teleprompter")  # Inherits from Teleprompter
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
    inputs = list(input(name = "text", class = S7::class_character)),
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
  expect_equal(demo1$inputs$text, trainset$text[demo1$inputs$text %in% trainset$text][1])

  # Empty trainset
  empty_trainset <- data.frame(text = character(), label = character())
  optimized_empty <- compile(tp, mod, empty_trainset)
  expect_length(optimized_empty$demos, 0)

  # No sampling
  tp_no_sample <- LabeledFewShot(k = 2L, sample = FALSE)
  optimized_no_sample <- compile(tp_no_sample, mod, trainset)
  expect_equal(optimized_no_sample$demos[[1]]$inputs$text, "hello")
  expect_equal(optimized_no_sample$demos[[2]]$inputs$text, "world")
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

test_that("Module state management methods work", {
  # Create a compiled module with demos
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
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
  expect_equal(module$demos[[1]]$output, "result")  # Original unchanged

  # Test is_compiled
  expect_true(module$is_compiled())
  expect_false(reset$is_compiled())
  expect_true(copy$is_compiled())
})

test_that("compile generic dispatches correctly", {
  sig <- Signature(
    inputs = list(input(name = "x", class = S7::class_character)),
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
      input(name = "text", class = S7::class_character),
      input(name = "context", class = S7::class_character)
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
  demos <- format_trainset_as_demos(trainset, sig)
  expect_length(demos, 2)
  expect_equal(demos[[1]]$inputs$text, "q1")
  expect_equal(demos[[1]]$inputs$context, "c1")
  expect_equal(demos[[1]]$output, "a1")

  # Missing input column
  trainset_missing <- data.frame(
    text = c("q1", "q2"),
    output = c("a1", "a2")
  )
  demos_missing <- format_trainset_as_demos(trainset_missing, sig)
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
    demos_multi <- format_trainset_as_demos(trainset_multi, sig),
    "Multiple potential output columns"
  )
  expect_equal(demos_multi[[1]]$output, "p1")
})