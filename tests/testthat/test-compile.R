test_that("compile_module validates inputs", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )
  module <- Predict(signature = sig)

  # Invalid teleprompter
  expect_error(
    compile_module(module, "not a teleprompter", data.frame()),
    "must be a Teleprompter object"
  )

  # Invalid trainset
  tp <- LabeledFewShot()
  expect_error(
    compile_module(module, tp, "not a data frame"),
    "trainset must be a data frame"
  )

  # Valid inputs
  trainset <- data.frame(text = "test", label = "result")
  result <- compile_module(module, tp, trainset)
  expect_s3_class(result, "dsprrr::Predict")
})

test_that("compile_module works with different teleprompters", {
  # Create module
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  module <- Predict(signature = sig, template = "Q: {question}\nA:")

  # Create training data
  trainset <- data.frame(
    question = c("What is 2+2?", "What is the capital of France?", "Who wrote Hamlet?"),
    answer = c("4", "Paris", "Shakespeare"),
    stringsAsFactors = FALSE
  )

  # Test with LabeledFewShot
  tp_labeled <- LabeledFewShot(k = 2L)
  compiled_labeled <- compile_module(module, tp_labeled, trainset)
  expect_length(compiled_labeled@demos, 2)
  expect_true(compiled_labeled@config$compiled)
  expect_equal(compiled_labeled@config$teleprompter, "LabeledFewShot")

  # Test with GridSearchTeleprompter (mock evaluation)
  variants <- data.frame(
    id = c("brief", "detailed"),
    instructions_suffix = c(". Be brief.", ". Provide detailed explanation."),
    stringsAsFactors = FALSE
  )
  tp_grid <- GridSearchTeleprompter(
    variants = variants,
    metric = metric_exact_match(field = "answer"),
    k = 1L,
    verbose = FALSE
  )

  # This will use mock evaluation in the current implementation
  compiled_grid <- compile_module(module, tp_grid, trainset)
  expect_true(compiled_grid@config$compiled)
  expect_equal(compiled_grid@config$teleprompter, "GridSearchTeleprompter")
  expect_true("best_variant" %in% names(compiled_grid@config))
  expect_true("all_scores" %in% names(compiled_grid@config))
})

test_that("compile_module warns on recompilation", {
  sig <- Signature(
    inputs = list(input(name = "x", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )
  module <- Predict(
    signature = sig,
    config = list(compiled = TRUE, teleprompter = "PreviousOptimizer")
  )

  trainset <- data.frame(x = "test", y = "result")
  tp <- LabeledFewShot()

  expect_warning(
    compile_module(module, tp, trainset),
    "already compiled"
  )
})

test_that("dsp_trainset creates training data correctly", {
  # From scratch
  trainset <- dsp_trainset(
    text = c("hello", "world"),
    label = c("greeting", "noun")
  )
  expect_equal(nrow(trainset), 2)
  expect_equal(trainset$text, c("hello", "world"))
  expect_equal(trainset$label, c("greeting", "noun"))

  # With existing data frame
  base_df <- data.frame(id = 1:2)
  trainset2 <- dsp_trainset(
    .data = base_df,
    text = c("hello", "world")
  )
  expect_equal(nrow(trainset2), 2)
  expect_equal(trainset2$id, 1:2)
  expect_equal(trainset2$text, c("hello", "world"))

  # Error cases
  expect_error(dsp_trainset(), "Must provide either data arguments")
  expect_error(
    dsp_trainset(x = 1:2, y = 1:3),
    "same length"
  )

  # Empty trainset warning
  expect_warning(
    empty <- dsp_trainset(.data = data.frame()),
    "empty training set"
  )
  expect_equal(nrow(empty), 0)
})

test_that("evaluate_dsp evaluates modules", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )
  module <- Predict(signature = sig)

  dataset <- data.frame(
    text = c("hello", "world"),
    expected = c("greeting", "noun")
  )

  metric <- metric_exact_match(field = "result")

  # Mock evaluation (actual would need LLM)
  results <- evaluate_dsp(
    module = module,
    dataset = dataset,
    metric = metric,
    verbose = FALSE
  )

  expect_true(is.list(results))
  expect_true("mean_score" %in% names(results))
  expect_true("scores" %in% names(results))
  expect_true("n_evaluated" %in% names(results))
  expect_true("n_errors" %in% names(results))
  expect_equal(length(results$scores), nrow(dataset))

  # Empty dataset
  empty_results <- evaluate_dsp(
    module = module,
    dataset = data.frame(),
    metric = metric,
    verbose = FALSE
  )
  expect_true(is.na(empty_results$mean_score))
  expect_equal(empty_results$n_evaluated, 0)

  # Invalid inputs
  expect_error(
    evaluate_dsp(module, "not a df", metric),
    "must be a data frame"
  )
  expect_error(
    evaluate_dsp(module, dataset, "not a function"),
    "must be a function"
  )
})

test_that("compile workflow with validation set", {
  sig <- Signature(
    inputs = list(input(name = "text", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Classify sentiment"
  )
  module <- Predict(signature = sig)

  trainset <- data.frame(
    text = c("love it", "hate it", "okay", "great", "terrible"),
    sentiment = c("pos", "neg", "neutral", "pos", "neg"),
    stringsAsFactors = FALSE
  )

  valset <- data.frame(
    text = c("amazing", "awful"),
    sentiment = c("pos", "neg"),
    stringsAsFactors = FALSE
  )

  # GridSearch with explicit validation set
  variants <- data.frame(
    id = c("v1", "v2"),
    instructions_suffix = c(" (positive/negative/neutral)", " - be precise"),
    stringsAsFactors = FALSE
  )

  tp <- GridSearchTeleprompter(
    variants = variants,
    metric = metric_exact_match(field = "sentiment"),
    eval_sample_size = 2L,
    verbose = FALSE
  )

  compiled <- compile_module(module, tp, trainset, valset)
  expect_true(compiled@config$compiled)

  # The validation set should have been used for evaluation
  # (In real use, scores would differ based on actual LLM calls)
})

test_that("compile integration with module pipeline", {
  # Simulate a more complex workflow

  # 1. Create signature using string notation
  sig <- Signature(
    inputs = list(
      input(name = "context", class = S7::class_character),
      input(name = "question", class = S7::class_character)
    ),
    output_type = ellmer::type_object(
      answer = ellmer::type_string(),
      confidence = ellmer::type_number()
    ),
    instructions = "Answer questions based on context"
  )

  # 2. Create module
  qa_module <- Predict(
    signature = sig,
    template = "Context: {context}\n\nQuestion: {question}\n\nAnswer:"
  )

  # 3. Prepare training data
  trainset <- dsp_trainset(
    context = c(
      "The sky is blue during the day.",
      "Water boils at 100 degrees Celsius.",
      "Paris is the capital of France."
    ),
    question = c(
      "What color is the sky?",
      "At what temperature does water boil?",
      "What is the capital of France?"
    ),
    answer = c("blue", "100 degrees Celsius", "Paris")
  )

  # 4. Compile with LabeledFewShot
  tp <- LabeledFewShot(k = 2L)
  compiled_qa <- compile_module(qa_module, tp, trainset)

  expect_s3_class(compiled_qa, "dsprrr::Predict")
  expect_true(is_compiled(compiled_qa))
  expect_length(compiled_qa@demos, 2)

  # 5. Test state management
  reset_qa <- reset_copy(compiled_qa)
  expect_false(is_compiled(reset_qa))
  expect_length(reset_qa@demos, 0)

  copy_qa <- deepcopy(compiled_qa)
  expect_true(is_compiled(copy_qa))
  expect_equal(copy_qa@demos, compiled_qa@demos)

  # Modify copy without affecting original
  copy_qa@config$test <- "modified"
  expect_null(compiled_qa@config$test)
})