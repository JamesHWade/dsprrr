mock_llm <- new_test_chat(
  chat_structured = function(prompt, type, ...) {
    if (inherits(type, "ellmer::TypeObject")) {
      # Return minimal structured response
      props <- names(type@properties)
      stats::setNames(
        lapply(props, function(name) {
          if (grepl("confidence", name, fixed = TRUE)) {
            0.5
          } else {
            "mock"
          }
        }),
        props
      )
    } else {
      "mock"
    }
  }
)

test_that("compile_module validates inputs", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )
  mod <- module(signature = sig, type = "predict")

  # Invalid teleprompter
  expect_error(
    compile_module(mod, "not a teleprompter", data.frame()),
    "must be a Teleprompter object"
  )

  # Invalid trainset
  tp <- LabeledFewShot()
  expect_error(
    compile_module(mod, tp, "not a data frame"),
    "trainset must be a data frame"
  )

  # Valid inputs
  trainset <- data.frame(text = "test", label = "result")
  result <- compile_module(mod, tp, trainset)
  expect_true(inherits(result, "Module"))
})

test_that("compile entry points require a current ellmer Chat", {
  mod <- module(signature("text -> answer"), type = "predict")
  tp <- LabeledFewShot()
  trainset <- data.frame(text = character(), answer = character())

  expect_error(
    compile_module(
      mod,
      tp,
      trainset,
      .llm = structure(list(), class = "Chat")
    ),
    class = "dsprrr_chat_type_error"
  )
  expect_error(
    compile(tp, mod, trainset, .llm = function() new_test_chat()),
    class = "dsprrr_chat_type_error"
  )
})

test_that("compile_module works with different teleprompters", {
  # Create module
  sig <- Signature(
    inputs = list(input(name = "question", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(
    signature = sig,
    type = "predict",
    template = "Q: {question}\nA:"
  )

  # Create training data
  trainset <- data.frame(
    question = c(
      "What is 2+2?",
      "What is the capital of France?",
      "Who wrote Hamlet?"
    ),
    answer = c("4", "Paris", "Shakespeare"),
    stringsAsFactors = FALSE
  )

  # Test with LabeledFewShot
  tp_labeled <- LabeledFewShot(k = 2L)
  compiled_labeled <- compile_module(mod, tp_labeled, trainset)
  expect_length(compiled_labeled$demos, 2)
  expect_true(compiled_labeled$config$compiled)
  expect_equal(compiled_labeled$config$teleprompter, "LabeledFewShot")

  # Test with GridSearchTeleprompter (mock evaluation)
  variants <- data.frame(
    id = c("brief", "detailed"),
    instructions_suffix = c(". Be brief.", ". Provide detailed explanation."),
    stringsAsFactors = FALSE
  )
  tp_grid <- GridSearchTeleprompter(
    variants = variants,
    metric = function(prediction, expected) {
      identical(prediction, expected$answer)
    },
    k = 1L,
    verbose = FALSE
  )

  # This will use mock evaluation in the current implementation
  compiled_grid <- compile_module(mod, tp_grid, trainset, .llm = mock_llm)
  expect_true(compiled_grid$config$compiled)
  expect_equal(compiled_grid$config$teleprompter, "GridSearchTeleprompter")
  expect_true("best_variant" %in% names(compiled_grid$config))
  expect_true("all_scores" %in% names(compiled_grid$config))
})

test_that("compile_module warns on recompilation", {
  sig <- Signature(
    inputs = list(input(name = "x", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Test"
  )
  mod <- module(
    signature = sig,
    type = "predict",
    config = list(compiled = TRUE, teleprompter = "PreviousOptimizer")
  )
  # Set compiled state for warning test
  mod$state$compiled <- TRUE

  trainset <- data.frame(x = "test", y = "result")
  tp <- LabeledFewShot()

  expect_warning(
    compile_module(mod, tp, trainset),
    "already compiled"
  )
})

test_that("compile workflow with validation set", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Classify sentiment"
  )
  mod <- module(signature = sig, type = "predict")

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
    metric = function(prediction, expected) {
      identical(prediction, expected$sentiment)
    },
    eval_sample_size = 2L,
    verbose = FALSE
  )

  compiled <- compile_module(mod, tp, trainset, valset, .llm = mock_llm)
  expect_true(compiled$config$compiled)

  # The validation set should have been used for evaluation
  # (In real use, scores would differ based on actual LLM calls)
})

test_that("compile integration with module pipeline", {
  # Simulate a more complex workflow

  # 1. Create signature using string notation
  sig <- Signature(
    inputs = list(
      input(name = "context", type = "string"),
      input(name = "question", type = "string")
    ),
    output_type = ellmer::type_object(
      answer = ellmer::type_string(),
      confidence = ellmer::type_number()
    ),
    instructions = "Answer questions based on context"
  )

  # 2. Create module
  qa_mod <- module(
    signature = sig,
    template = "Context: {context}\n\nQuestion: {question}\n\nAnswer:"
  )

  # 3. Prepare training data
  trainset <- data.frame(
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
    answer = c("blue", "100 degrees Celsius", "Paris"),
    stringsAsFactors = FALSE
  )

  # 4. Compile with LabeledFewShot
  tp <- LabeledFewShot(k = 2L)
  compiled_qa <- compile_module(qa_mod, tp, trainset)

  expect_true(inherits(compiled_qa, "Module"))
  expect_true(compiled_qa$is_compiled())
  expect_length(compiled_qa$demos, 2)

  # 5. Test state management
  reset_qa <- compiled_qa$reset_copy()
  expect_false(reset_qa$is_compiled())
  expect_length(reset_qa$demos, 0)

  copy_qa <- compiled_qa$deepcopy()
  expect_true(copy_qa$is_compiled())
  expect_equal(copy_qa$demos, compiled_qa$demos)

  # Modify copy without affecting original
  copy_qa$config$test <- "modified"
  expect_null(compiled_qa$config$test)
})

test_that("evaluate generic executes modules", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Echo"
  )
  mod <- module(signature = sig, type = "predict", template = "{text}")

  dataset <- data.frame(text = c("A", "B"), stringsAsFactors = FALSE)

  eval_llm <- new_test_chat(
    chat_structured = function(prompt, ...) {
      # Return the final line of the prompt (the input text)
      lines <- strsplit(prompt, "\n")[[1]]
      tail(lines, 1L)
    }
  )

  results <- evaluate(
    mod,
    dataset,
    metric = function(pred, row) identical(pred, row$text),
    .llm = eval_llm,
    .progress = FALSE
  )

  expect_equal(results$mean_score, 1)
  expect_equal(results$n_evaluated, 2)
  expect_equal(unlist(results$predictions), dataset$text)
})

test_that("evaluate returns dsprrr_evaluation class", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )
  mod <- module(signature = sig, type = "predict")

  dataset <- data.frame(
    text = c("hello", "world"),
    expected = c("greeting", "noun")
  )

  metric <- function(prediction, expected) {
    identical(prediction, expected$expected)
  }

  results <- evaluate(
    mod,
    dataset,
    metric = metric,
    .llm = mock_llm,
    .progress = FALSE
  )

  # Check S3 class

  expect_s3_class(results, "dsprrr_evaluation")
  expect_true(inherits(results, "dsprrr_evaluation"))

  # Print method should work without error
  output <- capture.output(print(results), type = "message")
  expect_true(any(grepl("Evaluation Results", output, fixed = TRUE)))
  expect_true(any(grepl("Evaluated", output, fixed = TRUE)))
})

test_that("dsprrr_evaluation print method handles errors", {
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )
  mod <- module(signature = sig, type = "predict")

  # Create evaluation with some errors (metric fails)
  dataset <- data.frame(
    text = c("hello", "world"),
    expected = c("greeting", "noun")
  )

  # Metric that always fails
  bad_metric <- function(pred, row) stop("intentional failure")

  results <- expect_test_warnings(
    evaluate(
      mod,
      dataset,
      metric = bad_metric,
      .llm = mock_llm,
      .progress = FALSE
    ),
    "Metric evaluation failed"
  )

  expect_s3_class(results, "dsprrr_evaluation")
  expect_equal(results$n_errors, 2)

  # Print should still work
  output <- capture.output(print(results), type = "message")
  expect_true(any(grepl("Errors", output, fixed = TRUE)))
})

test_that("evaluate() counts failed rows as 0 in mean_score (dsprrr-tn1)", {
  # Regression: mean_score used na.rm = TRUE, so failed rows vanished from the
  # number that drives optimizer selection. A failing row must count as 0 so a
  # config that errors on hard examples cannot outrank a robust one.
  sig <- Signature(
    inputs = list(input(name = "text", type = "string")),
    output_type = ellmer::type_string(),
    instructions = "Classify"
  )
  mod <- module(signature = sig, type = "predict")

  dataset <- data.frame(
    text = c("a", "b"),
    expected = c("a", "b")
  )

  # Scores row 1 as 1, errors on row 2 (-> NA -> counted as 0).
  half_failing_metric <- function(pred, row) {
    if (identical(row$text, "b")) {
      stop("intentional failure")
    }
    1
  }

  results <- expect_test_warnings(
    evaluate(
      mod,
      dataset,
      metric = half_failing_metric,
      .llm = mock_llm,
      .progress = FALSE
    ),
    "Metric evaluation failed"
  )

  expect_equal(results$n_errors, 1)
  expect_equal(results$mean_score, 0.5) # (1 + 0) / 2, not 1.0
})

test_that("evaluate preserves run failures instead of replacing them with metric errors", {
  mod <- module(signature("text -> answer"), type = "predict")
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, ...) {
      if (grepl("fail", prompt, fixed = TRUE)) {
        stop("primary provider failure")
      }
      "ok"
    }
  )
  metric_calls <- 0L
  metric <- function(prediction, expected) {
    metric_calls <<- metric_calls + 1L
    if (anyNA(prediction)) {
      stop("secondary metric failure")
    }
    1
  }

  expect_warning(
    result <- evaluate(
      mod,
      data.frame(text = c("works", "fail")),
      metric = metric,
      .llm = mock_llm,
      .progress = FALSE
    ),
    "Failed to process item 2"
  )

  expect_identical(metric_calls, 1L)
  expect_equal(result$n_errors, 1L)
  expect_equal(result$n_run_errors, 1L)
  expect_equal(result$n_metric_errors, 0L)
  expect_match(result$run_errors, "primary provider failure")
  expect_false(any(grepl(
    "secondary metric failure",
    result$errors,
    fixed = TRUE
  )))
  expect_match(result$data$.error[[2]], "primary provider failure")
})
