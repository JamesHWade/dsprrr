# Tests for ellmer module-as-tool integration

test_that("as_ellmer_tool creates tool with correct function signature", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> sentiment")
  )

  tool <- as_ellmer_tool(
    mod,
    name = "test_sentiment",
    description = "Analyze sentiment"
  )

  # Tool should be an ellmer ToolDef (which is also a function)
  expect_s3_class(tool, "ellmer::ToolDef")
  expect_true(is.function(tool))

  # Tool name and description should match
  expect_equal(tool@name, "test_sentiment")
  expect_equal(tool@description, "Analyze sentiment")

  # Function should have named parameter matching signature input
  # ToolDef IS the function, so formals() works directly
  fn_args <- names(formals(tool))
  expect_equal(fn_args, "text")
})

test_that("as_ellmer_tool creates tool with multiple inputs", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("context, question -> answer")
  )

  tool <- as_ellmer_tool(
    mod,
    name = "qa_tool",
    description = "Answer questions"
  )

  # Function should have both parameters
  fn_args <- names(formals(tool))
  expect_equal(fn_args, c("context", "question"))
})

test_that("as_ellmer_tool function can be invoked with mock LLM", {
  skip_if_not_installed("ellmer")

  # Create mock LLM that returns predictable output
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      list(sentiment = "positive")
    }
  )

  mod <- module(
    signature("text -> sentiment")
  )

  tool <- as_ellmer_tool(
    mod,
    name = "test_sentiment",
    description = "Analyze sentiment",
    .llm = mock_llm
  )

  # Actually invoke the tool function (ToolDef IS the function)
  result <- tool(text = "I love this!")

  # Native structured values should flow through unchanged
  expect_type(result, "list")
  expect_equal(result$sentiment, "positive")
})

test_that("as_ellmer_tool passes annotations through to ellmer", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> sentiment")
  )

  tool <- as_ellmer_tool(
    mod,
    name = "annotated_sentiment",
    annotations = ellmer::tool_annotations(
      read_only_hint = TRUE,
      destructive_hint = FALSE
    )
  )

  expect_true(tool@annotations$read_only_hint)
  expect_false(tool@annotations$destructive_hint)
  expect_null(tool@annotations$open_world_hint)
})

test_that("as_ellmer_tool marks prediction modules read-only and closed-world", {
  predicts <- list(
    module = module(signature("text -> sentiment")),
    chain_of_thought = chain_of_thought("question -> answer"),
    with_reasoning = module(with_reasoning("question -> answer")),
    knn = dsprrr:::KNNFewShotModule$new(
      module = module(signature("text -> answer")),
      k = 1L,
      vectorizer = function(texts) matrix(1, nrow = length(texts)),
      input_text = function(row) "x",
      train_embeddings = matrix(1, nrow = 1),
      trainset_demos = list(list(text = "x", answer = "y"))
    )
  )
  expected <- ellmer::tool_annotations(
    read_only_hint = TRUE,
    open_world_hint = FALSE
  )

  expect_identical(
    vapply(predicts, function(x) class(x)[[1L]], character(1)),
    c(
      module = "PredictModule",
      chain_of_thought = "PredictModule",
      with_reasoning = "PredictModule",
      knn = "KNNFewShotModule"
    )
  )
  for (kind in names(predicts)) {
    tool <- as_ellmer_tool(predicts[[kind]], name = "predict_tool")
    expect_identical(tool@annotations, expected, info = kind)
  }
})

test_that("as_ellmer_tool claims nothing for modules that can run code or tools", {
  factory <- function() r_code_runner()
  reward_fn <- function(args, pred) 1
  others <- list(
    module_fn = module_fn("text -> answer", function(text) list(answer = text)),
    react = react("question -> answer"),
    code_act = code_act("question -> answer", interpreter_factory = factory),
    program_of_thought = program_of_thought(
      "question -> answer",
      interpreter_factory = factory
    ),
    rlm = rlm_module("question -> answer", interpreter_factory = factory),
    flex = suppressWarnings(flex("question -> answer")),
    pipeline = pipeline(module(signature("question -> answer"))),
    best_of_n = best_of_n(
      module(signature("question -> answer")),
      reward_fn = reward_fn
    ),
    knn_react = dsprrr:::KNNFewShotModule$new(
      module = react("text -> answer"),
      k = 1L,
      vectorizer = function(texts) matrix(1, nrow = length(texts)),
      input_text = function(row) "x",
      train_embeddings = matrix(1, nrow = 1),
      trainset_demos = list(list(text = "x", answer = "y"))
    )
  )

  for (kind in names(others)) {
    tool <- as_ellmer_tool(others[[kind]], name = "other_tool")
    expect_identical(tool@annotations, list(), info = kind)
  }
})

test_that("as_ellmer_tool uses explicit annotations as given", {
  mod <- module(signature("text -> sentiment"))
  custom <- ellmer::tool_annotations(
    read_only_hint = FALSE,
    destructive_hint = TRUE
  )

  none <- as_ellmer_tool(mod, name = "plain", annotations = list())
  given <- as_ellmer_tool(mod, name = "custom", annotations = custom)
  fn_given <- as_ellmer_tool(
    module_fn("text -> answer", function(text) list(answer = text)),
    name = "fn_custom",
    annotations = ellmer::tool_annotations(read_only_hint = TRUE)
  )

  expect_identical(none@annotations, list())
  expect_identical(given@annotations, custom)
  expect_identical(
    fn_given@annotations,
    ellmer::tool_annotations(read_only_hint = TRUE)
  )
})

test_that("as_ellmer_tool supports output serialization modes", {
  skip_if_not_installed("ellmer")

  mod <- module_fn(
    "text -> sentiment, confidence: number",
    function(text, ...) {
      list(sentiment = "positive", confidence = 0.9)
    }
  )

  json_tool <- as_ellmer_tool(mod, name = "json_sentiment", output = "json")
  expect_equal(
    json_tool(text = "good"),
    "{\"sentiment\":\"positive\",\"confidence\":0.9}"
  )

  text_mod <- module_fn(
    "text -> answer",
    function(text, ...) list(answer = paste("Answer:", text))
  )
  text_tool <- as_ellmer_tool(text_mod, name = "text_answer", output = "text")
  expect_equal(text_tool(text = "ok"), "Answer: ok")
})

test_that("format_ellmer_tool_output reorders fields for 'auto', preserves them for 'raw'", {
  skip_if_not_installed("ellmer")

  out_type <- ellmer::type_object(
    first = ellmer::type_string(),
    second = ellmer::type_string()
  )
  unordered <- list(second = "B", first = "A")

  auto_result <- dsprrr:::format_ellmer_tool_output(unordered, out_type, "auto")
  raw_result <- dsprrr:::format_ellmer_tool_output(unordered, out_type, "raw")

  expect_equal(names(auto_result), c("first", "second"))
  expect_equal(names(raw_result), c("second", "first"))
})

test_that("as_ellmer_tool output = 'text' falls back to JSON for multi-field results", {
  skip_if_not_installed("ellmer")

  mod <- module_fn(
    "text -> a, b",
    function(text, ...) list(a = "x", b = "y")
  )
  tool <- as_ellmer_tool(mod, name = "multi_text", output = "text")
  expect_equal(tool(text = "ok"), "{\"a\":\"x\",\"b\":\"y\"}")
})

test_that("as_ellmer_tool rejects non-Module input", {
  skip_if_not_installed("ellmer")

  expect_error(
    as_ellmer_tool(list()),
    "must be a DSPrrr Module"
  )
})

test_that("as_ellmer_tool copy = 'deep' avoids mutating source traces", {
  skip_if_not_installed("ellmer")
  local_reset_cache()

  mod <- module_fn("text -> answer", function(text, ...) list(answer = text))

  deep_tool <- as_ellmer_tool(mod, name = "deep_tool", copy = "deep")
  deep_tool(text = "one")
  expect_length(mod$state$traces, 0)

  shared_tool <- as_ellmer_tool(mod, name = "shared_tool", copy = "none")
  shared_tool(text = "two")
  expect_length(mod$state$traces, 1)
})

test_that("as_ellmer_tool preserves structured argument schemas", {
  skip_if_not_installed("ellmer")

  sig <- Signature(
    inputs = list(
      input(
        "payload",
        ellmer::type_object(
          query = ellmer::type_string(),
          limit = ellmer::type_integer(),
          ignored = ellmer::type_ignore()
        )
      )
    ),
    output_type = ellmer::type_string()
  )

  tool <- as_ellmer_tool(module(sig), name = "structured")

  payload_type <- tool@arguments@properties$payload

  expect_s3_class(payload_type, "ellmer::TypeObject")
  expect_true("ignored" %in% names(payload_type@properties))
  expect_s3_class(
    payload_type@properties$ignored,
    "ellmer::TypeIgnore"
  )
})

test_that("as_ellmer_tool generates description from signature if not provided", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> sentiment", instructions = "Analyze the sentiment")
  )

  tool <- as_ellmer_tool(mod, name = "test_tool")

  # Should use instructions as description
  expect_equal(tool@description, "Analyze the sentiment")
})

test_that("as_ellmer_tool generates fallback description without instructions", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> sentiment")
  )

  tool <- as_ellmer_tool(mod, name = "test_tool")

  # Should generate description from inputs
  expect_match(tool@description, "text")
})

test_that("as_ellmer_tool generates name from signature if not provided", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> sentiment")
  )

  tool <- as_ellmer_tool(mod, description = "Test tool")

  # Should generate a name (implementation-dependent)
  expect_type(tool@name, "character")
  expect_true(nzchar(tool@name))
})

test_that("as_ellmer_tool function environment has access to run", {
  skip_if_not_installed("ellmer")

  mod <- module(
    signature("text -> answer")
  )

  tool <- as_ellmer_tool(mod, name = "test", description = "test")

  # The function environment should have access to dsprrr functions
  fn_env <- environment(tool)

  # Should be able to find 'run' in the environment chain
  expect_true(
    exists("run", envir = fn_env, inherits = TRUE),
    info = "Tool function environment should have access to 'run'"
  )
})

test_that("as_ellmer_tool handles errors from module", {
  skip_if_not_installed("ellmer")

  # Create mock LLM that throws an error
  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      stop("API error")
    }
  )

  mod <- module(
    signature("text -> sentiment")
  )

  tool <- as_ellmer_tool(
    mod,
    name = "test_sentiment",
    description = "Analyze sentiment",
    .llm = mock_llm
  )

  # Default behavior emits a warning and returns a structured observation.
  expect_warning(
    result <- tool(text = "test"),
    class = "dsprrr_tool_error_warning"
  )
  expect_true(result$error)
  expect_s3_class(result, "dsprrr_tool_observation")
  expect_match(result$message, "API error")
  expect_equal(result$tool, "test_sentiment")

  abort_tool <- as_ellmer_tool(
    mod,
    name = "test_sentiment",
    description = "Analyze sentiment",
    .llm = mock_llm,
    error = "abort"
  )
  expect_error(abort_tool(text = "test"), "API error")
})

test_that("as_ellmer_tool error = 'return' signals a classed condition", {
  skip_if_not_installed("ellmer")

  mock_llm <- new_test_chat(
    chat_structured = function(prompt, type, ...) {
      stop("API error")
    }
  )

  mod <- module(signature("text -> sentiment"))
  tool <- as_ellmer_tool(
    mod,
    name = "return_tool",
    .llm = mock_llm,
    error = "return"
  )

  err <- tryCatch(
    suppressWarnings(tool(text = "test")),
    dsprrr_tool_error = function(e) e
  )

  expect_s3_class(err, "dsprrr_tool_error")
  expect_s3_class(err$payload, "dsprrr_tool_observation")
  expect_equal(err$payload$tool, "return_tool")
  expect_match(conditionMessage(err), "API error")
})
