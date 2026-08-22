# --- is_ellmer_type tests ---

test_that("is_ellmer_type identifies ellmer types correctly", {
  # Test with actual ellmer types
  expect_true(is_ellmer_type(ellmer::type_string()))
  expect_true(is_ellmer_type(ellmer::type_boolean()))
  expect_true(is_ellmer_type(ellmer::type_integer()))
  expect_true(is_ellmer_type(ellmer::type_number()))
  expect_true(is_ellmer_type(ellmer::type_array(items = ellmer::type_string())))
  expect_true(is_ellmer_type(ellmer::type_object(
    a = ellmer::type_string(),
    b = ellmer::type_number()
  )))
  expect_true(is_ellmer_type(ellmer::type_enum(values = c("a", "b", "c"))))

  # Test with non-ellmer types
  expect_false(is_ellmer_type("string"))
  expect_false(is_ellmer_type(123))
  expect_false(is_ellmer_type(list()))
  expect_false(is_ellmer_type(NULL))
  expect_false(is_ellmer_type(data.frame()))
})

test_that("is_ellmer_type handles edge cases", {
  # Objects with similar class names but not from ellmer
  fake_type <- structure(list(), class = "Type")
  expect_false(is_ellmer_type(fake_type))

  # Nested types
  nested_type <- ellmer::type_object(
    items = ellmer::type_array(items = ellmer::type_string())
  )
  expect_true(is_ellmer_type(nested_type))

  # Multiple inheritance - should still work
  expect_true(is_ellmer_type(ellmer::type_string()))
})

# --- has_ellmer_credentials tests ---

test_that("has_ellmer_credentials detects API keys", {
  # Save original values
  orig_openai <- Sys.getenv("OPENAI_API_KEY")
  orig_anthropic <- Sys.getenv("ANTHROPIC_API_KEY")
  orig_google <- Sys.getenv("GOOGLE_GEMINI_API_KEY")

  # Clear all keys
  withr::local_envvar(
    OPENAI_API_KEY = "",
    ANTHROPIC_API_KEY = "",
    GOOGLE_GEMINI_API_KEY = ""
  )

  expect_false(dsprrr:::has_ellmer_credentials())

  # Test with OpenAI key only
  withr::local_envvar(OPENAI_API_KEY = "test-key")
  expect_true(dsprrr:::has_ellmer_credentials())
})

test_that("has_ellmer_credentials detects Anthropic key", {
  withr::local_envvar(
    OPENAI_API_KEY = "",
    ANTHROPIC_API_KEY = "test-anthropic-key",
    GOOGLE_GEMINI_API_KEY = ""
  )

  expect_true(dsprrr:::has_ellmer_credentials())
})

test_that("has_ellmer_credentials detects Google key", {
  withr::local_envvar(
    OPENAI_API_KEY = "",
    ANTHROPIC_API_KEY = "",
    GOOGLE_GEMINI_API_KEY = "test-google-key"
  )

  expect_true(dsprrr:::has_ellmer_credentials())
})

test_that("has_ellmer_credentials detects multiple keys", {
  withr::local_envvar(
    OPENAI_API_KEY = "key1",
    ANTHROPIC_API_KEY = "key2",
    GOOGLE_GEMINI_API_KEY = ""
  )

  expect_true(dsprrr:::has_ellmer_credentials())
})

# --- null coalescing operator tests ---

test_that("null coalescing operator works correctly", {
  `%||%` <- dsprrr:::`%||%`

  # NULL case
  expect_equal(NULL %||% "default", "default")

  # Non-NULL case
  expect_equal("value" %||% "default", "value")

  # Nested case
  expect_equal(NULL %||% NULL %||% "final", "final")

  # With various types

  expect_equal(0 %||% 1, 0) # 0 is not NULL
  expect_equal(FALSE %||% TRUE, FALSE) # FALSE is not NULL
  expect_equal("" %||% "default", "") # Empty string is not NULL
  expect_equal(NA %||% "default", NA) # NA is not NULL
  expect_equal(list() %||% list(a = 1), list()) # Empty list is not NULL
})

# --- Helper function tests from run.R ---

test_that("format_output handles various types", {
  # Simple string
  expect_equal(dsprrr:::format_output("hello"), "hello")

  # Number
  expect_equal(dsprrr:::format_output(42), "42")

  # List gets converted to JSON
  result <- dsprrr:::format_output(list(a = 1, b = "test"))
  expect_true(is.character(result))
  expect_true(grepl("a", result, fixed = TRUE))
  expect_true(grepl("test", result, fixed = TRUE))

  # Logical
  expect_equal(dsprrr:::format_output(TRUE), "TRUE")
})

test_that("format_inputs creates proper format", {
  sig_inputs <- list(
    input(
      name = "text",
      type = "string",
      description = "Input text"
    ),
    input(name = "count", type = "integer", description = "A count")
  )

  inputs <- list(text = "hello", count = 5)

  result <- dsprrr:::format_inputs(inputs, sig_inputs)

  expect_true(grepl("Input text", result, fixed = TRUE))
  expect_true(grepl("text: hello", result, fixed = TRUE))
  expect_true(grepl("A count", result, fixed = TRUE))
  expect_true(grepl("count: 5", result, fixed = TRUE))
})

test_that("format_inputs handles empty inputs", {
  result <- dsprrr:::format_inputs(list(), list())
  expect_equal(result, "")
})

test_that("format_inputs handles inputs without descriptions", {
  sig_inputs <- list(
    input(name = "text", type = "string")
  )

  inputs <- list(text = "hello")

  result <- dsprrr:::format_inputs(inputs, sig_inputs)

  expect_true(grepl("text: hello", result, fixed = TRUE))
  expect_false(grepl("#", result, fixed = TRUE)) # No description comment
})

# --- Optimization helpers tests ---

test_that("merge_optimization_control sets defaults", {
  result <- dsprrr:::merge_optimization_control(NULL)

  expect_true("progress" %in% names(result))
  expect_true("parallel" %in% names(result))
  expect_true("grid_type" %in% names(result))
  expect_true("grid_levels" %in% names(result))

  expect_false(result$parallel)
  expect_equal(result$grid_type, "regular")
  expect_equal(result$grid_levels, 3L)
})

test_that("merge_optimization_control respects user overrides", {
  result <- dsprrr:::merge_optimization_control(list(
    parallel = TRUE,
    grid_levels = 5L
  ))

  expect_true(result$parallel)
  expect_equal(result$grid_levels, 5L)
})

test_that("merge_optimization_control handles grid_type validation", {
  result <- dsprrr:::merge_optimization_control(list(grid_type = "RANDOM"))
  expect_equal(result$grid_type, "random")

  result2 <- dsprrr:::merge_optimization_control(list(grid_type = "Regular"))
  expect_equal(result2$grid_type, "regular")
})

test_that("expand_grid_from_list creates correct grid", {
  params <- list(
    a = c(1, 2),
    b = c("x", "y", "z")
  )

  result <- dsprrr:::expand_grid_from_list(params)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 6) # 2 * 3
  expect_true("a" %in% names(result))
  expect_true("b" %in% names(result))
})

test_that("expand_grid_from_list rejects empty parameters", {
  expect_error(
    dsprrr:::expand_grid_from_list(list()),
    "at least one element"
  )
})

test_that("expand_grid_from_list rejects unnamed parameters", {
  expect_error(
    dsprrr:::expand_grid_from_list(list(c(1, 2), c(3, 4))),
    "must be named"
  )
})

# --- find_closest_match tests ---

test_that("find_closest_match finds exact matches", {
  options <- c("question", "answer", "context")
  expect_equal(dsprrr:::find_closest_match("question", options), "question")
  expect_equal(dsprrr:::find_closest_match("answer", options), "answer")
})

test_that("find_closest_match finds typos", {
  options <- c("question", "answer", "context")

  # One character off
  expect_equal(dsprrr:::find_closest_match("questoin", options), "question")
  expect_equal(dsprrr:::find_closest_match("anser", options), "answer")

  # Two characters off
  expect_equal(dsprrr:::find_closest_match("qestion", options), "question")
})

test_that("find_closest_match returns NULL for distant matches", {
  options <- c("question", "answer", "context")

  # Too different
  expect_null(dsprrr:::find_closest_match("xyz", options))
  expect_null(dsprrr:::find_closest_match("completely_different", options))
})

test_that("find_closest_match handles empty options", {
  expect_null(dsprrr:::find_closest_match("question", character(0)))
})

test_that("find_closest_match is case insensitive", {
  options <- c("Question", "Answer", "Context")

  expect_equal(dsprrr:::find_closest_match("question", options), "Question")
  expect_equal(dsprrr:::find_closest_match("ANSWER", options), "Answer")
})

test_that("suggest_match returns formatted suggestion", {
  options <- c("question", "answer")

  suggestion <- dsprrr:::suggest_match("questoin", options)
  expect_true(grepl("Did you mean", suggestion, fixed = TRUE))
  expect_true(grepl("question", suggestion, fixed = TRUE))
})
test_that("suggest_match returns NULL for no match", {
  options <- c("question", "answer")

  expect_null(dsprrr:::suggest_match("xyz", options))
})
test_that("trace_cost accepts only canonical current-call fields", {
  expect_equal(dsprrr:::trace_cost(list(cost = 0.1)), 0.1)
  expect_true(is.na(dsprrr:::trace_cost(list(total_cost = 0.2))))
})

test_that("undeclared dot-prefixed inputs are rejected", {
  sig <- signature("q -> a")

  expect_error(
    validate_signature_inputs(sig, list(q = "x", .bogus = TRUE)),
    class = "dsprrr_reserved_input_error"
  )
})

test_that("removed runtime arguments name their replacement", {
  sig <- signature("q -> a")

  expect_error(
    validate_signature_inputs(sig, list(q = "x", .parallel = TRUE)),
    ".concurrency",
    fixed = TRUE
  )
  expect_error(
    validate_signature_inputs(sig, list(q = "x", .parallel_method = "mirai")),
    ".concurrency",
    fixed = TRUE
  )
})

test_that("dot-prefixed inputs are rejected for zero-input signatures", {
  sig <- signature(
    inputs = list(),
    output_type = ellmer::type_string(),
    instructions = "Say hello"
  )

  expect_error(
    validate_signature_inputs(sig, list(.parallel = TRUE)),
    class = "dsprrr_reserved_input_error"
  )
})

test_that("a declared dot-prefixed field is still accepted", {
  sig <- signature(
    inputs = list(input(".ok")),
    output_type = ellmer::type_string()
  )

  expect_silent(validate_signature_inputs(sig, list(.ok = "x")))
})

test_that("run() rejects the removed .parallel argument", {
  mod <- module(signature("q -> a"))

  expect_error(
    run(mod, q = "hi", .parallel = TRUE, .llm = new_test_chat()),
    class = "dsprrr_reserved_input_error"
  )
})

test_that("dataset APIs reject removed arguments before empty returns", {
  mod <- module(signature("q -> a"))
  empty <- data.frame(q = character())

  expect_snapshot(
    error = TRUE,
    run_dataset(mod, empty, .parallel = TRUE)
  )
  expect_snapshot(
    error = TRUE,
    evaluate(
      mod,
      empty,
      metric = function(...) 1,
      .parallel_method = "mirai"
    )
  )
})

test_that("dataset runtime-name validation does not force arguments", {
  mod <- module(signature("q -> a"))
  empty <- data.frame(q = character())
  forced <- FALSE

  condition <- rlang::catch_cnd(run_dataset(
    mod,
    empty,
    .parallel = {
      forced <<- TRUE
      TRUE
    }
  ))

  expect_s3_class(condition, "dsprrr_reserved_input_error")
  expect_identical(forced, FALSE)
  expect_no_error(run_dataset(mod, empty, .cache = FALSE))
})
