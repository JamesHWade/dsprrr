# Test helpers for KNNFewShot

# Deterministic vectorizer that creates embeddings based on text content
# Similar texts will have similar embeddings
fake_vectorizer <- function(texts) {
  # Create deterministic embeddings based on character codes
  embeddings <- t(vapply(
    texts,
    function(text) {
      chars <- utf8ToInt(text)
      # Create a 10-dimensional embedding based on character frequencies
      embedding <- numeric(10)
      for (char in chars) {
        idx <- (char %% 10) + 1
        embedding[idx] <- embedding[idx] + 1
      }
      # Normalize
      norm <- sqrt(sum(embedding^2))
      if (norm > 0) {
        embedding <- embedding / norm
      }
      embedding
    },
    numeric(10)
  ))
  embeddings
}

# Mock LLM for testing
mock_llm <- list(
  chat_structured = function(prompt, type, ...) {
    list(answer = "mocked response")
  }
)

test_that("KNNFewShot can be created and validated", {
  tp <- KNNFewShot(k = 3L, vectorizer = fake_vectorizer)
  expect_s3_class(tp, "dsprrr::KNNFewShot")
  expect_s3_class(tp, "dsprrr::Teleprompter")
  expect_equal(tp@k, 3L)
  expect_identical(tp@vectorizer, fake_vectorizer)
  expect_null(tp@input_text)
  expect_true(tp@cache_embeddings)
  expect_false(tp@merge_demos)

  # Custom parameters
  input_fn <- function(x) x$question
  tp_custom <- KNNFewShot(
    k = 5L,
    vectorizer = fake_vectorizer,
    input_text = input_fn,
    cache_embeddings = FALSE,
    merge_demos = TRUE
  )
  expect_equal(tp_custom@k, 5L)
  expect_identical(tp_custom@input_text, input_fn)
  expect_false(tp_custom@cache_embeddings)
  expect_true(tp_custom@merge_demos)
})

test_that("KNNFewShot validates properties", {
  # k must be at least 1
  expect_error(
    KNNFewShot(k = 0L, vectorizer = fake_vectorizer),
    "k must be at least 1"
  )
  expect_error(
    KNNFewShot(k = -1L, vectorizer = fake_vectorizer),
    "k must be at least 1"
  )

  # vectorizer must be a function
  expect_error(
    KNNFewShot(k = 3L, vectorizer = "not a function"),
    "function"
  )

  # input_text must be function or NULL
  expect_error(
    KNNFewShot(k = 3L, vectorizer = fake_vectorizer, input_text = "not a fn"),
    "input_text must be a function"
  )
})

test_that("KNNFewShot compile creates KNNFewShotModule", {
  # Create a simple module
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer the question"
  )
  mod <- module(signature = sig, type = "predict")

  # Create training data
  trainset <- data.frame(
    question = c(
      "What is 2+2?",
      "What is 3+3?",
      "What is 4+4?",
      "What is 5+5?"
    ),
    answer = c("4", "6", "8", "10"),
    stringsAsFactors = FALSE
  )

  # Compile with KNNFewShot
  tp <- KNNFewShot(k = 2L, vectorizer = fake_vectorizer)
  optimized <- compile(tp, mod, trainset)

  expect_s3_class(optimized, "KNNFewShotModule")
  expect_true(inherits(optimized, "Module"))
  expect_true(optimized$config$compiled)
  expect_equal(optimized$config$teleprompter, "KNNFewShot")
  expect_equal(optimized$config$compilation_k, 2L)
  expect_equal(optimized$config$n_train_examples, 4L)
})

test_that("KNNFewShot compile validates inputs", {
  tp <- KNNFewShot(k = 2L, vectorizer = fake_vectorizer)

  # Must be a Module
  expect_error(
    compile(tp, "not a module", data.frame(x = 1)),
    "only supports Module objects"
  )

  # Must be a data frame
  sig <- Signature(
    inputs = list(input(name = "x")),
    output_type = ellmer::type_string()
  )
  mod <- module(signature = sig, type = "predict")
  expect_error(
    compile(tp, mod, list(x = 1)),
    "trainset must be a data frame"
  )

  # Empty trainset returns unmodified
  empty_trainset <- data.frame(x = character(), answer = character())
  expect_warning(
    result <- compile(tp, mod, empty_trainset),
    "Empty trainset"
  )
})

test_that("KNNFewShot selects demos dynamically at runtime", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig, type = "predict")

  # Training data with varied questions
  trainset <- data.frame(
    question = c(
      "What is 2+2?",
      "What is 3+3?",
      "What is the capital of France?",
      "What is the capital of Germany?"
    ),
    answer = c("4", "6", "Paris", "Berlin"),
    stringsAsFactors = FALSE
  )

  tp <- KNNFewShot(k = 2L, vectorizer = fake_vectorizer)
  compiled <- compile(tp, mod, trainset)

  # Run with a math question - should get math demos
  result1 <- compiled$forward(
    list(question = "What is 1+1?"),
    .llm = mock_llm
  )

  selection1 <- compiled$last_selection()
  expect_equal(selection1$n_demos, 2)
  expect_length(selection1$indices, 2)

  # Run with a geography question - should get different demos
  result2 <- compiled$forward(
    list(question = "What is the capital of Spain?"),
    .llm = mock_llm
  )

  selection2 <- compiled$last_selection()
  expect_equal(selection2$n_demos, 2)
  expect_length(selection2$indices, 2)

  # Verify selections may differ (depending on embedding similarity)
  # At minimum, verify the structure is correct
  expect_s3_class(result1, "tbl_df")
  expect_s3_class(result2, "tbl_df")
})

test_that("KNNFewShot works with batch inputs", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = c("Q1", "Q2", "Q3"),
    answer = c("A1", "A2", "A3"),
    stringsAsFactors = FALSE
  )

  tp <- KNNFewShot(k = 2L, vectorizer = fake_vectorizer)
  compiled <- compile(tp, mod, trainset)

  # Batch input as data frame
  batch <- data.frame(
    question = c("Test Q1", "Test Q2"),
    stringsAsFactors = FALSE
  )

  result <- compiled$forward(batch, .llm = mock_llm)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)

  # Each row should have gotten its own demo selection
  expect_equal(length(compiled$state$demo_selections), 2)
})

test_that("KNNFewShot with custom input_text function", {
  sig <- Signature(
    inputs = list(
      input(name = "context", class = S7::class_character),
      input(name = "question", class = S7::class_character)
    ),
    output_type = ellmer::type_string(),
    instructions = "Answer based on context"
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    context = c("The sky is blue.", "Grass is green."),
    question = c("What color is sky?", "What color is grass?"),
    answer = c("blue", "green"),
    stringsAsFactors = FALSE
  )

  # Custom input_text that uses both context and question
  custom_input_text <- function(example) {
    paste(example$context, example$question, sep = " | ")
  }

  tp <- KNNFewShot(
    k = 1L,
    vectorizer = fake_vectorizer,
    input_text = custom_input_text
  )
  compiled <- compile(tp, mod, trainset)

  result <- compiled$forward(
    list(context = "The sun is yellow.", question = "What color is sun?"),
    .llm = mock_llm
  )

  expect_s3_class(result, "tbl_df")
  selection <- compiled$last_selection()
  expect_true(grepl("\\|", selection$query_text)) # Used custom format
})

test_that("KNNFewShot merge_demos preserves original demos", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig, type = "predict")

  # Add initial demos to module
  mod$demos <- list(
    list(
      inputs = list(question = "Original Q1"),
      output = "Original A1"
    )
  )

  trainset <- data.frame(
    question = c("Train Q1", "Train Q2"),
    answer = c("Train A1", "Train A2"),
    stringsAsFactors = FALSE
  )

  # With merge_demos = TRUE
  tp <- KNNFewShot(k = 1L, vectorizer = fake_vectorizer, merge_demos = TRUE)
  compiled <- compile(tp, mod, trainset)

  result <- compiled$forward(
    list(question = "Test Q"),
    .llm = mock_llm
  )

  selection <- compiled$last_selection()
  # Should have original demo + 1 KNN demo = 2 total
  expect_equal(selection$n_demos, 2)
})

test_that("KNNFewShotModule can be deep copied", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = c("Q1", "Q2"),
    answer = c("A1", "A2"),
    stringsAsFactors = FALSE
  )

  tp <- KNNFewShot(k = 1L, vectorizer = fake_vectorizer)
  compiled <- compile(tp, mod, trainset)

  # Run once to populate state
  compiled$forward(list(question = "Test"), .llm = mock_llm)

  # Deep copy
  copied <- compiled$deepcopy()

  expect_s3_class(copied, "KNNFewShotModule")
  expect_equal(copied$k, compiled$k)
  expect_equal(copied$config$compiled, TRUE)

  # Modifying copy should not affect original
  copied$state$knn_queries <- 999L
  expect_equal(compiled$state$knn_queries, 1L)
})

test_that("KNNFewShotModule reset clears selection history", {
  sig <- Signature(
    inputs = list(input(name = "question", class = S7::class_character)),
    output_type = ellmer::type_string(),
    instructions = "Answer"
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = c("Q1", "Q2"),
    answer = c("A1", "A2"),
    stringsAsFactors = FALSE
  )

  tp <- KNNFewShot(k = 1L, vectorizer = fake_vectorizer)
  compiled <- compile(tp, mod, trainset)

  # Run a few times
  compiled$forward(list(question = "Test1"), .llm = mock_llm)
  compiled$forward(list(question = "Test2"), .llm = mock_llm)
  expect_equal(compiled$state$knn_queries, 2L)
  expect_length(compiled$state$demo_selections, 2)

  # Reset
  compiled$reset()
  expect_equal(compiled$state$knn_queries, 0L)
  expect_length(compiled$state$demo_selections, 0)
})

test_that("cosine_similarity computes correctly", {
  # Test vectors
  x <- c(1, 0, 0)
  y <- matrix(
    c(
      1,
      0,
      0, # Same as x, similarity = 1
      0,
      1,
      0, # Orthogonal, similarity = 0
      -1,
      0,
      0 # Opposite, similarity = -1
    ),
    nrow = 3,
    byrow = TRUE
  )

  similarities <- cosine_similarity(x, y)
  expect_equal(similarities[1], 1, tolerance = 1e-10)
  expect_equal(similarities[2], 0, tolerance = 1e-10)
  expect_equal(similarities[3], -1, tolerance = 1e-10)
})

test_that("find_knn returns correct indices", {
  query <- c(1, 0, 0)
  train_embeddings <- matrix(
    c(
      0.9,
      0.1,
      0, # Most similar
      0.5,
      0.5,
      0, # Less similar
      0,
      0,
      1, # Orthogonal
      0.8,
      0.2,
      0 # Second most similar
    ),
    nrow = 4,
    byrow = TRUE
  )

  # Find top 2
  indices <- find_knn(query, train_embeddings, 2)
  expect_equal(indices[1], 1) # Most similar
  expect_equal(indices[2], 4) # Second most similar
})

test_that("KNNFewShot handles vectorizer dimension mismatches", {
  bad_vectorizer <- function(texts) {
    # Returns wrong number of rows
    matrix(runif(5 * 10), nrow = 5)
  }

  sig <- Signature(
    inputs = list(input(name = "question")),
    output_type = ellmer::type_string()
  )
  mod <- module(signature = sig, type = "predict")

  trainset <- data.frame(
    question = c("Q1", "Q2", "Q3"),
    answer = c("A1", "A2", "A3"),
    stringsAsFactors = FALSE
  )

  tp <- KNNFewShot(k = 2L, vectorizer = bad_vectorizer)

  expect_error(
    compile(tp, mod, trainset),
    "wrong number of embeddings"
  )
})
