test_that("rag_module() runs through run() with a custom retriever", {
  local_reset_cache()
  prompts <- character()
  llm <- new_test_chat(
    chat_structured = function(prompt, ...) {
      prompts <<- c(prompts, prompt)
      list(answer = "Paris")
    }
  )
  retriever <- function(query, k) {
    c("Paris is the capital of France.", "Lyon is in France.")
  }
  rag <- rag_module(
    "question, relevant_context -> answer",
    retriever = retriever,
    k = 2L
  )

  result <- run(rag, question = "What is the capital of France?", .llm = llm)

  expect_equal(result$answer, "Paris")
  expect_match(
    prompts[[1]],
    "[1] Paris is the capital of France.",
    fixed = TRUE
  )
  expect_equal(rag$supplied_inputs(), "relevant_context")
})

test_that("wrappers inherit the inputs a RAG module supplies", {
  local_reset_cache()
  llm <- new_test_chat(chat_structured = function(...) list(answer = "Paris"))
  rag <- rag_module(
    "question, relevant_context -> answer",
    retriever = function(query, k) "Paris is the capital of France."
  )
  wrapped <- best_of_n(rag, N = 2L)

  result <- run(wrapped, question = "Capital of France?", .llm = llm)

  expect_equal(result$answer, "Paris")
  expect_equal(wrapped$supplied_inputs(), "relevant_context")
})

test_that("run_dataset() and evaluate() need no column for supplied inputs", {
  local_reset_cache()
  llm <- new_test_chat(chat_structured = function(...) list(answer = "Paris"))
  rag <- rag_module(
    "question, relevant_context -> answer",
    retriever = function(query, k) "Paris is the capital of France."
  )
  data <- tibble::tibble(question = "Capital of France?", answer = "Paris")

  results <- run_dataset(rag, data, .llm = llm)
  expect_equal(results$result[[1]]$answer, "Paris")

  scores <- evaluate(
    rag,
    data,
    metric = metric_exact_match(field = "answer"),
    .llm = llm
  )
  expect_equal(scores$mean_score, 1)
})

test_that("assertion and ensemble wrappers inherit supplied inputs", {
  local_reset_cache()
  llm <- new_test_chat(chat_structured = function(...) list(answer = "Paris"))
  make_rag <- function() {
    rag_module(
      "question, relevant_context -> answer",
      retriever = function(query, k) "Paris is the capital of France."
    )
  }

  checked <- with_assertions(
    make_rag(),
    assertions = list(assert_not_empty("answer"))
  )
  expect_equal(checked$supplied_inputs(), "relevant_context")
  expect_equal(
    run(checked, question = "Capital?", .llm = llm, .cache = FALSE)$answer,
    "Paris"
  )

  voters <- ensemble(list(make_rag(), make_rag()), reduce_fn = reduce_first())
  expect_equal(voters$supplied_inputs(), "relevant_context")
  expect_equal(
    run(voters, question = "Capital?", .llm = llm, .cache = FALSE)$answer,
    "Paris"
  )
})
