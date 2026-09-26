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
