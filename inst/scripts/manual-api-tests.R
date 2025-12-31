# Manual API Tests for dsprrr
#
# Run this script interactively to test dsprrr functionality with live APIs.
# Requires API keys to be set (e.g., OPENAI_API_KEY).
#
# Usage:
#   source("inst/scripts/manual-api-tests.R")
#
# Or run sections interactively in RStudio/VS Code.

# Load packages
library(dsprrr)
library(ellmer)

# Set up LLM
llm <- chat_openai(model = "gpt-4o-mini")
# llm <- chat_anthropic(model = "claude-sonnet-4-20250514")

# =============================================================================
# 1. Test basic module functionality with STRUCTURED return format
# =============================================================================
cat("\n=== Test 1: Basic Module ===\n")

mod <- module(signature("question -> answer"), type = "predict")

# Use structured format to get cost/token metadata
result <- run(mod, question = "What is 2+2?", .llm = llm, .return_format = "structured")
print(result)

# Now these should work (structured format includes metadata)
cat("\nCost: "); print(get_cost(result))
cat("Tokens: "); print(get_tokens(result))
cat("\nSession cost:\n"); print(session_cost())

# =============================================================================
# 2. Test parallel batch processing
# =============================================================================
cat("\n=== Test 2: Parallel Batch Processing ===\n")

# For true parallel: omit .llm so each worker creates its own client
# This requires OPENAI_API_KEY to be set in environment
batch_results <- run(
  mod,
  question = c("What is 2+2?", "Capital of France?", "Who wrote Hamlet?"),
  .parallel = TRUE,  # No .llm - workers create their own clients
  .return_format = "structured"
)
print(batch_results)

# Alternative: Sequential batch (use .llm, no .parallel)
cat("\nSequential batch for comparison:\n")
seq_results <- run(
  mod,
  question = c("What is 2+2?", "Capital of France?"),
  .llm = llm,
  .return_format = "structured"
)
print(seq_results)

# =============================================================================
# 3. Test RAG module empty retrieval logging
# =============================================================================
cat("\n=== Test 3: RAG Module Empty Retrieval ===\n")

empty_retriever <- function(query, k = 3) {
 character(0)
}

rag_mod <- rag_module(
 signature = signature("question -> answer"),
 retriever = empty_retriever
)

# Should log: "No documents matched the retrieval query"
rag_result <- run(rag_mod, question = "What is dsprrr?", .llm = llm)
print(rag_result)

# =============================================================================
# 4. Test ellmer tool integration
# =============================================================================
cat("\n=== Test 4: ellmer Tool Integration ===\n")

sentiment_mod <- module(
 signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
 type = "predict"
)

tool <- as_ellmer_tool(
 sentiment_mod,
 name = "analyze_sentiment",
 description = "Analyze sentiment of text",
 .llm = llm
)
print(tool)

# Register and use
agent <- chat_openai(model = "gpt-4o-mini")
register_dsprrr_tool(agent, sentiment_mod, name = "sentiment_analyzer", .llm = llm)
response <- agent$chat("Analyze the sentiment of: 'I absolutely love this!'")
cat("\nAgent response:\n")
print(response)

# =============================================================================
# 5. Test streaming (correct signature)
# =============================================================================
cat("\n=== Test 5: Streaming ===\n")

stream_mod <- module(signature("topic -> summary"), type = "predict")

cat("Streaming response:\n")
stream_mod$stream(
 topic = "Benefits of R programming in 2-3 sentences",
 .llm = llm,
 callback = function(chunk) cat(chunk)
)
cat("\n")

# =============================================================================
# 6. Test timeout handling (optional - uncomment to test)
# =============================================================================
# cat("\n=== Test 6: Timeout Handling ===\n")
#
# # Set very short timeout to force timeout errors
# options(dsprrr.parallel_timeout = 0.001)  # 1ms
#
# tryCatch({
#   run(mod, question = c("test1", "test2"), .llm = llm, .parallel = TRUE)
# }, error = function(e) {
#   cat("Expected timeout error:\n")
#   print(e)
# })
#
# # Reset timeout
# options(dsprrr.parallel_timeout = NULL)

# =============================================================================
# Summary
# =============================================================================
cat("\n=== Session Summary ===\n")
print(session_cost())
cat("\nAll manual tests completed!\n")
