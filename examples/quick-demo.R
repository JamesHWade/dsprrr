# Quick demo of dsprrr's improved expressive syntax
# ==================================================

library(dsprrr)
library(ellmer)

# Configure LLM (replace with your provider)
llm <- chat_openai(
  api_key = Sys.getenv("OPENAI_API_KEY"),
  model = "gpt-4o-mini"
)

# 1. SIMPLE CLASSIFICATION - Look how easy this is!
# --------------------------------------------------

sentiment_classifier <- Predict(
  signature = sig("text -> sentiment: enum('positive', 'negative', 'neutral')")
)

# That's it! Compare with the old way:
# OLD: Signature(
#        inputs = list(input("text", S7::class_character)),
#        output_type = type_enum(c("positive", "negative", "neutral"))
#      )

# 2. QUESTION ANSWERING - Multiple inputs are easy
# -------------------------------------------------

qa_system <- Predict(
  signature = sig("context, question -> answer"),
  template = "Context: {context}\n\nQuestion: {question}\n\nAnswer:"
)

# 3. MORE COMPLEX EXAMPLE - Structured outputs
# ---------------------------------------------

# When you need structured outputs, you can still use explicit notation
# but inputs are much simpler now!
analyzer <- Predict(
  signature = Signature(
    inputs = list(
      input("text"),                    # Defaults to string!
      input("max_items", "integer")     # Simple type specification
    ),
    output_type = type_object(
      summary = type_string(),
      keywords = type_array(type_string()),
      sentiment = type_enum(c("positive", "negative", "neutral")),
      confidence = type_number()
    )
  )
)

# 4. USE THE MODULES
# ------------------

if (interactive()) {
  # Test sentiment classification
  result1 <- forward(
    sentiment_classifier,
    text = "This new syntax is so much cleaner!",
    .llm = llm
  )
  print(result1)  # Should be "positive"

  # Test QA
  result2 <- forward(
    qa_system,
    context = "dsprrr is an R package for building AI applications with LLMs.",
    question = "What is dsprrr?",
    .llm = llm
  )
  print(result2)
}

# 5. COMPARISON: OLD vs NEW
# --------------------------

cat("\n=== EXPRESSIVENESS COMPARISON ===\n\n")

cat("OLD WAY (verbose and confusing):\n")
cat("--------------------------------\n")
cat('Signature(\n')
cat('  inputs = list(\n')
cat('    input("text", S7::class_character, "Text to analyze")\n')
cat('  ),\n')
cat('  output_type = type_enum(c("positive", "negative")),\n')
cat('  instructions = "Classify sentiment"\n')
cat(')\n\n')

cat("NEW WAY (simple and expressive):\n")
cat("--------------------------------\n")
cat('sig("text -> sentiment: enum(\'positive\', \'negative\')",\n')
cat('    instructions = "Classify sentiment")\n\n')

cat("The improvement is dramatic! Users no longer need to:\n")
cat("- Know about S7 classes\n")
cat("- Write verbose input specifications\n")
cat("- Deal with complex list structures for simple cases\n")
cat("\nJust write what you mean in a natural, DSPy-style notation!\n")