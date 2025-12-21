# Getting Started with dsprrr

## Introduction

**dsprrr** is an R package that brings the power of DSPy (Declarative
Self-improving Language Programs) to the R ecosystem. It provides a
principled framework for building AI applications using Large Language
Models (LLMs), moving beyond brittle prompt strings to structured,
composable, and optimizable programs.

Instead of wrestling with prompt engineering, dsprrr lets you:

- **Declare** what you want your LLM to do using expressive DSPy-style
  signatures
- **Execute** your AI logic through modular `Predict` components
- **Compose** multiple modules into sophisticated pipelines
- **Optimize** your programs systematically (coming soon!)

### Expressive DSPy-Style Syntax

dsprrr features user-friendly syntax inspired by DSPy:

``` r
# Simple and expressive!
signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
```

## Installation and Setup

Install dsprrr from GitHub:

``` r
# install.packages("pak")
pak::pak("jameshwade/dsprrr")
```

Load the package and configure your LLM:

``` r
library(dsprrr)
library(ellmer) # For LLM providers

# Configure your LLM provider
llm <- chat_claude(
  api_key = Sys.getenv("ANTHROPIC_API_KEY"),
  model = "claude-sonnet-4-20250514"
)
```

## Quick Start

Here’s the simplest way to get started with dsprrr using R’s pipe
operator:

``` r
# The R way: Build and execute with pipes!
result <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')"
) |>
  module(type = "predict", template = "Analyze: {text}") |>
  run(text = "This package is amazing!", .llm = llm)

print(result) # "positive"
```

That’s it! No complex setup, just simple and expressive code that flows
naturally with R’s pipe operator (`|>`).

### Flexible API Design

dsprrr provides both user-friendly functions and direct constructor
access:

``` r
# Recommended approach: Using the main API functions
result1 <- signature("text -> summary") |>
  module(type = "predict", template = "Summarize: {text}") |>
  run(
    text = "The tidyverse is a collection of R packages designed for data science. All packages share an underlying design philosophy, grammar, and data structures.",
    .llm = llm
  )

# Alternative: Using the Signature constructor directly
# Note: This approach gives more control over structured types
sig <- Signature(
  inputs = list(input("text")),
  output_type = ellmer::type_string()
)
result2 <- module(signature = sig, type = "predict", template = "Summarize: {text}") |>
  run(
    text = "The tidyverse is a collection of R packages designed for data science. All packages share an underlying design philosophy, grammar, and data structures.",
    .llm = llm
  )

# The function approach is recommended for most users
```

### Why Pipes and Consistency Matter

The combination of pipes and consistent naming makes dsprrr feel truly
native to R:

``` r
# Clear, readable, and predictable
sentiment_analysis <- signature("text -> sentiment") |>
  module(type = "predict") |>
  run(text = "I love this consistent API!", .llm = llm)

print(sentiment_analysis)

# Consistent API: signature(), module(), run()
# Or use constructors directly: Signature(), module(), run()

# Future vision: Build complex pipelines naturally
# analysis <- raw_data |>
#   preprocess() |>
#   extract_insights() |>
#   generate_report() |>
#   translate(to = "Spanish")
```

## Core Concepts

### 1. Signatures: Declaring LLM Operations

A `Signature` defines the schema for an LLM operation - what inputs it
takes, what output it produces, and optional instructions for the task.

dsprrr supports two ways to create signatures:

1.  **String notation** (recommended for simplicity): DSPy-style compact
    strings like `"text -> sentiment"`
2.  **Explicit notation** (for complex cases): Full control over
    input/output specifications

#### Unified Type System

dsprrr uses a consistent type system based on ellmer for both inputs and
outputs. You no longer need to know about S7 classes!

**For inputs**, you can use: - Simple strings: `"string"`, `"number"`,
`"boolean"`, `"integer"` - Default behavior: Omit type and it defaults
to string - Ellmer types:
[`type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
[`type_number()`](https://ellmer.tidyverse.org/reference/type_boolean.html),
etc. for advanced needs

**For outputs**, you have: - String notation in signatures:
`"-> answer: string"`, `"-> score: number[0,100]"` - Ellmer types for
complex structures: `type_object(...)`, `type_array(...)` - Enums:
`type_enum(values = c("a", "b", "c"))`

``` r
# DSPy-style string notation (recommended for simplicity)
sentiment_sig <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the sentiment of the given text."
)

# Or even simpler without explicit type
simple_sig <- signature("text -> sentiment")

# Multiple inputs
qa_sig <- signature("context, question -> answer")

# With type constraints
summary_sig <- signature("text -> summary: string[50, 200]")

print(sentiment_sig)
```

### 2. Predict Modules: Executing LLM Operations

A `Predict` module pairs a signature with an optional template to create
an executable unit. With pipes, this becomes beautifully concise:

``` r
# Create and execute in one pipeline
result <- sentiment_sig |>
  module(type = "predict", template = "Text: {text}\n\nSentiment:") |>
  run(text = "This package makes LLM programming so much easier!", .llm = llm)

print(result) # "positive"

# Or save the module for reuse
sentiment_classifier <- sentiment_sig |>
  module(type = "predict", template = "Text: {text}\n\nSentiment:")

# Then use it multiple times with pipes
result1 <- sentiment_classifier |>
  run(text = "I love this approach!", .llm = llm)

result2 <- sentiment_classifier |>
  run(text = "This is confusing.", .llm = llm)
```

## Practical Examples

### Example 1: Classification with Confidence Scores

Let’s build a more sophisticated classifier that returns both a label
and confidence score:

``` r
# Method 1: Simple classification with pipes
simple_result <- signature(
  "sentence -> sentiment: enum('positive', 'negative', 'neutral')"
) |>
  module(type = "predict", template = "Analyze: {sentence}") |>
  run(
    sentence = "The product works okay, but the support is terrible.",
    .llm = llm
  )

# Method 2: Structured output with pipes (for complex cases)
result <- Signature(
  inputs = list(
    input("sentence", description = "Sentence to classify")
  ),
  output_type = type_object(
    sentiment = type_enum(values = c("positive", "negative", "neutral")),
    confidence = type_number(),
    reasoning = type_string()
  ),
  instructions = "Classify the sentiment and provide confidence with reasoning."
) |>
  module(type = "predict", template = "Analyze this sentence: {sentence}") |>
  run(
    sentence = "The product works okay, but the support is terrible.",
    .llm = llm
  )

# Result structure:
# list(
#   sentiment = "negative",
#   confidence = 0.75,
#   reasoning = "Mixed review with stronger negative sentiment about support"
# )
```

### Example 2: Information Extraction

Extract structured information from unstructured text:

``` r
# Simple extraction with pipes
simple_info <- signature("article -> title, summary") |>
  module(type = "predict") |>
  run(article = "Your article text here...", .llm = llm)

# Complex structured extraction with pipes
text <- "Apple Inc. announced its latest iPhone 15 today in Cupertino.
         CEO Tim Cook highlighted the new camera system and A17 chip.
         The device will be available starting September 22nd for $799."

info <- Signature(
  inputs = list(
    input("article", description = "News article text")
  ),
  output_type = type_object(
    title = type_string(),
    entities = type_array(
      type_object(
        name = type_string(),
        type = type_enum(
          values = c("person", "organization", "location", "product")
        )
      )
    ),
    key_facts = type_array(type_string()),
    summary = type_string(description = "Brief summary")
  ),
  instructions = "Extract structured information from the article."
) |>
  module(type = "predict") |>
  run(article = text, .llm = llm)

# Access structured output
print(info$title) # "Apple Announces iPhone 15"
print(info$entities) # List of entities with names and types
print(info$key_facts) # Vector of key facts
print(info$summary) # Brief summary
```

### Example 3: Question Answering with Context

Build a simple RAG (Retrieval-Augmented Generation) system:

``` r
context <- "The R language was created by Ross Ihaka and Robert Gentleman
           at the University of Auckland, New Zealand. The project was
           conceived in 1992, with an initial version released in 1995."

# Approach 1: Simple QA with pipes
simple_answer <- signature("context, question -> answer") |>
  module(
    type = "predict",
    template = "Context: {context}\n\nQuestion: {question}"
  ) |>
  run(
    context = context,
    question = "Who created the R language?",
    .llm = llm
  )

# Approach 2: Structured output with pipes for more control
result <- Signature(
  inputs = list(
    input("context", description = "Relevant context information"),
    input("question", description = "Question to answer")
  ),
  output_type = type_object(
    answer = type_string(),
    confidence = type_enum(values = c("high", "medium", "low")),
    sources_used = type_boolean()
  ),
  instructions = "Answer the question based on the provided context.
                 Indicate if you used the context in your answer."
) |>
  module(
    type = "predict",
    template = "Context: {context}\n\nQuestion: {question}\n\nAnswer:"
  ) |>
  run(
    context = context,
    question = "Who created the R language?",
    .llm = llm
  )

print(result$answer) # "Ross Ihaka and Robert Gentleman"
print(result$confidence) # "high"
print(result$sources_used) # TRUE
```

### Example 4: Using Few-Shot Demonstrations

Improve performance by providing examples:

``` r
# Build a math solver with demonstrations using pipes
math_solver <- Signature(
  inputs = list(
    input("problem", description = "Math word problem")
  ),
  output_type = type_object(
    reasoning = type_string(),
    answer = type_number()
  ),
  instructions = "Solve the math word problem step by step."
) |>
  module(
    type = "predict",
    demos = list(
      list(
        inputs = list(
          problem = "If John has 5 apples and buys 3 more, how many does he have?"
        ),
        output = list(
          reasoning = "John starts with 5 apples and adds 3 more: 5 + 3 = 8",
          answer = 8
        )
      ),
      list(
        inputs = list(
          problem = "A store sold 12 items on Monday and twice as many on Tuesday. What's the total?"
        ),
        output = list(
          reasoning = "Monday: 12 items. Tuesday: 12 × 2 = 24 items. Total: 12 + 24 = 36",
          answer = 36
        )
      )
    )
  )

# Solve problems with pipes
result <- math_solver |>
  run(
    problem = "Sarah has 15 cookies. She gives 1/3 to her brother. How many does she have left?",
    .llm = llm
  )

print(result$reasoning) # Step-by-step solution
print(result$answer) # 10
```

### Example 5: Text Transformation Pipeline

Chain multiple operations together:

``` r
# Create reusable modules with pipes
summarizer <- Signature(
  inputs = list(input("text")),
  output_type = type_object(
    summary = type_string(description = "Summary (approx. 200 words)"),
    key_points = type_array(type_string())
  ),
  instructions = "Summarize the text and extract key points."
) |>
  module(type = "predict")

translator <- signature(
  "text, target_language -> translation",
  instructions = "Translate the text to the target language."
) |>
  module(type = "predict")

# Pipeline function leveraging R's functional programming
process_document <- function(document, language = "Spanish", llm = NULL) {
  # Step 1: Summarize with pipes
  summary_result <- summarizer |>
    run(text = document, .llm = llm)

  # Step 2: Translate with pipes
  translation <- translator |>
    run(
      text = summary_result$summary,
      target_language = language,
      .llm = llm
    )

  # Return structured result
  list(
    original_summary = summary_result$summary,
    key_points = summary_result$key_points,
    translation = translation
  )
}

# Even better: Create a pure pipe chain
process_with_pipes <- function(document, language = "Spanish", llm = NULL) {
  # Extract summary
  summary_result <- document |>
    (\(x) run(summarizer, text = x, .llm = llm))()

  # Translate and combine
  summary_result$summary |>
    (\(x) {
      run(translator, text = x, target_language = language, .llm = llm)
    })() |>
    (\(trans) {
      list(
        original_summary = summary_result$summary,
        key_points = summary_result$key_points,
        translation = trans
      )
    })()
}

# Use the pipeline
long_document <- "Your long document text here..."
result <- process_document(long_document, "French", llm = llm)
```

## Working with Types

### Input Types

dsprrr provides flexible ways to specify input types - no more S7
classes required!

``` r
# Method 1: Default to string (simplest and most common)
inp1 <- input("text")
inp2 <- input("name", description = "User's name")

# Method 2: String shortcuts (simple and readable)
inp3 <- input("count", "integer")
inp4 <- input("score", "number")
inp5 <- input("active", "boolean")

# Method 3: Ellmer types (for advanced constraints)
inp6 <- input("tags", type_array(type_string()))
inp7 <- input("status", type_enum(values = c("pending", "done")))

# Method 4: Helper functions (typed convenience)
inp8 <- input_string("description")
inp9 <- input_number("price")
inp10 <- input_boolean("enabled")
inp11 <- input_enum("priority", c("low", "medium", "high"))
```

### Output Types

dsprrr leverages ellmer’s type system for structured outputs:

``` r
# String notation with type constraints
summary_sig <- signature("text -> summary: string[50, 200]")
score_sig <- signature("essay -> score: number[0, 100]")
verify_sig <- signature("claim, evidence -> is_valid: boolean")
tags_sig <- signature("content -> tags: array(string)")

# For complex structures, use explicit notation
report_sig <- Signature(
  inputs = list(
    input("data") # Simple! No more S7::class_character
  ),
  output_type = type_object(
    metadata = type_object(
      date = type_string(),
      author = type_string(),
      version = type_number()
    ),
    sections = type_array(
      type_object(
        title = type_string(),
        content = type_string(),
        priority = type_enum(values = c("high", "medium", "low"))
      )
    ),
    approved = type_boolean()
  )
)
```

## The Power of Pipes in dsprrr

### Why Pipes Make dsprrr More R-Native

The pipe operator (`|>`) isn’t just syntactic sugar in dsprrr—it
fundamentally makes LLM programming feel like natural R:

``` r
# Traditional approach: Nested function calls (hard to read)
result1 <- run(
  module(
    signature("text -> summary"),
    type = "predict",
    template = "Summarize: {text}"
  ),
  text = "Long text...",
  .llm = llm
)

# With pipes: Clear, left-to-right flow (the R way!)
result2 <- signature("text -> summary") |>
  module(type = "predict", template = "Summarize: {text}") |>
  run(text = "Long text...", .llm = llm)
```

### Building Complex Workflows

Pipes really shine when building multi-step LLM workflows:

``` r
# A complete analysis pipeline using pipes
sentiment_extractor <- signature(
  "feedback -> sentiment, issues: array(string)"
) |>
  module(type = "predict")

response_generator <- signature("sentiment, issues -> response") |>
  module(
    type = "predict",
    template = "Sentiment: {sentiment}\nIssues: {issues}\n\nGenerate response:"
  )

sentiment_step <- function(text, llm = NULL) {
  result <- sentiment_extractor |>
    run(
      feedback = text,
      .llm = llm,
      .return_format = "structured"
    )
  result$output
}

response_step <- function(analysis, llm = NULL) {
  response_generator |>
    run(
      sentiment = analysis$sentiment,
      issues = paste(analysis$issues, collapse = ", "),
      .llm = llm
    )
}

"The product quality is great but shipping was slow and packaging was damaged." |>
  sentiment_step(llm = llm) |>
  response_step(llm = llm)
```

### Composable Module Patterns

Create libraries of reusable modules that work seamlessly with pipes:

``` r
# Create a module library
modules <- list(
  summarize = signature("text -> summary: string") |>
    module(type = "predict", template = "Summarize: {text}"),
  sentiment = signature(
    "text -> sentiment: enum('positive', 'negative', 'neutral')"
  ) |>
    module(type = "predict"),
  translate = signature("text, language -> translation") |>
    module(type = "predict", template = "Translate to {language}: {text}"),
  extract_entities = signature("text -> entities: array(string)") |>
    module(type = "predict")
)

# Compose them with pipes
analyze_text <- function(text, target_lang = "Spanish", llm = NULL) {
  list(
    summary = modules$summarize |> run(text = text, .llm = llm),
    sentiment = modules$sentiment |> run(text = text, .llm = llm),
    entities = modules$extract_entities |> run(text = text, .llm = llm),
    translation = modules$translate |>
      run(text = text, language = target_lang, .llm = llm)
  )
}
```

## Advanced Signature Features

### Multiple Output Fields

dsprrr supports multiple output fields in signatures, similar to DSPy:

``` r
# Multiple outputs with types
qa_with_confidence <- signature(
  "question -> answer: string, confidence: enum('low', 'medium', 'high')"
) |>
  module(type = "predict")

# Use it
result <- qa_with_confidence |>
  run(
    question = "What is the capital of France?",
    .llm = llm
  )

print(result$answer) # "Paris"
print(result$confidence) # "high"
```

### Complex Type Annotations

dsprrr supports advanced type annotations including Optional, Union, and
dict types:

``` r
# Optional fields (may be NULL)
extraction_sig <- signature(
  "text -> title: string, subtitle: Optional[string], author: Optional[string]"
)

# Dict types for key-value structures
metadata_sig <- signature(
  "document -> metadata: dict[string, string], tags: list[string]"
)

# Nested structures
analysis_sig <- signature(
  "data -> insights: list[dict[str, str]], summary: string[100, 500]"
)
```

### Field-Level Descriptions with ellmer Types

For detailed field descriptions, use ellmer types directly with their
`description` parameter:

``` r
# Create a signature with detailed field descriptions
analysis_module <- signature(
  inputs = list(
    input(
      "text",
      type = type_string(
        description = "The document to analyze for key themes"
      )
    )
  ),
  output_type = type_object(
    themes = type_array(
      type_string(),
      description = "Main themes found in the document, max 5"
    ),
    sentiment = type_enum(
      values = c('positive', 'negative', 'neutral', 'mixed'),
      description = "Overall sentiment of the document"
    ),
    key_quotes = type_array(
      type_string(),
      description = "Most impactful quotes that support the themes",
      required = FALSE
    )
  ),
  instructions = "Analyze the document for themes and sentiment"
) |>
  module(type = "predict")

# The field descriptions guide the LLM's understanding
result <- analysis_module |>
  run(text = "Your document text here...", .llm = llm)

# Simpler: pass description to input() directly for string types
simple_module <- signature(
  inputs = list(
    input("query", "string", description = "User's search query"),
    input("limit", "integer", description = "Max results to return")
  )
) |>
  module(type = "predict")
```

### DSPy-Compatible String Notation

dsprrr fully supports DSPy’s string notation for signatures:

``` r
# Just like DSPy!
classify <- signature("sentence -> sentiment: bool")
summarize <- signature("document -> summary")
rag <- signature("context: list[str], question: str -> answer: str")

# Multiple outputs DSPy-style
reasoning <- signature(
  "question, choices: list[str] -> reasoning: str, selection: int"
)
```

## Best Practices

### 1. Clear Instructions and Descriptions

Be specific in your signatures:

``` r
# Good: Specific instructions
good_sig <- Signature(
  inputs = list(
    input("email", description = "Customer support email requiring response")
  ),
  output_type = type_object(
    category = type_enum(
      values = c("complaint", "question", "feedback", "request")
    ),
    priority = type_enum(values = c("urgent", "normal", "low")),
    suggested_response = type_string()
  ),
  instructions = "Categorize the support email, assess priority based on
                 customer sentiment and issue severity, and draft a
                 professional response."
)
```

### 2. Use Templates for Consistent Formatting

Templates help maintain consistent prompt structure:

``` r
# Define a signature for analysis
analysis_sig <- signature(
  "data, analysis_type -> insights: array(string), summary: string"
)

analyzer <- module(
  analysis_sig,
  type = "predict",
  template = "
## Task: Analyze the following data

**Input Data:**
{data}

**Analysis Required:**
{analysis_type}

Please provide a comprehensive analysis following the specified format.
"
)
```

### 3. Leverage Demonstrations for Complex Tasks

Examples significantly improve performance on complex tasks:

``` r
# Define the code analysis signature
code_analysis_sig <- signature(
  "code -> language: string, complexity: enum('simple', 'moderate', 'complex'), has_error: boolean"
)

# Create demonstrations from validated examples
demos <- list(
  list(
    inputs = list(code = "def add(a, b): return a + b"),
    output = list(
      language = "python",
      complexity = "simple",
      has_error = FALSE
    )
  ),
  list(
    inputs = list(code = "for (i = 0; i < n; i++) { sum += arr[i]; }"),
    output = list(
      language = "c",
      complexity = "simple",
      has_error = FALSE
    )
  )
)

code_analyzer <- module(
  code_analysis_sig,
  type = "predict",
  demos = demos
)
```

### 4. Modular Design

Build reusable components:

``` r
# Define the required signatures
sentiment_sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
entity_sig <- signature("text -> entities: array(string)")
summary_sig <- signature("text -> summary: string[50, 200]")

# Create reusable modules
sentiment_module <- module(sentiment_sig, type = "predict")
entity_module <- module(entity_sig, type = "predict")
summary_module <- module(summary_sig, type = "predict")

# Compose them into pipelines
analyze_feedback <- function(text, llm = NULL) {
  list(
    sentiment = run(sentiment_module, text = text, .llm = llm),
    entities = run(entity_module, text = text, .llm = llm),
    summary = run(summary_module, text = text, .llm = llm)
  )
}
```

## Debugging and Development

Use verbose mode to see generated prompts:

``` r
# Create a simple classifier for debugging
sentiment_classifier <- signature("sentence -> sentiment") |>
  module(type = "predict")

# Enable verbose output to debug prompts
result <- run(
  sentiment_classifier,
  sentence = "Debug this prompt",
  .llm = llm,
  .verbose = TRUE
)
# Will print the generated prompt before making the LLM call
```

## Next Steps

While this vignette covers the foundational features currently available
in dsprrr, the package roadmap includes exciting upcoming features:

1.  **Optimization**: Automatic prompt optimization using techniques
    like bootstrap few-shot learning
2.  **Advanced Modules**: ChainOfThought, ReAct agents, and other
    reasoning strategies
3.  **Evaluation Framework**: Built-in metrics and evaluation pipelines
4.  **Caching and Persistence**: Automatic caching of LLM calls and
    results
5.  **Multi-model Support**: Easy switching between different LLM
    providers

## Learn More

- [Package Documentation](https://github.com/jameshwade/dsprrr)
- [DSPy Paper](https://arxiv.org/abs/2310.03714) - The original DSPy
  framework
- [ellmer Package](https://github.com/hadley/ellmer) - LLM provider
  integration

## Summary

dsprrr brings the elegance of DSPy to R with:

- **Expressive syntax**: Write `signature("text -> sentiment")` for
  clear, concise specifications
- **Simple input types**: Use `input("text")` or
  `input("count", "integer")` for straightforward type definitions
- **Flexible approaches**: Use string notation for simple cases,
  explicit notation for complex ones
- **Consistent type system**: Both inputs and outputs use the same
  ellmer-based types

## Getting Help

If you encounter issues or have questions:

1.  Check the package documentation:
    [`?dsprrr`](../reference/dsprrr-package.md)
2.  File an issue on
    [GitHub](https://github.com/jameshwade/dsprrr/issues)
3.  See more examples in other vignettes

Remember: dsprrr helps you write **programs**, not prompts. Think about
your LLM operations as composable functions with well-defined inputs and
outputs, and let the framework handle the complexity of prompt
engineering.
