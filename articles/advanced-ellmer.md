# Advanced ellmer Integration

``` r
library(dsprrr)
library(ellmer)
```

## Introduction

dsprrr is built on top of ellmer, Posit’s R package for LLM
interactions. This vignette explores advanced integration patterns that
leverage ellmer’s full capabilities.

## Parallel Processing

dsprrr supports two parallel processing methods for batch operations:

### Method 1: mirai (Default)

The default parallel method uses mirai for multi-process parallelism:

``` r
mod <- module(signature("text -> sentiment"), type = "predict")

# Process multiple items in parallel using mirai
results <- run(
  mod,
  text = c("I love this!", "This is terrible", "It's okay"),
  .parallel = TRUE,
  .parallel_method = "mirai"  # Default
)
```

### Method 2: ellmer Native

For more efficient parallelism, use ellmer’s native
[`parallel_chat_structured()`](https://ellmer.tidyverse.org/reference/parallel_chat.html):

``` r
results <- run(
  mod,
  text = c("I love this!", "This is terrible", "It's okay"),
  .parallel = TRUE,
  .parallel_method = "ellmer"  # Uses ellmer's parallel HTTP requests
)
```

The ellmer method is more efficient because: - Single process (no R
subprocess overhead) - Native async HTTP requests - Better error
handling - Automatic rate limit handling

## Converting Modules to ellmer Tools

dsprrr modules can be converted to ellmer tools for use in agentic
workflows:

``` r
# Create a sentiment analysis module
sentiment_mod <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
  type = "predict"
)

# Convert to an ellmer tool
sentiment_tool <- as_ellmer_tool(
  sentiment_mod,
  name = "analyze_sentiment",
  description = "Analyze the sentiment of text"
)

# The tool can now be registered with any Chat
chat <- chat_openai()
chat$register_tool(sentiment_tool)

# The LLM can now use the sentiment tool
chat$chat("Analyze the sentiment of: 'I love this product!'")
```

### Registering Tools Directly

For convenience, use
[`register_dsprrr_tool()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_tool.md)
to create and register in one step:

``` r
chat <- chat_openai()

# Create module
qa_mod <- module(
  signature("question -> answer", instructions = "Answer factual questions"),
  type = "predict"
)

# Register directly
register_dsprrr_tool(chat, qa_mod, name = "knowledge_lookup")

# Use the tool
chat$chat("Use knowledge_lookup to find: What is the capital of France?")
```

## Leveraging ellmer’s Cost Tracking

ellmer provides robust token and cost tracking. dsprrr integrates with
this via accessor functions:

``` r
# After running some predictions
result <- dsp("question -> answer", question = "What is 2+2?")

# Get cost and token info from results using public accessors
get_cost(result)    # Cost in dollars
get_tokens(result)  # Token counts

# For session-wide aggregates
session_cost()
```

### Token Usage from Batch Results

``` r
# Run batch predictions
mod <- module(signature("q -> a"), type = "predict")
results <- run(mod, q = c("Hello", "World"), .return_format = "batch")

# Get aggregated cost and tokens from batch
get_cost(results)    # Total cost for batch
get_tokens(results)  # Total tokens for batch
```

## Advanced Chat Patterns

### Using Chat Objects Across Multiple Calls

``` r
# Create a Chat and reuse it
chat <- chat_openai()

# First call - establishes conversation
result1 <- chat |> dsp("q -> a", q = "What is R?")

# Second call - same Chat, conversation continues
result2 <- chat |> dsp("q -> a", q = "What about Python?")

# The Chat remembers context from previous calls
```

### Default Chat Management

``` r
# Set a default Chat for all dsp() calls
set_default_chat(chat_openai(model = "gpt-4o"))

# Now dsp() uses the default
result <- dsp("q -> a", q = "What is 2+2?")

# Check current configuration
dsprrr_sitrep()

# Clear when done
clear_default_chat()
```

## Multimodal Support

dsprrr inherits ellmer’s multimodal capabilities:

``` r
mod <- module(
  signature("image, question -> answer"),
  type = "predict"
)

# Pass an image via ellmer Content objects
result <- run(
  mod,
  image = ellmer::ContentImageRemote("https://example.com/image.jpg"),
  question = "What is in this image?"
)
```

## Streaming Responses

For long-form generation, use streaming:

``` r
mod <- module(
  signature("topic -> essay"),
  type = "predict"
)

# Stream with callback - pass named arguments directly
mod$stream(
  topic = "The future of AI",
  callback = function(chunk) cat(chunk)
)

# Or use async streaming with promises
library(promises)
stream_async(mod, topic = "The future of AI") %...>%
  print()  # Prints the final result when complete
```

## Error Handling and Retries

ellmer handles retries automatically. Configure via options:

``` r
# Set max retries (default is 3)
options(ellmer_max_tries = 5)

# Set timeout (default is 60 seconds)
options(ellmer_timeout = 120)

# dsprrr wraps errors with helpful context
tryCatch(
  dsp("q -> a", q = "test"),
  error = function(e) {
    # Error includes model info, suggestions, etc.
    print(e)
  }
)
```

## Summary

Key integration points with ellmer: 1. **Parallel processing**: Choose
between mirai (multi-process) or ellmer native (async HTTP) 2. **Tool
integration**: Convert modules to ellmer tools with
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md)
3. **Cost tracking**: Use
[`get_cost()`](https://jameshwade.github.io/dsprrr/reference/get_cost.md),
[`get_tokens()`](https://jameshwade.github.io/dsprrr/reference/get_tokens.md),
and
[`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md)
for usage tracking 4. **Chat management**: Use
[`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/set_default_chat.md)
for session-wide defaults 5. **Multimodal**: Pass ellmer Content objects
for images, PDFs, etc. 6. **Streaming**: Use `stream()` or
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
for long-form generation 7. **Error handling**: Benefit from ellmer’s
automatic retries and dsprrr’s context-rich errors
