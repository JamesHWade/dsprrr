# Troubleshooting

This guide helps you diagnose and fix common issues when using dsprrr.

## Quick Diagnostic

When something goes wrong, start here:

``` r

library(dsprrr)

# 1. Check your configuration
dsprrr_sitrep()

# 2. If a call failed, inspect the last prompt
get_last_prompt()

# 3. View recent prompt history
inspect_history(n = 5)
```

------------------------------------------------------------------------

## Setup & Configuration Issues

### “No default Chat available”

**Error:**

    Error: No default Chat available
    ℹ Set an API key environment variable, or call `dsp_configure()`
    ℹ Supported: OPENAI_API_KEY, ANTHROPIC_API_KEY, GOOGLE_API_KEY

**Cause:** dsprrr couldn’t find an LLM provider to use.

**Solutions:**

1.  **Set an API key** (recommended):

``` r

# In your .Renviron file (run usethis::edit_r_environ())
OPENAI_API_KEY=sk-your-key-here

# Or set in R session
Sys.setenv(OPENAI_API_KEY = "sk-your-key-here")
```

2.  **Configure explicitly:**

``` r

dsp_configure(provider = "openai", model = "gpt-4o-mini")
```

3.  **Pass a Chat object directly:**

``` r

chat <- ellmer::chat_openai()
mod <- module(signature("question -> answer"))
run(mod, question = "What is 2+2?", .llm = chat)
```

### “Invalid provider”

**Error:**

    Error: Invalid provider
    ✖ Provider must be one of: openai, anthropic, google

**Solution:** Use a supported provider name:

``` r

dsp_configure(provider = "openai")    # OpenAI
dsp_configure(provider = "anthropic") # Anthropic Claude
dsp_configure(provider = "google")    # Google Gemini
```

### “chat must be an ellmer Chat object”

**Error:**

    Error: `chat` must be an ellmer Chat object

**Cause:** You passed something other than an ellmer Chat to a function
expecting one.

**Solution:**

``` r

# Wrong - passing a string
mod <- module(sig, chat = "openai")

# Correct - pass an actual Chat object
chat <- ellmer::chat_openai()
mod <- module(sig, chat = chat)
```

------------------------------------------------------------------------

## Signature Parsing Errors

### “Signature string cannot be empty”

**Error:**

    Error: Signature string cannot be empty
    ✖ Provide a signature like "question -> answer"

**Solution:** Provide a valid signature string:

``` r

# Wrong
sig <- signature("")

# Correct
sig <- signature("question -> answer")
```

### “Missing ‘-\>’ separator”

**Error:**

    Error: Missing '->' separator in signature
    ℹ Did you mean: 'question -> answer'?

**Cause:** Your signature is missing the arrow that separates inputs
from outputs.

**Solution:**

``` r

# Wrong - no arrow
sig <- signature("question answer")

# Correct
sig <- signature("question -> answer")
```

### “Use ‘-\>’ not ‘=\>’” (or ‘–\>’, ‘\<-’)

**Error:**

    Error: Invalid signature format
    ✖ Use '->' not '=>'
    ℹ Corrected: 'question -> answer'

**Cause:** You used the wrong arrow type.

**Solution:**

``` r

# Wrong arrows
signature("text => sentiment")   # JavaScript-style
signature("text --> sentiment")  # Double dash
signature("sentiment <- text")   # R assignment (reversed)

# Correct
signature("text -> sentiment")
```

### “Multiple ‘-\>’ separators found”

**Error:**

    Error: Multiple '->' separators found in signature
    ✖ Only one '->' is allowed

**Solution:** Use commas to separate multiple outputs, not additional
arrows:

``` r

# Wrong
signature("question -> reasoning -> answer")

# Correct - multiple outputs use commas
signature("question -> reasoning, answer")
```

------------------------------------------------------------------------

## Input Validation Errors

### “Missing required inputs”

**Error:**

    Error: Missing required inputs
    ✖ Missing: question
    ℹ Signature requires: context, question

**Cause:** You didn’t provide all the inputs the signature requires.

**Solution:**

``` r

sig <- signature("context, question -> answer")
mod <- module(sig)

# Wrong - missing 'context'
run(mod, question = "What is R?", .llm = chat)

# Correct
run(
  mod,
  context = "R is a programming language.",
  question = "What is R?",
  .llm = chat
)
```

### “Did you mean: …?” (typo in input name)

**Error:**

    Error: Missing required inputs
    ✖ Missing: question
      Did you mean: qeustion?

**Cause:** You likely misspelled an input name.

**Solution:** Fix the typo:

``` r

# Wrong - typo
mod <- module(signature("question -> answer"))
run(mod, qeustion = "What is 2+2?", .llm = chat)

# Correct
run(mod, question = "What is 2+2?", .llm = chat)
```

### “Ignoring unknown input”

**Warning:**

    Warning: Ignoring unknown input: `extra_field`
    ℹ Available fields: question

**Cause:** You passed an input that’s not in the signature.

**Solution:** Either remove the extra input or add it to your signature:

``` r

# This warns because 'context' isn't in the signature
mod <- module(signature("question -> answer"))
run(mod, question = "Hi", context = "extra", .llm = chat)

# Option 1: Remove extra input
run(mod, question = "Hi", .llm = chat)

# Option 2: Add to signature
context_mod <- module(signature("context, question -> answer"))
run(context_mod, context = "extra", question = "Hi", .llm = chat)
```

------------------------------------------------------------------------

## LLM & API Errors

### “Rate limit exceeded”

**Error:**

    Error: LLM call failed
    ✖ Error code: 429 - Too many requests
    ℹ Model: gpt-4o-mini via OpenAI
    ! Rate limit exceeded
    ℹ Suggestion: Wait a few seconds and try again, or use a different model

**Solutions:**

1.  **Wait and retry:**

``` r

Sys.sleep(5)
result <- run(mod, question = "Try again", .llm = chat)
```

2.  **Use a different model:**

``` r

dsp_configure(provider = "openai", model = "gpt-3.5-turbo")
```

3.  **For batch processing, add delays:**

``` r

results <- list()
mod <- module(signature("text -> result"))
for (i in seq_len(nrow(data))) {
  results[[i]] <- run(mod, text = data$text[i], .llm = chat)
  Sys.sleep(0.5)  # Rate limit buffer
}
```

### “Authentication failed”

**Error:**

    Error: LLM call failed
    ✖ Invalid API key
    ! Authentication failed
    ℹ Check that your API key is set correctly
    ℹ Run `dsprrr_sitrep()` to check configuration

**Solutions:**

1.  **Verify your API key is set:**

``` r

dsprrr_sitrep()

# Check the raw value (careful - don't share this!)
Sys.getenv("OPENAI_API_KEY")
```

2.  **Re-set your API key:**

``` r

Sys.setenv(OPENAI_API_KEY = "sk-your-actual-key")
dsp_configure()  # Re-initialize
```

3.  **Check for invisible characters** (copy-paste issues):

``` r

# Sometimes copy-pasting adds hidden characters
key <- "sk-your-key"
nchar(key)  # Should match expected length
charToRaw(key)  # Check for unexpected bytes
```

### “Request timed out”

**Error:**

    Error: LLM call failed
    ✖ Connection timed out
    ! Request timed out
    ℹ Try reducing prompt length or using a faster model

**Solutions:**

1.  **Simplify your prompt:**

``` r

# Instead of a huge context, summarize first
summarizer <- module(signature("text -> brief_summary: string[100]"))
answerer <- module(signature("context, question -> answer"))
summary <- run(summarizer, text = long_text, .llm = chat)
answer <- run(answerer, context = summary, question = q, .llm = chat)
```

2.  **Use a faster model:**

``` r

dsp_configure(provider = "openai", model = "gpt-4o-mini")  # Faster than gpt-4o
```

### “Prompt too long” / “Context length exceeded”

**Error:**

    Error: LLM call failed
    ✖ This model's maximum context length is 8192 tokens
    ! Prompt too long (15000 characters)
    ℹ Reduce input size or use a model with larger context window

**Solutions:**

1.  **Truncate your input:**

``` r

max_chars <- 10000
truncated_context <- substr(long_context, 1, max_chars)
```

2.  **Use a model with larger context:**

``` r

# GPT-4o supports 128k tokens
dsp_configure(provider = "openai", model = "gpt-4o")

# Claude supports 200k tokens
dsp_configure(provider = "anthropic", model = "claude-3-5-sonnet-latest")
```

3.  **Chunk and summarize:**

``` r

# Process in chunks
chunks <- split_text(long_doc, chunk_size = 5000)
summarizer <- module(signature("text -> summary"))
summaries <- lapply(chunks, function(chunk) {
  run(summarizer, text = chunk, .llm = chat)
})
combiner <- module(signature("summaries -> combined"))
final <- run(
  combiner,
  summaries = paste(summaries, collapse = "\n"),
  .llm = chat
)
```

### “Response parsing failed”

**Error:**

    Error: LLM call failed
    ✖ Failed to parse JSON response
    ! Response parsing failed
    ℹ The LLM returned invalid JSON. Try simplifying the output type
    ℹ Check `get_last_prompt()` to see the raw response

**Cause:** The LLM didn’t return valid structured output.

**Solutions:**

1.  **Inspect what happened:**

``` r

get_last_prompt()  # See the prompt and response
```

2.  **Simplify your output type:**

``` r

# Complex nested types can confuse some models
# Instead of:
sig <- signature("text -> data: dict[string, list[dict[string, int]]]")

# Try simpler:
sig <- signature("text -> items: list[string]")
```

3.  **Use a more capable model:**

``` r

# GPT-4o is better at structured output than GPT-3.5
dsp_configure(provider = "openai", model = "gpt-4o")
```

4.  **Add explicit instructions:**

``` r

sig <- signature(
 "text -> result",
  instructions = "Return only valid JSON. No explanations or markdown."
)
```

### “Content was blocked by safety filters”

**Error:**

    Error: LLM call failed
    ✖ Content blocked by safety system
    ! Content was blocked by safety filters
    ℹ Rephrase your input to avoid triggering content filters

**Solution:** Rephrase your input to be less likely to trigger safety
filters. Consider the context and framing of your request.

------------------------------------------------------------------------

## Module & Optimization Errors

### “First argument must be a Signature object”

**Error:**

    Error: First argument must be a Signature object

**Solution:**

``` r

# Wrong - passing a string directly to module()
mod <- module("question -> answer")

# Correct - create signature first
sig <- signature("question -> answer")
mod <- module(sig)
```

### “Unknown module type”

**Error:**

    Error: Unknown module type: 'chain'
    ℹ Available types: predict, react

**Solution:** Use a supported module type:

``` r

mod <- module(sig)  # Standard text generation
mod <- react(sig)    # Tool-calling agent
```

### “devset must contain at least one row”

**Error:**

    Error: devset must contain at least one row

**Solution:** Ensure your development set has data:

``` r

# Check your data
nrow(devset)

# Make sure filtering didn't remove everything
devset <- train_data |> filter(category == "A")
if (nrow(devset) == 0) {
  stop("Filter removed all rows!")
}
```

### “No Chat provided and no stored Chat”

**Error:**

    Error: No Chat provided and no stored Chat in module
    ℹ Either provide `.llm` or configure default via `dsp_configure()`

**Solution:**

``` r

# Option 1: Pass .llm explicitly
result <- run(mod, question = "Hi", .llm = chat_openai())

# Option 2: Store Chat in module
mod <- module(sig, chat = chat_openai())

# Option 3: Configure default
dsp_configure(provider = "openai")
result <- run(mod, question = "Hi")
```

------------------------------------------------------------------------

## Debugging Workflow

When you encounter an issue, follow this systematic approach:

### Step 1: Check Configuration

``` r

dsprrr_sitrep()
```

This shows: - Whether a default Chat is configured - Which provider and
model are active - API key status

### Step 2: Inspect the Last Call

``` r

# See the full prompt that was sent
get_last_prompt()

# Get the latest module trace with prompts and outputs
trace <- tail(
  export_traces(mod, include_prompts = TRUE, include_outputs = TRUE),
  1
)
```

### Step 3: View History

``` r

# See recent prompts and responses
inspect_history(n = 5)

# Include full prompts
inspect_history(n = 3, include_prompts = TRUE)
```

### Step 4: Test Incrementally

``` r

# 1. Test the signature parses correctly
sig <- signature("question -> answer")
sig  # Should print without error

# 2. Test the module creates correctly
mod <- module(sig)
mod  # Should print module info

# 3. Test a simple call
result <- run(mod, question = "What is 1+1?", .llm = chat_openai())

# 4. If that works, test your actual use case
result <- run(mod, question = your_complex_question, .llm = chat_openai())
```

### Step 5: Simplify

If something complex isn’t working:

``` r

# Start with the simplest possible signature
mod <- module(signature("q -> a"))
run(mod, q = "test", .llm = chat)

# Then add complexity incrementally
run(module(signature("question -> answer")), question = "test", .llm = chat)
run(
  module(signature("question -> answer: string")),
  question = "test",
  .llm = chat
)
run(
  module(signature("context, question -> answer")),
  context = "ctx",
  question = "test",
  .llm = chat
)
```

### Step 6: Check for Known Issues

``` r

# Ensure ellmer is up to date
packageVersion("ellmer")

# Ensure dsprrr is up to date
packageVersion("dsprrr")

# Check for any warnings during package load
library(dsprrr)
```

------------------------------------------------------------------------

## Getting Help

If you’re still stuck:

1.  **Check the documentation:**

``` r

?module
?run
?signature
vignette("getting-started", package = "dsprrr")
vignette("cheatsheet", package = "dsprrr")
```

2.  **Gather diagnostic info:**

``` r

sessionInfo()
dsprrr_sitrep()
```

3.  **Create a minimal reproducible example** that shows the issue

4.  **Report issues** at: <https://github.com/JamesHWade/dsprrr/issues>
