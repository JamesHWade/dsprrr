# Getting Started with dsprrr

## What is dsprrr?

dsprrr brings [DSPy](https://github.com/stanfordnlp/dspy) to R. Instead
of tweaking prompt strings, you declare what you want and let the
framework handle how to ask for it.

``` r
library(dsprrr)
library(ellmer)
```

## Three Ways to Use dsprrr

### 1. Quick Calls with `dsp()`

If you use ellmer, this will feel familiar.
[`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md) is like
`chat$chat_structured()` but with signatures:

``` r
chat <- chat_openai()

# One-liner structured calls
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"

# Signatures define input AND output structure
chat |> dsp(
  "text -> sentiment: enum('positive', 'negative', 'neutral')",
  text = "This is great!"
)
#> "positive"

# Even simpler: auto-detect Chat from API keys
dsp("capital: string", country = "France")
#> "Paris"
```

### 2. Reusable Modules with `as_module()`

When you’ll use the same configuration repeatedly:

``` r
classifier <- chat_openai() |>
  as_module("text -> sentiment: enum('positive', 'negative', 'neutral')")

# Reuse it
classifier$predict(text = "Love it!")
#> "positive"

classifier$predict(text = "Hate it!")
#> "negative"

# Batch processing
classifier$predict(text = c("Great!", "Awful", "Meh"))
#> c("positive", "negative", "neutral")
```

### 3. Full Control with `signature()` + `module()`

For optimization workflows and complex configurations:

``` r
sig <- signature(
 "context, question -> answer",
  instructions = "Answer based only on the provided context."
)

mod <- module(sig, type = "predict")

mod |> run(
  context = "R was created in 1993 by Ross Ihaka and Robert Gentleman.",
  question = "Who created R?",
  .llm = chat_openai()
)
#> "Ross Ihaka and Robert Gentleman"
```

## Signatures

Signatures declare what goes in and what comes out.

### String Notation

``` r
# Basic
signature("text -> summary")

# With output type
signature("review -> rating: enum('1', '2', '3', '4', '5')")

# Multiple inputs
signature("context, question -> answer")

# Multiple outputs
signature("text -> sentiment, confidence: number")

# With instructions
signature(
  "code -> explanation",
  instructions = "Explain like I'm 5."
)
```

### Explicit Notation

For complex output structures:

``` r
sig <- signature(
  inputs = list(
    input("article", description = "News article to analyze")
  ),
  output_type = type_object(
    headline = type_string(),
    entities = type_array(type_string()),
    sentiment = type_enum(values = c("positive", "negative", "neutral"))
  ),
  instructions = "Extract key information from the article."
)
```

## Modules

Modules wrap signatures with configuration.

### PredictModule (default)

Standard text generation:

``` r
mod <- signature("text -> summary") |>
  module(
    type = "predict",
    template = "Summarize this:\n\n{text}"
  )

mod |> run(text = "Long article here...", .llm = chat)
```

### ReactModule (with tools)

For agents that can use tools:

``` r
# Define a tool
calc_tool <- ellmer::tool(
 function(expression) eval(parse(text = expression)),
  name = "calculate",
  description = "Evaluate a math expression",
  arguments = list(expression = ellmer::type_string())
)

# Create agent with tools
agent <- signature("problem -> solution") |>
  module(type = "react", tools = list(calc_tool), max_iterations = 5)

agent$predict(problem = "What is 15% of 847?")
```

## Batch Processing

### Multiple Inputs

``` r
mod <- signature("text -> sentiment") |> module(type = "predict")

# Vector input
mod |> run(
  text = c("Great!", "Terrible!", "Okay I guess"),
  .llm = chat
)
#> c("positive", "negative", "neutral")
```

### Dataset Processing

``` r
data <- tibble::tibble(
  text = c("Love it", "Hate it", "It's fine")
)

mod |> run_dataset(data, .llm = chat)
#> # A tibble with result column added
```

### tidymodels Interface

``` r
predict(mod, new_data = data)
```

## Streaming

For long responses, stream the output:

``` r
mod <- signature("topic -> story") |> module(type = "predict")

# With callback
mod$stream(topic = "a cat learning to code", callback = cat)

# Or get a generator
gen <- mod$stream(topic = "space exploration")
coro::loop(for (chunk in gen) cat(chunk))
```

## Async Operations

Run multiple calls in parallel:

``` r
library(promises)

# Fire off multiple requests
p1 <- run_async(mod, text = "First text")
p2 <- run_async(mod, text = "Second text")
p3 <- run_async(mod, text = "Third text")

# Wait for all
promise_all(p1, p2, p3) |>
  then(function(results) {
    # All three results ready
  })
```

## Tracing

Every call is traced for debugging:

``` r
# Run some predictions
mod$predict(text = "Hello")
mod$predict(text = "World")

# See what happened
mod$trace_summary()
#> n_traces: 2
#> total_tokens: 156
#> total_cost: $0.0012

# Export for analysis
traces <- export_traces(mod, format = "tibble")

# Get the most recent trace
get_last_trace()
```

## Optimization

### Few-Shot Examples

Add demonstrations to improve performance:

``` r
mod <- signature("problem -> answer: number") |>
  module(
    type = "predict",
    demos = list(
      list(
        inputs = list(problem = "2 + 2"),
        output = list(answer = 4)
      ),
      list(
        inputs = list(problem = "10 * 5"),
        output = list(answer = 50)
      )
    )
  )
```

### Grid Search

Find the best configuration:

``` r
trainset <- dsp_trainset(
  text = c("Great!", "Awful", "Meh"),
  sentiment = c("positive", "negative", "neutral")
)

mod$optimize_grid(
  devset = trainset,
  metric = metric_exact_match(),
  parameters = list(
    temperature = c(0.1, 0.5, 1.0)
  )
)

# Check results
module_trials(mod)
mod$state$best_score
```

### Teleprompters

Automated optimization strategies:

``` r
# Add few-shot examples from training data
optimized <- compile_module(
  program = mod,
  teleprompter = LabeledFewShot(k = 3),
  trainset = trainset
)

# Grid search over instruction variants
optimized <- compile_module(
  program = mod,
  teleprompter = GridSearchTeleprompter(
    variants = tibble::tibble(
      id = c("concise", "detailed"),
      instructions_suffix = c(". Be brief.", ". Explain your reasoning.")
    ),
    metric = metric_exact_match(),
    k = 2
  ),
  trainset = trainset
)
```

## Metrics

Built-in evaluation metrics:

``` r
# Exact string match
metric_exact_match()
metric_exact_match(field = "answer", ignore_case = TRUE)

# Token overlap (F1)
metric_f1()
metric_f1(field = "summary")

# Substring check
metric_contains("error", ignore_case = TRUE)

# Custom logic
metric_custom(function(pred, expected) {
  nchar(pred) <= 100
}, name = "concise")

# Threshold wrapper
metric_threshold(metric_f1(), threshold = 0.8)
```

## Default Chat

dsprrr can auto-detect your LLM from environment variables:

``` r
# Set once
set_default_chat(chat_openai())

# Now you don't need .llm everywhere
dsp("q -> a", q = "Hello?")
mod |> run(text = "Hello")

# Or let it auto-detect from OPENAI_API_KEY, ANTHROPIC_API_KEY, etc.
get_default_chat(create = TRUE)

# Clear when done
clear_default_chat()
```

## Production Patterns

### Save and Restore Modules

``` r
library(pins)
board <- board_local()

# Save optimized configuration
pin_module_config(mod, board, "my-classifier")

# Restore later
mod <- restore_module_config(board, "my-classifier")
```

### vitals Integration

``` r
library(vitals)

# Use dsprrr modules as vitals solvers
solver <- as_vitals_solver(mod)

# Use vitals scorers as dsprrr metrics
metric <- as_dsprrr_metric(model_graded_qa())
```

## Quick Reference

| Task            | Code                                            |
|-----------------|-------------------------------------------------|
| Quick call      | `chat \|> dsp("q -> a", q = "Hello")`           |
| Reusable module | `chat \|> as_module("text -> sentiment")`       |
| Full control    | `signature(...) \|> module(...) \|> run(...)`   |
| Batch           | `run(mod, text = c("a", "b", "c"))`             |
| Dataset         | `run_dataset(mod, df)`                          |
| Stream          | `mod$stream(..., callback = cat)`               |
| Async           | `run_async(mod, ...)`                           |
| Optimize        | `mod$optimize_grid(devset, metric, parameters)` |
| Trace           | `mod$trace_summary()`                           |

## Next Steps

- [`vignette("compilation-optimization")`](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)
  — Deep dive into optimization
- [`vignette("vitals-integration")`](https://jameshwade.github.io/dsprrr/articles/vitals-integration.md)
  — Evaluation with vitals
- [`vignette("orchestration")`](https://jameshwade.github.io/dsprrr/articles/orchestration.md)
  — Production workflows
