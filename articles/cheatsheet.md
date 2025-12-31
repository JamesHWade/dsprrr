# Quick Reference

A quick reference for common dsprrr operations.

## Setup & Configuration

``` r
library(dsprrr)
library(ellmer)
```

### Configure Default LLM

``` r
# Auto-detect from environment variables (OPENAI_API_KEY, ANTHROPIC_API_KEY, etc.)
dsp_configure()

# Explicitly set provider and model
dsp_configure(provider = "openai", model = "gpt-4o-mini")
dsp_configure(provider = "anthropic", model = "claude-3-5-sonnet-latest")

# With additional parameters
dsp_configure(provider = "openai", model = "gpt-4o", temperature = 0.7)
```

### Check Configuration Status

``` r
dsprrr_sitrep()
#> dsprrr Configuration
#> ────────────────────────────────────────
#> Default Chat: ✓ Active
#>   Provider: OpenAI
#>   Model: gpt-4o-mini
```

------------------------------------------------------------------------

## Signature Notation

Signatures define what goes in and what comes out.

### Basic Format

    inputs -> outputs

### Input Types

| Notation              | Meaning             |
|-----------------------|---------------------|
| `question`            | Single string input |
| `context, question`   | Multiple inputs     |
| `items: list[string]` | Typed list input    |

### Output Types

| Notation                | R/ellmer Type                                                                | Use Case        |
|-------------------------|------------------------------------------------------------------------------|-----------------|
| `answer`                | [`type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html)  | Free-form text  |
| `answer: string`        | [`type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html)  | Explicit string |
| `score: int`            | [`type_integer()`](https://ellmer.tidyverse.org/reference/type_boolean.html) | Whole numbers   |
| `score: float`          | [`type_number()`](https://ellmer.tidyverse.org/reference/type_boolean.html)  | Decimal numbers |
| `score: number[0, 100]` | [`type_number()`](https://ellmer.tidyverse.org/reference/type_boolean.html)  | Bounded numbers |
| `valid: bool`           | [`type_boolean()`](https://ellmer.tidyverse.org/reference/type_boolean.html) | True/false      |

### Constrained Types

| Notation                                   | Description              |
|--------------------------------------------|--------------------------|
| `sentiment: enum('pos', 'neg', 'neutral')` | Fixed choices            |
| `label: Literal['a', 'b', 'c']`            | Python-style enum        |
| `summary: string[50, 200]`                 | String with length hints |

### Collection Types

| Notation                  | Description      |
|---------------------------|------------------|
| `tags: list[string]`      | List of strings  |
| `items: array(string)`    | Array notation   |
| `words: string[]`         | Bracket notation |
| `data: dict[string, int]` | Dictionary/map   |

### Optional Types

| Notation                 | Description |
|--------------------------|-------------|
| `note: Optional[string]` | May be null |

### Multiple Outputs

``` r
# Multiple output fields
sig <- signature("question -> answer, confidence: float")

# With different types
sig <- signature("text -> sentiment: enum('pos', 'neg'), score: float")
```

### With Instructions

``` r
sig <- signature(
  "context, question -> answer",
  instructions = "Answer based only on the provided context. Be concise."
)
```

------------------------------------------------------------------------

## Creating Modules

### Quick: `dsp()` for One-Off Calls

``` r
# With explicit Chat
chat <- chat_openai()
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"

# With auto-detected Chat (uses configured default)
dsp("question -> answer", question = "What is 2+2?")
```

### Reusable: `as_module()` for Repeated Use

``` r
# Create from Chat
classifier <- chat_openai() |>
  as_module("text -> sentiment: enum('positive', 'negative', 'neutral')")

# Use repeatedly
classifier$predict(text = "Love it!")
classifier$predict(text = "Hate it!")
```

### Full Control: `signature()` + `module()`

``` r
# For optimization and complex configurations
sig <- signature("context, question -> answer")
mod <- module(sig, type = "predict")

# With custom template
mod <- module(
  sig,
  type = "predict",
  template = "Context:\n{context}\n\nQuestion: {question}"
)
```

------------------------------------------------------------------------

## Module Types Decision Tree

    What do you need?
    │
    ├─ Simple text in/out → type = "predict" (PredictModule)
    │   └─ Q&A, classification, summarization, extraction
    │
    └─ Tool use / multi-step reasoning → type = "react" (ReactModule)
        └─ Agents, search, calculations, API calls

| Type        | Class           | Use Case                 |
|-------------|-----------------|--------------------------|
| `"predict"` | `PredictModule` | Standard text generation |
| `"react"`   | `ReactModule`   | Tool-calling agents      |

------------------------------------------------------------------------

## Running Modules

### Single Execution

``` r
# Using run()
result <- run(mod, question = "What is R?", .llm = chat_openai())

# Using predict method
result <- mod$predict(question = "What is R?")
```

### Batch Processing

``` r
# Vector inputs
results <- mod$predict(text = c("Great!", "Awful!", "Meh"))

# Data frame
new_data <- data.frame(text = c("A", "B", "C"))
results <- predict(mod, new_data = new_data)

# run_dataset for full control
results <- run_dataset(mod, dataset, .llm = llm)
```

### Show Prompt

``` r
# See the prompt being sent
result <- run(mod, question = "Test", .llm = llm, .show_prompt = TRUE)
```

------------------------------------------------------------------------

## Metrics & Evaluation

### Built-in Metrics

| Metric                                                                                        | Use Case                           |
|-----------------------------------------------------------------------------------------------|------------------------------------|
| [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md) | Exact string equality              |
| [`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md)                   | Token overlap similarity           |
| [`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md)       | Output contains expected substring |
| `metric_field_match(fields)`                                                                  | Match specific output fields       |
| `metric_threshold(metric, 0.8)`                                                               | Apply threshold to any metric      |
| `metric_custom(fn)`                                                                           | Custom scoring function            |

### Metric Selection Guide

    What are you measuring?
    │
    ├─ Exact correctness → metric_exact_match()
    │   └─ Facts, names, simple answers
    │
    ├─ Approximate match → metric_f1() or metric_threshold(metric_f1(), 0.8)
    │   └─ Paraphrased answers, spelling tolerance
    │
    ├─ Contains key info → metric_contains()
    │   └─ Important terms must appear
    │
    ├─ Specific fields → metric_field_match(c("answer", "score"))
    │   └─ Multi-field outputs
    │
    └─ Custom logic → metric_custom(my_scorer_fn)
        └─ Domain-specific evaluation

### Running Evaluation

``` r
# Basic evaluation
result <- evaluate(mod, testset, metric = metric_exact_match(), .llm = llm)
result$mean_score
#> 0.85

# With dsp()
result <- evaluate_dsp(
  "question -> answer",
  testset,
  metric = metric_exact_match()
)
```

------------------------------------------------------------------------

## Optimization

### Grid Search

``` r
# Search over parameters
mod$optimize_grid(
  devset = train_data,
  metric = metric_exact_match(),
  .llm = llm,
  parameters = list(
    prompt_style = c("concise", "detailed"),
    temperature = c(0.0, 0.3, 0.7)
  )
)

# Check results
module_trials(mod)
module_metrics(mod)
```

### Teleprompters

``` r
# Few-shot learning
tp <- LabeledFewShot(k = 4L, metric = metric_exact_match())
compiled <- compile(tp, mod, trainset, .llm = llm)

# Grid search teleprompter
tp <- GridSearchTeleprompter(
  instructions = c("Be concise", "Be thorough"),
  metric = metric_exact_match()
)
compiled <- compile(tp, mod, trainset, .llm = llm)
```

------------------------------------------------------------------------

## Debugging & Inspection

### Inspect Last Call

``` r
# Get the last prompt sent
get_last_prompt()
#> ── Last Prompt ──
#> Model: gpt-4o-mini via OpenAI
#> Input tokens: 45
#>
#> Question: What is 2+2?
```

### Prompt History

``` r
# View recent prompts
inspect_history(n = 5)

# Clear history
clear_prompt_history()
```

### Module Inspection

``` r
# Detailed module state
mod$inspect()

# Last trace from dsp()
get_last_trace()
```

### Traces

``` r
# Get all traces from a module
traces <- mod$get_traces()

# Export to tibble
export_traces(mod)

# Summary statistics
summarize_traces(mod)

# Clear traces
clear_traces(mod)
```

------------------------------------------------------------------------

## Production

### Save/Restore Configuration

``` r
# Save optimized config to pins board
pin_module_config(mod, board, "my-classifier-v1")

# Restore later
mod <- restore_module_config(board, "my-classifier-v1", .llm = llm)
```

### Vitals Integration

``` r
# Convert module to vitals solver
solver <- as_vitals_solver(mod)

# Use with vitals::eval_task()
library(vitals)
result <- eval_task(my_task, solver)

# Convert vitals scorer to dsprrr metric
my_metric <- as_dsprrr_metric(vitals_scorer)
```

### Project Template

``` r
# Create production-ready project structure
use_dsprrr_template("my_project")

# Validate workflow configuration
validate_workflow("workflow.yml")
```

------------------------------------------------------------------------

## Common Patterns

### RAG (Retrieval-Augmented Generation)

``` r
sig <- signature(
  "context, question -> answer",
  instructions = "Answer based only on the provided context."
)
mod <- module(sig, type = "predict")

# Use with retrieved context
result <- run(mod,
  context = retrieved_docs,
  question = user_query,
  .llm = llm
)
```

### Classification

``` r
classifier <- chat_openai() |>
  as_module("text -> label: enum('spam', 'not_spam')")

labels <- classifier$predict(text = emails)
```

### Extraction

``` r
extractor <- chat_openai() |>
  as_module("text -> entities: list[string], summary: string")

result <- extractor$predict(text = document)
result$entities
result$summary
```

### Chain of Thought (via instructions)

``` r
sig <- signature(
  "problem -> answer",
  instructions = "Think step by step. Show your reasoning before the final answer."
)
```
