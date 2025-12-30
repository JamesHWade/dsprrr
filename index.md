# dsprrr

## Programming—not prompting—LLMs in R

dsprrr brings the power of [DSPy](https://dspy.ai) to R. Instead of
wrestling with prompt strings, **declare** what you want, **compose**
modules into pipelines, and let **optimization** find the best prompts
automatically.

``` r
# Install
pak::pak("JamesHWade/dsprrr")

# That's it. Start using LLMs.
library(dsprrr)
dsp("question -> answer", question = "What is the capital of France?")
#> "Paris"
```

## Getting Started: Configure Your LLM

- OpenAI
- Anthropic
- Gemini
- Ollama
- Auto-detect

``` r
library(dsprrr)
library(ellmer)

chat <- chat_openai(model = "gpt-4o-mini")
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

``` r
library(dsprrr)
library(ellmer)

chat <- chat_claude(model = "claude-sonnet-4-20250514")
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

``` r
library(dsprrr)
library(ellmer)

chat <- chat_google_gemini(model = "gemini-2.0-flash")
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

``` r
library(dsprrr)
library(ellmer)

chat <- chat_ollama(model = "llama3.2")
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

``` r
# dsprrr auto-detects from environment variables
library(dsprrr)

# Uses OPENAI_API_KEY, ANTHROPIC_API_KEY, or GOOGLE_API_KEY
dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

## Building Modules

Modules are reusable LLM components with typed inputs and outputs.

- Classification
- Q&A
- Extraction
- Agents

``` r
# Sentiment classification with constrained output
classifier <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')"
) |> module(type = "predict")

classifier$predict(text = "I love this product!")
#> "positive"

# Batch processing
classifier$predict(text = c("Great!", "Terrible!", "It's okay"))
#> c("positive", "negative", "neutral")
```

``` r
# Context-aware QA
qa <- signature("context, question -> answer") |>
  module(type = "predict")

qa$predict(
  context = "R was created by Ross Ihaka and Robert Gentleman in 1993.",
  question = "Who created R?"
)
#> "Ross Ihaka and Robert Gentleman"
```

``` r
# Structured output with multiple fields
extractor <- signature(
  "text -> title: string, entities: array(string), sentiment: enum('pos', 'neg', 'neu')"
) |> module(type = "predict")

extractor$predict(text = "Apple announced the iPhone 16 today. Investors are excited.")
#> $title
#> "Apple iPhone 16 Announcement"
#> $entities
#> c("Apple", "iPhone 16")
#> $sentiment
#> "pos"
```

``` r
# ReAct agent with tool use
library(ellmer)

search_tool <- tool(
  function(query) wikipedia_search(query),
  "Search Wikipedia for information"
)

agent <- signature("question -> answer") |>
  module(type = "react", tools = list(search_tool))

agent$predict(question = "What is the population of Tokyo?")
#> "Tokyo has a population of approximately 14 million people."
```

## Automatic Optimization

dsprrr can automatically optimize your prompts using your data.

- Few-Shot
- Grid Search
- Evaluation

``` r
# Add examples automatically
trainset <- dsp_trainset(
  text = c("Great product!", "Awful experience", "It works"),
  sentiment = c("positive", "negative", "neutral")
)

optimized <- compile(
  LabeledFewShot(k = 3),
  classifier,
  trainset
)

# Now includes 3 examples in every prompt
optimized$predict(text = "Amazing service!")
#> "positive"
```

**Result**: Few-shot examples improve accuracy on edge cases.

``` r
# Search over configurations
classifier$optimize_grid(
  devset = validation_data,
  metric = metric_exact_match(),
  parameters = list(
    temperature = c(0.1, 0.5, 1.0),
    prompt_style = c("concise", "detailed")
  )
)

# View results
module_trials_summary(classifier)
#> # A tibble: 6 × 4
#>   temperature prompt_style score    n
#>         <dbl> <chr>        <dbl> <int>
#> 1         0.1 concise      0.92    100
#> 2         0.1 detailed     0.88    100
#> ...
```

**Result**: Find the best configuration for your task.

``` r
# Rigorous evaluation with metrics
results <- evaluate(
  classifier,
  test_data,
  metric = metric_exact_match()
)

results$mean_score
#> 0.94

# Integrate with vitals for advanced evaluation
library(vitals)
solver <- as_vitals_solver(classifier)
```

**Result**: Measure and track performance systematically.

## Why dsprrr?

##### Declarative

Define **what** you want, not **how** to prompt. Signatures like
`“text -> sentiment”` describe your task clearly.

##### Composable

Build complex pipelines from simple modules. Each module is testable,
optimizable, and reusable.

##### Optimizable

Automatically improve prompts with your data. Few-shot learning, grid
search, and advanced teleprompters.

##### Integrated

Built on [ellmer](https://ellmer.tidyverse.org) for LLM access and
[vitals](https://vitals.tidyverse.org) for evaluation. Works with
tidyverse.

##### Observable

Every LLM call is traced. Inspect prompts, debug failures, track costs.

##### Production-Ready

Persistence with pins, orchestration with targets, deployment with
vetiver.

## Learn More

### Tutorials

- [Getting
  Started](https://jameshwade.github.io/dsprrr/articles/getting-started.md)
  — Your first dsprrr module
- [Compilation &
  Optimization](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)
  — Improve with data
- [Vitals
  Integration](https://jameshwade.github.io/dsprrr/articles/vitals-integration.md)
  — Advanced evaluation
- [Production
  Orchestration](https://jameshwade.github.io/dsprrr/articles/orchestration.md)
  — Deploy to production

### Reference

- [Function
  Reference](https://jameshwade.github.io/dsprrr/reference/index.md) —
  All functions documented

## Ecosystem

dsprrr integrates with much of Posit’s LLM ecosystem:

| Package                                             | Purpose                  |
|-----------------------------------------------------|--------------------------|
| [ellmer](https://ellmer.tidyverse.org)              | Chat with LLMs from R    |
| [vitals](https://vitals.tidyverse.org)              | LLM evaluation framework |
| [shinychat](https://posit-dev.github.io/shinychat/) | Chat UIs for Shiny       |

Inspired by [DSPy](https://dspy.ai) from Stanford NLP.
