# dsprrr <img src="man/figures/logo.png" align="right" width="120" alt="dsprrr hex sticker" />

## Programming—not prompting—LLMs in R

dsprrr brings the power of [DSPy](https://dspy.ai) to R. Instead of wrestling with prompt strings, **declare** what you want, **compose** modules into pipelines, and let **optimization** find the best prompts automatically.

```r
# Install
pak::pak("JamesHWade/dsprrr")

# That's it. Start using LLMs.
library(dsprrr)
dsp("question -> answer", question = "What is the capital of France?")
#> "Paris"
```

## Getting Started: Configure Your LLM

<ul class="nav nav-tabs" id="providerTabs" role="tablist">
  <li class="nav-item" role="presentation">
    <button class="nav-link active" id="openai-tab" data-bs-toggle="tab" data-bs-target="#openai" type="button" role="tab">OpenAI</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="anthropic-tab" data-bs-toggle="tab" data-bs-target="#anthropic" type="button" role="tab">Anthropic</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="gemini-tab" data-bs-toggle="tab" data-bs-target="#gemini" type="button" role="tab">Gemini</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="ollama-tab" data-bs-toggle="tab" data-bs-target="#ollama" type="button" role="tab">Ollama</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="auto-tab" data-bs-toggle="tab" data-bs-target="#auto" type="button" role="tab">Auto-detect</button>
  </li>
</ul>
<div class="tab-content" id="providerTabsContent">
  <div class="tab-pane fade show active" id="openai" role="tabpanel">

```r
library(dsprrr)
library(ellmer)

chat <- chat_openai(model = "gpt-4o-mini")
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

  </div>
  <div class="tab-pane fade" id="anthropic" role="tabpanel">

```r
library(dsprrr)
library(ellmer)

chat <- chat_claude(model = "claude-sonnet-4-20250514")
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

  </div>
  <div class="tab-pane fade" id="gemini" role="tabpanel">

```r
library(dsprrr)
library(ellmer)

chat <- chat_google_gemini(model = "gemini-2.0-flash")
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

  </div>
  <div class="tab-pane fade" id="ollama" role="tabpanel">

```r
library(dsprrr)
library(ellmer)

chat <- chat_ollama(model = "llama3.2")
chat |> dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

  </div>
  <div class="tab-pane fade" id="auto" role="tabpanel">

```r
# dsprrr auto-detects from environment variables
library(dsprrr)

# Uses OPENAI_API_KEY, ANTHROPIC_API_KEY, or GOOGLE_API_KEY
dsp("question -> answer", question = "What is 2+2?")
#> "4"
```

  </div>
</div>

## Building Modules

Modules are reusable LLM components with typed inputs and outputs.

<ul class="nav nav-tabs" id="moduleTabs" role="tablist">
  <li class="nav-item" role="presentation">
    <button class="nav-link active" id="classify-tab" data-bs-toggle="tab" data-bs-target="#classify" type="button" role="tab">Classification</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="qa-tab" data-bs-toggle="tab" data-bs-target="#qa" type="button" role="tab">Q&A</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="extract-tab" data-bs-toggle="tab" data-bs-target="#extract" type="button" role="tab">Extraction</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="agent-tab" data-bs-toggle="tab" data-bs-target="#agent" type="button" role="tab">Agents</button>
  </li>
</ul>
<div class="tab-content" id="moduleTabsContent">
  <div class="tab-pane fade show active" id="classify" role="tabpanel">

```r
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

  </div>
  <div class="tab-pane fade" id="qa" role="tabpanel">

```r
# Context-aware QA
qa <- signature("context, question -> answer") |>
  module(type = "predict")

qa$predict(
  context = "R was created by Ross Ihaka and Robert Gentleman in 1993.",
  question = "Who created R?"
)
#> "Ross Ihaka and Robert Gentleman"
```

  </div>
  <div class="tab-pane fade" id="extract" role="tabpanel">

```r
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

  </div>
  <div class="tab-pane fade" id="agent" role="tabpanel">

```r
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

  </div>
</div>

## Automatic Optimization

dsprrr can automatically optimize your prompts using your data.

<ul class="nav nav-tabs" id="optimizeTabs" role="tablist">
  <li class="nav-item" role="presentation">
    <button class="nav-link active" id="fewshot-tab" data-bs-toggle="tab" data-bs-target="#fewshot" type="button" role="tab">Few-Shot</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="grid-tab" data-bs-toggle="tab" data-bs-target="#grid" type="button" role="tab">Grid Search</button>
  </li>
  <li class="nav-item" role="presentation">
    <button class="nav-link" id="eval-tab" data-bs-toggle="tab" data-bs-target="#eval" type="button" role="tab">Evaluation</button>
  </li>
</ul>
<div class="tab-content" id="optimizeTabsContent">
  <div class="tab-pane fade show active" id="fewshot" role="tabpanel">

```r
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

  </div>
  <div class="tab-pane fade" id="grid" role="tabpanel">

```r
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

  </div>
  <div class="tab-pane fade" id="eval" role="tabpanel">

```r
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

  </div>
</div>

## Why dsprrr?

<div class="row row-cols-1 row-cols-md-3 g-4 my-4">
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Declarative</h5>
<p class="card-text">Define <strong>what</strong> you want, not <strong>how</strong> to prompt. Signatures like <code>"text -> sentiment"</code> describe your task clearly.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Composable</h5>
<p class="card-text">Build complex pipelines from simple modules. Each module is testable, optimizable, and reusable.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Optimizable</h5>
<p class="card-text">Automatically improve prompts with your data. Few-shot learning, grid search, and advanced teleprompters.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Integrated</h5>
<p class="card-text">Built on <a href="https://ellmer.tidyverse.org">ellmer</a> for LLM access and <a href="https://vitals.tidyverse.org">vitals</a> for evaluation. Works with tidyverse.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Observable</h5>
<p class="card-text">Every LLM call is traced. Inspect prompts, debug failures, track costs.</p>
</div>
</div>
</div>
<div class="col">
<div class="card h-100 border-start border-primary border-4">
<div class="card-body">
<h5 class="card-title">Production-Ready</h5>
<p class="card-text">Persistence with pins, orchestration with targets, deployment with vetiver.</p>
</div>
</div>
</div>
</div>

## Learn More

### Tutorials

- [Getting Started](articles/getting-started.html) — Your first dsprrr module
- [Compilation & Optimization](articles/compilation-optimization.html) — Improve with data
- [Vitals Integration](articles/vitals-integration.html) — Advanced evaluation
- [Production Orchestration](articles/orchestration.html) — Deploy to production

### Reference

- [Function Reference](reference/index.html) — All functions documented

## Ecosystem

dsprrr integrates with much of Posit's LLM ecosystem:

| Package | Purpose |
|---------|---------|
| [ellmer](https://ellmer.tidyverse.org) | Chat with LLMs from R |
| [vitals](https://vitals.tidyverse.org) | LLM evaluation framework |
| [shinychat](https://posit-dev.github.io/shinychat/) | Chat UIs for Shiny |

Inspired by [DSPy](https://dspy.ai) from Stanford NLP.
