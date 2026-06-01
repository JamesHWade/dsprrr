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

## Compose programs with reusable primitives

Every dsprrr program is built from the same three pieces. Learn these
and the rest of the package falls into place.

### Signatures

**Declare your task.** Define typed inputs and outputs instead of
wrestling with prompt strings. Portable, maintainable, and easy to
iterate on.

[Learn about signatures
→](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)

``` r

# Route a support ticket
sig <- signature(
  "ticket -> urgency: enum('low', 'high'), team: string"
)
```

### Modules

**Same interface, different strategy.** Modules control how a signature
executes—reason step by step, use tools, or run ensembles—without
rewriting the task.

[Explore modules
→](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md)

``` r

sig <- signature(
  "ticket -> urgency: enum('low', 'high'), team: string"
)

# Direct completion
classify <- module(sig, type = "predict")

# Add step-by-step reasoning
classify <- module(sig, type = "chain_of_thought")

# Add a tool-use loop
lookup_tool <- ellmer::tool(
  function(query) paste("Found:", query),
  description = "Look up support policy details",
  arguments = list(query = ellmer::type_string())
)
classify <- module(sig, type = "react", tools = list(lookup_tool))
```

### Optimizers

**Compile your program against a metric.** Give dsprrr examples and a
scoring function; it tunes prompts and demos automatically until quality
converges.

[Try optimizers
→](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)

``` r

route_sig <- signature("ticket -> urgency: enum('low', 'high')")
router <- module(route_sig, type = "predict")
trainset <- dsp_trainset(
  ticket  = c("Package lost", "Need a receipt"),
  urgency = c("high", "low")
)

tp <- GEPA(metric = metric_exact_match(field = "urgency"))
optimized <- compile(tp, router, trainset)

board <- pins::board_temp()
pin_module_config(board, "ticket-router-v2", optimized)
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

## Define a task. Grow it into a system.

Start with a single signature and grow it into a multi-step program—the
same building blocks scale from a one-line extractor to a full pipeline.

- Extract
- Agent
- Pipeline
- Multimodal
- Optimize

Signatures define a task and enforce typed outputs.

``` r

# Extract several typed fields in one call
extract <- signature(
  "message -> name: string, email: string,
   intent: enum('meeting', 'intro', 'follow-up')"
) |> module(type = "predict")

result <- run(
  extract,
  message = "I'm Sarah (sarah@acme.co). Meet Thursday?",
  .llm = chat_openai()
)

# In simple mode (the default), run() returns the parsed output directly
result$name    #> "Sarah"
result$email   #> "sarah@acme.co"
result$intent  #> "meeting"
```

[Extract structured data
→](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.md)

Define tools as functions and hand them to a ReAct module.

``` r

kb_search <- function(query) {
  paste(
    "Evaluators compare module outputs with labeled examples.",
    "Optimizers use those scores to select better prompts and demos."
  )
}

search <- ellmer::tool(
  function(query) kb_search(query),
  description = "Search a knowledge base",
  arguments = list(query = ellmer::type_string())
)

agent <- signature("question -> answer") |>
  module(type = "react", tools = list(search))

answer <- run(
  agent,
  question = "How do dsprrr optimizers improve a module?",
  .llm = chat_openai()
)
answer$answer
#> "They score outputs against examples, then keep better prompts and demos."
```

[Build a tool-using agent
→](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md)

Compose modules into a pipeline with `%>>%`—outputs flow to inputs.

``` r

# Pull a claim, then verify it against the source
find <- signature("article -> claim: string, source: string") |>
  module(type = "chain_of_thought")

verify <- signature("claim, source -> verdict") |>
  module(type = "chain_of_thought")

factcheck <- find %>>% verify

news_article <- "Acme reported that revenue grew 12% in Q4."
verdict <- run(factcheck, article = news_article, .llm = chat_openai())
verdict$verdict
#> "supported"
```

[Chain modules into pipelines
→](https://jameshwade.github.io/dsprrr/articles/chaining-modules.md)

Name an image input in the signature and pass an ellmer content object.

``` r

analyze <- signature("image, question -> answer") |>
  module(type = "predict")

run(
  analyze,
  image    = ellmer::ContentImageRemote(
    "https://www.r-project.org/logo/Rlogo.png"
  ),
  question = "What logo is shown?",
  .llm     = chat_openai()
)
#> "The image shows the R project logo."
```

[Work with multimodal inputs
→](https://jameshwade.github.io/dsprrr/articles/advanced-ellmer.md)

Optimizers improve a program against a metric—no prompt rewriting.

``` r

extract <- signature(
  "message -> intent: enum('meeting', 'intro')"
) |>
  module(type = "predict")

trainset <- dsp_trainset(
  message = c("I'm Sarah (sarah@acme.co). Meet Thursday?",
              "Hi, this is Dev—just saying hello!"),
  intent  = c("meeting", "intro")
)

optimized <- compile(
  GEPA(metric = metric_exact_match(field = "intent")),
  extract,
  trainset
)

board <- pins::board_temp()
pin_module_config(board, "extract-v2", optimized)
```

[Optimize with your data
→](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)

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
module_trials(classifier)
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

| Package | Purpose |
|----|----|
| [ellmer](https://ellmer.tidyverse.org) | Chat with LLMs from R |
| [vitals](https://vitals.tidyverse.org) | LLM evaluation framework |
| [shinychat](https://posit-dev.github.io/shinychat/) | Chat UIs for Shiny |

Inspired by [DSPy](https://dspy.ai) from Stanford NLP.
