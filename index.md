# dsprrr

## Programming—not prompting—LLMs in R

dsprrr brings the power of [DSPy](https://dspy.ai) to R. Instead of
wrestling with prompt strings, **declare** what you want, **compose**
modules into pipelines, and use **optimization** to improve prompts,
examples, or the program that executes them.

``` r

# Install
pak::pak("JamesHWade/dsprrr")

# Configure credentials for an ellmer-supported provider, then:
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
scoring function; it selects the best candidate observed within the
configured budget.

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

## Find the release cohort that broke

A release-level average can reveal a regression without explaining it.
If the useful grouping and calculation are not known in advance, an RLM
keeps the source object in an R environment, lets the model inspect it
with R code, and returns only bounded observations between steps.

The deterministic tutorial asks an RLM to explore 40,000 checkout
sessions and 200 change records. The answer is fixed and independently
validated:

``` text
release:      2.4.0
cohort:       platform=mobile / plan=pro
before_rate:  0.92
after_rate:   0.61
drop_pp:      31
change_id:    CHG-1842
```

``` r

investigator <- rlm_module(
  paste(
    "sessions, changes, question ->",
    "release: string, cohort: string, before_rate: number,",
    "after_rate: number, drop_pp: number, change_id: string, evidence: string"
  ),
  interpreter_factory = function() {
    r_code_runner(timeout = 30, persistent = TRUE)
  },
  max_iters = 8,
  max_llm_calls = 0L
)
```

This persistent callr runner stages rich R objects once, but it is
explicitly trusted-input-only. For compact JSON-compatible context, the
one-call [`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md)
helper creates a fresh managed `mcp-repl` OS sandbox by default. It
disables network access but allows writes inside the workspace. Install
`mcptools` and the external `mcp-repl` executable first. The final
JSON-RPC request has a 7 KB wire bound, with gzip/base64 attempted when
the raw request is too large, and RLM control frames have a 3,000-byte
encoded bound.

[Investigate the deterministic regression
→](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.md)
 ·  [Understand the RLM execution contract
→](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.md)

### ProgramOfThought, RLM, or Flex?

| If the task requires… | Use… |
|----|----|
| A calculation whose steps are already known | [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md) |
| A new exploration path for this input at inference time | [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) |
| A reusable implementation discovered from labeled examples | [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) with GEPA |

RLM is experimental inference-time exploration; one invocation does not
learn an exploration program. Supported optimizers can still tune its
action and fallback instructions. Flex searches during compilation for
an implementation to reuse.

## Optimize the program, not only the prompt

Most optimizers improve instructions inside a workflow you designed.
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) lets
GEPA change the workflow itself: which predictors run, where
deterministic R is enough, and when to call a tool you supplied.

The worked example starts with a support router that asks a model about
every ticket. In a deterministic GEPA replay, the selected program
checks known incident codes directly and reserves the model for
ambiguous prose. It keeps all six held-out routes correct while cutting
predictor calls from six to three.

> Flex did not find a better prompt. It found that half the tickets did
> not need one.

[See the complete router demo and decide when Flex fits
→](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md)

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
- Agentic
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

# Keep agent-proposed analysis inside an OS sandbox
runner <- mcp_repl_runner()

harness <- MetaHarness(
  metric = metric_exact_match(field = "sentiment"),
  max_iterations = 6L,
  max_candidates_per_iteration = 3L
)

optimized <- compile(
  harness,
  classifier,
  trainset,
  valset = validation_data,
  .llm = task_chat,
  .agent_llm = proposer_chat,
  runner = runner
)
```

**Result**: Search coordinated instructions and templates across an
entire module graph while dsprrr retains control of evaluation, budgets,
lineage, and accepted state.

[Run agentic optimization
→](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md)

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

Inspect bounded module traces, debug failures, and track usage when
providers report it.

##### Deployment building blocks

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
- [Release Regression with an
  RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.md)
  — Investigate a large R object adaptively
- [Agentic Optimization
  Harnesses](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md)
  — Run sandboxed AutoResearch and Meta-Harness loops
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
- [How the RLM
  Works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.md)
  — Execution, replay, typed submission, and runner boundaries

## Ecosystem

dsprrr integrates with much of Posit’s LLM ecosystem:

| Package | Purpose |
|----|----|
| [ellmer](https://ellmer.tidyverse.org) | Chat with LLMs from R |
| [vitals](https://vitals.tidyverse.org) | LLM evaluation framework |
| [shinychat](https://posit-dev.github.io/shinychat/) | Chat UIs for Shiny |
| [mcptools](https://posit-dev.github.io/mcptools/) | Connect R programs to MCP tools |
| [mcp-repl](https://github.com/posit-dev/mcp-repl) | Persistent OS-sandboxed R execution |

Inspired by [DSPy](https://dspy.ai) from Stanford NLP.
