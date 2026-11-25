# dsprrr

Program LLMs in R instead of hand-tuning prompt strings.

Declare a task’s inputs and typed outputs once, swap reasoning and
tool-use strategies without rewriting prompts, measure quality on
labeled data, and let an optimizer fit instructions and examples to your
metric. Built on [ellmer](https://ellmer.tidyverse.org) and inspired by
[DSPy](https://dspy.ai).

[Get
started](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md)
[How dsprrr
works](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)
[Coming from
DSPy? →](https://jameshwade.github.io/dsprrr/articles/dspy-comparison.md)

## Why program instead of prompt?

A hand-written prompt mixes three fragile things into one string: *what*
you want, *how* to coax a particular model, and *which examples* to
show. `dsprrr` keeps them separate so you can test, compare, and
optimize LLM features like any other R code.

Hand-tuned prompt

``` r

# Task and wording tangled in one string
chat$chat(paste(
  "Route this ticket. Return urgency",
  "(low/high) and team. Ticket:", ticket
))
```

- Free-text output you have to parse by hand
- Hard to test across rows or compare versions
- Breaks quietly when you change models

Declarative dsprrr program

``` r

sig <- signature(
  "ticket -> urgency: enum('low', 'high')"
)
router <- chain_of_thought(sig) |>
  compile(
    BootstrapFewShot(metric = metric),
    trainset = train
  )
```

- Typed R list returned via structured schemas
- Swap strategies or chain steps in one line
- Measure with
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
  re-compile for any model

If you have a one-off prompt that already works and no data to measure
it against, plain [ellmer](https://ellmer.tidyverse.org) is enough.
`dsprrr` earns its keep once an LLM call becomes a repeatable workflow
you need to measure, compose, or port across models.

## The R modeling mental model

If you have ever fit a model in R, you already know the loop:

1\. Declare

[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)

Like `y ~ x`

State inputs, typed outputs, and task intent without writing prompt
plumbing.

2\. Build

[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)

Like a model spec

Choose direct prediction, chain-of-thought, ReAct tools, or `%>>%`
pipelines.

3\. Measure

[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)

Like `yardstick`

Score predictions across a data frame and inspect prompts, traces, and
cost.

4\. Optimize

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

Like `fit()`

Search instructions and few-shot demos on training data to raise your
metric.

## How the pieces fit

One support-ticket workflow from declaration to a saved, compiled
program:

01

### Signatures

**Declare the contract.** Put inputs on the left of `->`, typed outputs
on the right, and optional instructions. `dsprrr` builds the prompt and
JSON schema from this, so results arrive as typed R values ready for a
data frame.

[How dsprrr
works →](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)

``` r

library(dsprrr)

sig <- signature(
  "ticket -> urgency: enum('low', 'high'), team: string",
  instructions = "Route a support ticket."
)
```

02

### Modules

**Swap how it thinks.** The same signature can run as one structured
call, with step-by-step reasoning first, or in a tool-calling loop. Run
any module on one input with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) or over
a data frame with
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).

[Choose a module
type →](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md)

``` r

chat <- ellmer::chat_openai(model = "gpt-6-luna")

router <- module(sig)                # one call
router_cot <- chain_of_thought(sig)  # reason first

lookup <- ellmer::tool(
  function(q) "Duplicate charges go to billing.",
  description = "Look up routing policy",
  arguments = list(q = ellmer::type_string())
)
router_agent <- react(sig, tools = list(lookup))

run(router, ticket = "Charged twice!", .llm = chat)
```

03

### Pipelines

**Compose multi-step programs.** Chain modules with `%>>%` or wire
inputs explicitly with
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
and [`step()`](https://jameshwade.github.io/dsprrr/reference/step.md). A
pipeline is itself a module, so
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
and
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
work across all steps together.

[Chain modules into
pipelines →](https://jameshwade.github.io/dsprrr/articles/chaining-modules.md)

``` r

draft <- module(signature(
  "urgency, team -> reply",
  instructions = "Write a brief acknowledgment."
))

# Outputs connect to matching inputs by name
triage <- module(sig) %>>% draft

run(triage, ticket = "Charged twice!", .llm = chat)
```

04

### Metrics & evaluation

**Measure instead of eyeballing.** Split your labeled rows, run the
module over the validation set, and score every prediction. Inspect
misses, prompts
([`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md)),
traces
([`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md)),
and spend
([`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md)).

[Metrics and
evaluation →](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md)

``` r

tickets <- tibble::tribble(
  ~ticket,                       ~urgency, ~team,
  "Charged twice for my order",  "high",   "billing",
  "Change my profile photo?",    "low",    "account",
  "App deletes files on sync",   "high",   "technical",
  "Need last year's invoice",    "low",    "billing",
  "Unknown login on my account", "high",   "account",
  "Export fails in Safari",      "low",    "technical"
)

splits <- split_dataset(tickets, prop = 0.67, seed = 1)
metric <- metric_exact_match(field = "urgency")

baseline <- evaluate(
  router, splits$val, metric = metric, .llm = chat
)
baseline$mean_score
```

05

### Optimizers & persistence

**Fit prompts to your data.**
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
returns a new module whose demos or instructions were chosen to raise
your metric on training rows (`BootstrapFewShot`, `MIPROv2`, `GEPA`, and
more). Verify on held-out rows, then save with
[`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).
Switch models? Swap `chat` and re-compile.

[Compile and
optimize →](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)

``` r

optimizer <- BootstrapFewShot(
  metric = metric,
  max_bootstrapped_demos = 2L,
  max_labeled_demos = 0L
)

optimized <- router |>
  compile(optimizer, splits$train, .llm = chat)

evaluate(
  optimized, splits$val, metric, .llm = chat
)
save_program(optimized, "ticket_router.rds")
```

## Works with your R stack

[ellmer
providers](https://jameshwade.github.io/dsprrr/articles/models-and-providers.md)

Runs on any chat ellmer supports: OpenAI, Anthropic, Google, Ollama,
Bedrock, and more.

[vitals
evaluation](https://jameshwade.github.io/dsprrr/articles/vitals-recipes.md)

Expose modules as vitals solvers and reuse vitals scorers directly as
dsprrr metrics.

[ragnar
retrieval](https://jameshwade.github.io/dsprrr/articles/rag-workflows.md)

Connect document stores to RAG modules and ReAct agents with
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md).

[tidymodels](https://jameshwade.github.io/dsprrr/articles/tidymodels-integration.md)

Tune LLM parameters alongside workflows with the
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md)
parsnip spec.

[pins &
targets](https://jameshwade.github.io/dsprrr/articles/orchestration.md)

Version compiled configs with
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
and orchestrate batch runs.

[Caching, async &
traces](https://jameshwade.github.io/dsprrr/reference/index.html#run-programs)

Built-in memory and disk cache,
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
and token/cost logs.

## Where to go next

New to dsprrr

[Follow the 6-step
tutorials →](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md)

Go from your first typed call to structured extraction, demos,
optimization, and saving a compiled module.

Already using ellmer

[Read how dsprrr
works →](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)

Understand signatures, R6 module state, traces, and when a plain ellmer
prompt is still the right tool.

Coming from Python DSPy

[See the DSPy
comparison →](https://jameshwade.github.io/dsprrr/articles/dspy-comparison.md)

Translate signatures, modules, teleprompters, and pipelines between
Python DSPy and idiomatic R.

Ready to optimize

[Choose an
optimizer →](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md)

Compare all eleven teleprompters — from `LabeledFewShot` and
`BootstrapFewShot` to `MIPROv2` and `GEPA`.

## Installation

Install the released version from CRAN:

``` r

install.packages("dsprrr")
```

Or install the development version from GitHub:

``` r

# install.packages("pak")
pak::pak("JamesHWade/dsprrr")
```

Set a provider API key in your `.Renviron` (for example `OPENAI_API_KEY`
or `ANTHROPIC_API_KEY`), then run
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)
to verify your setup. Installing from source requires a C compiler
(Rtools on Windows, Xcode Command Line Tools on macOS, or your Linux
distribution’s build tools); the compiled helper preserves exact
filesystem identities for safe persistence.

**Experimental features:**
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
lets a model explore a large R object by writing code ([How RLM
works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.md)),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) lets
GEPA rewrite a whole program
([Flex](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md)),
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md)
and
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md)
calibrate decision thresholds ([Calibrated
decisions](https://jameshwade.github.io/dsprrr/articles/calibrated-decisions.md)),
and [`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md)
and the agentic harnesses compose optimizers
([Omni](https://jameshwade.github.io/dsprrr/articles/omni-meta-optimization.md),
[agentic
harnesses](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md)).

`dsprrr` is experimental; the API may change. It is inspired by
[DSPy](https://dspy.ai) from Stanford NLP, builds on
[ellmer](https://ellmer.tidyverse.org) for model access, and bridges to
[vitals](https://vitals.tidyverse.org) for evaluation.
