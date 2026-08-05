
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dsprrr <img src="man/figures/logo.png" align="right" height="139" alt="dsprrr hex sticker" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/JamesHWade/dsprrr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/dsprrr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/JamesHWade/dsprrr/graph/badge.svg)](https://app.codecov.io/gh/JamesHWade/dsprrr)
<!-- badges: end -->

dsprrr adds signatures, optimization, and tracing on top of
[ellmer](https://ellmer.tidyverse.org). It implements ideas from
[DSPy](https://dspy.ai) for R.

**The problem:** Hand-tuned prompts are fragile. They break when you
switch models, change requirements, or scale up. dsprrr treats prompts
as programs that can be systematically improved using your data.

**Use cases:**

- RAG pipelines where you want to optimize retrieval + generation
  together
- Classification or extraction tasks with labeled examples to learn from
- Multi-step agents where you need to trace what went wrong
- Any LLM workflow you want to improve without manually rewriting
  prompts

**When to just use ellmer:** If you have a prompt that works and don’t
need to optimize it with data. ellmer already tracks conversation
history and token costs.

## Installation

``` r
# install.packages("pak")
pak::pak("JamesHWade/dsprrr")
```

## What dsprrr adds

### Signatures

A compact notation for defining LLM inputs and outputs:

``` r
library(dsprrr)
#> 
#> Attaching package: 'dsprrr'
#> The following object is masked from 'package:stats':
#> 
#>     step
#> The following object is masked from 'package:methods':
#> 
#>     signature

# Arrow notation: inputs -> output
signature("question -> answer")
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • question: "string" - Input: question
#> 
#> ── Output
#> Type: "object(answer: string)"
#> 
#> ── Instructions
#> Given the fields `question`, produce the fields `answer`.

# Multiple inputs
signature("context, question -> answer")
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • context: "string" - Input: context
#> • question: "string" - Input: question
#> 
#> ── Output
#> Type: "object(answer: string)"
#> 
#> ── Instructions
#> Given the fields `context`, `question`, produce the fields `answer`.

# Typed outputs (uses ellmer types under the hood)
signature("review -> rating: enum('1', '2', '3', '4', '5')")
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • review: "string" - Input: review
#> 
#> ── Output
#> Type: "object(rating: enum(1, 2, 3, 4, 5))"
#> 
#> ── Instructions
#> Given the fields `review`, produce the fields `rating`.

# With instructions
signature("text -> summary", instructions = "Maximum 50 words.")
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • text: "string" - Input: text
#> 
#> ── Output
#> Type: "object(summary: string)"
#> 
#> ── Instructions
#> Maximum 50 words.
```

### Modules

Reusable, stateful wrappers around LLM calls:

``` r
library(ellmer)

# Create a module from a signature
mod <- module(signature("text -> sentiment"), type = "predict")

# Run it
run(mod, text = "This is great!", .llm = chat_openai())

# Or convert an existing Chat
classifier <- chat_openai() |>
  as_module("text -> sentiment: enum('positive', 'negative', 'neutral')")

classifier$predict(text = "Terrible experience")
```

### Pipelines

Chain modules together with the `%>>%` operator. Outputs flow
automatically to inputs:

``` r
# Chain three modules together
qa_pipeline <- mod_extract %>>% mod_answer %>>% mod_format

# Run the pipeline
result <- run(qa_pipeline, document = "...", .llm = chat_openai())

# With explicit field mapping when names don't match
rag_pipeline <- pipeline(
  mod_retrieve,
  step(mod_answer, map = c(documents = "context")),
  mod_summarize
)
```

### Optimization

Automatically improve programs using training data. dsprrr implements
several optimizers inspired by DSPy:

- **LabeledFewShot**: Add examples from your training set as
  demonstrations
- **MIPROv2**: Joint optimization of instructions and examples using
  Bayesian search
- **GEPA**: Reflection-based instruction and constrained Flex-structure
  optimization with whole-program Pareto selection
- **AutoResearch and MetaHarness**: Agentic, sandboxed search over
  multi-module instructions and templates

`flex()` is an experimental module, not an optimizer. It exposes a
validated JSON predictor plan that GEPA can search as a structural
parameter.

``` r
# Compile with few-shot examples
optimized <- compile(
  LabeledFewShot(k = 3),
  mod,
  trainset = my_labeled_data
)

# Grid search over parameters
mod$optimize_grid(
  devset = dev_data,
  metric = metric_exact_match(),
  parameters = list(temperature = c(0.1, 0.5, 1.0))
)
```

### Tracing

ellmer tracks individual chat history and costs. dsprrr adds
module-level traces across pipelines—useful for debugging multi-step
workflows:

``` r
mod$trace_summary()
export_traces(mod)
```

## Quick example

``` r
library(dsprrr)
library(ellmer)

# Define what you want
sig <- signature(
  "context, question -> answer",
  instructions = "Answer based only on the given context."
)

# Create a module
mod <- module(sig, type = "predict")

# Run it
result <- run(
  mod,
  context = "R is a programming language for statistical computing.",
  question = "What is R used for?",
  .llm = chat_openai()
)
```

## Module types

| Type | Use case |
|----|----|
| `predict` | Basic text generation |
| `react` | Tool use (wraps ellmer tools) |
| `chain_of_thought` | Step-by-step reasoning |
| `multichain` | Ensemble reasoning with multiple chains |
| `program_of_thought` | Generate and execute R code |
| `codeact` | Combine tools with R code execution |
| `rlm` | Explore large context through an R REPL |
| `flex` | Execute a validated, structurally optimizable predictor plan (experimental) |

``` r
# ReAct agent with tools
agent <- module(
  signature("question -> answer"),
  type = "react",
  tools = list(my_search_tool)
)

# Chain of thought
mod <- module(signature("question -> answer"), type = "chain_of_thought")
```

### Experimental structural programs

`flex()` represents a predictor graph as a canonical, versioned JSON
intermediate representation. Version 1 permits only allowlisted
`predict` and `chain_of_thought` steps with typed references between
them. The source is parsed as data—it is never evaluated as R or
Python—and `module_src` is read-only; use `bind()` to validate and
replace it transactionally.

``` r
program <- flex("question -> answer")
program$module_src

optimized <- compile(
  GEPA(metric = metric_exact_match(field = "answer"), seed = 42L),
  program,
  trainset = question_answer_data,
  .llm = llm
)
```

This constrained design is intentionally narrower than DSPy’s
experimental Flex module; dsprrr does not claim arbitrary-source or full
Flex parity. GEPA-lite proposes complete source values, validates them
transactionally, and ranks whole-program candidates. It does not
reproduce DSPy’s independent per-component frontiers or inference-time
search. See [Structural Optimization with
Flex](https://jameshwade.github.io/dsprrr/articles/flex-optimization.html)
for the JSON schema and safety boundary.

### Code-runner lifecycles

`program_of_thought()`, `code_act()`, and `rlm_module()` require exactly
one of two execution bindings:

- `runner` is a caller-owned object that dsprrr reuses and never closes.
  Whether execution state persists, and whether `reset()` exists,
  depends on the backend. Serialize stateful backends and reset them
  between isolated jobs when the backend supports it.
- `interpreter_factory` is a zero-argument function. dsprrr calls it
  once per invocation, uses the fresh runner only for that invocation,
  and closes it exactly once, including when execution fails.

Use synchronous `run()` for these specialized modules. `run_async()`,
`stream_async()`, and `$stream()` reject their direct provider path
before creating a factory runner or making a provider call.
`run_stream()` preserves its one-shot `forward()` fallback, but
preflights and rejects an actual token stream request that would bypass
a specialized step.

``` r
pot <- program_of_thought(
  "question -> answer",
  interpreter_factory = function() r_code_runner(timeout = 30)
)
```

## ellmer compatibility

dsprrr uses ellmer for all LLM calls. The integration is
straightforward:

| ellmer                         | dsprrr equivalent               |
|--------------------------------|---------------------------------|
| `chat_openai()`                | Pass to `run(..., .llm = )`     |
| `type_string()`, `type_enum()` | Used inside signatures          |
| `tool()`                       | Pass to `module(..., tools = )` |
| `chat$chat_structured()`       | `dsp(chat, signature, ...)`     |

## Learning more

**Start here:** - [Getting
Started](https://jameshwade.github.io/dsprrr/articles/getting-started.html)
— Choose your learning path

**Tutorial sequence** (learn step by step): 1. [Your First LLM
Call](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.html)
2. [Building a
Classifier](https://jameshwade.github.io/dsprrr/articles/tutorial-build-classifier.html)
3. [Structured
Outputs](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.html)
4. [Improving with
Examples](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.html)
5.
[Optimization](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.html)
6.
[Production](https://jameshwade.github.io/dsprrr/articles/tutorial-deploy-to-production.html)

**How-to guides:** - [Compile &
Optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.html) -
[Build RAG
Pipelines](https://jameshwade.github.io/dsprrr/articles/rag-workflows.html)

**Concepts:** - [The DSPy
Philosophy](https://jameshwade.github.io/dsprrr/articles/concepts-dspy-philosophy.html) -
[How Optimization
Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.html)

**Reference:** - [Quick
Reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.html) -
[API
Documentation](https://jameshwade.github.io/dsprrr/reference/index.html)

## Status

Experimental. The API may change. See the [open
issues](https://github.com/JamesHWade/dsprrr/issues) for the roadmap.

## Acknowledgments

Built on [ellmer](https://ellmer.tidyverse.org) and
[S7](https://rconsortium.github.io/S7/). Inspired by
[DSPy](https://github.com/stanfordnlp/dspy).
