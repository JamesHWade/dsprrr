
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
mod <- module(signature("text -> sentiment"))

# Run it
run(mod, text = "This is great!", .llm = chat_openai())

# Or store an existing Chat on the module
classifier <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
  chat = chat_openai()
)

run(classifier, text = "Terrible experience")
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
- **GEPA**: Reflection-based complete-program optimization with
  validation-example and multi-objective Pareto selection
- **AutoResearch and MetaHarness**: Agentic, sandboxed search over
  multi-module instructions and templates

`flex()` gives GEPA a different search target: not only what a predictor
says, but how the module executes. It can vary predictor choice, control
flow, deterministic R, and selected tools.

``` r
# Compile with few-shot examples
optimized <- mod |>
  compile(LabeledFewShot(k = 3), my_labeled_data)

# Grid search over parameters
optimized <- optimize_grid(
  mod,
  data = dev_data,
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
mod <- module(sig)

# Run it
result <- run(
  mod,
  context = "R is a programming language for statistical computing.",
  question = "What is R used for?",
  .llm = chat_openai()
)
```

## Program constructors

Use `module()` for ordinary structured prediction. Choose a named
constructor when the program has different execution semantics:

| Constructor | Use case |
|----|----|
| `module()` | Standard structured prediction |
| `react()` | Tool use with an explicit ReAct loop |
| `chain_of_thought()` | Step-by-step reasoning |
| `multi_chain_comparison()` | Ensemble reasoning with multiple chains |
| `program_of_thought()` | Generate and execute R code |
| `code_act()` | Combine tools with R code execution |
| `rlm_module()` | Adaptively investigate large or awkward R objects |
| `flex()` | Let GEPA optimize predictors, R logic, and selected tools |

``` r
# ReAct agent with tools
agent <- react(
  signature("question -> answer"),
  tools = list(my_search_tool)
)

# Chain of thought
mod <- chain_of_thought(signature("question -> answer"))
```

## Find the cohort that regressed

A release-level average tells you that checkout conversion fell. It does
not tell you which cohort changed or which release note matters. When
the useful grouping is not known in advance, an RLM can inspect the
source object with R, show the model only bounded observations, and
revise the investigation between steps.

The deterministic tutorial uses 40,000 sessions and 200 change records.
A successful run must recover this independently checkable result:

``` text
release:      2.4.0
cohort:       platform=mobile / plan=pro
before_rate:  0.92
after_rate:   0.61
drop_pp:      31
change_id:    CHG-1842
```

For that rich data-frame input, opt into a persistent trusted callr
runner:

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
  max_iterations = 8,
  max_llm_calls = 0L
)
```

This persistent `r_code_runner()` preserves rich R objects but is
trusted-input-only, not a security sandbox. The one-call `rlm()` helper
instead creates a fresh managed `mcp-repl` OS sandbox by default. It
disables network access but permits writes inside the allowed workspace.
Install the suggested R package `mcptools` and the external `mcp-repl`
executable first (for example, `uv tool install posit-mcp-repl`). This
path currently suits compact, JSON-compatible context: the final
JSON-RPC request has a 7 KB wire bound, with gzip/base64 attempted when
the raw request is too large, and RLM control frames have a 3,000-byte
encoded bound.

[Work through the release-regression
investigation](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.html),
then read [the RLM execution
contract](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.html).

### Choose the right kind of code generation

| If the task requires… | Use… |
|----|----|
| A calculation whose steps are already known | `program_of_thought()` |
| A new exploration path for this input at inference time | `rlm_module()` |
| A reusable implementation discovered from labeled examples | `flex()` with GEPA |

RLM is not an optimizer. It adapts its investigation for each
invocation. Its action and fallback instructions can still be compiled
with a supported optimizer, but the exploration remains inference-time
work. Flex searches during compilation for a program to reuse on later
invocations.

## Optimize the program, not only the prompt

Most optimizers improve instructions inside a workflow you designed.
`flex()` lets GEPA change the workflow itself: which predictors run,
where deterministic R is enough, and when to call a selected tool.

The worked example begins with a support router that asks a model about
every ticket. In a deterministic GEPA replay, Flex selects a hybrid:
known incident codes use a catalog lookup; ambiguous prose still uses a
predictor. It preserves holdout accuracy while cutting predictor calls
in half.

> Flex did not find a better prompt. It found that half the tickets did
> not need one.

Executable candidates require a fresh OS-sandboxed interpreter. See
[Flex: Optimize the Whole
Program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.html)
for the complete demo, and [Advanced
Modules](https://jameshwade.github.io/dsprrr/articles/advanced-modules.html)
for code-runner ownership and async support.

## ellmer compatibility

dsprrr uses ellmer for all LLM calls. The integration is
straightforward:

| ellmer                         | dsprrr equivalent                          |
|--------------------------------|--------------------------------------------|
| `chat_openai()`                | Pass to `run(..., .llm = )`                |
| `type_string()`, `type_enum()` | Used inside signatures                     |
| `tool()`                       | Pass to `react(..., tools = )`             |
| `chat$chat_structured()`       | `run(module(signature), ..., .llm = chat)` |

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
Pipelines](https://jameshwade.github.io/dsprrr/articles/rag-workflows.html) -
[Investigate a Release Regression with an
RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.html)

**Concepts:** - [The DSPy
Philosophy](https://jameshwade.github.io/dsprrr/articles/concepts-dspy-philosophy.html) -
[How Optimization
Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.html) -
[How the RLM
Works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.html)

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
