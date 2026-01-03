
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dsprrr <img src="man/figures/logo.png" align="right" width="160" alt="dsprrr hex sticker" />

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/dsprrr)](https://CRAN.R-project.org/package=dsprrr)
[![R-CMD-check](https://github.com/JamesHWade/dsprrr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/dsprrr/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/JamesHWade/dsprrr/graph/badge.svg)](https://app.codecov.io/gh/JamesHWade/dsprrr)
<!-- badges: end -->

dsprrr adds signatures, optimization, and tracing on top of [ellmer](https://ellmer.tidyverse.org). It's an R implementation of ideas from [DSPy](https://github.com/stanfordnlp/dspy).

**When to use dsprrr vs ellmer directly:**

- Use **ellmer** when you have a prompt that works and just need to call an LLM
- Use **dsprrr** when you want to systematically improve prompts using labeled data, or when you need to trace and debug LLM calls across a pipeline

## Installation

```r
# install.packages("pak")
pak::pak("JamesHWade/dsprrr")
```

## What dsprrr adds

### Signatures

A compact notation for defining LLM inputs and outputs:

```r
library(dsprrr)

# Arrow notation: inputs -> output
signature("question -> answer")

# Multiple inputs
signature("context, question -> answer")

# Typed outputs (uses ellmer types under the hood)
signature("review -> rating: enum('1', '2', '3', '4', '5')")

# With instructions
signature("text -> summary", instructions = "Maximum 50 words.")
```

### Modules

Reusable, stateful wrappers around LLM calls:

```r
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

### Optimization

Automatically improve prompts using training data:

```r
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

Every LLM call is recorded:

```r
mod$trace_summary()
export_traces(mod, format = "tibble")
```

## Quick example

```r
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
|------|----------|
| `predict` | Basic text generation |
| `react` | Tool use (wraps ellmer tools) |
| `chain_of_thought` | Step-by-step reasoning |
| `program_of_thought` | Generate and execute R code |

```r
# ReAct agent with tools
agent <- module(
  signature("question -> answer"),
  type = "react",
  tools = list(my_search_tool)
)

# Chain of thought
cot <- signature("question -> answer") |> with_reasoning()
mod <- module(cot, type = "predict")
```

## ellmer compatibility

dsprrr uses ellmer for all LLM calls. The integration is straightforward:

| ellmer | dsprrr equivalent |
|--------|-------------------|
| `chat_openai()` | Pass to `run(..., .llm = )` |
| `type_string()`, `type_enum()` | Used inside signatures |
| `tool()` | Pass to `module(..., tools = )` |
| `chat$chat_structured()` | `dsp(chat, signature, ...)` |

## Learning more

```r
vignette("getting-started", package = "dsprrr")
vignette("compilation-optimization", package = "dsprrr")
```

## Status

Experimental. The API may change. See [PLAN.md](PLAN.md) for the roadmap.

## Acknowledgments

Built on [ellmer](https://ellmer.tidyverse.org) and [S7](https://rconsortium.github.io/S7/). Inspired by [DSPy](https://github.com/stanfordnlp/dspy).
