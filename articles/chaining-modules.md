# Chaining Modules and Pipelines

## Overview

Chaining lets you build multi-step LLM workflows by passing outputs from
one module to the next. dsprrr provides two approaches:

- `%>>%` for quick, readable pipelines with automatic field matching
- [`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md) +
  [`step()`](https://jameshwade.github.io/dsprrr/reference/step.md) for
  explicit control over mappings and selections

Both approaches produce a pipeline module you can run with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) or
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
just like any other module.

## Create Building-Block Modules

Start with small modules that do one thing well:

``` r

library(dsprrr)
library(ellmer)

mod_extract <- signature("document -> facts") |>
  module(type = "predict", template = "Extract key facts: {document}")

mod_answer <- signature("facts, question -> answer") |>
  module(type = "predict", template = "Use facts: {facts}\nQ: {question}")

mod_format <- signature("answer -> response") |>
  module(type = "predict", template = "Format: {answer}")
```

## Simple Chaining with `%>>%`

When field names align, outputs connect automatically:

``` r

qa_pipeline <- mod_extract %>>% mod_answer %>>% mod_format

llm <- chat_openai()
result <- run(
  qa_pipeline,
  document = "...",
  question = "What happened?",
  .llm = llm
)
```

## Map Inputs When Names Differ

If a downstream module expects a different input name, map fields
explicitly:

``` r

mod_retrieve <- signature("query -> documents") |>
  module(type = "predict", template = "Retrieve docs for: {query}")

mod_summarize <- signature("context -> summary") |>
  module(type = "predict", template = "Summarize: {context}")

rag_pipeline <- mod_retrieve %>>%
  map_inputs(mod_summarize, documents = "context")

result <- run(rag_pipeline, query = "dsprrr pipelines", .llm = llm)
```

## Inject Static Inputs

Use
[`with_inputs()`](https://jameshwade.github.io/dsprrr/reference/with_inputs.md)
to add constants that don't come from upstream outputs:

``` r

mod_answer <- signature("facts, question, tone -> answer") |>
  module(type = "predict")

qa_pipeline <- mod_extract %>>%
  with_inputs(mod_answer, tone = "concise") %>>%
  mod_format
```

## Select Outputs to Pass Forward

Keep only specific fields from a structured output:

``` r

mod_reason <- signature("question -> answer, reasoning") |>
  module(type = "chain_of_thought")

mod_present <- signature("answer -> response") |>
  module(type = "predict")

pipeline_filtered <- mod_reason %>>%
  select_outputs(mod_present, "answer")
```

## Explicit Pipelines with `pipeline()` and `step()`

For more control (or when you prefer not to use `%>>%`), build a
pipeline explicitly:

``` r

explicit_pipeline <- pipeline(
  mod_retrieve,
  step(mod_summarize, map = c(documents = "context")),
  mod_format
)

result <- run(explicit_pipeline, query = "...", .llm = llm)
```

## Batch Execution with `run_dataset()`

Pipelines work with data frames the same way as single modules.
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
adds a `result` column to your input data:

``` r

library(tibble)

questions <- tibble(
  document = c("Doc A", "Doc B"),
  question = c("What changed?", "What is the summary?")
)

results <- run_dataset(
  qa_pipeline,
  questions,
  .llm = llm
)

# Inspect results
results$result
```

## Tips

- Prefer small, composable modules. They are easier to debug and
  optimize.
- Use `trace_summary()` and
  [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)
  to inspect multi-step behavior.
- When column names in your data frame don't match the pipeline inputs,
  rename them first or wrap the pipeline with
  [`map_inputs()`](https://jameshwade.github.io/dsprrr/reference/map_inputs.md)
  to align fields.
