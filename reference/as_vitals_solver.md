# Convert a dsprrr module into a vitals solver

Creates a function compatible with vitals Tasks that executes a DSPrrr
module against batches of inputs. The solver forwards arguments to
[`run_dataset()`](run_dataset.md) and returns vitals-friendly objects
containing results, chat logs, and metadata.

## Usage

``` r
as_vitals_solver(
  module,
  .llm = NULL,
  .parallel = FALSE,
  .return_format = "structured",
  ...
)
```

## Arguments

- module:

  A DSPrrr module (e.g., created via [`module()`](module.md)).

- .llm:

  Optional ellmer chat object. When `NULL`, each invocation will create
  a fresh default client.

- .parallel:

  Logical; forwarded to [`run_dataset()`](run_dataset.md). Defaults to
  `FALSE` to avoid sharing LLM state across workers.

- .return_format:

  One of `"structured"` (default) or `"simple"`.

- ...:

  Additional arguments forwarded to [`run_dataset()`](run_dataset.md).

## Value

A function accepting a data frame of inputs and returning a list with
components `result`, `solver_chat`, and `metadata`.
