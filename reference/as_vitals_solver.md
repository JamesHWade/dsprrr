# Convert a dsprrr module into a vitals solver

Creates a function compatible with vitals Tasks that executes a DSPrrr
module against batches of inputs. The solver forwards arguments to
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
and returns vitals-friendly objects containing results, chat logs, and
metadata.

## Usage

``` r
as_vitals_solver(
  module,
  .llm = NULL,
  .parallel = FALSE,
  .return_format = "structured",
  .input_column = NULL,
  ...
)
```

## Arguments

- module:

  A DSPrrr module (e.g., created via
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)).

- .llm:

  Optional ellmer chat object. When `NULL`, each invocation will create
  a fresh default client.

- .parallel:

  Logical; forwarded to
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).
  Defaults to `FALSE` to avoid sharing LLM state across workers.

- .return_format:

  One of `"structured"` (default) or `"simple"`.

- .input_column:

  Column name to use for the module's input. Defaults to the first input
  name from the module's signature. Used to map vitals' "input" column
  to the module's expected input column.

- ...:

  Additional arguments forwarded to
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).

## Value

A function accepting a data frame of inputs and returning a list with
components `result`, `solver_chat`, and `metadata`.
