# Convert a dsprrr module into a vitals solver

Creates a function compatible with vitals Tasks that executes a DSPrrr
module against batches of inputs. The solver uses
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
internally, ensuring that the module's demos, templates, and input
descriptions are properly used in prompt construction.

For multi-input modules, the solver expects the vitals `input` column to
contain nested data (list of tibbles/lists) where each element has
fields matching the module's signature inputs. Use
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md)
to automatically create this structure from a flat dataset.

Batch execution is sequential unless `.concurrency` requests another
backend. For structured outputs, mock Chat objects are created for
vitals logging compatibility (following the same pattern as vitals'
[`generate_structured()`](https://vitals.tidyverse.org/reference/generate_structured.html)).

## Usage

``` r
as_vitals_solver(module, .llm = NULL, .concurrency = NULL, ...)
```

## Arguments

- module:

  A DSPrrr module (e.g., created via
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)).

- .llm:

  An ellmer chat object. If `NULL` (default), uses the module's stored
  chat or falls back to
  [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md).
  The chat is cloned for each batch invocation.

- .concurrency:

  Optional policy created by
  [`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md).
  Omission uses sequential execution.

- ...:

  Additional arguments forwarded to
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).

## Value

A function accepting a list of input objects and returning a list with
components `result`, `solver_chat`, and optionally `solver_metadata`.
