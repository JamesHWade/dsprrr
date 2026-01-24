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

The solver uses ellmer's parallel processing for efficiency. For
structured outputs, mock Chat objects are created for vitals logging
compatibility (following the same pattern as vitals'
`generate_structured()`).

## Usage

``` r
as_vitals_solver(module, .llm = NULL, ...)
```

## Arguments

- module:

  A DSPrrr module (e.g., created via
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)).

- .llm:

  An ellmer chat object (required). This will be cloned for each chat
  invocation.

- ...:

  Additional arguments forwarded to
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).

## Value

A function accepting a list of input objects and returning a list with
components `result`, `solver_chat`, and optionally `solver_metadata`.
