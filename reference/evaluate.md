# Evaluate a DSPrrr module

Generic evaluation entry point for DSPrrr modules. Executes the module
on a dataset, applies a metric to each example, and returns aggregate
statistics together with the predictions and metadata required for
downstream analysis.

## Usage

``` r
evaluate(module, ...)
```

## Arguments

- module:

  A DSPrrr module created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- ...:

  Additional arguments including:

  - dataset: A data frame or tibble containing columns that match the
    module's signature inputs plus any expected fields used by metric

  - metric: A function applied per example with signature
    metric(prediction, expected_row)

  - .llm: Optional ellmer chat object supplied to run()

  - .parallel: Logical; whether to allow parallel execution

  - .progress: Logical; whether to display progress while evaluating

## Value

A list with elements

- `mean_score`: numeric mean over all successful metric evaluations.

- `scores`: per-example numeric scores (coerced from logical metrics).

- `predictions`: list of model outputs.

- `metadata`: list of metadata captured from
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

- `n_evaluated`: number of successful evaluations.

- `n_errors`: number of metric failures.

- `errors`: character vector with error messages, when any.

- `dataset`: input dataset augmented with prediction metadata.
