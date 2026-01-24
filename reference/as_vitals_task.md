# Create a vitals Task from a dsprrr module

Convenience function that builds a vitals
[vitals::Task](https://vitals.tidyverse.org/reference/Task.html) from a
dsprrr module and dataset. This makes it trivial to evaluate dsprrr
modules using vitals infrastructure without manual solver wrapping.

For multi-input modules, the function automatically nests all signature
input columns into a single `input` list column that vitals expects. The
solver then extracts these fields when processing each sample.

## Usage

``` r
as_vitals_task(
  module,
  dataset,
  scorer = NULL,
  .llm = NULL,
  name = NULL,
  epochs = 1L,
  metrics = NULL,
  dir = NULL,
  .parallel = FALSE,
  ...
)
```

## Arguments

- module:

  A DSPrrr module (e.g., created via
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)).

- dataset:

  A tibble/data frame with columns matching the module's signature
  inputs plus a `target` column. The function will nest signature inputs
  into the `input` column format vitals requires.

- scorer:

  A vitals scorer function (e.g.,
  [`vitals::model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html),
  [`vitals::detect_match()`](https://vitals.tidyverse.org/reference/scorer_detect.html)).
  Defaults to
  [`vitals::model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html).

- .llm:

  Optional ellmer chat object for the solver. When `NULL`, each
  invocation will create a fresh default client.

- name:

  Optional name for the task. Defaults to the dataset name.

- epochs:

  Number of times to repeat each sample for statistical significance.
  Defaults to 1L.

- metrics:

  Optional named list of metric functions. Each function takes a vector
  of scores and returns a single numeric value.

- dir:

  Directory for evaluation logs. Defaults to
  [`vitals::vitals_log_dir()`](https://vitals.tidyverse.org/reference/vitals_log_dir.html).

- .parallel:

  Logical; whether to run solver in parallel. Defaults to FALSE.

- ...:

  Additional arguments passed to
  [`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md).

## Value

A vitals
[vitals::Task](https://vitals.tidyverse.org/reference/Task.html) object
ready for evaluation.

## Details

The returned Task object can be evaluated by calling its `$eval()`
method, which runs the solver, scores results, computes metrics, and
logs output. Use `$view()` to see results interactively.

## Examples

``` r
if (FALSE) { # \dontrun{
# Single-input module
mod <- module(signature("question -> answer"))
test_data <- tibble::tibble(
  question = c("What is 2+2?", "Capital of France?"),
  target = c("4", "Paris")
)
task <- as_vitals_task(mod, test_data, scorer = vitals::detect_includes())

# Multi-input module
mod <- module(signature("shapes, pick -> answer"))
test_data <- tibble::tibble(
  shapes = c("square, circle", "triangle, star"),
  pick = c("square", "star"),
  target = c("square", "star")
)
task <- as_vitals_task(mod, test_data, scorer = vitals::detect_includes())
} # }
```
