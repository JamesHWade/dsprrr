# Create a vitals Task from a dsprrr module

Convenience function that builds a vitals
[vitals::Task](https://vitals.tidyverse.org/reference/Task.html) from a
dsprrr module and dataset. This makes it trivial to evaluate dsprrr
modules using vitals infrastructure without manual solver wrapping.

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

  A tibble/data frame with columns `input` and `target`. The `input`
  column contains prompts and `target` contains expected values or
  grading guidance.

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
# Create a simple QA module
mod <- module(signature("question -> answer"))

# Prepare test dataset
test_data <- tibble::tibble(
  input = c("What is 2+2?", "What is the capital of France?"),
  target = c("4", "Paris")
)

# Create task with string detection scorer
task <- as_vitals_task(
  module = mod,
  dataset = test_data,
  scorer = vitals::detect_includes(),
  .llm = ellmer::chat_openai()
)

# Run evaluation and view results
task
} # }
```
