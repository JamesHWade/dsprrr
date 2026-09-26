# Build a vitals Task from a module and a data set

`as_vitals_task()` creates a vitals `Task` that uses the module as its
solver (see
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md)),
so you can evaluate the module with vitals' scoring, logging and viewer.
Call the task's `$eval()` method to run it and `$view()` to browse the
results.

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
  .concurrency = NULL,
  ...
)
```

## Arguments

- module:

  A module, such as one created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- dataset:

  A data frame with the signature's input columns and a `target` column.

- scorer:

  A vitals scorer, such as
  [`vitals::detect_includes()`](https://vitals.tidyverse.org/reference/scorer_detect.html).
  The default,
  [`vitals::model_graded_qa()`](https://vitals.tidyverse.org/reference/scorer_model.html),
  asks the solver's chat to grade.

- .llm:

  An ellmer Chat for the solver. `NULL` (the default) uses the module's
  own chat or the default chat, resolved when the task is created.

- name:

  Task name. Defaults to the expression passed as `dataset`.

- epochs:

  Integer number of times each sample is run (default `1L`).

- metrics:

  Optional named list of functions that summarize a vector of scores
  into one number.

- dir:

  Directory for the evaluation logs. Defaults to
  [`vitals::vitals_log_dir()`](https://vitals.tidyverse.org/reference/vitals_log_dir.html).

- .concurrency:

  Optional policy from
  [`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md).

- ...:

  Further arguments passed to
  [`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md).

## Value

A vitals `Task` object.

## Details

`dataset` needs one column per signature input and a `target` column.
The input columns are nested into the `input` list-column that vitals
expects; other columns are kept.

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md),
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
[`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md),
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md),
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md),
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md),
[`register_dsprrr_engine()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_engine.md),
[`summarize_traces_df()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces_df.md),
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
qa <- module(signature("question -> answer"))
test_data <- data.frame(
  question = c("What is 2+2?", "Capital of France?"),
  target = c("4", "Paris")
)
tsk <- as_vitals_task(
  qa,
  test_data,
  scorer = vitals::detect_includes(),
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  dir = tempdir()
)
tsk
#> An evaluation task test-data.

if (FALSE) { # \dontrun{
tsk$eval()
tsk$get_cost()
} # }
```
