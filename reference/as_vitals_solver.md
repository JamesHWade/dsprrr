# Use a dsprrr module as a vitals solver

`as_vitals_solver()` wraps a module in a function that a vitals `Task`
can call as its solver. The solver runs the module with
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
so the module's demonstrations, template and input descriptions are used
as usual.
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md)
builds the whole task in one step.

## Usage

``` r
as_vitals_solver(module, .llm = NULL, .concurrency = NULL, ...)
```

## Arguments

- module:

  A module, such as one created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- .llm:

  An ellmer Chat. `NULL` (the default) uses the module's own chat or the
  default chat from
  [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
  resolved when the solver is created. Each batch runs on a fresh clone.

- .concurrency:

  Optional policy from
  [`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md).

- ...:

  Further arguments passed to
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).

## Value

A function `function(inputs, ..., solver_chat)` that returns a list with
`result` (character vector), `solver_chat` (the chats used) and, for
non-text outputs, `solver_metadata`.

## Details

vitals passes the solver a list of inputs. For a module with several
inputs, each element must be a one-row data frame or list with fields
named after the signature inputs;
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md)
creates that structure from a flat data set.

String and enum outputs, including a single string or enum field, are
returned as plain text, which string scorers such as
[`vitals::detect_match()`](https://vitals.tidyverse.org/reference/scorer_detect.html)
can compare. Other outputs are returned as JSON, together with per-row
metadata. Rows run one after another unless `.concurrency` asks for
more.

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md),
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
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
solver <- as_vitals_solver(
  module(signature("question -> answer")),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)

if (FALSE) { # \dontrun{
tsk <- vitals::Task$new(
  dataset = tibble::tibble(input = "What is 2 + 2?", target = "4"),
  solver = solver,
  scorer = vitals::detect_includes()
)
tsk$eval()
} # }
```
