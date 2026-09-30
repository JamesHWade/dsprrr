# Copy a workflow template into a project

`use_dsprrr_template()` copies starter files for running dsprrr in a
pipeline: a targets pipeline (`_targets.R`) that prepares data,
optimizes and evaluates a module and pins the results, and a Quarto
report (`report.qmd`). Edit the copies to fit your project.

## Usage

``` r
use_dsprrr_template(
  template = c("targets", "quarto", "all"),
  path = ".",
  overwrite = FALSE
)
```

## Arguments

- template:

  `"targets"`, `"quarto"` or `"all"`.

- path:

  Directory to copy into (default: the working directory). It is created
  if needed.

- overwrite:

  Whether to replace existing files (default `FALSE`). Existing files
  are otherwise skipped with a warning.

## Value

The paths of the created files, invisibly.

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md),
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
[`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md),
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md),
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md),
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md),
[`register_dsprrr_engine()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_engine.md),
[`summarize_traces_df()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces_df.md),
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
project <- file.path(tempdir(), "my-project")
use_dsprrr_template("targets", path = project)
#> Created: /tmp/RtmpQlgZnT/my-project/_targets.R
list.files(project)
#> [1] "_targets.R"
```
