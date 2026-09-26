# Top-p parameter for dials

`top_p()` creates a dials parameter for nucleus sampling (the share of
probability mass the model samples from), for tuning
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md)
specifications or building grids for
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).
Like
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
it applies to gpt-6-luna only when its reasoning effort is `"none"`.

## Usage

``` r
top_p(range = c(0, 1), trans = NULL)
```

## Arguments

- range:

  Range of values (default `c(0, 1)`).

- trans:

  Optional transformation from the scales package; `NULL` (the default)
  means none.

## Value

A dials quantitative parameter.

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
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
top_p()
#> Top P (quantitative)
#> Range: [0, 1]
top_p(range = c(0.5, 1))
#> Top P (quantitative)
#> Range: [0.5, 1]
```
