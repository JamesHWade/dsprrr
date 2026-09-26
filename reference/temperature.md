# Temperature parameter for dials

`temperature()` creates a dials parameter for a model's sampling
temperature, for tuning
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md)
specifications or building grids for
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).
Reasoning models restrict it: gpt-6-luna, for example, accepts
`temperature` only when its reasoning effort is `"none"`.

## Usage

``` r
temperature(range = c(0, 1), trans = NULL)
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
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
temperature()
#> Temperature (quantitative)
#> Range: [0, 1]
temperature(range = c(0.1, 0.9))
#> Temperature (quantitative)
#> Range: [0.1, 0.9]
dials::grid_regular(temperature(), levels = 3)
#> # A tibble: 3 × 1
#>   temperature
#>         <dbl>
#> 1         0  
#> 2         0.5
#> 3         1  
```
