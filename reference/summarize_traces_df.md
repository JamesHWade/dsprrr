# Summarize a traces data frame

`summarize_traces_df()` totals tokens, cost and latency over a traces
data frame. It is the data frame version of
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md),
which takes a module; use it for traces exported with
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)
or converted from vitals with
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md).

## Usage

``` r
summarize_traces_df(traces)
```

## Arguments

- traces:

  A traces data frame with `input_tokens`, `output_tokens`,
  `total_tokens`, `latency_ms`, `cost` and, optionally, `model` columns.

## Value

A `dsprrr_trace_summary` list with `n_traces`, `total_tokens`,
`total_input_tokens`, `total_output_tokens`, `total_cost`,
`total_latency_ms`, `avg_latency_ms`, `avg_tokens_per_request`,
`token_breakdown` (input, output and their ratio) and `model_usage` (a
data frame of requests per model).

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
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
traces <- data.frame(
  model = c("gpt-6-luna", "gpt-6-luna"),
  input_tokens = c(52L, 55L),
  output_tokens = c(6L, 7L),
  total_tokens = c(58L, 62L),
  latency_ms = c(820, 640),
  cost = c(0.0001, 0.0001)
)
summary <- summarize_traces_df(traces)
summary$total_tokens
#> [1] 120
summary$model_usage
#>        model n_requests
#> 1 gpt-6-luna          2
```
