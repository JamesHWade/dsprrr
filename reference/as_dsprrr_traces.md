# Convert vitals samples to dsprrr traces

`as_dsprrr_traces()` reshapes vitals samples (from a `Task`'s
`$get_samples()` or from
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md))
into the traces format, so that
[`summarize_traces_df()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces_df.md)
and other trace tools can analyze them.

## Usage

``` r
as_dsprrr_traces(samples, include_prompts = TRUE, include_outputs = TRUE)
```

## Arguments

- samples:

  A samples tibble.

- include_prompts:

  Whether to add a `prompt` column from `input` (default `TRUE`).

- include_outputs:

  Whether to add an `output` list-column from `result` (default `TRUE`).

## Value

A tibble with columns `timestamp`, `latency_ms`, `input_tokens`,
`output_tokens`, `total_tokens`, `cost`, `model` and `prompt_length`,
plus `prompt` and `output` when requested.

## Details

Latency, token counts, cost and timestamp are read from the
`solver_metadata` (or `metadata`) list-column; missing values are `NA`
and missing timestamps are set to the current time.

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
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
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
samples <- as_vitals_samples(tibble::tibble(
  prompt = "question: What is 2+2?",
  output = list(list(answer = "4")),
  model = "gpt-6-luna",
  input_tokens = 52L,
  output_tokens = 6L,
  cost = 0.0001
))
as_dsprrr_traces(samples)
#> # A tibble: 1 × 10
#>   timestamp           latency_ms input_tokens output_tokens total_tokens   cost
#>   <dttm>                   <dbl>        <int>         <int>        <int>  <dbl>
#> 1 2026-09-26 22:00:24         NA           52             6           58 0.0001
#> # ℹ 4 more variables: model <chr>, prompt_length <int>, prompt <chr>,
#> #   output <list>

if (FALSE) { # \dontrun{
traces <- as_dsprrr_traces(tsk$get_samples())
summarize_traces_df(traces)
} # }
```
