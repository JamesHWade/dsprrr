# Convert dsprrr traces to vitals samples

`as_vitals_samples()` reshapes a traces data frame into the samples
format that a vitals `Task`'s `$get_samples()` returns, for example to
combine dsprrr runs with vitals results using
[`vitals::vitals_bind()`](https://vitals.tidyverse.org/reference/vitals_bind.html).
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md)
converts the other way.

## Usage

``` r
as_vitals_samples(traces, input_column = NULL, include_chats = FALSE)
```

## Arguments

- traces:

  A traces data frame, such as
  [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)
  output. Export with `include_prompts = TRUE` and
  `include_outputs = TRUE`; otherwise `input` and `result` are empty.

- input_column:

  Name of the column to use as `input`. `NULL` (the default) uses
  `prompt`.

- include_chats:

  Whether to keep a `solver_chat` column when `traces` has one (default
  `FALSE`).

## Value

A tibble with columns `id` (`"trace_0001"`, ...), `input`, `result`
(list-column from the `output` column), `solver_metadata` (list-column
with latency, tokens, cost and timestamp), `model` and `epoch` (always
1).

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
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
traces <- tibble::tibble(
  prompt = c("question: What is 2+2?", "question: Capital of France?"),
  output = list(list(answer = "4"), list(answer = "Paris")),
  model = "gpt-6-luna",
  latency_ms = c(820, 640),
  input_tokens = c(52L, 55L),
  output_tokens = c(6L, 7L),
  cost = c(0.0001, 0.0001)
)
as_vitals_samples(traces)
#> # A tibble: 2 × 6
#>   id         input                      result       solver_metadata model epoch
#>   <chr>      <chr>                      <list>       <list>          <chr> <int>
#> 1 trace_0001 question: What is 2+2?     <named list> <named list>    gpt-…     1
#> 2 trace_0002 question: Capital of Fran… <named list> <named list>    gpt-…     1

if (FALSE) { # \dontrun{
samples <- as_vitals_samples(
  export_traces(classifier, include_prompts = TRUE, include_outputs = TRUE)
)
vitals::vitals_bind(dsprrr = samples)
} # }
```
