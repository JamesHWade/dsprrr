# Report dsprrr costs in the vitals format

`as_vitals_cost()` converts dsprrr cost information into the tibble that
a vitals `Task`'s `$get_cost()` method returns, so costs from both
packages can be reported together.

## Usage

``` r
as_vitals_cost(x, source = "solver", ...)
```

## Arguments

- x:

  One of:

  - a session summary from
    [`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md)
    (one row per model);

  - a traces data frame, such as
    [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)
    output, with `model`, `input_tokens`, `output_tokens` and `cost`
    columns (one row per model);

  - a cost summary from
    [`get_cost()`](https://jameshwade.github.io/dsprrr/reference/accessors.md)
    or an
    [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
    result (a single row with the total price, and model and token
    counts unknown). This currently fails when the total cost is
    unknown, for example for a model without price data.

- source:

  Label for the `source` column (default `"solver"`, the vitals
  convention; vitals also uses `"scorer"`).

- ...:

  Unused.

## Value

A tibble with columns `source`, `provider` (guessed from the model
name), `model`, `input` and `output` (token counts), and `price` (text
such as `"$0.01"`).

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
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
traces <- data.frame(
  model = c("gpt-6-luna", "gpt-6-luna", "claude-sonnet-4-5"),
  input_tokens = c(12000L, 8000L, 20000L),
  output_tokens = c(3000L, 2500L, 6000L),
  cost = c(0.21, 0.15, 0.42)
)
as_vitals_cost(traces)
#> # A tibble: 2 × 6
#>   source provider  model             input output price
#>   <chr>  <chr>     <chr>             <int>  <int> <chr>
#> 1 solver OpenAI    gpt-6-luna        20000   5500 $0.36
#> 2 solver Anthropic claude-sonnet-4-5 20000   6000 $0.42

# Nothing has run in this session yet
as_vitals_cost(session_cost())
#> # A tibble: 0 × 6
#> # ℹ 6 variables: source <chr>, provider <chr>, model <chr>, input <int>,
#> #   output <int>, price <chr>

if (FALSE) { # \dontrun{
result <- evaluate(
  classifier,
  testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
as_vitals_cost(result)
} # }
```
