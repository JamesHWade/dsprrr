# Summarize a traces data frame

Provides summary statistics for a traces data frame. This is a
standalone version of
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md)
that works on a data frame rather than requiring a Module object. Useful
for analyzing converted vitals samples.

## Usage

``` r
summarize_traces_df(traces)
```

## Arguments

- traces:

  A traces tibble (from
  [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
  [`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
  or module `$get_traces()`).

## Value

A list with:

- `n_traces`: Number of traces

- `total_tokens`: Total tokens used across all traces

- `total_input_tokens`: Total input tokens

- `total_output_tokens`: Total output tokens

- `total_cost`: Total cost in USD

- `total_latency_ms`: Sum of latencies

- `avg_latency_ms`: Average latency per request

- `avg_tokens_per_request`: Average tokens per request

- `model_usage`: Data frame with per-model breakdown

## Examples

``` r
if (FALSE) { # \dontrun{
# Analyze traces from vitals samples
traces <- as_dsprrr_traces(task$get_samples())
summary <- summarize_traces_df(traces)
print(summary)
} # }
```
