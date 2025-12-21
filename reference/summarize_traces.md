# Summarize Module Traces

Provide a statistical summary of module traces for performance analysis.

## Usage

``` r
summarize_traces(module)
```

## Arguments

- module:

  A DSPrrr module with recorded traces

## Value

A list containing:

- n_traces: Number of traces

- total_tokens: Total tokens used across all traces

- total_cost: Total cost in USD

- avg_latency_ms: Average latency per request

- avg_tokens_per_request: Average tokens per request

- token_breakdown: List with input/output token totals

- model_usage: Table of requests per model

## Examples

``` r
if (FALSE) { # \dontrun{
summary <- summarize_traces(my_module)
print(summary)
} # }
```
