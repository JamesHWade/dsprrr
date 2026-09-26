# Summarize a module's traces

`summarize_traces()` totals a module's traces: the number of calls,
tokens, cost and latency, and the calls per model. Printing the result
gives a short report.

## Usage

``` r
summarize_traces(module)
```

## Arguments

- module:

  A module.

## Value

A list of class `dsprrr_trace_summary` with `n_traces`, `total_tokens`,
`total_latency_ms` and `total_cost` (in US dollars, `NA` when a cost is
unknown). For a module with traces, it also has `total_input_tokens`,
`total_output_tokens`, `total_cached_tokens`, `total_duration_s`,
`avg_latency_ms`, `avg_tokens_per_request`, `token_breakdown` (input and
output totals and their ratio) and `model_usage` (a data frame of calls
per model).

## See also

Other inspection:
[`accessors`](https://jameshwade.github.io/dsprrr/reference/accessors.md),
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md),
[`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md),
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
[`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md),
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md),
[`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md)

## Examples

``` r
if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
run(qa, question = "What is 2 + 2?", .llm = ellmer::chat_openai(model = "gpt-6-luna"))
summarize_traces(qa)
} # }
```
