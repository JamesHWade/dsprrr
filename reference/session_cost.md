# Summarize this session's token use and cost

`session_cost()` adds up the tokens and estimated cost of the model
calls in the prompt history (see
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md)),
overall and per model.

## Usage

``` r
session_cost()
```

## Value

A list of class `dsprrr_session_cost` with `n_calls`, `tokens_in`,
`tokens_out`, `total_tokens`, `cost` (in US dollars; `NA` if any call's
cost is unknown) and `by_model`, a tibble with the same totals per
model.

## Details

The prompt history keeps the most recent 100 calls by default
(`options(dsprrr.prompt_history_max = )`), so older calls drop out of
the totals, and
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md)
resets them. Only calls recorded in the history count: see
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md)
for which ones are. Costs are ellmer's estimates.

## See also

Other inspection:
[`accessors`](https://jameshwade.github.io/dsprrr/reference/accessors.md),
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md),
[`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md),
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
[`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md),
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md),
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md)

## Examples

``` r
session_cost()
#> 
#> ── dsprrr Session Cost 
#> No LLM calls recorded in this session
session_cost()$total_tokens
#> [1] 0
```
