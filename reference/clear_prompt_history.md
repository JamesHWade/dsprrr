# Clear the prompt history

`clear_prompt_history()` empties the session's prompt history, which
[`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md),
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md)
and
[`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md)
read. Module traces are kept; see
[`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md)
for those.

## Usage

``` r
clear_prompt_history()
```

## Value

The number of entries removed, invisibly. A message reports it when the
history was not empty.

## See also

Other inspection:
[`accessors`](https://jameshwade.github.io/dsprrr/reference/accessors.md),
[`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md),
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
[`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md),
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md),
[`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md),
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md)

## Examples

``` r
removed <- clear_prompt_history()
removed
#> [1] 0
```
