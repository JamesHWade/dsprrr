# Clear a module's traces

`clear_traces()` removes the traces recorded on a module and keeps
everything else, such as its demos and settings. The module is changed
in place. The session's prompt history is separate; see
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md).

## Usage

``` r
clear_traces(module)
```

## Arguments

- module:

  A module.

## Value

The module, invisibly. A message reports how many traces were removed.

## See also

Other inspection:
[`accessors`](https://jameshwade.github.io/dsprrr/reference/accessors.md),
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md),
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
[`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md),
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md),
[`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md),
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md)

## Examples

``` r
shout <- module_fn("text -> reply", function(text) toupper(text))
run(shout, text = "hello")
#> $reply
#> [1] "HELLO"
#> 
clear_traces(shout)
#> Cleared 1 trace
nrow(export_traces(shout))
#> No traces recorded in this module
#> [1] 0
```
