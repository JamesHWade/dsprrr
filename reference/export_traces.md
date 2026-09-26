# Export a module's traces as a tibble

Every call of a module records a trace on it: when it ran, how long it
took, the tokens and cost, and the model. `export_traces()` returns
these traces as a tibble with one row per call, for analysis or
plotting. Prompts and outputs are left out unless you ask for them,
because they can contain sensitive data.

## Usage

``` r
export_traces(module, include_prompts = FALSE, include_outputs = FALSE)
```

## Arguments

- module:

  A module.

- include_prompts:

  If `TRUE`, add the prompts, as `prompt` (plain text),
  `prompt_markdown` and `prompt_html`.

- include_outputs:

  If `TRUE`, add the outputs and responses, as `output` (a list-column),
  `turns`, `response`, `response_text`, `response_markdown` and
  `response_html`.

## Value

A tibble with one row per trace and the columns `timestamp`,
`latency_ms`, `input_tokens`, `cached_input_tokens`, `output_tokens`,
`total_tokens`, `cost` (in US dollars, when known), `model`,
`prompt_length`, `program_artifact_id` and `trace_context`, plus the
columns requested above. A module without traces gives an empty tibble
and a message.

## See also

Other inspection:
[`accessors`](https://jameshwade.github.io/dsprrr/reference/accessors.md),
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md),
[`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md),
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
run(shout, text = "goodbye")
#> $reply
#> [1] "GOODBYE"
#> 
export_traces(shout)
#> # A tibble: 2 × 11
#>     timestamp latency_ms input_tokens cached_input_tokens output_tokens
#>         <dbl>      <dbl>        <int>               <int>         <int>
#> 1 1790460038.     0.0203           NA                  NA            NA
#> 2 1790460038.     0.0212           NA                  NA            NA
#> # ℹ 6 more variables: total_tokens <int>, cost <dbl>, model <chr>,
#> #   prompt_length <int>, program_artifact_id <chr>, trace_context <list>
export_traces(shout, include_outputs = TRUE)$output
#> [[1]]
#> [[1]]$reply
#> [1] "HELLO"
#> 
#> 
#> [[2]]
#> [[2]]$reply
#> [1] "GOODBYE"
#> 
#> 
```
