# Session Cost Summary

Get cost and token usage summary for the current dsprrr session. This
aggregates data from all LLM calls tracked in the prompt history.

## Usage

``` r
session_cost()
```

## Value

A list with:

- `n_calls`: Integer, number of LLM calls

- `tokens_in`: Integer, total input tokens

- `tokens_out`: Integer, total output tokens

- `total_tokens`: Integer, sum of input and output tokens

- `cost`: Numeric, total estimated cost in USD

- `by_model`: A tibble with per-model breakdown (if available)

## Examples

``` r
if (FALSE) { # \dontrun{
# After running a module a few times
mod <- module(signature("question -> answer"))
run(mod, question = "What is 2+2?")
run(mod, question = "What is the capital of France?")

# Get session summary
session_cost()
#> $n_calls
#> [1] 2
#> $tokens_in
#> [1] 45
#> $tokens_out
#> [1] 12
#> $cost
#> [1] 0.0001

# Access total cost directly
session_cost()$cost
} # }
```
