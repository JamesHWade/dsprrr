# Inspect LLM Call History

Returns a tibble of recent LLM calls across all modules and
[`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md) calls.
Similar to DSPy's `dspy.inspect_history(n)`.

## Usage

``` r
inspect_history(n = 10, include_prompts = TRUE, include_responses = TRUE)
```

## Arguments

- n:

  Number of recent calls to return. Default is 10.

- include_prompts:

  Logical; whether to include full prompt text. Default is TRUE.

- include_responses:

  Logical; whether to include full response text. Default is TRUE.

## Value

A tibble with one row per LLM call containing:

- `timestamp`: When the call was made

- `source`: Where the call originated ("dsp()" or module class name)

- `model`: The model used

- `tokens_in`: Input tokens

- `tokens_out`: Output tokens

- `cost`: Cost in USD (if available)

- `duration_s`: Duration in seconds (if available)

- `prompt`: Full prompt text (if `include_prompts = TRUE`)

- `response`: Full response text (if `include_responses = TRUE`)

## Examples

``` r
if (FALSE) { # \dontrun{
# View last 5 LLM calls
inspect_history(n = 5)

# Get history as tibble for analysis
history <- inspect_history(n = 20)
sum(history$cost)  # Total cost
} # }
```
