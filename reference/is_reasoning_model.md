# Check if a model is a reasoning model

Reasoning models (OpenAI o1/o3/o4-mini, GPT-5 series) use different
parameters than traditional models. They don't support `temperature` or
`top_p`, instead using `reasoning_effort` (low/medium/high).

## Usage

``` r
is_reasoning_model(model_name)
```

## Arguments

- model_name:

  Character string of the model name (e.g., "o3", "gpt-5").

## Value

Logical indicating whether the model is a reasoning model.

## Examples

``` r
is_reasoning_model("gpt-4o")      # FALSE
#> [1] FALSE
is_reasoning_model("o3")          # TRUE
#> [1] TRUE
is_reasoning_model("o4-mini")     # TRUE
#> [1] TRUE
is_reasoning_model("gpt-5")       # TRUE
#> [1] TRUE
```
