# Fit LLM Predict Model

Internal function to fit an llm_predict model. Creates a dsprrr module
with the specified configuration.

## Usage

``` r
fit_llm_predict(x, y, signature = NULL, temperature = NULL, top_p = NULL)
```

## Arguments

- x:

  Training data predictors (data frame).

- y:

  Training data outcomes.

- signature:

  Signature for the module.

- temperature:

  Temperature parameter.

- top_p:

  Top-p parameter.

## Value

A fitted dsprrr module.

## Examples

``` r
fit_llm_predict(
  data.frame(text = c("helpful", "unhelpful")),
  factor(c("positive", "negative"))
)
#> 
#> ── PredictModule ──
#> 
#> ── Signature 
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • text: "string" - Input: text
#> 
#> ── Output 
#> Type: "object(output: enum(positive, negative))"
#> 
#> ── Instructions 
#> Given the fields `text`, produce the fields `output`.
```
