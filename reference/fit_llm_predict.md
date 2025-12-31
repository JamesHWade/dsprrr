# Fit LLM Predict Model

Internal function to fit an llm_predict model. Creates a dsprrr module
with the specified configuration.

## Usage

``` r
fit_llm_predict(
  x,
  y,
  signature = NULL,
  temperature = NULL,
  top_p = NULL,
  model = NULL,
  provider = NULL,
  ...
)
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

- model:

  LLM model name.

- provider:

  LLM provider.

- ...:

  Additional arguments.

## Value

A fitted dsprrr module.
