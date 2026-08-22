# LLM Prediction Model Specification

Creates a parsnip model specification for LLM-based prediction using
dsprrr.

## Usage

``` r
llm_predict(
  mode = "classification",
  signature = NULL,
  temperature = NULL,
  top_p = NULL
)
```

## Arguments

- mode:

  Model mode, typically "classification" or "regression" (for text
  tasks, classification is most common).

- signature:

  A dsprrr signature string or Signature object.

- temperature:

  Temperature parameter for LLM (tune-able).

- top_p:

  Top-p parameter for LLM (tune-able).

## Value

A parsnip model specification object.

## Examples

``` r
if (FALSE) { # \dontrun{
library(parsnip)
library(tune)

# Create LLM model spec
llm_spec <- llm_predict(
  mode = "classification",
  signature = "text -> sentiment: enum('positive', 'negative', 'neutral')"
) |>
  set_engine("dsprrr")

# With tunable parameters
llm_spec_tuned <- llm_predict(
  mode = "classification",
  signature = "text -> sentiment",
  temperature = tune()
) |>
  set_engine("dsprrr")
} # }
```
