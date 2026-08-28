# Predict Numeric Values with LLM

Internal function for numeric predictions with llm_predict models.

## Usage

``` r
predict_llm_numeric(object, new_data, ...)
```

## Arguments

- object:

  A fitted dsprrr module.

- new_data:

  New data for prediction.

- ...:

  Additional arguments.

## Value

A tibble with .pred column.

## Examples

``` r
if (FALSE) { # \dontrun{
set_default_chat(ellmer::chat_openai())
fit <- fit_llm_predict(
  data.frame(text = c("short", "much longer")),
  c(1, 2)
)
predict_llm_numeric(fit, data.frame(text = "medium length"))
} # }
```
