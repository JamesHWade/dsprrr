# Predict Class Labels with LLM

Internal function for class predictions with llm_predict models.

## Usage

``` r
predict_llm_class(object, new_data, ...)
```

## Arguments

- object:

  A fitted dsprrr module.

- new_data:

  New data for prediction.

- ...:

  Additional arguments.

## Value

A tibble with .pred_class column.

## Examples

``` r
if (FALSE) { # \dontrun{
set_default_chat(ellmer::chat_openai())
fit <- fit_llm_predict(
  data.frame(text = c("helpful", "unhelpful")),
  factor(c("positive", "negative"))
)
predict_llm_class(fit, data.frame(text = "clear and useful"))
} # }
```
