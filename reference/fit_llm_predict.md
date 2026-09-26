# Engine functions behind llm_predict()

These functions implement the "dsprrr" engine of
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md).
parsnip calls them; they are exported for that purpose, not for direct
use.

- `fit_llm_predict()` builds a Predict module from the predictors `x`
  and outcome `y`. Without a `signature`, the inputs are the columns of
  `x` and the output is an enum of the outcome's levels (factor or
  character `y`) or a number.

- `predict_llm_class()` runs the module on `new_data` with the default
  chat and returns a tibble with a `.pred_class` factor column.

- `predict_llm_numeric()` does the same and returns a `.pred` column;
  outputs that cannot be converted to numbers become `NA` with a
  warning.

## Usage

``` r
fit_llm_predict(x, y, signature = NULL, temperature = NULL, top_p = NULL)

predict_llm_class(object, new_data, ...)

predict_llm_numeric(object, new_data, ...)
```

## Arguments

- x:

  Data frame of predictors.

- y:

  Outcome vector.

- signature:

  Optional signature string or
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
  object.

- temperature, top_p:

  Optional sampling settings stored in the module's config.

- object:

  A module returned by `fit_llm_predict()`.

- new_data:

  Data frame with the predictor columns.

- ...:

  Unused.

## Value

`fit_llm_predict()` returns a Predict module. The predict functions
return a tibble with one row per row of `new_data`.

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

if (FALSE) { # \dontrun{
set_default_chat(ellmer::chat_openai(model = "gpt-6-luna"))
fit <- fit_llm_predict(
  data.frame(text = c("helpful", "unhelpful")),
  factor(c("positive", "negative"))
)
predict_llm_class(fit, data.frame(text = "clear and useful"))
} # }
```
