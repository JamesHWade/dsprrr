# Predict Method for Modules (tidymodels-style)

S3 predict method for dsprrr Modules, providing a tidymodels-familiar
interface. This is an alternative to
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
that matches the pattern used by parsnip and other tidymodels packages.

## Usage

``` r
# S3 method for class 'Module'
predict(object, new_data, .llm = NULL, ...)
```

## Arguments

- object:

  A dsprrr Module object

- new_data:

  A data frame or tibble with columns matching the module's signature
  inputs

- .llm:

  Optional ellmer Chat object. When supplied, it takes precedence over
  the Chat stored on `object` and the package default.

- ...:

  Additional arguments passed to
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)

## Value

A tibble with the input columns plus prediction results. The output
column is named according to the signature's output field.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a module
mod <- signature("text -> sentiment") |>
  module( chat = chat_openai())

# Use predict() like parsnip models
new_data <- tibble::tibble(text = c("Great!", "Terrible"))
predict(mod, new_data)

# Equivalent to run_dataset()
run_dataset(mod, new_data, .llm = mod$chat)
} # }
```
