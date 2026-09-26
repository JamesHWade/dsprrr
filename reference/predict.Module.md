# Predict with a module on new data

[`predict()`](https://rdrr.io/r/stats/predict.html) on a module is
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
under the name tidymodels users expect: `predict(module, new_data)` is
`run_dataset(module, new_data)`.

## Usage

``` r
# S3 method for class 'Module'
predict(object, new_data, .llm = NULL, ...)
```

## Arguments

- object:

  A module, such as one created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- new_data:

  A data frame with one column per signature input.

- .llm:

  An ellmer Chat to use instead of the one stored on `object` or the
  default chat.

- ...:

  Passed to
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
  for example `.return_format`, `.concurrency` or `.cache`.

## Value

A tibble with the columns of `new_data` plus a `result` list-column
holding each row's output as a named list, exactly as returned by
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).

## See also

Other execution:
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md),
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)

## Examples

``` r
shout <- module_fn("text -> reply", function(text) toupper(text))
predictions <- predict(shout, data.frame(text = c("great", "terrible")))
predictions
#> # A tibble: 2 × 2
#>   text     result          
#>   <chr>    <list>          
#> 1 great    <named list [1]>
#> 2 terrible <named list [1]>
vapply(predictions$result, function(r) r$reply, character(1))
#> [1] "GREAT"    "TERRIBLE"
```
