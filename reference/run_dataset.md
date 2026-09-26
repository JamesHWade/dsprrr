# Run a module on each row of a data frame

`run_dataset()` runs a module once per row of `data`, taking each input
from the column with the same name, and adds the outputs as a `result`
list-column. It works for every module type, including those that
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) only
accepts one input at a time.

## Usage

``` r
run_dataset(module, ...)

# S3 method for class 'Module'
run_dataset(
  module,
  data,
  .llm = NULL,
  .verbose = FALSE,
  .concurrency = NULL,
  .progress = TRUE,
  .return_format = "simple",
  ...,
  .trace_context = list()
)
```

## Arguments

- module:

  A module, such as one created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  or
  [`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md).

- ...:

  Only `.cache` is accepted here, with the same meaning as in
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md). Any
  other dot-prefixed argument is an error.

- data:

  A data frame with one column per required signature input. Other
  columns (such as expected answers) are kept in the result but not sent
  to the module.

- .llm:

  An ellmer Chat for all rows. See
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
  how it is chosen when omitted.

- .verbose:

  If `TRUE`, print the rendered input section of each prompt.

- .concurrency:

  A policy from
  [`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md).
  The default runs rows one after another.

- .progress:

  Show a progress bar. Default `TRUE`.

- .return_format:

  `"simple"` (the default) or `"structured"`.

- .trace_context:

  A named, JSON-compatible list copied into each row's metadata and
  traces. When omitted inside another dsprrr operation, the active
  context is inherited.

## Value

A tibble with the columns of `data` plus `result`, a list-column holding
each row's output (a named list, as returned by
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)). With
`.return_format = "structured"`, it also has `.error` (`NA` for rows
that succeeded), `.metadata` and `.chat`.

## Details

A row that fails gets `NA` in `result` and a warning; with
`.return_format = "structured"` its error message is in `.error`. A
zero-row data frame returns a zero-row tibble with the same columns,
without calling the model.

## See also

Other execution:
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`predict.Module()`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md),
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md),
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)

## Examples

``` r
# A function-backed module runs without a model
shout <- module_fn("text -> reply", function(text) toupper(text))
reviews <- data.frame(text = c("great", "broken"), stars = c(5, 1))
results <- run_dataset(shout, reviews)
results
#> # A tibble: 2 × 3
#>   text   stars result          
#>   <chr>  <dbl> <list>          
#> 1 great      5 <named list [1]>
#> 2 broken     1 <named list [1]>
results$result[[1]]$reply
#> [1] "GREAT"

if (FALSE) { # \dontrun{
classify <- module(signature("text -> sentiment"))
run_dataset(
  classify,
  reviews,
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .return_format = "structured"
)
} # }
```
