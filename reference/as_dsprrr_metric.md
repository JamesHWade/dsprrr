# Adapt a vitals scorer for use as a dsprrr metric

Converts a vitals scorer function into a per-example metric compatible
with DSPrrr compilation and evaluation. The scorer is invoked on a
single-row tibble constructed from the prediction and the expected row.

## Usage

``` r
as_dsprrr_metric(
  vitals_scorer,
  input_column = "input",
  target_column = "target",
  result_column = "result"
)
```

## Arguments

- vitals_scorer:

  A function that accepts a tibble/data frame and returns a tibble with
  a `score` column (following vitals conventions).

- input_column:

  Name of the column to populate with the example input. If the column
  is absent in `expected_row`, `NA` is supplied.

- target_column:

  Name of the column holding the ground-truth label inside the vitals
  sample tibble.

- result_column:

  Name of the column that receives the model prediction.

## Value

A metric function with signature `function(prediction, expected_row)`
returning numeric values in `[0, 1]` or `NA` when the scorer output
cannot be interpreted.

## Examples

``` r
exact_match <- function(samples) {
  tibble::tibble(
    score = as.numeric(samples$result[[1]] == samples$target[[1]])
  )
}
metric <- as_dsprrr_metric(exact_match)
metric("yes", data.frame(input = "Continue?", target = "yes"))
#> [1] 1
```
