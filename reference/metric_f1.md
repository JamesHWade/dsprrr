# Token-overlap F1 metric

`metric_f1()` makes a metric that scores how many words the prediction
and the expected text share, as the F1 score (harmonic mean of precision
and recall) of their word counts. It gives partial credit, which suits
free-text answers.

## Usage

``` r
metric_f1(field = NULL, normalize = TRUE)
```

## Arguments

- field:

  Name of the output field to compare.
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
  and
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  pass the whole data row as `expected`; when `field` is `NULL`, the
  metric compares the one prediction field that is also a column of that
  row, and errors if there is not exactly one.

- normalize:

  If `TRUE` (the default), lower-case both texts and replace punctuation
  with spaces before splitting them into words.

## Value

A function `function(prediction, expected)` returning a number between 0
and 1. Two empty texts score 1. `field` is stored in its `"field"`
attribute.

## See also

Other metrics:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
[`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md),
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
[`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
[`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
f1 <- metric_f1(field = "answer")
row <- data.frame(question = "Where is the Louvre?", answer = "in Paris, France")
f1(list(answer = "Paris"), row)
#> [1] 0.5
f1(list(answer = "It is in Paris, France"), row)
#> [1] 0.75
```
