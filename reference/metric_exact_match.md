# Exact-match metric

`metric_exact_match()` makes a metric that is `TRUE` when the prediction
equals the expected value, compared as text, and `FALSE` otherwise.

## Usage

``` r
metric_exact_match(field = NULL, ignore_case = FALSE, normalize = TRUE)
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

- ignore_case:

  If `TRUE`, compare without regard to case.

- normalize:

  If `TRUE` (the default), trim white space at both ends and collapse
  runs of white space before comparing.

## Value

A function `function(prediction, expected)` returning `TRUE` or `FALSE`,
for
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
and the optimizers. `field` is stored in its `"field"` attribute.

## See also

Other metrics:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
[`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md),
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
[`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
[`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
match_sentiment <- metric_exact_match(field = "sentiment")

# How evaluate() calls it: the prediction, then the whole data row
row <- data.frame(text = "Great!", sentiment = "positive")
match_sentiment(list(sentiment = "positive"), row)
#> [1] TRUE
match_sentiment(list(sentiment = "negative"), row)
#> [1] FALSE

# Called directly on two values
metric_exact_match(ignore_case = TRUE)("Paris ", "paris")
#> [1] TRUE
```
