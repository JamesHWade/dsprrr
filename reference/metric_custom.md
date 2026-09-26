# Wrap a custom metric function

`metric_custom()` wraps your own scoring function so that its errors
name the metric and its numeric scores stay between 0 and 1. Any
function `function(prediction, expected)` already works as a metric; the
wrapper only adds these checks.

## Usage

``` r
metric_custom(fn, name = NULL)
```

## Arguments

- fn:

  A function called as `fn(prediction, expected)`, where `prediction` is
  the output (a named list) and `expected` the whole data row, as a
  one-row data frame. It returns one logical or numeric score, or
  `list(score = , feedback = )`.

- name:

  A name used in error messages and warnings.

## Value

A metric function with the same return values as `fn`, except that
numeric scores outside 0 to 1 are clipped to that range with a warning,
and errors are re-raised with the metric's name.

## See also

Other metrics:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
[`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
[`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
# Partial credit for answers that are close in length
length_ratio <- metric_custom(
  function(prediction, expected) {
    nchar(prediction$answer) / nchar(expected$answer)
  },
  name = "length_ratio"
)
row <- data.frame(question = "Capital of France?", answer = "Paris")
length_ratio(list(answer = "Pa"), row)
#> [1] 0.4
length_ratio(list(answer = "Paris, France"), row)
#> Warning: Metric `length_ratio()` returned value outside [0, 1]
#> ℹ Value: 2.6
#> [1] 1
```
