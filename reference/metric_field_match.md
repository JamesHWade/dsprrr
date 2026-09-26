# Metric that compares several output fields

`metric_field_match()` makes a metric that compares several fields of
the prediction with the columns of the same names in the expected row.
Values must be exactly equal (numbers may differ in storage type only).

## Usage

``` r
metric_field_match(fields, require_all = TRUE)
```

## Arguments

- fields:

  Names of the fields to compare.

- require_all:

  If `TRUE` (the default), every field must match; if `FALSE`, one match
  is enough.

## Value

A function `function(prediction, expected)` returning `TRUE` or `FALSE`.
A field missing from either side is an error.

## See also

Other metrics:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
[`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md),
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
[`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
both <- metric_field_match(c("city", "country"))
row <- data.frame(question = "Where is the Louvre?", city = "Paris", country = "France")
both(list(city = "Paris", country = "France"), row)
#> [1] TRUE
both(list(city = "Paris", country = "Belgium"), row)
#> [1] FALSE

either <- metric_field_match(c("city", "country"), require_all = FALSE)
either(list(city = "Paris", country = "Belgium"), row)
#> [1] TRUE
```
