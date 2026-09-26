# Metric that checks for a pattern in the output

`metric_contains()` makes a metric that is `TRUE` when the prediction
contains `pattern`. The pattern is fixed when you create the metric: the
metric ignores its `expected` argument, so every row is checked for the
same pattern. To compare against a column of the data, use
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md)
or a custom metric.

## Usage

``` r
metric_contains(pattern, field = NULL, ignore_case = FALSE, fixed = TRUE)
```

## Arguments

- pattern:

  The text to look for; a regular expression when `fixed = FALSE`.

- field:

  The output field to search. Give it for outputs with more than one
  field; with `NULL`, the whole prediction is searched.

- ignore_case:

  If `TRUE`, ignore case.

- fixed:

  If `TRUE` (the default), match `pattern` as plain text.

## Value

A function `function(prediction, expected = NULL)` returning `TRUE` or
`FALSE`. `field` is stored in its `"field"` attribute.

## See also

Other metrics:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md),
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
[`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
[`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
cites_source <- metric_contains("[source]", field = "answer")
cites_source(list(answer = "Paris [source]"))
#> [1] TRUE
cites_source(list(answer = "Paris"))
#> [1] FALSE

# The expected value is ignored
cites_source(list(answer = "Paris [source]"), data.frame(answer = "Lyon"))
#> [1] TRUE

# A regular expression
has_number <- metric_contains("[0-9]+", field = "answer", fixed = FALSE)
has_number(list(answer = "The answer is 42"))
#> [1] TRUE
```
