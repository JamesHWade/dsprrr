# Create a Threshold Metric

Wraps a metric to return TRUE/FALSE based on a threshold. Logical,
numeric, feedback, and trace-aware metrics all use the package-wide
metric protocol; feedback and trace dispatch are preserved by the
wrapper.

## Usage

``` r
metric_threshold(metric, threshold = 0.5, comparison = ">=")
```

## Arguments

- metric:

  A metric function returning a logical/numeric score or
  `list(score = , feedback = )`.

- threshold:

  The threshold value for success

- comparison:

  One of "\>=", "\>", "==", "\<", "\<="

## Value

A metric function returning logical scores. Trace-aware and feedback
protocols are preserved when present on `metric`.

## Examples

``` r
# F1 score with threshold
metric <- metric_threshold(metric_f1(), threshold = 0.8)
metric("the quick brown fox", "the fast brown fox")  # FALSE (0.75 < 0.8)
#> [1] FALSE
```
