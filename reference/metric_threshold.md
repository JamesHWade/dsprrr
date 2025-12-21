# Create a Threshold Metric

Wraps a numeric metric to return TRUE/FALSE based on a threshold.

## Usage

``` r
metric_threshold(metric, threshold = 0.5, comparison = ">=")
```

## Arguments

- metric:

  A metric function that returns numeric values

- threshold:

  The threshold value for success

- comparison:

  One of "\>=", "\>", "==", "\<", "\<="

## Value

A function with signature function(prediction, expected) -\> logical

## Examples

``` r
# F1 score with threshold
metric <- metric_threshold(metric_f1(), threshold = 0.8)
metric("the quick brown fox", "the fast brown fox")  # FALSE (0.75 < 0.8)
#> [1] FALSE
```
