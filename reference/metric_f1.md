# Create an F1 Score Metric

Creates a metric function that calculates the F1 score between predicted
and expected text based on token overlap.

## Usage

``` r
metric_f1(field = NULL, normalize = TRUE)
```

## Arguments

- field:

  Optional field name to extract from structured outputs

- normalize:

  Logical, whether to normalize text before tokenization

## Value

A function with signature function(prediction, expected) -\> numeric

## Examples

``` r
# Token-based F1 score
metric <- metric_f1()
metric("the quick brown fox", "the fast brown fox")  # 0.75
#> [1] 0.75

# Field extraction
metric <- metric_f1(field = "answer")
metric(
  list(answer = "the quick brown fox"),
  list(answer = "the fast brown fox")
)
#> [1] 0.75
```
