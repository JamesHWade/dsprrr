# Create an Exact Match Metric

Creates a metric function that checks for exact string match between
predicted and expected values. Can optionally extract a specific field
from structured outputs.

## Usage

``` r
metric_exact_match(field = NULL, ignore_case = FALSE, normalize = TRUE)
```

## Arguments

- field:

  Optional field name to extract from structured outputs

- ignore_case:

  Logical, whether to ignore case when comparing

- normalize:

  Logical, whether to normalize whitespace

## Value

A function with signature function(prediction, expected) -\> logical

## Examples

``` r
# Simple exact match
metric <- metric_exact_match()
metric("hello", "hello")  # TRUE
#> [1] TRUE
metric("hello", "world")  # FALSE
#> [1] FALSE

# Field extraction for structured outputs
metric <- metric_exact_match(field = "sentiment")
metric(list(sentiment = "positive"), list(sentiment = "positive"))  # TRUE
#> [1] TRUE

# Case insensitive matching
metric <- metric_exact_match(ignore_case = TRUE)
metric("Hello", "hello")  # TRUE
#> [1] TRUE
```
