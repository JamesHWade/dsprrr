# Create a Field Equality Metric

Creates a metric that checks equality of multiple fields in structured
outputs.

## Usage

``` r
metric_field_match(fields, require_all = TRUE)
```

## Arguments

- fields:

  Character vector of field names to compare

- require_all:

  Logical, whether all fields must match (AND) or any (OR)

## Value

A function with signature function(prediction, expected) -\> logical

## Examples

``` r
# Check multiple fields
metric <- metric_field_match(c("sentiment", "confidence"))
metric(
  list(sentiment = "positive", confidence = 0.9),
  list(sentiment = "positive", confidence = 0.9)
)  # TRUE
#> [1] TRUE
```
