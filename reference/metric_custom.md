# Create a Custom Metric

Wrapper for creating custom metric functions with consistent interface
and error handling.

## Usage

``` r
metric_custom(fn, name = NULL)
```

## Arguments

- fn:

  A function with signature function(prediction, expected) -\>
  numeric/logical

- name:

  Optional name for the metric (for debugging)

## Value

A metric function with enhanced error handling

## Examples

``` r
# Custom length comparison
length_metric <- metric_custom(function(pred, exp) {
  nchar(as.character(pred)) == nchar(as.character(exp))
}, name = "length_match")

# Custom scoring function
score_metric <- metric_custom(function(pred, exp) {
  # Return value between 0 and 1
  min(nchar(pred) / nchar(exp), 1)
})
```
