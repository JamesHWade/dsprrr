# Create a Contains Metric

Creates a metric function that checks if the prediction contains a
specific substring or pattern.

## Usage

``` r
metric_contains(pattern, field = NULL, ignore_case = FALSE, fixed = TRUE)
```

## Arguments

- pattern:

  The pattern to search for (can be a regex)

- field:

  Optional field name to extract from structured outputs

- ignore_case:

  Logical, whether to ignore case

- fixed:

  Logical, whether pattern is a fixed string (not regex)

## Value

A function with signature function(prediction, expected) -\> logical

## Examples

``` r
# Check for substring
metric <- metric_contains("positive", ignore_case = TRUE)
metric("The result is POSITIVE", NULL)  # TRUE
#> [1] TRUE

# Regex pattern
metric <- metric_contains("\\d+", fixed = FALSE)
metric("The answer is 42", NULL)  # TRUE
#> [1] TRUE
```
