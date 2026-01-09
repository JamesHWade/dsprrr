# Assert Numeric Value in Range

Create an assertion that validates a numeric output value is within a
range.

## Usage

``` r
assert_range(
  field = NULL,
  min = NULL,
  max = NULL,
  type = c("assert", "suggest")
)
```

## Arguments

- field:

  The output field to check. If NULL, checks the entire output.

- min:

  Minimum value (inclusive). Default NULL (no minimum).

- max:

  Maximum value (inclusive). Default NULL (no maximum).

- type:

  "assert" for hard assertion (default), "suggest" for soft suggestion.

## Value

An Assertion object

## Examples

``` r
if (FALSE) { # \dontrun{
# Score must be between 0 and 100
assert_range("score", min = 0, max = 100)

# Confidence must be positive
assert_range("confidence", min = 0)
} # }
```
