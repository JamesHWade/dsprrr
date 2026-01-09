# Assert Output is One Of

Create an assertion that validates the output is one of a set of allowed
values.

## Usage

``` r
assert_one_of(
  field = NULL,
  values,
  ignore_case = FALSE,
  type = c("assert", "suggest")
)
```

## Arguments

- field:

  The output field to check. If NULL, checks the entire output.

- values:

  Character vector of allowed values.

- ignore_case:

  Logical. If TRUE, comparison is case-insensitive. Default FALSE.

- type:

  "assert" for hard assertion (default), "suggest" for soft suggestion.

## Value

An Assertion object

## Examples

``` r
if (FALSE) { # \dontrun{
# Must be a valid sentiment
assert_one_of("sentiment", c("positive", "negative", "neutral"))

# Case-insensitive check
assert_one_of("category", c("A", "B", "C"), ignore_case = TRUE)
} # }
```
