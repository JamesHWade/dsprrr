# Assert Output Does Not Contain Substring

Create an assertion that validates the output does not contain a
specific substring.

## Usage

``` r
assert_not_contains(
  field = NULL,
  pattern,
  ignore_case = FALSE,
  type = c("assert", "suggest")
)
```

## Arguments

- field:

  The output field to check. If NULL, checks the entire output.

- pattern:

  The substring that must NOT be present.

- ignore_case:

  Logical. If TRUE, comparison is case-insensitive. Default FALSE.

- type:

  "assert" for hard assertion (default), "suggest" for soft suggestion.

## Value

An Assertion object

## Examples

``` r
if (FALSE) { # \dontrun{
# Must not contain profanity (simple example)
assert_not_contains("answer", "badword")

# Must not reveal internal details
assert_not_contains("response", "internal_api_key")
} # }
```
