# Assert Output Contains Substring

Create an assertion that validates the output contains a specific
substring.

## Usage

``` r
assert_contains(
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

  The substring that must be present.

- ignore_case:

  Logical. If TRUE, comparison is case-insensitive. Default FALSE.

- type:

  "assert" for hard assertion (default), "suggest" for soft suggestion.

## Value

An Assertion object

## Examples

``` r
if (FALSE) { # \dontrun{
# Must contain "important"
assert_contains("answer", "important")

# Case-insensitive check
assert_contains("summary", "conclusion", ignore_case = TRUE)
} # }
```
