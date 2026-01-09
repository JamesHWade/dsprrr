# Assert Output Matches Pattern

Create an assertion that validates the output matches a regular
expression.

## Usage

``` r
assert_matches(
  field = NULL,
  pattern,
  message = NULL,
  ignore_case = FALSE,
  type = c("assert", "suggest")
)
```

## Arguments

- field:

  The output field to check. If NULL, checks the entire output.

- pattern:

  The regular expression pattern to match.

- message:

  Optional custom error message. If NULL, generates a default.

- ignore_case:

  Logical. If TRUE, matching is case-insensitive. Default FALSE.

- type:

  "assert" for hard assertion (default), "suggest" for soft suggestion.

## Value

An Assertion object

## Examples

``` r
if (FALSE) { # \dontrun{
# Must start with capital letter
assert_matches("answer", "^[A-Z]", "Must start with capital letter")

# Must be a valid email format (simple check)
assert_matches("email", "^[^@]+@[^@]+\\.[^@]+$", "Must be valid email")

# Must end with period
assert_matches("summary", "\\.$", "Must end with period")
} # }
```
