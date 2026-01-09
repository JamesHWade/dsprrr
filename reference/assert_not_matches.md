# Assert Output Does Not Match Pattern

Create an assertion that validates the output does NOT match a regular
expression.

## Usage

``` r
assert_not_matches(
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

  The regular expression pattern that must NOT match.

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
# Must not contain URLs
assert_not_matches("answer", "https?://", "Must not contain URLs")

# Must not contain code blocks
assert_not_matches("summary", "```", "Must not contain code blocks")
} # }
```
