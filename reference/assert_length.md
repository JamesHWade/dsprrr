# Assert Output Length

Create an assertion that validates the character length of a field.

## Usage

``` r
assert_length(
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

  Minimum length (inclusive). Default NULL (no minimum).

- max:

  Maximum length (inclusive). Default NULL (no maximum).

- type:

  "assert" for hard assertion (default), "suggest" for soft suggestion.

## Value

An Assertion object

## Examples

``` r
if (FALSE) { # \dontrun{
# Max 100 characters
assert_length("answer", max = 100)

# Between 10 and 200 characters
assert_length("summary", min = 10, max = 200)

# Soft suggestion for length
assert_length("answer", max = 50, type = "suggest")
} # }
```
