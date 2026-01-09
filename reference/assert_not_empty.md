# Assert Output is Not Empty

Create an assertion that validates the output is not empty or
whitespace-only.

## Usage

``` r
assert_not_empty(field = NULL, type = c("assert", "suggest"))
```

## Arguments

- field:

  The output field to check. If NULL, checks the entire output.

- type:

  "assert" for hard assertion (default), "suggest" for soft suggestion.

## Value

An Assertion object

## Examples

``` r
if (FALSE) { # \dontrun{
# Answer must not be empty
assert_not_empty("answer")

# All fields must have content
assert_not_empty("summary")
assert_not_empty("conclusion")
} # }
```
