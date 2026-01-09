# Assert Custom Condition

Create a custom assertion with a user-defined condition function. This
is a convenience wrapper around
[`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)
with clearer semantics.

## Usage

``` r
assert_custom(condition, message, field = NULL, type = c("assert", "suggest"))
```

## Arguments

- condition:

  A function or formula that takes the output and returns TRUE/FALSE.

- message:

  Error message when assertion fails.

- field:

  Optional. The specific output field to validate.

- type:

  "assert" for hard assertion (default), "suggest" for soft suggestion.

## Value

An Assertion object

## Examples

``` r
if (FALSE) { # \dontrun{
# Custom validation: answer must have exactly 3 sentences
assert_custom(
  ~ length(gregexpr("\\.", .x$answer)[[1]]) == 3,
  "Answer must have exactly 3 sentences"
)

# Custom validation: summary must be shorter than original text
assert_custom(
  function(x) nchar(x$summary) < nchar(x$original),
  "Summary must be shorter than original"
)
} # }
```
