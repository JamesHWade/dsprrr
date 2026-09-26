# Assert a custom condition

`assert_custom()` builds an assertion from your own condition. It is
[`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)
or
[`suggest_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md),
chosen by `type`.

## Usage

``` r
assert_custom(condition, message, field = NULL, type = c("assert", "suggest"))
```

## Arguments

- condition:

  A function, or a formula using `.x`, that takes the output (or the
  `field`) and returns `TRUE` or `FALSE`.

- message:

  The message shown when the check fails, which
  [`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)
  also sends back to the model on a retry.

- field:

  The output field passed to `condition`. With `NULL` (the default), the
  whole output (a named list) is passed.

- type:

  `"assert"` (the default) for a hard assertion, which makes
  [`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)
  retry when it fails, or `"suggest"` for a soft one, which only gives a
  warning.

## Value

An assertion for
[`assertion_set()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)
or
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md).

## See also

Other assertions:
[`assert_contains()`](https://jameshwade.github.io/dsprrr/reference/assert_contains.md),
[`assert_length()`](https://jameshwade.github.io/dsprrr/reference/assert_length.md),
[`assert_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_matches.md),
[`assert_not_contains()`](https://jameshwade.github.io/dsprrr/reference/assert_not_contains.md),
[`assert_not_empty()`](https://jameshwade.github.io/dsprrr/reference/assert_not_empty.md),
[`assert_not_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_not_matches.md),
[`assert_one_of()`](https://jameshwade.github.io/dsprrr/reference/assert_one_of.md),
[`assert_range()`](https://jameshwade.github.io/dsprrr/reference/assert_range.md),
[`assertions`](https://jameshwade.github.io/dsprrr/reference/assertions.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
# Exactly three sentences
assert_custom(
  ~ lengths(regmatches(.x$answer, gregexpr("[.!?]", .x$answer))) == 3,
  "Answer in exactly three sentences"
)
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "Answer in exactly three sentences"

# Compare two output fields
assert_custom(
  function(x) nchar(x$summary) < nchar(x$details),
  "The summary must be shorter than the details"
)
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "The summary must be shorter than the details"
```
