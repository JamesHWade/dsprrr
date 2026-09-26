# Assert that an output is one of a set of values

`assert_one_of()` checks that an output field equals one of `values`.

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

  The output field to check. With `NULL`, the whole output is checked,
  which suits outputs with a single field. A missing field fails the
  check.

- values:

  A character vector of allowed values.

- ignore_case:

  If `TRUE`, ignore case when comparing.

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
[`assert_custom()`](https://jameshwade.github.io/dsprrr/reference/assert_custom.md),
[`assert_length()`](https://jameshwade.github.io/dsprrr/reference/assert_length.md),
[`assert_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_matches.md),
[`assert_not_contains()`](https://jameshwade.github.io/dsprrr/reference/assert_not_contains.md),
[`assert_not_empty()`](https://jameshwade.github.io/dsprrr/reference/assert_not_empty.md),
[`assert_not_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_not_matches.md),
[`assert_range()`](https://jameshwade.github.io/dsprrr/reference/assert_range.md),
[`assertions`](https://jameshwade.github.io/dsprrr/reference/assertions.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
assert_one_of("sentiment", c("positive", "negative", "neutral"))
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "sentiment must be one of: positive, negative, neutral"
assert_one_of("grade", c("A", "B", "C"), ignore_case = TRUE)
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "grade must be one of: A, B, C"
```
