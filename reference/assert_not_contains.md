# Assert that an output does not contain a string

`assert_not_contains()` checks that an output field does not contain
`pattern`, matched as a fixed string. A missing field passes.

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

  The output field to check. With `NULL`, the whole output is checked,
  which suits outputs with a single field. A missing field fails the
  check.

- pattern:

  The string that must not appear.

- ignore_case:

  If `TRUE`, ignore case when matching.

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
[`assert_not_empty()`](https://jameshwade.github.io/dsprrr/reference/assert_not_empty.md),
[`assert_not_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_not_matches.md),
[`assert_one_of()`](https://jameshwade.github.io/dsprrr/reference/assert_one_of.md),
[`assert_range()`](https://jameshwade.github.io/dsprrr/reference/assert_range.md),
[`assertions`](https://jameshwade.github.io/dsprrr/reference/assertions.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
assert_not_contains("answer", "As an AI")
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "answer must not contain 'As an AI'"
assert_not_contains("reply", "password", ignore_case = TRUE)
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "reply must not contain 'password'"
```
