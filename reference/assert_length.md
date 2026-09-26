# Assert the length of an output

`assert_length()` checks that the number of characters in an output
field is within `min` and `max`. Like the other `assert_*()` helpers, it
is shorthand for
[`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md)
(or
[`suggest_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md))
with a ready-made condition.

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

  The output field to check. With `NULL`, the whole output is checked,
  which suits outputs with a single field. A missing field fails the
  check.

- min, max:

  Inclusive bounds on the number of characters. Give at least one.

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
assert_length("answer", max = 100)
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "answer: Length must be at most 100 characters"
assert_length("summary", min = 10, max = 200, type = "suggest")
#> 
#> ── Soft Suggestion 
#> • Field: any field
#> • Message: "summary: Length must be between 10 and 200 characters"
```
