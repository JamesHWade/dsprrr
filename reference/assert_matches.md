# Assert that an output matches a regular expression

`assert_matches()` checks that an output field matches a Perl-compatible
regular expression.

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

  The output field to check. With `NULL`, the whole output is checked,
  which suits outputs with a single field. A missing field fails the
  check.

- pattern:

  A regular expression (Perl syntax).

- message:

  The message shown when the check fails, which
  [`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)
  also sends back to the model on a retry. With `NULL`, a message naming
  the field and pattern.

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
[`assert_not_contains()`](https://jameshwade.github.io/dsprrr/reference/assert_not_contains.md),
[`assert_not_empty()`](https://jameshwade.github.io/dsprrr/reference/assert_not_empty.md),
[`assert_not_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_not_matches.md),
[`assert_one_of()`](https://jameshwade.github.io/dsprrr/reference/assert_one_of.md),
[`assert_range()`](https://jameshwade.github.io/dsprrr/reference/assert_range.md),
[`assertions`](https://jameshwade.github.io/dsprrr/reference/assertions.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
assert_matches("answer", "^[A-Z]", "Start with a capital letter")
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "Start with a capital letter"
assert_matches("email", "^[^@]+@[^@]+\\.[^@]+$", "Return a valid email address")
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "Return a valid email address"
```
