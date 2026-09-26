# Define output assertions

Assertions are checks on a module's output. `assert_output()` makes a
hard assertion: when it fails,
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)
retries the module with feedback. `suggest_output()` makes a soft one: a
failure only gives a warning. `assertion_set()` groups assertions. The
`assert_*()` helpers, such as
[`assert_length()`](https://jameshwade.github.io/dsprrr/reference/assert_length.md),
build common conditions for you.

## Usage

``` r
assert_output(condition, message = "Assertion failed", field = NULL)

suggest_output(condition, message = "Suggestion not met", field = NULL)

assertion_set(...)
```

## Arguments

- condition:

  A function, or a formula using `.x` such as
  `~ nchar(.x$answer) <= 100`, that takes the output (or the `field`)
  and returns `TRUE` or `FALSE`.

- message:

  The message shown when the check fails.
  [`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)
  also sends it back to the model on a retry, so phrase it as an
  instruction.

- field:

  The output field passed to `condition`. With `NULL` (the default), the
  whole output is passed.

- ...:

  For `assertion_set()`: assertions made with `assert_output()`,
  `suggest_output()` or the `assert_*()` helpers, or one list of them.
  An empty set gives a warning.

## Value

`assert_output()` and `suggest_output()` return an assertion, and
`assertion_set()` a set of assertions, for
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md).

## Details

A condition is a function, or a formula using `.x`, that receives the
output (a named list) or, with `field`, that field's value, and returns
`TRUE` or `FALSE`. A condition that errors, returns `NA` or returns
anything other than a single logical value counts as failed and gives a
warning; so does a `field` that is missing from the output.

## See also

Other assertions:
[`assert_contains()`](https://jameshwade.github.io/dsprrr/reference/assert_contains.md),
[`assert_custom()`](https://jameshwade.github.io/dsprrr/reference/assert_custom.md),
[`assert_length()`](https://jameshwade.github.io/dsprrr/reference/assert_length.md),
[`assert_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_matches.md),
[`assert_not_contains()`](https://jameshwade.github.io/dsprrr/reference/assert_not_contains.md),
[`assert_not_empty()`](https://jameshwade.github.io/dsprrr/reference/assert_not_empty.md),
[`assert_not_matches()`](https://jameshwade.github.io/dsprrr/reference/assert_not_matches.md),
[`assert_one_of()`](https://jameshwade.github.io/dsprrr/reference/assert_one_of.md),
[`assert_range()`](https://jameshwade.github.io/dsprrr/reference/assert_range.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
short <- assert_output(~ nchar(.x$answer) <= 100, "Keep the answer under 100 characters")
short
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "Keep the answer under 100 characters"

capitalized <- suggest_output(
  ~ grepl("^[A-Z]", .x),
  "Start with a capital letter",
  field = "answer"
)

assertion_set(short, capitalized)
#> 
#> ── AssertionSet ──
#> 
#> • 1 hard assertion
#> • 1 soft suggestion
#> 
#> ── Details 
#> 
#> ── Hard Assertion 
#> • Field: any field
#> • Message: "Keep the answer under 100 characters"
#> 
#> ── Soft Suggestion 
#> • Field: answer
#> • Message: "Start with a capital letter"
```
