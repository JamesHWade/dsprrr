# Assertions for Output Validation

Use `assert_output()` and `suggest_output()` to define output validation
constraints, then combine them with `assertion_set()`. Hard assertions
trigger retries when they fail, while soft suggestions log warnings and
allow execution to continue.

Define validation constraints for module outputs. Hard assertions
(`assert_output`) trigger retries when they fail, while soft suggestions
(`suggest_output`) log warnings but allow execution to continue.

## Usage

``` r
assert_output(condition, message = "Assertion failed", field = NULL)

suggest_output(condition, message = "Suggestion not met", field = NULL)

assertion_set(...)
```

## Arguments

- condition:

  A formula or function that takes the output and returns TRUE/FALSE.
  For formulas, use `.x` to reference the output (e.g.,
  `~ nchar(.x$answer) <= 100`).

- message:

  Error message to display when the assertion fails.

- field:

  Optional. The specific output field to validate. If NULL, the entire
  output is passed to the condition.

- ...:

  Output assertions created by `assert_output()` or `suggest_output()`,
  or one list of such assertions.

## Value

An output assertion for `assertion_set()` or
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md).

An assertion set for
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md).

## Details

### Backtracking Behavior

When wrapped with
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md),
modules will:

1.  Run the module normally

2.  Evaluate all assertions against the output

3.  If hard assertions fail and retries remain, inject feedback and
    retry

4.  If max retries exceeded, raise an error (or warning if configured)

5.  Soft suggestions always log but never trigger retries

### Condition Functions

Conditions can be specified as:

- **Formulas**: `~ nchar(.x$answer) <= 100` - `.x` is the output

- **Functions**: `function(x) nchar(x$answer) <= 100`

## Examples

``` r
if (FALSE) { # \dontrun{
# Hard assertion - must be satisfied
assert_output(~ nchar(.x$answer) <= 100, "Answer must be 100 chars or less")

# Soft suggestion - logs warning but continues
suggest_output(~ grepl("^[A-Z]", .x$answer), "Should start with capital")

# Field-specific assertion
assert_output(~ nchar(.x) <= 50, "Too long", field = "summary")
} # }

if (FALSE) { # \dontrun{
assertions <- assertion_set(
  assert_output(~ nchar(.x$answer) <= 100, "Too long"),
  suggest_output(~ grepl("^[A-Z]", .x$answer), "Should capitalize")
)
} # }
```
