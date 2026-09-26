# Test whether a signature has a reasoning field

`has_reasoning()` checks whether a signature's output has a field named
`reasoning_field`, as added by
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md).

## Usage

``` r
has_reasoning(sig, reasoning_field = "reasoning")
```

## Arguments

- sig:

  A signature from
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md).

- reasoning_field:

  Name of the field to look for.

## Value

`TRUE` or `FALSE`. Anything other than a signature gives `FALSE`.

## See also

Other signatures:
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md),
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
[`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md),
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md),
[`without_reasoning()`](https://jameshwade.github.io/dsprrr/reference/without_reasoning.md)

## Examples

``` r
sig <- signature("question -> answer")
has_reasoning(sig)
#> [1] FALSE
has_reasoning(with_reasoning(sig))
#> [1] TRUE
```
