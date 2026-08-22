# Check if a Signature has Chain-of-Thought

Tests whether a signature has been transformed with
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md).
Checks for the presence of a reasoning field in the output type.

## Usage

``` r
has_reasoning(sig, reasoning_field = "reasoning")
```

## Arguments

- sig:

  A signature object created by
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md).

- reasoning_field:

  Character. Name of reasoning field to check for.

## Value

Logical. TRUE if signature has chain-of-thought reasoning.

## Examples

``` r
sig <- signature("question -> answer")
has_reasoning(sig)
#> [1] FALSE
# FALSE

cot_sig <- with_reasoning(sig)
has_reasoning(cot_sig)
#> [1] TRUE
# TRUE
```
