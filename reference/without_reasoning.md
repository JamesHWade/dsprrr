# Remove Chain-of-Thought from a Signature

Reverses the
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md)
transform by removing the reasoning field from the output type. Useful
for comparing reasoning vs non-reasoning module performance.

## Usage

``` r
without_reasoning(sig, reasoning_field = "reasoning")
```

## Arguments

- sig:

  A Signature object (typically one created with
  [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md))

- reasoning_field:

  Character. Name of reasoning field to remove.

## Value

A new Signature without the reasoning field

## Examples

``` r
cot_sig <- with_reasoning("question -> answer")
plain_sig <- without_reasoning(cot_sig)
```
