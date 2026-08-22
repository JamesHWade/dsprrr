# Signature Transforms for Advanced Reasoning Modules

Functions for transforming signatures to enable different reasoning
patterns. These are composable transforms that modify the output type of
a signature to include additional fields like chain-of-thought
reasoning.

`with_instructions()` replaces a signature's instructions while
preserving its input and output fields. `append_instructions()` layers
additional instructions after the existing text, separated by a blank
line. Both functions return a new signature object and never mutate the
original.

## Usage

``` r
with_instructions(x, instructions)

append_instructions(x, instructions)
```

## Arguments

- x:

  A signature object created by
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or string notation such as `"question -> answer"`.

- instructions:

  A single, non-missing character string. Empty text is allowed when the
  resulting signature remains valid; for `append_instructions()` it is a
  no-op.

## Value

A new signature object.

## Examples

``` r
base <- signature(
  "text -> summary",
  instructions = "Summarize the text."
)

concise <- append_instructions(base, "Use at most 30 words.")
base@instructions
#> [1] "Summarize the text."
concise@instructions
#> [1] "Summarize the text.\n\nUse at most 30 words."

replaced <- with_instructions(base, "Return one sentence.")
```
