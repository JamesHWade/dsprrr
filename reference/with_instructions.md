# Replace or extend signature instructions

`with_instructions()` replaces a signature's instructions.
`append_instructions()` adds text after the existing instructions,
separated by a blank line. Both keep the input and output fields and
return a new signature; the original is unchanged.

## Usage

``` r
with_instructions(x, instructions)

append_instructions(x, instructions)
```

## Arguments

- x:

  A signature from
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or a signature string such as `"question -> answer"`.

- instructions:

  One string. Empty text is allowed when the signature stays valid;
  `append_instructions()` then returns the instructions unchanged.

## Value

A new signature.

## See also

Other signatures:
[`has_reasoning()`](https://jameshwade.github.io/dsprrr/reference/has_reasoning.md),
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md),
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md),
[`without_reasoning()`](https://jameshwade.github.io/dsprrr/reference/without_reasoning.md)

## Examples

``` r
base <- signature(
  "text -> summary",
  instructions = "Summarize the text."
)

concise <- append_instructions(base, "Use at most 30 words.")
concise@instructions
#> [1] "Summarize the text.\n\nUse at most 30 words."

with_instructions(base, "Return one sentence.")@instructions
#> [1] "Return one sentence."
base@instructions
#> [1] "Summarize the text."
```
