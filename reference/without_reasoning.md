# Remove the reasoning field from a signature

`without_reasoning()` drops the reasoning field that
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md)
added, for example to compare a module with and without
chain-of-thought. The instructions are kept as they are.

## Usage

``` r
without_reasoning(sig, reasoning_field = "reasoning")
```

## Arguments

- sig:

  A signature from
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  usually one returned by
  [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md).

- reasoning_field:

  Name of the field to remove.

## Value

A new signature without the field, or `sig` unchanged if it has no such
field. If no output field would remain, the output becomes a single
string field named `answer`.

## See also

Other signatures:
[`has_reasoning()`](https://jameshwade.github.io/dsprrr/reference/has_reasoning.md),
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md),
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
[`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md),
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md)

## Examples

``` r
cot <- with_reasoning("question -> answer")
without_reasoning(cot)
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • question: "string" - Input: question
#> 
#> ── Output 
#> Type: "object(answer: string)"
#> 
#> ── Instructions 
#> Given the fields `question`, produce the fields `answer`. Think through your
#> reasoning step by step before providing the answer.
```
