# Add a reasoning field to a signature

`with_reasoning()` adds a string output field, `reasoning` by default,
before the existing output fields, so the model writes out its reasoning
before it answers (chain-of-thought prompting).
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
builds a module from the result.

## Usage

``` r
with_reasoning(
  x,
  prefix = "Let's think step by step in order to",
  reasoning_field = "reasoning",
  instructions = NULL,
  ...
)
```

## Arguments

- x:

  A signature from
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or a signature string such as `"question -> answer"`.

- prefix:

  Start of the reasoning field's description. The description reads
  "Reasoning: `prefix` produce the `fields`.", where `fields` names the
  original output fields.

- reasoning_field:

  Name of the added field.

- instructions:

  New instructions. With `NULL` (the default), existing instructions get
  " Think through your reasoning step by step before providing the
  answer." appended, and empty instructions are replaced by "Given
  `inputs`, think step by step and produce `outputs`."

- ...:

  Ignored.

## Value

A new signature whose output is an object with the reasoning field
first, followed by the original output fields. A bare output type
becomes a field named `answer`.

## See also

Other signatures:
[`has_reasoning()`](https://jameshwade.github.io/dsprrr/reference/has_reasoning.md),
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md),
[`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
[`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md),
[`without_reasoning()`](https://jameshwade.github.io/dsprrr/reference/without_reasoning.md)

## Examples

``` r
with_reasoning("question -> answer")
#> 
#> ── Signature ──
#> 
#> ── Inputs 
#> • question: "string" - Input: question
#> 
#> ── Output 
#> Type: "object(reasoning: string, answer: string)"
#> 
#> ── Instructions 
#> Given the fields `question`, produce the fields `answer`. Think through your
#> reasoning step by step before providing the answer.

# The prefix goes into the description of the reasoning field
sig <- with_reasoning(
  "math_problem -> solution: float",
  prefix = "Let me solve this step by step, then"
)
sig@output_type@properties$reasoning@description
#> [1] "Reasoning: Let me solve this step by step, then produce the solution."
```
