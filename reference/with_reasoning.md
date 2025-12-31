# Add Chain-of-Thought Reasoning to a Signature

Transforms a signature to include a reasoning field before the original
output. This implements the Chain-of-Thought prompting pattern where the
model is asked to "show its work" before providing the final answer.

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

  A Signature object or string notation (e.g., "question -\> answer")

- prefix:

  Character. The prefix for the reasoning field description. Default
  uses DSPy-style "Let's think step by step" prompt.

- reasoning_field:

  Character. Name of the reasoning field to add. Default is "reasoning".

- instructions:

  Character. Optional new instructions for the signature. If NULL
  (default), original instructions are preserved with reasoning context.

- ...:

  Additional arguments (unused)

## Value

A new Signature object with reasoning field added to output_type

## Details

The transform works by:

1.  Extracting existing output fields from the signature's output_type

2.  Creating a new output_type with reasoning as the first field

3.  Adding appropriate description to guide the model

The reasoning field is always placed first to encourage the model to
reason before answering (per Chain-of-Thought research).

## Examples

``` r
# Basic usage with string notation
sig <- with_reasoning("question -> answer")

# Custom prefix
sig <- with_reasoning(
  "math_problem -> solution",
  prefix = "Let me solve this step by step:"
)

# With explicit signature
sig <- signature("context, question -> answer")
cot_sig <- with_reasoning(sig)
```
