# Create a Program of Thought Module

Factory function to create a ProgramOfThoughtModule that generates and
executes R code to solve problems.

## Usage

``` r
program_of_thought(
  signature,
  runner,
  max_iters = 3L,
  extract_answer = TRUE,
  ...
)
```

## Arguments

- signature:

  A Signature object or string notation defining inputs/outputs

- runner:

  An RCodeRunner object for code execution. Required.

- max_iters:

  Maximum code generation/repair iterations (default 3)

- extract_answer:

  Logical. If TRUE (default), use LLM to extract final answer from
  execution result. If FALSE, return execution result directly.

- ...:

  Additional arguments passed to the module

## Value

A ProgramOfThoughtModule object

## Examples

``` r
if (FALSE) { # \dontrun{
runner <- r_code_runner(timeout = 30)
pot <- program_of_thought("question -> answer", runner = runner)
result <- run(pot, question = "Calculate 847 * 293", .llm = llm)
} # }
```
