# Create a Chain-of-Thought Module

Convenience function that creates a PredictModule with chain-of-thought
reasoning enabled. This is equivalent to calling
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md)
on a signature and then creating a module from it.

## Usage

``` r
chain_of_thought(x, prefix = "Let's think step by step in order to", ...)
```

## Arguments

- x:

  A Signature object or string notation

- prefix:

  Character. The prefix for the reasoning field.

- ...:

  Additional arguments passed to
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)

## Value

A PredictModule with reasoning enabled

## Examples

``` r
# Create a chain-of-thought QA module
mod <- chain_of_thought("question -> answer")

# Use it like any other module
# result <- run(mod, question = "What is 15 * 24?", .llm = llm)
# result$reasoning contains step-by-step reasoning
# result$answer contains the final answer
```
