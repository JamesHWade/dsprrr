# Create a MultiChainComparison Module

Factory function to create a MultiChainComparison module that generates
M reasoning chains and synthesizes the best answer.

## Usage

``` r
multi_chain_comparison(
  signature,
  inner_module = NULL,
  M = 3L,
  temperature = 0.7,
  comparison_template = NULL,
  ...
)
```

## Arguments

- signature:

  Signature for the task, either string notation or Signature object

- inner_module:

  Optional pre-created inner module. If NULL, creates a ChainOfThought
  module from the signature.

- M:

  Number of reasoning chains to generate (default 3)

- temperature:

  Temperature for attempt diversity (default 0.7)

- comparison_template:

  Optional custom template for comparison prompt

- ...:

  Additional arguments passed to module constructor

## Value

A MultiChainComparisonModule object

## Examples

``` r
# Basic usage
mcc <- multi_chain_comparison("question -> answer", M = 3)

# With custom inner module
cot <- chain_of_thought("question -> answer")
mcc <- multi_chain_comparison(
  "question -> answer",
  inner_module = cot,
  M = 5,
  temperature = 0.8
)
```
