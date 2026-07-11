# Create a CodeAct Module

Factory function to create a CodeActModule that can use both tools and R
code execution to solve problems.

## Usage

``` r
code_act(signature, tools = list(), runner, max_iterations = 10L, ...)
```

## Arguments

- signature:

  A Signature object or string notation defining inputs/outputs

- tools:

  List of ellmer ToolDef objects for the agent to use

- runner:

  A code runner implementing `execute()` and `policy()`. Required.

- max_iterations:

  Maximum tool/code iterations before forcing answer (default 10)

- ...:

  Additional arguments passed to the module

## Value

A CodeActModule object

## Examples

``` r
if (FALSE) { # \dontrun{
runner <- r_code_runner(timeout = 30)
agent <- code_act(
  "question -> answer",
  tools = list(),
  runner = runner
)
result <- run(agent, question = "Calculate 2^10", .llm = llm)
} # }
```
