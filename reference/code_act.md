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

  List of ellmer ToolDef objects for the agent to use. Non-empty list
  element names become the registered tool names; unnamed elements keep
  their ToolDef name. Effective names may contain only letters, numbers,
  hyphens, and underscores.

- runner:

  A code runner implementing `execute()` and `policy()`. Required. The
  module retains this object; reset persistent runners between logically
  isolated jobs and do not use one runner concurrently.

- max_iterations:

  Maximum outer agent iterations and maximum tool calls within one
  invocation (default 10). Exceeding the inner tool-call budget raises a
  `dsprrr_codeact_iteration_limit` error.

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
