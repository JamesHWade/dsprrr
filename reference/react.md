# Create a ReAct module

Create a tool-using module that alternates reasoning with tool calls.
Tool use is explicit: passing tools to
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
never changes a prediction module into a ReAct agent.

## Usage

``` r
react(
  signature,
  tools = list(),
  max_iterations = 10L,
  chat = NULL,
  template = "",
  demos = list(),
  config = list(),
  ...
)
```

## Arguments

- signature:

  A Signature object or string notation defining inputs and outputs.

- tools:

  A list of ellmer ToolDef objects.

- max_iterations:

  Maximum number of tool-call iterations.

- chat:

  Optional ellmer Chat object.

- template:

  Optional glue template for prompt generation.

- demos:

  Optional list of demonstration examples.

- config:

  Optional prediction configuration.

- ...:

  Must be empty.

## Value

A ReactModule executed with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

## Examples

``` r
agent <- react(
  "question -> answer",
  tools = list(),
  max_iterations = 10
)
```
