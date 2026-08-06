# Create a CodeAct Module

Factory function to create a CodeActModule that can use both tools and R
code execution to solve problems. Use
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) to
execute it.
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
supports factory-backed modules; async streaming and module `$stream()`
reject CodeAct.
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
preserves the synchronous `forward()` fallback unless a matching
token-stream request is active; that request is rejected first.

## Usage

``` r
code_act(
  signature,
  tools = list(),
  runner = NULL,
  max_iterations = 10L,
  ...,
  interpreter_factory = NULL
)
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

  Optional caller-owned code runner implementing `execute()` and
  `policy()`. It is retained, never automatically closed, and must not
  be shared concurrently when persistent.

- max_iterations:

  Maximum outer agent iterations and maximum tool calls within one
  invocation (default 10). Exceeding the inner tool-call budget raises a
  `dsprrr_codeact_iteration_limit` error.

- ...:

  Additional arguments passed to the module

- interpreter_factory:

  Optional zero-argument function returning a fresh runner with
  `execute()`, `policy()`, optional
  [`start()`](https://rdrr.io/r/stats/start.html), and idempotent
  terminal `shutdown()` or
  [`close()`](https://rdrr.io/r/base/connections.html). Supply exactly
  one of `runner` and `interpreter_factory`.

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
