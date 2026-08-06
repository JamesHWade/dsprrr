# Create a Program of Thought Module

Factory function to create a ProgramOfThoughtModule that generates and
executes R code to solve problems. Use
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) to
execute it.
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
supports factory-backed modules; async streaming and module `$stream()`
entry points reject ProgramOfThought.
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
preserves the synchronous `forward()` fallback unless a matching
token-stream request is active; that request is rejected first.

## Usage

``` r
program_of_thought(
  signature,
  runner = NULL,
  max_iters = 3L,
  extract_answer = TRUE,
  ...,
  interpreter_factory = NULL
)
```

## Arguments

- signature:

  A Signature object or string notation defining inputs/outputs

- runner:

  Optional caller-owned code runner implementing `execute()` and
  `policy()`. It is retained, never automatically closed, and must not
  be shared concurrently when persistent.

- max_iters:

  Maximum code generation/repair iterations (default 3)

- extract_answer:

  Logical. If TRUE (default), use LLM to extract final answer from
  execution result. If FALSE, return execution result directly.

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

A ProgramOfThoughtModule object

## Examples

``` r
if (FALSE) { # \dontrun{
runner <- r_code_runner(timeout = 30)
pot <- program_of_thought("question -> answer", runner = runner)
result <- run(pot, question = "Calculate 847 * 293", .llm = llm)
} # }
```
