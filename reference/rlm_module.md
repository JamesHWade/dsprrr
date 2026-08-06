# Create a Recursive Language Model (RLM) Module

Factory function to create an RLMModule that enables LLMs to
programmatically explore large contexts through a REPL interface. Use
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) to
execute it.
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
supports factory-backed modules; async streaming and module `$stream()`
reject RLM.
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
preserves the synchronous `forward()` fallback unless a matching
token-stream request is active; that request is rejected first.

## Usage

``` r
rlm_module(
  signature,
  runner = NULL,
  max_iterations = 20L,
  max_llm_calls = 50L,
  max_output_chars = 100000L,
  sub_lm = NULL,
  verbose = FALSE,
  tools = list(),
  max_iters = NULL,
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

- max_iterations:

  Maximum REPL iterations before fallback (default 20)

- max_llm_calls:

  Maximum recursive LLM calls allowed (default 50)

- max_output_chars:

  Maximum characters per execution output (default 100000)

- sub_lm:

  Optional ellmer Chat for recursive queries. NULL = disabled.

- verbose:

  Logical. Print execution progress (default FALSE)

- tools:

  Named list of user-defined host functions. Guest code emits an
  authenticated request, dsprrr invokes the original function in the
  host, and the guest is replayed with the response. Closures are never
  deparsed or serialized into generated code.

- max_iters:

  DSPy 3.3-compatible alias for `max_iterations`. Supply only one of
  these arguments.

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

An RLMModule object

## Examples

``` r
if (FALSE) { # \dontrun{
runner <- r_code_runner(timeout = 30)
rlm <- rlm_module("question -> answer", runner = runner)
result <- run(rlm, question = "What is the 10th Fibonacci number?", .llm = llm)
} # }
```
