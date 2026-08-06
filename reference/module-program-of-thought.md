# Program of Thought Module

A module that generates R code to solve problems, executes it through an
explicitly configured runner, and uses the execution results to produce
answers. This is particularly effective for tasks requiring exact
computation (arithmetic, statistics, data manipulation) where LLMs alone
are unreliable.

## Details

The execution flow is:

1.  LLM generates R code based on the inputs

2.  Code is executed by the configured code runner

3.  If execution fails, the error is fed back to the LLM for repair

4.  Steps 2-3 repeat until success or max_iters is reached

5.  Final answer is extracted from the execution result

Security: Code execution requires explicit opt-in via `runner` or
`interpreter_factory`. The built-in runner uses a separate process but
is NOT a security sandbox. Inspect `runner$policy()` before execution.
For untrusted inputs, provide a runner backed by OS-level sandboxing
(such as a container or AppArmor).

Runner lifecycle: supply exactly one runtime source. `runner` is
caller-owned, reused across calls, and never closed by dsprrr. The
backend determines whether execution state persists and whether
`reset()` is available; serialize access to stateful backends.
`interpreter_factory` is a zero-argument function that returns a fresh
runner implementing `execute()`, `policy()`, optional
[`start()`](https://rdrr.io/r/stats/start.html), and terminal
`shutdown()` or [`close()`](https://rdrr.io/r/base/connections.html).
The module owns that runner for one invocation and shuts it down exactly
once on success, error, or interrupt.

[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
supports factory-backed ProgramOfThought in an isolated mirai process.
It rejects caller-owned runners.
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
and a module's `$stream()` method remain unavailable because streaming
would bypass code execution.
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
may use its one-shot `forward()` fallback, but rejects an actual
token-stream request before provider or factory work.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a runner (required for code execution)
runner <- r_code_runner(timeout = 30)

# Create a Program of Thought module
pot <- program_of_thought(
  signature = "question -> answer",
  runner = runner
)

# Use it for computation tasks
result <- run(pot, question = "What is the sum of primes under 100?", .llm = llm)
} # }
```
