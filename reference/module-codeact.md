# CodeAct Module

A hybrid agent module that combines tool calling with R code execution.
The model can choose between calling registered tools or generating R
code to solve problems. This enables flexible agentic workflows that
leverage both external tools and computational capabilities.

## Details

CodeAct extends the ReAct pattern by adding an `execute_r_code` tool
that allows the agent to write and run R code. The execution flow is:

1.  Agent receives the task and available tools (including code
    execution)

2.  Agent iteratively calls tools or executes code until it has enough
    info

3.  Agent produces final structured answer

Security: Code execution requires explicit opt-in via `runner` or
`interpreter_factory`. The built-in runner uses a separate process but
is NOT a security sandbox. Inspect `runner$policy()` before execution.
For untrusted inputs, provide a runner backed by OS-level sandboxing.

Runner lifecycle: supply exactly one runtime source. `runner` is
caller-owned, reused across calls, and never closed by dsprrr. The
backend determines whether execution state persists and whether
`reset()` is available; serialize access to stateful backends.
`interpreter_factory` is a zero-argument function that returns a fresh
runner implementing `execute()`, `policy()`, optional
[`start()`](https://rdrr.io/r/stats/start.html), and terminal
`shutdown()`. The module owns that runner for one invocation and shuts
it down exactly once on success, error, or interrupt. Any retained code
tool becomes terminal after shutdown.

[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
supports factory-backed CodeAct in an isolated mirai process. It rejects
caller-owned runners.
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
and a module's `$stream()` method remain unavailable because streaming
would bypass execution. The
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
one-shot `forward()` fallback remains available.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a runner for code execution
runner <- r_code_runner(timeout = 30)

# Create a CodeAct agent with custom tools
search_tool <- ellmer::tool(
  function(query) "Search results...",
  description = "Search for information"
)

agent <- code_act(
  signature = "question -> answer",
  tools = list(search = search_tool),
  runner = runner
)

# The agent can now search AND compute
result <- run(agent,
  question = "What is 10% of France's population?",
  .llm = llm
)
} # }
```
