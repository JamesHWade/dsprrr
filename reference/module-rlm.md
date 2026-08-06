# Recursive Language Model (RLM) Module

A module that transforms context from "input" to "environment", enabling
LLMs to programmatically explore large contexts through a REPL interface
rather than embedding them in prompts.

## Details

Instead of `llm(prompt, context=huge_document)`, RLM stores context as R
variables that the LLM can peek, slice, search, and recursively query.

The execution flow is:

1.  Context is made available as variables in an R execution environment

2.  LLM generates R code to explore and analyze the context

3.  Code is executed by the configured code runner

4.  Results are fed back to the LLM for the next iteration

5.  Process continues until SUBMIT() is called or max_iterations reached

6.  If max_iterations reached without SUBMIT(), fallback extraction is
    used

Available REPL tools:

- `SUBMIT(...)`: Terminate and return final output values

- `peek(var, start, end)`: View a slice of a variable (default: first
  1000 chars)

- `search(var, pattern)`: Regex search in variable

- `llm_query(query, context_slice)`: Recursive LLM call (requires
  sub_lm)

- `llm_query_batched(queries, slices)`: Batched recursive calls

Security: Code execution requires explicit opt-in via `runner` or
`interpreter_factory`. The built-in runner uses a separate process but
is NOT a security sandbox. Inspect `runner$policy()` before execution.
For untrusted inputs, provide a runner backed by OS-level sandboxing,
such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md).
Authenticated RLM control frames sent through mcp-repl are limited to
3,000 encoded bytes. If aggregate output is compacted into a file
preview or pager, the iteration fails closed because mcp-repl does not
expose structured compaction metadata; dsprrr does not read a
sandbox-disclosed path from the host process.

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
supports factory-backed RLM in an isolated mirai process and rejects
caller-owned runners.
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
and a module's `$stream()` method remain unavailable because streaming
would bypass execution. The
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
one-shot `forward()` fallback remains available.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a runner (required for code execution)
runner <- r_code_runner(timeout = 30)

# Create an RLM module for exploring large documents
rlm <- rlm_module(
  signature = "document, question -> answer",
  runner = runner
)

# Use it for context exploration
long_doc <- paste(readLines("large_file.txt"), collapse = "\n")
result <- run(rlm, document = long_doc, question = "What are the main themes?", .llm = llm)

# Enable recursive LLM calls for complex reasoning
rlm_recursive <- rlm_module(
  signature = "document -> summary",
  runner = runner,
  sub_lm = ellmer::chat_openai(model = "gpt-4o-mini"),
  max_llm_calls = 10
)
} # }
```
