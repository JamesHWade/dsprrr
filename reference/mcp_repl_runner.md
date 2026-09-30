# Run R code in an operating-system sandbox with mcp-repl

`mcp_repl_runner()` creates a code runner backed by Posit's
[`mcp-repl`](https://github.com/posit-dev/mcp-repl) MCP server.
`mcp-repl` keeps a long-lived R session and enforces a sandbox with
operating-system primitives, so it is suitable for code written by a
model or an optimizer. Use it with
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md)
and the agentic optimizers
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
and
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md).

It needs the suggested mcptools package (1.0.1 or later) and the
external `mcp-repl` executable.

## Usage

``` r
mcp_repl_runner(
  repl = NULL,
  command = "mcp-repl",
  interpreter = "r",
  sandbox = "workspace-write",
  timeout = 30,
  max_output_chars = 100000L,
  oversized_output = "files",
  extra_args = character()
)
```

## Arguments

- repl:

  Optional function implementing the mcp-repl `repl` tool; see Details.

- command:

  Path or name of the `mcp-repl` executable.

- interpreter:

  Interpreter passed to mcp-repl. Only `"r"` is supported.

- sandbox:

  Sandbox policy, `"workspace-write"` (the default). `"inherit-codex"`
  is rejected because
  [`mcptools::mcp_tools()`](https://posit-dev.github.io/mcptools/reference/client.html)
  does not pass on the Codex sandbox metadata it needs.

- timeout:

  Maximum time for one `$execute()` or `$reset()` call, in seconds
  (default 30). Code still running after it is interrupted and reported
  as a timeout error; see Details.

- max_output_chars:

  Maximum number of output characters returned per call (default
  `100000L`).

- oversized_output:

  mcp-repl's mode for oversized output (default `"files"`). RLM rejects
  file previews; executable Flex accepts one bounded message in a plain
  file preview. dsprrr tries to reset an active pager before reporting a
  failure.

- extra_args:

  Reserved for future vetted mcp-repl options and must be empty, because
  arbitrary server flags could weaken the sandbox.

## Value

An `McpReplRunner` object implementing the runner interface described in
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md):
`$execute()`, `$policy()`, `$reset()` and `$shutdown()`.

## Details

### Sandbox

By default, the runner starts `mcp-repl` through
[`mcptools::mcp_tools()`](https://posit-dev.github.io/mcptools/reference/client.html)
with the R interpreter and the `workspace-write` sandbox: network access
is disabled, writes are allowed inside the workspace, and oversized
output is written to files the sandbox can see. `sandbox = "off"` is not
accepted, because this runner promises an enforced sandbox; use
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
for trusted input without a sandbox.

### Your own connection

`repl` accepts a function with the mcp-repl tool contract,
`repl(input, timeout_ms)`, for an MCP connection you manage or for
tests. Because dsprrr did not start that server, it cannot vouch for the
sandbox: the runner is marked unverified and optimizers that require a
sandbox reject it. `$shutdown()` then ends the runner but leaves your
connection open. A runner that dsprrr starts shuts down only the
transport it started.

### Long-running code

mcptools waits only about 4 seconds for each reply from mcp-repl, so
dsprrr asks mcp-repl to wait at most 3 seconds per request. Code that
runs longer keeps running: mcp-repl reports it as busy, and dsprrr
collects the rest of its output with further requests until it finishes,
then returns all of it together, without mcp-repl's busy status lines.
Code still running after `timeout` seconds is interrupted, as with
Ctrl-C, and `$execute()` returns a timeout error; the session and its
variables are kept. If the interrupt does not stop the code, or mcp-repl
does not reply in time, the runner cannot be used again, so that a late
reply can never be read as the answer to a later request.

### Output limits

RLM exchanges control messages with the guest session, capped at 3,000
bytes each so they stay below mcp-repl's inline-output limit. If
mcp-repl still replies with a file preview or a pager (for example
because the code printed a large value first), the iteration fails
rather than accepting a partial message; dsprrr never follows a file
path reported by the sandbox. Executable Flex programs read one bounded
message per step and can recover it from a plain file preview; anything
ambiguous fails. Host requests that are too large are compressed and, if
still too large, rejected before sending.

## See also

Other code execution:
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
if (FALSE) { # \dontrun{
runner <- mcp_repl_runner()
runner$policy()$sandboxed
runner$execute("mean(1:10)")$result
runner$reset()
runner$shutdown()

# Give each RLM call a fresh sandboxed session
analyst <- rlm_module(
  "document, question -> answer",
  interpreter_factory = function() mcp_repl_runner(timeout = 30)
)
} # }
```
