# Posit mcp-repl Code Runner

Creates a dsprrr code runner backed by Posit's
[`mcp-repl`](https://github.com/posit-dev/mcp-repl) MCP server.
`mcp-repl` keeps a long-lived R session and enforces its sandbox with
operating-system primitives. This makes it suitable for code proposed by
an optimizer or language model.

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

  Optional function implementing the mcp-repl `repl` tool.

- command:

  Path or command name for the `mcp-repl` executable.

- interpreter:

  Interpreter passed to mcp-repl. Currently only `"r"` is supported by
  this runner.

- sandbox:

  mcp-repl sandbox policy. Defaults to `"workspace-write"`.
  `"inherit-codex"` may be used only when the MCP client propagates
  Codex sandbox metadata;
  [`mcptools::mcp_tools()`](https://posit-dev.github.io/mcptools/reference/client.html)
  does not currently do so.

- timeout:

  Maximum execution time in seconds.

- max_output_chars:

  Maximum number of output characters returned to the optimizer.

- oversized_output:

  mcp-repl oversized-output mode.

- extra_args:

  Additional command-line arguments passed to mcp-repl.

## Value

An `McpReplRunner` implementing the dsprrr code-runner protocol.

## Details

By default, `mcp_repl_runner()` starts `mcp-repl` through
[`mcptools::mcp_tools()`](https://posit-dev.github.io/mcptools/reference/client.html)
with:

- the R interpreter;

- the `workspace-write` sandbox;

- network access disabled by the sandbox; and

- oversized output written to sandbox-visible files.

The sandbox is deliberately on by default. Setting `sandbox = "off"` is
rejected because this runner advertises itself as safe for untrusted
code. Use
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
explicitly for trusted-input-only subprocess isolation.

Supplying `repl` is useful for an already-managed MCP connection and for
deterministic tests. It must be a function with the mcp-repl tool
contract: `repl(input, timeout_ms)`.

## Examples

``` r
if (FALSE) { # \dontrun{
runner <- mcp_repl_runner()
runner$execute("mean(1:10)")
runner$reset()
} # }
```
