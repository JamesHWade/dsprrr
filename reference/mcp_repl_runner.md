# Posit mcp-repl Code Runner

Creates a dsprrr code runner backed by Posit's
[`mcp-repl`](https://github.com/posit-dev/mcp-repl) MCP server.
`mcp-repl` keeps a long-lived R session and enforces its sandbox with
operating-system primitives. This makes it suitable for code proposed by
an optimizer or language model.

For nonce-bound RLM submit/query traffic, dsprrr caps each encoded
control frame at 3,000 bytes so it stays below mcp-repl's inline-output
threshold. If mcp-repl nevertheless returns a file-preview or
active-pager marker (for example because user code printed a large value
first), the runner fails the iteration instead of accepting an
unverifiable partial control frame. These markers are plain text in the
upstream protocol, so detection is necessarily conservative and can only
fail closed; dsprrr never follows a disclosed sandbox file path from the
host process.

Executable Flex decodes one bounded current-step frame from raw output
before display truncation. It may also recover that frame from a plain
file preview. The frame remains untrusted and is still subject to Flex's
host-side request, budget, and output validation. Ambiguous previews,
pagers, bundles, and MCP errors fail closed. Host-generated requests
that exceed the wire bound are compressed before transport and rejected
before sending if they still do not fit.

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
  `"inherit-codex"` is rejected because
  [`mcptools::mcp_tools()`](https://posit-dev.github.io/mcptools/reference/client.html)
  does not currently propagate the required Codex sandbox metadata.

- timeout:

  Maximum execution time in seconds.

- max_output_chars:

  Maximum number of output characters returned to the optimizer.

- oversized_output:

  mcp-repl oversized-output mode. RLM previews fail closed. Executable
  Flex accepts only one bounded current-step frame in a plain file
  preview. dsprrr attempts to reset active pager state before returning
  a failure.

- extra_args:

  Reserved for future vetted mcp-repl options. It must be empty because
  arbitrary server flags can weaken the managed sandbox policy.

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

The sandbox is deliberately on by default. It disables network access
but the `workspace-write` policy still permits mutation inside allowed
workspace paths. Setting `sandbox = "off"` is rejected because this
runner advertises an enforced sandbox. Use
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
explicitly for trusted-input-only subprocess isolation.

Supplying `repl` is useful for an externally managed MCP connection and
for deterministic tests. It must be a function with the mcp-repl tool
contract: `repl(input, timeout_ms)`. Because dsprrr did not launch that
function's server, its runner policy is deliberately marked unverified
and it is rejected by optimizers that require an OS sandbox. Calling
`$shutdown()` makes the wrapper terminal but does not close that
caller-managed connection.

A managed runner captures and shuts down only the mcp-repl transport it
starts. Some supported mcptools versions do not expose public per-server
teardown, so dsprrr uses a guarded compatibility shim and fails setup if
deterministic ownership cannot be captured. This path is tested against
mcptools 1.0.1.

## Examples

``` r
if (FALSE) { # \dontrun{
runner <- mcp_repl_runner()
runner$execute("mean(1:10)")
runner$reset()
runner$shutdown()
} # }
```
