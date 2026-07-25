# mcp_repl_runner refuses an unsandboxed policy

    Code
      mcp_repl_runner(repl = function(input, timeout_ms) input, sandbox = "off")
    Condition
      Error in `mcp_repl_runner()`:
      ! `sandbox` must be "workspace-write" or "inherit-codex"
      i Use `r_code_runner()` explicitly for trusted, unsandboxed code.
