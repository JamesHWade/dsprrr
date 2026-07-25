# agentic harnesses require an OS-sandboxed runner by default

    Code
      compile(tp, harness_program(), harness_data(), .llm = make_harness_task_llm(),
      .agent_llm = agent)
    Condition
      Error in `harness_validate_runner()`:
      ! An OS-sandboxed code runner is required
      i Supply `runner = mcp_repl_runner()`.
      i Set `sandbox = FALSE` only when the agent must not execute code.

---

    Code
      compile(tp, harness_program(), harness_data(), .llm = make_harness_task_llm(),
      .agent_llm = agent, runner = r_code_runner())
    Condition
      Error in `harness_validate_runner()`:
      ! The agentic harness requires an OS-sandboxed runner
      x Runner "callr" advertises `sandboxed = FALSE`.
      i Use `mcp_repl_runner()` or set `sandbox = FALSE` to disable agent code execution.
