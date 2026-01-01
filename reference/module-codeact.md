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

Security: Code execution requires explicit opt-in via a runner
parameter. The runner provides subprocess isolation but is NOT a
security sandbox. For untrusted inputs, use OS-level sandboxing.

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
