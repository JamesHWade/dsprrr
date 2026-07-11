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

Security: Code execution requires explicit opt-in via a runner
parameter. The built-in runner uses a separate process but is NOT a
security sandbox. Inspect `runner$policy()` before execution. For
untrusted inputs, provide a runner backed by OS-level sandboxing (such
as a container or AppArmor).

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
