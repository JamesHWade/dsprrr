# Advanced Reasoning Modules

``` r

library(dsprrr)
library(ellmer)
```

## Overview

Choose a module by the work it needs to do:

- **Step-by-step reasoning** with ChainOfThought
- **Multiple attempts** with BestOfN
- **Iterative refinement** with Refine
- **Ensemble reasoning** with MultiChainComparison
- **Exact computation** with ProgramOfThought (code generation)
- **Hybrid agents** with CodeAct (tools + code execution)
- **Adaptive investigation** of large or awkward R objects with RLM
- **Implementation search** across predictors, R logic, and tools with
  Flex

The sections below show each execution pattern and its tradeoffs.

## ChainOfThought

ChainOfThought (CoT) asks the model for a reasoning field before the
final answer.

### Why Use ChainOfThought?

Use it when intermediate reasoning is useful to the task or evaluator.
It adds tokens and does not make the reasoning inherently faithful, so
evaluate the final output against a task-specific metric.

### Basic Usage

The simplest way to use CoT is with
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md):

``` r

# Create a CoT module
math_solver <- chain_of_thought("problem -> solution")

# Run it
result <- run(
  math_solver,
  problem = "If a train travels 120 miles in 2 hours, what is its average speed?",
  .llm = chat_openai()
)

# Result includes both reasoning and answer
result$reasoning
#> "To find average speed, I need to divide total distance by total time.
#>  Distance = 120 miles, Time = 2 hours.
#>  Speed = 120 / 2 = 60 miles per hour."

result$solution
#> "60 miles per hour"
```

### Signature Transforms

Under the hood,
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
uses
[`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md)
to transform the signature. You can use this directly for more control:

``` r

# Start with a regular signature
sig <- signature("question -> answer: string")

# Transform it to include reasoning
cot_sig <- with_reasoning(sig)

# The output now includes a reasoning field
names(cot_sig@output_type@properties)
#> [1] "reasoning" "answer"

# Check if a signature has reasoning
has_reasoning(cot_sig)
#> TRUE
has_reasoning(sig)
#> FALSE
```

### Custom Reasoning Prefix

You can customize the reasoning prompt:

``` r

# Default: "Let's think step by step in order to"
math_cot <- with_reasoning(
  "equation -> result",
  prefix = "Let me solve this equation carefully:"
)

# For code tasks
code_cot <- with_reasoning(
  "task -> code",
  prefix = "Let me break down the implementation:"
)
```

### Removing Reasoning

For A/B testing CoT vs non-CoT performance:

``` r

cot_sig <- with_reasoning("question -> answer")
plain_sig <- without_reasoning(cot_sig)

has_reasoning(plain_sig)
#> FALSE
```

## BestOfN

BestOfN addresses output variance by running a module multiple times and
selecting the best result based on a reward function.

### Why Use BestOfN?

LLM outputs can be inconsistent. The same prompt might produce correct
output 70% of the time. BestOfN increases reliability by: - Making
multiple attempts - Scoring each attempt with a reward function -
Returning the highest-scoring result - Optionally stopping early when a
threshold is met

### Basic Usage

``` r

# Create a QA module
qa <- module(signature("question -> answer"))

# Wrap with BestOfN (default N=3)
reliable_qa <- best_of_n(qa, N = 5)

# Run - internally makes up to 5 attempts
result <- run(
  reliable_qa,
  question = "What is the capital of France?",
  .llm = chat_openai()
)
```

### Reward Functions

The power of BestOfN comes from custom reward functions that score
outputs:

``` r

# Reward function signature: function(prediction, inputs) -> [0, 1]

# Example: Prefer single-word answers
one_word_reward <- function(pred, inputs) {

  words <- strsplit(as.character(pred$answer), "\\s+")[[1]]
  if (length(words) == 1) 1.0 else 0.0
}

# Example: Prefer confident answers
confidence_reward <- function(pred, inputs) {
  # Check for hedging language
  hedges <- c("maybe", "perhaps", "possibly", "might")
  answer <- tolower(pred$answer)
  if (any(sapply(hedges, grepl, answer))) 0.3 else 1.0
}

wrapper <- best_of_n(
  qa,
  N = 5,
  reward_fn = one_word_reward,
  threshold = 1.0  # Stop early if we get a one-word answer
)
```

### Using Metrics as Rewards

Convert existing metrics to reward functions with
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md):

``` r

# When you have expected values in your inputs
wrapper <- best_of_n(
  qa,
  N = 3,
  reward_fn = as_reward_fn(
    metric_exact_match(field = "answer"),
    expected_field = "expected_answer"
  )
)

# Run with expected value for reward calculation
result <- run(
  wrapper,
  question = "What is 2+2?",
  expected_answer = "4",
  .llm = chat_openai()
)
```

### Inspecting Attempts

After running, you can examine all attempts:

``` r

# Get attempts from last run
attempts <- wrapper$get_attempts()
attempts
#> # A tibble: 3 x 4
#>     run attempt prediction       score
#>   <int>   <int> <list>           <dbl>
#> 1     1       1 <named list [1]>   0
#> 2     1       2 <named list [1]>   1
#> 3     1       3 <named list [1]>   0

# Get all attempts across multiple runs
all_attempts <- wrapper$get_attempts(all = TRUE)
```

### Metadata

BestOfN tracks useful metadata. Use `.return_format = "structured"` to
access it:

``` r

# Use structured format to access metadata
result <- run(wrapper, question = "Test", .llm = llm, .return_format = "structured")

# Access metadata fields
result$metadata$n_attempts     # How many attempts were made
result$metadata$best_score     # Score of selected result
result$metadata$all_scores     # Scores of all attempts
result$metadata$early_stopped  # Did we hit threshold?
result$metadata$total_tokens   # Tokens across all attempts
result$metadata$total_cost     # Cost across all attempts

# For batch operations with run_dataset(), use .metadata column:
# batch_result$.metadata[[1]]$n_attempts
```

## Refine

Refine extends BestOfN with a feedback loop. After each failed attempt,
it generates feedback explaining what was wrong and injects this into
the next attempt.

### Why Use Refine?

While BestOfN makes independent attempts, Refine learns from mistakes.
Each iteration receives feedback about the previous attempt, allowing
the model to correct specific issues.

### Basic Usage

``` r

# Create module that accepts feedback
qa <- module(signature("question, feedback -> answer"))

# One-word answer reward
one_word_reward <- function(pred, inputs) {
  words <- strsplit(as.character(pred$answer), "\\s+")[[1]]
  if (length(words) == 1) 1.0 else 0.0
}

# Wrap with Refine
refined <- refine(
  qa,
  N = 3,
  reward_fn = one_word_reward,
  threshold = 1.0,
  feedback_template = "Your answer '{prediction}' scored {score}. Please give a single word answer."
)

result <- run(
  refined,
  question = "What is the capital of France?",
  .llm = chat_openai()
)
```

### Feedback Templates

Feedback templates use glue syntax with these variables: - `{score}` -
The score from the reward function -
[prediction](https://github.com/bbolker/prediction) - The previous
output (formatted as string) - Any input field names from your signature

``` r

# Reference input fields
template <- "For the question '{question}', your answer '{prediction}' scored {score}. Try again."

# Be specific about what's wrong
template <- "Score: {score}. Your answer was too verbose. Give only the city name."

# Use conditional language
template <- "Previous attempt scored {score}/1.0. Focus on precision and brevity."
```

### Custom Feedback Field

By default, feedback is injected as a field called `feedback`. You can
customize this:

``` r

refined <- refine(
  module(signature("question, hint -> answer")),
  N = 3,
  reward_fn = my_reward,
  feedback_field = "hint"  # Use 'hint' instead of 'feedback'
)
```

### Feedback History

Track the feedback generated across iterations:

``` r

result <- run(refined, question = "Test", .llm = llm)

# Get feedback from last run
refined$get_feedback_history()
#> [1] "Your answer 'The capital is Paris' scored 0..."
#> [2] "Your answer 'Paris, France' scored 0..."

# Get all feedback across runs
refined$get_feedback_history(all = TRUE)
```

## MultiChainComparison

MultiChainComparison (MCC) runs several independent reasoning chains and
asks a final model call to synthesize one answer.

### Why Use MultiChainComparison?

MCC: - Generates M diverse reasoning attempts (using temperature for
variation) - Compares all attempts in a synthesis step - Produces one
answer from the compared attempts

### Basic Usage

``` r

# Create MCC module
mcc <- multi_chain_comparison(
  "question -> answer",
  M = 3,           # Number of reasoning chains
  temperature = 0.7 # Higher = more diversity
)

result <- run(
  mcc,
  question = "What are the pros and cons of renewable energy?",
  .llm = chat_openai()
)

# Result is synthesized from all chains
result$reasoning
result$answer
```

### Using the Module Factory

MCC is also available via the
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
factory:

``` r

mcc <- module(
  signature("context, question -> answer"),
  type = "multichain",
  M = 5,
  temperature = 0.8
)
```

### Custom Inner Module

By default, MCC uses ChainOfThought for the inner module. You can
provide your own:

``` r

# Use a custom CoT module
cot <- chain_of_thought(
  "question -> answer",
  prefix = "Let me analyze this from multiple angles:"
)

mcc <- multi_chain_comparison(
  "question -> answer",
  inner_module = cot,
  M = 5
)
```

### Custom Comparison Template

Customize how attempts are compared:

``` r

mcc <- multi_chain_comparison(
  "question -> answer",
  M = 3,
  comparison_template = paste0(
    "You have {M} expert analyses of the same question.\n\n",
    "{attempts_text}\n\n",
    "Synthesize these into a single authoritative answer. ",
    "Note where experts agree and resolve any disagreements."
  )
)
```

### Inspecting Chains

View the individual reasoning chains:

``` r

result <- run(mcc, question = "Complex question...", .llm = llm)

# Get all chain results
chains <- mcc$get_attempts()
chains
#> # A tibble: 3 x 3
#>     run attempt prediction
#>   <int>   <int> <list>
#> 1     1       1 <named list [2]>
#> 2     1       2 <named list [2]>
#> 3     1       3 <named list [2]>

# Each prediction has reasoning and answer
chains$prediction[[1]]
#> $reasoning
#> [1] "First, let me consider..."
#> $answer
#> [1] "The answer is..."
```

## ProgramOfThought

ProgramOfThought addresses a fundamental LLM limitation: they’re
unreliable at exact computation. Instead of asking the model to compute
directly, it generates R code that R executes.

### Why Use ProgramOfThought?

LLMs frequently make arithmetic errors, especially with multi-step
calculations. ProgramOfThought solves this by: - Having the LLM generate
R code to solve the problem - Executing that code in an isolated
subprocess - If execution fails, feeding the error back for code
repair - Extracting the final answer from the execution result

### Setting Up Code Execution

Code execution requires explicit opt-in via a runner or interpreter
factory. Here is the caller-owned form:

``` r

# Create a runner - this enables code execution
runner <- r_code_runner(
  timeout = 30,                    # Max execution time
  allowed_packages = c("base", "stats", "utils")  # Allowed packages
)
```

**Security note**:
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
provides subprocess isolation but is not a security sandbox. For
untrusted generated code, use a fresh managed
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md)
from an interpreter factory or another runner with verified OS-level
sandboxing.

ProgramOfThought, CodeAct, and RLM accept exactly one execution binding.
Pass `runner` to retain a caller-owned runner object that dsprrr reuses
and never closes. Whether execution state persists depends on the
backend. Serialize a stateful runner, and reset it between unrelated
jobs when that backend supports `reset()`. Alternatively, pass a
zero-argument `interpreter_factory`; dsprrr calls it once per
invocation, owns the fresh runner, and closes it exactly once when the
invocation ends, including after an error:

``` r

pot <- program_of_thought(
  "question -> answer",
  interpreter_factory = function() r_code_runner(timeout = 30)
)
```

The factory form prevents state from crossing invocation boundaries. A
direct runner is caller-owned and sequential; never share a stateful
runner across concurrent calls. Supplying both forms is an error.
Factory-backed ProgramOfThought, CodeAct, and RLM support
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
and isolated mirai batch execution because every invocation owns a fresh
runner. Caller-owned runners remain sequential. Specialized token
streaming is still rejected before provider or factory work;
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
without a matching token listener preserves the ordinary synchronous
`forward()` path.

### Basic Usage

``` r

# Create a ProgramOfThought module
pot <- program_of_thought("question -> answer", runner = runner)

# Run it - the LLM generates code, R executes it
result <- run(
  pot,
  question = "What is the sum of all prime numbers under 100?",
  .llm = chat_openai()
)

# Result is the computed answer
result$answer
#> "1060"
```

### Automatic Error Recovery

If the generated code fails, ProgramOfThought automatically feeds the
error back to the LLM for repair:

``` r

pot <- program_of_thought(
  "question -> answer",
  runner = runner,
  max_iters = 3  # Try up to 3 times to get working code
)

# Even if first attempt has a bug, it may self-correct
result <- run(pot, question = "Calculate factorial of 10", .llm = llm)
```

### Accessing Execution History

Track the code generation and execution process:

``` r

# After running, inspect execution history
executions <- pot$get_executions()
executions[[1]]$iterations  # List of code attempts
executions[[1]]$success     # Whether it succeeded
```

### Using Context Data

Pass data to your code via the `.context` list:

``` r

pot <- program_of_thought("data, question -> answer", runner = runner)

result <- run(
  pot,
  data = mtcars,
  question = "What is the correlation between mpg and hp?",
  .llm = llm
)
# The LLM can generate: cor(.context$data$mpg, .context$data$hp)
```

## CodeAct

CodeAct combines declared host tools with generated R execution. Use it
when a task needs both an external action and computation inside one
bounded agent loop.

### Why Use CodeAct?

Some tasks require multiple capabilities: - Search for information (tool
calling) - Perform calculations on that information (code execution) -
Iterate until the answer is found (agent loop)

CodeAct provides all of these in a single module.

### Basic Usage

``` r

# Create tools
search_tool <- ellmer::tool(
  function(query) search_api(query),
  description = "Search for information",
  arguments = list(query = ellmer::type_string())
)

# Create CodeAct agent with tools and code execution
runner <- r_code_runner(timeout = 30)
agent <- code_act(
  "question -> answer",
  tools = list(search = search_tool),
  runner = runner
)

# The agent can search AND compute
result <- run(
  agent,
  question = "What is 10% of France's current population?",
  .llm = chat_openai()
)
# Agent might: 1) Search for France's population, 2) Execute: 67000000 * 0.10
```

### Built-in Code Execution Tool

CodeAct automatically includes an `execute_r_code` tool that the LLM can
call:

``` r

agent <- code_act("question -> answer", runner = runner)

# The LLM sees this tool:
# execute_r_code(code): Execute R code in an isolated environment.
#   The input data is available in the `.context` list.
```

### Controlling Iterations

``` r

agent <- code_act(
  "question -> answer",
  runner = runner,
  # Caps outer agent iterations and inner tool calls; excess tool calls error.
  max_iterations = 10
)
```

### Inspecting Agent Trajectory

Track the agent’s decision-making process:

``` r

result <- run(agent, question = "Complex question...", .llm = llm)

# Get the trajectory
trajectories <- agent$get_trajectories()
trajectories[[1]]$iterations    # Number of iterations
trajectories[[1]]$trajectory    # List of steps taken
```

### Combining with Custom Tools

``` r

# Create multiple tools
weather_tool <- ellmer::tool(
  function(city) get_weather(city),
  description = "Get current weather",
  arguments = list(city = ellmer::type_string())
)

database_tool <- ellmer::tool(
  function(query) run_sql(query),
  description = "Query the database",
  arguments = list(query = ellmer::type_string())
)

# CodeAct with multiple tools + code execution
agent <- code_act(
  "question -> answer",
  tools = list(weather = weather_tool, database = database_tool),
  runner = runner
)
```

## Recursive Language Model (experimental)

Use an RLM when the answer is inside an R object but the useful slice
and calculation are not known in advance. The model proposes one R
operation, observes bounded output, and chooses the next operation. The
full object does not enter every model prompt.

That is a different job from the other code-oriented modules:

| Need | Module |
|----|----|
| Execute a calculation whose steps are already known | [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md) |
| Discover an exploration path for this invocation | [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) |
| Learn a reusable implementation from labeled examples | [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) with GEPA |

### Investigate a large R object

The release-regression tutorial uses 40,000 session rows and 200 change
records. A persistent callr runner stages those rich R objects once and
keeps derived values between iterations:

``` r

incident <- rlm_module(
  paste(
    "sessions, changes, question ->",
    "release: string, cohort: string, before_rate: number,",
    "after_rate: number, drop_pp: number, change_id: string, evidence: string"
  ),
  interpreter_factory = function() {
    r_code_runner(timeout = 30, persistent = TRUE)
  },
  max_iters = 8,
  max_llm_calls = 0L,
  max_output_chars = 10000
)

result <- run(
  incident,
  sessions = sessions,
  changes = changes,
  question = "Which cohort regressed, and which change best explains it?",
  .llm = chat_openai(),
  .return_format = "structured"
)
```

This configuration is appropriate only when the fixture and generated
code are trusted. `persistent = TRUE` preserves one callr process, but
that process has the host user’s file, network, and environment
permissions.

The default `max_output_chars = 10000` keeps a head-and-tail excerpt
from each execution in the next prompt. It bounds model-visible
evidence; it does not increase a runner’s transport limit.

### Recursive queries return values

`sub_lm = NULL` inherits the outer model passed to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).
Generated code can therefore assign and use the result of a focused
query:

``` r

candidate <- subset(.context$changes, component == "checkout-auth")
interpretation <- llm_query(
  "Which change could reduce successful token refreshes?",
  paste(candidate$note, collapse = "\n")
)
SUBMIT(answer = interpretation)
```

The guest emits a nonce-bound, schema-checked request, dsprrr calls the
model in the host, then replays the same code evaluation with the
returned value. One ordered ledger prevents query/tool replay from
changing operation kind. It cannot roll back an external side effect
performed directly by generated code before the query, so RLM code
should keep pre-query work read-only.

`SUBMIT()` is checked against the signature. Missing required, extra, or
incompatible fields become a repairable observation so the next
iteration can correct the submission; optional fields may be omitted. If
the iteration budget ends first, the extraction predictor attempts to
produce the best typed answer supported by the trajectory; provider or
type-validation failure remains terminal.

The action and fallback extraction steps are graph-visible child
predictors:

``` r

names(incident$graph_children())
#> [1] "generate_action" "extract"
```

Structured results report whether the answer came from `SUBMIT()` or
fallback, and retain the bounded trajectory:

``` r

result$output
result$metadata$output_source
result$metadata$repl_history
result$metadata$runner_policy
```

### Choose the execution boundary

The one-call helper creates a fresh managed MCP sandbox by default:

``` r

answer <- rlm(
  "document, question -> answer",
  document = "Owner: team-a\nCommitment: rotate signing keys quarterly",
  question = "Which commitments have no owner?",
  .llm = chat_openai(),
  .max_iterations = 4L,
  .max_llm_calls = 0L
)
```

Managed `mcp-repl` requires the suggested R package `mcptools` plus the
external `mcp-repl` executable. It disables network access and applies
an OS sandbox, but workspace writes remain allowed. Requests have a 7 KB
wire bound and RLM control frames have a 3,000-byte encoded bound. When
a raw request is too large, dsprrr first tries a gzip/base64 wrapper;
the final JSON-RPC request must still fit the wire bound. Oversized
output may be rejected by the runner before the module’s
10,000-character head-and-tail formatter. Host tools run outside that
sandbox with host permissions. Use explicit persistent
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
for trusted large data frames and fitted models; it is not a sandbox.

See [Investigate a Release Regression with an
RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.md)
for the deterministic demo and [How the RLM
Works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.md)
for the complete execution contract.

## Flex (experimental)

Use [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
when the implementation strategy is the search problem. GEPA can change
which predictors run, add deterministic R logic, or call a selected
tool—not only rewrite instructions inside a fixed module.

``` r

program <- flex("question -> answer")
program$module_src
```

Use a regular module when its shape is known, or an explicit pipeline
when people should own the workflow. See [Flex: Optimize the Whole
Program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md)
for a deterministic GEPA replay that preserves router accuracy while
removing unnecessary model calls.

## Combining Modules

These modules can be composed when one execution pattern is not enough:

``` r

# ChainOfThought inside BestOfN
cot <- chain_of_thought("math_problem -> solution")
reliable_cot <- best_of_n(cot, N = 3, reward_fn = math_checker)

# Refine with CoT
cot_with_feedback <- module(
  with_reasoning(signature("question, feedback -> answer"))
)
refined_cot <- refine(cot_with_feedback, N = 3, reward_fn = quality_score)

# MCC already uses CoT internally by default
```

## Optimization Support

Wrapper modules retain their underlying optimizable predictors. RLM
exposes separate `generate_action` and `extract` children, but optimizer
support is deliberately explicit:

| Optimizer | RLM support |
|----|----|
| [`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md) | Tunes both child predictors with end-to-end feedback; [`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md) can derive feedback from the bounded RLM trajectory |
| [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md) / [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) | Discovers and applies both graph children |
| [`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md) | Tunes child instructions only when `max_bootstrapped_demos = 0L` |
| [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md) / [`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md) | Programs containing Flex or an RLM are rejected; use GEPA, or instruction-only MIPROv2 for an RLM graph |
| [`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md) | Programs containing an RLM are rejected because root examples do not match child signatures |

Nested MIPRO demo bootstrapping fails with an actionable typed error
until RLM collects predictor-local child evidence. This avoids attaching
task-level demos to incompatible `state -> ...` predictors.

``` r

# Grid search over wrapper parameters
wrapper <- best_of_n(qa, N = 3)
wrapper$optimize_grid(
  data = dev_data,
  metric = metric_exact_match(),
  parameters = list(
    N = c(3, 5, 7),
    threshold = c(0.8, 0.9, 1.0)
  )
)

# Teleprompter compilation
tp <- LabeledFewShot(k = 4)
compiled <- compile(tp, wrapper, trainset)
```

## Performance Considerations

### Token Usage

Advanced modules trade additional calls for reasoning, retries,
comparison, or exploration. The actual cost depends on early stopping,
provider behavior, trajectory length, and recursive queries. Set
explicit iteration and call budgets, then inspect returned metadata
rather than relying on a fixed multiplier.

### Cost Tracking

Structured results expose the usage and cost metadata available for the
module:

``` r

result <- run(
  mcc,
  question = "Test",
  .llm = llm,
  .return_format = "structured"
)
result$metadata$cost
result$metadata$total_tokens
```

## Summary

These modules cover distinct execution strategies:

| Module | Best For | Trade-off |
|----|----|----|
| [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md) | Complex reasoning, math, logic | Longer model output |
| [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md) | High-variance tasks, critical outputs | Additional candidate calls |
| [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md) | Tasks with clear failure modes | Iterative feedback calls |
| [`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md) | Complex analysis, multiple valid approaches | Candidate and comparison calls |
| [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md) | Exact computation, data analysis | Code execution overhead |
| [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md) | Tasks needing both tools AND computation | Agent loop overhead |
| [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) | Adaptive exploration of large or irregular R objects | Experimental; iterative calls and an explicit execution boundary |
| [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) | Optimizing the choice among predictors, R logic, and tools | Experimental; executable source requires a sandbox |

**Getting started:** - Start with **ChainOfThought** for complex
reasoning tasks - Add **BestOfN** when you need reliability - Use
**ProgramOfThought** for exact computation (math, statistics) - Use
**CodeAct** when you need tools AND code execution together - Use
**RLM** when the exploration path is unknown for this input - Use
**Flex** when the implementation strategy itself is the experiment

## Further Reading

**Tutorials:** - [Improving with
Examples](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md)
— Learn few-shot prompting - [Finding Best
Configuration](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md)
— Grid search optimization - [Investigate a Release Regression with an
RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.md)
— Explore a deterministic large object

**How-to Guides:** - [Compile &
Optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)
— Full optimization workflow with advanced modules - [Build RAG
Pipelines](https://jameshwade.github.io/dsprrr/articles/rag-workflows.md)
— Use modules in retrieval workflows

**Concepts:** - [Understanding Signatures &
Modules](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)
— S7 vs R6 design choices - [How Optimization
Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)
— Teleprompter theory - [How the RLM
Works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.md) —
Lifecycle, replay, typed submission, and runner boundaries

**Reference:** - [Quick
Reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md) —
Syntax and patterns at a glance
