# Create a CodeAct agent that calls tools and runs R code

`code_act()` creates an agent module that works on a task step by step.
In each step the model either calls one of your tools or writes R code,
which `runner` executes; it sees the result and continues until it can
give the final answer. Use it when a task needs both external tools and
computation. Run it with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

## Usage

``` r
code_act(
  signature,
  tools = list(),
  runner = NULL,
  interpreter_factory = NULL,
  max_iterations = 10L,
  config = list(),
  chat = NULL,
  ...
)
```

## Arguments

- signature:

  A
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
  object or a signature string such as `"question -> answer"`.

- tools:

  A list of ellmer tools created with
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html).
  Non-empty list names become the tool names; unnamed elements keep
  their own names. Names may contain only letters, numbers, hyphens and
  underscores.

- runner:

  A code runner you own, such as
  [`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md).
  It is reused across calls and never shut down by dsprrr.

- interpreter_factory:

  A function with no arguments that returns a fresh runner for each
  call. Supply exactly one of `runner` and `interpreter_factory`.

- max_iterations:

  Integer maximum number of agent steps, and of tool calls within one
  call (default `10L`). Exceeding the tool-call limit raises a
  `dsprrr_codeact_iteration_limit` error.

- config:

  Optional module configuration, such as runtime settings.

- chat:

  Optional ellmer Chat stored on the module.

- ...:

  Must be empty.

## Value

A CodeAct module.

## Details

CodeAct extends the ReAct pattern of
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) with
a built-in `execute_r_code` tool. The name `execute_r_code` is reserved.

Code runs only through the runtime you supply, as either `runner` or
`interpreter_factory`; see
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
for how each is owned and shut down.
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
runs code in a separate process with your permissions and is not a
sandbox. For untrusted input, use a sandboxed runner such as
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md);
`runner$policy()` shows what a runner enforces. A stateful `runner` must
not be used by two calls at the same time.

[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
supports CodeAct with an `interpreter_factory`, in a separate mirai
process, but rejects a caller-owned `runner`. Token streaming with
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
or the module's `$stream()` method is unavailable, because it would
bypass code execution.
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
runs the module as one non-streaming call and rejects requests for token
streaming.

## See also

Other program constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md),
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

Other code execution:
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
search_tool <- ellmer::tool(
  function(query) paste("No results for", query),
  description = "Search the product catalog",
  arguments = list(query = ellmer::type_string("Search terms")),
  name = "search"
)

agent <- code_act(
  "question -> answer",
  tools = list(search = search_tool),
  runner = r_code_runner(timeout = 30)
)
agent
#> 
#> ── CodeActModule 
#> • Signature: question -> answer
#> • Max iterations: 10
#> • User tools: "search"
#> • Runner: callr (caller-owned)

if (FALSE) { # \dontrun{
run(
  agent,
  question = "What is 10% of 2,450?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
