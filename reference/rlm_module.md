# Create an RLM module that explores its inputs with R code

**\[experimental\]**

`rlm_module()` creates a recursive language model (RLM) module for
inputs whose useful evidence is too large, irregular or unpredictable to
fit in one prompt. The inputs stay in an R session. In each step the
model writes R code to inspect them, the runner executes it, and the
model decides what to look at next, until it submits an answer.
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md) runs a
one-off investigation.

Use RLM when the inspection path is not known in advance. Use ordinary R
once that path is stable, or
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) when
labeled examples should discover a reusable implementation.

## Usage

``` r
rlm_module(
  signature,
  runner = NULL,
  interpreter_factory = NULL,
  max_iterations = 20L,
  max_llm_calls = 50L,
  max_output_chars = 10000L,
  sub_lm = NULL,
  verbose = FALSE,
  tools = list(),
  config = list(),
  chat = NULL,
  generate_action = NULL,
  extract = NULL,
  ...
)
```

## Arguments

- signature:

  A
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
  object or a signature string. Outputs must use explicit string,
  number, integer, boolean, enum, array or object types.

- runner:

  A persistent code runner you own, such as
  `r_code_runner(persistent = TRUE)`. Its policy must declare
  `persistent = TRUE`, and it must not be used by two calls at the same
  time.

- interpreter_factory:

  A function with no arguments that returns a fresh persistent runner
  for each call, such as `function() mcp_repl_runner()`.

- max_iterations:

  Integer maximum number of code steps before the fallback extraction
  (default `20L`).

- max_llm_calls:

  Integer maximum number of sub-queries (default `50L`). Use `0L` to
  disable them.

- max_output_chars:

  Maximum number of characters of each execution result shown to the
  model (default `10000L`).

- sub_lm:

  Optional ellmer Chat for sub-queries. `NULL` uses the chat of the
  call.

- verbose:

  Whether to print progress (default `FALSE`).

- tools:

  Named list of R functions or ellmer tools that the generated code can
  call. They run in the host process with your permissions. The
  generated code sends a request that dsprrr checks before calling the
  function; arguments are limited to JSON-compatible values (logical,
  integer, double, character, `NULL` and nested lists), and one step may
  make at most 1,000 tool calls. Tool definitions guide the model, but
  the function must still check its arguments.

- config:

  Optional module configuration, such as runtime settings.

- chat:

  Optional ellmer Chat stored on the module.

- generate_action:

  Optional replacement for the predictor that writes the code
  (advanced).

- extract:

  Optional replacement for the fallback extraction predictor (advanced).

- ...:

  Must be empty.

## Value

An RLM module.

## Details

### What the generated code can use

Each input is available under `.context`. Besides ordinary R, the code
can call:

- `SUBMIT(...)` to finish with the output values, by position or by
  name.

- `peek(var, start, end)` to look at a slice of a string (characters) or
  a vector (elements).

- `search(var, pattern)` to search a variable with a regular expression.

- `llm_query(query, context_slice)` and
  `llm_query_batched(queries, slices)` to ask a model a sub-question and
  get the answer back as a value. These count against `max_llm_calls`.

- The functions supplied in `tools`.

Sub-queries and tools run in the host R process, not in the runner: the
generated code pauses, dsprrr handles the request, and the code is
replayed with the response. Variables persist across steps within one
call, so the runner's `policy()` must declare `persistent = TRUE`.
Invalid submissions become errors that the model can repair. If no valid
submission is made in `max_iterations` steps, the `extract` predictor
makes one final typed attempt.

RLM checks string, number, integer, boolean, enum, array and object
outputs. Opaque `TypeJsonSchema` outputs are rejected when the module is
created, because they cannot be checked in the repair loop.

### Runners

Supply exactly one of `runner` and `interpreter_factory`. For
model-written code, prefer a factory that returns a fresh sandboxed
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md).
That transport is bounded and works best for compact, JSON-compatible
inputs; its default sandbox disables network access but allows writes in
the workspace. It needs the suggested mcptools package and Posit's
`mcp-repl` executable. For large data frames or other rich R objects,
`r_code_runner(persistent = TRUE)` can stage the inputs once, but it
runs with your permissions, so use it for trusted input only. `tools`
always run in the host process with your permissions.

A `runner` you supply is reused and never shut down by dsprrr; a runner
from `interpreter_factory` belongs to one call and is shut down when it
ends.
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
and isolated
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
execution work with a factory; token streaming is unavailable. A direct
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) call
stages each input as one variable, whatever its length. For several
investigations, use
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
with list-columns for data frames, vectors, lists or other rich values.

### Optimization and traces

The `generate_action` and `extract` predictors appear in
[`named_modules()`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
so [`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md) can
tune them and
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md)
can tune their instructions (with `max_bootstrapped_demos = 0L`).
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
and
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md)
reject programs that contain an RLM.

The model sees at most `max_output_chars` characters of each execution
result, as a head-and-tail excerpt, and that view is kept in the
returned trajectory. Traces keep hashes and sizes of the inputs, not
their values. Structured metadata counts action, recursive and
extraction calls separately from verified provider calls. A cache hit
counts as zero provider calls, and totals are `NA` when any provider
turn cannot be verified.

## See also

Other program constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md),
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md)

Other code execution:
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md)

## Examples

``` r
# Trusted input only: r_code_runner() runs with your permissions
runner <- r_code_runner(persistent = TRUE)
analyst <- rlm_module("document, question -> answer", runner = runner)
analyst
#> 
#> ── RLMModule 
#> • Signature: document, question -> answer
#> • Max iterations: 20
#> • Max LLM calls: 50
#> • Runner: callr (caller-owned)
#> • Recursive queries: "outer LM"
#> • Custom tools: 0
runner$shutdown()

if (FALSE) { # \dontrun{
# A fresh sandboxed session for each call
analyst <- rlm_module(
  "document, question -> answer",
  interpreter_factory = function() mcp_repl_runner(timeout = 30)
)
run(
  analyst,
  document = "Owner: team-a\nObligation: rotate keys quarterly",
  question = "Which obligations have no owner?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
