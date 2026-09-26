# Flex: a module whose implementation can be optimized

**\[experimental\]**

`flex()` creates a module whose whole implementation, not just its
instructions, can be rewritten by an optimizer. Use it when the best
number, order or kind of model and tool calls is unknown. If the shape
of the program is already clear, use a regular module or an explicit
pipeline.
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md)
optimizes the Flex source; most other optimizers only tune instructions
or demonstrations inside a fixed module.

The implementation is a source string. Declarative JSON source describes
a bounded graph of predictor steps as data. Executable R source contains
a complete `forward()` program, for tasks that need control flow,
computation, predictors created on the fly, or named host tools; it runs
only in a fresh runner from `interpreter_factory`, which must provide an
enforced sandbox by default. With the default `source_format = "auto"`,
the baseline and JSON-looking sources are treated as JSON, and a
factory, tools, or source that does not look like JSON select R.

## Usage

``` r
flex(
  signature,
  module_src = NULL,
  max_predictor_calls = 100L,
  config = list(),
  chat = NULL,
  tools = list(),
  interpreter_factory = NULL,
  source_format = c("auto", "json", "r"),
  require_sandbox = TRUE,
  max_tool_calls = 100L,
  ...
)
```

## Arguments

- signature:

  A signature object created by
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  or a DSPy-style signature string.

- module_src:

  A complete Flex source string, or `NULL` for a baseline.

- max_predictor_calls:

  Maximum number of predictor calls per run (default `100L`), or `NULL`
  for no limit. Declarative sources are checked against it before they
  are installed. Agentic predictors, such as `ReAct`, have their own
  limits for the work they do internally.

- config:

  Optional module configuration passed to each fresh predictor.

- chat:

  Optional ellmer `Chat` used unless `.llm` is supplied at run time.

- tools:

  Named host functions or ellmer ToolDef objects exposed only to
  executable Flex source.

- interpreter_factory:

  A function with no arguments that returns a fresh code runner for
  every run of executable source, such as
  `function() mcp_repl_runner()`. Required for R source; not accepted
  for JSON source.

- source_format:

  Source language: `"auto"` (the default), `"json"` or `"r"`. `"auto"`
  selects R when tools or a factory are supplied, or when a non-`NULL`
  source does not look like JSON; otherwise it selects JSON.

- require_sandbox:

  Whether executable mode must reject runners that do not advertise an
  enforced sandbox. Keep the default for generated or otherwise
  untrusted source.

- max_tool_calls:

  Maximum number of direct host-tool calls per run of executable source
  (default `100L`), or `NULL` for no limit.

- ...:

  Must be empty.

## Value

A Flex module.

## Details

### JSON sources

Version 1 sources contain `schema_version`, an ordered `steps` array and
an `outputs` object. Each step has a unique `name`, a `primitive` of
`"predict"` or `"chain_of_thought"`, a `signature` (`"$outer"` or a
signature string), an optional `instructions` string and an `inputs`
object. Inputs refer to `"$input.<name>"` or
`"$step.<earlier-step>.<field>"`, and `outputs` maps every output field
to one of those references. Sources are type checked before they are
used. Supported field types are string, number, integer, boolean, enum,
array and non-empty object; opaque `TypeJsonSchema` values and empty
objects are rejected because their interfaces cannot be checked.

### R sources

Executable sources can use a small set of constructors: `Predict`,
`ChainOfThought`, `ReAct`, `ReActV2`, `RLM`, `CodeAct`,
`ProgramOfThought`, `Prediction`, `Tool`, and the named `tools` you
supply. Predictor and tool calls cross a versioned JSON boundary and run
in the host; source written by an optimizer is never evaluated in your R
session. The tools you supply run with your permissions, even though the
source runs in a sandbox. After each call across the boundary, the
source runs again from the start with the recorded responses replayed,
so keep its own computation free of side effects and its loops bounded.

### Baseline and binding

With `module_src = NULL`, the baseline is one `Predict` call (or one
`RLM` call for executable mode with tools). `$bind()` and
`$apply_optimization_params()` validate new source before installing it,
so an invalid candidate cannot replace the working implementation. The
current source is available as `$module_src`.

Token streaming is unsupported, because Flex creates predictors at run
time.
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
concurrency works for declarative sources with zero or one step;
executable and multi-step sources fail before any provider call when a
concurrent backend is requested.

## See also

Other program constructors:
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md),
[`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md),
[`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
program <- flex("question -> answer")
#> Warning: `flex()` is experimental and its module source schema may change
#> ℹ The default source is declarative JSON; executable R source requires an
#>   explicit interpreter factory.
cat(program$module_src)
#> {"schema_version":1,"steps":[{"name":"predict","primitive":"predict","signature":"$outer","inputs":{"question":"$input.question"}}],"outputs":{"answer":"$step.predict.answer"}}

if (FALSE) { # \dontrun{
run(
  program,
  question = "Why is the sky blue?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
