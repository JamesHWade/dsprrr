# Experimental Flex Module

`flex()` creates an experimental module whose implementation is itself
an optimization parameter. Its safe default is a canonical, declarative
JSON graph. An opt-in `source_format = "r"` mode accepts complete R
source and evaluates it only in a fresh runner returned by
`interpreter_factory`.

Version 1 sources contain `schema_version`, an ordered `steps` array,
and an `outputs` object. Each step has a safe, unique `name`, a
`primitive` of `"predict"` or `"chain_of_thought"`, a `signature`
(`"$outer"` or DSPy string notation), an optional `instructions` string,
and an `inputs` object. Input references use `"$input.<name>"` or
`"$step.<earlier-step>.<field>"`. `outputs` maps every outer output
field to one of the same reference forms. Sources are type checked
before binding. Flex v1 supports string, number, integer, boolean, enum,
array, and non-empty object signature types. Opaque `TypeJsonSchema`
values and empty objects are rejected because their interfaces cannot be
checked safely by this compiler.

Executable sources use a deliberately small guest DSL: `Predict`,
`ChainOfThought`, `ReAct`, `ReActV2`, `RLM`, `CodeAct`,
`ProgramOfThought`, `Prediction`, `Tool`, and explicitly supplied named
tools. Predictor and tool calls cross a versioned JSON boundary and run
on the host; optimizer-authored source is never evaluated by the host R
session. Guest bindings have a separate lexical environment from bridge
state. Executable mode requires an explicit zero-argument interpreter
factory and, by default, a runner that advertises an enforced sandbox.

When `module_src` is `NULL`, the baseline is one `Predict` call (or one
`RLM` call when executable mode has tools). `$bind()` and
`$apply_optimization_params()` validate new source transactionally, so
an invalid candidate cannot replace the active implementation. The
source is available through the read-only `$module_src` active binding.

GEPA treats each Flex source as one component. Candidate programs are
explored from the union of validation-example winners, while component
selection and lineage-aware merges decide what to mutate. Invalid source
candidates are auditable but cannot replace the active implementation.

Token streaming remains unsupported because Flex creates predictors at
run time. Dataset concurrency is currently available for declarative
zero- and one-step sources; executable and multi-step sources fail
before provider work when a concurrent backend is requested.

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
  max_tool_calls = 100L
)
```

## Arguments

- signature:

  A
  [Signature](https://jameshwade.github.io/dsprrr/reference/signature.md)
  object or DSPy-style signature string.

- module_src:

  A complete Flex source string, or `NULL` for a baseline.

- max_predictor_calls:

  Maximum number of predictor calls allowed in one invocation, or `NULL`
  for no limit. Declarative sources are also checked against this bound
  before they are installed.

- config:

  Optional module configuration passed to each fresh predictor.

- chat:

  Optional ellmer `Chat` used unless `.llm` is supplied at run time.

- tools:

  Named host functions or ellmer ToolDef objects exposed only to
  executable Flex source.

- interpreter_factory:

  Zero-argument factory returning a fresh code runner for every
  executable Flex invocation. Required when `source_format = "r"`; not
  accepted for declarative JSON.

- source_format:

  Source language: `"json"`, `"r"`, or `"auto"`. Auto selects R when
  tools or a factory are supplied, JSON for JSON-looking source and the
  safe JSON baseline otherwise.

- require_sandbox:

  Whether executable mode must reject runners that do not advertise an
  enforced sandbox. Keep the default for generated or otherwise
  untrusted source.

- max_tool_calls:

  Maximum number of direct host-tool calls allowed in one executable
  invocation, or `NULL` for no limit. The safe default is 100.

## Value

An experimental `FlexModule`.

## Examples

``` r
program <- flex("question -> answer")
#> Warning: `flex()` is experimental and its module source schema may change
#> ℹ The default source is declarative JSON; executable R source requires an
#>   explicit interpreter factory.
program$module_src
#> [1] "{\"schema_version\":1,\"steps\":[{\"name\":\"predict\",\"primitive\":\"predict\",\"signature\":\"$outer\",\"inputs\":{\"question\":\"$input.question\"}}],\"outputs\":{\"answer\":\"$step.predict.answer\"}}"

if (FALSE) { # \dontrun{
result <- run(program, question = "Why is the sky blue?", .llm = llm)
} # }
```
