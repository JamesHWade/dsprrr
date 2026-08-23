# Optimize a Module's Implementation with Flex

`flex()` creates an experimental module whose implementation can be
optimized. Use it when the best number, order, or kind of model and tool
calls is unknown. If the program shape is already clear, use a regular
module or an explicit pipeline instead.

The default `source_format = "json"` represents a bounded predictor
graph as data. Opt-in `source_format = "r"` accepts a complete R
`forward()` program for tasks that need control flow, deterministic
computation, dynamic predictors, or named host tools. Executable source
runs only in a fresh runner returned by `interpreter_factory` and
requires an enforced sandbox by default.

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

  Maximum number of predictor invocations allowed across the Flex
  bridge, or `NULL` for no limit. Declarative sources are also checked
  against this bound before they are installed. Configure separate
  limits for work performed inside agentic predictors.

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
  tools or a factory are supplied, or when a non-`NULL` source does not
  look like JSON. It selects JSON for JSON-looking source and for the
  default `NULL` baseline.

- require_sandbox:

  Whether executable mode must reject runners that do not advertise an
  enforced sandbox. Keep the default for generated or otherwise
  untrusted source.

- max_tool_calls:

  Maximum number of direct host-tool calls allowed in one executable
  invocation, or `NULL` for no limit. Defaults to 100.

- ...:

  Must be empty. Flex accepts only its documented arguments.

## Value

An experimental `FlexModule`.

## Details

Most teleprompters optimize instructions or demonstrations inside a
fixed module.
[GEPA](https://jameshwade.github.io/dsprrr/reference/GEPA.md) can also
search each Flex module's complete `module_src`. Invalid candidates
remain auditable but cannot replace the active program.

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

Executable sources receive a small guest DSL: `Predict`,
`ChainOfThought`, `ReAct`, `ReActV2`, `RLM`, `CodeAct`,
`ProgramOfThought`, `Prediction`, `Tool`, and explicitly supplied named
tools. Predictor and tool calls cross a versioned JSON boundary and run
on the host; optimizer-authored source is never evaluated by the host R
session. Guest bindings have a separate lexical environment from bridge
state. Supplied tools are privileged host capabilities even though
generated source runs in a sandbox.

After each bridged request, executable source runs again from the
beginning with recorded host responses replayed. Keep guest-side
computation pure and loops bounded because guest side effects can
repeat.

When `module_src` is `NULL`, the baseline is one `Predict` call (or one
`RLM` call when executable mode has tools). `$bind()` and
`$apply_optimization_params()` validate new source transactionally, so
an invalid candidate cannot replace the active implementation. The
source is available through the read-only `$module_src` active binding.

Token streaming remains unsupported because Flex creates predictors at
run time. Dataset concurrency is currently available for declarative
zero- and one-step sources; executable and multi-step sources fail
before provider work when a concurrent backend is requested.

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
