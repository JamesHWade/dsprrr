# Structural Optimization with Flex

[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) is an
experimental module whose complete implementation is one optimization
parameter. It has two source modes:

- `source_format = "json"` is the safe default: a bounded, versioned
  graph parsed strictly as data; and
- `source_format = "r"` is opt-in: complete R source evaluated only
  inside a fresh runner returned by an explicit `interpreter_factory`.

The second mode brings deterministic computation, control flow, host
tools, and dynamically selected predictors much closer to DSPy 3.3 Flex.
It does not turn the host R session into an evaluation environment.
Predictor and tool requests cross a versioned JSON bridge, generated
source receives only a small guest DSL in a separate lexical
environment, and typed outputs are checked again on the host.

## Start with the safe baseline

With no `module_src`,
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
creates one Predict step over the outer signature:

``` r

library(dsprrr)

program <- flex("question -> answer")
program$module_src

result <- run(
  program,
  question = "Why does ice float?",
  .llm = ellmer::chat_openai()
)
```

The `module_src` active binding is read-only and canonical. Equivalent
input JSON is normalized to one stable representation, which supports
reliable fingerprints, comparisons, and optimizer candidates.

## Describe a two-step plan

Version 1 JSON source has exactly three top-level fields:

- `schema_version`, currently the number `1`;
- `steps`, an ordered array (which may be empty for an input-only
  program); and
- `outputs`, a mapping for every output in the outer signature.

Each step declares a unique `name`, an allowlisted `primitive`, a
`signature`, and an `inputs` mapping. Optional `instructions` refine the
step. Input and output references may point to an outer input
(`$input.<field>`) or an output from an earlier step
(`$step.<name>.<field>`).

``` r

source <- jsonlite::toJSON(
  list(
    schema_version = 1,
    steps = list(
      list(
        name = "draft",
        primitive = "chain_of_thought",
        signature = "question -> draft",
        instructions = "Work out the relevant causal mechanism.",
        inputs = list(question = "$input.question")
      ),
      list(
        name = "final",
        primitive = "predict",
        signature = "draft -> answer",
        instructions = "Give a concise answer for a general reader.",
        inputs = list(draft = "$step.draft.draft")
      )
    ),
    outputs = list(answer = "$step.final.answer")
  ),
  auto_unbox = TRUE
)

program <- flex(
  "question -> answer",
  module_src = source,
  max_predictor_calls = 4
)
```

Version 1 permits only `predict` and `chain_of_thought`. Step names and
references follow a restricted grammar, references can only point
backwards, and every step-signature input needs exactly one compatible
binding while every outer output needs exactly one compatible mapping. A
plan may reuse or ignore outer inputs, although runtime inputs must
still match the outer signature. `max_predictor_calls` bounds graph size
before execution. Version 1 supports ellmer string, number, integer,
boolean, enum, array, and non-empty object field types. Opaque
`TypeJsonSchema` values and empty objects are rejected explicitly; the
IR does not guess whether an arbitrary JSON Schema is assignable.

## Opt in to executable R source

Executable Flex requires a zero-argument factory, not a shared runner.
Each invocation gets a fresh runner and dsprrr closes it on success,
failure, or interrupt. The default `require_sandbox = TRUE` also
requires that runner’s validated policy to advertise an enforced
sandbox.

``` r

source <- paste(
  "draft <- ChainOfThought('$outer', instructions = 'Solve carefully.')",
  "forward <- function(question) {",
  "  result <- draft(question = question)",
  "  Prediction(answer = result$answer)",
  "}",
  sep = "\n"
)

program <- flex(
  "question -> answer",
  module_src = source,
  interpreter_factory = my_sandboxed_runner,
  source_format = "r"
)
```

The guest DSL exposes `Predict`, `ChainOfThought`, `ReAct`, `ReActV2`,
`RLM`, `CodeAct`, `ProgramOfThought`, `Prediction`, and `Tool`.
Constructors return callable functions. Signatures use `"$outer"` or
ordinary string notation. No adapter, optimizer, package-loading,
provider-configuration, nested Flex, or unrestricted host namespace is
exposed. Plain functions are available as direct host calls and to
`RLM`; `ReAct` and `CodeAct` receive only supplied ellmer ToolDef
objects because provider-facing tools require an explicit description
and argument schema.

Named host tools retain their original closure environments and run on
the host, outside the sandbox:

``` r

offset <- 5L

tool_program <- flex(
  "value: integer -> result: integer",
  module_src = paste(
    "forward <- function(value) {",
    "  Prediction(result = add_offset(value = value))",
    "}",
    sep = "\n"
  ),
  tools = list(add_offset = function(value) value + offset),
  interpreter_factory = my_sandboxed_runner,
  source_format = "r"
)
```

Treat host tools as privileged capabilities: the sandbox restricts
generated source, not what an explicitly supplied host function can do.
Bridge values must be JSON-compatible. `max_predictor_calls` limits
bridged predictor calls; `max_tool_calls` independently bounds direct
host-tool calls and defaults to 100. Neither limits deterministic R
statements. The current portable bridge replays guest source with
immutable prior responses until it reaches a new request or final
output; host predictor and tool side effects occur once, but
deterministic guest statements before a request may run again. Keep
those statements pure and loops explicitly bounded. Structured runner
results carry large control values without stdout truncation. Text-only
runners advertise a finite frame limit and fail with a typed size error
before accepting a partial frame.

## Replace a plan transactionally

Use `bind()` to replace the active source:

``` r

program$bind(source)
program$module_src # The validated, canonical source is now active
```

dsprrr parses, validates, type-checks, and compiles the complete
candidate before changing the module. If validation fails, the previous
source and plan remain active. Direct assignment is rejected:

``` r

program$module_src <- source
# Error: module_src is read-only; use bind()
```

Optimizer infrastructure can use
`program$apply_optimization_params(list(module_src = candidate))` as the
same transactional boundary. The module advertises its graph as an
optimization parameter, but an optimizer only changes `module_src` if
that optimizer explicitly supports structural candidates. Ordinary
prompt optimization does not silently rewrite the plan.

## Search complete programs with GEPA

[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md)
explicitly supports both Flex source modes. For a program that contains
Flex leaves, each Flex source is one complete component alongside
ordinary predictor-instruction components. Source text is never spliced:
dsprrr copies the program, validates and binds each complete source
transactionally, then evaluates it through the selected Flex runtime.

``` r

metric <- metric_exact_match(field = "answer")
optimizer <- GEPA(
  metric = metric,
  population_size = 6L,
  generations = 2L,
  seed = 42L
)

optimized <- compile(
  optimizer,
  program,
  trainset = question_answer_data,
  .llm = llm
)

optimized$module_src
optimized$config$optimizer$component_semantics
```

The dedicated source proposer sees the task objective, root and
component signatures, field descriptions and schemas, source language,
allowed primitives and tools, current complete source, row-aligned
failures, metric feedback, and the predictor and host-tool call limits.
Provider failures propagate instead of being disguised as unchanged
candidates. Invalid source proposals receive aligned failure records for
audit and budget accounting but are never selectable.

DSPy/GEPA’s validation frontier is the union of complete candidate
programs that win on at least one validation example. dsprrr combines
those winners with the multi-metric objective Pareto front when choosing
parents. Components are selected for mutation with round-robin,
budget-atomic all-component, or custom selection; they do not have
separate Pareto frontiers. With a separate `valset`, training rows drive
discovery/reflection and validation rows drive selection, winners, and
retained outputs. dsprrr records stable candidate IDs, immediate parents
and ancestry, aggregate and per-example scores, discovery evaluation
counts, per-example winners, optional best outputs, attempted merge
counts, and common-ancestor three-way merges. Fine-grained checkpoint
resume and GEPA’s cached subsample pre-acceptance gate for merges remain
explicit differences.

Freezing a module removes its components from this search. If the
complete graph has no mutable components, GEPA returns a fresh unchanged
program without calling the proposer or evaluator and records
`skip_reason = "no_mutable_components"` in optimizer metadata.

## Understand the safety boundary

JSON Flex controls what predictors run and how typed values flow between
them. It cannot:

- evaluate R or Python expressions;
- call arbitrary functions, tools, files, or network services;
- construct module classes outside the allowlist;
- reference a later step or create a cycle; or
- exceed its source-size and predictor-call bounds.

Executable Flex deliberately can express control flow and call
allowlisted capabilities, so its safety depends on the selected runner
policy.
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
is a trusted-input subprocess runner and does not satisfy the default
sandbox requirement. Set `require_sandbox = FALSE` only for source you
trust.

Flex runs synchronously. Native dataset concurrency is implemented for
JSON sources with zero or one predictor step. Concurrent executable Flex
and multi-step JSON requests fail before provider work; their per-row
asynchronous execution engine is not implemented yet. Matching
token-stream requests are also rejected because runtime-created
predictors are not statically discoverable. One-shot
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md)
fallback still uses the ordinary `forward()` contract.

Program artifacts persist the source language and sandbox policy.
Executable artifacts also store tools and the factory as registry-backed
runtime values, so restoring them requires the same registry (or
explicit trusted embedding) and never invokes a factory during
serialization or restoration.

## Choose Flex deliberately

Use JSON Flex when the search question is a bounded topology choice,
such as whether a task benefits from an intermediate draft. Use
executable Flex when optimization genuinely needs coupled source edits
across deterministic logic, control flow, tools, and predictors and you
have an appropriate sandbox. Use a regular module when one known
topology plus optimized instructions is enough; use an explicit pipeline
when humans should own and review the topology.

DSPy Flex executes a Python module class and ships a default
Deno/Pyodide interpreter. dsprrr Flex executes a top-level R `forward()`
function and requires an explicit factory; source is not portable
between the two languages. Because both APIs are experimental, persist
`module_src`, runtime bindings, and package versions, then revalidate a
candidate after upgrades.
