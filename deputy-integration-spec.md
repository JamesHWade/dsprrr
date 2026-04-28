# dsprrr + deputy Integration Spec and Demonstration Plan

Version: 0.2  
Date: April 27, 2026  
Audience: maintainers, contributors, vignette authors, and early users of `dsprrr` and `deputy`

## 1. Executive summary

`dsprrr` and `deputy` should be integrated as complementary layers in an R-native LLM application stack.

`dsprrr` should remain the layer for declarative LLM skills: signatures, modules, pipelines, evaluation, tracing, and optimization.

`deputy` should remain the runtime layer for agentic execution: tools, permissions, hooks, streaming, sessions, structured output, CLI workflows, and multi-agent orchestration.

The integration should be built around `ellmer`, not around direct coupling between the two packages. The central design principle is:

> Use `ellmer` as the interoperability contract. Let `dsprrr` produce optimized, typed LLM skills. Let `deputy` deploy those skills inside governed agents. Let each package remain useful independently.

The most compelling integrated workflow is:

1. Define an LLM skill in `dsprrr` using a signature.
2. Evaluate and optimize that skill with `dsprrr`.
3. Convert the optimized module into an `ellmer` tool.
4. Register that tool with a `deputy` agent.
5. Run the agent under `deputy` permissions, hooks, sessions, and streaming.
6. Optionally wrap the full `deputy` agent back into a `dsprrr` module for evaluation and regression testing.

The integration should be implemented in phases. Phase 1 formalizes what is already nearly possible: use `deputy` tools inside `dsprrr` modules, and use `dsprrr` modules as `deputy` tools. Later phases add full-agent evaluation, signature-to-schema conversion, and richer trace/event bridging.

## 2. Package roles

### 2.1 `dsprrr`

`dsprrr` is the development, measurement, and optimization layer. Its responsibilities are:

- Define typed LLM tasks with signatures.
- Build reusable modules such as `predict`, `react`, `chain_of_thought`, `multichain`, `program_of_thought`, and related module types.
- Compose modules into pipelines.
- Evaluate modules on datasets with metrics.
- Optimize prompts, demonstrations, and configuration using teleprompters such as `LabeledFewShot`, `MIPROv2`, `GEPA`, and related optimizers.
- Trace module behavior across multi-step programs.
- Export or pin optimized module configurations for reuse.

In the integrated system, `dsprrr` should be the place where users ask, "Does this LLM skill work, how well does it work, and how can I improve it?"

### 2.2 `deputy`

`deputy` is the governed agent runtime. Its responsibilities are:

- Wrap an `ellmer` chat object in an agent runtime.
- Register and execute tools.
- Enforce permissions for tool use, code execution, file access, and budget/turn limits.
- Provide hooks around agent behavior.
- Stream events and final responses.
- Persist and resume sessions.
- Support structured output.
- Coordinate multi-agent workflows.
- Expose an R-native API.

In the integrated system, `deputy` should be the place where users ask, "Can this LLM system safely do work in the world with tools, sessions, and human-visible execution?"

### 2.3 `ellmer`

`ellmer` should remain the common substrate:

- Chat objects.
- Provider abstraction.
- Tool definitions via `ellmer::tool()` (which accepts `name`, `description`, `arguments`, and `annotations`).
- Tool schemas and type objects.
- Structured output types.
- Token/cost tracking where available.

The integration should not introduce a hard circular dependency. `dsprrr` should not require `deputy`; `deputy` should only suggest `dsprrr` for optional integration helpers.

## 3. Target integrated user stories

The integration should enable four core user stories.

### 3.1 Optimized `dsprrr` skill deployed as a `deputy` tool

A user defines a task-specific skill in `dsprrr`, evaluates it on examples, optimizes it, and deploys it as a tool inside a `deputy` agent.

```r
ticket_sig <- dsprrr::signature(
  "ticket -> category: enum('bug', 'billing', 'feature', 'other')"
)

triage <- dsprrr::module(ticket_sig, chat = ellmer::chat("openai/gpt-4o-mini"))

triage_opt <- dsprrr::compile(
  dsprrr::LabeledFewShot(k = 4),
  triage,
  trainset = train_tickets
)

agent <- deputy::Agent$new(
  chat = ellmer::chat("openai/gpt-4o"),
  tools = list(
    deputy::tool_dsprrr_module(
      triage_opt,
      name = "triage_ticket",
      description = "Classify a support ticket by category."
    )
  ),
  permissions = deputy::permissions_readonly()
)

result <- agent$run_sync("Triage this ticket: Customer cannot access invoice history.")
```

The value is that agent tools become tested and optimized capabilities rather than one-off prompts embedded in an agent system prompt.

### 3.2 `deputy` tools used inside a `dsprrr` tool-using module

A user builds a `dsprrr` `react` module that can use `deputy` tool bundles. This allows tool-using behavior to be evaluated, traced, and optimized in `dsprrr`.

```r
mod <- dsprrr::module(
  dsprrr::signature("question -> answer"),
  type = "react",
  tools = deputy::tools_preset("minimal"),
  chat = ellmer::chat("openai/gpt-4o")
)

result <- dsprrr::run(mod, question = "What R files are in this package?")
```

The value is that users can benchmark and improve tool-use prompting before deploying to an agent runtime.

### 3.3 Full `deputy` agent evaluated as a `dsprrr` module

A user wraps a complete `deputy` agent as a `dsprrr` module and evaluates it on a benchmark dataset.

```r
agent <- deputy::Agent$new(
  chat = ellmer::chat("openai/gpt-4o"),
  tools = deputy::tools_preset("minimal"),
  permissions = deputy::permissions_readonly()
)

agent_mod <- deputy::agent_as_module(
  agent,
  signature = dsprrr::signature("task -> answer"),
  max_turns = 12
)

score <- dsprrr::evaluate(
  agent_mod,
  data = agent_eval_set,
  metric = function(example, prediction) {
    grepl(example$expected, prediction$answer, fixed = TRUE)
  }
)
```

The value is that agent systems can be regression-tested and compared before deployment.

### 3.4 `dsprrr` signatures as `deputy` structured output schemas

A user defines the output contract once using a `dsprrr` signature, then uses the same contract for `deputy` structured output.

```r
sig <- dsprrr::signature(
  "question -> answer, confidence: number, evidence: array(string)"
)

output_format <- deputy::output_format_from_signature(sig)

result <- agent$run_sync(
  "Answer this question with evidence: ...",
  output_format = output_format
)

# Parsed R object with answer, confidence, and evidence fields
result$structured_output$parsed
```

The value is that `dsprrr` signatures become reusable contracts across both skill optimization and agent runtime output validation.

## 4. Current baseline and gap analysis

### 4.1 What is already working

The integration is already structurally plausible because both packages use `ellmer`.

Known working interop paths (verified against current code):

- `deputy` tools are `ellmer` tools, so they can be passed directly into `dsprrr` tool-using modules today.
- `dsprrr` exports `as_ellmer_tool()`, so a `dsprrr` module can be adapted into an `ellmer` tool and registered with a `deputy` agent today.
- `ellmer::tool()` accepts an `annotations` argument natively; the `as_ellmer_tool()` adapter just needs to pass it through.
- `Module$copy(deep = TRUE)` already exists in the base `Module` class and handles both module state and chat cloning. Deep copy is not a question to be resolved — it is implemented.
- `run_sync()` in deputy accepts `max_turns` and `output_format`, matching the requirements for `agent_as_module()`.
- Both packages have structured-output concepts that share a JSON schema substrate.

### 4.2 Primary gaps

The gap is not fundamental incompatibility. The gap is that compatibility needs to be made explicit, safe, documented, and tested.

The main gaps are:

1. `dsprrr::as_ellmer_tool()` lacks `annotations`, `output`, `copy`, and `error` arguments.
2. `deputy` lacks a convenience wrapper so users can register `dsprrr` modules as tools without thinking about low-level `ellmer` details.
3. `dsprrr` lacks a public extension point (`module_fn()`) for arbitrary callable modules, which blocks full-agent wrapping.
4. `deputy` needs `agent_as_module()` once `module_fn()` exists in `dsprrr`.
5. Neither package has a `signature_to_json_schema()` / `output_format_from_signature()` bridge, though the underlying ellmer types already carry JSON schema information.
6. Permission and trace semantics need to be documented carefully.
7. Tests and vignettes need to demonstrate supported workflows rather than leaving users to infer them.

## 5. Non-goals for the first integration release

The first release should not:

- Make `dsprrr` depend on `deputy`.
- Make `deputy` import `dsprrr`; `Suggests` is sufficient.
- Merge `dsprrr` and `deputy` code-execution systems.
- Force a unified trace/event format.
- Require a live LLM provider for most tests.
- Require users to adopt `deputy` to use `dsprrr`, or vice versa.
- Attempt to optimize every part of a `deputy` agent automatically.
- Hide permission boundaries or imply that `deputy` permissions apply outside a deputy-managed runtime.

## 6. Architecture

### 6.1 Layered architecture

```text
ellmer
  Common chat, provider, type, structured-output, and tool substrate.

    ↑                                  ↑
    |                                  |

dsprrr                             deputy
  Signatures, modules,               Agent runtime, tools,
  pipelines, evaluation,             permissions, hooks,
  optimization, tracing.             sessions, streaming.
```

The bridge consists of small adapter functions:

```text
dsprrr module -> ellmer tool -> deputy agent tool registry

deputy tools -> ellmer tools -> dsprrr react module

deputy agent -> module_fn wrapper -> dsprrr module

dsprrr signature -> JSON schema -> deputy output_format
```

### 6.2 Direction 1: `dsprrr` module as `deputy` tool

```text
User defines dsprrr signature
      ↓
User creates dsprrr module
      ↓
User evaluates and optimizes module
      ↓
dsprrr::as_ellmer_tool() adapts module to ellmer tool
      ↓
deputy::tool_dsprrr_module() provides deputy-friendly wrapper with safe defaults
      ↓
deputy Agent registers tool
      ↓
Agent invokes optimized skill under deputy permissions and hooks
```

### 6.3 Direction 2: `deputy` tools inside `dsprrr`

```text
User creates deputy tool bundle (e.g., tools_preset("minimal"))
      ↓
Tools are passed as ellmer tools to dsprrr::module(..., type = "react")
      ↓
dsprrr evaluates, traces, and optimizes the tool-using module
```

### 6.4 Direction 3: `deputy` agent as `dsprrr` module

```text
User creates deputy Agent
      ↓
deputy::agent_as_module() wraps agent execution using dsprrr::module_fn()
      ↓
dsprrr::evaluate() benchmarks the full agent, cloning the agent per row
```

### 6.5 Direction 4: signature-to-schema bridge

```text
dsprrr::signature("question -> answer, confidence: number")
      ↓
dsprrr::signature_to_json_schema() extracts schema from ellmer output_type
      ↓
deputy::output_format_from_signature() wraps as list(type = "json_schema", schema = ...)
      ↓
result$structured_output$parsed contains the structured R object
```

## 7. Implementation spec

### 7.1 Enhance `dsprrr::as_ellmer_tool()`

#### Purpose

Convert a `dsprrr` module into a robust `ellmer` tool suitable for registration in any `ellmer`-compatible runtime, especially `deputy`.

#### Proposed API

```r
as_ellmer_tool <- function(
  module,
  name = NULL,
  description = NULL,
  .llm = NULL,
  annotations = list(),
  output = c("auto", "json", "text", "raw"),
  copy = c("none", "deep"),
  error = c("reject", "abort", "return")
)
```

#### Arguments

`module`  
A `dsprrr` module (R6 object inheriting from `Module`).

`name`  
Tool name. If omitted, derive from the module name if available; otherwise derive from the signature inputs.

`description`  
Tool description. If omitted, derive from the module signature instructions and input/output fields.

`.llm`  
Optional chat object to pass to `dsprrr::run()`. If omitted, use the module's stored chat or `dsprrr` default behavior.

`annotations`  
An `ellmer` tool annotations list (from `ellmer::tool_annotations()` or a plain list), passed directly to `ellmer::tool(annotations = ...)`. This is the primary hook allowing `deputy` to reason about read-only, destructive, idempotent, and open-world behavior without `dsprrr` importing `deputy`. Default is `list()`, meaning no annotation hints are set.

`output`  
Controls tool-result serialization:

- `"auto"` (default): scalar single-output modules return a scalar/text value; structured multi-field outputs return JSON-like structured values.
- `"json"`: always return stable JSON for the module output.
- `"text"`: return the primary output field coerced to text. If no clear primary field, return compact JSON-like text.
- `"raw"`: return the raw R result, preserving current behavior.

`copy`  
Controls module state sharing:

- `"none"` (default): invoke the supplied module directly. Preserves existing behavior; traces/state may accumulate.
- `"deep"`: invoke a cloned module via `module$copy(deep = TRUE)` so tool calls do not mutate the original module's trace or state. This is already implemented in `Module$copy()` and safe to use for all standard module classes.

`error`  
Controls failure behavior:

- `"reject"` (default): return a structured rejection message that gives an agent a chance to recover and retry.
- `"abort"`: throw the original error.
- `"return"`: return a structured error object as the tool result.

#### Default behavior

These defaults preserve backward compatibility while being safe for most uses:

```r
output = "auto"
copy = "none"
error = "reject"
```

For `deputy::tool_dsprrr_module()`, the defaults are stricter:

```r
copy = "deep"        # Protect optimized module state
error = "reject"
```

#### Tool argument schema

Arguments are generated from the module signature inputs. Rules:

1. Each input field becomes a tool parameter.
2. Input type maps to the closest `ellmer` type (already implemented; no changes needed).
3. Field descriptions come from the signature input description when available.
4. Requiredness follows signature input requirements.

This behavior already works correctly in the current implementation.

#### Tool result serialization

`output = "json"` returns stable JSON with deterministic field order. For example, a module output of:

```r
list(answer = "Formulation A", confidence = 0.82, evidence = c("Higher strength", "Lower drift"))
```

becomes:

```json
{"answer": "Formulation A", "confidence": 0.82, "evidence": ["Higher strength", "Lower drift"]}
```

`output = "text"` returns the primary output field as a string when the signature has a clear primary field; otherwise returns compact JSON-like text.

`output = "auto"` is conservative:
- Single scalar output: return that scalar or text.
- Multiple outputs: return structured JSON-like output.
- Complex nested output: return JSON.

#### Tool annotations

Pass the `annotations` argument directly to `ellmer::tool(annotations = annotations)`. The `ellmer::tool()` function already accepts annotations as its fifth parameter.

Example:

```r
dsprrr::as_ellmer_tool(
  summarize_module,
  name = "summarize_report",
  annotations = ellmer::tool_annotations(
    read_only_hint = TRUE,
    destructive_hint = FALSE,
    open_world_hint = FALSE,
    idempotent_hint = TRUE
  )
)
```

The default `annotations = list()` sets no hints, leaving behavior up to the downstream runtime. This is conservative and safe.

#### Copy behavior

`Module$copy(deep = TRUE)` already exists in `R/module-base.R` and handles both R6 state cloning and chat cloning (with fallback behavior when cloning fails). Use it directly:

```r
if (copy == "deep") {
  working_module <- module$copy(deep = TRUE)
} else {
  working_module <- module
}
```

No "if unsupported, abort" logic is needed for standard module classes.

#### Error behavior

Structured rejection message for `error = "reject"`:

```r
list(
  error = TRUE,
  type = class(err)[1],
  message = conditionMessage(err),
  tool = name
)
```

If `ellmer` adds a `tool_reject()` helper in future versions, prefer it. For now, return the structured list.

#### Acceptance criteria

- Existing `as_ellmer_tool()` usage continues to work.
- Users can pass `annotations` and those annotations reach the resulting `ellmer` tool.
- Tool argument names match signature input names (already working).
- Structured module outputs can be returned as stable JSON.
- `copy = "deep"` prevents trace/state mutation of the original module.
- `error = "reject"` produces a recoverable tool observation.

### 7.2 Add `deputy::tool_dsprrr_module()`

#### Purpose

Provide a deputy-native convenience function for registering optimized `dsprrr` modules as agent tools with safe defaults.

#### Proposed API

```r
tool_dsprrr_module <- function(
  module,
  name = NULL,
  description = NULL,
  chat = NULL,
  annotations = NULL,
  output = c("auto", "json", "text", "raw"),
  copy = c("deep", "none"),
  error = c("reject", "abort", "return")
)
```

Note: `copy` defaults to `"deep"` here (opposite ordering from `as_ellmer_tool()`), protecting the optimized module from state mutation during agent tool calls.

#### Dependency

`deputy` should put `dsprrr` in `Suggests`, not `Imports`.

```r
tool_dsprrr_module <- function(
  module,
  name = NULL,
  description = NULL,
  chat = NULL,
  annotations = NULL,
  output = c("auto", "json", "text", "raw"),
  copy = c("deep", "none"),
  error = c("reject", "abort", "return")
) {
  rlang::check_installed("dsprrr")

  output <- match.arg(output)
  copy <- match.arg(copy)
  error <- match.arg(error)

  if (is.null(annotations)) {
    annotations <- ellmer::tool_annotations(
      read_only_hint = TRUE,
      destructive_hint = FALSE,
      open_world_hint = FALSE,
      idempotent_hint = FALSE
    )
  }

  dsprrr::as_ellmer_tool(
    module = module,
    name = name,
    description = description,
    .llm = chat,
    annotations = annotations,
    output = output,
    copy = copy,
    error = error
  )
}
```

#### Default annotations

Default to read-only. This matches the most common use case: a `dsprrr` module as a reasoning, classification, summarization, extraction, or scoring skill.

Users must override annotations when the module itself can write files, execute code, call external tools, or perform irreversible actions:

```r
riskier_tool <- deputy::tool_dsprrr_module(
  code_module,
  name = "generate_and_run_analysis",
  annotations = ellmer::tool_annotations(
    read_only_hint = FALSE,
    destructive_hint = TRUE,
    open_world_hint = TRUE,
    idempotent_hint = FALSE
  )
)
```

#### Acceptance criteria

- `tool_dsprrr_module()` creates a valid `ellmer` tool from a `dsprrr` module.
- The function errors cleanly if `dsprrr` is not installed.
- The wrapper defaults to safe read-only annotations.
- The wrapper can be used directly in `Agent$new(tools = list(...))`.
- No hard dependency from `deputy` to `dsprrr`.

### 7.3 Add `dsprrr::module_fn()`

#### Purpose

Create a public `dsprrr` extension point for arbitrary callable modules. This enables `deputy::agent_as_module()` and is broadly useful for wrapping non-`dsprrr` systems, custom R functions, retrieval systems, rule engines, and external services.

#### Design decision: R6 subclass

`module_fn()` should create an R6 object that inherits from `Module`. This is the cleanest approach because:

- It gets dispatch through `run()`, `evaluate()`, and `dsprrr` generics for free.
- It participates in the existing `forward()` contract.
- It does not require changes to generic dispatch.
- It can use `Module$copy()` for cloning.

The `FnModule` class overrides only `forward()`, delegating to the user-supplied function.

#### Proposed API

```r
module_fn <- function(
  signature,
  forward,
  chat = NULL,
  name = NULL,
  config = list()
)
```

#### Arguments

`signature`  
A `dsprrr` signature object or signature string.

`forward`  
A function receiving named signature inputs plus optional `.llm` and `...`. Must return a named list matching output fields, or a scalar when the signature has one output field.

`chat`  
Optional default chat object.

`name`  
Optional module name stored in config.

`config`  
Optional configuration metadata.

#### Implementation sketch

```r
FnModule <- R6::R6Class(
  "FnModule",
  inherit = Module,
  private = list(
    .forward_fn = NULL
  ),
  public = list(
    initialize = function(signature, forward_fn, chat = NULL, name = NULL, config = list()) {
      super$initialize(signature = signature, config = config, chat = chat)
      private$.forward_fn <- forward_fn
      if (!is.null(name)) self$config$name <- name
    },
    forward = function(batch, .llm = NULL, trace = TRUE, ...) {
      # Validate inputs against signature
      # Call user-supplied function with named inputs
      # Wrap result in standard trace tibble
      # Record trace if trace = TRUE
    }
  )
)

module_fn <- function(signature, forward, chat = NULL, name = NULL, config = list()) {
  if (is.character(signature)) {
    signature <- dsprrr::signature(signature)
  }
  FnModule$new(
    signature = signature,
    forward_fn = forward,
    chat = chat,
    name = name,
    config = config
  )
}
```

#### Forward function contract

The function receives named input fields from the signature, plus `.llm` and `...`:

```r
forward = function(text, .llm = NULL, ...) {
  list(summary = paste("Summary:", substr(text, 1, 100)))
}
```

Return value must be:
- A named list matching output fields, OR
- A scalar when the signature has exactly one output field.

#### Optimization support in v1

`module_fn()` objects should support `run()` and `evaluate()` in v1. Optimizer support (e.g., `compile()`) is not required for v1. If an optimizer is called on a `FnModule`, it should error clearly with a message explaining that callable modules do not support optimization.

#### Acceptance criteria

- A callable module can be created from a signature and function.
- `run()` validates inputs and outputs according to the signature.
- `evaluate()` can score the callable module on a dataset.
- Metadata from the forward function can be attached to result metadata.
- Users do not need to subclass internal module classes.
- Optimizers error clearly when called on a `FnModule`.

### 7.4 Add `deputy::agent_as_module()`

#### Purpose

Wrap a full `deputy` `Agent` as a `dsprrr` module so it can be evaluated, benchmarked, and regression-tested.

#### Proposed API

```r
agent_as_module <- function(
  agent,
  signature,
  task_template = NULL,
  output_format = NULL,
  clone_agent = TRUE,
  include_events = FALSE,
  include_turns = FALSE,
  max_turns = NULL,
  name = NULL
)
```

#### Arguments

`agent`  
A `deputy::Agent` object.

`signature`  
A `dsprrr` signature object or signature string.

`task_template`  
Optional glue-style template for rendering the agent prompt from signature inputs. If omitted, inputs are rendered deterministically as a labeled block.

`output_format`  
Optional deputy output format list. If omitted and the signature has structured outputs, infer from the signature via `output_format_from_signature()`.

`clone_agent`  
Logical, default `TRUE`. Whether to clone the agent before each `run()` or `evaluate()` call. Cloning prevents turn history from accumulating across evaluation rows, which would contaminate later predictions with context from earlier ones. Set to `FALSE` only when reusing turn history is intentional.

`include_events`  
Whether to include the full list of `AgentEvent` objects in `dsprrr` result metadata. Default `FALSE` to avoid flooding traces.

`include_turns`  
Whether to include full conversation turns in `dsprrr` result metadata. Default `FALSE`.

`max_turns`  
Optional maximum turns override for the wrapped evaluation call. Passed directly to `agent$run_sync()`.

`name`  
Optional module name.

#### Implementation sketch

```r
agent_as_module <- function(
  agent,
  signature,
  task_template = NULL,
  output_format = NULL,
  clone_agent = TRUE,
  include_events = FALSE,
  include_turns = FALSE,
  max_turns = NULL,
  name = NULL
) {
  rlang::check_installed("dsprrr")

  sig <- dsprrr::signature(signature)

  forward <- function(..., .llm = NULL) {
    inputs <- list(...)
    task <- render_agent_task(inputs, task_template)

    fmt <- output_format %||% output_format_from_signature(sig)

    # Clone agent to prevent turn history contamination across eval rows
    working_agent <- if (clone_agent) agent$clone(deep = TRUE) else agent

    result <- working_agent$run_sync(
      task,
      max_turns = max_turns,
      output_format = fmt
    )

    # Extract actual parsed output from structured_output$parsed
    output <- if (!is.null(result$structured_output$parsed)) {
      result$structured_output$parsed
    } else {
      list(answer = result$response)
    }

    metadata <- list(
      deputy = list(
        session_id = result$session_id,
        stop_reason = result$stop_reason,
        duration = result$duration,
        cost = result$cost,
        n_events = length(result$events),
        n_turns = length(result$turns)
      )
    )

    if (include_events) metadata$deputy$events <- result$events
    if (include_turns) metadata$deputy$turns <- result$turns

    # Return in a form compatible with dsprrr result infrastructure
    # Use the pattern from run.R: list with class = "dsprrr_result"
    structure(
      list(output = output, metadata = metadata),
      class = "dsprrr_result"
    )
  }

  dsprrr::module_fn(
    signature = sig,
    forward = forward,
    name = name %||% "deputy_agent"
  )
}
```

#### Agent cloning behavior

This is a critical correctness requirement. Without cloning, the agent's turn history accumulates across evaluation rows. Row 3's response would include context from rows 1 and 2, making evaluation unreliable.

Default `clone_agent = TRUE` prevents this. Users who explicitly want multi-turn context to carry over (e.g., testing multi-step task dependencies) can set `clone_agent = FALSE`.

The `Agent` R6 class inherits from R6 and supports `$clone(deep = TRUE)`.

#### Task template behavior

If `task_template` is provided, use glue-style replacement:

```r
task_template = "Answer using the context.\n\nContext:\n{context}\n\nQuestion:\n{question}"
```

If omitted, render inputs as a labeled block:

```text
context:
{context value}

question:
{question value}
```

#### Output mapping

Rules:

1. If `result$structured_output$parsed` is non-null, use it as the module output. Note: `result$structured_output` is a list with `format`, `raw`, `parsed`, `valid`, `errors`, and `schema_validation_skipped` fields. The actual R object lives at `$parsed`.
2. If the signature has a single output field, map `result$response` to that field.
3. If the signature has multiple output fields and no structured output, map `result$response` to a field named `answer` when it exists; otherwise error with a clear message.

#### Permission behavior

`agent_as_module()` must not bypass `deputy` permissions. The wrapped agent runs exactly as configured, with its tools, permissions, hooks, budgets, and max-turn rules.

#### Acceptance criteria

- A `deputy` agent can be evaluated with `dsprrr::evaluate()`.
- `deputy` permissions are still enforced.
- The agent is cloned per evaluation row by default (`clone_agent = TRUE`).
- `result$structured_output$parsed` is used for structured output extraction.
- Compact `deputy` metadata is included by default.
- Full events and turns are opt-in.
- Agent metadata distinguishes quality failures from policy failures (e.g., `stop_reason = "max_turns"` vs `stop_reason = "complete"`).

### 7.5 Add a signature-to-output-format bridge

#### Purpose

Convert `dsprrr` signatures into JSON schemas or `deputy` output formats.

#### Implementation split

- `dsprrr::signature_to_json_schema()`: lives in `dsprrr` and converts the signature's `output_type` (which is already an ellmer type object carrying JSON schema information) into a plain R list matching JSON Schema structure.
- `deputy::output_format_from_signature()`: thin wrapper that calls `dsprrr::signature_to_json_schema()` and wraps the result.

The implementation is simpler than it appears because `output_type` is already an ellmer type. ellmer types are S7 objects; the JSON schema conversion is a traversal of the ellmer type structure.

#### `dsprrr::signature_to_json_schema()`

```r
signature_to_json_schema <- function(signature) {
  # signature@output_type is already an ellmer type object
  # Traverse the ellmer type structure to produce JSON Schema
  ellmer_type_to_json_schema(signature@output_type)
}

ellmer_type_to_json_schema <- function(type) {
  # Dispatch on type class and produce corresponding JSON Schema
  # type_string -> list(type = "string")
  # type_number -> list(type = "number")
  # type_integer -> list(type = "integer")
  # type_boolean -> list(type = "boolean")
  # type_enum -> list(type = "string", enum = values)
  # type_array -> list(type = "array", items = recurse(items_type))
  # type_object -> list(type = "object", properties = ..., required = ..., additionalProperties = FALSE)
}
```

#### `deputy::output_format_from_signature()`

```r
output_format_from_signature <- function(signature) {
  rlang::check_installed("dsprrr")
  schema <- dsprrr::signature_to_json_schema(signature)
  list(type = "json_schema", schema = schema)
}
```

#### Type mapping

| ellmer type | JSON Schema |
|-------------|-------------|
| `type_string()` | `{"type": "string"}` |
| `type_number()` | `{"type": "number"}` |
| `type_integer()` | `{"type": "integer"}` |
| `type_boolean()` | `{"type": "boolean"}` |
| `type_enum(values)` | `{"type": "string", "enum": values}` |
| `type_array(items)` | `{"type": "array", "items": <recurse>}` |
| `type_object(...)` | `{"type": "object", "properties": {...}, "required": [...], "additionalProperties": false}` |
| Unknown | `{"type": "string"}` with a warning |

#### Required fields

All signature output fields are required in the generated JSON schema unless the signature explicitly marks them optional. Use `additionalProperties = FALSE`.

#### Example

```r
sig <- dsprrr::signature(
  "question -> answer, confidence: number, citations: array(string)"
)
```

Output:

```r
list(
  type = "object",
  properties = list(
    answer = list(type = "string"),
    confidence = list(type = "number"),
    citations = list(type = "array", items = list(type = "string"))
  ),
  required = c("answer", "confidence", "citations"),
  additionalProperties = FALSE
)
```

#### Acceptance criteria

- String, number, integer, boolean, enum, array, and object outputs convert correctly.
- The output can be passed to `agent$run_sync(..., output_format = ...)`.
- `result$structured_output$parsed` contains the expected structured R object.
- Unsupported types warn clearly rather than failing silently.

## 8. Permission and safety spec

### 8.1 Principle

Permission enforcement belongs to the runtime that executes tool calls.

When a `dsprrr` module is used as a `deputy` tool, `deputy` permissions govern whether the tool may be called, based on annotations and deputy policy.

When `deputy` tools are passed directly into a `dsprrr` module, `deputy` permission modes are not automatically active unless those calls are occurring through a deputy-managed agent or a chat object with deputy callbacks installed.

### 8.2 Documentation requirement

Both packages should document this rule prominently:

> Passing `deputy` tool objects directly into a `dsprrr` module gives the module access to those tools as `ellmer` tools. `deputy` permission modes are enforced by the `deputy` agent runtime and its callbacks. If a tool is used outside that runtime, deputy permissions are not active.

### 8.3 Default policy for `dsprrr` modules as deputy tools

The `tool_dsprrr_module()` wrapper defaults to read-only annotations because most wrapped `dsprrr` modules are pure reasoning skills.

```r
ellmer::tool_annotations(
  read_only_hint = TRUE,
  destructive_hint = FALSE,
  open_world_hint = FALSE,
  idempotent_hint = FALSE
)
```

Users must override this for modules with side effects.

### 8.4 Risk categories

| Module type | Annotations |
|-------------|-------------|
| Pure reasoning (classification, summarization, extraction, scoring) | `read_only_hint = TRUE`, `destructive_hint = FALSE`, `open_world_hint = FALSE` |
| File-reading or local retrieval | `read_only_hint = TRUE`, `destructive_hint = FALSE`, `open_world_hint = FALSE` |
| Web-backed retrieval | `read_only_hint = TRUE`, `destructive_hint = FALSE`, `open_world_hint = TRUE` |
| Code-executing (`program_of_thought`, `code_act`) | `read_only_hint = FALSE`, `destructive_hint = TRUE`, `open_world_hint = FALSE` (or `TRUE` if network access) |
| File-writing or external-action | `read_only_hint = FALSE`, `destructive_hint = TRUE`, `open_world_hint = TRUE` |

### 8.5 Code execution

Do not merge code-execution runtimes in v1.

`dsprrr` code-execution modules and `deputy` code tools remain separate surfaces. The first integration should only document how to annotate such modules and when to avoid wrapping them as read-only tools.

## 9. State, trace, and event spec

### 9.1 State boundaries

`dsprrr` modules may contain traces, trials, demos, configuration, optimization state, and prompt history.

`deputy` agents may contain turns, sessions, loaded skills, hooks, permissions, working directories, and cost/budget state.

Adapters must make state-sharing explicit.

### 9.2 `dsprrr` module as deputy tool

Default deputy wrapper behavior uses `copy = "deep"` so a deployed tool call does not accidentally mutate the source optimized module. `Module$copy(deep = TRUE)` handles this correctly for all standard module classes.

Metadata from tool calls should include:

```r
list(
  dsprrr = list(
    module_name = name,
    output_mode = output
  )
)
```

### 9.3 deputy agent as `dsprrr` module

The default metadata summarizes deputy execution compactly using fields from `AgentResult`:

```r
metadata = list(
  deputy = list(
    session_id = result$session_id,         # character or NULL
    stop_reason = result$stop_reason,       # "complete", "max_turns", etc.
    duration = result$duration,             # numeric seconds
    cost = result$cost,                     # list(input, output, cached, total)
    n_events = length(result$events),       # integer
    n_turns = length(result$turns)          # integer
  )
)
```

Full events and turns are opt-in:

```r
agent_as_module(..., include_events = TRUE, include_turns = TRUE)
```

The `stop_reason` field is particularly important for evaluation: it distinguishes quality failures (`"complete"` with a wrong answer) from policy failures (`"max_turns"`, permission denials) so metrics can be interpreted correctly.

### 9.4 No unified trace format in v1

Do not force `dsprrr` traces and `deputy` events into one format. Preserve each system's native observability and add cross-links via compact metadata instead.

## 10. Error-handling spec

### 10.1 `dsprrr` module as tool

Three modes:

- `"reject"` (default): return a structured rejection message allowing the agent to recover.
- `"return"`: return a structured error object as the tool result.
- `"abort"`: throw the original error.

Rejection message format:

```r
list(
  error = TRUE,
  type = class(err)[1],
  message = conditionMessage(err),
  tool = name
)
```

### 10.2 `deputy` agent as module

Agent failures should be mapped into `dsprrr` evaluation failures without losing context:

```r
list(
  deputy = list(
    error = TRUE,
    error_class = class(err),
    error_message = conditionMessage(err),
    session_id = result$session_id %||% NULL,
    stop_reason = result$stop_reason %||% NULL
  )
)
```

Distinguish between:
- Quality failure: agent completed but answer was wrong (score = 0).
- Policy failure: agent stopped due to `max_turns`, permission denial, or budget exhaustion. These should be scorable as failures with appropriate metadata, not as test-suite crashes.

### 10.3 Evaluation behavior

`dsprrr::evaluate()` should treat failed agent runs as scored failures rather than test-suite crashes. If this requires changes to how `evaluate()` handles exceptions from the `forward` function, that should be addressed as part of Phase 5 implementation.

## 11. Dependency and file-layout spec

### 11.1 `dsprrr`

Suggested files:

```text
R/ellmer.R                      # enhanced as_ellmer_tool (existing file)
R/module-fn.R                   # module_fn and FnModule class
R/signature-json-schema.R       # signature_to_json_schema

tests/testthat/test-ellmer-tool-adapter.R
tests/testthat/test-module-fn.R
tests/testthat/test-signature-json-schema.R

vignettes/deputy-tools.Rmd
```

DESCRIPTION changes:
- No dependency on `deputy`.
- `jsonlite` is already in `deputy`'s Suggests; verify it is available or already Imported in `dsprrr` for JSON serialization.

### 11.2 `deputy`

Suggested files:

```text
R/tools-dsprrr.R              # tool_dsprrr_module
R/agent-dsprrr.R              # agent_as_module
R/output-format-dsprrr.R      # output_format_from_signature

tests/testthat/test-tools-dsprrr.R
tests/testthat/test-agent-as-module.R
tests/testthat/test-output-format-dsprrr.R

vignettes/dsprrr-integration.Rmd
vignettes/agent-evaluation.Rmd
```

DESCRIPTION changes:

```text
Suggests:
    dsprrr
```

No hard import.

## 12. Testing plan

### 12.1 Test strategy

Most tests should use mock or fake chat objects, not live provider calls.

Live LLM tests should be optional and skipped on CI unless credentials and environment flags are present.

### 12.2 `dsprrr` tests

1. `as_ellmer_tool()` creates a valid `ellmer` tool.
2. Tool argument names match signature input names.
3. Tool argument descriptions are generated from signature metadata.
4. `annotations` are passed through to the resulting `ellmer` tool.
5. `output = "json"` returns stable JSON for structured outputs.
6. `output = "text"` returns the primary output field when possible.
7. `output = "auto"` behaves sensibly for scalar and structured outputs.
8. `copy = "deep"` prevents original module state mutation.
9. `error = "reject"` returns a structured rejection list.
10. `module_fn()` works with `run()`.
11. `module_fn()` works with `evaluate()`.
12. `module_fn()` errors clearly when an optimizer is called on it.
13. `signature_to_json_schema()` handles string, number, integer, boolean, enum, array, and object outputs.
14. `signature_to_json_schema()` warns on unsupported types rather than silently failing.
15. A `deputy` tool bundle can be passed into a `dsprrr` `react` module. (Skip if `deputy` not installed.)

### 12.3 `deputy` tests

1. `tool_dsprrr_module()` errors cleanly if `dsprrr` is missing.
2. `tool_dsprrr_module()` creates a valid `ellmer` tool when `dsprrr` is present.
3. Default annotations are read-only (`read_only_hint = TRUE`, `destructive_hint = FALSE`).
4. Explicit annotations override defaults.
5. A read-only `dsprrr` tool is allowed under `permissions_readonly()`.
6. A destructive `dsprrr` tool is denied under `permissions_readonly()`.
7. `agent_as_module()` creates an object accepted by `dsprrr::run()`.
8. `agent_as_module()` creates an object accepted by `dsprrr::evaluate()`.
9. `agent_as_module()` clones the agent per call by default.
10. `agent_as_module()` correctly reads `result$structured_output$parsed`, not `result$structured_output`.
11. `agent_as_module()` preserves `deputy` permission enforcement.
12. `agent_as_module()` includes compact `deputy` metadata by default.
13. Full events and turns are included only when requested.
14. `stop_reason` is preserved in metadata for both success and policy-failure cases.
15. `output_format_from_signature()` returns a valid `deputy` output format list.

### 12.4 Integration tests

Run only when both packages are installed:

```r
testthat::skip_if_not_installed("dsprrr")
testthat::skip_if_not_installed("deputy")
```

Scenarios:

- `dsprrr` summarizer module registered and called as a `deputy` tool.
- `deputy` file tools passed into a `dsprrr` `react` module.
- `deputy` agent wrapped as a `dsprrr` module and evaluated on a two-row dataset.
- Signature-to-output-format bridge used in an agent call with mock structured output.
- Two-row evaluation verifies agent is cloned per row (turn history does not contaminate).

## 13. Implementation roadmap

### Phase 1: Make existing interop explicit

Deliverables:
- Documentation showing `deputy` tools inside `dsprrr` modules.
- Documentation showing `dsprrr` modules as `deputy` tools via `as_ellmer_tool()`.
- Minimal tests that prevent regressions.

No major API changes required.

### Phase 2: Harden `dsprrr::as_ellmer_tool()`

Deliverables:
- Add `annotations`, `output`, `copy`, and `error` arguments.
- Implement stable output serialization.
- Implement recoverable error behavior.
- Use `module$copy(deep = TRUE)` for `copy = "deep"` (already implemented in Module).
- Add tests for the adapter.

**Highest-priority implementation step.**

### Phase 3: Add `deputy` convenience wrappers

Deliverables:
- Add `deputy::tool_dsprrr_module()`.
- Default read-only annotations.
- Tests against `deputy` permission modes.
- Deputy vignette for optimized `dsprrr` tools.

### Phase 4: Add callable module support in `dsprrr`

Deliverables:
- Add `dsprrr::module_fn()` via `FnModule` R6 subclass.
- Ensure it works with `run()` and `evaluate()`.
- Clear error when optimizers are called on `FnModule`.
- Tests and documentation.

### Phase 5: Add full-agent wrapping

Deliverables:
- Add `deputy::agent_as_module()`.
- Agent cloning per evaluation row (`clone_agent = TRUE`).
- Correct `structured_output$parsed` extraction.
- `stop_reason` preservation for policy-failure detection.
- Agent-evaluation vignette.

### Phase 6: Add signature-to-schema bridge

Deliverables:
- Add `dsprrr::signature_to_json_schema()`.
- Add `deputy::output_format_from_signature()`.
- Type-mapping tests.
- Use bridge in agent evaluation and structured-output examples.

## 14. Vignettes and examples

### 14.1 Vignette: "Using deputy tools in dsprrr modules"

Repository: `dsprrr`  
File: `vignettes/deputy-tools.Rmd`  
Difficulty: introductory

**Purpose**: Show that `deputy` tool bundles can be passed into a `dsprrr` `react` module because both use `ellmer` tools.

**Demo scenario**: "Answer questions about an R package directory using file-reading tools."

**Key permission caveat**: `deputy` permissions are runtime-level and are not automatically active unless the tool calls are executing through a deputy-managed agent or chat object with deputy callbacks installed.

**Minimal code skeleton**:

```r
mod <- module(
  signature("question -> answer"),
  type = "react",
  tools = deputy::tools_preset("minimal"),
  chat = ellmer::chat("openai/gpt-4o-mini")
)

result <- run(mod, question = "What files are available in this project?")
mod$trace_summary()
```

### 14.2 Vignette: "Deploying optimized dsprrr modules as deputy tools"

Repository: `deputy`  
File: `vignettes/dsprrr-integration.Rmd`  
Difficulty: intermediate

**Purpose**: Show the core flagship workflow: define, evaluate, optimize, and deploy a `dsprrr` module as a `deputy` tool.

**Demo scenario**: "Classify support tickets and let an agent use the optimized classifier as a tool."

```r
ticket_sig <- dsprrr::signature(
  "ticket -> category: enum('bug', 'billing', 'feature', 'other'), urgency: enum('low', 'medium', 'high')"
)

triage <- dsprrr::module(ticket_sig, chat = ellmer::chat("openai/gpt-4o-mini"))

triage_opt <- dsprrr::compile(
  dsprrr::LabeledFewShot(k = 4),
  triage,
  trainset = train_tickets
)

triage_tool <- deputy::tool_dsprrr_module(
  triage_opt,
  name = "triage_ticket",
  description = "Classify a support ticket by category and urgency."
)

agent <- deputy::Agent$new(
  chat = ellmer::chat("openai/gpt-4o"),
  tools = list(triage_tool),
  permissions = deputy::permissions_readonly()
)

agent$run_sync("Triage this ticket: Customer cannot access invoice history.")
```

### 14.3 Vignette: "Evaluating deputy agents with dsprrr"

Repository: `deputy`  
File: `vignettes/agent-evaluation.Rmd`  
Difficulty: intermediate to advanced

**Purpose**: Show how a complete `deputy` agent can be wrapped as a `dsprrr` module and evaluated before deployment.

```r
eval_set <- tibble::tibble(
  task = c(
    "Which files define exported functions?",
    "Does the package include tests?"
  ),
  expected = c("R/", "tests/")
)

agent <- deputy::Agent$new(
  chat = ellmer::chat("openai/gpt-4o-mini"),
  tools = deputy::tools_preset("minimal"),
  permissions = deputy::permissions_readonly()
)

agent_mod <- deputy::agent_as_module(
  agent,
  signature = dsprrr::signature("task -> answer"),
  max_turns = 8
)

results <- dsprrr::evaluate(
  agent_mod,
  data = eval_set,
  metric = function(example, prediction) {
    grepl(example$expected, prediction$answer, fixed = TRUE)
  }
)
```

### 14.4 Vignette: "Structured agent outputs from dsprrr signatures"

Repository: `deputy`  
File: `vignettes/signature-output-format.Rmd`  
Difficulty: intermediate

**Purpose**: Show that users can define an output contract once with a `dsprrr` signature and use it in `deputy` structured output.

```r
review_sig <- dsprrr::signature(
  "document -> summary, risks: array(string), confidence: number"
)

output_format <- deputy::output_format_from_signature(review_sig)

result <- agent$run_sync(
  "Review this technical note: ...",
  output_format = output_format
)

result$structured_output$parsed  # list(summary = ..., risks = ..., confidence = ...)
```

### 14.5 Vignette: "Governed research analyst agent"

Repository: either package; link from both  
File: `vignettes/research-analyst-agent.Rmd`  
Difficulty: advanced

**Purpose**: Demonstrate the most compelling applied use case: a research copilot that reads files, reasons through technical context, uses optimized skills, and operates under permissions.

**Demo scenario**: "Review experiment summaries and identify which formulation looks most promising, with evidence and caveats."

```r
analysis_sig <- dsprrr::signature(
  "question, context -> answer, evidence: array(string), caveats: array(string), confidence: number",
  instructions = "Answer only from the supplied context. Include caveats when evidence is incomplete."
)
```

This is the flagship example because it shows both packages at their best: `dsprrr` provides the optimized technical-analysis skill; `deputy` provides safe access to files and governed session-level execution; the output is structured, evidence-grounded, and auditable; the behavior can be evaluated before deployment.

## 15. Recommended vignette prioritization

| Priority | Title | Demonstrates |
|----------|-------|-------------|
| 1 | Optimized module as deputy tool | Core combined value proposition |
| 2 | Deputy tools in dsprrr react modules | Reverse direction; tool-use benchmarking |
| 3 | Evaluate a deputy agent with dsprrr | Major agent-deployment pain point |
| 4 | Structured output from signatures | Shared contract across packages |
| 5 | Governed research analyst agent | Flagship applied story |

## 16. Open design questions

These questions remain genuinely open and should be resolved during implementation.

1. What is the exact current structure of `ellmer` tool annotations, and how should annotations be introspected in tests? (The `annotations` parameter accepts a list; the exact field names need to be confirmed against the current `ellmer` release.)
2. Should `dsprrr::as_ellmer_tool()` default to `annotations = list()` (no hints) or inferred read-only annotations? Recommend `list()` to stay conservative; read-only default belongs in the deputy wrapper only.
3. Should `tool_dsprrr_module()` default to `output = "auto"` or `output = "json"`? Recommend `"auto"` with documentation noting when to use `"json"`.
4. What helper should construct a `dsprrr` result object with metadata from `module_fn()`? Use the existing `class = "dsprrr_result"` pattern from `R/run.R:239` rather than creating a new public constructor.
5. Should `agent_as_module()` use `agent$clone(deep = TRUE)` or reconstruct a fresh `Agent` for each evaluation row? Prefer `clone(deep = TRUE)` for simplicity; reconstruct only if clone proves unreliable.
6. How should persistent sessions behave during repeated evaluation rows? Default is no persistence across rows (`clone_agent = TRUE`). Document that session persistence is opt-in and incompatible with independent row evaluation.
7. Where should `signature_to_json_schema()` live if it requires access to non-exported signature internals? It should be in `dsprrr` since it needs access to ellmer type internals; `deputy::output_format_from_signature()` is the thin public wrapper.
8. How should permission denials be scored during agent evaluation? Recommend: treat as a scored failure with `stop_reason` preserved in metadata, not as a test-suite crash.

## 17. Suggested default decisions

1. Keep `dsprrr` independent of `deputy`.
2. Put `dsprrr` in `deputy` `Suggests` only.
3. Use `ellmer` tools and schemas as the common contract.
4. Use `FnModule` R6 subclass (inheriting from `Module`) for `module_fn()`.
5. Default `copy = "none"` in `as_ellmer_tool()`, `copy = "deep"` in `tool_dsprrr_module()`.
6. Default `deputy` wrapper annotations to read-only; require explicit override for risky modules.
7. Default `clone_agent = TRUE` in `agent_as_module()`.
8. Use `result$structured_output$parsed` (not `result$structured_output`) for output extraction.
9. Store compact metadata by default; make full event/turn capture opt-in.
10. Avoid merging code execution systems in v1.
11. Prefer mock chat objects in tests.
12. Prioritize documentation that shows safe, read-only examples first.

## 18. Definition of done

The integration is complete for the first major release when:

1. A `dsprrr` module can be converted into a `deputy` tool with annotations, stable output handling, and clear error behavior.
2. A `deputy` tool bundle can be used in a `dsprrr` tool-using module, with permission caveats documented.
3. A full `deputy` agent can be wrapped as a `dsprrr` module and evaluated on a dataset, with agent cloning per row.
4. A `dsprrr` signature can be converted into a `deputy` structured-output format for all common output types.
5. Tests cover module-to-tool, tool-to-module, agent-to-module, permission behavior, structured output, agent cloning, and metadata capture.
6. Vignettes demonstrate all five prioritized workflows.
7. The README or website describes the combined lifecycle clearly.
8. The integration does not introduce hard circular dependencies.
9. Safety and permission semantics are documented honestly, including the caveat that `deputy` permissions are not active outside a deputy-managed runtime.
10. The flagship research or data-analysis example shows why the integration matters in practice.
