# dsprrr vs. DSPy: Feature Comparison

dsprrr is an R implementation of [DSPy](https://dspy.ai)’s programming
model, built on [ellmer](https://ellmer.tidyverse.org) and tidyverse
conventions. If you know DSPy, this page tells you what carries over,
what is different, and what is not (yet) available.

## Version baseline

This comparison was checked against DSPy **3.3.0**, released on
2026-08-03. That release introduces experimental `Flex` and `ReActV2`
modules, advances an experimental typed provider-neutral
`LMRequest -> LMResponse` migration path, and hardens errors, execution
limits, and serialization. See the [DSPy 3.3.0 release
notes](https://github.com/stanfordnlp/dspy/releases/tag/3.3.0).

dsprrr adopts the durable contracts that fit R and ellmer. It does not
present an existing prompt optimizer as equivalent to a new DSPy feature
when the execution or safety model differs.

dsprrr is not a line-by-line port. It follows DSPy’s concepts —
signatures, modules, metrics, and optimizers (“teleprompters”) — while
embracing R idioms: tibbles in and out, S7/R6 objects, and ellmer for
all provider communication.

## Modules

| DSPy | dsprrr | Notes |
|----|----|----|
| `dspy.Predict` | `module(sig, type = "predict")` | Core predictor |
| `dspy.ChainOfThought` | [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md), [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md) | Implemented as signature transforms |
| `dspy.ReAct` / experimental `ReActV2` | `module(sig, type = "react")` | Native ellmer turn history, tool-call IDs, parallel calls per assistant turn, enforced iteration limit, then structured finalization; behaviorally aligned, not a port of the Python class |
| `dspy.ProgramOfThought` | [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md) | Generates and executes **R** code (not Python); the configured runner is retained, not created fresh per invocation |
| `dspy.CodeAct` | [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md) | Hybrid tools + R code execution with an enforced inner tool-call limit; the built-in runner is trusted-input-only, and sandboxed backends can implement the runner protocol. The configured runner is retained, not created fresh per invocation |
| `dspy.BestOfN` | [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md) | Reward-function-guided retries |
| `dspy.Refine` | [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md) | Retries with LLM-generated feedback |
| `dspy.MultiChainComparison` | [`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md) |  |
| `dspy.RLM` | [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) | Recursive language models over an R REPL; accepts the 3.3 `max_iters` spelling, validates tool names, inputs, and sub-LM text responses strictly, and authenticates submit/query frames per invocation. mcp-repl control frames are bounded for inline transport and compacted responses fail closed. The configured runner is retained, not created fresh per invocation |
| `dspy.Parallel` / `Module.batch` | [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md), `run(..., .parallel = TRUE)` | Batch over a data frame; heterogeneous (module, example) fan-out is not yet a dedicated module |
| `dspy.majority` | [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble_module.md) with [`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md) | Plus [`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md), [`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md) |
| `dspy.KNN` | `KNNFewShot` teleprompter / KNN module | Bring-your-own vectorizer (e.g., `ragnar::embed_openai()`) |
| Retrieval (custom functions) | [`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md) + ragnar | First-class ragnar retriever integration |

One notable difference: DSPy 3.0 *removed* `dspy.Assert`/`dspy.Suggest`
in favor of `BestOfN`/`Refine`. dsprrr keeps both styles: declarative
assertions with retry/backtracking
([`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md),
[`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md),
[`suggest_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md))
*and* the
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md)/[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md)
wrappers. If you prefer the modern DSPy style, use the wrappers; use
assertions when you want declarative output contracts with automatic
feedback injection.

## Optimizers (teleprompters)

| DSPy | dsprrr | Fidelity notes |
|----|----|----|
| `LabeledFewShot` | `LabeledFewShot` | Equivalent |
| `BootstrapFewShot` | `BootstrapFewShot` | Equivalent; compiles pipelines jointly (demos for every step harvested from end-to-end traces) |
| `BootstrapFewShotWithRandomSearch` | `BootstrapFewShotWithRandomSearch` | Equivalent |
| `MIPROv2` | `MIPROv2` | Discrete Bayesian optimization with UCB over instruction + demo candidates |
| `SIMBA` | `SIMBA` | Adapted: hard-example mining + LLM-generated rules; simplified vs. the full introspective algorithm |
| `GEPA` | `GEPA` | Adapted (“GEPA-lite”): reflective mutation + Pareto selection; supports feedback metrics via [`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md); no per-component selection or inference-time search yet |
| `COPRO` | `COPRO` | Equivalent (coordinate ascent over instructions) |
| `KNNFewShot` | `KNNFewShot` | Equivalent |
| `Ensemble` | `Ensemble` | Equivalent |
| `BetterTogether` | `BetterTogether` | Chains prompt optimizers via strategy strings; does **not** alternate prompt/weight optimization (no finetuning backend) |
| Experimental `Flex` | — | No analogue. dsprrr’s agentic harnesses may edit validated instruction and template fields, but they do not optimize arbitrary R source, module structure, or graph topology |
| `BootstrapFinetune` | — | Not implemented (planned); dsprrr currently optimizes prompts, not weights |
| `GRPO` (RL via Arbor) | — | Not implemented |
| `BootstrapFewShotWithOptuna`, `AvatarOptimizer`, `InferRules` | — | Niche/legacy in DSPy; not planned |
| — | `GridSearchTeleprompter`, [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) | dsprrr addition: tidymodels-style grid search over module parameters |
| — | `Omni` | dsprrr addition: independent best-of exploration plus a fresh continuation optimizer, with common validation scoring and optional mirai concurrency |
| — | `AutoResearch` | dsprrr addition: persistent research-agent loop over validated, jointly editable module snapshots with sandboxed R analysis |
| — | `MetaHarness` | dsprrr addition: fresh batch proposers plus host-owned frontier selection, lineage, budgets, and checkpoint resume |

### GEPA feedback metrics

DSPy’s GEPA expects metrics that return a score *and textual feedback*.
dsprrr supports the same protocol:

``` r

metric <- metric_with_feedback(
  function(prediction, expected) {
    if (identical(prediction$answer, expected)) {
      list(score = 1, feedback = "Correct.")
    } else {
      list(
        score = 0,
        feedback = paste("Wrong: expected", expected, "- check the arithmetic.")
      )
    }
  },
  field = "answer"
)

tp <- GEPA(metric = metric, generations = 5L)
compiled <- compile(tp, mod, trainset, .llm = llm)
```

The feedback for failed examples is injected into GEPA’s reflection
prompt, so the reflection LLM learns *why* outputs failed, not just that
they did.

### Trace-aware metrics

DSPy 3.3 makes execution traces available to metrics used by reflective
optimization. dsprrr provides the same durable capability through
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md).
The wrapped function receives the prediction, expected row, and a stable
trace envelope:

``` r

metric <- metric_with_trace(
  function(prediction, expected, program_trace) {
    correct <- identical(prediction$answer, expected$answer)
    list(
      score = as.numeric(correct),
      feedback = paste(
        program_trace$status,
        "with",
        length(program_trace$events),
        "trace events"
      )
    )
  },
  field = "answer"
)

result <- evaluate(mod, testset, metric, .llm = llm)
result$traces[[1]][c("row_id", "epoch", "status")]
```

Each trace contains `row_id`, `epoch`, `status`, ordered `events`, and
module `metadata`. With repeated evaluation, `result$epoch_traces`
preserves the row-aligned traces for every epoch. Trace events may
contain prompts, inputs, and model responses, so handle them as
potentially sensitive data.

## Signatures and types

| DSPy | dsprrr |
|----|----|
| `"question -> answer: int"` string signatures | `signature("question -> answer: integer")` |
| Class-based signatures with `InputField`/`OutputField` | `signature(inputs = list(input(...)), output_type = ...)` |
| `Signature.with_instructions()` / `Signature.append_instructions()` | [`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/signature-transforms.md) / [`append_instructions()`](https://jameshwade.github.io/dsprrr/reference/signature-transforms.md); both return a new signature without mutating the original |
| Pydantic-typed outputs | ellmer type objects ([`type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html), [`type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html), [`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html), [`type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html)) |
| `dspy.Image`, `dspy.Audio`, `dspy.File` | ellmer `Content` objects (images, PDFs) passed as inputs |
| `dspy.History` | Native ellmer turns preserved in ReAct metadata and traces; not a signature type |
| `dspy.Tool`, `dspy.ToolCalls`, `ToolCallResults` | ellmer `ToolDef`, `ContentToolRequest`, and `ContentToolResult`; IDs remain attached to native turns |
| `dspy.Reasoning` (native reasoning traces) | Not yet first-class; [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md) adds a prompted reasoning field |

## Programs and composition

DSPy composes programs as Python classes with multiple predictors.
dsprrr composes pipelines:

``` r

program <- mod_retrieve %>>%
  map_inputs(mod_answer, documents = "context") %>>%
  mod_format
```

`BootstrapFewShot` compiles pipelines **jointly**, like DSPy: the
teacher pipeline runs end-to-end, final outputs are scored, and each
step harvests demonstrations from passing traces. Other teleprompters
currently optimize a pipeline’s steps individually (instruction-level
optimizers operate on single modules).

## Infrastructure

| Capability | DSPy | dsprrr |
|----|----|----|
| LM client | `dspy.LM`; experimental typed `LMRequest -> LMResponse` migration boundary in 3.3 | Provider-neutral ellmer `Chat`; `build_module_request()` normalizes prompt/content input, but a complete package-wide invocation record is still planned |
| Configuration | `dspy.configure()` / `dspy.context()` | [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md), [`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md), [`local_lm()`](https://jameshwade.github.io/dsprrr/reference/local_lm.md) |
| Caching | Two-tier memory + disk | Two-tier memory + disk ([`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md)) |
| Async | `acall`/`aforward`, `asyncify` | [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md) with promises |
| Streaming | `streamify()` + `StreamListener` | [`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md) + [`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md); token streaming for single string fields, status events per pipeline step |
| Usage tracking | `track_usage` | [`get_tokens()`](https://jameshwade.github.io/dsprrr/reference/get_tokens.md), [`get_cost()`](https://jameshwade.github.io/dsprrr/reference/get_cost.md), [`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md) |
| Parallel evaluation | `Evaluate(num_threads = ...)` | `evaluate(.parallel = TRUE)` via mirai or ellmer’s native parallelism |
| Saving programs | `save`/`load`; sanitized LM state and explicit unsafe-class opt-in in 3.3 | Versioned whole-program artifacts via [`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md) / [`load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md) or pins, with registry-backed runtime IDs and explicit trusted opt-in |
| Observability | MLflow autolog, OpenTelemetry callbacks | Traces tibble, [`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md), [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md); package-level OpenTelemetry spans are planned on top of ellmer |
| Adapters (Chat/JSON/XML/TwoStep/BAML) | Yes | No adapter layer; ellmer’s `chat_structured()` handles structured output |
| Evaluation framework | `dspy.Evaluate`, including trace-aware metrics | [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md), [`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md), [`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md), plus **vitals** integration |

## What dsprrr has that DSPy doesn’t

- **tidymodels integration**: use modules as parsnip engines, tune with
  dials parameters (`temperature`, `top_p`, `reasoning_effort`).
- **vitals integration**: bridge modules and metrics to the vitals
  evaluation framework
  ([`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
  [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)).
- **ragnar integration**: production RAG with
  [`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md)
  and
  [`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md).
- **Assertions with backtracking**: kept and maintained (removed in DSPy
  3.0).
- **Grid search compilation**:
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
  for explicit, tidymodels-style parameter sweeps.

## Known gaps (roadmap)

In rough priority order, based on the stable DSPy 3.3 runtime:

1.  **Fresh interpreter factories for code-executing modules**: DSPy 3.3
    creates and tears down an interpreter per invocation. dsprrr’s
    ProgramOfThought, CodeAct, and RLM modules currently retain the
    configured runner, so persistent backends must be reset between
    logically isolated jobs and must not be shared by concurrent calls.
    A factory API must also define teardown and whole-program artifact
    semantics before it can be a safe default.
2.  **Flex-style structural optimization**: there is no safe analogue
    for optimizer-authored R source or arbitrary graph-topology changes.
    Existing harnesses deliberately accept only allowlisted instruction
    and template edits.
3.  **One package-wide invocation/result contract** carrying native
    turns, usage, cost, cache state, timing, and normalized errors
    across every module.
4.  **Package-level OpenTelemetry spans** for module, optimizer,
    evaluation, cache, and tool activity, composed with ellmer’s
    provider telemetry.
5.  **Native reasoning-trace capture** as a typed output (analogous to
    `dspy.Reasoning`).
6.  **Joint multi-step optimization for instruction optimizers**
    (MIPROv2, GEPA per-component selection); demo bootstrapping is
    already joint.
7.  **Adapter-style fallbacks** for models with weak structured-output
    support (analogous to `TwoStepAdapter`).
8.  **Weight and RL optimization**, after provider-neutral training
    data, reproducibility, cost accounting, and artifact contracts are
    stable.

If one of these blocks your use case, please [open an
issue](https://github.com/JamesHWade/dsprrr/issues).
