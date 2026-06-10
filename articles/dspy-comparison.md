# dsprrr vs. DSPy: Feature Comparison

dsprrr is an R implementation of [DSPy](https://dspy.ai)’s programming
model, built on [ellmer](https://ellmer.tidyverse.org) and tidyverse
conventions. If you know DSPy, this page tells you what carries over,
what is different, and what is not (yet) available. It reflects DSPy 3.x
as of mid-2026.

dsprrr is not a line-by-line port. It follows DSPy’s concepts —
signatures, modules, metrics, and optimizers (“teleprompters”) — while
embracing R idioms: tibbles in and out, S7/R6 objects, and ellmer for
all provider communication.

## Modules

| DSPy | dsprrr | Notes |
|----|----|----|
| `dspy.Predict` | `module(sig, type = "predict")` | Core predictor |
| `dspy.ChainOfThought` | [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md), [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md) | Implemented as signature transforms |
| `dspy.ReAct` | `module(sig, type = "react")` | Tool-calling agent loop via ellmer tools |
| `dspy.ProgramOfThought` | [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md) | Generates and executes **R** code (not Python) |
| `dspy.CodeAct` | [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md) | Hybrid tools + R code execution; requires an explicit, opt-in [`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md) |
| `dspy.BestOfN` | [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md) | Reward-function-guided retries |
| `dspy.Refine` | [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md) | Retries with LLM-generated feedback |
| `dspy.MultiChainComparison` | [`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md) |  |
| `dspy.RLM` | [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) | Recursive language models over an R REPL |
| `dspy.Parallel` / `Module.batch` | [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md), `run(..., .parallel = TRUE)` | Batch over a data frame; heterogeneous (module, example) fan-out is not yet a dedicated module |
| `dspy.majority` | [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble_module.md) with [`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md) | Plus [`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md), [`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md) |
| `dspy.KNN` | `KNNFewShot` teleprompter / KNN module | Bring-your-own vectorizer (e.g., [`ragnar::embed_openai()`](https://ragnar.tidyverse.org/reference/embed_ollama.html)) |
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
| `BootstrapFinetune` | — | Not implemented (planned); dsprrr currently optimizes prompts, not weights |
| `GRPO` (RL via Arbor) | — | Not implemented |
| `BootstrapFewShotWithOptuna`, `AvatarOptimizer`, `InferRules` | — | Niche/legacy in DSPy; not planned |
| — | `GridSearchTeleprompter`, [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) | dsprrr addition: tidymodels-style grid search over module parameters |

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

## Signatures and types

| DSPy | dsprrr |
|----|----|
| `"question -> answer: int"` string signatures | `signature("question -> answer: integer")` |
| Class-based signatures with `InputField`/`OutputField` | `signature(inputs = list(input(...)), output_type = ...)` |
| Pydantic-typed outputs | ellmer type objects ([`type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html), [`type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html), [`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html), [`type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html)) |
| `dspy.Image`, `dspy.Audio`, `dspy.File` | ellmer `Content` objects (images, PDFs) passed as inputs |
| `dspy.History` | Implicit via ellmer `Chat$get_turns()`; not a signature type |
| `dspy.Tool`, `dspy.ToolCalls` | ellmer `ToolDef` via [`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md) / [`register_dsprrr_tool()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_tool.md) |
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
| LM client | `dspy.LM` (LiteLLM; decoupling in 3.2+) | ellmer `Chat` (100+ providers via ellmer) |
| Configuration | `dspy.configure()` / `dspy.context()` | [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md), [`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md), [`local_lm()`](https://jameshwade.github.io/dsprrr/reference/local_lm.md) |
| Caching | Two-tier memory + disk | Two-tier memory + disk ([`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md)) |
| Async | `acall`/`aforward`, `asyncify` | [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md) with promises |
| Streaming | `streamify()` + `StreamListener` | [`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md) + [`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md); token streaming for single string fields, status events per pipeline step |
| Usage tracking | `track_usage` | [`get_tokens()`](https://jameshwade.github.io/dsprrr/reference/get_tokens.md), [`get_cost()`](https://jameshwade.github.io/dsprrr/reference/get_cost.md), [`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md) |
| Parallel evaluation | `Evaluate(num_threads = ...)` | `evaluate(.parallel = TRUE)` via mirai or ellmer’s native parallelism |
| Saving programs | `save`/`load`, whole-program serialization | [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md) / [`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md) (pins-based) |
| Observability | MLflow autolog, OpenTelemetry callbacks | Traces tibble, [`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md), [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md); MLflow integration planned |
| Adapters (Chat/JSON/XML/TwoStep/BAML) | Yes | No adapter layer; ellmer’s `chat_structured()` handles structured output |
| Evaluation framework | `dspy.Evaluate` | [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md), [`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md), plus **vitals** integration |

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

In rough priority order:

1.  **Weight optimization**: `BootstrapFinetune` / RL-based optimizers.
2.  **Native reasoning-trace capture** as a typed output (analogous to
    `dspy.Reasoning`).
3.  **Joint multi-step optimization for instruction optimizers**
    (MIPROv2, GEPA per-component selection); demo bootstrapping is
    already joint.
4.  **Adapter-style fallbacks** for models with weak structured-output
    support (analogous to `TwoStepAdapter`).
5.  **MLflow / OpenTelemetry observability**.

If one of these blocks your use case, please [open an
issue](https://github.com/JamesHWade/dsprrr/issues).
