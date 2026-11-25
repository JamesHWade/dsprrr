# dsprrr for DSPy users

dsprrr brings [DSPy](https://dspy.ai)’s programming model to R on top of
[ellmer](https://ellmer.tidyverse.org) and tidyverse conventions. It
keeps DSPy’s core concepts—signatures, modules, metrics, and
teleprompters—without forcing Python idioms onto R: datasets are
tibbles, signatures are immutable S7 values, modules are stateful R6
objects, and ellmer owns every provider call and structured schema. If
you already know DSPy, this page maps what carries over, what differs,
and what is still on the roadmap.

## Side by side

A question-answering program, compiled with labeled few-shot examples
and evaluated, in DSPy:

``` python
import dspy

dspy.configure(lm=dspy.LM("openai/gpt-6-luna"))

qa = dspy.Predict("question -> answer")
qa(question="What is the capital of France?").answer

def metric(example, pred, trace=None):
    return example.answer == pred.answer

trainset = [
    dspy.Example(question="What is 2 + 2?", answer="4").with_inputs("question"),
    # ...
]
devset = [
    dspy.Example(question="What is 3 + 5?", answer="8").with_inputs("question"),
    # ...
]
compiled = dspy.LabeledFewShot(k=3).compile(qa, trainset=trainset)
dspy.Evaluate(devset=devset, metric=metric)(compiled)
```

and in dsprrr:

``` r

library(dsprrr)

dsp_configure(provider = "openai", model = "gpt-6-luna")

qa <- module(signature("question -> answer"))
run(qa, question = "What is the capital of France?")$answer

metric <- function(prediction, expected) {
  prediction$answer == expected$answer
}

trainset <- data.frame(
  question = c("What is 2 + 2?", "Who wrote Hamlet?", "What is the largest planet?"),
  answer = c("4", "Shakespeare", "Jupiter")
)
devset <- data.frame(
  question = c("What is 3 + 5?", "Who painted the Mona Lisa?"),
  answer = c("8", "Leonardo da Vinci")
)
compiled <- qa |> compile(LabeledFewShot(k = 3L), trainset)
evaluate(compiled, devset, metric)
```

The examples below reuse `qa`, `trainset`, `devset`, and this default
chat.

The everyday differences to keep in mind:

| DSPy | dsprrr |
|----|----|
| `dspy.configure(lm=...)` | [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md), [`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md), or `.llm =` per call |
| `dspy.Predict("question -> answer")` | `module(signature("question -> answer"))`; other constructors also take strings directly |
| `pred.answer` on a `Prediction` | `result$answer` on a named list |
| `metric(example, pred, trace=None)` | `metric(prediction, expected)`: prediction first, `expected` is the whole data row |
| List of `dspy.Example(...).with_inputs(...)` | A data frame; signature input names identify input columns |
| `optimizer.compile(program, trainset=...)` | `compile(program, optimizer, trainset)` (returns a new program; original unchanged) |
| `k=3` | `k = 3L`: many S7 integer settings require an integer literal |

## Modules

| DSPy | dsprrr | Notes |
|----|----|----|
| `dspy.Predict` | `module(sig)` | Core structured predictor |
| `dspy.ChainOfThought` | [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md), [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md) | Default `"rationale"` field, plus `"plan"` and `"native"` thinking modes |
| `dspy.ReAct` | `react(sig)` | Native ellmer tool turns, `max_iterations`, opt-in `finalization = "submit"` |
| `dspy.ProgramOfThought` *(removed in DSPy 3.5)* | [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md) | Retained in dsprrr; generates and executes R code via `runner` or `interpreter_factory` |
| `dspy.CodeAct` *(removed in DSPy 3.5)* | [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md) | Retained in dsprrr; combines ellmer tools with R code execution |
| `dspy.BestOfN` | [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md) | Reward-guided retries |
| `dspy.Refine` | [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md) | Retries with rendered feedback templates |
| `dspy.MultiChainComparison` | [`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md) | Runs `M` CoT chains and reconciles them |
| `dspy.RLM` | [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) *(experimental)* | Explores R objects iteratively with `generate_action` / `extract` predictors and `SUBMIT()` |
| `dspy.Flex` | [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) *(experimental)* | Bounded JSON predictor graphs or sandboxed R `forward()` programs optimized by GEPA |
| `dspy.Parallel` / `Module.batch` | [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md), `.concurrency = concurrency_control(...)` | Batch execution via `ellmer` or `mirai` |
| `dspy.majority` | [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md) + [`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md) | Plus [`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md) and [`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md) |
| `dspy.KNN` | [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md) with [`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md) | Bring your own vectorizer (e.g., `ragnar::embed_openai()`) |
| Retrieval | [`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md) + `ragnar` | Plain R retrievers or `ragnar` stores ([`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md)) |
| `Noul`, `Score`, `Choice` *(experimental)* | [`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md) + [`decision_bool()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md) / [`decision_score()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md) / [`decision_choice()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md) *(experimental)* | Probability evidence via ellmer structured output, decoded locally; no “System One” classifier backend |

Code-running modules
([`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
[`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md))
accept either a caller-owned `runner` or a per-call
`interpreter_factory`. Executable
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
requires `interpreter_factory` and an enforced sandbox by default, while
JSON [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
needs no interpreter; see [Flex: optimize a whole
program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.html).

### Reasoning modes, ReAct `submit`, history, and async

- **Reasoning modes:**
  [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
  defaults to `"rationale"` (adds a `reasoning` field). Pass
  `mode = "plan"` for a concise plan field, or `mode = "native"` with
  `.return_format = "structured"` to keep the output schema unchanged
  and read provider thinking blocks from
  `get_metadata(result)$native_reasoning`.
- **ReAct `finalization = "submit"`:** Lets the model return its typed
  answer through a reserved `submit` tool call inside the tool loop
  (avoiding the extra structured-output request of the default
  `finalization = "structured"`).
- **Portable conversation history:** Scalar
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) calls
  on [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md)
  and
  [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
  accept `.history` (from
  [`conversation_history()`](https://jameshwade.github.io/dsprrr/reference/conversation_history.md),
  [`get_history()`](https://jameshwade.github.io/dsprrr/reference/accessors.md),
  or
  [`load_history()`](https://jameshwade.github.io/dsprrr/reference/load_history.md))
  when `.return_format = "structured"`, and persist safely as JSON with
  [`save_history()`](https://jameshwade.github.io/dsprrr/reference/save_history.md).
  ReAct preserves native turns without re-executing tools; RLM carries
  bounded textual summaries and requires an `interpreter_factory`. ReAct
  and RLM histories cannot be mixed, and dataset/async/streaming routes
  reject `.history`.
- **Async ReAct:**
  [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
  awaits the full sequential ReAct tool loop and accepts
  `.control = react_async_control(on_progress = ...)` for ordered
  progress events and cooperative cancellation (`control$cancel()`). See
  [Models, providers and
  streaming](https://jameshwade.github.io/dsprrr/articles/models-and-providers.md).

``` r

# Native reasoning + ReAct submit + portable history
native_qa <- chain_of_thought("question -> answer", mode = "native")
agent <- react("question -> answer", finalization = "submit")

first <- run(
  agent,
  question = "What is the capital of France?",
  .history = conversation_history(),
  .return_format = "structured"
)
second <- run(
  agent,
  question = "Which river runs through it?",
  .history = get_history(first),
  .return_format = "structured"
)
```

### RLM

dsprrr mirrors DSPy’s RLM model—an interpreter per invocation,
graph-visible `generate_action` and `extract` predictors, `llm_query()`
/ `llm_query_batched()` (including text + image slices), selectable
`reasoning_mode`, multi-turn `.history`, and repairable typed `SUBMIT()`
validation—while executing **R** code with signature inputs staged under
`.context`. See [How RLM
works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.html)
for the complete contract and limits, and [Investigate a regression with
RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.html)
for a worked example.

| Boundary | DSPy | dsprrr |
|----|----|----|
| Generated language | Python | R, with inputs under `.context` |
| Interpreter binding | Per-call `interpreter_factory` or global setting | Bound on [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md), or managed `mcp-repl` sandbox by default in [`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md) |
| Local persistent runner | `LocalInterpreter` (CPython) | `r_code_runner(persistent = TRUE)` (trusted `callr` process) |
| Typed outputs | Pydantic / signature adapters | ellmer string, number, integer, boolean, enum, array, and object types |
| Host tools | Interpreter-defined | Execute in the host R process outside the runner |

### Assertions

DSPy 3.0 removed `dspy.Assert`/`dspy.Suggest` in favor of
`BestOfN`/`Refine`. dsprrr keeps both: declarative output assertions
with retry feedback
([`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md),
[`assert_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md),
[`suggest_output()`](https://jameshwade.github.io/dsprrr/reference/assertions.md))
*and* the
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md)
/ [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md)
wrappers.

## Optimizers (teleprompters)

| DSPy | dsprrr | Fidelity notes |
|----|----|----|
| `LabeledFewShot` | `LabeledFewShot` | Equivalent for root prediction modules; rejects nested graphs |
| `BootstrapFewShot` | `BootstrapFewShot` | Compiles pipelines jointly; rejects programs containing [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) or [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) |
| `BootstrapFewShotWithRandomSearch` | `BootstrapFewShotWithRandomSearch` | Random search over labeled/bootstrapped candidates; rejects [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) / [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) |
| `MIPROv2` | `MIPROv2` | Searches demos + instructions via a UCB1 bandit (rather than Bayesian optimization); nested graphs require `max_bootstrapped_demos = 0L` |
| `SIMBA` | `SIMBA` | Simplified adaptation: hard-example mining + LLM-generated rules |
| `GEPA` | `GEPA` | Reflective evolution for instructions and [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) sources, with feedback metrics, multi-objective Pareto selection, separate `.reflection_llm`, and checkpoint resume on plain predictor graphs |
| `COPRO` | `COPRO` | Equivalent (coordinate ascent over model-rewritten instructions) |
| `KNNFewShot` | `KNNFewShot` | Equivalent |
| `ReAnchor` *(experimental)* | [`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md) *(experimental)* | Same gap-midpoint search and fold check; single prediction modules only |
| `Ensemble` | [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md) | Direct module constructor rather than a teleprompter |
| `BetterTogether` | `BetterTogether` | Chains prompt optimizers via strategy strings (no weight finetuning) |
| `BootstrapFinetune`, `GRPO` | Not available | Planned; dsprrr optimizes prompts and decisions, not model weights |
| No equivalent | `GridSearchTeleprompter`, [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) | Explicit tidymodels-style parameter and variant sweeps |
| No equivalent | [`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md) *(experimental)* | Best-of parallel/sequential exploration plus a continuation optimizer |
| No equivalent | [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md), [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) *(experimental)* | Agentic optimization loops over validated module snapshots with sandboxed R analysis |

### GEPA feedback, reflection models, and checkpoints

Wrap metrics in
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md)
so failed rows feed diagnostic text into GEPA’s reflection prompt, or
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md)
to score execution traces as well. Pass `.reflection_llm` to separate
proposal generation from task evaluation (`.llm`), and set
`checkpoint_path` / `resume = TRUE` in
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
to pause and resume sequential `round_robin` searches over plain
prediction modules and built-in pipelines (see
[`?GEPA`](https://jameshwade.github.io/dsprrr/reference/GEPA.md) for
exact checkpoint rules):

``` r

feedback_metric <- metric_with_feedback(
  function(prediction, expected) {
    if (identical(prediction$answer, expected$answer)) {
      list(score = 1, feedback = "Correct.")
    } else {
      list(
        score = 0,
        feedback = paste("Expected", expected$answer, "- check the arithmetic.")
      )
    }
  },
  field = "answer"
)

compiled <- compile(
  qa,
  GEPA(metric = feedback_metric, generations = 5L),
  trainset,
  valset = devset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .reflection_llm = ellmer::chat_openai(model = "gpt-6-astra"),
  control = optimizer_control(
    checkpoint_path = "gepa-search.rds",
    max_metric_calls = 50L
  )
)
```

### Decision types and `ReAnchor()`

Like DSPy 3.4, dsprrr keeps the output space in the signature and
attaches tunable thresholds, cuts, or weights to the module with
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md):

``` r

sig <- signature(
  inputs = list(input("pair", description = "Two listings")),
  output_type = ellmer::type_object(
    match = ellmer::type_boolean("Are they the same item?")
  )
)
matcher <- module(sig) |> with_decisions(match = decision_bool(threshold = 0.7))
tuned <- compile(
  matcher,
  ReAnchor(metric = metric_exact_match(field = "match")),
  pair_train,
  valset = pair_val
)
decision_settings(tuned)
```

See [Calibrated
decisions](https://jameshwade.github.io/dsprrr/articles/calibrated-decisions.md)
for
[`decision_score()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md),
[`decision_choice()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md),
and
[`decision_evidence()`](https://jameshwade.github.io/dsprrr/reference/decision_evidence.md).

## Signatures, composition, and infrastructure

| Area | DSPy | dsprrr |
|----|----|----|
| Signatures | String notation or `dspy.Signature` class; [`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md) | `signature("a -> b: int")` or [`input()`](https://jameshwade.github.io/dsprrr/reference/input.md) + ellmer types; immutable [`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md) / [`append_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md) |
| Multimodal inputs | `dspy.Image`, `dspy.Audio`, `dspy.File` | ellmer `Content` objects (images, PDFs) |
| Composition | Python `dspy.Module` subclass with `forward()` | [`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md) / `%>>%` + [`step()`](https://jameshwade.github.io/dsprrr/reference/step.md) (jointly compilable), or [`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md) for custom R control flow |
| LM client & adapters | `dspy.LM` (`lm15`), XML/Chat/JSON adapters | Provider-neutral ellmer `Chat` + `chat_structured()` (no separate adapter layer) |
| Configuration & caching | `dspy.configure()`, `dspy.context()`, two-tier cache | [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md), [`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md), [`local_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md), two-tier [`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md) |
| Async & streaming | `acall` / `aforward`, `streamify()` | [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md) (Predict, ReAct, factory code modules), [`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md) + [`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md), [`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md) |
| Saving programs | [`save()`](https://rdrr.io/r/base/save.html) / [`load()`](https://rdrr.io/r/base/load.html) | Versioned whole-program artifacts ([`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md), [`load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md), [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)) |
| Evaluation & traces | `dspy.Evaluate`, MLflow/OTel callbacks | [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md), [`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md), [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md), [`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md), plus `vitals` integration |

## What dsprrr has that DSPy doesn’t

| Feature | What it gives you |
|----|----|
| `tidymodels` integration | [`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md) `parsnip` engine, `yardstick` scoring, and `dials` parameters ([`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md), [`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md), [`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md)) |
| `vitals` integration | [`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md), [`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md), and [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md) |
| `ragnar` integration | Built-in DuckDB hybrid search via [`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md) and [`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md) |
| Assertions with backtracking | Maintained alongside [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md) / [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md) after removal in DSPy 3.0 |
| Grid search compilation | [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) and [`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md) for explicit sweeps |
| Meta & agentic harnesses *(experimental)* | [`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md), [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md), and [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) |

## Roadmap gaps

Key capabilities not yet implemented in dsprrr:

1.  **Custom input staging** for interpreter-backed modules beyond
    `r_code_runner(persistent = TRUE)` and JSON-serialized `mcp-repl`
    inputs.
2.  **Concurrent execution and checkpoint resume** for executable
    [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
    modules and calibrated decision modules.
3.  **Whole-program decision calibration** (calibrating
    [`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md)
    across composed pipelines rather than single prediction modules) and
    **RLM child-demo bootstrapping** in
    [`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md).
4.  **Package-level OpenTelemetry spans** layered on top of ellmer’s
    provider telemetry, and a dedicated typed reasoning output field
    analogous to `dspy.Reasoning`.
5.  **Model-weight and RL optimization** (`BootstrapFinetune`, `GRPO`).

*Checked against [DSPy
3.5.0](https://github.com/stanfordnlp/dspy/releases/tag/3.5.0)
(2026-11-10). If one of these gaps blocks your work, please [open an
issue](https://github.com/JamesHWade/dsprrr/issues).*
