# dsprrr for DSPy users

dsprrr is an R implementation of [DSPy](https://dspy.ai)’s programming
model, built on [ellmer](https://ellmer.tidyverse.org) and tidyverse
conventions. It keeps DSPy’s concepts (signatures, modules, metrics and
optimizers, or “teleprompters”) but is not a line-by-line port: data
goes in and out as tibbles, signatures are S7 objects, modules are R6
objects, and ellmer handles every provider call. If you know DSPy, this
page shows what carries over, what differs, and what is not available
yet.

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

The examples further down reuse `qa`, `trainset`, `devset` and this
default chat.

The differences that show up in everyday code:

| DSPy | dsprrr |
|----|----|
| `dspy.configure(lm=...)` | [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md) or [`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md), or `.llm =` on a single call |
| `dspy.Predict("question -> answer")` | `module(signature("question -> answer"))`; other constructors such as [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md) also accept the string |
| `pred.answer` on a `Prediction` | `result$answer` on a named list |
| `metric(example, pred, trace=None)` | `metric(prediction, expected)`: the prediction comes first, and `expected` is the data row |
| A list of `dspy.Example(...).with_inputs(...)` | A data frame; the signature’s input names say which columns are inputs |
| `optimizer.compile(program, trainset=...)` | `compile(program, optimizer, trainset)`, which returns a new program and leaves the original unchanged |
| `k=3` | `k = 3L`: integer settings need an integer literal |

## Version baseline

This comparison was checked against DSPy **3.4.0**, released on
2026-09-25, which DSPy calls its “LM transition release”:

- DSPy’s LM layer now runs through bundled native engines. The
  experimental 3.3 `LMRequest`/`LMResponse` types have been removed, and
  OpenAI-style `lm(messages = ...)` calls are deprecated until 3.5.
- It adds experimental decision types (`Noul`, `Score`, `Choice`) and
  the ReAnchor calibration optimizer.
- It adds a persistent `LocalInterpreter`, async `ReActV2`, and
  call-time interpreter factories for RLM.

The 3.3.1 patch release, also covered here, deprecated `CodeAct` and
`ProgramOfThought`, added objective-aware GEPA frontiers, and exposed
optimizer and interpreter callback events. The 3.3.0 release introduced
the experimental `Flex` and `ReActV2` modules that dsprrr’s
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) and
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md)
follow. See the [DSPy 3.4.0 release
notes](https://github.com/stanfordnlp/dspy/releases/tag/3.4.0).

## Modules

| DSPy | dsprrr | Notes |
|----|----|----|
| `dspy.Predict` | `module(sig)` | Core predictor |
| `dspy.ChainOfThought` | [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md), [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md) | Implemented as signature transforms |
| `dspy.ReAct` / experimental `ReActV2` | `react(sig)` | Native ellmer turn history, tool-call IDs, parallel calls per assistant turn, enforced iteration limit, then structured finalization; behaviorally aligned, not a port of the Python class. There is no counterpart to `ReActV2.acall()`: [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md) does not accept [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) modules, so run them with [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) |
| `dspy.ProgramOfThought` (deprecated in DSPy 3.3.1) | [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md) | Generates and executes **R** code (not Python); accepts either a caller-owned reused runner or a fresh per-invocation interpreter factory. dsprrr keeps it supported; DSPy points users to `RLM` and `Flex` |
| `dspy.CodeAct` (deprecated in DSPy 3.3.1) | [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md) | Hybrid tools + R code execution with an enforced inner tool-call limit; accepts either a caller-owned reused runner or a fresh per-invocation interpreter factory. The built-in runner is trusted-input-only, and sandboxed backends can implement the runner protocol |
| `dspy.BestOfN` | [`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md) | Reward-function-guided retries |
| `dspy.Refine` | [`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md) | Retries with LLM-generated feedback |
| `dspy.MultiChainComparison` | [`multi_chain_comparison()`](https://jameshwade.github.io/dsprrr/reference/multi_chain_comparison.md) |  |
| `dspy.RLM` | [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md) (experimental) | Inference-time adaptive exploration over an R REPL, with optimizable action and extraction predictors, inherited or separate sub-LMs, replayed recursive values, typed submission repair, and trajectory metadata |
| Experimental `dspy.Flex` | [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) | GEPA can optimize a bounded predictor graph or an R `forward()` program. dsprrr requires an explicit fresh interpreter for executable source rather than choosing a default sandbox |
| `dspy.Parallel` / `Module.batch` | [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md), `run(..., .concurrency = concurrency_control(...))` | Batch over a data frame; heterogeneous (module, example) fan-out is not yet a dedicated module |
| `dspy.majority` | [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md) with [`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md) | Plus [`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md), [`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md) |
| `dspy.KNN` | [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md) with the `KNNFewShot` teleprompter | The compiled module picks the nearest training examples as demos for each input. Bring your own vectorizer (e.g., `ragnar::embed_openai()`) |
| Retrieval (custom functions) | [`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md) + ragnar | Built-in ragnar retriever integration |
| Experimental decision outputs on `Predict` (`Noul`, `Score`, `Choice`) | [`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md) with [`decision_bool()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md), [`decision_score()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md), [`decision_choice()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md) (experimental) | Probability evidence from ellmer structured output, decoded locally with per-field `threshold`, `cuts`, and `weights`. Native logical/character values are returned, and evidence is available through [`decision_evidence()`](https://jameshwade.github.io/dsprrr/reference/decision_evidence.md). DSPy’s TypeSafe “System One” backend has no R counterpart |

ProgramOfThought, CodeAct, and RLM take either a caller-owned `runner`
or a zero-argument `interpreter_factory` that creates a fresh runner per
invocation. Executable Flex accepts only the factory form and requires
an enforced sandbox by default, while JSON Flex needs no interpreter;
see [Flex: optimize a whole
program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md).

### RLM

dsprrr follows the DSPy RLM execution contract, which DSPy 3.4 leaves
unchanged apart from interpreter binding: one interpreter per
invocation, `generate_action` and `extract` child predictors that graph
optimizers can see, a sub-LM that inherits the outer LM unless you set
one, `llm_query()` results replayed into the running R code, and invalid
typed `SUBMIT()` calls returned to the model as repairable observations.
[How RLM
works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.md)
describes the contract in full. The differences a DSPy user will notice:

| Boundary | DSPy | dsprrr |
|----|----|----|
| Generated language | Python | R, with signature inputs under `.context` |
| Interpreter binding | A keyword-only `interpreter_factory` at call time (3.4), or a `dspy.settings.interpreter_factory` default | Runner or factory bound when constructing [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md); [`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md) creates that binding for one invocation |
| Persistent local interpreter | `LocalInterpreter` (3.4), CPython without Deno | `r_code_runner(persistent = TRUE)`, one trusted callr R process per invocation |
| Default local execution | DSPy interpreter configuration | The one-call [`rlm()`](https://jameshwade.github.io/dsprrr/reference/rlm.md) helper creates a fresh managed `mcp-repl` OS sandbox with network disabled and workspace writes allowed; it suits compact JSON-compatible context |
| Returned value | Prediction with trajectory and top-level `final_reasoning` | [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) output plus structured metadata, including a bounded trajectory |
| Typed outputs | Signature adapters validate the Python/Pydantic type | ellmer string, number, integer, boolean, enum, array, and object types; opaque `TypeJsonSchema` nodes are rejected |
| Host tools | Interpreter-defined | Run in the host R process, outside the guest sandbox |

RLM remains experimental in dsprrr: the execution and result contracts
are tested, but the convenience API may still change. For a worked
example, see [Investigate a regression with
RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.md).

### Assertions

DSPy 3.0 *removed* `dspy.Assert`/`dspy.Suggest` in favor of
`BestOfN`/`Refine`. dsprrr keeps both styles: declarative assertions
with retry/backtracking
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
| `LabeledFewShot` | `LabeledFewShot` | Equivalent for a root Predict module; nested-predictor graphs, including pipelines, RLM and Flex, are rejected rather than receiving mismatched root-task demos |
| `BootstrapFewShot` | `BootstrapFewShot` | Compiles ordinary pipelines jointly; programs containing Flex or RLM are rejected because their runtime predictors cannot receive root-task demos |
| `BootstrapFewShotWithRandomSearch` | `BootstrapFewShotWithRandomSearch` | Random search over ordinary labeled/bootstrap candidates; rejects programs containing Flex or RLM rather than returning a baseline-only no-op |
| `MIPROv2` | `MIPROv2` | Root Predict modules search instruction + demo candidates; nested graphs search child instructions with `max_bootstrapped_demos = 0L`. Candidates are chosen with a UCB1 bandit rather than Bayesian optimization |
| `SIMBA` | `SIMBA` | Adapted: hard-example mining + LLM-generated rules; simplified vs. the full introspective algorithm |
| `GEPA` | `GEPA` | Adapted reflective optimization for instructions and complete Flex sources, with separate train/validation roles, multi-objective selection, lineage, and optional retained outputs. DSPy 3.3.1’s per-metric `objective_scores` and `frontier_type` map to dsprrr’s named `metrics` list with Pareto selection. Cached subsample merge acceptance, fine-grained resume, parallel candidate evaluation, and built-in experiment trackers remain different |
| `COPRO` | `COPRO` | Equivalent (coordinate ascent over instructions) |
| `KNNFewShot` | `KNNFewShot` | Equivalent |
| Experimental `ReAnchor` (3.4) | [`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md) (experimental) | Same gap-midpoint candidates, fold check, and restore-if-not-better rule; single Predict modules only. [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md) runs the module over the training set to record evidence (twice when outputs first have to be switched to evidence decoding) and scores any `valset`; the search over settings then re-decodes that evidence without further provider calls |
| `Ensemble` | [`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md) | Direct module constructor rather than a teleprompter |
| `BetterTogether` | `BetterTogether` | Chains prompt optimizers via strategy strings; does **not** alternate prompt/weight optimization (no finetuning backend) |
| `BootstrapFinetune` | Not available | Planned; dsprrr currently optimizes prompts, not weights |
| `GRPO` (RL via Arbor) | Not available |  |
| `BootstrapFewShotWithOptuna`, `AvatarOptimizer`, `InferRules` | Not available | Niche/legacy in DSPy; not planned |
| No equivalent | `GridSearchTeleprompter`, [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) | tidymodels-style grid search over module parameters |
| No equivalent | `Omni` | Independent best-of exploration plus a fresh continuation optimizer, with common validation scoring and optional mirai concurrency |
| No equivalent | `AutoResearch` | Persistent research-agent loop over validated, jointly editable module snapshots with sandboxed R analysis |
| No equivalent | `MetaHarness` | Fresh batch proposers plus host-owned frontier selection, lineage, budgets, and checkpoint resume |

### GEPA feedback metrics

DSPy’s GEPA expects metrics that return a score *and textual feedback*.
dsprrr supports the same protocol. The metric receives the whole data
row as `expected`, so read the answer column from it:

``` r

metric <- metric_with_feedback(
  function(prediction, expected) {
    if (identical(prediction$answer, expected$answer)) {
      list(score = 1, feedback = "Correct.")
    } else {
      list(
        score = 0,
        feedback = paste(
          "Wrong: expected", expected$answer, "- check the arithmetic."
        )
      )
    }
  },
  field = "answer"
)

tp <- GEPA(metric = metric, generations = 5L)
compiled <- compile(qa, tp, trainset)
```

The feedback for failed examples goes into GEPA’s reflection prompt, so
the reflection model sees why each output failed as well as which ones
did.

### Trace-aware metrics

DSPy 3.3 makes execution traces available to metrics used by reflective
optimization. In dsprrr, wrap the metric with
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md);
the wrapped function receives the prediction, the expected row, and a
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

result <- evaluate(qa, devset, metric)
result$traces[[1]][c("row_id", "epoch", "status")]
```

Each trace contains `row_id`, `epoch`, `status`, ordered `events`, and
module `metadata`. With repeated evaluation, `result$epoch_traces`
preserves the row-aligned traces for every epoch. Trace events may
contain prompts, inputs, and model responses, so handle them as
potentially sensitive data.

### Decision types and ReAnchor

DSPy 3.4 declares decisions as signature annotations and keeps the
tunable settings on the predictor:

``` python
class Match(dspy.Signature):
    pair: str = dspy.InputField(desc="Two listings.")
    match: bool = dspy.OutputField(desc="Are they the same item?")

matcher = dspy.Predict(Match)
matcher.fields["match"] = {"threshold": 0.7}
tuned = ReAnchor(metric).compile(matcher, trainset=trainset, valset=valset)
```

dsprrr splits the declaration the same way. The answer space stays in
the signature, and the settings live on the module. Here `pair_train`
and `pair_val` are data frames with a `pair` column and a logical
`match` column:

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
for Score and Choice decisions, the evidence schema, and how ReAnchor
searches.

## Signatures and types

| DSPy | dsprrr |
|----|----|
| `"question -> answer: int"` string signatures | The same string: `signature("question -> answer: int")`. `str`, `int`, `float`, `bool`, `list[...]`, `Optional[...]` and `Literal[...]` all parse |
| Class-based signatures with `InputField`/`OutputField` | `signature(inputs = list(input(...)), output_type = ...)` |
| `Signature.with_instructions()` / `Signature.append_instructions()` | [`with_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md) / [`append_instructions()`](https://jameshwade.github.io/dsprrr/reference/with_instructions.md); both return a new signature without mutating the original |
| Pydantic-typed outputs | ellmer type objects ([`type_string()`](https://ellmer.tidyverse.org/reference/type_boolean.html), [`type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html), [`type_object()`](https://ellmer.tidyverse.org/reference/type_boolean.html), [`type_array()`](https://ellmer.tidyverse.org/reference/type_boolean.html)) |
| `dspy.Image`, `dspy.Audio`, `dspy.File` | ellmer `Content` objects (images, PDFs) passed as inputs |
| `dspy.History` | Native ellmer turns preserved in ReAct metadata and traces; not a signature type |
| `dspy.Tool`, `dspy.ToolCalls`, `ToolCallResults` | ellmer `ToolDef`, `ContentToolRequest`, and `ContentToolResult`; IDs remain attached to native turns |
| `dspy.Reasoning` (native reasoning traces) | Not yet built in; [`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md) adds a prompted reasoning field |
| Experimental `Noul`, `Score[...]`, `Choice[...]` annotations | Plain [`type_boolean()`](https://ellmer.tidyverse.org/reference/type_boolean.html) / [`type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html) outputs plus [`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md) settings on the module; the answer space stays in the signature, and the tunable settings live on the module |

## Programs and composition

DSPy composes programs as Python classes whose `forward()` calls several
predictors. dsprrr composes pipelines of modules:

``` r

outline <- module(signature("question -> outline"))
write_up <- module(signature("notes -> answer"))

program <- pipeline(
  outline,
  step(write_up, map = c(outline = "notes"))
)
run(program, question = "Why is the sky blue?")
```

The first step receives the pipeline’s inputs. Each later step receives
only the previous step’s outputs, renamed by `map` (upstream field on
the left, this step’s input on the right), plus any static inputs passed
to [`step()`](https://jameshwade.github.io/dsprrr/reference/step.md).
For control flow that a pipeline cannot express, wrap an R function with
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md);
it runs and evaluates like any module, but
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
refuses it.

`BootstrapFewShot` compiles pipelines **jointly**, like DSPy: the
teacher pipeline runs end to end, final outputs are scored, and each
step harvests demonstrations from passing traces. MIPROv2 and GEPA also
optimize the whole program: they propose instructions for each Predict
step (paths `$/steps/1`, `$/steps/2`, …) and score candidates on the
pipeline’s final output; MIPROv2 needs `max_bootstrapped_demos = 0L`
there.
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md)
rejects pipelines, because root examples do not match each step’s
signature.

## Infrastructure

| Capability | DSPy | dsprrr |
|----|----|----|
| LM client | `dspy.LM` on bundled native engines (`engine = "auto"`) with a LiteLLM fallback in 3.4; the 3.3 `LMRequest`/`LMResponse` types were removed | Provider-neutral ellmer `Chat`; [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) normalizes prompt and content inputs into typed requests, while a complete package-wide invocation record is still planned |
| Configuration | `dspy.configure()` / `dspy.context()` | [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md), [`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md), [`local_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md) |
| Caching | Two-tier memory + disk | Two-tier memory + disk ([`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md)) |
| Async | `acall`/`aforward`, `asyncify` | [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md) with promises for ordinary Predict modules and isolated background workflows for factory-backed ProgramOfThought, CodeAct, and RLM. Other module types such as [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) are rejected, as are caller-owned interpreters and specialized streaming, rather than shared or bypassed |
| Streaming | `streamify()` + `StreamListener` | [`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md) + [`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md); one-shot fallback preserves specialized `forward()` semantics, while direct token streaming is limited to ordinary Predict steps and emits status events per pipeline step |
| Usage tracking | `track_usage` | [`get_tokens()`](https://jameshwade.github.io/dsprrr/reference/accessors.md), [`get_cost()`](https://jameshwade.github.io/dsprrr/reference/accessors.md), [`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md) |
| Parallel evaluation | `Evaluate(num_threads = ...)` | `evaluate(.concurrency = concurrency_control(...))` via mirai or ellmer’s native parallelism. Declarative zero/one-step Flex is supported; executable and multi-step Flex currently require sequential rows |
| Saving programs | `save`/`load`; sanitized LM state and explicit unsafe-class opt-in in 3.3 | Versioned whole-program artifacts via [`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md) / [`load_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md) or pins, with registry-backed runtime IDs and explicit trusted opt-in |
| Observability | MLflow autolog, OpenTelemetry callbacks; 3.3.1 adds optimizer-compile and interpreter-lifecycle callback events | Traces tibble, [`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md), [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md); package-level OpenTelemetry spans are planned on top of ellmer |
| Adapters (Chat/JSON/XML/TwoStep/BAML) | Yes | No adapter layer; ellmer’s `chat_structured()` handles structured output |
| Evaluation framework | `dspy.Evaluate`, including trace-aware metrics | [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md), [`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md), [`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md), plus **vitals** integration |

## What dsprrr has that DSPy doesn’t

| Feature | What it gives you |
|----|----|
| tidymodels integration | Modules as parsnip engines, tuned with dials parameters ([`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md), [`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md), [`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md)) |
| vitals integration | Modules and metrics bridged to the vitals evaluation framework ([`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md), [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)) |
| ragnar integration | Retrieval with [`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md) and [`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md) |
| Assertions with backtracking | Kept and maintained after their removal in DSPy 3.0 |
| Grid search compilation | [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) for explicit, tidymodels-style parameter sweeps |

## Known gaps (roadmap)

Besides the RLM differences above, these package-level gaps remain, in
rough priority order:

1.  General custom input staging for interpreter-backed modules,
    analogous to DSPy’s `SandboxSerializable`, beyond the current
    persistent-callr and runner-specific paths.
2.  Remaining Flex gaps: concurrent executable evaluation, fine-grained
    checkpoint resume, and GEPA’s cached subsample merge-acceptance gate
    remain unimplemented. The top-level R `forward()` function, explicit
    interpreter factory, and non-portable R/Python source are
    intentional API differences, not missing parity.
3.  One package-wide invocation/result contract carrying native turns,
    usage, cost, cache state, timing, and normalized errors across every
    module.
4.  Package-level OpenTelemetry spans for module, optimizer, evaluation,
    cache, and tool activity, composed with ellmer’s provider telemetry.
5.  Native reasoning-trace capture as a typed output (analogous to
    `dspy.Reasoning`).
6.  Predictor-local RLM demo evidence so MIPROv2 can bootstrap child
    demos; its graph mode currently tunes child instructions only, while
    GEPA selects the RLM child components explicitly.
7.  Adapter-style fallbacks for models with weak structured-output
    support (analogous to `TwoStepAdapter`).
8.  Weight and RL optimization, after provider-neutral training data,
    reproducibility, cost accounting, and artifact contracts are stable.
9.  Whole-program decision calibration.
    [`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md)
    fits a single Predict module exactly; DSPy also re-runs composed
    programs with cached answers. Concurrent execution of decision
    modules is also not yet supported.

If one of these blocks your use case, please [open an
issue](https://github.com/JamesHWade/dsprrr/issues).
