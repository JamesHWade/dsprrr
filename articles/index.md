# Articles

### Tutorials

Start here. Six short steps from a first call to a saved, optimized
module; each one builds on the last.

- [Tutorial 1: Your first LLM
  call](https://jameshwade.github.io/dsprrr/articles/tutorial-hello-world.md):

  Declare a task with signature(), turn it into a module, and get typed
  R values—strings, numbers, labels, and booleans—back from an LLM.

- [Tutorial 2: Build a
  classifier](https://jameshwade.github.io/dsprrr/articles/tutorial-build-classifier.md):

  Scale a sentiment classifier from a single review to vectors and data
  frames with run_dataset(), steer borderline cases with instructions,
  and describe inputs with input().

- [Tutorial 3: Extract structured
  data](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.md):

  Extract multiple typed fields, arrays, nested records, and optional
  values from unstructured text into R lists and tibbles.

- [Tutorial 4: Improve with
  examples](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md):

  Score a classifier against labeled data with evaluate(), attach worked
  examples by hand or with LabeledFewShot(), and check whether they
  help.

- [Tutorial 5: Optimize a
  module](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md):

  Search instruction variants, settings, and few-shot demos
  automatically with optimize_grid() and GridSearchTeleprompter(), then
  evaluate on a held-out test set.

- [Tutorial 6: Save and reuse a
  module](https://jameshwade.github.io/dsprrr/articles/tutorial-deploy-to-production.md):

  Save a compiled module to disk with save_program() or share versioned
  copies on a pins board, restore it in a new session, and export
  execution traces.

### Projects

Longer worked examples on a realistic task.

- [Generate an llms.txt
  file](https://jameshwade.github.io/dsprrr/articles/llms-txt.md):

  Build a multi-stage dsprrr program that writes an llms.txt summary for
  an R package, executes its generated R examples in callr as a metric,
  and improves it with BootstrapFewShot().

- [Investigate a regression with
  RLM](https://jameshwade.github.io/dsprrr/articles/tutorial-rlm-dsprrr.md):

  Use an RLM to explore a large R data frame, isolate a conversion
  regression, and connect it to a relevant change record.

- [Build a text
  adventure](https://jameshwade.github.io/dsprrr/articles/text-adventure.md):

  Build an interactive console text adventure in R where dsprrr modules
  invent rooms, referee risky actions with calibrated probability
  thresholds, and narrate outcomes that you can score with metrics.

### Build

Pick a module type, compose modules, and add retrieval or decisions.

- [Choose a module
  type](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md):

  Compare dsprrr’s module constructors—prediction, chain-of-thought,
  retry wrappers, ensembles, ReAct tools, RAG, code execution, and
  custom functions—on one shared task.

- [Chain modules into
  pipelines](https://jameshwade.github.io/dsprrr/articles/chaining-modules.md):

  Connect modules into multi-step pipelines with pipeline(), step(), and
  %\>\>%, control field flow with map and select, inspect step traces,
  and compile pipelines jointly.

- [Retrieval-augmented
  generation](https://jameshwade.github.io/dsprrr/articles/rag-workflows.md):

  Ground answers in your own documents using rag_module() with a custom
  R retriever or a ragnar store, build agentic search with
  ragnar_tool(), and evaluate retrieval quality.

- [Calibrated
  decisions](https://jameshwade.github.io/dsprrr/articles/calibrated-decisions.md):

  Turn boolean and enum outputs into probability-backed decisions with
  with_decisions(), inspect the evidence behind each call, and fit
  thresholds or weights with ReAnchor().

### Measure and improve

Evaluate modules and optimize them with data.

- [Compile and
  optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md):

  Walk through a full optimization run: define a task and metric,
  compile with LabeledFewShot and GridSearchTeleprompter, evaluate on
  held-out data, inspect what changed, and save the result.

- [Choose an
  optimizer](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md):

  Compare dsprrr’s teleprompters by what they optimize, data
  requirements, and model-call cost, with runnable call templates and
  budget/checkpoint options.

- [A GEPA learning
  journey](https://jameshwade.github.io/dsprrr/articles/gepa-learning-journey.md):

  Follow one complete GEPA optimization run—baseline failures, textual
  feedback, reflective prompt rewrites, candidate lineage, holdout
  check, and budget accounting.

- [Evaluate with
  vitals](https://jameshwade.github.io/dsprrr/articles/vitals-recipes.md):

  Evaluate dsprrr modules as vitals tasks and solvers, compare variants
  across epochs, browse vitals logs, and wrap vitals scorers as dsprrr
  metrics.

### Run

Configure models and run dsprrr in larger workflows.

- [Models, providers and
  streaming](https://jameshwade.github.io/dsprrr/articles/models-and-providers.md):

  Configure ellmer Chats, provider parameters, reasoning models,
  multimodal inputs, tool/agent integrations, timeouts, async promises,
  and streaming.

- [Run dsprrr in
  pipelines](https://jameshwade.github.io/dsprrr/articles/orchestration.md):

  Run dsprrr inside targets workflows and Quarto reports, validate
  inputs before execution, pin evaluation logs and traces, and control
  batch concurrency and cost.

- [Use dsprrr with
  tidymodels](https://jameshwade.github.io/dsprrr/articles/tidymodels-integration.md):

  Score dsprrr predictions with yardstick, build tuning grids with
  dials, and wrap zero-shot modules in the llm_predict() parsnip model
  specification.

- [Troubleshooting](https://jameshwade.github.io/dsprrr/articles/troubleshooting.md):

  Diagnose and fix common dsprrr setup, signature, module, run(),
  evaluation, concurrency, provider, and response-cache errors.

### Concepts

How dsprrr works and why, and how it maps to DSPy.

- [How dsprrr
  works](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md):

  The core mental model behind dsprrr: immutable S7 signatures, stateful
  R6 modules, the five-function lifecycle, and when plain prompting is
  still enough.

- [Metrics and
  evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md):

  Turn subjective prompt checks into reproducible numbers: how dsprrr
  calls metrics, built-in and trace-aware metrics, evaluate(), epochs,
  and sample-size pitfalls.

- [How optimization
  works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md):

  What dsprrr optimizers search over, how metrics and data splits guide
  them, why winning validation scores are optimistic, and where model
  calls go.

- [dsprrr for DSPy
  users](https://jameshwade.github.io/dsprrr/articles/dspy-comparison.md):

  Translate your DSPy mental model to R: side-by-side examples, module
  and optimizer comparisons against DSPy 3.5.0, infrastructure mapping,
  and roadmap gaps.

- [How RLM
  works](https://jameshwade.github.io/dsprrr/articles/how-rlm-works.md):

  What happens during an RLM call: where the inputs live, what the model
  sees, reasoning modes, recursive and multimodal queries, multi-turn
  history, where code runs, and the limits that apply.

### Experimental

Features whose interfaces may change.

- [Flex: optimize a whole
  program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md):

  Start with declarative JSON, then use executable Flex when
  optimization needs deterministic R logic or selected host tools.

- [Compose optimizers with
  Omni](https://jameshwade.github.io/dsprrr/articles/omni-meta-optimization.md):

  Explore several dsprrr teleprompters from one seed, compare them
  fairly, and continue from the strongest candidate.

- [Agentic optimization
  harnesses](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md):

  Compare AutoResearch() and MetaHarness() on a two-step pipeline, see
  how dsprrr enforces editable paths, budgets, and sandboxing, and
  configure live runs.

### Reference

Quick lookup.

- [Quick
  reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md):

  Copyable snippets for everyday dsprrr tasks: model setup, signatures,
  module constructors, running, evaluation, teleprompters, inspection,
  caching, and saving programs.
