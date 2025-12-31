# NA

### 1. Vision & Philosophy

`dsprrr` helps R programmers build **principled, test-driven,
optimisable** LLM systems. We want iterative LLM development to feel
like a tidyverse project: data lives in tibbles, contracts are explicit,
experiments are reproducible, and evaluation tooling (including
`vitals`) plugs in without adapters.

Guiding tenets: - **Typed boundaries.** Every module advertises a
signature; inputs and outputs are validated before the model runs. -
**Stateful, observable modules.** Prompt code, configuration, losses,
and traces live together so optimisation has first-class data to work
with. - **Data-first ergonomics.** Traces, metrics, and evaluation sets
are tidy data frames that flow through `dplyr`, `ggplot2`, `targets`,
`vitals`, and Quarto alike. - **Composable tooling.** Lean on ellmer for
LLM clients/tools, r-lib for developer UX, tidymodels for tuning, and
vitals for rigorous evaluation.

------------------------------------------------------------------------

### 2. Core Architecture

#### 2.1 Signatures (S7)

- `Signature` remains an S7 class in `R/signature.R` describing
  `inputs`, `output_type`, and `instructions`.
- `validate_io(signature, inputs, outputs)` enforces the contract at
  runtime (module `$forward()` and
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) reuse
  it).
- Pillar-friendly printing highlights argument names, expected types,
  and guidance for downstream tooling.

#### 2.2 Modules (R6)

- R6 base `Module` owns:
  - `signature`: immutable reference to an S7 `Signature`.
  - `config`: structured list capturing tunables, defaults, and
    optimisation metadata.
  - `state`: reference list storing mutable artefacts (cached demos,
    fitted recipes, traces, vitals hooks).
- Core methods:
  - `$forward(batch, .llm = NULL, trace = TRUE, ...)`: run predictions,
    returning a tibble with `output`, `trace`, `metadata`, and the
    echoed inputs.
  - `$optimize(devset, objective, control)`: tidy interface for tuning;
    delegates to tidymodels helpers.
  - `$reset(hard = FALSE)`: clear mutable state/config to prepare for
    fresh optimisation.
  - `$print()` / `$format()` / pillar methods: show signature, most
    recent config, trace summary, optimisation status.
  - `$as_vitals_solver(...)`: thin wrapper for
    [`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md)
    so the vitals bridge is discoverable.
- Subclasses: `PredictModule`, `ReactModule`, `ChainOfThoughtModule`,
  etc. Each subclass implements `private$build_prompt()`,
  `private$postprocess()`, and tool orchestration hooks.

#### 2.3 Ellmer Tools

- Tool definitions live in `R/tools-ellmer.R` and expose typed
  requests/responses.
- Modules declare required tools via `self$config$tools`; traces capture
  tool usage (arguments, latency, tokens, outcome state).

#### 2.4 Teleprompters & Optimisers

- Teleprompters become light-weight strategy objects that call
  `$optimize()` with preferences/messages.
- Existing grid-search logic migrates to tidymodels (see §5) but retains
  compatibility with vitals scorers via
  [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md).

------------------------------------------------------------------------

### 3. Module Lifecycle & Developer UX

1.  **Define signature.**
    `sig <- signature("question -> answer: string", instructions = "Be concise.")`
2.  **Instantiate module.**
    `mod <- module(sig, type = "predict", template = "Q: {question}\nA:")`
    returns R6 PredictModule
3.  **Forward pass.** `run(mod, question = "…")` delegates to
    `mod$forward()` and returns results (simple or structured format)
4.  **Optimise.** `compile_module(mod, teleprompter, trainset)`
    optimizes and stores config in `mod$state` and `mod$config`
5.  **Evaluate.** `evaluate(mod, dataset, metric = function(...))` runs
    module and applies metrics per example
6.  **Interop.** `as_vitals_solver(mod)` generates a vitals solver
    function; `as_dsprrr_metric(vitals::scorer)` converts scorers
7.  **Persist.** Use `pins` and `MLflow` helpers to save signatures,
    configs, traces, and vitals run metadata.

Pipe ergonomics remain intact because the exported verbs
([`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md))
accept the module as the first argument and return tidy outputs or the
module itself for chaining.

------------------------------------------------------------------------

### 4. tidyverse & r-lib Integration

- **tibble/dplyr/tidyr/purrr**: All traces, tuning results, evaluation
  outputs, and vitals adapters return tibbles; `purrr` drives
  deterministic optimisation loops and batching.
- **stringr/glue**: Prompt construction, logging, and diagnostics.
- **cli/pillar**: Styled output for signatures, modules, optimisation
  progress, and vitals bridges (display solver status, scorer names).
- **rlang**: Condition system, argument validation (`check_installed()`,
  classed errors), quasiquotation for templating.
- **vctrs**: Stable column types for trace steps and configuration
  parameters to keep joins predictable.
- **lifecycle**: Manage API evolution; mark deprecated S7 helpers once
  R6 rollout is complete.
- **usethis/devtools/roxygen2/testthat/lintr/waldo**: Package
  scaffolding, docs, testing, linting, diffing.

------------------------------------------------------------------------

### 5. Tidymodels Strategy

#### Phase 1 – Pragmatic Grid Search

- Define tunables with
  [`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
  (`temperature()`, `top_p()`, `cot_depth()`, `values_set()` for tool
  choices).
- Generate candidates via `grid_regular()` / `grid_random()`.
- Resample with
  [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html)
  or `bootstraps()` on developer datasets.
- Score with
  [`yardstick::metric_set()`](https://yardstick.tidymodels.org/reference/metric_set.html)
  (including metrics derived from vitals scorers via
  [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)).
- Implement in `$optimize_grid(devset, metrics, control)`; expose
  `optimize_grid(mod, ...)` wrapper for piping.
- Optional `finetune::tune_race_anova()` for adaptive pruning.
  - **Tasks:**
    - Create `R/optimize.R` with helpers
      [`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md),
      `module_grid()`, and `collect_trials()` that operate on R6
      modules.
    - Refactor `R/teleprompter.R` grid search logic to call the new
      helpers instead of hand-rolled loops.
    - Update `tests/testthat/test-compile.R` to exercise tidy grid
      search with a deterministic mock LLM and verify tidy outputs.
    - Add documentation chunks in `vignettes/optimisation.Rmd` showing
      [`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
      and
      [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html)
      usage.

#### Phase 2 – Native tune/workflows

- Define parsnip spec for `"dsprrr_module"` and custom engine.
- Support `recipes::recipe()` preprocessing (retrieval features, prompt
  augmentation).
- Provide `workflows::workflow()` helpers bundling recipe + module spec.
- Expose `tune::tune_grid()` / `tune_bayes()` for Bayesian optimisation
  and `stacks::stacks()` for ensembling module variants.
  - **Tasks:**
    - Register the model via `parsnip::set_new_model()` in
      `R/optimize.R` and implement the corresponding `fit.model_spec()`
      method.
    - Add a `prep_recipe()` helper that accepts a module signature and
      builds default recipes for text/token columns.
    - Write integration tests in `tests/testthat/test-compile.R`
      (skipped on CRAN) that demonstrate a `workflow()` with a dummy
      recipe and confirm `tune::tune_grid()` runs end-to-end using the
      mock LLM.
    - Expand `vignettes/optimisation.Rmd` with a section on
      workflows/tune/stacks, including guidance for vitals scorer
      interoperability.

------------------------------------------------------------------------

### 6. vitals Integration

Current state: -
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md)
wraps
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
for Predict modules and is already exported with tests. -
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)
converts vitals scorers to dsprrr-compatible metrics; README + vignette
document the workflow. - `vitals-integration.Rmd` shows end-to-end
optimisation + evaluation across both packages.

Next steps with the R6 refactor: - Update
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md)
to accept R6 modules (call `$forward()` or `$run_dataset()` shim) while
keeping the public API stable. - Teach modules to expose vitals
metadata: standardise structured return fields (`result`, `.chat`,
`.metadata`, `.trace`) so vitals Tasks can log solver runs without
guesswork. - Ensure
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
returns a tibble whose columns match vitals expectations (`result`,
`.metadata`, `.chat`, trace summary columns) and is reused by both
dsprrr and vitals bridges. - Add `$as_vitals_solver()` method on
`Module` for discoverability, delegating to the existing helper. -
Extend optimisation control to accept vitals scorers directly (e.g.,
`objective = vitals::scorer_modelgraded()`) by auto-wrapping with
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md). -
Sync logging: expose a `vitals_log_format()` helper that collapses
traces into vitals-friendly step logs (tool call, tokens, latency,
outcome) for use in Inspect dashboards. - Refresh
`VITALS_INTEGRATION.md` and vignette with R6 examples, including: module
creation,
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md)
usage, running vitals Tasks, and feeding vitals scorers into
`$optimize()`.

Longer-term opportunities: - Allow teleprompters to request evaluation
via vitals Tasks (e.g., GEPA loop selecting the best config using vitals
scorers and datasets). - Explore a `dsprrr.vitals` extension that
bundles joint pipelines (`targets` template using vitals tasks plus
dsprrr tuning). - Coordinate with vitals maintainers on shared trace
schema or even shared S7 components if the overlap deepens.

------------------------------------------------------------------------

### 7. Orchestration, Artifacts, Observability

- **targets**: canonical orchestration layer. Pipelines load data,
  instantiate modules, run optimisation/evaluation, call vitals tasks,
  and render reports. Provide `_targets.R` templates with optional
  vitals steps.
- **pins**: store signatures, configs, prompt templates, best traces,
  and vitals run summaries. Helpers:
  [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
  [`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
  [`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md).
- **MLflow (optional)**: log params, metrics, traces, vitals scores, and
  artifacts. Provide `use_mlflow()` helper to initialise runs.
- **Quarto**: experiment reports summarising tuning grids, metrics,
  cost, and vitals comparisons. Provide `.qmd` templates fed by targets
  outputs.

------------------------------------------------------------------------

### 8. Package Layout

    R/
      signature.R          # S7 signatures + validation helpers
      module-base.R        # R6 Module base class + utilities
      module-predict.R     # PredictModule subclass
      module-react.R       # Tool-aware module (future)
      module-utils.R       # Prompt builders, post-processors, validation
      tools-ellmer.R       # Typed tool definitions + adapters
      optimize.R           # Tidymodels glue (grid, tune, finetune)
      evaluate.R           # Evaluation generics returning tidy outputs
      traces.R             # Trace tibble constructors + pillar printers
      vitals.R             # as_vitals_solver(), as_dsprrr_metric(), log adapters
      metrics-yardstick.R  # Yardstick metric registry + wrappers
      orchestration.R      # targets/pins/MLflow helpers (incl. vitals logging)
      printing.R           # cli/pillar formatting utilities
      utils.R              # Shared rlang/stringr helpers
    inst/templates/
      targets/_targets.R   # Experiment pipeline (with vitals optional steps)
      quarto/report.qmd    # Experiment report template
    vignettes/
      signatures.Rmd
      modules.Rmd
      optimisation.Rmd
      vitals-integration.Rmd
      orchestration.Rmd

------------------------------------------------------------------------

### 9. Roadmap & Milestones

#### Milestone A – Module Foundations ✅ COMPLETED

- Implement R6 `Module` base + `PredictModule` subclass.
- Update
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  constructor to return R6 objects; retire S7 cloning helpers.
- Rework
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)/[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)/[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
  to delegate to `$forward()` and emit standard trace/vitals columns.
- Add pillar/cli printing and ensure tests cover forward/evaluate paths.
- Adapt
  [`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md)
  and
  [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)
  to the new module internals; expand tests to cover structured outputs
  and trace metadata.
  - **Completed Implementation:**
    - ✅ Created `R/module-base.R` with R6 `Module` base class including
      `$forward()`, `$reset()`, `$optimize()`, `$trace_summary()`,
      `$is_compiled()`, `$as_vitals_solver()`
    - ✅ Created `R/module-predict.R` with `PredictModule` subclass,
      migrated template/demos logic from S7 Predict
    - ✅ Updated
      [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
      factory to return R6 PredictModule instances
    - ✅ Removed S7 Predict class and associated methods (`reset_copy`,
      `deepcopy`, `is_compiled` now R6 methods)
    - ✅ Refactored
      [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) to
      use standard S3 dispatch with `run.Module` method
    - ✅ Updated
      [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
      to work with R6 modules via standard S3 dispatch
    - ✅ Modified vitals integration to check for Module class
      inheritance
    - ✅ Implemented R6 print methods with cli formatting
    - ✅ Cleaned up NAMESPACE - R6 classes marked as internal with
      `@noRd`
    - ✅ Moved R6 from Suggests to Imports in DESCRIPTION
    - ✅ Updated all tests to use R6 API
    - ✅ Fixed documentation generation issues with R6 classes
  - **Quality Improvements (Dec 2024):**
    - ✅ Refactored `run_batch()` to eliminate ~200 lines of code
      duplication
    - ✅ Extracted `process_batch_item()`, `extract_simple_output()`,
      `create_error_result()` helpers
    - ✅ Split into `run_batch_sequential()` and `run_batch_parallel()`
      for clarity
    - ✅ Added comprehensive parallel execution tests
    - ✅ Expanded `test-vitals.R` from 59 → 340 lines
    - ✅ Expanded `test-utils.R` from 20 → 227 lines
    - ✅ Fixed memory leak: chat object storage now optional via
      `config$store_chat_in_traces`
    - ✅ Improved `Module$run()` with proper batch processing and
      warning for parallel misuse

#### Milestone B – Tidymodels Integration ✅ COMPLETED

- Define tunable parameters with `dials` and implement grid/random
  search in `$optimize_grid()`.
- Integrate `rsample` resampling and `yardstick` scoring; ensure vitals
  scorers can be wrapped automatically.
- Surface `finetune` racing as an optional control method.
- Document optimisation workflow + vitals scorer interoperability in
  vignette and Quarto template.
  - **Tasks & Status:**

    | Task                                                                                               | Status       | Notes                                                                                                                                                                                                                      |
    |----------------------------------------------------------------------------------------------------|--------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
    | Implement `$optimize_grid()` and `$optimize()` dispatch                                            | ✅ COMPLETED | Baseline grid search with state tracking                                                                                                                                                                                   |
    | Helper functions in `R/optimize.R`                                                                 | ✅ COMPLETED | [`merge_optimization_control()`](https://jameshwade.github.io/dsprrr/reference/merge_optimization_control.md), [`prepare_candidate_grid()`](https://jameshwade.github.io/dsprrr/reference/prepare_candidate_grid.md), etc. |
    | Refactor `GridSearchTeleprompter`                                                                  | ✅ COMPLETED | Delegates to [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md), records trials/best variant                                                                                              |
    | Tests for dials grids + yardstick metrics                                                          | ✅ COMPLETED | Deterministic specs for delegation and state                                                                                                                                                                               |
    | Update docs (README/vignettes)                                                                     | ✅ COMPLETED | Vignette updated with tidymodels helpers                                                                                                                                                                                   |
    | `finetune::tune_race_anova()` regression test                                                      | ✅ COMPLETED | Integration tests in test-integration.R                                                                                                                                                                                    |
    | [`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md) helper | ✅ COMPLETED | Derives dials parameters from module                                                                                                                                                                                       |
    | [`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md) helper         | ✅ COMPLETED | Tidy summary of optimization trials                                                                                                                                                                                        |
    | [`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md) helper       | ✅ COMPLETED | Per-trial yardstick metrics                                                                                                                                                                                                |
    | Signature → parameter mapping                                                                      | ✅ COMPLETED | Extracts enum types from signature inputs                                                                                                                                                                                  |

#### Milestone C – Orchestration & Persistence ✅ COMPLETED

- Ship `pins` helpers for saving module configs/traces/vitals logs.
- Provide targets template with both dsprrr tuning and vitals evaluation
  stages.
- Optional MLflow logging layer with toggled dependency.
- Add Quarto report template summarising tidymodels results and vitals
  scores.
  - **Tasks & Status:**

    | Task                                                                                                                                                                                                                                                                      | Status       | Notes                                                     |
    |---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------|-----------------------------------------------------------|
    | Implement [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md), [`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md), [`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md) | ✅ COMPLETED | Full R/orchestration.R with restore helpers               |
    | Add `_targets.R` template                                                                                                                                                                                                                                                 | ✅ COMPLETED | inst/templates/targets/\_targets.R with complete pipeline |
    | Create Quarto report template                                                                                                                                                                                                                                             | ✅ COMPLETED | inst/templates/quarto/report.qmd with visualizations      |
    | Update pkgdown configuration                                                                                                                                                                                                                                              | ✅ COMPLETED | Articles and reference sections configured                |
    | Add integration tests for pins helpers                                                                                                                                                                                                                                    | ✅ COMPLETED | Round-trip tests, skipped when pins not installed         |
    | Document in vignettes/orchestration.Rmd                                                                                                                                                                                                                                   | ✅ COMPLETED | Comprehensive vignette with examples                      |
    | [`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md) helper                                                                                                                                                                    | ✅ COMPLETED | Copy templates to user projects                           |
    | [`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md) helper                                                                                                                                                                | ✅ COMPLETED | Reconstruct modules from pinned configs                   |
    | [`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md) helper                                                                                                                                                                        | ✅ COMPLETED | Pre-flight checks for production workflows                |

#### Milestone D – Advanced Modules & Teleprompters (ongoing)

- Implement Chain-of-Thought and tool-aware module subclasses.
- Port DSPy 3.0 optimizers (MIPROv2, SIMBA, GEPA) as teleprompters.
- Add programmatic search teleprompters once tool-aware traces are
  stable.
- Introduce routing/judge modules with enforced signature/type checks.
  - **Tasks:**
    - Scaffold `R/module-chainofthought.R` and `R/module-react.R` with
      subclass implementations, reusing base hooks for prompt assembly
      and trace logging.
    - Define a tool schema registry in `R/tools-ellmer.R` and extend
      traces to capture action/observation steps compatible with vitals
      logs.
    - Implement advanced teleprompters in `R/teleprompter.R`:
      - `MIPROv2Teleprompter`: bootstrapping + grounded proposal stages
        (based on DSPy 3.0)
      - `SIMBATeleprompter`: learns from custom feedback for
        agentic/long-horizon tasks
      - `GEPATeleprompter`: Genetic-Pareto optimizer with NL reflection
        for shorter, better prompts
    - Add tests in `tests/testthat/test-teleprompter.R` covering
      advanced optimizer behaviours with mocked outputs.
    - Update `vignettes/modules.Rmd` and
      `vignettes/vitals-integration.Rmd` with examples of tool-aware
      modules and vitals evaluation.

#### Milestone E – Ecosystem Integration (NEW)

- Integrate with ellmer 0.3.0+ features (simplified chat API, enhanced
  tool specs).
- Add shinychat support for building chat UIs with dsprrr modules.
- Native observability integration with MLflow tracing.
  - **Tasks:**
    - Update ellmer integration to use new `chat()` function and tool
      enhancements from 0.3.0+
    - Create `as_shinychat_solver()` helper for integrating modules with
      shinychat apps
    - Add `R/shinychat.R` with UI helpers for module-powered chatbots
    - Implement `use_mlflow()` helper for native observability (tracing,
      optimizer tracking)
    - Add querychat integration example showing data Q&A with dsprrr
      modules
    - Update vignettes with shinychat and MLflow examples

#### Milestone F – DSPy 3.0 Feature Parity

- Bring dsprrr up to date with DSPy 3.0 capabilities.
  - **New Modules:**
    - `CodeActModule`: code execution with tool-aware agent loops
    - `RefineModule`: iterative refinement with feedback
    - Improved ReAct with better action/observation handling
  - **New Features:**
    - PEP 604-style union types in signatures (e.g., `string | null`)
    - Token streaming support via ellmer async
    - `dsprrr.syncify()` for running optimizers on async programs
  - **Optimizers:**
    - GRPO: reinforcement learning training of compound AI systems
    - Bayesian optimization via `tune::tune_bayes()`

------------------------------------------------------------------------

### 10. Testing & Quality

- **Contract tests**: signatures reject bad inputs; modules validate
  arguments; trace tibbles have stable schemas consumed by vitals and
  tidymodels.
- **Golden tests**: snapshot representative prompts, outputs, traces,
  and optimisation histories with tolerant matchers.
- **Optimisation tests**: run grid search on mock LLM returning
  deterministic outputs to verify yardstick + vitals scorers
  integration.
- **vitals smoke tests**: round-trip
  [`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md)
  through a vitals Task stub to ensure outputs/logging stay compatible.
- **Performance smoke**: assert `$forward()` latency and cost stay
  within bounds on fixture data.
- **Linting & style**: enforce tidyverse style (lintr, styler) and
  maintain high coverage via covr.
- **CI pipelines**: run unit tests, optimisation smoke tests, vitals
  adapters, lint, and pkgdown builds on every PR.

------------------------------------------------------------------------

### 11. Current Status & Next Steps

**Completed:** - ✅ Milestone A: Full R6 module architecture with
quality improvements - ✅ Milestone B: Tidymodels integration with
dials, yardstick, finetune compatibility - ✅ Milestone C: Orchestration
& persistence with pins, targets, Quarto templates - ✅ Vitals
integration maintained and functional - ✅ Documentation stable (4
vignettes: getting-started, compilation-optimization,
vitals-integration, orchestration) - ✅ Test suite comprehensive (run,
vitals, utils, integration, orchestration coverage) - ✅ Code quality:
eliminated duplication, fixed memory leak, improved batch processing

**Ready for:** - Milestone D: Building more module types
(ChainOfThought, React, CodeAct) - Milestone D: Advanced optimizers
(MIPROv2, SIMBA, GEPA) - Milestone E: Shinychat integration for chat
UIs - Milestone E: MLflow native observability

### 11.1 Ecosystem Dependencies

| Package        | Min Version | Status     | Integration Notes                                    |
|----------------|-------------|------------|------------------------------------------------------|
| **ellmer**     | 0.3.0+      | ✅ Active  | New `chat()` API, enhanced tools; update integration |
| **vitals**     | 0.1.0+      | ✅ Active  | Task framework with datasets/solvers/scorers         |
| **shinychat**  | 0.3.0+      | ⏳ Planned | Chat UI for module-powered chatbots                  |
| **querychat**  | latest      | ⏳ Planned | Data Q&A integration                                 |
| **tidymodels** | current     | ✅ Active  | dials, rsample, yardstick, finetune                  |
| **mlflow**     | optional    | ⏳ Planned | Native observability                                 |

**Recent Ecosystem Updates to Track:** - ellmer 0.3.0 (Jul 2025):
Simplified `chat()` function, enhanced tool specs, quality-of-life
improvements - ellmer 0.2.0 (May 2025): Tool calling improvements, async
support (Garrick Aden-Buie joined team) - vitals 0.1.0 (Jun 2025): Task
framework port of Python Inspect, Inspect log viewer - shinychat 0.3.0
(Nov 2025): Streaming markdown, custom content types, ellmer 0.3.0+
integration

**DSPy 3.0 Features to Port:** - MIPROv2: Bootstrapping + grounded
proposal with automatic hyperparameter selection - SIMBA: Custom
feedback learning for agentic tasks - GEPA: Genetic-Pareto optimizer
with NL reflection - CodeAct: Code execution in agent loops - Refine:
Iterative refinement module - PEP 604 union types in signatures

### 12. Open Questions & Follow-Ups

- How opinionated should the `Module` base be about tool orchestration
  vs leaving it to subclasses?
- What default metric set should we export (yardstick vs vitals scorers)
  and where do we draw the line between built-in vs user-defined
  options?
- Which orchestration patterns should be first-class (single module vs
  multi-module programs) and how do we surface vitals Tasks within
  `targets` templates?
- When do we turn on MLflow logging by default vs keeping it opt-in?
- How do we balance rich trace logging with cost/log volume concerns,
  especially when vitals also records solver trajectories?
- Do we evolve towards a shared trace schema/package with vitals or keep
  adapters in `dsprrr`?

------------------------------------------------------------------------

### 13. Progress Log

**Dec 2025 (Milestone C):** - Completed Milestone C - Orchestration &
Persistence: - Created `R/orchestration.R` with pins helpers: -
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
/
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)
for saving/loading modules -
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md)
for persisting execution traces -
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md)
for evaluation results -
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)
for pre-flight checks -
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md)
for copying templates - Created `inst/templates/targets/_targets.R` with
complete pipeline template - Created `inst/templates/quarto/report.qmd`
with visualization report - Created `vignettes/orchestration.Rmd` with
comprehensive documentation - Updated `_pkgdown.yml` with articles and
reference sections - Added 25 tests for orchestration helpers (skipped
when pins not installed) - Updated DESCRIPTION with pins, targets,
tarchetypes, quarto in Suggests

**Dec 2025 (continued):** - Completed Milestone B - Tidymodels
Integration: - Updated optimization vignette with comprehensive
tidymodels helper documentation - Added sections for
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md),
[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md),
[`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md) -
Added complete tidymodels workflow example in vignette - Added
finetune::tune_race_anova() integration tests in test-integration.R -
Verified signature → parameter mapping with
`signature_parameter_defaults()` - All tidymodels helpers tested with
dials grid functions (regular, random, latin_hypercube)

**Dec 2025:** - Completed major Milestone A quality improvements: -
Refactored `run_batch()` eliminating ~200 lines of duplicated code -
Extracted reusable helpers: `process_batch_item()`,
`extract_simple_output()`, `create_error_result()` - Split
parallel/sequential execution into separate functions for clarity -
Fixed memory leak by making chat object storage optional
(`config$store_chat_in_traces`) - Expanded test coverage: test-vitals.R
(59→340 lines), test-utils.R (20→227 lines) - Added comprehensive
parallel execution tests - Improved `Module$run()` with proper batch
processing - Updated CLAUDE.md with comprehensive codebase
documentation - Reviewed DSPy 3.0 features for future integration
(MIPROv2, SIMBA, GEPA, CodeAct) - Researched ecosystem updates: ellmer
0.3.0, vitals 0.1.0, shinychat 0.3.0

**Previous:** - Milestone B kickoff: tidymodels integration planning
begun with shared tracker and dependency mapping. - Implemented base
`$optimize_grid()` flow with stateful trial tracking and helper
scaffolding. - Realigned ellmer integrations to the latest API (shared
`api_args` flow, `chat_claude()` usage) and updated docs/tests
accordingly. - GridSearch teleprompter now delegates to
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md);
module state stores trials/best variant and tests cover the new path. -
Added
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md)/[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md)
helpers to bridge tidymodels parameter sets and optimisation summaries.

------------------------------------------------------------------------

### 14. References

- [DSPy Framework](https://dspy.ai/) - The framework for programming
  language models
- [DSPy GitHub](https://github.com/stanfordnlp/dspy) - Source code and
  releases
- [DSPy Optimizers](https://dspy.ai/learn/optimization/optimizers/) -
  MIPROv2, SIMBA, GEPA documentation
- [ellmer Package](https://ellmer.tidyverse.org/) - Chat with LLMs in R
- [ellmer 0.3.0
  Release](https://tidyverse.org/blog/2025/07/ellmer-0-3-0/) - Latest
  features
- [vitals Package](https://vitals.tidyverse.org/) - LLM evaluation
  framework
- [vitals
  Introduction](https://tidyverse.org/blog/2025/06/vitals-0-1-0/) -
  Toolkit overview
- [shinychat Package](https://posit-dev.github.io/shinychat/) - Chat UI
  for Shiny
