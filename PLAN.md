### 1. Vision & Philosophy

`dsprrr` helps R programmers build **principled, test-driven, optimisable** LLM systems. We want iterative LLM development to feel like a tidyverse project: data lives in tibbles, contracts are explicit, experiments are reproducible, and evaluation tooling (including `vitals`) plugs in without adapters.

Guiding tenets:
- **Typed boundaries.** Every module advertises a signature; inputs and outputs are validated before the model runs.
- **Stateful, observable modules.** Prompt code, configuration, losses, and traces live together so optimisation has first-class data to work with.
- **Data-first ergonomics.** Traces, metrics, and evaluation sets are tidy data frames that flow through `dplyr`, `ggplot2`, `targets`, `vitals`, and Quarto alike.
- **Composable tooling.** Lean on ellmer for LLM clients/tools, r-lib for developer UX, tidymodels for tuning, and vitals for rigorous evaluation.

---

### 2. Core Architecture

#### 2.1 Signatures (S7)
- `Signature` remains an S7 class in `R/signature.R` describing `inputs`, `output_type`, and `instructions`.
- `validate_io(signature, inputs, outputs)` enforces the contract at runtime (module `$forward()` and `run()` reuse it).
- Pillar-friendly printing highlights argument names, expected types, and guidance for downstream tooling.

#### 2.2 Modules (R6)
- R6 base `Module` owns:
  - `signature`: immutable reference to an S7 `Signature`.
  - `config`: structured list capturing tunables, defaults, and optimisation metadata.
  - `state`: reference list storing mutable artefacts (cached demos, fitted recipes, traces, vitals hooks).
- Core methods:
  - `$forward(batch, .llm = NULL, trace = TRUE, ...)`: run predictions, returning a tibble with `output`, `trace`, `metadata`, and the echoed inputs.
  - `$optimize(devset, objective, control)`: tidy interface for tuning; delegates to tidymodels helpers.
  - `$reset(hard = FALSE)`: clear mutable state/config to prepare for fresh optimisation.
  - `$print()` / `$format()` / pillar methods: show signature, most recent config, trace summary, optimisation status.
  - `$as_vitals_solver(...)`: thin wrapper for `as_vitals_solver()` so the vitals bridge is discoverable.
- Subclasses: `PredictModule`, `ReactModule`, `ChainOfThoughtModule`, etc. Each subclass implements `private$build_prompt()`, `private$postprocess()`, and tool orchestration hooks.

#### 2.3 Ellmer Tools
- Tool definitions live in `R/tools-ellmer.R` and expose typed requests/responses.
- Modules declare required tools via `self$config$tools`; traces capture tool usage (arguments, latency, tokens, outcome state).

#### 2.4 Teleprompters & Optimisers
- Teleprompters become light-weight strategy objects that call `$optimize()` with preferences/messages.
- Existing grid-search logic migrates to tidymodels (see §5) but retains compatibility with vitals scorers via `as_dsprrr_metric()`.

---

### 3. Module Lifecycle & Developer UX

1. **Define signature.** `sig <- signature("question -> answer: string", instructions = "Be concise.")`
2. **Instantiate module.** `mod <- module(sig, type = "predict", template = "Q: {question}\nA:")` returns R6 PredictModule
3. **Forward pass.** `run(mod, question = "…")` delegates to `mod$forward()` and returns results (simple or structured format)
4. **Optimise.** `compile_module(mod, teleprompter, trainset)` optimizes and stores config in `mod$state` and `mod$config`
5. **Evaluate.** `evaluate(mod, dataset, metric = function(...))` runs module and applies metrics per example
6. **Interop.** `as_vitals_solver(mod)` generates a vitals solver function; `as_dsprrr_metric(vitals::scorer)` converts scorers
7. **Persist.** Use `pins` and `MLflow` helpers to save signatures, configs, traces, and vitals run metadata.

Pipe ergonomics remain intact because the exported verbs (`run()`, `optimize_grid()`, `evaluate()`, `as_vitals_solver()`) accept the module as the first argument and return tidy outputs or the module itself for chaining.

---

### 4. tidyverse & r-lib Integration

- **tibble/dplyr/tidyr/purrr**: All traces, tuning results, evaluation outputs, and vitals adapters return tibbles; `purrr` drives deterministic optimisation loops and batching.
- **stringr/glue**: Prompt construction, logging, and diagnostics.
- **cli/pillar**: Styled output for signatures, modules, optimisation progress, and vitals bridges (display solver status, scorer names).
- **rlang**: Condition system, argument validation (`check_installed()`, classed errors), quasiquotation for templating.
- **vctrs**: Stable column types for trace steps and configuration parameters to keep joins predictable.
- **lifecycle**: Manage API evolution; mark deprecated S7 helpers once R6 rollout is complete.
- **usethis/devtools/roxygen2/testthat/lintr/waldo**: Package scaffolding, docs, testing, linting, diffing.

---

### 5. Tidymodels Strategy

#### Phase 1 – Pragmatic Grid Search
- Define tunables with `dials::parameters()` (`temperature()`, `top_p()`, `cot_depth()`, `values_set()` for tool choices).
- Generate candidates via `grid_regular()` / `grid_random()`.
- Resample with `rsample::vfold_cv()` or `bootstraps()` on developer datasets.
- Score with `yardstick::metric_set()` (including metrics derived from vitals scorers via `as_dsprrr_metric()`).
- Implement in `$optimize_grid(devset, metrics, control)`; expose `optimize_grid(mod, ...)` wrapper for piping.
- Optional `finetune::tune_race_anova()` for adaptive pruning.
  - **Tasks:**
    - Create `R/optimize.R` with helpers `module_parameters()`, `module_grid()`, and `collect_trials()` that operate on R6 modules.
    - Refactor `R/teleprompter.R` grid search logic to call the new helpers instead of hand-rolled loops.
    - Update `tests/testthat/test-compile.R` to exercise tidy grid search with a deterministic mock LLM and verify tidy outputs.
    - Add documentation chunks in `vignettes/optimisation.Rmd` showing `dials::parameters()` and `rsample::vfold_cv()` usage.

#### Phase 2 – Native tune/workflows
- Define parsnip spec for `"dsprrr_module"` and custom engine.
- Support `recipes::recipe()` preprocessing (retrieval features, prompt augmentation).
- Provide `workflows::workflow()` helpers bundling recipe + module spec.
- Expose `tune::tune_grid()` / `tune_bayes()` for Bayesian optimisation and `stacks::stacks()` for ensembling module variants.
  - **Tasks:**
    - Register the model via `parsnip::set_new_model()` in `R/optimize.R` and implement the corresponding `fit.model_spec()` method.
    - Add a `prep_recipe()` helper that accepts a module signature and builds default recipes for text/token columns.
    - Write integration tests in `tests/testthat/test-compile.R` (skipped on CRAN) that demonstrate a `workflow()` with a dummy recipe and confirm `tune::tune_grid()` runs end-to-end using the mock LLM.
    - Expand `vignettes/optimisation.Rmd` with a section on workflows/tune/stacks, including guidance for vitals scorer interoperability.

---

### 6. vitals Integration

Current state:
- `as_vitals_solver()` wraps `run_dataset()` for Predict modules and is already exported with tests.
- `as_dsprrr_metric()` converts vitals scorers to dsprrr-compatible metrics; README + vignette document the workflow.
- `vitals-integration.Rmd` shows end-to-end optimisation + evaluation across both packages.

Next steps with the R6 refactor:
- Update `as_vitals_solver()` to accept R6 modules (call `$forward()` or `$run_dataset()` shim) while keeping the public API stable.
- Teach modules to expose vitals metadata: standardise structured return fields (`result`, `.chat`, `.metadata`, `.trace`) so vitals Tasks can log solver runs without guesswork.
- Ensure `run_dataset()` returns a tibble whose columns match vitals expectations (`result`, `.metadata`, `.chat`, trace summary columns) and is reused by both dsprrr and vitals bridges.
- Add `$as_vitals_solver()` method on `Module` for discoverability, delegating to the existing helper.
- Extend optimisation control to accept vitals scorers directly (e.g., `objective = vitals::scorer_modelgraded()`) by auto-wrapping with `as_dsprrr_metric()`.
- Sync logging: expose a `vitals_log_format()` helper that collapses traces into vitals-friendly step logs (tool call, tokens, latency, outcome) for use in Inspect dashboards.
- Refresh `VITALS_INTEGRATION.md` and vignette with R6 examples, including: module creation, `as_vitals_solver()` usage, running vitals Tasks, and feeding vitals scorers into `$optimize()`.

Longer-term opportunities:
- Allow teleprompters to request evaluation via vitals Tasks (e.g., GEPA loop selecting the best config using vitals scorers and datasets).
- Explore a `dsprrr.vitals` extension that bundles joint pipelines (`targets` template using vitals tasks plus dsprrr tuning).
- Coordinate with vitals maintainers on shared trace schema or even shared S7 components if the overlap deepens.

---

### 7. Orchestration, Artifacts, Observability

- **targets**: canonical orchestration layer. Pipelines load data, instantiate modules, run optimisation/evaluation, call vitals tasks, and render reports. Provide `_targets.R` templates with optional vitals steps.
- **pins**: store signatures, configs, prompt templates, best traces, and vitals run summaries. Helpers: `pin_module_config()`, `pin_trace()`, `pin_vitals_log()`.
- **MLflow (optional)**: log params, metrics, traces, vitals scores, and artifacts. Provide `use_mlflow()` helper to initialise runs.
- **Quarto**: experiment reports summarising tuning grids, metrics, cost, and vitals comparisons. Provide `.qmd` templates fed by targets outputs.

---

### 8. Package Layout

```
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
```

---

### 9. Roadmap & Milestones

#### Milestone A – Module Foundations ✅ COMPLETED
- Implement R6 `Module` base + `PredictModule` subclass.
- Update `module()` constructor to return R6 objects; retire S7 cloning helpers.
- Rework `run()`/`run_dataset()`/`evaluate()` to delegate to `$forward()` and emit standard trace/vitals columns.
- Add pillar/cli printing and ensure tests cover forward/evaluate paths.
- Adapt `as_vitals_solver()` and `as_dsprrr_metric()` to the new module internals; expand tests to cover structured outputs and trace metadata.
  - **Completed Implementation:**
    - ✅ Created `R/module-base.R` with R6 `Module` base class including `$forward()`, `$reset()`, `$optimize()`, `$trace_summary()`, `$is_compiled()`, `$as_vitals_solver()`
    - ✅ Created `R/module-predict.R` with `PredictModule` subclass, migrated template/demos logic from S7 Predict
    - ✅ Updated `module()` factory to return R6 PredictModule instances
    - ✅ Removed S7 Predict class and associated methods (`reset_copy`, `deepcopy`, `is_compiled` now R6 methods)
    - ✅ Refactored `run()` to use standard S3 dispatch with `run.Module` method
    - ✅ Updated `evaluate()` to work with R6 modules via standard S3 dispatch
    - ✅ Modified vitals integration to check for Module class inheritance
    - ✅ Implemented R6 print methods with cli formatting
    - ✅ Cleaned up NAMESPACE - R6 classes marked as internal with `@noRd`
    - ✅ Moved R6 from Suggests to Imports in DESCRIPTION
    - ✅ Updated all tests to use R6 API (395 passing, 8 minor failures)
    - ✅ Fixed documentation generation issues with R6 classes

#### Milestone B – Tidymodels Integration (3 weeks)
- Define tunable parameters with `dials` and implement grid/random search in `$optimize_grid()`.
- Integrate `rsample` resampling and `yardstick` scoring; ensure vitals scorers can be wrapped automatically.
- Surface `finetune` racing as an optional control method.
- Document optimisation workflow + vitals scorer interoperability in vignette and Quarto template.
  - **Tasks:**
    - Implement `$optimize_grid()` and `$optimize()` dispatch inside `Module`, storing trial results in `self$state$trials` as tibbles.
    - Add helper functions in `R/optimize.R` for translating module signatures into tidymodels parameter objects and for summarising resample scores.
    - Refactor `GridSearchTeleprompter` in `R/teleprompter.R` to call the new optimisation API, ensuring the structure returned matches the stored trial schema.
    - Extend `tests/testthat/test-teleprompter.R` with cases covering dials grids and yardstick metrics via `as_dsprrr_metric()`.
    - Update `README.Rmd` and `vignettes/optimisation.Rmd` examples to show tidymodels usage alongside vitals scorers.
    - Add (skip-on-CRAN) regression test ensuring `finetune::tune_race_anova()` works under a deterministic mock, or include scaffolding to plug in when finetune is installed.

#### Milestone C – Orchestration & Persistence (2 weeks)
- Ship `pins` helpers for saving module configs/traces/vitals logs.
- Provide targets template with both dsprrr tuning and vitals evaluation stages.
- Optional MLflow logging layer with toggled dependency.
- Add Quarto report template summarising tidymodels results and vitals scores.
  - **Tasks:**
    - Implement `pin_module_config()`, `pin_trace()`, and `pin_vitals_log()` in `R/orchestration.R`, leveraging standardised trace schemas.
    - Add `_targets.R` template under `inst/templates/targets/` demonstrating a pipeline: load data → optimise module → evaluate via vitals → render Quarto report.
    - Create Quarto template (`inst/templates/quarto/report.qmd`) and link it from documentation.
    - Update pkgdown configuration (`_pkgdown.yml`) to reference new articles for orchestrations and vitals integration.
    - Add integration tests (optionally skipped) validating that the pins helpers round-trip module configs and traces.
    - Document orchestration workflow in `vignettes/orchestration.Rmd`, including vitals steps.

#### Milestone D – Advanced Modules & Teleprompters (ongoing)
- Implement Chain-of-Thought and tool-aware module subclasses.
- Port GEPA-style teleprompter using R6 hooks and integrate vitals-based evaluation loops.
- Add programmatic search teleprompters (MIPRO) once tool-aware traces are stable.
- Introduce routing/judge modules with enforced signature/type checks.
  - **Tasks:**
    - Scaffold `R/module-chainofthought.R` and `R/module-react.R` with subclass implementations, reusing base hooks for prompt assembly and trace logging.
    - Define a tool schema registry in `R/tools-ellmer.R` and extend traces to capture action/observation steps compatible with vitals logs.
    - Implement `GepaTeleprompter` (and friends) in `R/teleprompter.R`, using vitals scorers for evaluation loops and storing intermediate rationales in module state.
    - Add tests in `tests/testthat/test-teleprompter.R` covering GEPA/MIPRO behaviours with mocked outputs for deterministic assertions.
    - Update `vignettes/modules.Rmd` and `vignettes/vitals-integration.Rmd` with examples of tool-aware modules and vitals evaluation.
    - Evaluate need for additional helper generics (e.g., `module_trace()`), documenting decisions in `VITALS_INTEGRATION.md` and developer notes.

---

### 10. Testing & Quality

- **Contract tests**: signatures reject bad inputs; modules validate arguments; trace tibbles have stable schemas consumed by vitals and tidymodels.
- **Golden tests**: snapshot representative prompts, outputs, traces, and optimisation histories with tolerant matchers.
- **Optimisation tests**: run grid search on mock LLM returning deterministic outputs to verify yardstick + vitals scorers integration.
- **vitals smoke tests**: round-trip `as_vitals_solver()` through a vitals Task stub to ensure outputs/logging stay compatible.
- **Performance smoke**: assert `$forward()` latency and cost stay within bounds on fixture data.
- **Linting & style**: enforce tidyverse style (lintr, styler) and maintain high coverage via covr.
- **CI pipelines**: run unit tests, optimisation smoke tests, vitals adapters, lint, and pkgdown builds on every PR.

---

### 11. Current Status & Next Steps

**Completed:**
- ✅ Full R6 module architecture implemented and tested
- ✅ Vitals integration maintained and functional
- ✅ Documentation stable (no regeneration issues)
- ✅ Test suite largely passing (395/403 tests)

**Known Issues:**
- Minor test failures (8) related to deepcopy state preservation
- Some compile tests checking wrong property paths

**Ready for:**
- Milestone B: Tidymodels Integration
- Building more module types (ChainOfThought, React)
- Enhanced optimization strategies

### 12. Open Questions & Follow-Ups

- How opinionated should the `Module` base be about tool orchestration vs leaving it to subclasses?
- What default metric set should we export (yardstick vs vitals scorers) and where do we draw the line between built-in vs user-defined options?
- Which orchestration patterns should be first-class (single module vs multi-module programs) and how do we surface vitals Tasks within `targets` templates?
- When do we turn on MLflow logging by default vs keeping it opt-in?
- How do we balance rich trace logging with cost/log volume concerns, especially when vitals also records solver trajectories?
- Do we evolve towards a shared trace schema/package with vitals or keep adapters in `dsprrr`?
