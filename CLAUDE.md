# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Project Overview

`dsprrr` is an R package for building principled, test-driven, and
optimizable applications using Large Language Models. It implements the
DSP (Declarative Self-improving Language Programs) framework in R,
providing a structured programming model where LLM workflows are treated
as programs that can be systematically improved.

The package uses: - **S7** for immutable `Signature` objects defining
inputs/outputs - **R6** for stateful `Module` objects with mutable
configuration and traces - **ellmer** for LLM API calls with structured
output support - **tidyverse conventions** for data manipulation
(tibbles, pipes, tidy data)

## Development Commands

### Package Development

``` r
devtools::load_all()           # Load package for development
devtools::test()               # Run all tests
testthat::test_file("tests/testthat/test-<name>.R")  # Run specific test
devtools::document()           # Rebuild documentation
devtools::check()              # Full R CMD check
devtools::install()            # Install package locally
```

### Building README and Vignettes

``` r
devtools::build_readme()       # Rebuild README.md from README.Rmd
# For vignettes, use build_rmd() - DO NOT use devtools::build_vignettes()
pkgdown::build_site()          # Build pkgdown site
```

### Continuous Integration

GitHub Actions runs R CMD check across multiple R versions and OS
platforms, with test coverage reporting to Codecov.

### PR Workflow

Before creating a pull request, run these checks:

``` bash
# 1. Format ALL code with air (R/ and tests/)
air format R/ tests/testthat/

# 2. Lint with jarl
jarl check R/

# 3. Run tests
Rscript -e "devtools::test()"

# 4. Run R CMD check (catches codoc mismatches, missing docs, etc.)
Rscript -e "devtools::check()"

# 5. Build pkgdown site (checks _pkgdown.yml coverage)
Rscript -e "devtools::document(); pkgdown::build_site(preview = FALSE)"
```

**Important**: The CI runs `air format --check` on both `R/` and
`tests/` directories. Always format test files too!

When adding new exported functions: 1. Add roxygen documentation with
`@export` 2. For internal functions, use `@noRd` (not
`@keywords internal`) to avoid codoc mismatch warnings with S7 classes
3. Run `devtools::document()` to generate `.Rd` files 4. Add the
function to the appropriate section in `_pkgdown.yml` 5. Rebuild pkgdown
to verify coverage

Create PRs on feature branches:

``` bash
git checkout -b feature/my-feature
# ... make changes ...
git add -A && git commit -m "Description"
git push -u origin feature/my-feature
gh pr create --title "Title" --body "Description"
```

After PR is merged, clean up with:

``` r
usethis::pr_finish()
```

## Core Architecture

### Package Layout

    R/
      signature.R           # S7 Signature class + string parsing
      signature-parser.R    # DSPy-style string notation parser
      signature-transforms.R # Signature transforms (with_reasoning, chain_of_thought)
      input.R               # input() helper for signature definitions
      module-base.R         # R6 Module base class (forward, optimize, traces)
      module-predict.R      # PredictModule subclass for text generation
      module-wrapper.R      # BestOfNModule and RefineModule wrapper classes
      module-multichain.R   # MultiChainComparisonModule for ensemble reasoning
      module.R              # module() factory function
      run.R                 # run() and run_dataset() generics
      evaluate.R            # evaluate() generic for metric computation
      optimize.R            # optimize_grid() and tidymodels helpers
      optimizer-core.R      # Optimizer infrastructure (OptimizerControl, EvalResult, eval_program)
      optimizer-logging.R   # Trial logging and persistence (Trial, TrialLog, JSONL I/O)
      teleprompter.R        # S7 Teleprompter classes (LabeledFewShot, GridSearch)
      compile.R             # compile() generic for teleprompter-based optimization
      metrics.R             # Built-in metrics (exact_match, etc.)
      vitals.R              # as_vitals_solver(), as_dsprrr_metric() bridges
      traces.R              # Trace tibble constructors
      utils.R               # Shared utilities

### Key Classes

**Signature (S7)** - Declarative schema for LLM operations: - `inputs`:
List of
[`input()`](https://jameshwade.github.io/dsprrr/reference/input.md)
specifications - `output_type`: ellmer type object (e.g.,
`type_string()`, `type_enum()`) - `instructions`: System prompt text

``` r
# String notation (DSPy-style)
sig <- signature("question -> answer: string")
sig <- signature("context, question -> answer", instructions = "Be concise")

# Explicit notation
sig <- signature(
  inputs = list(input("text", description = "Text to analyze")),
  output_type = ellmer::type_string(),
  instructions = "Analyze sentiment"
)
```

**Module (R6)** - Stateful execution unit: - `signature`: Immutable S7
Signature reference - `config`: Mutable configuration (temperature,
template, etc.) - `state`: Mutable runtime state (traces, trials,
compilation status) - `forward()`: Execute on batch input, return tibble
with output/metadata -
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md):
Run grid search over parameter configurations - `is_compiled()`: Check
if module has been optimized

``` r
mod <- module(sig, type = "predict")
result <- run(mod, question = "What is 2+2?", .llm = llm)
```

**PredictModule (R6)** - Subclass for text generation: - `template`:
glue template for prompt construction - `demos`: List of few-shot
examples - `apply_optimization_params()`: Hook for updating state after
optimization

### Core Generics

| Function                                  | Purpose                          |
|-------------------------------------------|----------------------------------|
| `run(module, ...)`                        | Execute module with named inputs |
| `run_dataset(module, dataset, ...)`       | Batch execute on data frame      |
| `evaluate(module, dataset, metric)`       | Compute metrics on dataset       |
| `optimize_grid(module, devset, metric)`   | Grid search optimization         |
| `compile(teleprompter, module, trainset)` | Teleprompter-based optimization  |

### Teleprompters (S7)

Optimization strategies that compile modules: - `LabeledFewShot`: Add k
examples from training set as demonstrations - `GridSearchTeleprompter`:
Search over instruction/template variants

``` r
tp <- LabeledFewShot(k = 4L, metric = metric_exact_match())
compiled <- compile(tp, mod, trainset)
```

### Vitals Integration

Bridge to the `vitals` package for evaluation: -
`as_vitals_solver(module)`: Convert module to vitals-compatible solver -
`as_dsprrr_metric(vitals_scorer)`: Adapt vitals scorer for dsprrr
metrics

## Key Patterns

### Creating and Running Modules

``` r
# 1. Define signature
sig <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')")

# 2. Create module
mod <- module(sig, type = "predict")

# 3. Run with LLM
llm <- ellmer::chat_openai()
result <- run(mod, text = "I love this!", .llm = llm)

# 4. Batch processing
results <- run(mod, text = c("Great!", "Terrible!"), .llm = llm)
```

### Optimization Workflow

``` r
# Grid search with custom parameters
mod$optimize_grid(
  devset = train_data,
  metric = metric_exact_match(),
  parameters = list(prompt_style = c("concise", "detailed")),
  objective = "maximize"
)

# Check optimization results
module_trials(mod)
module_metrics(mod)
```

### Evaluation

``` r
eval_result <- evaluate(mod, test_data, metric = metric_exact_match())
# Returns: mean_score, scores, predictions, metadata, n_evaluated, n_errors
```

## Testing Patterns

### Test Structure

- Unit tests in `tests/testthat/test-*.R`
- VCR cassettes for HTTP recording in `tests/_vcr/`
- Integration tests often skip on CRAN (`skip_on_cran()`)

### Key Test Conventions

``` r
# Mock LLM for deterministic testing
mock_llm <- list(
  chat_structured = function(prompt, type, ...) {
    list(answer = "mocked response")
  }
)

# Test module behavior
test_that("module returns expected output", {
  sig <- signature("q -> a")
  mod <- module(sig, type = "predict")
  result <- mod$forward(list(q = "test"), .llm = mock_llm)
  expect_s3_class(result, "tbl_df")
})
```

### VCR Cassettes (HTTP Recording)

Integration tests use [vcr](https://docs.ropensci.org/vcr/) to record
and replay HTTP interactions with LLM APIs. This allows tests to run
without API keys and ensures reproducible results.

**Cassette locations:** - `tests/_vcr/` - Test cassettes (OpenAI) -
`vignettes/_vcr/` - Vignette cassettes (Anthropic Claude)

**Recording cassettes:**

``` bash
# Use the helper script (requires API keys set)
Rscript inst/scripts/record-cassettes.R
```

Or interactively:

``` r
source("inst/scripts/record-cassettes.R")
main()  # Run recording
```

**When to re-record:** - After changing API request format (prompt
templates, output types) - After upgrading ellmer (may change request
structure) - When cassettes become stale (API response format changes)

**VCR in tests:**

``` r
test_that("integration test with cassette", {
  skip_if_not_installed("vcr")
  cassette_file <- testthat::test_path("_vcr", "my-test.yml")
  skip_if_not(file.exists(cassette_file), "VCR cassette not recorded")

  vcr::local_cassette("my-test")
  llm <- ellmer::chat_openai(model = "gpt-4o-mini")
  # ... test code
})
```

**VCR in vignettes:** Vignettes use
[`vcr::setup_knitr()`](https://docs.ropensci.org/vcr/reference/setup_knitr.html)
which automatically names cassettes based on chunk labels. Set
`eval = FALSE` for chunks that don’t need recording.

## Implementation Status

### Completed (Milestone A & B)

- R6 Module base class with `forward()`,
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md),
  `reset()`, trace methods
- PredictModule subclass with template and demo support
- ReactModule subclass with tool support (ReAct-style agents)
- S7 Signature with DSPy-style string parsing
- ellmer integration via `chat_structured()`
- Chat-centric API:
  [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md),
  [`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md),
  default Chat management
- Streaming support: `mod$stream()` with callbacks or generators
- Async support:
  [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
  [`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
  with promises
- vitals bridges (`as_vitals_solver`, `as_dsprrr_metric`)
- Grid search optimization with tidymodels parameter support
- LabeledFewShot and GridSearchTeleprompter
- [`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md),
  [`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md),
  and
  [`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md)
  helpers
- Module persistence:
  [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
  [`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)

### Completed (Milestone C - Advanced Module Types)

- ChainOfThought via signature transforms
  ([`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md),
  [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md))
- BestOfN wrapper module with reward functions
- Refine wrapper module with feedback loop
- MultiChainComparison module for ensemble reasoning
- [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  factory support for `type = "multichain"`
- Utility functions:
  [`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
  [`has_reasoning()`](https://jameshwade.github.io/dsprrr/reference/has_reasoning.md),
  [`without_reasoning()`](https://jameshwade.github.io/dsprrr/reference/without_reasoning.md)

### Completed (Milestone D - Optimizer Infrastructure)

- `OptimizerControl` (S7): Configuration for optimizer behavior (seed,
  max_trials, max_errors, etc.)
- `EvalResult` (S7): Evaluation result container with per-example and
  aggregated statistics
- `CostSummary` (S7): Cumulative cost tracking across optimizer trials
- `Trial` (S7): Single optimization trial record with metadata and
  results
- `TrialLog` (R6): Collection of trials with JSONL persistence support
- [`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md):
  Standard evaluation function for optimizers
- [`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md)
  /
  [`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md):
  Deterministic sampling with RNG state preservation
- `check_budget()`: Budget stopping condition checker
- [`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)
  /
  [`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md):
  JSONL persistence with error handling

### Planned

- DSPy-inspired optimizers: BootstrapFewShot,
  BootstrapFewShotWithRandomSearch, KNNFewShot
- Advanced teleprompters: COPRO, MIPROv2, SIMBA, GEPA
- Ensemble optimizer combining multiple strategies
- ProgramOfThought (code generation + execution)

## Coding Conventions

### General

- Use tidyverse style (snake_case, pipe-friendly APIs)
- Prefer editing existing files over creating new ones
- Avoid `-old`, `-new`, `-improved` file name suffixes
- Keep changes focused; avoid over-engineering

### R6 Classes

- Mark internal classes with `@keywords internal` and `@noRd`
- Use `public` for user-facing methods, `private` for implementation
  details
- Clone with `$clone(deep = TRUE)` for stateful copies

### S7 Classes

- Export S7 classes that users should construct directly
- Use `@export` for constructors, `@noRd` for internal methods

### Error Handling

- Use
  [`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
  for user-facing errors
- Use
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html)
  for warnings
- Validate inputs early with clear error messages

### Dependencies

Required (Imports): - `cli`, `ellmer`, `glue`, `jsonlite`, `lifecycle`,
`mirai`, `R6`, `rlang`, `S7`, `tibble`

Suggested: - `dials`, `knitr`, `rmarkdown`, `testthat`, `vcr`, `vitals`

## Known Issues

- Some test files have `-improved` suffix that should be renamed
- Minor test failures related to deepcopy state preservation
- For internal S7 classes with complex default values, use `@noRd`
  instead of `@keywords internal` to avoid R CMD check codoc mismatch
  warnings

## File References

- **PLAN.md**: Detailed roadmap with milestones and task tracking
- **VITALS_INTEGRATION.md**: Documentation for vitals package
  integration
- **inst/scripts/record-cassettes.R**: Helper script for re-recording
  VCR cassettes
- **vignettes/**: User-facing tutorials
  - `getting-started.Rmd`: Introduction and basic usage
  - `compilation-optimization.Rmd`: Optimization workflow
  - `vitals-integration.Rmd`: Vitals bridge usage
  - `orchestration.Rmd`: Production workflow patterns
