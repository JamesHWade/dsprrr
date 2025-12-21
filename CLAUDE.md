# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`dsprrr` is an R package for building principled, test-driven, and optimizable applications using Large Language Models. It implements the DSP (Declarative Self-improving Language Programs) framework in R, providing a structured programming model where LLM workflows are treated as programs that can be systematically improved.

The package uses:
- **S7** for immutable `Signature` objects defining inputs/outputs
- **R6** for stateful `Module` objects with mutable configuration and traces
- **ellmer** for LLM API calls with structured output support
- **tidyverse conventions** for data manipulation (tibbles, pipes, tidy data)

## Development Commands

### Package Development
```r
devtools::load_all()           # Load package for development
devtools::test()               # Run all tests
testthat::test_file("tests/testthat/test-<name>.R")  # Run specific test
devtools::document()           # Rebuild documentation
devtools::check()              # Full R CMD check
devtools::install()            # Install package locally
```

### Building README and Vignettes
```r
devtools::build_readme()       # Rebuild README.md from README.Rmd
# For vignettes, use build_rmd() - DO NOT use devtools::build_vignettes()
```

### Continuous Integration
GitHub Actions runs R CMD check across multiple R versions and OS platforms, with test coverage reporting to Codecov.

## Core Architecture

### Package Layout
```
R/
  signature.R           # S7 Signature class + string parsing
  signature-parser.R    # DSPy-style string notation parser
  input.R               # input() helper for signature definitions
  module-base.R         # R6 Module base class (forward, optimize, traces)
  module-predict.R      # PredictModule subclass for text generation
  module.R              # module() factory function
  run.R                 # run() and run_dataset() generics
  evaluate.R            # evaluate() generic for metric computation
  optimize.R            # optimize_grid() and tidymodels helpers
  teleprompter.R        # S7 Teleprompter classes (LabeledFewShot, GridSearch)
  compile.R             # compile() generic for teleprompter-based optimization
  metrics.R             # Built-in metrics (exact_match, etc.)
  vitals.R              # as_vitals_solver(), as_dsprrr_metric() bridges
  traces.R              # Trace tibble constructors
  utils.R               # Shared utilities
```

### Key Classes

**Signature (S7)** - Declarative schema for LLM operations:
- `inputs`: List of `input()` specifications
- `output_type`: ellmer type object (e.g., `type_string()`, `type_enum()`)
- `instructions`: System prompt text

```r
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

**Module (R6)** - Stateful execution unit:
- `signature`: Immutable S7 Signature reference
- `config`: Mutable configuration (temperature, template, etc.)
- `state`: Mutable runtime state (traces, trials, compilation status)
- `forward()`: Execute on batch input, return tibble with output/metadata
- `optimize_grid()`: Run grid search over parameter configurations
- `is_compiled()`: Check if module has been optimized

```r
mod <- module(sig, type = "predict")
result <- run(mod, question = "What is 2+2?", .llm = llm)
```

**PredictModule (R6)** - Subclass for text generation:
- `template`: glue template for prompt construction
- `demos`: List of few-shot examples
- `apply_optimization_params()`: Hook for updating state after optimization

### Core Generics

| Function | Purpose |
|----------|---------|
| `run(module, ...)` | Execute module with named inputs |
| `run_dataset(module, dataset, ...)` | Batch execute on data frame |
| `evaluate(module, dataset, metric)` | Compute metrics on dataset |
| `optimize_grid(module, devset, metric)` | Grid search optimization |
| `compile(teleprompter, module, trainset)` | Teleprompter-based optimization |

### Teleprompters (S7)

Optimization strategies that compile modules:
- `LabeledFewShot`: Add k examples from training set as demonstrations
- `GridSearchTeleprompter`: Search over instruction/template variants

```r
tp <- LabeledFewShot(k = 4L, metric = metric_exact_match())
compiled <- compile(tp, mod, trainset)
```

### Vitals Integration

Bridge to the `vitals` package for evaluation:
- `as_vitals_solver(module)`: Convert module to vitals-compatible solver
- `as_dsprrr_metric(vitals_scorer)`: Adapt vitals scorer for dsprrr metrics

## Key Patterns

### Creating and Running Modules
```r
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
```r
# Grid search with custom parameters
mod$optimize_grid(
  devset = train_data,
  metric = metric_exact_match(),
  parameters = list(prompt_style = c("concise", "detailed")),
  objective = "maximize"
)

# Check optimization results
module_trials_summary(mod)
module_metric_summary(mod)
```

### Evaluation
```r
eval_result <- evaluate(mod, test_data, metric = metric_exact_match())
# Returns: mean_score, scores, predictions, metadata, n_evaluated, n_errors
```

## Testing Patterns

### Test Structure
- Unit tests in `tests/testthat/test-*.R`
- VCR cassettes for HTTP recording in `tests/_vcr/`
- Integration tests often skip on CRAN (`skip_on_cran()`)

### Key Test Conventions
```r
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

## Implementation Status

### Completed (Milestone A)
- R6 Module base class with `forward()`, `optimize_grid()`, `reset()`, trace methods
- PredictModule subclass with template and demo support
- S7 Signature with DSPy-style string parsing
- ellmer integration via `chat_structured()`
- vitals bridges (`as_vitals_solver`, `as_dsprrr_metric`)
- Grid search optimization with tidymodels parameter support
- LabeledFewShot and GridSearchTeleprompter

### In Progress (Milestone B)
- Tidymodels integration refinement
- `module_parameter_set()` and `module_metric_summary()` helpers
- Documentation updates for optimization workflow

### Planned
- Chain-of-Thought and tool-aware module subclasses
- Advanced teleprompters (MIPRO, GEPA)
- Orchestration helpers (pins, targets templates)

## Coding Conventions

### General
- Use tidyverse style (snake_case, pipe-friendly APIs)
- Prefer editing existing files over creating new ones
- Avoid `-old`, `-new`, `-improved` file name suffixes
- Keep changes focused; avoid over-engineering

### R6 Classes
- Mark internal classes with `@keywords internal` and `@noRd`
- Use `public` for user-facing methods, `private` for implementation details
- Clone with `$clone(deep = TRUE)` for stateful copies

### S7 Classes
- Export S7 classes that users should construct directly
- Use `@export` for constructors, `@noRd` for internal methods

### Error Handling
- Use `cli::cli_abort()` for user-facing errors
- Use `cli::cli_warn()` for warnings
- Validate inputs early with clear error messages

### Dependencies
Required (Imports):
- `cli`, `ellmer`, `glue`, `jsonlite`, `lifecycle`, `mirai`, `R6`, `rlang`, `S7`, `tibble`

Suggested:
- `dials`, `knitr`, `rmarkdown`, `testthat`, `vcr`, `vitals`

## Known Issues

- Some test files have `-improved` suffix that should be renamed
- Documentation warnings may appear for R6 classes (expected)
- Minor test failures related to deepcopy state preservation
- CLAUDE.md and PLAN.md should be in .Rbuildignore for CRAN

## File References

- **PLAN.md**: Detailed roadmap with milestones and task tracking
- **VITALS_INTEGRATION.md**: Documentation for vitals package integration
- **vignettes/**: User-facing tutorials
  - `getting-started.Rmd`: Introduction and basic usage
  - `compilation-optimization.Rmd`: Optimization workflow
  - `vitals-integration.Rmd`: Vitals bridge usage
