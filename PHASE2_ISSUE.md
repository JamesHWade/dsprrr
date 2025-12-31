# Phase 2: API Refinements - Comprehensive Redesign

## Overview

Phase 2 transforms dsprrr from a functional DSPy port into a polished,
tidyverse-aligned R package with consistent APIs, predictable return
types, and excellent error messages.

**Builds on:** Phase 1 (PR \#8) \| **Parent issue:** \#6

> **Note:** Since dsprrr is not yet published to CRAN, we can make
> breaking changes directly without deprecation cycles. This is the
> ideal time to get the API right.

------------------------------------------------------------------------

## Executive Summary

Based on comprehensive API analysis, we identified **73 exported
functions** with several categories of issues:

| Category               | Issues Found      | Priority |
|------------------------|-------------------|----------|
| Naming inconsistencies | 8 patterns        | High     |
| Return type variations | 4 major           | High     |
| Parameter naming       | 5 inconsistencies | Medium   |
| Missing functionality  | 8 gaps vs DSPy    | Medium   |
| Error message quality  | Throughout        | Medium   |

------------------------------------------------------------------------

## 2.1 Naming Convention Audit ✅

### Completed Renames

| Old Name                                                                              | New Name                                                                                    | Status  |
|---------------------------------------------------------------------------------------|---------------------------------------------------------------------------------------------|---------|
| `last_trace()`                                                                        | [`get_last_trace()`](https://jameshwade.github.io/dsprrr/reference/get_last_trace.md)       | ✅ Done |
| `last_prompt()`                                                                       | [`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md)     | ✅ Done |
| `module_parameter_set()`                                                              | [`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md) | ✅ Done |
| `module_trials_summary()`                                                             | [`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md)         | ✅ Done |
| `module_metric_summary()`                                                             | [`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md)       | ✅ Done |
| [`compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.md) | (keep - documented)                                                                         | ✅ Done |

### Tasks

**Audit all 73 exports** against tidyverse naming conventions

**Rename functions directly** (no deprecation needed pre-CRAN)

**Update all internal calls** to use new names

**Update tests, vignettes, and examples**

**Document naming rationale** in CLAUDE.md or style guide

------------------------------------------------------------------------

## 2.2 Return Type Consistency

### Problem: `dsp()` Returns Different Types

``` r
# Single-field output: returns the value directly
dsp("q -> answer", q = "What is 2+2?")
#> "4"

# Multi-field output: returns a named list
dsp("q -> answer, confidence", q = "What is 2+2?")
#> list(answer = "4", confidence = 0.95)
```

This implicit simplification is “magic” - users can’t predict what
they’ll get.

### Problem: `run()` Return Type Varies

``` r
# Single input + simple format: just the output
run(mod, text = "hello")

# Single input + structured format: list with metadata
run(mod, text = "hello", .return_format = "structured")

# Batch input: list of outputs or list of structured results
run(mod, text = c("a", "b"))
```

### Tasks

**Add `.simplify` parameter to
[`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md)** -
explicit control over output simplification ✅

``` r
dsp("q -> a", q = "...", .simplify = TRUE)   # Returns value (current default)
dsp("q -> a", q = "...", .simplify = FALSE)  # Always returns list
```

**Standardize
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
returns** - always return consistent structure

``` r
# Always return tibble, even for single input
run(mod, text = "hello")
#> # A tibble: 1 x 3
#>   .input    .output     .metadata
#>   <chr>     <chr>       <list>
#> 1 hello     response    <named list>
```

**Add S3 classes to results** for type safety

- `dsprrr_result` for single results
- `dsprrr_batch_result` for batch results
- `dsprrr_evaluation` for evaluate() results

**Create accessor functions** for structured results

``` r
get_output(result)
get_metadata(result)
get_tokens(result)
get_cost(result)
```

**Add `.return_format` to
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)**
for consistency with
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) ✅

------------------------------------------------------------------------

## 2.3 Parameter Naming Standardization

### Current Inconsistencies

| Context        | Current Names                                   | Issue                              |
|----------------|-------------------------------------------------|------------------------------------|
| Dataset params | `dataset`, `devset`, `trainset`                 | 3 different names for same concept |
| Size limits    | `n`, `k`, (none)                                | Inconsistent across functions      |
| LLM storage    | `mod$chat`, `mod$config$llm`                    | Two locations                      |
| Return format  | `.return_format` in run(), absent in evaluate() | Missing option                     |

### Tasks

**Standardize dataset parameters**

- Use `data` for user-provided datasets (matches tidyverse)
- Keep `trainset` only in
  [`dsp_trainset()`](https://jameshwade.github.io/dsprrr/reference/dsp_trainset.md)
  (it’s creating, not consuming)

**Standardize size/limit parameters**

- Use `n` for “number of items to return”
- Use `k` for “number of examples/shots”

**Remove legacy `config$llm`** - use only `$chat` (ellmer alignment)

**Add missing parameters** to functions for consistency

- `evaluate(..., .return_format = "simple")`
- `export_traces(..., n = NULL)` for limiting output

------------------------------------------------------------------------

## 2.4 Configuration & Parameter Validation

### Leverage ellmer’s Parameter System

ellmer already handles model-specific parameter validation:

``` r
# ellmer/R/params.R - standardise_params()
unknown <- setdiff(names(standard), provider_params)
if (length(unknown) > 0) {
  cli::cli_warn("Ignoring unsupported parameters: {.str {unknown}}")
}
```

**Key insight:** Don’t duplicate this logic in dsprrr. Pass parameters
through to ellmer and let it validate per-provider.

### Tasks

**Document passthrough behavior** - make clear that dsprrr delegates
param validation to ellmer

**Add `dsp_params()` helper** - thin wrapper around
[`ellmer::params()`](https://ellmer.tidyverse.org/reference/params.html)
for discoverability

``` r
dsp_params <- function(temperature = NULL, max_tokens = NULL, ...) {
  ellmer::params(
    temperature = temperature,
    max_tokens = max_tokens,
    ...
  )
}
```

**Improve
[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)**
with comprehensive checks:

``` r
dsprrr_sitrep()
#> dsprrr configuration

#> - ellmer version: 0.2.0 (OK)
#> - Default chat: set (OpenAI/gpt-4o-mini)
#> - API keys: OPENAI_API_KEY found
#> - Prompt history: 12 entries (max 100)
#> - Active modules: 2
```

**Add global options documentation**

``` r
options(
  dsprrr.verbose = TRUE,
  dsprrr.prompt_history_max = 100,
  dsprrr.default_return_format = "simple"
)
```

------------------------------------------------------------------------

## 2.5 Error Message Excellence

### Current State

dsprrr has basic error handling via `wrap_llm_error()` (added in Phase
1). However, errors could be more actionable.

### Patterns to Adopt (from ellmer)

``` r
# Multi-line errors with context
cli::cli_abort(c(
"Missing required inputs",
  "x" = "Missing: {.field {missing_fields}}",
  "i" = "Signature expects: {.field {required_fields}}",
  "i" = "Example: {.code dsp('q -> a', q = 'your question')}"
))

# Actionable suggestions
cli::cli_abort(c(
  "{.arg signature} must be a Signature object, not {.cls {class(x)[1]}}",
  "i" = "Create one with: {.code signature('question -> answer')}"
))
```

### Tasks

**Audit all
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)
calls** for consistency and helpfulness

**Add “Did you mean?” suggestions** for more scenarios:

- Module method names (`mod$forwrd` -\> `mod$forward`)
- Metric function names (`metric_ecact_match` -\> `metric_exact_match`)
- Configuration option names

**Provider-specific error suggestions**

``` r
# OpenAI
"Rate limit exceeded" -> "Wait 60 seconds or upgrade your API tier"
"Context length exceeded" -> "Reduce input size or use gpt-4-turbo (128k context)"

# Anthropic
"Overloaded" -> "Retry with exponential backoff"

# All providers
"Invalid API key" -> "Check your API key with dsprrr_sitrep()"
```

**Add error codes** for programmatic handling

``` r
# Errors have a class for catching
tryCatch(
  dsp("q -> a", q = "..."),
  dsprrr_rate_limit = function(e) Sys.sleep(60),
  dsprrr_auth_error = function(e) stop("Check API key")
)
```

**Cross-reference troubleshooting vignette** in error messages

``` r
cli::cli_abort(c(
  "LLM call failed: {error_msg}",
  "i" = "See {.url https://jameshwade.github.io/dsprrr/articles/troubleshooting.html}"
))
```

------------------------------------------------------------------------

## 2.6 Missing Functionality (vs DSPy)

### Priority Features

| Feature               | DSPy Equivalent         | Complexity | Value  |
|-----------------------|-------------------------|------------|--------|
| ChainOfThought module | `dspy.ChainOfThought`   | Medium     | High   |
| BootstrapFewShot      | `dspy.BootstrapFewShot` | High       | High   |
| Retry with backoff    | Built into DSPy         | Low        | High   |
| Cost tracking         | `dspy.inspect_history`  | Low        | Medium |
| A/B comparison        | Custom in DSPy          | Medium     | Medium |

### Tasks

**Add retry logic to
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)**

``` r
run(mod, ..., .max_retries = 3, .retry_delay = c(1, 2, 4))
```

**Add cost tracking helpers**

``` r
mod$get_total_cost()
mod$get_cost_summary()  # By call, with timestamps
session_cost()          # Across all modules
```

**Document ChainOfThought module roadmap** (separate issue)

**Document BootstrapFewShot roadmap** (separate issue)

------------------------------------------------------------------------

## 2.7 Documentation Consistency

### Tasks

**Create documentation template** for all exported functions:

``` r
#' @param .llm An ellmer Chat object. If `NULL`, uses `get_default_chat()`.
#' @param .return_format Character: `"simple"` (default) returns just output,
#'   `"structured"` returns list with output, metadata, and chat object.
#' @param .verbose Logical: show progress? Default from `getOption("dsprrr.verbose")`.
```

**Standardize @return documentation** - be specific about types

**Add cross-references** between related functions

``` r
#' @seealso [run()] for single execution, [evaluate()] for metric computation
```

**Verify all examples run** without error

------------------------------------------------------------------------

## Implementation Plan

Since we’re pre-CRAN, we can tackle these in order of impact:

### Phase 2a: API Consistency (High Impact)

- Rename inconsistent functions (`last_trace` → `get_last_trace`, etc.)
- Standardize parameter names (`dataset`, `n`, `k`)
- Standardize return types (always tibble for batch, S3 classes)
- Add `.simplify` to
  [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md),
  `.return_format` to
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)

### Phase 2b: Error & Config Excellence

- Audit and improve all error messages with cli markup
- Provider-specific error suggestions
- Enhanced
  [`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)
  with comprehensive checks
- Add `dsp_params()` wrapper for discoverability

### Phase 2c: New Features

- Add retry logic with exponential backoff
- Add cost tracking helpers
- Create accessor functions for structured results
- Document ChainOfThought/BootstrapFewShot roadmap

### Phase 2d: Documentation Polish

- Standardize all roxygen documentation
- Verify all examples run
- Add cross-references between related functions

------------------------------------------------------------------------

## Acceptance Criteria

All exported functions follow consistent naming patterns (verb_noun or
get_noun)

Return types are documented precisely and consistent within function
families

Parameter names are standardized (`data`, `n`, `k`, `.llm`,
`.return_format`)

[`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)
provides comprehensive configuration diagnosis

Error messages include actionable suggestions with cli markup

All examples in documentation run without error

R CMD check passes with no new warnings

Test coverage maintained or improved

Vignettes and README updated with new API

------------------------------------------------------------------------

## API Inventory Reference

Complete list of 73 exported functions (click to expand)

### Core Functions

- [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md),
  [`input()`](https://jameshwade.github.io/dsprrr/reference/input.md),
  [`input_string()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md),
  [`input_number()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md),
  [`input_boolean()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md),
  [`input_integer()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md),
  [`input_enum()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md),
  [`input_array()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md),
  [`input_object()`](https://jameshwade.github.io/dsprrr/reference/input_helpers.md)
- [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
  [`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md)
- [`dsp()`](https://jameshwade.github.io/dsprrr/reference/dsp.md),
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
  [`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
  [`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)

### Evaluation & Optimization

- [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
  [`evaluate_dsp()`](https://jameshwade.github.io/dsprrr/reference/evaluate_dsp.md)
- [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md),
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
  [`compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.md)
- [`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md),
  [`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md),
  [`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md)

### Metrics

- [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
  [`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
  [`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
  [`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
  [`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
  [`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md)

### Teleprompters

- `Teleprompter`, `LabeledFewShot`, `GridSearchTeleprompter`

### Inspection & Debugging

- [`get_last_trace()`](https://jameshwade.github.io/dsprrr/reference/get_last_trace.md),
  [`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md),
  [`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md),
  [`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md)
- [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
  [`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md),
  [`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md)

### Configuration

- [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
  [`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/set_default_chat.md),
  [`clear_default_chat()`](https://jameshwade.github.io/dsprrr/reference/clear_default_chat.md)
- [`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
  [`dsprrr_sitrep()`](https://jameshwade.github.io/dsprrr/reference/dsprrr_sitrep.md)

### Orchestration

- [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
  [`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)
- [`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
  [`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md)
- [`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
  [`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)

### Vitals Integration

- [`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
  [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)

### Training Data

- [`dsp_trainset()`](https://jameshwade.github.io/dsprrr/reference/dsp_trainset.md)

### Utilities

- [`eval_vignette()`](https://jameshwade.github.io/dsprrr/reference/eval_vignette.md),
  [`has_ellmer_credentials()`](https://jameshwade.github.io/dsprrr/reference/has_ellmer_credentials.md)

------------------------------------------------------------------------

## Related

- Parent issue: \#6
- Phase 1 PR: \#8
- ellmer params:
  <https://github.com/tidyverse/ellmer/blob/main/R/params.R>
- DSPy docs: <https://dspy.ai>
