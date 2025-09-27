# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`dsprrr` is an R package for building principled, test-driven, and optimizable applications using Large Language Models. It implements the DSP (Declarative Self-improving Language Programs) framework in R, providing a structured programming model where LLM workflows are treated as programs that can be systematically improved.

The package is built on S7 (R's modern object system) and follows tidyverse design principles. It represents prompts, examples, and evaluation results as data frames with composable, pipe-friendly APIs.

## Development Commands

### Package Development
- Build and check: `devtools::check()`
- Run tests: `devtools::test()`
- Run specific test file: `testthat::test_file("tests/testthat/test-<name>.R")`
- Build documentation: `devtools::document()`
- Build README: `devtools::build_readme()` (edits should be made to README.Rmd)
- Install package locally: `devtools::install()`
- Load for development: `devtools::load_all()`

### Continuous Integration
The package uses GitHub Actions for CI/CD with:
- R CMD check across multiple R versions and OS platforms
- Test coverage reporting to Codecov
- Automated checks trigger on push to main/master and pull requests

## Core Architecture

### S7 Classes (planned implementation)
- **`Signature`**: Declarative schema for LLM operations with inputs, output_type, and instructions
- **`Predict`**: Stateless execution module pairing a Signature with a glue template
- **`Teleprompter`**: Base class for optimization strategies (e.g., GridSearchTeleprompter)

### Core Generics (planned)
- `forward()`: Executes a module with named arguments
- `compile()`: Optimizes a program using a Teleprompter
- `evaluate()`: Evaluates program performance on test sets

### Dependencies
Core packages imported:
- `cli`: Console output and messaging
- `glue`: String interpolation for templates
- `lifecycle`: Manage function lifecycle
- `rlang`: Advanced R programming and error handling
- `withr`: Temporary state changes

The package will integrate with `ellmer` for LLM API calls and follow tidyverse design patterns for data manipulation.

## Implementation Status

The package is in early development. The PLAN.md file contains the detailed roadmap with 4 milestones:
1. ✅ S7 Foundation & Core Execution (COMPLETED)
   - Implemented `Signature` and `Predict` S7 classes with validators
   - Implemented `forward()` S7 generic and method for Predict class
   - Integrated with ellmer for LLM API calls
   - Created comprehensive test suite
2. The Compilation Engine (5 weeks) - NOT STARTED
3. Ecosystem & Ergonomics (7 weeks) - NOT STARTED
4. Future Vision & Extensibility - NOT STARTED

### Completed Components
- `input()`: Helper function to create input specifications
- `Signature`: S7 class for declarative LLM operation schemas
- `Predict`: S7 class for stateless execution modules
- `forward()`: Generic for executing modules with structured output
- Integration with ellmer's `chat_structured()` for type-safe LLM calls
- Test suite with 54 passing tests

### Known Issues
- Documentation warnings need to be addressed for CRAN submission
- Some imported packages (tibble, withr) are not yet used
- Non-standard top-level files (CLAUDE.md, PLAN.md) should be added to .Rbuildignore for CRAN
- Do not use `-old` or `-new` or `-improved` files names. You should replace outdated code instead of preserving the old stuff.
- Do not use `devtools::build_vignettes()`. You should use build_rmd() instead.