# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## ⚠️ CRITICAL: Feature Branch Workflow

**NEVER commit directly to main.** The `main` branch is protected—direct
pushes are rejected by GitHub. The ONLY way to get code into main is
through a Pull Request.

Before starting ANY implementation work:

``` bash
# 1. ALWAYS create a feature branch FIRST
git checkout -b feature/<short-description>
# or: git checkout -b fix/<short-description>

# 2. Then claim the issue and start work
kata claim <id> --agent
```

This is mandatory even for small changes. The only commits to main
should be merge commits from PRs.

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

**Before creating a pull request**, run quality gates and automated
review:

``` bash
# 1. Format ALL code with air (entire project)
air format .

# 2. Lint with jarl (auto-fix issues)
jarl check --fix

# 3. Run R CMD check (catches codoc mismatches, missing docs, etc.)
Rscript -e "devtools::check()"

# 4. Build pkgdown site (checks _pkgdown.yml coverage)
Rscript -e "pkgdown::build_site(preview = FALSE)"
```

**Important**: The CI runs `air format --check` on both `R/` and
`tests/` directories. Always format test files too!

#### Automated PR Review (Recommended)

**After passing quality gates**, use the pr-review-toolkit to catch
issues before creating the PR:

``` bash
# Run comprehensive automated review
/pr-review-toolkit:review-pr all

# Or run specific reviews based on changes:
/pr-review-toolkit:review-pr code tests      # Code quality + test coverage
/pr-review-toolkit:review-pr errors comments # Error handling + comment accuracy
/pr-review-toolkit:review-pr simplify        # Simplify and refine code
```

**Available review aspects:** - `code` - General code quality, bugs,
project guidelines (always run) - `tests` - Test coverage quality and
completeness (if tests changed) - `errors` - Silent failures and error
handling (if error handling changed) - `comments` - Comment accuracy and
maintainability (if comments/docs added) - `types` - Type design and
invariants (if new types added) - `simplify` - Code simplification and
clarity (run after passing other reviews) - `all` - Run all applicable
reviews (recommended)

**Review workflow:** 1. Make changes and commit locally 2. Run
`/pr-review-toolkit:review-pr all` 3. Address any **critical** or
**important** issues found 4. Re-run specific reviews to verify fixes 5.
Create PR when all reviews pass

**Automated Workflow (Recommended):**

Use the custom `/create-reviewed-pr` command (in `.claude/commands/`)
that automates the entire workflow:

    /create-reviewed-pr

This command will: 1. Check you’re on a feature branch with all changes
committed 2. Auto-detect R package project and run appropriate quality
gates: - `air format .` (formatting) - `jarl check --fix` (linting) -
`devtools::check()` (R CMD check) -
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
(documentation) 3. Run comprehensive automated PR review via
pr-review-toolkit 4. Stop if critical issues found, warn if important
issues found 5. Create PR only when safe to proceed

This ensures consistent quality and catches issues before they reach
GitHub.

**Sharing with other projects**: Copy
`.claude/commands/create-reviewed-pr.md` to other projects’
`.claude/commands/` directory. See `.claude/README.md` for details.

**Example review output:**

    # PR Review Summary

    ## Critical Issues (0 found)
    (none - ready to proceed)

    ## Important Issues (1 found)
    - [code-reviewer]: Missing error handling in cache_key() [R/cache.R:425]

    ## Suggestions (2 found)
    - [comment-analyzer]: Comment could be clearer [R/module-predict.R:185]
    - [code-simplifier]: Consider extracting helper function [R/run.R:450]

    ## Recommended Action
    1. Fix the important error handling issue
    2. Consider the suggestions
    3. Create PR

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

mod <- module(sig)
result <- run(mod, question = "What is 2+2?", .llm = llm)
```

**PredictModule (R6)** - Subclass for text generation: - `template`:
glue template for prompt construction - `demos`: List of few-shot
examples - `apply_optimization_params()`: Hook for updating state after
optimization

### Core Generics

| Function | Purpose |
|----|----|
| `run(module, ...)` | Execute module with named inputs |
| `run_dataset(module, dataset, ...)` | Batch execute on data frame |
| `evaluate(module, dataset, metric)` | Compute metrics on dataset |
| `optimize_grid(module, devset, metric)` | Grid search optimization |
| `compile(module, teleprompter, trainset)` | Teleprompter-based optimization |

### Teleprompters (S7)

Optimization strategies that compile modules: - `LabeledFewShot`: Add k
examples from training set as demonstrations - `GridSearchTeleprompter`:
Search over instruction/template variants

``` r

tp <- LabeledFewShot(k = 4L, metric = metric_exact_match())
compiled <- compile(mod, tp, trainset)
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
mod <- module(sig)

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

Every model boundary requires a real ellmer `Chat` R6 object. A plain
list with a `chat_structured` element is rejected with
`dsprrr_chat_type_error`. Use `new_test_chat()` from
`tests/testthat/helper-chat.R`, which builds an R6 `Chat` whose methods
you supply:

``` r

# Mock LLM for deterministic testing
mock_llm <- new_test_chat(
  chat_structured = function(...) list(answer = "mocked response")
)

# Test module behavior
test_that("module returns expected output", {
  sig <- signature("q -> a")
  mod <- module(sig)
  result <- mod$forward(list(q = "test"), .llm = mock_llm)
  expect_s3_class(result, "tbl_df")
})
```

`new_test_chat()` also takes `clone`, `get_turns`, `set_turns`,
`last_turn`, and `get_model` overrides for tests that exercise Chat
isolation.

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

### Caching in Tests

**IMPORTANT**: dsprrr automatically caches LLM responses to speed up
development. This can cause test failures when tests expect different
responses across multiple calls with the same prompt.

#### When to Disable Caching in Tests

Disable caching when: 1. **Tests use stateful mock LLMs** - Mock returns
different values based on call count 2. **Testing with epochs \> 1** -
Each epoch should get fresh responses, not cached ones 3. **Testing
cache bypass behavior** - Need to verify `.cache = FALSE` actually
bypasses cache 4. **Tests rely on response variation** - Multiple calls
need different responses

#### How to Disable Caching

**Option 1: Per-test basis** (recommended):

``` r

test_that("epochs get fresh responses", {
  # ... test setup ...

  result <- evaluate(
    module,
    data = dataset,
    metric = metric,
    .llm = mock_llm,
    epochs = 3L,
    .cache = FALSE  # Disable cache for this call
  )
})
```

**Option 2: Use `local_reset_cache()` helper**:

``` r

test_that("stateful mock LLM", {
  local_reset_cache()  # Clear cache at start of test
  configure_cache(enable = FALSE)  # Disable for this test

  # ... test code with stateful mock ...
})
```

**Option 3: Clean disk cache before test runs**:

``` bash
rm -rf tests/testthat/.dsprrr_cache
```

#### Common Pitfall

Tests that work in isolation may fail when run together if persistent
disk cache (`.dsprrr_cache/`) contains entries from previous test runs.
Always use `local_reset_cache()` or `.cache = FALSE` for tests with
stateful mocks.

**Example of problematic test without cache handling**:

``` r

# BAD: Will fail if cache has entries from previous run
test_that("mock returns different values", {
  call_count <- 0
  mock_llm <- new_test_chat(
    chat_structured = function(...) {
      call_count <<- call_count + 1
      paste("response", call_count)
    }
  )

  # First call: "response 1"
  r1 <- run(mod, input = "test", .llm = mock_llm)

  # Second call: expects "response 2" but gets cached "response 1"
  r2 <- run(mod, input = "test", .llm = mock_llm)
  expect_equal(r2, "response 2")  # FAILS with cache enabled
})

# GOOD: Explicitly disable cache
test_that("mock returns different values", {
  local_reset_cache()

  call_count <- 0
  mock_llm <- new_test_chat(
    chat_structured = function(...) {
      call_count <<- call_count + 1
      paste("response", call_count)
    }
  )

  r1 <- run(mod, input = "test", .llm = mock_llm, .cache = FALSE)
  r2 <- run(mod, input = "test", .llm = mock_llm, .cache = FALSE)
  expect_equal(r2, "response 2")  # Works correctly
})
```

## Implementation Status

### Completed

**Module Types (15):** - PredictModule, ReactModule,
ProgramOfThoughtModule, CodeActModule - RAGModule, RLMModule,
MultiChainComparisonModule, EnsembleModule - BestOfNModule,
RefineModule, KNNFewShotModule, FnModule, AssertModule - PipelineModule
(composition via
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md)
and `%>>%`) - FlexModule (bounded declarative Predict/ChainOfThought
graphs via
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)) -
ChainOfThought via signature transforms
([`with_reasoning()`](https://jameshwade.github.io/dsprrr/reference/with_reasoning.md),
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md))

**Teleprompters (10):** - LabeledFewShot, BootstrapFewShot,
BootstrapFewShotWithRandomSearch - MIPROv2, SIMBA, GEPA, COPRO,
KNNFewShot, GridSearch, BetterTogether - Ensembling is a module
(`EnsembleModule`), not a teleprompter - BootstrapFewShot compiles
pipelines **jointly**: per-step demos are harvested from passing
end-to-end traces (DSPy-style whole-program compilation) - GEPA supports
feedback metrics via
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md):
metrics may return `list(score = , feedback = )` and the feedback drives
reflection - GEPA can optimize a Flex program’s complete canonical
`module_src` as a whole-program component; this is intentionally
narrower than DSPy’s per-component frontier and inference-time search -
Fidelity notes: SIMBA and GEPA are intentionally simplified vs. their
papers (documented in roxygen and `vignettes/dspy-comparison.Rmd`)

**Infrastructure:** - R6 Module base class with `forward()`,
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md),
`reset()`, trace methods - S7 Signature with DSPy-style string parsing -
ellmer integration via `chat_structured()` - Two-tier caching (memory +
disk):
[`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md),
[`clear_cache()`](https://jameshwade.github.io/dsprrr/reference/clear_cache.md),
[`cache_stats()`](https://jameshwade.github.io/dsprrr/reference/cache_stats.md) -
LM configuration:
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md),
[`local_lm()`](https://jameshwade.github.io/dsprrr/reference/local_lm.md) -
Async support:
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)
with promises - Streaming listeners:
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md) +
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)
(per-field callbacks, pipeline status events) - vitals bridges
(`as_vitals_solver`, `as_dsprrr_metric`) - Optimizer infrastructure:
`OptimizerControl`, `EvalResult`, `CostSummary`, `Trial`, `TrialLog` -
Module persistence:
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md) -
ragnar integration for RAG; tidymodels integration via parsnip/dials

### Planned

**F - Ecosystem Integration:** - shinychat integration, MLflow
observability

**G - Further DSPy Alignment (dsprrr-9df, dsprrr-7r4, dsprrr-a3z,
dsprrr-deh):** - Native reasoning-trace capture (analogous to
`dspy.Reasoning`) - `tune_bayes()` integration, ParallelModule, Embedder
abstraction - Joint multi-step support for instruction optimizers
(MIPROv2/GEPA per-component selection); demo bootstrapping is already
joint

**H - Production Efficiency (dsprrr-1u0):** - BootstrapFinetune (model
distillation), RL-based optimizers

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

- For internal S7 classes with complex default values, use `@noRd`
  instead of `@keywords internal` to avoid R CMD check codoc mismatch
  warnings
- Instruction-level optimizers (MIPROv2, GEPA, COPRO) operate on single
  modules; only BootstrapFewShot compiles pipelines jointly

## Issue Tracking with Kata

This project uses **Kata** for its persistent backlog. The committed
`.kata.toml` binds every checkout and worktree to the `dsprrr` Kata
project; issue data lives in the Kata daemon rather than in git-tracked
database files.

The former Beads IDs are preserved as `beads-id:dsprrr-...` labels and
in each migrated issue body. To resolve an old reference, run
`kata search "dsprrr-..." --agent`.

### Essential Commands

``` bash
# Finding work
kata ready --agent                    # Open, unblocked work
kata list --status open --agent       # All active issues
kata show <id> --agent                # Details, comments, and relationships
kata search "text" --agent            # Search before creating

# Working on issues
kata claim <id> --agent               # Claim after creating a feature branch
kata comment <id> --body "Progress" --agent
kata unassign <id> --agent            # Release unfinished work

# Creating work safely
kata create "Fix bug" \
  --body "Observed behavior and intended outcome." \
  --priority 1 \
  --label bug \
  --idempotency-key "fix-bug-YYYY-MM-DD" \
  --agent

# Relationships
kata edit <id> --blocked-by <ref> --agent
kata edit <id> --related <ref> --agent

# Close only after verification, with evidence
kata close <id> --done \
  --message "Implemented the fix and verified the package checks." \
  --commit <sha>
```

Use Kata for multi-session work, dependencies, and discovered
follow-ups. A short in-session checklist can still be used for immediate
execution steps, but it does not replace the persistent Kata issue.

## Feature Branch + PR Workflow

### 1. Find Work and Create Feature Branch

**⚠️ IMPORTANT: Create the feature branch BEFORE claiming the issue or
writing any code.**

``` bash
kata ready --agent                    # Find available work
kata show <id> --agent                # Review issue details

# CREATE BRANCH FIRST - before any code changes!
git checkout -b feature/<short-description>
# or: git checkout -b fix/<short-description>

kata claim <id> --agent               # Now claim the work
```

### 2. Work and Record Progress

``` bash
# Make changes...
kata comment <id> --body "Implemented the first verified slice." --agent
```

### 3. Run Quality Gates

``` bash
# Format ALL code with air (R/ and tests/)
air format R/ tests/testthat/

# Lint with jarl
jarl check R/

# Run tests
Rscript -e "devtools::test()"

# Run R CMD check
Rscript -e "devtools::check()"

# Build pkgdown site
Rscript -e "devtools::document(); pkgdown::build_site(preview = FALSE)"
```

### 4. Create PR and Close Issue

When code is complete and ready for review:

``` bash
git add <specific-files>
git commit -m "feat: description (dsprrr#<id>)"
commit_sha="$(git rev-parse HEAD)"
kata close <id> --done \
  --message "Implemented and verified the requested work." \
  --commit "$commit_sha"
git push -u origin HEAD
gh pr create --title "..." --body "Resolves dsprrr#<id>"
```

**Important**: Close the Kata issue when the work is complete and
verified, not merely because a PR exists. The issue tracks
implementation; the PR tracks review and merge.

### 5. Human Reviews and Merges PR

Agents create PRs but **do not merge them**. Humans review and merge PRs
to main.

### 6. After PR Merged (Cleanup)

``` bash
git checkout main
git pull
git branch -d feature/<short-description>
```

Or use:

``` r

usethis::pr_finish()
```

## Session Completion Protocol

**CRITICAL**: Before ending a session, complete ALL steps. Work is NOT
complete until `git push` succeeds.

### Mandatory Checklist (Feature Branch Workflow)

``` bash
# 1. Verify you're on a feature branch (NOT main!)
git branch --show-current  # Should NOT be 'main'

# 2. File issues for remaining work
kata create "Follow-up task" \
  --body "What remains and why it is separate." \
  --priority 2 \
  --idempotency-key "follow-up-YYYY-MM-DD" \
  --agent

# 3. Run quality gates (if code changed)
air format R/ tests/testthat/
jarl check R/
Rscript -e "devtools::check()"

# 4. Release unfinished work with durable context
kata comment <unfinished-id> --body "What remains and the next step." --agent
kata unassign <unfinished-id> --agent

# 5. Commit specific files, then close completed work with that evidence
git add <specific-files>
git commit -m "feat: description (dsprrr#<id>)"
commit_sha="$(git rev-parse HEAD)"
kata close <completed-id> --done \
  --message "Implemented and verified the completed work." \
  --commit "$commit_sha"
git push -u origin HEAD

# 6. Create PR (if not already created)
gh pr create --title "..." --body "Resolves dsprrr#<id>"

# 7. Verify
git status  # Should show "up to date with origin"
```

### Critical Rules

- **NEVER commit directly to main** - always use feature branches
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing—that leaves work stranded locally
- NEVER say “ready to push when you are”—YOU must push
- If push fails, resolve and retry until it succeeds
- Record unfinished follow-up work in Kata before ending the session
- Include the Kata issue ID in commit messages when work is issue-driven

## Parallel Sessions & Worktrees

This project supports parallel work via git worktrees. Every worktree
resolves the same Kata project through `.kata.toml`, so backlog changes
are immediately visible without a tracker-specific git branch or merge
driver.

### Creating Worktrees for Parallel Features

``` bash
# From main repo, create worktree for a feature
git worktree add ../dsprrr-feature-x -b feature/feature-x
cd ../dsprrr-feature-x

# Kata commands resolve the shared dsprrr project
kata ready --agent
kata create "Implement feature" \
  --body "Implementation scope." \
  --priority 2 \
  --idempotency-key "implement-feature-YYYY-MM-DD" \
  --agent
```

Do not create worktree-local tracker databases or reintroduce `.beads/`.

### Cleanup After PR Merged

``` bash
git worktree remove ../dsprrr-feature-x
git worktree prune
```

### Troubleshooting: “Branch already checked out”

If git says a branch is checked out in another worktree:

``` bash
git worktree list
git worktree prune
```

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
