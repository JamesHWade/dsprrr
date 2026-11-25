# Agentic optimization harnesses

[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
and
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
*(both experimental)* put an LLM agent in charge of proposing edits to a
multi-module dsprrr program, while dsprrr keeps strict control of
candidate validation, evaluation, budgets, checkpoints, and final
selection.

When should you reach for a harness over
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md) or
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md)?
When improving a pipeline requires **coordinated edits across multiple
steps** (such as changing both an extractor’s output contract and a
downstream answerer’s template together), or when letting the proposer
run exploratory R analyses on failures in a sandbox helps it form better
hypotheses.

## `AutoResearch()` vs `MetaHarness()`

Both harnesses edit the exact same surface—`instructions` and `template`
fields on editable leaf modules—and never edit R source code or the
evaluator. They differ in how the proposer session is structured:

| Aspect | [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md) | [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) |
|----|----|----|
| Proposer session | **One persistent session** that remembers prior hypotheses and outcomes | **Fresh stateless clone** on every iteration, reading a host-saved frontier |
| Proposals per iteration | One candidate edit at a time | A bounded batch (`max_candidates_per_iteration`) |
| Best suited for | Iterative hypothesis refinement where conversational continuity helps | Broad exploration without context drift or stale reasoning |

``` text
AutoResearch:  proposer (persistent chat) <-> host eval & ledger (keep / branch / stop)

MetaHarness:   saved frontier -> fresh proposer clone -> batch of edits -> host eval & frontier update
```

> **Note on the offline runner:** To execute both journeys end-to-end
> during documentation builds without API keys or external sandbox
> binaries, the walkthrough below uses deterministic local Chat fixtures
> (`sandbox = FALSE`, loaded in a hidden setup chunk). Live sandboxed
> examples for both harnesses appear at the end of the page.

## 1. The two-step QA pipeline

Our starting pipeline answers factual questions in two steps:
`find_evidence` quotes the context, and `answer_from_evidence` returns
the first word of that evidence. Fixing only step 1 improves single-word
answers (`2021`, `18`, `Thursday`) but still truncates multi-word
answers (`"7 business days"` → `"7"`). Solving every row requires a
coordinated edit to both steps.

``` r

library(dsprrr)

trainset <- data.frame(
  context = c(
    "The North Hall opened in 2018.",
    "Refunds are processed in 5 business days.",
    "The Kyoto office employs 12 analysts.",
    "Support is available every Tuesday."
  ),
  question = c(
    "Which year did North Hall open?",
    "How long do refunds take?",
    "How many analysts work in Kyoto?",
    "Which weekday is support available?"
  ),
  answer = c("2018", "5 business days", "12", "Tuesday")
)

valset <- data.frame(
  context = c(
    "The Cedar lab opened in 2021.",
    "Standard returns are completed in 7 business days.",
    "The Porto office employs 18 staff.",
    "Releases ship every Thursday."
  ),
  question = c(
    "Which year did Cedar lab open?",
    "How long do standard returns take?",
    "How many staff work in Porto?",
    "Which weekday do releases ship?"
  ),
  answer = c("2021", "7 business days", "18", "Thursday")
)

holdout <- data.frame(
  context = c(
    "The Field team launched in 2016.",
    "Replacement parts arrive in 3 business days.",
    "The Tallinn group has 9 engineers.",
    "Backups are checked every Friday."
  ),
  question = c(
    "Which year did the Field team start?",
    "How long do replacement parts take?",
    "How many engineers are in Tallinn?",
    "Which weekday are backups checked?"
  ),
  answer = c("2016", "3 business days", "9", "Friday")
)

find_evidence <- module(
  signature(
    "context, question -> evidence: string",
    instructions = "Quote the supplied context as evidence."
  ),
  template = "Question: {question}\nContext: {context}\nEvidence:"
)
answer_from_evidence <- module(
  signature(
    "evidence -> answer: string",
    instructions = "Return the first word of the evidence as the answer."
  ),
  template = "Evidence: {evidence}\nAnswer:"
)
qa_program <- pipeline(find_evidence, answer_from_evidence)

metric <- metric_with_feedback(
  function(prediction, expected) {
    correct <- identical(prediction$answer, expected$answer)
    list(
      score = as.numeric(correct),
      feedback = if (correct) {
        "Correct."
      } else {
        paste0(
          "Expected '", expected$answer, "' but got '",
          prediction$answer, "'. Preserve the full value."
        )
      }
    )
  },
  field = "answer"
)
```

[`named_parameters()`](https://jameshwade.github.io/dsprrr/reference/module-graph.md)
reveals the exact module paths a harness is allowed to edit—here
`$/steps/1` and `$/steps/2`, each exposing `instructions` and
`template`. Any proposal targeting an unlisted path or field is rejected
before evaluation:

``` r

names(named_parameters(qa_program, include_root = TRUE, boundaries = "respect"))
#> [1] "$/steps/1" "$/steps/2"
```

## 2. `AutoResearch()`: one persistent research session

[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
evaluates the baseline, then runs one persistent proposer session. Here,
iteration 1 edits only `$/steps/1` (reaching `0.75` on `valset` because
`"7 business days"` still truncates), and iteration 2 coordinates both
steps to hit `target_score = 1`:

``` r

auto_compiled <- compile(
  qa_program,
  AutoResearch(
    metric = metric,
    max_iterations = 2L,
    patience = 2L,
    target_score = 1,
    seed = 42L,
    sandbox = FALSE,
    verbose = FALSE
  ),
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = auto_agent,
  control = optimizer_control(
    max_trials = 4L,
    max_metric_calls = 20L,
    max_provider_calls = 40L,
    progress = FALSE
  ),
  .cache = FALSE,
  objective = "Answer from the context and preserve the complete value."
)

auto_result <- optimization_result(auto_compiled)
auto_result$trials[, c("id", "parent_id", "name", "iteration", "score", "selected", "status")]
#> # A tibble: 3 × 7
#>   id                       parent_id       name  iteration score selected status
#>   <chr>                    <chr>           <chr>     <int> <dbl> <lgl>    <chr> 
#> 1 baseline-594b4430aa07    NA              base…         0  0    FALSE    evalu…
#> 2 candidate-1-3d25bbaa446c baseline-594b4… evid…         1  0.75 FALSE    evalu…
#> 3 candidate-2-7bae3460a9c0 candidate-1-3d… coor…         2  1    TRUE     evalu…
auto_result$stop_reason
#> [1] "target_score"
```

Checking the compiled pipeline on the untouched `holdout` rows confirms
the coordinated edit generalizes beyond `valset`:

``` r

auto_holdout <- compare_on_split(qa_program, auto_compiled, holdout)
auto_holdout$rows
#>                               question        expected    baseline
#> 1 Which year did the Field team start?            2016         The
#> 2  How long do replacement parts take? 3 business days Replacement
#> 3   How many engineers are in Tallinn?               9         The
#> 4   Which weekday are backups checked?          Friday     Backups
#>         optimized score
#> 1            2016     1
#> 2 3 business days     1
#> 3               9     1
#> 4          Friday     1
c(baseline = auto_holdout$before, optimized = auto_holdout$after)
#>  baseline optimized 
#>         0         1
```

## 3. `MetaHarness()`: fresh proposers over a scored frontier

[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
clones and resets `.agent_llm` before every outer iteration, so each
proposer reasons cleanly from the persisted frontier rather than from
long chat history, and submits a batch of candidates
(`max_candidates_per_iteration`):

``` r

meta_compiled <- compile(
  qa_program,
  MetaHarness(
    metric = metric,
    max_iterations = 2L,
    max_candidates_per_iteration = 2L,
    frontier_size = 2L,
    patience = 3L,
    seed = 42L,
    sandbox = FALSE,
    verbose = FALSE
  ),
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = meta_agent,
  control = optimizer_control(
    max_trials = 5L,
    max_metric_calls = 24L,
    max_provider_calls = 50L,
    progress = FALSE
  ),
  .cache = FALSE,
  objective = "Answer from the context and preserve the complete value."
)

meta_result <- optimization_result(meta_compiled)
meta_result$trials[, c("id", "parent_id", "name", "iteration", "score", "selected", "status")]
#> # A tibble: 4 × 7
#>   id                       parent_id       name  iteration score selected status
#>   <chr>                    <chr>           <chr>     <int> <dbl> <lgl>    <chr> 
#> 1 baseline-594b4430aa07    NA              base…         0  0    FALSE    evalu…
#> 2 candidate-1-3d25bbaa446c baseline-594b4… evid…         1  0.75 FALSE    evalu…
#> 3 candidate-1-7bae3460a9c0 candidate-1-3d… coor…         1  1    TRUE     evalu…
#> 4 candidate-2-ae5902503f42 candidate-1-7b… coor…         2  1    FALSE    evalu…
meta_result$extensions$meta_harness$frontier_ids
#> [1] "candidate-1-7bae3460a9c0" "candidate-2-ae5902503f42"
```

In iteration 1, the proposer tests both `evidence-only` (`0.75`) and
`coordinated-pipeline` (`1.00`) in one batch; in iteration 2, a fresh
proposer sees the frontier and explores a formatting variant branching
from the winner. On `holdout`, the selected program again rises from `0`
to `1`:

``` r

meta_holdout <- compare_on_split(qa_program, meta_compiled, holdout)
c(baseline = meta_holdout$before, optimized = meta_holdout$after)
#>  baseline optimized 
#>         0         1
```

## 4. Sandbox and trust boundaries

With the default `sandbox = TRUE`, exploratory R analyses requested by
the proposer must run in a runner that advertises a verified OS
sandbox—specifically
[`mcp_repl_runner()`](https://jameshwade.github.io/dsprrr/reference/mcp_repl_runner.md),
which launches Posit’s
[mcp-console](https://github.com/posit-dev/mcp-console) with network
access disabled and writes restricted to the workspace
(`workspace-write`). User-managed `repl =` connections and
[`r_code_runner()`](https://jameshwade.github.io/dsprrr/reference/r_code_runner.md)
are rejected when `sandbox = TRUE`.

| Limit | Bound | Behavior when exceeded |
|----|----|----|
| Exploratory R code per action | 12,000 characters | Action rejected without running |
| Sandbox output shown to the agent | 4,000 characters | Truncated and marked `[truncated]` |
| Editable fields | Leaf `instructions` & `template` only | Unknown paths/fields rejected before evaluation |
| Evaluation & selection | Host R process only | Agent never modifies R source or the metric |

## 5. Run live with a sandboxed runner

Install `mcptools` and `posit-mcp-repl`, pair a fast task Chat (`.llm`)
with a strong proposer Chat (`.agent_llm`), and set explicit
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
caps:

``` r

task_chat <- ellmer::chat_openai(model = "gpt-6-luna")
proposer_chat <- ellmer::chat_anthropic(model = "claude-sonnet-4-5")
runner <- mcp_repl_runner(sandbox = "workspace-write", timeout = 30)

# Option A: persistent session with AutoResearch()
auto_live <- compile(
  qa_program,
  AutoResearch(metric = metric, max_iterations = 4L, target_score = 1),
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = proposer_chat,
  runner = runner,
  control = optimizer_control(
    max_trials = 5L,
    max_metric_calls = 20L,
    max_provider_calls = 50L,
    max_cost = 5,
    checkpoint_path = file.path(tempdir(), "autoresearch-checkpoint.rds")
  ),
  objective = "Answer from supplied evidence and preserve complete values."
)

# Option B: fresh proposers per iteration with MetaHarness()
meta_live <- compile(
  qa_program,
  MetaHarness(
    metric = metric,
    max_iterations = 4L,
    max_candidates_per_iteration = 3L,
    frontier_size = 4L
  ),
  trainset,
  valset = valset,
  .llm = task_chat,
  .agent_llm = proposer_chat,
  runner = runner,
  control = optimizer_control(
    max_trials = 10L,
    max_metric_calls = 40L,
    max_provider_calls = 80L,
    max_cost = 5,
    checkpoint_path = file.path(tempdir(), "metaharness-checkpoint.rds")
  ),
  objective = "Answer from supplied evidence and preserve complete values."
)

optimization_result(auto_live)
```

Checkpoints restore the optimizer ledger, frontier, and RNG state (not
live Chats or runner processes). To combine either harness with other
teleprompters in one search, see [Compose optimizers with
Omni](https://jameshwade.github.io/dsprrr/articles/omni-meta-optimization.md).
