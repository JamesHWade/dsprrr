# Compose optimizers with Omni

No optimizer wins on every task. Demo selection may be the decisive
lever for one module, while instruction search or reflective mutation
wins for another.
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md) runs
several teleprompters from the same seed program, scores every result
with one validation metric, and hands the strongest candidate to a fresh
continuation optimizer. This page shows how to configure it, budget it
fairly, and read what it did.

The design follows the [`omni`
meta-optimizer](https://gepa-ai.github.io/gepa/blog/2026/07/22/optimize-anything-omni/)
from the open-source [GEPA project](https://github.com/gepa-ai/gepa). On
Frontier-CS tasks, the GEPA team found that no single engine won every
task, that each engine plateaus after its early gains, and that
exploring with several engines before continuing from the best result
beat every single engine under a matched budget.

dsprrr adapts that composition pattern to modules and teleprompters. It
is not a port of GEPA’s engine API, and the Frontier-CS result should
not be assumed to transfer to an R module or a different budget.
Benchmark the composition on your own task.

## How Omni runs

For explorers `A`, `B`, and `C`, Omni performs this sequence:

``` text
                         +-> explorer A --+
                         |                |
seed program + baseline -+-> explorer B --+-> shared validation -> winner
                         |                |                         |
                         +-> explorer C --+                         v
                                                        fresh continuation
                                                                  |
                                                                  v
                                                 shared validation + best
```

Three rules make the comparison fair:

1.  Every explorer receives an independent copy of the same seed
    program.
2.  The seed and every successful optimizer output are re-scored with
    the same metric on the same validation rows.
3.  The seed stays eligible through final selection, so a regressing
    explorer or continuation step cannot replace a better candidate.

The continuation runs its teleprompter from the exploration winner. It
does not resume an explorer’s internal search state.

## Configure Omni

The examples assume a module, a chat, and your labeled data in
`examples`, a data frame with `question` and `answer` columns:

``` r

library(dsprrr)

qa_module <- module(signature("question -> answer"))
llm <- ellmer::chat_openai(model = "gpt-6-luna")

split <- split_dataset(examples, prop = 0.8, seed = 42L)
trainset <- split$train
valset <- split$val
```

Start with optimizers that search in different ways. This configuration
combines demo search, instruction search, and reflective evolution, then
continues with SIMBA:

``` r

metric <- metric_exact_match(field = "answer")

omni <- Omni(
  metric = metric,
  explorers = list(
    demos = BootstrapFewShotWithRandomSearch(
      metric = metric,
      num_candidate_programs = 8L
    ),
    instructions = COPRO(
      metric = metric,
      breadth = 8L,
      depth = 2L
    ),
    reflection = GEPA(
      metric = metric,
      population_size = 8L,
      generations = 3L
    )
  ),
  continuation = SIMBA(metric = metric),
  seed = 42L
)

compiled <- compile(
  qa_module,
  omni,
  trainset,
  valset = valset,
  .llm = llm
)
```

Explorer names are part of the result, so name each one after its
strategy rather than its position in the list.

Choose a continuation that can edit an already compiled program, because
unless the seed wins, the continuation starts from an explorer’s output.
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md) and
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md)
can. [`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
and
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
leave compiled modules alone: as a continuation, GEPA returns the winner
unchanged and the two harnesses fail, in which case Omni keeps the
exploration winner. Use those three as explorers.

To pass compile-time arguments to one optimizer, name it. These
arguments cannot replace Omni’s own `program`, `trainset`, `valset` or
`.llm`:

``` r

compiled <- compile(
  qa_module,
  omni,
  trainset,
  valset = valset,
  .llm = llm,
  explorer_compile_args = list(
    demos = list(control = optimizer_control(max_metric_calls = 40L)),
    reflection = list(control = optimizer_control(max_metric_calls = 40L))
  ),
  continuation_compile_args = list(
    control = optimizer_control(max_metric_calls = 40L)
  )
)
```

## Use an explicit validation set

Without `valset`, Omni holds out a fraction of `trainset`
(`valset_ratio`, default 0.1). An explicit validation set, as above,
makes comparisons easier to reproduce and audit. All explorers train on
`trainset`; Omni uses `valset` to choose the exploration winner and the
final program. If you will try several Omni configurations, keep a
third, untouched test set for the final estimate: choosing explorers
repeatedly against the same validation rows can overfit the choice
itself.

## Budget the comparison

[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md) does
not impose one budget, because dsprrr teleprompters expose different
native controls. Give explorers comparable budgets when you construct
them, then count Omni’s own scoring separately. With `k` successful
explorers, Omni runs up to `k + 2` full validation evaluations: one for
the seed, one per explorer, and one for the continuation result. They
come on top of the work inside each optimizer.

For a fair benchmark, record at least:

| Quantity | Why it matters |
|----|----|
| Metric calls | Makes search effort comparable |
| Provider calls and tokens | Captures optimizer-side reflection and generation |
| Cost | Allows matched-budget comparisons across models |
| Elapsed time | Shows the value of parallel exploration |
| Seed, standalone, and Omni scores | Separates composition gains from optimizer strength |

Compare each explorer run on its own against the same explorer inside
Omni. A convincing result shows that diversity plus continuation beat
the best standalone candidate at a similar total budget, not merely that
Omni won.

## Inspect the candidates

The result stores one row for the baseline, each explorer, and the
continuation:

``` r

omni_result <- optimization_result(compiled)$extensions$omni

candidates <- omni_result$candidate_programs
candidates[, c("phase", "optimizer", "score", "selected", "error")]
```

The `program_config` list-column holds each candidate module’s `config`
list, or `NULL` for a branch that failed. The selected program is
`compiled` itself.

``` r

candidates$program_config[[which(candidates$selected)]]

omni_result$exploration_winner
omni_result$best_phase
omni_result$best_optimizer
omni_result$flag_compilation_error_occurred
```

A failed explorer is recorded in `error` and does not stop the other
branches. If the continuation fails, Omni returns the best exploration
candidate. `flag_compilation_error_occurred` is `TRUE` when any branch
failed, so a usable result never hides a failure.

## Parallel exploration

Set `parallel = TRUE` when branch runtime dominates and the provider can
take the added concurrency:

``` r

omni_parallel <- Omni(
  metric = metric,
  explorers = omni@explorers,
  continuation = omni@continuation,
  parallel = TRUE,
  num_workers = 3L,
  seed = 42L
)

compiled <- compile(
  qa_module,
  omni_parallel,
  trainset,
  valset = valset,
  .llm = NULL
)
```

Chat objects are not sent to mirai workers, so parallel exploration
requires `.llm = NULL` and an `OPENAI_API_KEY`, `ANTHROPIC_API_KEY` or
`GOOGLE_API_KEY` that the workers can see. Each worker builds its own
chat: the provider’s default model for the first key it finds, in that
order. Workers ignore
[`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md),
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md),
[`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)
and the program’s own chat. The baseline, the scoring of every
candidate, and the continuation run in your session and do use them.
Unless both resolve to the same provider and model, explorers are
optimized for one model and judged on another; use `parallel = FALSE`
unless you know they match.

Parallel execution reduces wall-clock time, not evaluation cost.
Provider rate limits may make fewer workers faster in practice.

## Choose explorers

Favor search behaviors that complement each other over several
variations of one teleprompter:

| Search need | Candidate explorer |
|----|----|
| Select or bootstrap demonstrations | `BootstrapFewShotWithRandomSearch` |
| Rewrite instructions directly | `COPRO` |
| Focus on hard examples | `SIMBA` |
| Reflect on failures and evolve candidates | `GEPA` |
| Try a fixed set of instruction or template variants | `GridSearchTeleprompter` |

Two or three different explorers are a better first experiment than a
large portfolio. Add a branch only when it represents a plausible search
behavior the current set does not cover.

## How dsprrr’s Omni differs from GEPA’s

| GEPA `optimize_anything` | dsprrr [`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md) |
|----|----|
| Composes engines over arbitrary text artifacts | Composes teleprompters over dsprrr modules |
| Ships GEPA, AutoResearch, and Meta-Harness engines | Provides [`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md), [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md), and [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) as teleprompters, best used as explorers |
| Terrarium enforces a shared task, server, and budget contract | You align the budgets of different teleprompters |
| Published Frontier-CS results use a matched \$20 budget | No cross-task performance claim; benchmark locally |
| Continues the best artifact with a selected engine | Compiles a continuation teleprompter from the winning module |

The shared idea is optimizer diversity: when search strategies fail in
different ways, the strongest partial result can give a fresh optimizer
a better starting point than the original seed. To run the agentic
teleprompters as explorers, see [Agentic optimization
harnesses](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md).
