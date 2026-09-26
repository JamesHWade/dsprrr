# Choose an optimizer

dsprrr has eleven optimizers, called teleprompters. They differ in what
they change in a module, how much labeled data they need, and how many
model calls they spend. This page compares them and shows a short call
for each. The workflow around them (baseline, held-out scores, saving)
is covered in [Compile and
optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md),
and the ideas behind them in [How optimization
works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md).

## Comparison

In the cost column, T is the number of training rows and V the number of
validation rows.

| Optimizer | What it changes | Data it needs | Model calls to compile | Use it when |
|----|----|----|----|----|
| `LabeledFewShot` | Demos, copied from training rows | `k` labeled rows | None | You want a quick first improvement |
| `BootstrapFewShot` | Demos, from the module’s own passing outputs | More rows than `max_labeled_demos` | Up to T minus `max_labeled_demos` per round | Demos should show complete outputs, or the program is a pipeline |
| `BootstrapFewShotWithRandomSearch` | Demos, the best of several sets | `trainset` and `valset` | Several bootstrap runs, plus `num_candidate_programs` × V | You can pay to compare demo sets |
| `KNNFewShot` | Demos, picked for each input by similarity | A varied labeled pool | None; one embedding call per run | Inputs vary widely and similar examples help most |
| `GridSearchTeleprompter` | Instructions or templates that you write, plus demos | `valset`, or a fifth of `trainset` | Variants × V | You have a few specific wordings to compare |
| `COPRO` | Instructions, rewritten by a model | `trainset` and `valset` | About `breadth` × `depth` × V, plus passes over T | The instructions are the weak point |
| `MIPROv2` | Demos and instructions together | `trainset` and `valset` | Bootstrap runs, then 20, 50 or 100 trials (see below) | You want both searched on a fixed trial budget |
| `SIMBA` | Demos and rules from hard examples | `trainset` and `valset` | Per step, `num_candidates` × `bsize` + V | A few kinds of input keep failing |
| `GEPA` | Instructions, or a whole [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) program | `trainset` and `valset` | `generations` × `population_size` × (V + T), plus rewrites | Your metric can say why an output is wrong |
| `BetterTogether` | Whatever its steps change | `valset`, or 10% of `trainset` | Its steps, plus V per step | You want to chain optimizers and keep the best stage |
| `ReAnchor` | Decision thresholds, cuts and weights; never the prompt | Two or more rows | 2 × T, plus 2 × V with a `valset` | Yes/no or label decisions are miscalibrated |

Every optimizer except `LabeledFewShot` and `KNNFewShot` requires
`metric`. `COPRO`, `MIPROv2`, `SIMBA` and `GEPA` score candidates on
`trainset` when you leave out `valset`, which favors candidates that fit
the training rows rather than new ones.

A reasonable order: start with `LabeledFewShot`. If demos help but not
enough, try `BootstrapFewShot` or `BootstrapFewShotWithRandomSearch`. If
the wording is the problem, try `COPRO` or `GEPA`, and `MIPROv2`
searches both. Whatever you try, compare it with the uncompiled module
on held-out rows.

## Setup for the examples

The examples below route support tickets to a team:

``` r

library(dsprrr)
```

``` r

llm <- ellmer::chat_openai(model = "gpt-6-luna")

triage <- module(signature(
  inputs = list(input("ticket", description = "Customer support ticket")),
  output_type = ellmer::type_object(
    team = ellmer::type_enum(
      c("billing", "shipping", "technical"),
      "Which team should handle the ticket?"
    )
  ),
  instructions = "Route each support ticket to one team."
))

metric <- metric_exact_match(field = "team")

# Three rows show the shape; a real run needs dozens or more
tickets <- tibble::tibble(
  ticket = c(
    "I was charged twice for my subscription this month.",
    "The parcel says delivered, but nothing arrived.",
    "The app crashes when I open the settings page."
  ),
  team = c("billing", "shipping", "technical")
)

splits <- split_dataset(tickets, prop = 0.7, seed = 1)
trainset <- splits$train
valset <- splits$val
```

Counts and sizes are integer properties, so write them as integer
literals:

``` r

try(LabeledFewShot(k = 3))
#> Error : <dsprrr::LabeledFewShot> object properties are invalid:
#> - @k must be <integer>, not <double>
```

## LabeledFewShot

`LabeledFewShot` samples `k` training rows (using `seed`, 123 by
default) and attaches them as demos. It makes no model calls and does
not score anything, so the demos are only as good as the rows you give
it. `sample = FALSE` takes the first `k` rows instead.

``` r

labeled <- triage |> compile(LabeledFewShot(k = 4L), trainset)
```

## BootstrapFewShot

`BootstrapFewShot` runs the module on training rows and keeps the
outputs that pass the metric as demos. Those demos are the model’s own
complete outputs, including fields your data does not label, such as the
reasoning of a
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
module.

The first `max_labeled_demos` rows (16 by default) become labeled demos
as they are, and only the rows after them go to the model. A training
set of 16 rows or fewer therefore makes no model calls with the default
settings: every row becomes a labeled demo. Lower `max_labeled_demos` to
bootstrap on a small set:

``` r

bootstrapped <- triage |>
  compile(
    BootstrapFewShot(
      metric = metric,
      max_bootstrapped_demos = 4L,
      max_labeled_demos = 2L
    ),
    trainset,
    .llm = llm
  )
```

Rows are tried in order until `max_bootstrapped_demos` outputs have
passed. An output passes when its score reaches `metric_threshold`, or,
with no threshold, when it is above zero. `max_rounds` repeats the pass
with the demos found so far. For a pipeline, `BootstrapFewShot` runs the
whole program on each row, scores the final output, and takes demos for
every step from the runs that pass (see [Chain modules into
pipelines](https://jameshwade.github.io/dsprrr/articles/chaining-modules.md)).

## BootstrapFewShotWithRandomSearch

This optimizer builds `num_candidate_programs` candidates (16 by
default): the uncompiled program, a program with labeled demos only, and
several `BootstrapFewShot` runs. It scores each on `valset`, which is
required, and returns the best.

``` r

searched <- triage |>
  compile(
    BootstrapFewShotWithRandomSearch(
      metric = metric,
      num_candidate_programs = 8L,
      max_labeled_demos = 4L,
      stop_at_score = 0.95
    ),
    trainset,
    valset = valset,
    .llm = llm
  )

optimization_result(searched)$trials[c("name", "score")]
```

`stop_at_score` ends the search once a candidate reaches that score.
`num_threads` sets how many validation rows are scored at the same time
for each candidate; the candidates are still scored one after another.

## KNNFewShot

`KNNFewShot` does not fix the demos at compile time. It embeds every
training row once, then at run time embeds each input and uses the `k`
most similar training rows (by cosine similarity) as that call’s demos.

``` r

knn <- triage |>
  compile(
    KNNFewShot(
      k = 3L,
      vectorizer = ragnar::embed_openai(model = "text-embedding-3-small")
    ),
    trainset
  )

run(knn, ticket = "My card was charged twice this month.", .llm = llm)
```

`vectorizer` is any function that turns a character vector into a
numeric matrix with one row per string. `input_text` is an optional
function that turns a row into the text to embed; by default it pastes
the signature’s input columns together. Compiling makes no model calls,
but every
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) makes
one embedding call before the model call.

## GridSearchTeleprompter

`GridSearchTeleprompter` scores each row of `variants` and keeps the
best. `id` is required. An `instructions` column replaces the module’s
instructions, `instructions_suffix` is appended to them, and `template`
replaces the prompt template. It also attaches `k` demos (2 by default)
sampled from `trainset`.

``` r

variants <- data.frame(
  id = c("terse", "rules"),
  instructions = c(
    "Route each support ticket to one team. Answer with the team only.",
    paste(
      "Route each support ticket to one team.",
      "Charges and refunds go to billing, lost or damaged parcels to",
      "shipping, and errors or crashes to technical."
    )
  )
)

set.seed(1)
gridded <- triage |>
  compile(
    GridSearchTeleprompter(variants = variants, metric = metric, k = 2L),
    trainset,
    valset = valset,
    .llm = llm
  )
```

Demos and, without a `valset`, the scoring rows are drawn with R’s
global random number generator, hence the
[`set.seed()`](https://rdrr.io/r/base/Random.html). Without a `valset`,
the variants are scored on a random fifth of `trainset` (rounded up, at
most `eval_sample_size` rows) and the demos come from the rest.

## COPRO

`COPRO` rewrites the instructions. It first runs the module on every
training row to find failures. Each of `depth` rounds then asks
`prompt_model` (by default the task chat) for `breadth` new versions of
the current best instructions, showing it up to three failed rows. Each
version is scored on `valset`, and the best one replaces the current
instructions if it scores higher. After each round that improves the
score, the training rows are checked again for failures.

``` r

rewritten <- triage |>
  compile(
    COPRO(
      metric = metric,
      prompt_model = ellmer::chat_openai(model = "gpt-6-luna"),
      breadth = 5L,
      depth = 2L
    ),
    trainset,
    valset = valset,
    .llm = llm
  )

rewritten$signature@instructions
```

## MIPROv2

`MIPROv2` searches over combinations of a demo set and an instruction
variant. The demo sets are one labeled set plus several bootstrapped
sets. The instruction variants are the original instructions plus
versions that append a short summary of the input and output fields and
one of five fixed tips, such as “Be concise and accurate.” No model
writes new instructions.

Each trial scores one combination. A UCB1 bandit picks it: untried
combinations first, then those with a high mean score or few trials.
Most trials score a small random batch of training rows; every few
trials, a full pass over `valset` checks the current pick, and the best
fully scored combination wins.

``` r

mipro <- triage |>
  compile(
    MIPROv2(metric = metric, auto = "light"),
    trainset,
    valset = valset,
    .llm = llm
  )
```

`auto` sets the size of the search:

| `auto` | Trials | Rows per batch | Full pass every | Demo sets, up to | Instruction variants |
|----|----|----|----|----|----|
| `"light"` (default) | 20 | 5 | 5 trials | 3 | 5 |
| `"medium"` | 50 | 10 | 10 trials | 5 | 6 |
| `"heavy"` | 100 | 20 | 20 trials | 7 | 6 |

`MIPROv2` needs more training rows than `max_labeled_demos` (4 by
default); with fewer, it returns the module unchanged, with status
`"partial"`. `num_candidates` applies only with `auto = NULL`, where it
sets both the number of trials and the number of instruction variants
(at most six). `task_model` runs the module during the search in place
of `.llm`. For a program with nested predictors, such as an RLM,
`MIPROv2` tunes each predictor’s instructions and requires
`max_bootstrapped_demos = 0L`.

## SIMBA

`SIMBA` works from hard examples. Each step samples `bsize` training
rows, runs the current module `num_candidates` times on them, and ranks
the rows by difficulty: a low mean score, or answers that differ between
runs. The hardest rows, up to `max_demos`, become labeled demos, and
`prompt_model` writes a short rule from them that is appended to the
instructions. Without `prompt_model`, the appended rule just quotes the
hardest row. A step is kept only if it raises the score on `valset`, and
the first step that does not ends the search.

``` r

hardened <- triage |>
  compile(
    SIMBA(
      metric = metric,
      bsize = 16L,
      num_candidates = 4L,
      max_steps = 4L,
      max_demos = 4L,
      prompt_model = ellmer::chat_openai(model = "gpt-6-luna")
    ),
    trainset,
    valset = valset,
    .llm = llm
  )
```

dsprrr’s `SIMBA` is a simplified adaptation of the DSPy optimizer of the
same name.

## GEPA

`GEPA` evolves instructions. It starts from the original instructions
and `population_size - 1` rewrites of them. In each of `generations`
rounds it scores every candidate on `valset` and runs it on `trainset`
to collect the rows it gets wrong. Candidates that score best on at
least one validation row become parents. A child takes each part of the
program whole from one parent or the other, so crossover never splices
text, and with probability `mutation_rate` the task chat (`.llm`)
rewrites the child’s instructions after reading its parent’s failed
rows.

GEPA reads textual feedback when the metric returns it, which makes the
rewrites more specific.
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md)
marks such a metric:

``` r

team_feedback <- metric_with_feedback(
  function(prediction, expected) {
    if (identical(prediction$team, expected$team)) {
      list(score = 1, feedback = "Correct team.")
    } else {
      list(
        score = 0,
        feedback = paste0(
          "Sent to ", prediction$team, ", but ", expected$team,
          " handles tickets like this."
        )
      )
    }
  },
  field = "team"
)

evolved <- triage |>
  compile(
    GEPA(
      metric = team_feedback,
      population_size = 6L,
      generations = 4L,
      mutation_rate = 0.5,
      seed = 42L
    ),
    trainset,
    valset = valset,
    .llm = llm
  )
```

In a single-predictor module the instructions are the only part, so
crossover can only copy a parent’s wording, and new wording comes from
rewrites alone. That is why this example raises `mutation_rate` from its
default of 0.1. A named list in `metrics` optimizes several objectives
at once, and parents then also come from the Pareto front. For
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
programs, GEPA can rewrite the program’s source itself; see [Flex:
optimize a whole
program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md).

## BetterTogether

`BetterTogether` runs other teleprompters in sequence, each starting
from the previous result, and returns the stage that scores best on
`valset`, the uncompiled module included. Name the optimizers and give
their order as a strategy:

``` r

combined <- triage |>
  compile(
    BetterTogether(
      metric = metric,
      optimizers = list(
        demos = BootstrapFewShot(metric = metric, max_labeled_demos = 2L),
        wording = COPRO(metric = metric, breadth = 4L, depth = 1L)
      ),
      default_strategy = "demos -> wording"
    ),
    trainset,
    valset = valset,
    .llm = llm
  )

optimization_result(combined)$extensions$better_together$candidate_programs
```

Without a `valset`, it holds out `valset_ratio` (10%) of `trainset`.
`compile(..., strategy = "wording -> demos")` changes the order for one
run. Later steps start from a compiled module, so
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
warns that the program is already compiled; here that is expected.

## ReAnchor

`ReAnchor` is experimental. It calibrates boolean and enum outputs and
leaves the prompt alone. It runs the module on `trainset`, then runs it
again asking for the probability behind each decision, and fits each
decision’s threshold, cut points or option weights to those recorded
probabilities without further model calls. A fitted setting is kept only
if it also wins a held-out check across folds of `trainset`.

``` r

calibrated <- triage |>
  compile(ReAnchor(metric = metric), trainset, valset = valset, .llm = llm)

decision_settings(calibrated)
```

The outputs to calibrate need a description, like `team` above, or a
setting from
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md).
`valset` is scored before and after calibration for the report and never
used for fitting. `ReAnchor` calibrates a single
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
only. [Calibrated
decisions](https://jameshwade.github.io/dsprrr/articles/calibrated-decisions.md)
explains decision outputs.

## Omni and the agentic harnesses

Three experimental teleprompters build on the ones above.
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md) runs
several teleprompters from the same start, compares them on one
validation set and continues from the winner; see [Compose optimizers
with
Omni](https://jameshwade.github.io/dsprrr/articles/omni-meta-optimization.md).
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md)
and
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md)
let a model propose program edits in a sandbox while dsprrr scores them;
see [Agentic optimization
harnesses](https://jameshwade.github.io/dsprrr/articles/agentic-optimization-harnesses.md).

## Combining compiled modules

To combine several compiled modules at run time, build an ensemble
module with
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md).
It is a module, not an optimizer, so it needs no training data:

``` r

voted <- ensemble(
  list(labeled, bootstrapped, rewritten),
  reduce_fn = reduce_majority(field = "team")
)

run(voted, ticket = "I was billed after cancelling.", .llm = llm)
```

[Choose a module
type](https://jameshwade.github.io/dsprrr/articles/advanced-modules.md)
covers ensembles and the other reducers.

## Budgets, logs and seeds

`BootstrapFewShot`, `BootstrapFewShotWithRandomSearch`, `COPRO`,
`MIPROv2`, `SIMBA` and `GEPA` accept `control = optimizer_control(...)`
in
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).
It caps calls, tokens, cost or time. When a cap is reached, the
optimizer stops and returns the best module found so far, with status
`"partial"`:

``` r

capped <- triage |>
  compile(
    COPRO(metric = metric, breadth = 5L, depth = 3L),
    trainset,
    valset = valset,
    .llm = llm,
    control = optimizer_control(max_provider_calls = 300L, max_cost = 2)
  )

optimization_result(capped)$status
optimization_result(capped)$stop_reason
```

A cost or token cap also stops the run when usage cannot be measured.
`BootstrapFewShot` and `MIPROv2` can resume an interrupted run: give
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
a `checkpoint_path`, then compile again with `resume = TRUE` and the
same module, data, metric and settings.
`BootstrapFewShotWithRandomSearch`, `COPRO`, `SIMBA` and `GEPA` refuse
`resume = TRUE`.

The same six teleprompters take `log_dir` and append one record per
trial to `trials.jsonl` in that directory.
`load_trial_log(log_dir)$as_tibble()` reads the log back as a tibble.
When you also pass `control`, set `log_dir` in
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
as well, because a `control` object replaces the teleprompter’s own
`log_dir` in every optimizer except `BootstrapFewShot`.

`LabeledFewShot`, `BootstrapFewShotWithRandomSearch`, `COPRO`,
`MIPROv2`, `SIMBA`, `GEPA` and `BetterTogether` take a `seed` for their
own sampling. `GridSearchTeleprompter` uses R’s global generator, and
`BootstrapFewShot` tries rows in order. No seed makes the model’s
answers repeatable.
