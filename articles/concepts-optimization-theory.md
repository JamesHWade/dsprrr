# How optimization works

An optimizer takes a module, labeled examples and usually a metric, and
searches for a version of the module that scores higher. This page
explains what that search can change, how the metric and the data splits
steer it, why the winning score overstates the result, and where the
model calls go. For the code, see [Compile and
optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)
and [Choose an
optimizer](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md).

## What the search can change

A module’s interface stays fixed: the signature’s input and output
fields and their types. Optimizers change what the module holds as data,
mostly the demos and the instructions, and each has its own way of
producing candidates:

| What changes | Where candidates come from | Optimizers |
|----|----|----|
| Demos | Labeled rows, copied as they are | `LabeledFewShot`, `GridSearchTeleprompter`, `SIMBA`, `MIPROv2` |
| Demos | The module’s own outputs that passed the metric | `BootstrapFewShot`, `BootstrapFewShotWithRandomSearch`, `MIPROv2` |
| Demos | Training rows retrieved for each input by similarity | `KNNFewShot` |
| Instructions or templates | Variants that you write | `GridSearchTeleprompter`, [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) |
| Instructions | A model rewriting them after reading failed rows | `COPRO`, `GEPA`, `SIMBA` (as appended rules) |
| Instructions | A data summary and fixed tips appended | `MIPROv2` |
| Decision thresholds and weights | Probabilities recorded in one extra pass | `ReAnchor` |
| Request settings, such as reasoning effort or temperature | A grid that you define | [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) |
| The program’s source | A model rewriting it | `GEPA` on [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md) modules, [`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md), [`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md) |

The three kinds of demos behave differently. Labeled demos show the
answers you wrote, but each takes its output from a single column, so a
demo for a three-field output shows one field. Bootstrapped demos show
the module’s complete output, including fields your data does not label,
such as a
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md)
reasoning field or the intermediate steps of a pipeline. They passed the
metric, which is not the same as being right: if the metric checks one
field, the others can be wrong. Retrieved demos change with every input,
so there is no fixed prompt to inspect.

Instructions are the other main lever, and the right wording depends on
the model. A rewrite that helps one model can hurt another, which is why
a search run on a cheaper model should be confirmed on the model you
will use.

Two targets go beyond the prompt text. `ReAnchor` changes numbers that
are never sent to the model: the threshold that turns a probability into
`TRUE`, or the weights that pick an option. It records probabilities
once and fits the numbers without further calls. At the other end, GEPA
can treat a whole
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
program as one candidate and rewrite its source; see [Flex: optimize a
whole
program](https://jameshwade.github.io/dsprrr/articles/flex-optimization.md).

Request settings, such as `reasoning_effort` or `temperature`, are
searched only by
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).
Reasoning models such as gpt-6-luna reject `temperature` while they
reason, so sweep `reasoning_effort` with them, or turn reasoning off
(`reasoning_effort = "none"`) before sweeping `temperature`.

## How the metric steers the search

Each candidate ends up as one number: the metric’s mean over some rows.
The optimizer sees nothing else, which has three consequences.

The search improves only what the metric measures. In [Compile and
optimize](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md),
a search scored on the category alone produced a module that got every
category right and two of three priorities wrong.

Ties are common. A pass/fail metric on a few rows gives many candidates
the same score, and optimizers keep the candidate they saw first,
usually the unchanged module or the first variant. A graded metric, such
as
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md)
or a custom metric that gives partial credit, separates candidates
better.

The metric can say more than a number. A metric built with
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md)
also returns a sentence about each failure, and GEPA passes those
sentences to the model that rewrites the instructions.
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md)
lets a metric score the steps a program took, not just its final output.
[Metrics and
evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md)
covers how to choose and test a metric.

## Training, validation and test rows

Optimizers use labeled rows for two jobs. The training rows are
material: demos are copied or bootstrapped from them, and COPRO, SIMBA
and GEPA show the model some of the training rows the module got wrong.
The validation rows are the yardstick that ranks candidates. A third
set, the test rows, is scored once at the end and never shown to an
optimizer. [Metrics and
evaluation](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md)
shows how to split one table three ways with
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md).

When you pass no `valset`, optimizers fall back in different ways:

| Optimizer | Without `valset` |
|----|----|
| `GridSearchTeleprompter` | Scores on a random fifth of `trainset` and takes demos from the rest |
| `BootstrapFewShotWithRandomSearch` | Stops with an error: `valset` is required |
| `BetterTogether`, `Omni` | Hold out 10% of `trainset` |
| `COPRO`, `MIPROv2`, `SIMBA`, `GEPA` | Score candidates on `trainset` itself |
| `ReAnchor` | Checks fitted settings on folds of `trainset`; `valset` is only reported |
| `LabeledFewShot`, `KNNFewShot`, `BootstrapFewShot` | Do not rank candidates |

Scoring on `trainset` rewards candidates that suit the training rows,
which is exactly what demos copied from those rows do. The gain then
shrinks, or disappears, on new inputs.

## Why the winning score is too high

Picking the best of several noisy scores favors the candidate that got
lucky on the validation rows. Suppose every candidate has the same true
accuracy, 70%, and each is scored on the same 20 validation rows. The
expected score of the winner still grows with the number of candidates
compared:

``` r

set.seed(1)
winner_score <- function(n_candidates, n_rows = 20, accuracy = 0.7) {
  max(rbinom(n_candidates, n_rows, accuracy) / n_rows)
}

n_candidates <- c(1, 5, 20, 100)
expected_winner <- vapply(
  n_candidates,
  function(k) mean(replicate(5000, winner_score(k))),
  numeric(1)
)
data.frame(n_candidates, expected_winner = round(expected_winner, 2))
#>   n_candidates expected_winner
#> 1            1            0.70
#> 2            5            0.82
#> 3           20            0.88
#> 4          100            0.93
```

None of these candidates is better than the others, yet the best of 20
looks far better than 70%. Optimizers that compare many candidates on
the same rows, such as `MIPROv2`, `GEPA` and
`BootstrapFewShotWithRandomSearch`, report this inflated number as
`best_score`. More validation rows shrink the effect, and a score on
test rows that no step has seen removes it. The same holds for your own
comparisons: if you try ten optimizers and keep the best, score the
winner again on fresh rows.

Two other effects push in the same direction. Training rows reach the
prompt, as demos, as examples shown to the rewriting model, and in
`SIMBA`’s fallback rules, so duplicates or near-duplicates between the
splits inflate every score. Keep the splits disjoint. And a test set
that you score after every change slowly turns into a validation set;
score it once per decision.

## Where the model calls go

Almost all the calls in a search are spent scoring: the number of
candidates times the rows each is scored on. Proposing candidates adds
comparatively little, one call per rewrite. With 200 training rows and
50 validation rows, the scoring calls come to roughly:

| Optimizer and settings | Scoring calls |
|----|----|
| `GridSearchTeleprompter` with 4 variants | 4 × 50 = 200 |
| `BootstrapFewShotWithRandomSearch`, default 16 candidates | 16 × 50 = 800, plus bootstrapping |
| `MIPROv2` with `auto = "light"` | 16 batches of 5 plus 4 passes over 50 = 280, plus bootstrapping |
| `COPRO` with the defaults, `breadth = 10L` and `depth = 3L` | Up to 30 × 50 = 1,500, plus 50 for the baseline and at least 200 to find failures |
| `GEPA` with the defaults, 20 candidates for 10 generations | 20 × 10 × (50 + 200) = 50,000 |

GEPA’s defaults suit large budgets; the example in [Choose an
optimizer](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md)
uses a population of 6 for 4 generations. That page gives a cost formula
for each optimizer.

There are three ways to spend less. Score fewer candidates or fewer
rows, accepting noisier comparisons. Run the search on a cheaper model
and confirm the winner on the one you will deploy. Or, with the
optimizers that support it, cap the search with
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
which stops at a limit on calls, tokens, cost or time and returns the
best module found so far.
