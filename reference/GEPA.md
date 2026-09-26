# GEPA: reflective prompt evolution

`GEPA()` improves a program's instructions by reflection. It scores a
population of candidate programs, shows a model the rows a candidate got
wrong (with the metric's feedback when there is any), and asks it to
rewrite the instructions. Over several generations the best candidates
are kept, combined and rewritten again. GEPA can also rewrite the
complete source of a
[`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
module.

Reflection uses the chat passed to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
as `.llm`, the same chat that runs the program; there is no separate
reflection model argument. Without `.llm`, no model is asked: rewritten
instructions get the fixed sentence "Focus on the failed cases and be
more explicit." (or "Be more explicit and accurate.") appended, and Flex
sources stay unchanged.

## Usage

``` r
GEPA(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  metrics = NULL,
  population_size = 20L,
  generations = 10L,
  mutation_rate = 0.1,
  crossover_rate = 0.7,
  selection = "pareto",
  component_selector = "round_robin",
  use_merge = TRUE,
  max_merge_invocations = 5L,
  seed = NULL,
  log_dir = NULL,
  verbose = TRUE,
  track_stats = TRUE,
  track_best_outputs = FALSE
)
```

## Arguments

- metric:

  A metric function, used when `metrics` is `NULL`. One of the two is
  required.

- metric_threshold:

  Rows scoring below this value count as failures and can be shown to
  reflection. `NULL` (the default) means 1, so any row short of a
  perfect score counts.

- max_errors:

  Integer; stop after this many consecutive failed evaluations when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets no `control` (default `5L`).

- metrics:

  Optional named list of metric functions. The first one picks the
  failed rows shown to reflection; with `selection = "pareto"`, all of
  them are used to rank candidates.

- population_size:

  Integer number of candidates per generation (default `20L`, minimum
  `2L`).

- generations:

  Integer number of generations (default `10L`).

- mutation_rate:

  Probability that a child is rewritten by reflection (default `0.1`).

- crossover_rate:

  Probability that a child combines two parents (default `0.7`).

- selection:

  `"pareto"` (the default) draws parents from candidates that win on at
  least one validation row or lie on the Pareto front of the metrics,
  and ranks by Pareto front when there are several metrics.
  `"current_best"` draws from all valid candidates and ranks by the
  first metric.

- component_selector:

  Component mutation strategy. Use `"round_robin"` to update one
  component at a time, `"all"` to update all components atomically, or a
  function called with `component_ids`, `candidate`, `failed_examples`,
  and `context`. The function must return one or more unique IDs from
  `component_ids`.

- use_merge:

  Whether to attempt lineage-aware merges of complementary component
  changes.

- max_merge_invocations:

  Maximum merge attempts, or `NULL` for no separate merge-attempt cap.

- seed:

  Optional whole-number random seed (default `NULL`).

- log_dir:

  Directory for a
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
  of the run, or `NULL` (the default).

- verbose:

  Whether to print progress messages (default `TRUE`).

- track_stats:

  Whether to keep per-generation statistics in the optimization result
  (default `TRUE`).

- track_best_outputs:

  Whether to retain each validation row's highest-scoring output.
  Requires `track_stats = TRUE`.

## Value

A `GEPA` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

### Feedback metrics

GEPA works best with metrics created by
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md).
When the metric returns `list(score = , feedback = )`, the feedback for
failed rows goes into the reflection prompt, so the model sees why an
output was wrong. This is the key mechanism of the GEPA paper ("GEPA:
Reflective Prompt Evolution Can Outperform Reinforcement Learning",
Agrawal et al., 2025). With a plain numeric metric, reflection sees only
the inputs, the expected values and the predictions.

### How a run works

The first population is the original program plus `population_size - 1`
rewrites of it. Each generation scores every candidate with every metric
on `valset` (or on `trainset` when no `valset` is given), so a run costs
about `population_size * generations` full evaluations plus the
reflection calls. Parents are picked by tournament. A child takes each
component (an instruction or a Flex source) from one of two parents with
probability `crossover_rate`, and is rewritten by reflection with
probability `mutation_rate`. Reflection sees up to five rows that the
parent failed on `trainset`. The best candidate over all generations is
returned; the candidates, their scores and lineage are stored in
`optimization_result(compiled)$extensions$gepa`.

### Differences from DSPy's GEPA

This is an adapted implementation. It keeps reflective rewriting guided
by failures and feedback, per-row winner frontiers, component selection,
lineage-aware merges and Pareto selection over several metrics, but runs
a fixed number of generations instead of DSPy's budget-driven search.
Checkpoint resume, cached subsample merge acceptance and inference-time
candidate selection are not implemented.

A Flex source is optimized as one component: its dynamically built inner
predictors are not tuned separately. The source proposer sees the task
and signature, field schemas, allowed tools and primitives, and the
failed rows with their feedback. Proposed sources are validated before
use; an invalid source is recorded with a failure score and can never be
selected.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
# A small run: 6 candidates over 2 generations
tp <- GEPA(
  metric = metric_exact_match(field = "answer"),
  population_size = 6L,
  generations = 2L,
  seed = 42L
)
tp
#> 
#> ── GEPA Teleprompter 
#> population_size: 6
#> generations: 2
#> mutation_rate: 0.1
#> crossover_rate: 0.7
#> selection: pareto
#> seed: 42

if (FALSE) { # \dontrun{
# Feedback tells the reflection step why an answer was wrong. The metric
# receives the run result and the whole data row.
feedback_metric <- metric_with_feedback(
  function(prediction, expected) {
    if (identical(prediction$answer, expected$answer)) {
      list(score = 1, feedback = "Correct.")
    } else {
      list(
        score = 0,
        feedback = paste0(
          "Expected '", expected$answer,
          "' but got '", prediction$answer, "'."
        )
      )
    }
  },
  field = "answer"
)
tp <- GEPA(
  metric = feedback_metric,
  population_size = 6L,
  generations = 2L,
  seed = 42L
)

qa <- module(signature("question -> answer"))
trainset <- data.frame(
  question = c("What is 2 + 2?", "What is the capital of France?"),
  answer = c("4", "Paris")
)
# `.llm` runs the program and writes the reflections
optimized <- compile(
  qa,
  tp,
  trainset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
optimization_result(optimized)
} # }
```
