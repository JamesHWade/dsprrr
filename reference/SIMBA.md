# SIMBA: stochastic introspective mini-batch ascent

`SIMBA()` finds the training rows the program struggles with, turns them
into an instruction rule and demonstrations, and keeps the change when
the program's score improves. Each step works on a random mini-batch,
which is where the name (stochastic introspective mini-batch ascent)
comes from.

## Usage

``` r
SIMBA(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  bsize = 32L,
  num_candidates = 6L,
  max_steps = 8L,
  max_demos = 4L,
  prompt_model = NULL,
  seed = 0L,
  log_dir = NULL
)
```

## Arguments

- metric:

  A metric function (required), such as
  `metric_exact_match(field = "answer")`. Its `field` also names the
  column that supplies the demonstrations' outputs.

- metric_threshold:

  Accepted for consistency with the other optimizers (see
  [`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md)).
  SIMBA prints it but does not use it.

- max_errors:

  Integer; stop after this many consecutive failed evaluations when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets no `control` (default `5L`).

- bsize:

  Integer mini-batch size (default `32L`, capped at the number of
  training rows).

- num_candidates:

  Integer number of runs per mini-batch (default `6L`).

- max_steps:

  Integer maximum number of steps (default `8L`).

- max_demos:

  Integer maximum number of demonstrations kept, and of hard rows taken
  per step (default `4L`).

- prompt_model:

  Optional ellmer Chat that writes the rules. SIMBA calls its `$chat()`
  method directly, so its conversation history grows during the run.
  `NULL` (the default) uses the fixed rule described in Details.

- seed:

  Seed for the mini-batch samples (default `0L`), or `NULL`.

- log_dir:

  Directory for a
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
  with one trial per kept step, or `NULL` (the default).

## Value

A `SIMBA` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

The program is first scored on `valset` (or `trainset`). Each of up to
`max_steps` steps then:

1.  Samples `bsize` rows of `trainset` and runs the current program on
    them `num_candidates` times.

2.  Rates each row's difficulty as one minus its mean score plus the
    share of runs that disagree with the most common output, and takes
    the hardest rows (at most `max_demos`).

3.  Asks `prompt_model` for a rule based on those rows and appends it to
    the instructions. Without `prompt_model`, the rule is the first hard
    row written out, as in `SIMBA rule: question: ..., expected: ...`.

4.  Adds the hard rows as demonstrations, keeping the latest
    `max_demos`.

5.  Scores the changed program and keeps it only if the score improves.

The search stops at the first step that does not improve the score. With
dsprrr's response cache on (the default), the repeated runs in step 1
return the same cached output, so difficulty reduces to one minus the
mean score; call `configure_cache(enable = FALSE)` to measure
disagreement.

### Differences from DSPy's SIMBA

This is an adapted implementation. It mines hard examples and asks a
model for improvement rules, but it does not reproduce every detail of
DSPy's SIMBA, such as introspection over trajectories of several
candidate programs. Expect similar behavior, not identical results.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
tp <- SIMBA(
  metric = metric_exact_match(field = "answer"),
  bsize = 8L,
  num_candidates = 4L,
  max_steps = 4L,
  max_demos = 2L
)
tp
#> 
#> ── SIMBA Teleprompter 
#> bsize: 8
#> num_candidates: 4
#> max_steps: 4
#> max_demos: 2
#> seed: 0

if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
trainset <- data.frame(
  question = c("Capital of France?", "Capital of Peru?", "Capital of Chad?"),
  answer = c("Paris", "Lima", "N'Djamena")
)
tp <- SIMBA(
  metric = metric_exact_match(field = "answer"),
  bsize = 8L,
  prompt_model = ellmer::chat_openai(model = "gpt-6-luna")
)
compiled <- compile(
  qa,
  tp,
  trainset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
optimization_result(compiled)$extensions$simba$rules
} # }
```
