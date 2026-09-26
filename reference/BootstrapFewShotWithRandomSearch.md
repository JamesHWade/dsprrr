# BootstrapFewShot with random search over candidate programs

`BootstrapFewShotWithRandomSearch()` compiles several candidate
programs, scores each one on a validation set, and returns the best. Use
it when a single
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
run is too sensitive to which demonstrations it happens to collect.
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
requires a `valset`.

## Usage

``` r
BootstrapFewShotWithRandomSearch(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  num_candidate_programs = 16L,
  num_threads = 1L,
  stop_at_score = NULL,
  max_bootstrapped_demos = 4L,
  max_labeled_demos = 16L,
  max_rounds = 1L,
  teacher_settings = NULL,
  seed = NULL,
  log_dir = NULL
)
```

## Arguments

- metric:

  A metric function (required), such as
  `metric_exact_match(field = "answer")`. It scores candidates on
  `valset` and is passed to each
  [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
  candidate.

- metric_threshold:

  Passed to each
  [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
  candidate: the minimum score for a bootstrapped output to become a
  demonstration. `NULL` (the default) keeps any output that scores above
  0.

- max_errors:

  Integer; stop after this many consecutive failed evaluations when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets no `control` (default `5L`).

- num_candidate_programs:

  Integer number of candidates, including the three fixed ones (default
  `16L`).

- num_threads:

  Integer number of validation rows scored at the same time (default
  `1L`, sequential). Ignored when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets a `control`, whose own `num_threads` applies.

- stop_at_score:

  Stop as soon as a candidate scores at least this value, or `NULL` (the
  default) to score every candidate.

- max_bootstrapped_demos, max_labeled_demos, max_rounds,
  teacher_settings:

  Passed to each
  [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
  candidate (defaults `4L`, `16L`, `1L` and `NULL`). `teacher_settings`
  is currently not applied.

- seed:

  Integer seed, such as `42L`, used to draw the candidates' seeds and to
  sample the `labeled_only` demonstrations. A double such as `42`
  currently makes the `labeled_only` candidate fail with a warning.

- log_dir:

  Directory for a
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
  with one trial per scored candidate, or `NULL` (the default).

## Value

A `BootstrapFewShotWithRandomSearch` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

`num_candidate_programs` candidates are built in this order:

1.  `baseline`: the uncompiled program.

2.  `labeled_only`:
    [`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md)
    with `k = max_labeled_demos`.

3.  `bootstrap_unshuffled`:
    [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
    with the settings below.

4.  `bootstrap_seed_<n>`: further
    [`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md)
    runs, each given a random seed.

Each candidate is scored on `valset`, and the highest mean score wins
(ties go to the earlier candidate). BootstrapFewShot does not currently
shuffle the training rows, so the seeded candidates see the rows in the
same order and differ only when the model's outputs differ. With
dsprrr's response cache on, repeated identical requests return identical
outputs.

The ranked candidates are stored in
`optimization_result(compiled)$extensions$bootstrap_few_shot_with_random_search$candidate_programs`.
Programs containing Flex or RLM modules are rejected.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
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
tp <- BootstrapFewShotWithRandomSearch(
  metric = metric_exact_match(field = "answer"),
  num_candidate_programs = 6L,
  max_labeled_demos = 2L,
  stop_at_score = 0.95,
  seed = 42L
)
tp
#> 
#> ── BootstrapFewShotWithRandomSearch Teleprompter 
#> num_candidate_programs: 6
#> num_threads: 1
#> max_bootstrapped_demos: 4
#> max_labeled_demos: 2
#> max_rounds: 1
#> stop_at_score: 0.95
#> seed: 42

if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
trainset <- data.frame(
  question = c("Capital of France?", "Capital of Peru?", "Capital of Chad?"),
  answer = c("Paris", "Lima", "N'Djamena")
)
valset <- data.frame(
  question = c("Capital of Japan?", "Capital of Kenya?"),
  answer = c("Tokyo", "Nairobi")
)
compiled <- compile(
  qa,
  tp,
  trainset,
  valset = valset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
top_trials(compiled)
} # }
```
