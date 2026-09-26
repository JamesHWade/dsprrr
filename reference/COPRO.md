# COPRO: refine instructions by coordinate ascent

`COPRO()` (coordinate prompt optimization) asks a model to rewrite the
program's instructions, scores each rewrite, and continues from the best
one. It changes instructions only, never demonstrations.

## Usage

``` r
COPRO(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  prompt_model = NULL,
  breadth = 10L,
  depth = 3L,
  init_temperature = 1.4,
  track_stats = TRUE,
  seed = 0L,
  log_dir = NULL
)
```

## Arguments

- metric:

  A metric function (required), such as
  `metric_exact_match(field = "answer")`.

- metric_threshold:

  Accepted for consistency with the other optimizers (see
  [`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md)).
  COPRO prints it but does not use it: its failure cutoff is fixed at
  0.5.

- max_errors:

  Integer; stop after this many consecutive failed evaluations when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets no `control` (default `5L`).

- prompt_model:

  Optional ellmer Chat that writes the rewrites. `NULL` (the default)
  uses the `.llm` passed to
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).
  COPRO calls its `$chat()` method directly, so its conversation history
  grows during the run.

- breadth:

  Integer number of rewrites requested per round (default `10L`).

- depth:

  Integer number of rounds (default `3L`).

- init_temperature:

  Intended sampling temperature for the rewrites (default `1.4`). It is
  currently not applied: requests use the prompt model's own settings.

- track_stats:

  Whether to record every scored instruction in the optimization result
  (default `TRUE`).

- seed:

  Seed for the failure scan's row sample (default `0L`), or `NULL`.

- log_dir:

  Directory for a
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
  with one trial per scored rewrite, or `NULL` (the default).

## Value

A `COPRO` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

Before the search, COPRO scores up to 20 random training rows and keeps
the rows scoring below 0.5 as failures; up to three of them are shown in
every rewrite request. Then, for each of `depth` rounds:

1.  `breadth` rewrites of the current best instructions are requested
    from `prompt_model` (or from `.llm` when `prompt_model` is `NULL`),
    and duplicates are dropped.

2.  Each rewrite is scored on `valset` (or `trainset`).

3.  The best rewrite becomes the new starting point if it beats the
    current score.

With `track_stats = TRUE` (the default), every scored instruction is
listed in `optimization_result(compiled)$trials` and in
`optimization_result(compiled)$extensions$copro$history`.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
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
tp <- COPRO(
  metric = metric_exact_match(field = "answer"),
  breadth = 4L,
  depth = 2L
)
tp
#> 
#> ── COPRO Teleprompter 
#> breadth: 4
#> depth: 2
#> init_temperature: 1.4
#> track_stats: TRUE
#> seed: 0

if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
trainset <- data.frame(
  question = c("Capital of France?", "Capital of Peru?", "Capital of Chad?"),
  answer = c("Paris", "Lima", "N'Djamena")
)
tp <- COPRO(
  metric = metric_exact_match(field = "answer"),
  prompt_model = ellmer::chat_openai(model = "gpt-6-luna"),
  breadth = 4L,
  depth = 2L
)
compiled <- compile(
  qa,
  tp,
  trainset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
optimization_result(compiled)$trials
} # }
```
