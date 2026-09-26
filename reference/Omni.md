# Omni: explore with several optimizers, then continue from the best

`Omni()` runs several optimizers (the explorers) independently from the
same program, scores each result on one validation set, and then runs a
`continuation` optimizer from the winner. The original program stays a
candidate throughout, so an explorer or continuation that makes things
worse cannot replace a better program.

## Usage

``` r
Omni(
  metric,
  explorers,
  continuation,
  metric_threshold = NULL,
  max_errors = 5L,
  valset_ratio = 0.1,
  parallel = FALSE,
  num_workers = NULL,
  seed = NULL,
  verbose = TRUE
)
```

## Arguments

- metric:

  A metric function (required) used to score every candidate on the same
  validation set, such as `metric_exact_match(field = "answer")`.

- explorers:

  Named list of at least two optimizer objects. Each starts from its own
  copy of the input program.

- continuation:

  An optimizer object run from the best exploration candidate.

- metric_threshold:

  Accepted for consistency with the other optimizers (see
  [`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md));
  `Omni()` does not use it.

- max_errors:

  Does not stop the run; set `max_errors` on each explorer and on the
  continuation instead.

- valset_ratio:

  Share of `trainset` held out for validation when no `valset` is given
  (default `0.1`, must be above 0).

- parallel:

  Whether to compile the explorers at the same time with mirai (default
  `FALSE`). Parallel exploration requires `.llm = NULL`: each worker
  creates its own default chat from `OPENAI_API_KEY`,
  `ANTHROPIC_API_KEY` or `GOOGLE_API_KEY`.

- num_workers:

  Number of mirai workers for parallel exploration. `NULL` (the default)
  uses one worker per explorer.

- seed:

  Optional whole-number seed for the validation split, sequential
  exploration and the mirai worker streams.

- verbose:

  Whether to print progress messages (default `TRUE`).

## Value

An `Omni` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

Omni adapts the explore, pick-best and continue pattern of the Omni
meta-optimizer in the [GEPA project](https://github.com/gepa-ai/gepa),
described in the [GEPA Omni
announcement](https://gepa-ai.github.io/gepa/blog/2026/07/22/optimize-anything-omni/).

Omni needs validation data: pass `valset` to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
or it holds out `floor(valset_ratio * nrow(trainset))` rows and stops
with an error when that is zero. The seed program, every explorer result
and the continuation result are all scored on it, and the highest score
wins.

Omni sets no shared budget, because each optimizer has its own budget
controls. Give the explorers comparable budgets before building
`Omni()`. The validation scoring is extra work on top of those budgets.

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
accepts extra arguments for this optimizer: `explorer_compile_args` (a
list, named by explorer, of further arguments for that explorer's
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
call), `continuation_compile_args`, and `valset_ratio`, `parallel`,
`num_workers` and `seed`, which override the values stored here. The
candidates are stored in
`optimization_result(compiled)$extensions$omni$candidate_programs`.

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
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
metric <- metric_exact_match(field = "answer")

tp <- Omni(
  metric = metric,
  explorers = list(
    bootstrap = BootstrapFewShotWithRandomSearch(metric = metric),
    gepa = GEPA(metric = metric, population_size = 4L, generations = 2L)
  ),
  continuation = GEPA(
    metric = metric,
    population_size = 4L,
    generations = 2L
  )
)
tp
#> 
#> ── Omni Teleprompter 
#> Explorers: bootstrap and gepa
#> Continuation: <dsprrr::GEPA>
#> Validation split: 0.1
#> Parallel exploration: FALSE

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
optimization_result(compiled)$extensions$omni$candidate_programs
} # }
```
