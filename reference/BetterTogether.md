# BetterTogether: run several optimizers in sequence

`BetterTogether()` chains optimizers. A strategy string such as
`"p -> g -> p"` runs the optimizer named `p`, then `g` on its result,
then `p` again. Every intermediate program is scored on a validation set
and the best one is returned. It mirrors DSPy's `BetterTogether`.

## Usage

``` r
BetterTogether(
  metric = NULL,
  optimizers = list(),
  ...,
  metric_threshold = NULL,
  max_errors = 5L,
  default_strategy = "p",
  valset_ratio = 0.1,
  shuffle_trainset_between_steps = TRUE,
  seed = NULL,
  verbose = TRUE
)
```

## Arguments

- metric:

  A metric function (required) used to score every candidate on the
  validation set, such as `metric_exact_match(field = "answer")`.

- optimizers:

  Named list of optimizer objects, such as
  `list(p = BootstrapFewShotWithRandomSearch(metric = metric))`.

- ...:

  Further named optimizer objects, combined with `optimizers`.

- metric_threshold:

  Accepted for consistency with the other optimizers (see
  [`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md));
  `BetterTogether()` does not use it.

- max_errors:

  Does not stop the run; set `max_errors` on each wrapped optimizer
  instead.

- default_strategy:

  Strategy used when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  gets no `strategy` (default `"p"`): optimizer names joined by `->`.

- valset_ratio:

  Share of `trainset` held out for validation when no `valset` is given
  (default `0.1`). `0` skips validation.

- shuffle_trainset_between_steps:

  Whether to shuffle the training rows before each step (default
  `TRUE`).

- seed:

  Optional seed for the validation split and the shuffles.

- verbose:

  Whether to print progress messages (default `TRUE`).

## Value

A `BetterTogether` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

Name the optimizers in `optimizers` or as named arguments in `...`; the
strategy refers to those names. Without any, `BetterTogether()` uses
`p = BootstrapFewShotWithRandomSearch(metric = metric)`.

[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
accepts extra arguments for this optimizer: `strategy` (overrides
`default_strategy`), `optimizer_compile_args` (a list, named by
optimizer, of further arguments for that optimizer's
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
call), and `valset_ratio`, `shuffle_trainset_between_steps` and `seed`,
which override the values stored here.

The validation set is `valset` when given. Otherwise
`floor(valset_ratio * nrow(trainset))` rows are held out, which is none
for fewer than 10 rows at the default ratio. Each step receives the
remaining training rows and the validation set. The original program is
scored as well, so the result can be the unchanged program. Without a
validation set, the last program in the strategy is returned. A step
that fails raises a warning and ends the run with the best program so
far; the default `p` optimizer fails this way when there is no
validation set.

The scored candidates are stored in
`optimization_result(compiled)$extensions$better_together$candidate_programs`.

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
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
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

## Examples

``` r
metric <- metric_exact_match(field = "answer")

tp <- BetterTogether(
  metric = metric,
  p = BootstrapFewShotWithRandomSearch(metric = metric),
  g = GEPA(metric = metric, population_size = 4L, generations = 2L),
  default_strategy = "p -> g -> p"
)
tp
#> 
#> ── BetterTogether Teleprompter 
#> Default strategy: "p -> g -> p"
#> Validation split: 0.1
#> Optimizers: p and g

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
llm <- ellmer::chat_openai(model = "gpt-6-luna")

compiled <- compile(qa, tp, trainset, valset = valset, .llm = llm)
optimization_result(compiled)$extensions$better_together$candidate_programs

# Try another strategy without building a new optimizer
compiled_g <- compile(
  qa,
  tp,
  trainset,
  valset = valset,
  .llm = llm,
  strategy = "g"
)
} # }
```
