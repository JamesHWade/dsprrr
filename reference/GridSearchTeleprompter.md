# Grid search over instructions and demos

`GridSearchTeleprompter()` tries each row of a `variants` table
(different instructions, prompt templates or settings) on one module,
scores every variant with `metric`, and returns a copy of the module
with the best variant applied. All variants share one set of `k`
demonstrations drawn from the training set.

## Usage

``` r
GridSearchTeleprompter(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  variants = tibble::tibble(id = 1L, instructions = NA_character_,
    template = NA_character_),
  k = 2L,
  eval_sample_size = 50L,
  verbose = TRUE
)
```

## Arguments

- metric:

  A metric function such as `metric_exact_match(field = "sentiment")`.
  Required when compiling.

- metric_threshold, max_errors:

  Accepted for consistency with the other optimizers (see
  [`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md));
  grid search does not use them.

- variants:

  A data frame with one row per variant and an `id` column; see Details.
  The default is a single variant that keeps the module's instructions
  and template.

- k:

  Integer number of demonstrations attached to every variant (default
  `2L`). Use `0L` for none.

- eval_sample_size:

  Integer cap on the number of held-out scoring rows when no `valset` is
  given (default `50L`).

- verbose:

  Whether to show a progress bar (default `TRUE`).

## Value

A `GridSearchTeleprompter` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

Each row of `variants` is one candidate. These columns have an effect:

- `id` (required) labels the variant.

- `instructions` replaces the signature's instructions (`NA` keeps
  them).

- `instructions_suffix` is appended to the original instructions.

- `template` replaces the prompt template.

- Runtime settings such as `temperature` or `top_p` are sent to the
  chat.

Other columns are stored in the module's `config` but do not change the
prompt.

When you pass `valset` to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
variants are scored on it and the demonstrations are drawn from all of
`trainset`. Without `valset`,
`min(eval_sample_size, ceiling(0.2 * nrow(trainset)))` random rows of
`trainset` are held out for scoring and the demonstrations come from the
remaining rows. The drawn demonstrations replace any the module already
had. The split and the draw use R's random number generator without a
fixed seed, so call [`set.seed()`](https://rdrr.io/r/base/Random.html)
first for reproducible results.

The search runs
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
on a copy of the module, so the scores of all variants are available
from
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md)
and
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md).

## See also

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

Other grid search:
[`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md),
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md),
[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md),
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)

## Examples

``` r
variants <- data.frame(
  id = c("terse", "explicit"),
  instructions = c(
    "Answer with one word.",
    "Label the sentiment of the text as positive, negative or neutral."
  )
)
tp <- GridSearchTeleprompter(
  metric = metric_exact_match(field = "sentiment"),
  variants = variants,
  k = 1L
)
tp@variants
#>         id                                                      instructions
#> 1    terse                                             Answer with one word.
#> 2 explicit Label the sentiment of the text as positive, negative or neutral.

if (FALSE) { # \dontrun{
classifier <- module(signature("text -> sentiment"))
trainset <- data.frame(
  text = c("I love it!", "Terrible experience", "It's okay", "Works well"),
  sentiment = c("positive", "negative", "neutral", "positive")
)
set.seed(1)
optimized <- compile(
  classifier,
  tp,
  trainset,
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
top_trials(optimized)
} # }
```
