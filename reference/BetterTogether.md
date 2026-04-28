# BetterTogether Teleprompter

A meta-teleprompter that runs multiple optimization strategies in
sequence. This mirrors DSPy's `BetterTogether` optimizer surface for
composing prompt optimizers and future weight optimizers with a strategy
string such as `"p -> g -> p"`.

`BetterTogether()` accepts optimizers either through the named
`optimizers` list or as named arguments in `...`. Strategy steps refer
to those names. Each intermediate program is evaluated on `valset` when
available; the best scored program is returned. Without a validation
set, the latest program in the strategy is returned.

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

  Metric function used to score candidate programs.

- optimizers:

  Named list of
  [Teleprompter](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md)
  objects. If omitted, dsprrr defaults to
  `p = BootstrapFewShotWithRandomSearch(metric = metric)`.

- ...:

  Named
  [Teleprompter](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md)
  objects, used as strategy keys. These are combined with `optimizers`.

- metric_threshold:

  Minimum score required to be considered successful.

- max_errors:

  Maximum number of errors allowed during evaluation.

- default_strategy:

  Strategy to use when
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  does not receive `strategy`. Defaults to `"p"`.

- valset_ratio:

  Fraction of `trainset` to hold out as validation when `valset` is not
  supplied. Set to `0` to skip validation.

- shuffle_trainset_between_steps:

  Whether to shuffle training rows before each optimizer step.

- seed:

  Optional random seed for reproducible splitting and shuffling.

- verbose:

  Whether to print progress messages.

## Examples

``` r
if (FALSE) { # \dontrun{
metric <- metric_exact_match(field = "answer")

tp <- BetterTogether(
  metric = metric,
  optimizers = list(
    p = BootstrapFewShotWithRandomSearch(metric = metric),
    g = GEPA(metric = metric, population_size = 4L, generations = 2L)
  ),
  default_strategy = "p -> g -> p"
)

compiled <- compile(tp, qa_module, trainset, valset = valset, .llm = llm)
compiled$config$optimizer$candidate_programs
} # }
```
