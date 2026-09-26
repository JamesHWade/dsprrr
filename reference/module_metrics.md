# Per-trial rows from a grid search

`module_metrics()` returns one row per grid-search trial recorded on a
module by
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
(or
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md)).
Like
[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md),
it reads only `module$state$trials`.

## Usage

``` r
module_metrics(module, metrics = NULL, truth = NULL, estimate = NULL, ...)
```

## Arguments

- module:

  A module.

- metrics:

  Optional yardstick metric or metric set. Currently never computed; see
  Details.

- truth, estimate:

  Column names for the yardstick metrics. Required when `metrics` is
  supplied.

- ...:

  Passed to the yardstick metrics.

## Value

A tibble with one row per trial and columns `trial_id`, `score`,
`mean_score` (the trial score), `median_score`, `std_dev`,
`n_evaluated`, `n_errors`, `params` (list-column of the trial's
parameters), `scores` and `yardstick`.

## Details

The per-example evaluation results are not kept with the trials, so
`median_score`, `std_dev`, `n_evaluated` and `n_errors` are `NA`,
`scores` is empty, and the yardstick metrics requested with `metrics`
are not computed: the `yardstick` column is always `NULL`. Use
[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md)
or
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)
for trial scores.

## See also

Other grid search:
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md),
[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md),
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)

## Examples

``` r
# Without trials the result is an empty tibble with these columns
module_metrics(module(signature("text -> sentiment")))
#> # A tibble: 0 × 10
#> # ℹ 10 variables: trial_id <int>, score <dbl>, mean_score <dbl>,
#> #   median_score <dbl>, std_dev <dbl>, n_evaluated <int>, n_errors <int>,
#> #   params <list>, scores <list>, yardstick <list>
```
