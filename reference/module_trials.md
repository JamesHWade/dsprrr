# Summarize grid search trials

`module_trials()` summarizes the trials that
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
recorded on a module: how many were run, the best trial, its score and
parameters, and the mean and standard error of all trial scores. It
reads only the grid-search trials in `module$state$trials`, which
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
and
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md)
write. For other optimizers, use
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md)
or
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md).

## Usage

``` r
module_trials(module, objective = c("maximize", "minimize"))
```

## Arguments

- module:

  A module.

- objective:

  `"maximize"` (the default) or `"minimize"`: which end of the scores
  counts as best.

## Value

A one-row tibble with columns:

- `n_trials`: number of trials.

- `best_trial`: identifier of the best trial.

- `best_score`: its score.

- `mean_score`, `std_error`: mean and standard error of the trial
  scores.

- `best_params`: list-column with the best trial's parameters.

- `trials`: list-column with the full trials tibble.

Without trials, `n_trials` is 0 and the scores are `NA`. When every
trial failed, a warning is raised and the scores are `NA`.

## See also

Other grid search:
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md),
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md),
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)

## Examples

``` r
mod <- module(signature("text -> sentiment"))
module_trials(mod)
#> # A tibble: 1 × 7
#>   n_trials best_trial best_score mean_score std_error best_params trials  
#>      <int>      <int>      <dbl>      <dbl>     <dbl> <list>      <list>  
#> 1        0         NA         NA         NA        NA <NULL>      <tibble>

if (FALSE) { # \dontrun{
optimize_grid(
  mod,
  data = devset,
  metric = metric_exact_match(field = "sentiment"),
  grid = data.frame(reasoning_effort = c("none", "low", "medium")),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
module_trials(mod)$best_params
} # }
```
