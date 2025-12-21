# Summarise optimisation trials for a module

Provide a tidy summary of the optimisation trials recorded on a module.
Useful for reporting best scores, average performance, and highlighting
the winning parameter combination.

## Usage

``` r
module_trials_summary(module, objective = c("maximize", "minimize"))
```

## Arguments

- module:

  A DSPrrr module that has been optimised with
  [`optimize_grid()`](optimize_grid.md).

- objective:

  Optimisation direction; `"maximize"` (default) selects the highest
  score, `"minimize"` selects the lowest.

## Value

A tibble with one row containing:

- `n_trials`: number of trials evaluated.

- `best_trial`: identifier of the best-performing trial.

- `best_score`: best score achieved.

- `mean_score`: mean across all scores.

- `std_error`: standard error of the scores.

- `best_params`: list-column containing the best parameter set.

- `trials`: list-column containing the full trials tibble.

## Examples

``` r
if (FALSE) { # \dontrun{
summary <- module_trials_summary(my_module)
summary$best_params
} # }
```
