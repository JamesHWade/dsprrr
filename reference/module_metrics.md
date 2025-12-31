# Summarise optimisation metrics per trial

Flatten the optimisation trials recorded on a module into a tidy data
frame containing per-trial metric summaries. Useful for producing tables
or visualisations comparing trial performance. When yardstick metrics
are supplied, the function also computes those metrics for each trial
using the stored evaluation datasets.

## Usage

``` r
module_metrics(module, metrics = NULL, truth = NULL, estimate = NULL, ...)
```

## Arguments

- module:

  A DSPrrr module optimised with
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).

- metrics:

  Optional yardstick metric (or metric set) to compute for each trial.

- truth:

  Column name (string) containing the ground-truth labels when computing
  yardstick metrics.

- estimate:

  Column name (string) containing the model predictions when computing
  yardstick metrics.

- ...:

  Additional arguments passed to yardstick metrics.

## Value

A tibble with one row per trial containing columns:

- `trial_id` - trial identifier.

- `score` - overall score recorded for the trial.

- `mean_score`, `median_score`, `std_dev` - summary statistics across
  the evaluation scores.

- `n_evaluated`, `n_errors` - counts reported by the evaluation.

- `params` - list-column with the parameters evaluated in the trial.

- `scores` - list-column with the raw per-example scores (if available).

- `yardstick` - list-column containing yardstick metric results when
  requested.

## Examples

``` r
if (FALSE) { # \dontrun{
trial_metrics <- module_metrics(my_module)
yardstick_metrics <- module_metrics(
  my_module,
  metrics = yardstick::metric_set(yardstick::accuracy),
  truth = target,
  estimate = result
)
} # }
```
