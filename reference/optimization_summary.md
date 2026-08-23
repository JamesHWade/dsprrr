# Get Optimization Summary

Get a concise summary of optimization results for a module. Combines
information from trials, best parameters, and cost tracking.

## Usage

``` r
optimization_summary(module)
```

## Arguments

- module:

  A DSPrrr module with optimization history.

## Value

A list with:

- `n_trials`: Number of trials evaluated

- `best_score`: Best score achieved

- `best_trial`: ID of the best trial

- `best_params`: Best parameter configuration

- `score_range`: Min and max scores across trials

- `total_cost`: Total cost of optimization (if tracked)

- `improvement`: Score improvement from first to best trial

## See also

Other optimizer accessors:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
if (FALSE) {
optimize_grid(mod, data, metric, parameters)
summary <- optimization_summary(mod)
print(summary)
}
```
