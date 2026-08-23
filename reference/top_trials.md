# Get Top Performing Trials

Extract the top k trials from a module's optimization history or a
TrialLog, ranked by score.

## Usage

``` r
top_trials(x, k = 5L, objective = c("maximize", "minimize"))
```

## Arguments

- x:

  A DSPrrr module with optimization trials, or a TrialLog object.

- k:

  Integer; number of top trials to return. Default is 5.

- objective:

  Optimization direction: "maximize" (default) or "minimize".

## Value

A tibble with the top k trials, including trial_id, score, parameters,
and other trial metadata.

## See also

Other optimizer accessors:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md)

## Examples

``` r
if (FALSE) {
# Get top 3 trials from module
top_trials(mod, k = 3)

# Get top trials from a TrialLog
log <- load_trial_log("path/to/logs")
top_trials(log, k = 10, objective = "minimize")
}
```
