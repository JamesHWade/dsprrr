# Extract Best Parameters from a Module

Get the best parameter configuration from an optimized module. This is
the parameter set that achieved the highest (or lowest, for
minimization) score during optimization.

## Usage

``` r
best_params(module, flatten = TRUE)
```

## Arguments

- module:

  A DSPrrr module that has been optimized.

- flatten:

  Logical; if TRUE (default), return a simple named list. If FALSE,
  return the parameters as stored (may include nested structure).

## Value

A named list of the best parameters, or NULL if the module has not been
optimized.

## See also

Other optimizer accessors:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
if (FALSE) {
mod <- module(signature("text -> sentiment"), type = "predict")
mod$optimize_grid(
  data = train_data,
  metric = metric_exact_match(),
  parameters = list(temperature = c(0.3, 0.7, 1.0))
)
best_params(mod)
# $temperature
# [1] 0.7
}
```
