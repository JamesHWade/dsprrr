# Compare Module Configuration Before and After Optimization

Show what configuration values changed during optimization. Useful for
understanding the effect of optimization on module settings.

## Usage

``` r
config_diff(module, baseline = NULL)
```

## Arguments

- module:

  A DSPrrr module (preferably compiled).

- baseline:

  Optional named list of baseline configuration values to compare
  against. If NULL, uses reasonable defaults.

## Value

A tibble with columns:

- `parameter`: Parameter name

- `before`: Value before optimization (or default)

- `after`: Current value

- `changed`: Logical indicating if value changed

## See also

Other optimizer accessors:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
if (FALSE) {
mod <- module(signature("text -> sentiment"), type = "predict")
mod$optimize_grid(data, metric, parameters = list(temperature = c(0.3, 1.0)))
config_diff(mod)
}
```
