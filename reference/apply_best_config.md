# Apply Best Configuration from One Module to Another

Copy the optimized configuration (best parameters, demos, etc.) from a
compiled module to a new or existing module. Useful for transferring
optimization results to a fresh module instance.

## Usage

``` r
apply_best_config(source, target = NULL, include = c("all", "params", "demos"))
```

## Arguments

- source:

  A compiled DSPrrr module with optimization results.

- target:

  A DSPrrr module to apply the configuration to. If NULL, a copy of the
  source module is created.

- include:

  Character vector specifying what to copy:

  - "params": Best parameter values (temperature, etc.)

  - "demos": Few-shot demonstration examples

  - "all": Both params and demos (default)

## Value

The target module with the applied configuration (modified in place if
target was provided, otherwise a new module).

## See also

Other optimizer accessors:
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
if (FALSE) {
# Transfer optimization from one module to another
optimized <- optimize_grid(mod, data, metric, parameters)
new_mod <- module(signature)
apply_best_config(optimized, new_mod)

# Create a fresh copy with the optimized config
fresh <- apply_best_config(optimized, target = NULL)
}
```
