# Extract Best Demos from a Compiled Module

Get the few-shot demonstration examples from a compiled module. Returns
the demos that were selected during optimization (e.g., by
LabeledFewShot or BootstrapFewShot teleprompters).

## Usage

``` r
best_demos(module, as_tibble = FALSE)
```

## Arguments

- module:

  A DSPrrr module that has been compiled with demos.

- as_tibble:

  Logical; if TRUE, return demos as a tibble. If FALSE (default), return
  as a list.

## Value

A list or tibble of demonstration examples, or NULL if the module has no
demos.

## See also

Other optimizer accessors:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
if (FALSE) {
tp <- LabeledFewShot(k = 4L)
compiled <- compile(mod, tp, trainset)
demos <- best_demos(compiled)
}
```
