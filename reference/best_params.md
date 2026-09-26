# Best parameters found by an optimizer

`best_params()` returns the winning parameter values recorded by
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
or
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md),
such as the best `temperature` of a grid search or the number of
demonstrations chosen by
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md).
It reads `optimization_result(module)$best_params`.

## Usage

``` r
best_params(module, flatten = TRUE)
```

## Arguments

- module:

  A module returned by
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  or modified by
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).

- flatten:

  If `TRUE` (the default), length-one list elements are unwrapped to
  plain values. If `FALSE`, the parameters are returned as stored.

## Value

A named list of parameters. For a module that has not been optimized,
`NULL` with a warning.

## See also

Other optimization results:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
classifier <- module(signature("text -> sentiment"))
trainset <- data.frame(
  text = c("I love it!", "Terrible experience", "It's okay"),
  sentiment = c("positive", "negative", "neutral")
)
compiled <- compile(classifier, LabeledFewShot(k = 2L), trainset)
best_params(compiled)
#> $k
#> [1] 2
#> 
```
