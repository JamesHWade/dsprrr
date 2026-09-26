# Copy optimized settings to another module

`apply_best_config()` copies the best parameters, the demonstrations and
the optimization result from an optimized module to another module, for
example to reuse a compiled configuration on a module with a different
chat.

## Usage

``` r
apply_best_config(source, target = NULL, include = c("all", "params", "demos"))
```

## Arguments

- source:

  An optimized module, returned by
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  or modified by
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).

- target:

  The module to update. It is modified in place. If `NULL`, a fresh copy
  of `source` is created and updated.

- include:

  What to copy: `"all"` (the default), `"params"` (the best parameters,
  such as `temperature`) or `"demos"` (the demonstrations).

## Value

The updated target module, invisibly.

## See also

Other optimization results:
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
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

fresh <- module(signature("text -> sentiment"))
apply_best_config(compiled, fresh, include = "demos")
length(fresh$demos)
#> [1] 2
```
