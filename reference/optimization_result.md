# Inspect what an optimizer did

`optimization_result()` reports what
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
or
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
did to a program: the optimizer, the baseline and best scores, the
winning parameters, every trial, and why the search stopped. All
optimizers report the same core fields; optimizer-specific details live
under `extensions`, keyed by the optimizer's name in snake case (for
example `extensions$gepa` or `extensions$better_together`).

## Usage

``` r
optimization_result(program)

# S3 method for class 'dsprrr_optimization_result'
print(x, ...)
```

## Arguments

- program:

  A module returned by
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  or modified by
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).

- x:

  An optimization result.

- ...:

  Additional arguments, currently unused.

## Value

A `dsprrr_optimization_result` list with fields:

- `version`: result schema version.

- `optimizer`: the optimizer's name.

- `status`: `"completed"`, or `"partial"` when a budget stopped the run.

- `baseline_score`, `best_score` and `best_trial`: outcome measures,
  `NA` for optimizers that score nothing.

- `best_params`: the winning parameter values.

- `trials`: trial-level results as a tibble.

- `lineage`: how the winning candidate was derived.

- `budget`: the budget used, for optimizers run under
  [`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md).

- `stop_reason`: why the optimizer stopped.

- `extensions`: optimizer-specific details.

Returns `NULL` when `program` has not been optimized.
[`print()`](https://rdrr.io/r/base/print.html) shows a short summary and
returns `x` invisibly.

## See also

Other optimization results:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
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

result <- optimization_result(compiled)
result
#> <dsprrr_optimization_result>
#> Optimizer: LabeledFewShot
#> Status: completed
#> Trials: 0
#> Stopped: completed
result$best_params
#> $k
#> [1] 2
#> 
result$extensions$labeled_few_shot
#> $sample
#> [1] TRUE
#> 
#> $seed
#> [1] 123
#> 

# Not optimized
optimization_result(classifier)
#> NULL
```
