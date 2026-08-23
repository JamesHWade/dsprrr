# Inspect an Optimization Result

`optimization_result()` is the stable, read-only boundary for learning
what an optimizer did. Every dsprrr teleprompter reports the same core
fields; optimizer-specific evidence lives under a namespaced
`extensions` entry.

## Usage

``` r
optimization_result(program)

# S3 method for class 'dsprrr_optimization_result'
print(x, ...)
```

## Arguments

- program:

  A dsprrr module returned by
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  or
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).

- x:

  An optimization result.

- ...:

  Additional arguments, currently unused.

## Value

A `dsprrr_optimization_result` with fields:

- `version`: Result schema version.

- `optimizer`: Optimizer identity.

- `status`: Either `"completed"` or `"partial"`.

- `baseline_score`, `best_score`, and `best_trial`: Comparable outcome
  measures when the optimizer evaluates candidates.

- `best_params`: Winning parameter values.

- `trials`: Trial-level evidence as a tibble.

- `lineage`: How the winning candidate was derived.

- `budget`: Planned and consumed optimization budget.

- `stop_reason`: Why the optimizer stopped.

- `extensions`: Optimizer-specific evidence, namespaced by optimizer.

Returns `NULL` when `program` has not been optimized.

## See also

Other optimizer accessors:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
if (FALSE) {
optimized <- compile(program, GEPA(metric = metric), trainset)
result <- optimization_result(optimized)
result$best_score
result$trials
result$extensions$gepa
}
```
