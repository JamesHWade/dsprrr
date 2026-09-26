# Compare a module's settings with baseline values

`config_diff()` lists the values in `module$config` next to baseline
values, changed rows first. Use it to see which settings an optimizer
changed.

## Usage

``` r
config_diff(module, baseline = NULL)
```

## Arguments

- module:

  A module.

- baseline:

  Optional named list of baseline values; it overrides the default
  baseline entry by entry.

## Value

A tibble with columns `parameter`, `before` and `after` (values
formatted as text) and `changed` (logical).

## Details

The default baseline assumes provider defaults: `temperature = 1`,
`top_p = 1`, `frequency_penalty = 0` and `presence_penalty = 0`. A value
the module never set is shown as `"<default>"` and counts as changed, as
do internal fields such as `.module_kind`, so pass a `baseline` that
matches your starting configuration for a meaningful comparison.

## See also

Other optimization results:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
mod <- module(signature("text -> sentiment"), config = list(temperature = 0))
config_diff(mod, baseline = list(temperature = 0.7))
#> # A tibble: 6 × 4
#>   parameter         before    after     changed
#>   <chr>             <chr>     <chr>     <lgl>  
#> 1 temperature       0.70      0         TRUE   
#> 2 top_p             1         <default> TRUE   
#> 3 frequency_penalty 0         <default> TRUE   
#> 4 presence_penalty  0         <default> TRUE   
#> 5 params            <default> 0         TRUE   
#> 6 .module_kind      <default> predict   TRUE   
```
