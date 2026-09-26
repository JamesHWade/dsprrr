# Demonstrations attached to a module

`best_demos()` returns the few-shot demonstrations a module currently
carries, such as those chosen by
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md)
or
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md).

## Usage

``` r
best_demos(module, as_tibble = FALSE)
```

## Arguments

- module:

  A module.

- as_tibble:

  If `TRUE`, return a tibble with one row per demonstration and one
  column per input and output field. If `FALSE` (the default), return
  the list of demonstrations as stored.

## Value

A list or tibble of demonstrations, or `NULL` when the module has none.

## See also

Other optimization results:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
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
best_demos(compiled, as_tibble = TRUE)
#> # A tibble: 2 × 2
#>   text       output  
#>   <chr>      <chr>   
#> 1 It's okay  neutral 
#> 2 I love it! positive

# An uncompiled module has none
best_demos(classifier)
#> NULL
```
