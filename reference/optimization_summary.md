# Summarize an optimization result

`optimization_summary()` condenses
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md)
into the numbers most often reported: the number of trials, the best
score and parameters, the score range, the total cost and the
improvement over the baseline.

## Usage

``` r
optimization_summary(module)

# S3 method for class 'dsprrr_optimization_summary'
print(x, ...)
```

## Arguments

- module:

  A module returned by
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  or modified by
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).

- x:

  A `dsprrr_optimization_summary` object.

- ...:

  Unused.

## Value

A `dsprrr_optimization_summary` list with `n_trials`, `best_score`,
`best_trial`, `best_params`, `score_range` (minimum and maximum trial
scores), `total_cost` (when trials record it), `improvement` (best score
minus the baseline or first score) and `compiled`.
[`print()`](https://rdrr.io/r/base/print.html) shows it and returns it
invisibly.

## See also

Other optimization results:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
classifier <- module(signature("text -> sentiment"))
trainset <- data.frame(
  text = c("I love it!", "Terrible experience", "It's okay"),
  sentiment = c("positive", "negative", "neutral")
)
compiled <- compile(classifier, LabeledFewShot(k = 2L), trainset)
optimization_summary(compiled)$best_params
#> $k
#> [1] 2
#> 

if (FALSE) { # \dontrun{
# After a search with several trials, printing gives an overview
optimize_grid(
  classifier,
  data = trainset,
  metric = metric_exact_match(field = "sentiment"),
  grid = data.frame(reasoning_effort = c("none", "low")),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
optimization_summary(classifier)
} # }
```
