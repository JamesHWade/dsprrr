# Highest-scoring optimization trials

`top_trials()` returns the `k` best trials of an optimized module, taken
from `optimization_result(x)$trials`, or of a
[TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md).

## Usage

``` r
top_trials(x, k = 5L, objective = c("maximize", "minimize"))
```

## Arguments

- x:

  A module returned by
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
  or modified by
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md),
  or a
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md).

- k:

  Integer number of trials to return (default `5L`).

- objective:

  `"maximize"` (the default) sorts the highest scores first;
  `"minimize"` sorts the lowest first.

## Value

A tibble with the top `k` trials. Modules are sorted by `score` (or
`mean_score`), trial logs by `mean_score`. When there are no trials, a
warning and an empty tibble.

## See also

Other optimization results:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md)

## Examples

``` r
log <- TrialLog$new(optimizer_name = "demo")
program <- module_fn("x -> answer", function(x) list(answer = toupper(x)))
data <- data.frame(x = c("a", "b"), answer = c("A", "B"))
result <- eval_program(program, data, metric_exact_match(field = "answer"))
log$add_trial(complete_trial(create_trial("demo"), result))
top_trials(log, k = 1L)
#> # A tibble: 1 × 20
#>   trial_id    optimizer_name status mean_score n_evaluated n_errors input_tokens
#>   <chr>       <chr>          <chr>       <dbl>       <int>    <int>        <int>
#> 1 trial_2026… demo           compl…          1           2        0           NA
#> # ℹ 13 more variables: output_tokens <int>, total_tokens <int>,
#> #   total_cost <dbl>, provider_calls <int>, metric_calls <int>,
#> #   provider_usage_unknown <lgl>, token_usage_unknown <lgl>, latency_ms <dbl>,
#> #   start_time <dttm>, end_time <dttm>, params <list>, trace_context <list>,
#> #   notes <chr>

if (FALSE) { # \dontrun{
classifier <- module(signature("text -> sentiment"))
devset <- data.frame(
  text = c("Great!", "Terrible."),
  sentiment = c("positive", "negative")
)
optimize_grid(
  classifier,
  data = devset,
  metric = metric_exact_match(field = "sentiment"),
  grid = data.frame(reasoning_effort = c("none", "low", "medium")),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
top_trials(classifier, k = 2L)
} # }
```
