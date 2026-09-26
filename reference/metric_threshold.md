# Turn a numeric metric into pass/fail

`metric_threshold()` wraps a metric so that it returns `TRUE` when the
score passes `threshold` and `FALSE` otherwise. Use it where a yes/no
judgement is needed, for example to count only answers with an F1 score
of at least 0.8 as correct.

## Usage

``` r
metric_threshold(metric, threshold = 0.5, comparison = ">=")
```

## Arguments

- metric:

  A metric returning a logical or numeric score, or
  `list(score = , feedback = )`.

- threshold:

  The score to compare against.

- comparison:

  How to compare the score with `threshold`: one of `">="` (the
  default), `">"`, `"=="`, `"<"` or `"<="`.

## Value

A metric returning `TRUE` or `FALSE`. If `metric` returns feedback, the
result is `list(score = TRUE/FALSE, feedback = )`. The `field` attribute
and trace-aware and feedback classes of `metric` are kept.

## See also

Other metrics:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
[`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md),
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
[`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
f1 <- metric_f1(field = "answer")
good_enough <- metric_threshold(f1, threshold = 0.8)
row <- data.frame(question = "Where is the Louvre?", answer = "The Louvre is in Paris")

f1(list(answer = "In Paris"), row)
#> [1] 0.5714286
good_enough(list(answer = "In Paris"), row)
#> [1] FALSE
good_enough(list(answer = "The Louvre is in Paris, France"), row)
#> [1] TRUE
```
