# Metric that returns a score and feedback

`metric_with_feedback()` marks a metric whose function returns textual
feedback along with its score, as
`list(score = , feedback = "what went wrong")`. Feedback-aware
optimizers such as
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md) use
the feedback to guide their reflection step, as in DSPy's GEPA.
Everywhere else, including
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
only the score is used;
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
also returns the feedback in `feedbacks`.

## Usage

``` r
metric_with_feedback(fn, field = NULL)
```

## Arguments

- fn:

  A function called as `fn(prediction, expected)`, where `prediction` is
  the output (a named list) and `expected` the whole data row, as a
  one-row data frame. It returns a logical or numeric score, or
  `list(score = , feedback = )` with `feedback` a single string.

- field:

  The name of the data column that holds the expected output. It is only
  stored, in the metric's `"field"` attribute, for optimizers that look
  it up; `fn` still receives the whole row and prediction.

## Value

A metric function of class `dsprrr_feedback_metric`.

## See also

Other metrics:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
[`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md),
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
[`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
[`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
graded <- metric_with_feedback(
  function(prediction, expected) {
    if (identical(prediction$answer, expected$answer)) {
      list(score = 1, feedback = "Correct.")
    } else {
      list(
        score = 0,
        feedback = paste0("Expected '", expected$answer, "' but got '", prediction$answer, "'.")
      )
    }
  },
  field = "answer"
)
row <- data.frame(question = "What is 2 + 2?", answer = "4")
graded(list(answer = "4"), row)
#> $score
#> [1] 1
#> 
#> $feedback
#> [1] "Correct."
#> 
graded(list(answer = "5"), row)
#> $score
#> [1] 0
#> 
#> $feedback
#> [1] "Expected '4' but got '5'."
#> 
```
