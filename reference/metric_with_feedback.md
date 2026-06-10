# Create a Metric with Textual Feedback

Wraps a metric function so it can return both a numeric score and
textual feedback explaining the score. Feedback-aware optimizers such as
[GEPA](https://jameshwade.github.io/dsprrr/reference/GEPA.md) use this
feedback to guide their reflection step, mirroring DSPy's GEPA
feedback-metric protocol.

The wrapped function must return either:

- a single numeric (or logical) score, or

- a list with elements `score` (numeric or logical) and optionally
  `feedback` (a single character string).

Feedback metrics work everywhere ordinary metrics do:
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and optimizers simply use the `score` element. Optimizers that
understand feedback additionally collect the `feedback` strings.

## Usage

``` r
metric_with_feedback(fn, field = NULL)
```

## Arguments

- fn:

  A function with signature `function(prediction, expected)` returning a
  score or a `list(score = , feedback = )`.

- field:

  Optional name of the column in training data containing the expected
  output (stored as the metric's `field` attribute, like other built-in
  metrics).

## Value

A metric function classed as `dsprrr_feedback_metric`.

## Examples

``` r
metric <- metric_with_feedback(
  function(prediction, expected) {
    if (identical(prediction, expected)) {
      list(score = 1, feedback = "Correct.")
    } else {
      list(
        score = 0,
        feedback = paste0("Expected '", expected, "' but got '", prediction, "'.")
      )
    }
  },
  field = "answer"
)
metric("4", "4")
#> $score
#> [1] 1
#> 
#> $feedback
#> [1] "Correct."
#> 
metric("5", "4")
#> $score
#> [1] 0
#> 
#> $feedback
#> [1] "Expected '4' but got '5'."
#> 
```
