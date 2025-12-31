# Convert a Metric to a Reward Function

Adapts a metric function (with signature
`function(prediction, expected)`) to a reward function (with signature
`function(prediction, inputs)`).

## Usage

``` r
as_reward_fn(metric, expected_field = "expected", prediction_field = NULL)
```

## Arguments

- metric:

  A metric function created with `metric_*()` functions

- expected_field:

  Character. Name of the field in inputs that contains the expected
  value. Default is "expected".

- prediction_field:

  Character. If the prediction is a list, extract this field before
  comparing. If NULL, uses the whole prediction.

## Value

A reward function suitable for
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md)
and
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md)

## Examples

``` r
# Convert exact match metric to reward function
reward <- as_reward_fn(metric_exact_match(), expected_field = "answer")

# With field extraction
reward <- as_reward_fn(
  metric_exact_match(field = "sentiment"),
  expected_field = "expected_sentiment"
)
```
