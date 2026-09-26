# Pick the most common ensemble output

`reduce_majority()` makes a reducer for
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md)
that returns the output whose value occurs most often. It is the default
reducer.

## Usage

``` r
reduce_majority(field = NULL, tie_breaker = "first")
```

## Arguments

- field:

  The output field to vote on. With `NULL`, the first field.

- tie_breaker:

  `"first"` picks, among tied values, the one that occurs first;
  `"random"` picks one at random.

## Value

A function `function(outputs, weights = NULL)` for the `reduce_fn`
argument of
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md).

## Details

Values are compared as strings. If the ensemble has `weights`, each vote
counts with its module's weight. The reducer returns the whole output of
the first module that gave the winning value.

## See also

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
[`module-graph`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md),
[`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md),
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
vote <- reduce_majority(field = "sentiment")
vote(list(
  list(sentiment = "positive", confidence = 0.6),
  list(sentiment = "negative", confidence = 0.9),
  list(sentiment = "positive", confidence = 0.8)
))
#> $sentiment
#> [1] "positive"
#> 
#> $confidence
#> [1] 0.6
#> 
```
