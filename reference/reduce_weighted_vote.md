# Pick the ensemble output with the most total weight

`reduce_weighted_vote()` makes a reducer for
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md)
that adds up the `weights` of the modules behind each value and returns
the output with the largest total. Without weights, every vote counts 1.

## Usage

``` r
reduce_weighted_vote(field = NULL)
```

## Arguments

- field:

  The output field to vote on. With `NULL`, the first field.

## Value

A function `function(outputs, weights = NULL)` for the `reduce_fn`
argument of
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md).

## Details

Values are compared as strings. Ties go to the value that sorts first
alphabetically. The reducer returns the whole output of the first module
that gave the winning value.

## See also

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
[`module-graph`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md),
[`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md),
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
vote <- reduce_weighted_vote(field = "sentiment")
outputs <- list(
  list(sentiment = "positive"),
  list(sentiment = "negative"),
  list(sentiment = "positive")
)
vote(outputs)
#> $sentiment
#> [1] "positive"
#> 

# The second module is trusted more than the other two together
vote(outputs, weights = c(0.4, 0.9, 0.4))
#> $sentiment
#> [1] "negative"
#> 
```
