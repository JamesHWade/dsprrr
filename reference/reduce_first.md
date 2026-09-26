# Pick the first successful ensemble output

`reduce_first()` makes a reducer for
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md)
that returns the output of the first module that succeeded, in the order
of `modules`. Use it for fallbacks: a module fails, and the next one
answers.

## Usage

``` r
reduce_first()
```

## Value

A function `function(outputs, weights = NULL)` for the `reduce_fn`
argument of
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md).

## Details

[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md)
still runs every module, so this does not save calls.

## See also

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
[`module-graph`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md),
[`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md),
[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md),
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
first <- reduce_first()
first(list(list(answer = "from module 1"), list(answer = "from module 2")))
#> $answer
#> [1] "from module 1"
#> 
```
