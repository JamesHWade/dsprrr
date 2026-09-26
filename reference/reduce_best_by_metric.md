# Pick the ensemble output that scores best against a known answer

`reduce_best_by_metric()` makes a reducer for
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md)
that scores every output with `metric` against an expected value and
returns the best one. The expected value is not part of the inputs: set
it with `attr(reducer, "set_expected")(value)` before each
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md), which
makes this reducer useful when you know the answer, for example while
studying how ensemble members differ on labelled data.

## Usage

``` r
reduce_best_by_metric(metric, maximize = TRUE)
```

## Arguments

- metric:

  A metric called as `metric(output, expected)`, such as
  `metric_f1(field = "answer")`. With a built-in metric and a `field`,
  set the expected value as a list containing that field.

- maximize:

  If `TRUE` (the default), return the highest-scoring output; if
  `FALSE`, the lowest.

## Value

A function `function(outputs, weights = NULL)` for the `reduce_fn`
argument of
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
with a `"set_expected"` attribute that stores the expected value for
later calls.

## Details

Calling the reducer on more than one output before an expected value is
set is an error. A metric error gives a warning and leaves that output
out; if no output can be scored, the reducer errors.

## See also

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
[`module-graph`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md),
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md),
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
pick <- reduce_best_by_metric(metric_f1(field = "answer"))
attr(pick, "set_expected")(list(answer = "the capital is Paris"))
pick(list(
  list(answer = "Paris"),
  list(answer = "the capital of France is Paris")
))
#> $answer
#> [1] "the capital of France is Paris"
#> 
```
