# Combine several modules into one

`ensemble()` builds a module that runs every module in `modules` on the
same inputs and combines their outputs with a reducer, by default a
majority vote. It is a module, not an optimizer:
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) it like
any other.

## Usage

``` r
ensemble(modules, reduce_fn = NULL, weights = NULL, ...)
```

## Arguments

- modules:

  A list of modules with the same input field names, for example
  variants compiled with different demos or instructions. The ensemble
  takes the signature of the first one.

- reduce_fn:

  A function called as `reduce_fn(outputs, weights)`, where `outputs` is
  the list of outputs from the modules that succeeded, that returns one
  output. The default is
  [`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md);
  see also
  [`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
  [`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md)
  and
  [`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md).

- weights:

  Optional numeric weights, one per module, such as validation scores.
  The reducer receives the weights of the modules that succeeded.

- ...:

  Passed to the ensemble module: `chat` or `config`.

## Value

A module (an R6 object of class `EnsembleModule`).

## Details

A module that fails gives a warning and is left out of the vote; the
ensemble fails only if every module fails. Identical modules sharing a
chat return identical cached responses when caching is on, so the
members should differ (demos, instructions, model or temperature) for
the vote to mean something. The returned module's
`get_individual_outputs()` method lists each member's output from the
last run.

## See also

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`best_of_n()`](https://jameshwade.github.io/dsprrr/reference/best_of_n.md),
[`module-graph`](https://jameshwade.github.io/dsprrr/reference/module-graph.md),
[`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md),
[`reduce_best_by_metric()`](https://jameshwade.github.io/dsprrr/reference/reduce_best_by_metric.md),
[`reduce_first()`](https://jameshwade.github.io/dsprrr/reference/reduce_first.md),
[`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md),
[`reduce_weighted_vote()`](https://jameshwade.github.io/dsprrr/reference/reduce_weighted_vote.md),
[`refine()`](https://jameshwade.github.io/dsprrr/reference/refine.md),
[`step()`](https://jameshwade.github.io/dsprrr/reference/step.md),
[`with_assertions()`](https://jameshwade.github.io/dsprrr/reference/with_assertions.md)

## Examples

``` r
# Offline stand-ins for three differently tuned classifiers
sig <- signature("text -> sentiment")
voters <- list(
  module_fn(sig, function(text) "positive"),
  module_fn(sig, function(text) "negative"),
  module_fn(sig, function(text) "positive")
)

majority <- ensemble(voters)
run(majority, text = "Not bad at all")
#> $sentiment
#> [1] "positive"
#> 

# Weight the second voter by its validation accuracy
weighted <- ensemble(
  voters,
  reduce_fn = reduce_weighted_vote(),
  weights = c(0.4, 0.9, 0.4)
)
run(weighted, text = "Not bad at all")
#> $sentiment
#> [1] "negative"
#> 

# A custom reducer: answer "unsure" unless every voter agrees
unanimous <- function(outputs, weights = NULL) {
  labels <- vapply(outputs, function(o) o$sentiment, character(1))
  if (length(unique(labels)) == 1) outputs[[1]] else list(sentiment = "unsure")
}
run(ensemble(voters, reduce_fn = unanimous), text = "Not bad at all")
#> $sentiment
#> [1] "unsure"
#> 
```
