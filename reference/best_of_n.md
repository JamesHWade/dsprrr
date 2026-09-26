# Run a module up to N times and keep the best result

`best_of_n()` wraps a module so that each
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) calls it
up to `N` times, scores every prediction with `reward_fn`, and returns
the best one. It stops early as soon as a prediction scores at least
`threshold`.

## Usage

``` r
best_of_n(
  module,
  N = 3L,
  reward_fn = NULL,
  threshold = 1,
  fail_count = NULL,
  ...
)
```

## Arguments

- module:

  The module to wrap.

- N:

  Maximum number of attempts.

- reward_fn:

  A function called as `reward_fn(prediction, inputs)`, where
  `prediction` is the attempt's output (a named list) and `inputs` are
  the inputs given to
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md). It
  returns a score, usually between 0 and 1; logical values become 0
  or 1. The default gives 1 to every prediction, so with the default
  `threshold` the first attempt that succeeds is returned and `N` only
  limits retries after errors.
  [`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md)
  turns a metric into a reward function.

- threshold:

  Score at which to stop early. Default 1.

- fail_count:

  Number of consecutive failed attempts after which to give up with an
  error. Defaults to `N`.

- ...:

  Passed to the wrapper: `chat` (an ellmer Chat, which defaults to the
  wrapped module's chat) or `config`.

## Value

A module (an R6 object of class `BestOfNModule`) with the same signature
as `module`.

## Details

Each attempt uses its own partition of the response cache, so attempts
get fresh responses even when caching is on. A failed attempt gives a
warning and is skipped. If a reward function errors or returns `NA`,
that attempt cannot be chosen; if no attempt has a score, the first
successful prediction is returned.

The returned module has a `get_attempts()` method that lists the
attempts of the last run (or of all runs, with `all = TRUE`) with their
scores. With `.return_format = "structured"`, the metadata also records
`n_attempts`, `best_score`, `all_scores` and `early_stopped`.

## See also

Other composition:
[`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md),
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble.md),
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
# An offline stand-in for a model that phrases its answer differently
# each time
answerer <- module_fn(
  "question -> answer",
  function(question) sample(c("Paris", "It is Paris", "The capital is Paris"), 1)
)
one_word <- function(prediction, inputs) {
  length(strsplit(prediction$answer, " ")[[1]]) == 1
}

set.seed(5)
best <- best_of_n(answerer, N = 5L, reward_fn = one_word)
run(best, question = "What is the capital of France?")
#> $answer
#> [1] "Paris"
#> 
best$get_attempts()
#> # A tibble: 3 × 4
#>     run attempt prediction       score
#>   <int>   <int> <list>           <dbl>
#> 1     1       1 <named list [1]>     0
#> 2     1       2 <named list [1]>     0
#> 3     1       3 <named list [1]>     1

if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
concise <- best_of_n(qa, N = 3L, reward_fn = one_word)
run(
  concise,
  question = "What is the capital of France?",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
