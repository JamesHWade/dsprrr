# Create a BestOfN Wrapper Module

Factory function to create a BestOfN wrapper around any module. Runs the
module N times and returns the best result according to a reward
function.

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

  A Module object to wrap

- N:

  Maximum number of attempts (default 3)

- reward_fn:

  Reward function with signature `function(prediction, inputs)`,
  returning a score between 0 and 1. Use
  [`as_reward_fn()`](https://jameshwade.github.io/dsprrr/reference/as_reward_fn.md)
  to convert a metric. If NULL, uses a default that returns 1.0 for all
  valid predictions.

- threshold:

  Score threshold for early stopping (default 1.0)

- fail_count:

  Maximum consecutive failures before erroring (default N)

- ...:

  Additional arguments passed to the module constructor

## Value

A BestOfNModule object

## Examples

``` r
# Create a basic QA module
qa <- module(signature("question -> answer"))

# Wrap it with best-of-3 selection
wrapper <- best_of_n(qa, N = 3)

# With a custom reward function
one_word_reward <- function(pred, inputs) {
  words <- strsplit(as.character(pred$answer), "\\s+")[[1]]
  if (length(words) == 1) 1.0 else 0.0
}
wrapper <- best_of_n(qa, N = 5, reward_fn = one_word_reward)

# Using a metric-based reward function
wrapper <- best_of_n(
  qa,
  N = 3,
  reward_fn = as_reward_fn(metric_exact_match(field = "answer")),
  threshold = 1.0
)
```
