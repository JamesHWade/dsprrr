# Tutorial 5: Optimize a module

In [Tutorial
4](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md),
you compared three versions of a classifier by hand. Once you have
several instruction ideas, demo counts, or model settings to test, doing
that manually gets tedious fast. Give dsprrr a set of candidates,
labeled data, and a metric, and it runs the experiment for you—scoring
every candidate with
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
or
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md)
and keeping the winner for a final check on held-out test data.

``` r

library(dsprrr)
library(ellmer)

chat <- chat_openai(model = "gpt-6-luna")
```

*Recorded outputs on the package website were captured with
`gpt-5-mini`; `gpt-6-luna` may answer slightly differently.*

## A classifier and two datasets

The task is product-review sentiment:

``` r

sig <- signature(
  "review -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the sentiment of this product review."
)
```

The search scores candidates on `trainset`. `testset` stays out of the
search, so the final check measures how the chosen configuration handles
new reviews rather than how well it fits the training ones:

``` r

trainset <- tibble::tribble(
  ~review,                                              ~sentiment,
  "Absolutely love this product! Best purchase ever.",  "positive",
  "Complete waste of money. Broke after one day.",      "negative",
  "It's okay. Does what it says.",                      "neutral",
  "Exceeded all my expectations!",                      "positive",
  "Terrible quality. Very disappointed.",               "negative",
  "Nothing special, but it works.",                     "neutral",
  "Amazing! Would buy again.",                          "positive",
  "Don't bother. Total junk.",                          "negative",
  "Decent for the price.",                              "neutral",
  "Fantastic quality and fast shipping!",               "positive"
)

testset <- tibble::tribble(
  ~review,                                     ~sentiment,
  "Great value for money!",                    "positive",
  "Stopped working after a week.",             "negative",
  "Average product, average price.",           "neutral",
  "Couldn't be happier with this purchase!",   "positive"
)
```

## Measure the baseline

Score the module as it is before searching, so you have a number to
beat:

``` r

baseline <- module(sig)

baseline_results <- evaluate(
  baseline,
  testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = chat
)

baseline_results$mean_score
#> [1] 1
baseline_results$scores
#> [1] 1 1 1 1
```

In the recorded run, `gpt-5-mini` labels all four test reviews
correctly, so here a search can at best match the baseline. Four reviews
are too few to rank configurations anyway: each is worth 25 percentage
points, and one changed answer can reverse a comparison. The datasets in
this tutorial are small to keep the walkthrough cheap; when the choice
matters, test on dozens to hundreds of labeled rows.

## Search instruction variants

[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
runs every candidate on `data`, scores it with `metric`, and leaves the
module set to the best candidate. `parameters` lists the values to try.
Each `instructions_suffix` is appended to the signature’s instructions,
so this grid compares the original instructions with two longer
versions:

``` r

classifier <- module(sig)

optimize_grid(
  classifier,
  data = trainset,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(
    instructions_suffix = c(
      "",
      "Consider the overall tone.",
      "Focus on how satisfied the customer is."
    )
  ),
  .llm = chat
)

optimization_result(classifier)$trials
#> # A tibble: 3 x 6
#>   trial_id parameters        score n_evaluated n_errors total_cost
#>      <int> <list>            <dbl>       <int>    <int>      <dbl>
#> 1        1 <named list [1]>    1.0          10        0     0.0012
#> 2        2 <named list [1]>    1.0          10        0     0.0013
#> 3        3 <named list [1]>    0.9          10        0     0.0013
```

Each candidate runs on all ten training reviews, 30 calls in total.
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
changes `classifier` in place.
[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md)
returns a one-row summary with the best score, the best parameters, and
a `trials` list-column; `optimization_result(classifier)$trials` unpacks
that into one row per candidate, with its score and cost.

When `parameters` names several settings,
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
tries every combination, so the number of calls is the product of the
numbers of values, times the number of rows. Settings you can search for
a module created with
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
include:

| Setting | What it changes |
|----|----|
| `instructions` | Replaces the signature’s instructions. |
| `instructions_suffix` | Appends text to the signature’s instructions. |
| `template` | The glue string that lays out the inputs, such as `"Review: {review}"`. |
| `reasoning_effort` | How much a reasoning model such as `gpt-6-luna` thinks before answering (`"none"` to `"max"`). |
| `temperature`, `top_p` | Sampling settings sent to the model. `gpt-6-luna` accepts them only with `reasoning_effort = "none"`. |

## Search instructions and demos together

[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md)
combines an instruction search with the demos of Tutorial 4: it attaches
`k` demos sampled from the training data, scores each instruction
variant with those demos in place, and keeps the best variant. Like
other teleprompters it works through
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
which returns a new module and leaves the one you pass in unchanged:

``` r

variants <- tibble::tibble(
  id = c("concise", "analytical", "empathetic"),
  instructions_suffix = c(
    "Respond with just the sentiment.",
    "Analyze the language carefully before deciding.",
    "Consider how the customer is feeling."
  )
)

tp <- GridSearchTeleprompter(
  variants = variants,
  metric = metric_exact_match(field = "sentiment"),
  k = 2L
)

set.seed(2026)
optimized <- compile(module(sig), tp, trainset, .llm = chat)

optimization_result(optimized)$best_params$id
#> [1] "concise"
```

Unless you pass `valset` to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md)
scores the variants on a random 20% of `trainset` (two reviews here) and
draws the demos from the rest.
[`set.seed()`](https://rdrr.io/r/base/Random.html) makes that split
repeatable. Two reviews cannot tell three variants apart, so give it a
larger `valset` when you have the data.

## Check the result on the test set

``` r

test_results <- evaluate(
  optimized,
  testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = chat
)

test_results$mean_score
#> [1] 1
```

Compare `test_results$mean_score` with `baseline_results$mean_score`. A
configuration that wins on the training reviews but not on the test set
has overfit the training reviews rather than improving on the task.

Each candidate costs a full pass over its data: the instruction grid
above makes 30 calls and the teleprompter 6. The `total_cost` column of
`optimization_result(classifier)$trials` records what each candidate
cost.

Next, [Tutorial
6](https://jameshwade.github.io/dsprrr/articles/tutorial-deploy-to-production.md)
saves a compiled module so that a later session can use it without
repeating the search.
