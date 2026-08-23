# Tutorial 5: Finding the Best Configuration

In [Tutorial
4](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md),
you improved your module with examples. But there are many other knobs
to tune: temperature, instructions, prompt templates. How do you find
the best combination?

The answer: **let dsprrr search for you**.

We return to a sentiment classifier—this time for product reviews—so you
can see how much headroom parameter tuning adds on top of few-shot
demos. This is also the module you’ll take to production in [Tutorial
6](https://jameshwade.github.io/dsprrr/articles/tutorial-deploy-to-production.md).

**Time**: 30-35 minutes

## What You’ll Build

An optimized module that automatically finds the best configuration
through grid search.

## Prerequisites

- Completed [Tutorial
  4](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md)
- `OPENAI_API_KEY` set in your environment

``` r

library(dsprrr)
library(ellmer)
library(tibble)

chat <- chat_openai(model = "gpt-5-mini")
```

## Step 1: Set Up the Problem

Let’s build a sentiment analyzer and optimize it:

``` r

sig <- signature(
  "review -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the sentiment of this product review."
)

classifier <- module(sig)
```

Create training and test data:

``` r

# Training data for optimization
trainset <- tibble::tibble(
  review = c(
    "Absolutely love this product! Best purchase ever.",
    "Complete waste of money. Broke after one day.",
    "It's okay. Does what it says.",
    "Exceeded all my expectations!",
    "Terrible quality. Very disappointed.",
    "Nothing special, but it works.",
    "Amazing! Would buy again.",
    "Don't bother. Total junk.",
    "Decent for the price.",
    "Fantastic quality and fast shipping!"
  ),
  sentiment = c(
    "positive", "negative", "neutral",
    "positive", "negative", "neutral",
    "positive", "negative", "neutral",
    "positive"
  )
)

# Held-out test data (never used for optimization)
testset <- tibble::tibble(
  review = c(
    "Great value for money!",
    "Stopped working after a week.",
    "Average product, average price.",
    "Couldn't be happier with this purchase!"
  ),
  sentiment = c("positive", "negative", "neutral", "positive")
)
```

## Step 2: Grid Search Over Temperature

Temperature controls randomness. Lower = more deterministic, higher =
more creative. Let’s find the best value:

``` r

optimize_grid(  classifier,
  data = trainset,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(
    temperature = c(0.0, 0.3, 0.7, 1.0)
  ),
  .llm = chat
)
```

## Step 3: View Optimization Results

See what happened:

``` r

# All trials
module_trials(classifier)
```

Get the summary:

``` r

# Metrics summary
module_metrics(classifier)
```

Check the best configuration:

``` r

# Best score achieved
optimization_result(classifier)$best_score

# Best parameters
optimization_result(classifier)$best_params
```

## Step 4: The Module Remembers

After optimization, the module automatically uses the best
configuration:

``` r

# This uses the best temperature found
run(classifier, review = "This product changed my life!", .llm = chat)
```

## Step 5: Grid Search Over Instructions

Instructions matter a lot. Let’s test different phrasings:

``` r

# Reset to try different parameters
classifier2 <- module(sig)

optimize_grid(  classifier2,
  data = trainset,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(
    instructions_suffix = c(
      "",
      " Be brief.",
      " Consider the overall tone.",
      " Focus on the customer's satisfaction level."
    )
  ),
  .llm = chat
)

module_trials(classifier2)
```

The `instructions_suffix` is appended to your base instructions.

## Step 6: Multi-Parameter Grid Search

Search over multiple parameters at once:

``` r

classifier3 <- module(sig)

optimize_grid(  classifier3,
  data = trainset,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(
    temperature = c(0.0, 0.5),
    instructions_suffix = c("", " Be decisive.")
  ),
  .llm = chat
)

module_trials(classifier3)
```

This tests all combinations: 2 temperatures × 2 instruction variants = 4
total configurations.

## Step 7: Using GridSearchTeleprompter

For more control, use `GridSearchTeleprompter` with explicit variants:

``` r

variants <- tibble(
  id = c("concise", "analytical", "empathetic"),
  instructions_suffix = c(
    " Respond with just the sentiment.",
    " Analyze the language carefully before deciding.",
    " Consider how the customer is feeling."
  )
)

teleprompter <- GridSearchTeleprompter(
  variants = variants,
  metric = metric_exact_match(field = "sentiment"),
  k = 2L  # Number of few-shot examples to include
)

optimized <- compile(
  program = module(sig),
  teleprompter = teleprompter,
  trainset = trainset,
  .llm = chat
)
```

This combines instruction optimization with few-shot example selection.

## Step 8: Evaluate on Held-Out Test Data

Always test on data the optimizer never saw:

``` r

# Evaluate the optimized module on test data
test_results <- evaluate(
  optimized,
  testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = chat
)

test_results
```

Compare to baseline:

``` r

baseline <- module(sig)

baseline_results <- evaluate(
  baseline,
  testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = chat
)

cat("Baseline test accuracy:", scales::percent(baseline_results$mean_score), "\n")
cat("Optimized test accuracy:", scales::percent(test_results$mean_score), "\n")
```

## Step 9: Different Metrics for Different Tasks

Not all tasks use exact match. dsprrr provides several metrics:

``` r

# For text generation - token overlap
metric_f1()

# Check if output contains a string
metric_contains("error", ignore_case = TRUE)

# Custom logic
metric_custom(function(prediction, expected) {
  # Return TRUE/FALSE or 0-1 score
  nchar(prediction) < 100
}, name = "concise")

# Threshold wrapper
metric_threshold(metric_f1(), threshold = 0.8)
```

## Step 10: Tracking Costs

Optimization uses LLM calls. Track the cost:

``` r

# After optimization
classifier$trace_summary()

# Total session cost
session_cost()
```

## What You Learned

In this tutorial, you:

1.  Used
    [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
    to search over parameters
2.  Viewed results with
    [`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md)
    and
    [`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md)
3.  Searched over temperature and instructions
4.  Combined parameters in multi-dimensional grids
5.  Used `GridSearchTeleprompter` for instruction + demo optimization
6.  Evaluated on held-out test data
7.  Explored different metrics
8.  Tracked optimization costs

## The Optimization Mindset

Key principles:

1.  **Measure first**: Know your baseline before optimizing
2.  **Use held-out data**: Never test on training data
3.  **Start simple**: Try temperature before complex instruction
    variants
4.  **Watch costs**: Grid search multiplies LLM calls
5.  **Diminishing returns**: Often 80% of improvement comes from first
    optimization

## When to Use Each Approach

| Approach | Best For |
|----|----|
| [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) | Quick parameter sweeps |
| `LabeledFewShot` | Adding examples from data |
| `GridSearchTeleprompter` | Instruction + example optimization |
| Manual tuning | Initial exploration |

## Next Steps

Your module is optimized. Now how do you save it and deploy it? Continue
to:

- **[Tutorial 6: Taking to
  Production](https://jameshwade.github.io/dsprrr/articles/tutorial-deploy-to-production.md)**
  — Save and deploy modules
- **[Quick
  Reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md)**
  — All teleprompters and parameters
- **[How Optimization
  Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)**
  — Theory behind the search
