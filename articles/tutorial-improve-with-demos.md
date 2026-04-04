# Tutorial 4: Improving with Examples

In [Tutorial
3](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.md),
you built extractors that return structured data. But how do you make
them more accurate? The answer: **show the LLM examples of correct
behavior**.

This technique—called few-shot learning—is one of the most powerful ways
to improve LLM performance.

**Time**: 25-30 minutes

## What You’ll Build

A classifier that learns from examples, with measurable accuracy
improvement.

## Prerequisites

- Completed [Tutorial
  3](https://jameshwade.github.io/dsprrr/articles/tutorial-structured-outputs.md)
- `OPENAI_API_KEY` set in your environment

``` r
library(dsprrr)
library(ellmer)
library(tibble)

chat <- chat_openai(model = "gpt-5-mini")
```

## Step 1: The Problem

Let’s build a customer support ticket classifier:

``` r
sig <- signature(
 "ticket -> category: enum('billing', 'technical', 'shipping', 'general')",
  instructions = "Classify the customer support ticket."
)

classifier <- module(sig, type = "predict")
```

Test it:

``` r
run(classifier, ticket = "My package hasn't arrived yet", .llm = chat)

run(classifier, ticket = "I was charged twice for my order", .llm = chat)

run(classifier, ticket = "The app keeps crashing when I try to login", .llm = chat)
```

This works, but how do we know it’s accurate? And how do we improve it?

## Step 2: Create Training Data

First, let’s create labeled examples using
[`dsp_trainset()`](https://jameshwade.github.io/dsprrr/reference/dsp_trainset.md):

``` r
trainset <- dsp_trainset(
  ticket = c(
    "I was charged twice for the same item",
    "How do I update my payment method?",
    "The website won't load on my phone",
    "My password reset email never arrived",
    "When will my order ship?",
    "Can I change my delivery address?",
    "The product arrived damaged",
    "I need a refund for my subscription",
    "How do I contact customer service?",
    "What are your business hours?"
  ),
  category = c(
    "billing",
    "billing",
    "technical",
    "technical",
    "shipping",
    "shipping",
    "shipping",
    "billing",
    "general",
    "general"
  )
)

trainset
```

The
[`dsp_trainset()`](https://jameshwade.github.io/dsprrr/reference/dsp_trainset.md)
function creates a properly formatted tibble where input columns are
matched with expected output columns.

## Step 3: Measure Baseline Accuracy

Before improving, let’s measure current performance:

``` r
baseline_results <- run_dataset(classifier, trainset, .llm = chat)
baseline_results
```

Calculate accuracy:

``` r
correct <- sum(baseline_results$category == trainset$category)
total <- nrow(trainset)
baseline_accuracy <- correct / total

cat("Baseline accuracy:", scales::percent(baseline_accuracy), "\n")
cat("Correct:", correct, "out of", total, "\n")
```

## Step 4: Add Manual Demonstrations

The simplest way to improve: show the LLM examples. Add them via the
`demos` parameter:

``` r
classifier_with_demos <- module(
  sig,
  type = "predict",
  demos = list(
    list(
      inputs = list(ticket = "I was charged twice for the same item"),
      output = list(category = "billing")
    ),
    list(
      inputs = list(ticket = "The app crashes on startup"),
      output = list(category = "technical")
    ),
    list(
      inputs = list(ticket = "My package is late"),
      output = list(category = "shipping")
    )
  )
)
```

Each demo has `inputs` (a list of input values) and `output` (a list of
expected outputs).

Test the improved classifier:

``` r
run(classifier_with_demos, ticket = "I need a receipt for my purchase", .llm = chat)

run(classifier_with_demos, ticket = "The button doesn't respond when clicked", .llm = chat)
```

## Step 5: Measure Improvement

Run on the same test set:

``` r
improved_results <- run_dataset(classifier_with_demos, trainset, .llm = chat)

correct_improved <- sum(improved_results$category == trainset$category)
improved_accuracy <- correct_improved / total

cat("Baseline accuracy:", scales::percent(baseline_accuracy), "\n")
cat("Improved accuracy:", scales::percent(improved_accuracy), "\n")
cat("Improvement:", scales::percent(improved_accuracy - baseline_accuracy), "\n")
```

## Step 6: Automatic Demo Selection with LabeledFewShot

Manually picking demos is tedious. The `LabeledFewShot` teleprompter
automatically selects good examples from your training data:

``` r
# Create a teleprompter that selects 3 examples
teleprompter <- LabeledFewShot(k = 3L)

# Compile the module with the teleprompter
compiled <- compile_module(
  program = classifier,
  teleprompter = teleprompter,
  trainset = trainset
)
```

The
[`compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.md)
function takes your module, a teleprompter strategy, and training data.
It returns an improved module with automatically selected
demonstrations.

## Step 7: Check What Was Selected

See which examples the teleprompter chose:

``` r
# The compiled module now has demos
compiled$config$demos
```

## Step 8: Compare All Three Versions

Let’s see how each version performs:

``` r
# Run all three on the test set
results_baseline <- run_dataset(classifier, trainset, .llm = chat)
results_manual <- run_dataset(classifier_with_demos, trainset, .llm = chat)
results_compiled <- run_dataset(compiled, trainset, .llm = chat)

# Calculate accuracies
acc_baseline <- mean(results_baseline$category == trainset$category)
acc_manual <- mean(results_manual$category == trainset$category)
acc_compiled <- mean(results_compiled$category == trainset$category)

comparison <- tibble(
  Version = c("Baseline (no demos)", "Manual demos (3)", "LabeledFewShot (3)"),
  Accuracy = scales::percent(c(acc_baseline, acc_manual, acc_compiled))
)

comparison
```

## Step 9: Using Built-in Metrics

dsprrr provides metrics for common evaluation tasks. Use
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md)
for classification:

``` r
metric <- metric_exact_match(field = "category")

# Evaluate the compiled module
eval_result <- evaluate(compiled, trainset, metric = metric, .llm = chat)

eval_result
```

The
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
function returns detailed results including: - `mean_score`: Overall
accuracy - `scores`: Per-example scores - `predictions`: What the model
predicted

## Step 10: Varying the Number of Examples

More examples aren’t always better. Let’s compare:

``` r
results <- list()

for (k in c(1L, 2L, 3L, 4L, 5L)) {
  tp <- LabeledFewShot(k = k)
  compiled_k <- compile_module(classifier, tp, trainset)

  eval_k <- evaluate(compiled_k, trainset, metric = metric, .llm = chat)

  results[[as.character(k)]] <- tibble(
    k = k,
    accuracy = eval_k$mean_score
  )
}

do.call(rbind, results)
```

You’ll often find that 2-4 examples works best. Too many can confuse the
LLM or exceed context limits.

## What You Learned

In this tutorial, you:

1.  Created labeled training data with
    [`dsp_trainset()`](https://jameshwade.github.io/dsprrr/reference/dsp_trainset.md)
2.  Measured baseline accuracy before improving
3.  Added manual demonstrations with the `demos` parameter
4.  Used `LabeledFewShot` for automatic demo selection
5.  Compiled modules with
    [`compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.md)
6.  Evaluated with
    [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md)
7.  Experimented with different numbers of examples

## Why Demos Work

LLMs are excellent at pattern matching. When you show examples of
correct behavior: - They understand the **format** you want - They learn
**edge cases** specific to your domain - They pick up on **subtle
distinctions** between categories

This is called “in-context learning”—the LLM learns from the examples in
the prompt without any weight updates.

## The Few-Shot Trade-off

| More Examples            | Fewer Examples      |
|--------------------------|---------------------|
| Better pattern coverage  | Lower token costs   |
| More edge cases shown    | Faster inference    |
| Risk of context overflow | May miss edge cases |

Start with 2-3 examples, measure, and adjust.

## Next Steps

You’ve improved your module with examples. But what about other
parameters—temperature, instructions, templates? Continue to:

- **[Tutorial 5: Finding Best
  Configuration](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md)**
  — Grid search over parameters
- **[Quick
  Reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md)**
  — All metrics and teleprompters
- **[How Optimization
  Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)**
  — The theory behind teleprompters
