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
#> 
#> Attaching package: 'dsprrr'
#> The following object is masked from 'package:methods':
#> 
#>     signature
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
#> Warning: Failed to serialize output_type for cache key
#> ℹ Using class name as fallback: "ellmer::TypeObject"
#> ℹ This may cause cache collisions for different output types
#> ✖ Original error: cannot coerce type 'object' to vector of type 'character'
#> $category
#> [1] "shipping"

run(classifier, ticket = "I was charged twice for my order", .llm = chat)
#> Warning: Failed to serialize output_type for cache key
#> ℹ Using class name as fallback: "ellmer::TypeObject"
#> ℹ This may cause cache collisions for different output types
#> ✖ Original error: cannot coerce type 'object' to vector of type 'character'
#> $category
#> [1] "billing"

run(classifier, ticket = "The app keeps crashing when I try to login", .llm = chat)
#> Warning: Failed to serialize output_type for cache key
#> ℹ Using class name as fallback: "ellmer::TypeObject"
#> ℹ This may cause cache collisions for different output types
#> ✖ Original error: cannot coerce type 'object' to vector of type 'character'
#> $category
#> [1] "technical"
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
#>                                   ticket  category
#> 1  I was charged twice for the same item   billing
#> 2     How do I update my payment method?   billing
#> 3     The website won't load on my phone technical
#> 4  My password reset email never arrived technical
#> 5               When will my order ship?  shipping
#> 6      Can I change my delivery address?  shipping
#> 7            The product arrived damaged  shipping
#> 8    I need a refund for my subscription   billing
#> 9     How do I contact customer service?   general
#> 10         What are your business hours?   general
```

The
[`dsp_trainset()`](https://jameshwade.github.io/dsprrr/reference/dsp_trainset.md)
function creates a properly formatted tibble where input columns are
matched with expected output columns.

## Step 3: Measure Baseline Accuracy

Before improving, let’s measure current performance:

``` r
baseline_results <- run_dataset(classifier, trainset, .llm = chat)
#> Processing 5/10 |  50% | ETA:  1s
#> Processing 10/10 | 100% | ETA:  0s
#> 
baseline_results
#> # A tibble: 10 × 3
#>    ticket                                category  result   
#>    <chr>                                 <chr>     <list>   
#>  1 I was charged twice for the same item billing   <chr [1]>
#>  2 How do I update my payment method?    billing   <chr [1]>
#>  3 The website won't load on my phone    technical <chr [1]>
#>  4 My password reset email never arrived technical <chr [1]>
#>  5 When will my order ship?              shipping  <chr [1]>
#>  6 Can I change my delivery address?     shipping  <chr [1]>
#>  7 The product arrived damaged           shipping  <chr [1]>
#>  8 I need a refund for my subscription   billing   <chr [1]>
#>  9 How do I contact customer service?    general   <chr [1]>
#> 10 What are your business hours?         general   <chr [1]>
```

Calculate accuracy:

``` r
correct <- sum(baseline_results$category == trainset$category)
total <- nrow(trainset)
baseline_accuracy <- correct / total

cat("Baseline accuracy:", scales::percent(baseline_accuracy), "\n")
#> Baseline accuracy: 100%
cat("Correct:", correct, "out of", total, "\n")
#> Correct: 10 out of 10
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
#> Warning: Failed to serialize output_type for cache key
#> ℹ Using class name as fallback: "ellmer::TypeObject"
#> ℹ This may cause cache collisions for different output types
#> ✖ Original error: cannot coerce type 'object' to vector of type 'character'
#> $category
#> [1] "billing"

run(classifier_with_demos, ticket = "The button doesn't respond when clicked", .llm = chat)
#> Warning: Failed to serialize output_type for cache key
#> ℹ Using class name as fallback: "ellmer::TypeObject"
#> ℹ This may cause cache collisions for different output types
#> ✖ Original error: cannot coerce type 'object' to vector of type 'character'
#> $category
#> [1] "technical"
```

## Step 5: Measure Improvement

Run on the same test set:

``` r
improved_results <- run_dataset(classifier_with_demos, trainset, .llm = chat)
#> Processing 5/10 |  50% | ETA:  1s
#> Processing 10/10 | 100% | ETA:  0s
#> 

correct_improved <- sum(improved_results$category == trainset$category)
improved_accuracy <- correct_improved / total

cat("Baseline accuracy:", scales::percent(baseline_accuracy), "\n")
#> Baseline accuracy: 100%
cat("Improved accuracy:", scales::percent(improved_accuracy), "\n")
#> Improved accuracy: 100%
cat("Improvement:", scales::percent(improved_accuracy - baseline_accuracy), "\n")
#> Improvement: 0%
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
#> NULL
```

## Step 8: Compare All Three Versions

Let’s see how each version performs:

``` r
# Run all three on the test set
results_baseline <- run_dataset(classifier, trainset, .llm = chat)
#> Processing 4/10 |  40% | ETA:  2s
#> Processing 10/10 | 100% | ETA:  0s
#> 
results_manual <- run_dataset(classifier_with_demos, trainset, .llm = chat)
#> Processing 4/10 |  40% | ETA:  2s
#> Processing 10/10 | 100% | ETA:  0s
#> 
results_compiled <- run_dataset(compiled, trainset, .llm = chat)
#> Processing 3/10 |  30% | ETA:  3s
#> Processing 9/10 |  90% | ETA:  0s
#> Processing 10/10 | 100% | ETA:  0s
#> 

# Calculate accuracies
acc_baseline <- mean(results_baseline$category == trainset$category)
acc_manual <- mean(results_manual$category == trainset$category)
acc_compiled <- mean(results_compiled$category == trainset$category)

comparison <- tibble(
  Version = c("Baseline (no demos)", "Manual demos (3)", "LabeledFewShot (3)"),
  Accuracy = scales::percent(c(acc_baseline, acc_manual, acc_compiled))
)

comparison
#> # A tibble: 3 × 2
#>   Version             Accuracy
#>   <chr>               <chr>   
#> 1 Baseline (no demos) 100%    
#> 2 Manual demos (3)    100%    
#> 3 LabeledFewShot (3)  100%
```

## Step 9: Using Built-in Metrics

dsprrr provides metrics for common evaluation tasks. Use
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md)
for classification:

``` r
metric <- metric_exact_match(field = "category")

# Evaluate the compiled module
eval_result <- evaluate(compiled, trainset, metric = metric, .llm = chat)
#> Processing 6/10 |  60% | ETA:  2s
#> Processing 10/10 | 100% | ETA:  0s
#> 

eval_result
#> $mean_score
#> [1] 1
#> 
#> $scores
#>  [1] 1 1 1 1 1 1 1 1 1 1
#> 
#> $predictions
#> $predictions[[1]]
#> $predictions[[1]]$category
#> [1] "billing"
#> 
#> 
#> $predictions[[2]]
#> $predictions[[2]]$category
#> [1] "billing"
#> 
#> 
#> $predictions[[3]]
#> $predictions[[3]]$category
#> [1] "technical"
#> 
#> 
#> $predictions[[4]]
#> $predictions[[4]]$category
#> [1] "technical"
#> 
#> 
#> $predictions[[5]]
#> $predictions[[5]]$category
#> [1] "shipping"
#> 
#> 
#> $predictions[[6]]
#> $predictions[[6]]$category
#> [1] "shipping"
#> 
#> 
#> $predictions[[7]]
#> $predictions[[7]]$category
#> [1] "shipping"
#> 
#> 
#> $predictions[[8]]
#> $predictions[[8]]$category
#> [1] "billing"
#> 
#> 
#> $predictions[[9]]
#> $predictions[[9]]$category
#> [1] "general"
#> 
#> 
#> $predictions[[10]]
#> $predictions[[10]]$category
#> [1] "general"
#> 
#> 
#> 
#> $n_evaluated
#> [1] 10
#> 
#> $n_errors
#> [1] 0
#> 
#> $errors
#> character(0)
#> 
#> $metadata
#> $metadata[[1]]
#> $metadata[[1]]$latency_ms
#> [1] 428.7372
#> 
#> $metadata[[1]]$prompt_length
#> [1] 272
#> 
#> $metadata[[1]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: I was charged twice for the same item"
#> 
#> $metadata[[1]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[1]]$timestamp
#> [1] "2026-01-10 03:16:03 UTC"
#> 
#> $metadata[[1]]$batch_index
#> [1] 1
#> 
#> 
#> $metadata[[2]]
#> $metadata[[2]]$latency_ms
#> [1] 441.3812
#> 
#> $metadata[[2]]$prompt_length
#> [1] 269
#> 
#> $metadata[[2]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: How do I update my payment method?"
#> 
#> $metadata[[2]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[2]]$timestamp
#> [1] "2026-01-10 03:16:03 UTC"
#> 
#> $metadata[[2]]$batch_index
#> [1] 2
#> 
#> 
#> $metadata[[3]]
#> $metadata[[3]]$latency_ms
#> [1] 442.9066
#> 
#> $metadata[[3]]$prompt_length
#> [1] 269
#> 
#> $metadata[[3]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: The website won't load on my phone"
#> 
#> $metadata[[3]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[3]]$timestamp
#> [1] "2026-01-10 03:16:04 UTC"
#> 
#> $metadata[[3]]$batch_index
#> [1] 3
#> 
#> 
#> $metadata[[4]]
#> $metadata[[4]]$latency_ms
#> [1] 453.8341
#> 
#> $metadata[[4]]$prompt_length
#> [1] 272
#> 
#> $metadata[[4]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: My password reset email never arrived"
#> 
#> $metadata[[4]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[4]]$timestamp
#> [1] "2026-01-10 03:16:04 UTC"
#> 
#> $metadata[[4]]$batch_index
#> [1] 4
#> 
#> 
#> $metadata[[5]]
#> $metadata[[5]]$latency_ms
#> [1] 451.5803
#> 
#> $metadata[[5]]$prompt_length
#> [1] 259
#> 
#> $metadata[[5]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: When will my order ship?"
#> 
#> $metadata[[5]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[5]]$timestamp
#> [1] "2026-01-10 03:16:05 UTC"
#> 
#> $metadata[[5]]$batch_index
#> [1] 5
#> 
#> 
#> $metadata[[6]]
#> $metadata[[6]]$latency_ms
#> [1] 464.1435
#> 
#> $metadata[[6]]$prompt_length
#> [1] 268
#> 
#> $metadata[[6]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: Can I change my delivery address?"
#> 
#> $metadata[[6]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[6]]$timestamp
#> [1] "2026-01-10 03:16:05 UTC"
#> 
#> $metadata[[6]]$batch_index
#> [1] 6
#> 
#> 
#> $metadata[[7]]
#> $metadata[[7]]$latency_ms
#> [1] 469.4114
#> 
#> $metadata[[7]]$prompt_length
#> [1] 262
#> 
#> $metadata[[7]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: The product arrived damaged"
#> 
#> $metadata[[7]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[7]]$timestamp
#> [1] "2026-01-10 03:16:06 UTC"
#> 
#> $metadata[[7]]$batch_index
#> [1] 7
#> 
#> 
#> $metadata[[8]]
#> $metadata[[8]]$latency_ms
#> [1] 608.8142
#> 
#> $metadata[[8]]$prompt_length
#> [1] 270
#> 
#> $metadata[[8]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: I need a refund for my subscription"
#> 
#> $metadata[[8]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[8]]$timestamp
#> [1] "2026-01-10 03:16:06 UTC"
#> 
#> $metadata[[8]]$batch_index
#> [1] 8
#> 
#> 
#> $metadata[[9]]
#> $metadata[[9]]$latency_ms
#> [1] 437.9733
#> 
#> $metadata[[9]]$prompt_length
#> [1] 269
#> 
#> $metadata[[9]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: How do I contact customer service?"
#> 
#> $metadata[[9]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[9]]$timestamp
#> [1] "2026-01-10 03:16:07 UTC"
#> 
#> $metadata[[9]]$batch_index
#> [1] 9
#> 
#> 
#> $metadata[[10]]
#> $metadata[[10]]$latency_ms
#> [1] 442.7891
#> 
#> $metadata[[10]]$prompt_length
#> [1] 264
#> 
#> $metadata[[10]]$prompt
#> [1] "Example 1:\nticket: The website won't load on my phone\nOutput: technical\n\nExample 2:\nticket: What are your business hours?\nOutput: general\n\nExample 3:\nticket: How do I update my payment method?\nOutput: billing\n\n\n# Input: ticket\nticket: What are your business hours?"
#> 
#> $metadata[[10]]$instructions
#> [1] "Classify the customer support ticket."
#> 
#> $metadata[[10]]$timestamp
#> [1] "2026-01-10 03:16:07 UTC"
#> 
#> $metadata[[10]]$batch_index
#> [1] 10
#> 
#> 
#> 
#> $data
#> # A tibble: 10 × 5
#>    ticket                              category result       .metadata    .chat 
#>    <chr>                               <chr>    <list>       <list>       <list>
#>  1 I was charged twice for the same i… billing  <named list> <named list> <Chat>
#>  2 How do I update my payment method?  billing  <named list> <named list> <Chat>
#>  3 The website won't load on my phone  technic… <named list> <named list> <Chat>
#>  4 My password reset email never arri… technic… <named list> <named list> <Chat>
#>  5 When will my order ship?            shipping <named list> <named list> <Chat>
#>  6 Can I change my delivery address?   shipping <named list> <named list> <Chat>
#>  7 The product arrived damaged         shipping <named list> <named list> <Chat>
#>  8 I need a refund for my subscription billing  <named list> <named list> <Chat>
#>  9 How do I contact customer service?  general  <named list> <named list> <Chat>
#> 10 What are your business hours?       general  <named list> <named list> <Chat>
#> 
#> attr(,"class")
#> [1] "dsprrr_evaluation"
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
#> Processing 3/10 |  30% | ETA:  3s
#> Processing 8/10 |  80% | ETA:  1s
#> Processing 10/10 | 100% | ETA:  0s
#> 
#> Processing 4/10 |  40% | ETA:  3s
#> Processing 9/10 |  90% | ETA:  1s
#> Processing 10/10 | 100% | ETA:  0s
#> 
#> Processing 5/10 |  50% | ETA:  3s
#> Processing 10/10 | 100% | ETA:  0s
#> 
#> Processing 4/10 |  40% | ETA:  4s
#> Processing 9/10 |  90% | ETA:  1s
#> Processing 10/10 | 100% | ETA:  0s
#> 
#> Processing 4/10 |  40% | ETA:  4s
#> Processing 8/10 |  80% | ETA:  1s
#> Processing 10/10 | 100% | ETA:  0s
#> 

do.call(rbind, results)
#> # A tibble: 5 × 2
#>       k accuracy
#> * <int>    <dbl>
#> 1     1        1
#> 2     2        1
#> 3     3        1
#> 4     4        1
#> 5     5        1
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
