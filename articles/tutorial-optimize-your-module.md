# Tutorial 5: Finding the Best Configuration

In [Tutorial
4](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md),
you improved your module with examples. But there are many other knobs
to tune: temperature, instructions, prompt templates. How do you find
the best combination?

The answer: **let dsprrr search for you**.

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
#> 
#> Attaching package: 'dsprrr'
#> The following object is masked from 'package:methods':
#> 
#>     signature
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

classifier <- module(sig, type = "predict")
```

Create training and test data:

``` r
# Training data for optimization
trainset <- dsp_trainset(
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
testset <- dsp_trainset(
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
classifier$optimize_grid(
  data = trainset,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(
    temperature = c(0.0, 0.3, 0.7, 1.0)
  ),
  .llm = chat
)
#> ℹ Using cached LLM responses
#> ℹ Disable with `configure_cache(enable = FALSE)` or `.cache = FALSE`
```

## Step 3: View Optimization Results

See what happened:

``` r
# All trials
module_trials(classifier)
#> # A tibble: 1 × 7
#>   n_trials best_trial best_score mean_score std_error best_params      trials  
#>      <int>      <int>      <dbl>      <dbl>     <dbl> <list>           <list>  
#> 1        4          1        0.9        0.9         0 <named list [1]> <tibble>
```

Get the summary:

``` r
# Metrics summary
module_metrics(classifier)
#> # A tibble: 4 × 10
#>   trial_id score mean_score median_score std_dev n_evaluated n_errors
#>      <int> <dbl>      <dbl>        <dbl>   <dbl>       <int>    <int>
#> 1        1   0.9        0.9            1   0.316          10        0
#> 2        2   0.9        0.9            1   0.316          10        0
#> 3        3   0.9        0.9            1   0.316          10        0
#> 4        4   0.9        0.9            1   0.316          10        0
#> # ℹ 3 more variables: params <list>, scores <list>, yardstick <list>
```

Check the best configuration:

``` r
# Best score achieved
classifier$state$best_score
#> [1] 0.9

# Best parameters
classifier$state$best_params
#> $temperature
#> [1] 0
#> 
#> attr(,"out.attrs")
#> attr(,"out.attrs")$dim
#> temperature 
#>           4 
#> 
#> attr(,"out.attrs")$dimnames
#> attr(,"out.attrs")$dimnames$temperature
#> [1] "temperature=0.0" "temperature=0.3" "temperature=0.7" "temperature=1.0"
```

## Step 4: The Module Remembers

After optimization, the module automatically uses the best
configuration:

``` r
# This uses the best temperature found
run(classifier, review = "This product changed my life!", .llm = chat)
#> $sentiment
#> [1] "positive"
```

## Step 5: Grid Search Over Instructions

Instructions matter a lot. Let’s test different phrasings:

``` r
# Reset to try different parameters
classifier2 <- module(sig, type = "predict")

classifier2$optimize_grid(
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
#> # A tibble: 1 × 7
#>   n_trials best_trial best_score mean_score std_error best_params      trials  
#>      <int>      <int>      <dbl>      <dbl>     <dbl> <list>           <list>  
#> 1        4          1        0.9        0.9         0 <named list [1]> <tibble>
```

The `instructions_suffix` is appended to your base instructions.

## Step 6: Multi-Parameter Grid Search

Search over multiple parameters at once:

``` r
classifier3 <- module(sig, type = "predict")

classifier3$optimize_grid(
  data = trainset,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(
    temperature = c(0.0, 0.5),
    instructions_suffix = c("", " Be decisive.")
  ),
  .llm = chat
)

module_trials(classifier3)
#> # A tibble: 1 × 7
#>   n_trials best_trial best_score mean_score std_error best_params      trials  
#>      <int>      <int>      <dbl>      <dbl>     <dbl> <list>           <list>  
#> 1        4          1        0.9        0.9         0 <named list [2]> <tibble>
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

optimized <- compile_module(
  program = module(sig, type = "predict"),
  teleprompter = teleprompter,
  trainset = trainset,
  .llm = chat
)
#> Optimizing 1/3 | Score: 0.5000
#> Optimizing 3/3 | Score: 0.5000
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
#> $mean_score
#> [1] 1
#> 
#> $scores
#> [1] 1 1 1 1
#> 
#> $predictions
#> $predictions[[1]]
#> $predictions[[1]]$sentiment
#> [1] "positive"
#> 
#> 
#> $predictions[[2]]
#> $predictions[[2]]$sentiment
#> [1] "negative"
#> 
#> 
#> $predictions[[3]]
#> $predictions[[3]]$sentiment
#> [1] "neutral"
#> 
#> 
#> $predictions[[4]]
#> $predictions[[4]]$sentiment
#> [1] "positive"
#> 
#> 
#> 
#> $n_evaluated
#> [1] 4
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
#> [1] 477.7474
#> 
#> $metadata[[1]]$prompt_length
#> [1] 172
#> 
#> $metadata[[1]]$prompt
#> [1] "Example 1:\nreview: Decent for the price.\nOutput: neutral\n\nExample 2:\nreview: Nothing special, but it works.\nOutput: neutral\n\n\n# Input: review\nreview: Great value for money!"
#> 
#> $metadata[[1]]$instructions
#> [1] "Classify the sentiment of this product review.  Respond with just the sentiment."
#> 
#> $metadata[[1]]$timestamp
#> [1] "2026-01-11 18:40:30 UTC"
#> 
#> $metadata[[1]]$batch_index
#> [1] 1
#> 
#> 
#> $metadata[[2]]
#> $metadata[[2]]$latency_ms
#> [1] 485.7812
#> 
#> $metadata[[2]]$prompt_length
#> [1] 179
#> 
#> $metadata[[2]]$prompt
#> [1] "Example 1:\nreview: Decent for the price.\nOutput: neutral\n\nExample 2:\nreview: Nothing special, but it works.\nOutput: neutral\n\n\n# Input: review\nreview: Stopped working after a week."
#> 
#> $metadata[[2]]$instructions
#> [1] "Classify the sentiment of this product review.  Respond with just the sentiment."
#> 
#> $metadata[[2]]$timestamp
#> [1] "2026-01-11 18:40:31 UTC"
#> 
#> $metadata[[2]]$batch_index
#> [1] 2
#> 
#> 
#> $metadata[[3]]
#> $metadata[[3]]$latency_ms
#> [1] 501.545
#> 
#> $metadata[[3]]$prompt_length
#> [1] 181
#> 
#> $metadata[[3]]$prompt
#> [1] "Example 1:\nreview: Decent for the price.\nOutput: neutral\n\nExample 2:\nreview: Nothing special, but it works.\nOutput: neutral\n\n\n# Input: review\nreview: Average product, average price."
#> 
#> $metadata[[3]]$instructions
#> [1] "Classify the sentiment of this product review.  Respond with just the sentiment."
#> 
#> $metadata[[3]]$timestamp
#> [1] "2026-01-11 18:40:31 UTC"
#> 
#> $metadata[[3]]$batch_index
#> [1] 3
#> 
#> 
#> $metadata[[4]]
#> $metadata[[4]]$latency_ms
#> [1] 500.0944
#> 
#> $metadata[[4]]$prompt_length
#> [1] 189
#> 
#> $metadata[[4]]$prompt
#> [1] "Example 1:\nreview: Decent for the price.\nOutput: neutral\n\nExample 2:\nreview: Nothing special, but it works.\nOutput: neutral\n\n\n# Input: review\nreview: Couldn't be happier with this purchase!"
#> 
#> $metadata[[4]]$instructions
#> [1] "Classify the sentiment of this product review.  Respond with just the sentiment."
#> 
#> $metadata[[4]]$timestamp
#> [1] "2026-01-11 18:40:32 UTC"
#> 
#> $metadata[[4]]$batch_index
#> [1] 4
#> 
#> 
#> 
#> $data
#> # A tibble: 4 × 5
#>   review                              sentiment result       .metadata    .chat 
#>   <chr>                               <chr>     <list>       <list>       <list>
#> 1 Great value for money!              positive  <named list> <named list> <Chat>
#> 2 Stopped working after a week.       negative  <named list> <named list> <Chat>
#> 3 Average product, average price.     neutral   <named list> <named list> <Chat>
#> 4 Couldn't be happier with this purc… positive  <named list> <named list> <Chat>
#> 
#> attr(,"class")
#> [1] "dsprrr_evaluation"
```

Compare to baseline:

``` r
baseline <- module(sig, type = "predict")

baseline_results <- evaluate(
  baseline,
  testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = chat
)
#> Processing 2/4 |  50% | ETA:  1s
#> Processing 4/4 | 100% | ETA:  0s
#> 

cat("Baseline test accuracy:", scales::percent(baseline_results$mean_score), "\n")
#> Baseline test accuracy: 100%
cat("Optimized test accuracy:", scales::percent(test_results$mean_score), "\n")
#> Optimized test accuracy: 100%
```

## Step 9: Different Metrics for Different Tasks

Not all tasks use exact match. dsprrr provides several metrics:

``` r
# For text generation - token overlap
metric_f1()
#> function (prediction, expected) 
#> {
#>     if (!is.null(field)) {
#>         prediction <- extract_field(prediction, field)
#>         expected <- extract_field(expected, field)
#>     }
#>     pred_str <- as.character(prediction)
#>     exp_str <- as.character(expected)
#>     if (normalize) {
#>         pred_str <- normalize_text(pred_str)
#>         exp_str <- normalize_text(exp_str)
#>     }
#>     pred_tokens <- unlist(strsplit(pred_str, "\\s+"))
#>     exp_tokens <- unlist(strsplit(exp_str, "\\s+"))
#>     common <- intersect(pred_tokens, exp_tokens)
#>     num_common <- length(common)
#>     if (length(pred_tokens) == 0 && length(exp_tokens) == 0) {
#>         return(1)
#>     }
#>     if (num_common == 0) {
#>         return(0)
#>     }
#>     precision <- num_common/length(pred_tokens)
#>     recall <- num_common/length(exp_tokens)
#>     f1 <- 2 * precision * recall/(precision + recall)
#>     f1
#> }
#> <bytecode: 0x561f53872d80>
#> <environment: 0x561f53871c98>

# Check if output contains a string
metric_contains("error", ignore_case = TRUE)
#> function (prediction, expected = NULL) 
#> {
#>     if (!is.null(field)) {
#>         prediction <- extract_field(prediction, field)
#>     }
#>     pred_str <- as.character(prediction)
#>     if (fixed && ignore_case) {
#>         grepl(tolower(pattern), tolower(pred_str), fixed = TRUE)
#>     }
#>     else if (fixed) {
#>         grepl(pattern, pred_str, fixed = TRUE)
#>     }
#>     else {
#>         grepl(pattern, pred_str, ignore.case = ignore_case, fixed = FALSE)
#>     }
#> }
#> <bytecode: 0x561f57fb7c08>
#> <environment: 0x561f5802fd40>

# Custom logic
metric_custom(function(prediction, expected) {
  # Return TRUE/FALSE or 0-1 score
  nchar(prediction) < 100
}, name = "concise")
#> function (prediction, expected) 
#> {
#>     tryCatch({
#>         result <- fn(prediction, expected)
#>         if (!is.logical(result) && !is.numeric(result)) {
#>             cli::cli_abort(c("Metric {.fn {metric_name}} must return logical or numeric value", 
#>                 x = "Got {.cls {class(result)}}"))
#>         }
#>         if (is.numeric(result)) {
#>             if (result < 0 || result > 1) {
#>                 cli::cli_warn(c("Metric {.fn {metric_name}} returned value outside [0, 1]", 
#>                   i = "Value: {result}"))
#>                 result <- max(0, min(1, result))
#>             }
#>         }
#>         result
#>     }, error = function(e) {
#>         cli::cli_abort(c(paste0("Error in metric ", metric_name), 
#>             x = e$message), parent = e)
#>     })
#> }
#> <bytecode: 0x561f563a4f80>
#> <environment: 0x561f51de0048>

# Threshold wrapper
metric_threshold(metric_f1(), threshold = 0.8)
#> function (prediction, expected) 
#> {
#>     score <- metric(prediction, expected)
#>     if (!is.numeric(score)) {
#>         cli::cli_abort("Base metric must return numeric value for threshold comparison")
#>     }
#>     result <- switch(comparison, `>=` = score >= threshold, `>` = score > 
#>         threshold, `==` = score == threshold, `<` = score < threshold, 
#>         `<=` = score <= threshold)
#>     result
#> }
#> <bytecode: 0x561f564d8d40>
#> <environment: 0x561f564d9b78>
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

| Approach                                                                            | Best For                           |
|-------------------------------------------------------------------------------------|------------------------------------|
| [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md) | Quick parameter sweeps             |
| `LabeledFewShot`                                                                    | Adding examples from data          |
| `GridSearchTeleprompter`                                                            | Instruction + example optimization |
| Manual tuning                                                                       | Initial exploration                |

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
