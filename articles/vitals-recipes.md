# Vitals Integration Recipes

This vignette provides practical recipes for integrating dsprrr modules
with vitals evaluation. Each recipe is self-contained and demonstrates a
specific pattern.

``` r
library(dsprrr)
#> 
#> Attaching package: 'dsprrr'
#> The following object is masked from 'package:methods':
#> 
#>     signature
library(vitals)
library(ellmer)
library(tibble)
```

## Recipe 1: Quick Start

The fastest way to evaluate a dsprrr module with vitals:

``` r
# 1. Create a simple module
classifier <- signature("input -> label: enum('positive', 'negative')") |>
  module()

# 2. Prepare test data (vitals expects 'input' and 'target' columns)
test_data <- tibble(
  input = c(
    "I love this product!",
    "Terrible experience, waste of money"
  ),
  target = c("positive", "negative")
)

# 3. Create a vitals Task with dsprrr's helper
task <- as_vitals_task(
  module = classifier,
  dataset = test_data,
  scorer = detect_match(),
  name = "sentiment",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-4o-mini")
)

# 4. Run evaluation
task$eval()
#> ℹ Solving
#> ✔ Solving [883ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [74ms]
#> 

# 5. View results
task$get_samples()
#> # A tibble: 2 × 9
#>   input              target    id result       solver_chat score scorer_metadata
#>   <chr>              <chr>  <int> <list>       <list>      <ord> <list>         
#> 1 I love this produ… posit…     1 <named list> <Chat>      C     <named list>   
#> 2 Terrible experien… negat…     2 <named list> <Chat>      C     <named list>   
#> # ℹ 2 more variables: scorer_explanation <chr>, scorer <chr>
```

That’s it! The
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md)
helper handles the integration details.

## Recipe 2: Train-Test Split Workflow

A common pattern: optimize on training data, evaluate on held-out test
data.

``` r
# Full dataset with ground truth
qa_data <- tibble(
  question = c(
    "What is the capital of France?",
    "Who wrote Romeo and Juliet?",
    "What is 2 + 2?",
    "What color is the sky?",
    "Who painted the Mona Lisa?",
    "What is the largest planet?"
  ),
  target = c(
    "Paris",
    "Shakespeare",
    "4",
    "Blue",
    "Leonardo da Vinci",
    "Jupiter"
  )
)

# Split into train/test
set.seed(42)
train_idx <- sample(nrow(qa_data), 4)
trainset <- qa_data[train_idx, ]
testset <- qa_data[-train_idx, ]

# Create and optimize module
qa_module <- signature("question -> answer") |>
  module()

# Optimize with few-shot examples from training set
optimized <- compile(
  LabeledFewShot(k = 2L),
  qa_module,
  trainset = trainset
)

# Evaluate on held-out test set
eval_task <- as_vitals_task(
  module = optimized,
  dataset = testset,
  scorer = detect_match(),
  name = "qa-test",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-4o-mini")
)

eval_task$eval()
#> ℹ Solving
#> ✔ Solving [355ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [58ms]
#> 

# Compare performance
cat("Test accuracy:", mean(eval_task$get_samples()$score == "C"), "\n")
#> Test accuracy: 1
```

## Recipe 3: Using Different Vitals Scorers

Vitals provides several scorers for different use cases:

### Exact Match (detect_match)

Best for factual questions with precise answers:

``` r
factual <- signature("question -> answer") |> module()

task <- as_vitals_task(
  module = factual,
  dataset = tibble(
    question = c("What is 5 * 7?", "What year did WWII end?"),
    target = c("35", "1945")
  ),
  scorer = detect_match(),
  name = "factual-qa",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-4o-mini")
)

task$eval()
#> ℹ Solving
#> ✔ Solving [346ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [55ms]
#> 
task$get_samples()
#> # A tibble: 2 × 10
#>   question     target input    id result       solver_chat score scorer_metadata
#>   <chr>        <chr>  <chr> <int> <list>       <list>      <ord> <list>         
#> 1 What is 5 *… 35     What…     1 <named list> <Chat>      C     <named list>   
#> 2 What year d… 1945   What…     2 <named list> <Chat>      C     <named list>   
#> # ℹ 2 more variables: scorer_explanation <chr>, scorer <chr>
```

### Model-Graded QA (model_graded_qa)

Best for open-ended questions where multiple phrasings are correct:

``` r
explainer <- signature("topic -> explanation") |>
  module(type = "chain_of_thought")

task <- as_vitals_task(
  module = explainer,
  dataset = tibble(
    topic = c("Why is the sky blue?", "How do plants make food?"),
    target = c(
      "Light scattering in atmosphere",
      "Photosynthesis using sunlight"
    )
  ),
  scorer = model_graded_qa(),
  name = "explanations",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-4o-mini")
)

task$eval()
#> ℹ Solving
#> ✔ Solving [357ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [333ms]
#> 
task$get_samples()
#> # A tibble: 2 × 10
#>   topic            target input    id result       solver_chat score scorer_chat
#>   <chr>            <chr>  <chr> <int> <list>       <list>      <ord> <list>     
#> 1 Why is the sky … Light… Why …     1 <named list> <Chat>      C     <Chat>     
#> 2 How do plants m… Photo… How …     2 <named list> <Chat>      C     <Chat>     
#> # ℹ 2 more variables: scorer_metadata <list>, scorer <chr>
```

### Substring Match (detect_includes)

Best for checking if specific text appears anywhere in output:

``` r
coder <- signature("task -> code") |> module()

task <- as_vitals_task(
  module = coder,
  dataset = tibble(
    task = c(
      "Write a function to add two numbers",
      "Create a loop that prints 1 to 5"
    ),
    target = c("function", "for")
  ),
  scorer = detect_includes(),
  name = "code-gen",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-4o-mini")
)

task$eval()
#> ℹ Solving
#> ✔ Solving [342ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [56ms]
#> 
task$get_samples()
#> # A tibble: 2 × 10
#>   task         target input    id result       solver_chat score scorer_metadata
#>   <chr>        <chr>  <chr> <int> <list>       <list>      <ord> <list>         
#> 1 Write a fun… funct… Writ…     1 <named list> <Chat>      I     <named list>   
#> 2 Create a lo… for    Crea…     2 <named list> <Chat>      C     <named list>   
#> # ℹ 2 more variables: scorer_explanation <chr>, scorer <chr>
```

## Recipe 4: Analyzing Evaluation Results

After running evaluation, dig into the results:

``` r
# Run an evaluation
sentiment <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')"
) |>
  module()

dataset <- tibble(
  text = c(
    "Best purchase ever!",
    "Complete garbage",
    "It's okay I guess",
    "Amazing quality!",
    "Never buying again"
  ),
  target = c("positive", "negative", "neutral", "positive", "negative")
)

task <- as_vitals_task(
  module = sentiment,
  dataset = dataset,
  scorer = detect_match(),
  name = "sentiment-analysis",
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-4o-mini")
)

task$eval()
#> ℹ Solving
#> ✔ Solving [876ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [129ms]
#> 

# Get detailed scores
scores <- task$get_samples()

# Find failures
failures <- scores[scores$score == "I", ]
cat("Failed on", nrow(failures), "of", nrow(scores), "examples\n")
#> Failed on 0 of 5 examples

# Examine what went wrong
if (nrow(failures) > 0) {
  cat("\nFailure analysis:\n")
  for (i in seq_len(nrow(failures))) {
    cat("Input:", failures$input[i], "\n")
    cat("Expected:", failures$target[i], "\n")
    cat("Got:", failures$answer[i], "\n\n")
  }
}

# Calculate accuracy
accuracy <- mean(scores$score == "C")
cat("Overall accuracy:", scales::percent(accuracy), "\n")
#> Overall accuracy: 100%
```

## Recipe 5: Comparing Module Variants

Test different module configurations side-by-side:

``` r
# Same signature, different approaches
sig <- signature("question -> answer")

# Variant 1: Basic prediction
basic <- module(sig)

# Variant 2: Chain-of-thought reasoning
cot <- module(sig, type = "chain_of_thought")

# Test dataset
test_data <- tibble(
  question = c(
    "If a train travels 60 mph for 2 hours, how far does it go?",
    "What is 15% of 80?"
  ),
  target = c("120 miles", "12")
)

llm <- chat_openai(model = "gpt-4o-mini")

# Evaluate both
results <- list()
for (name in c("basic", "cot")) {
  mod <- if (name == "basic") basic else cot

  task <- as_vitals_task(
    module = mod,
    dataset = test_data,
    scorer = model_graded_qa(),
    name = paste0("math-", name),
    dir = tempdir(),
    .llm = llm
  )
  task$eval()

  results[[name]] <- mean(task$get_samples()$score == "C")
}
#> ℹ Solving
#> ✔ Solving [358ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [216ms]
#> 
#> ℹ Solving
#> ✔ Solving [370ms]
#> 
#> ℹ Scoring
#> ✔ Scoring [225ms]
#> 

# Compare
cat("Basic accuracy:", scales::percent(results$basic), "\n")
#> Basic accuracy: 100%
cat("CoT accuracy:", scales::percent(results$cot), "\n")
#> CoT accuracy: 100%
```

## Recipe 6: Multiple Epochs for Confidence

Run multiple evaluation passes for more reliable metrics:

``` r
classifier <- signature(
  "text -> category: enum('tech', 'sports', 'politics')"
) |>
  module()

# Small dataset - need multiple epochs for confidence
test_data <- tibble(
  text = c(
    "New iPhone released today",
    "Lakers win championship",
    "Senate passes new bill"
  ),
  target = c("tech", "sports", "politics")
)

# Run 3 epochs (each example evaluated 3 times)
task <- as_vitals_task(
  module = classifier,
  dataset = test_data,
  scorer = detect_match(),
  name = "news-classification",
  epochs = 3L,
  dir = tempdir(),
  .llm = chat_openai(model = "gpt-4o-mini")
)

task$eval()
#> ℹ Solving
#> ✔ Solving [1.7s]
#> 
#> ℹ Scoring
#> ✔ Scoring [262ms]
#> 

# Aggregate scores across epochs
scores <- task$get_samples()
cat(
  "Total evaluations:",
  nrow(scores),
  "(3 epochs x",
  nrow(test_data),
  "examples)\n"
)
#> Total evaluations: 9 (3 epochs x 3 examples)
cat("Overall accuracy:", scales::percent(mean(scores$score == "C")), "\n")
#> Overall accuracy: 100%
```

## Recipe 7: Custom Evaluation Pipeline

For complex evaluation logic, build a custom pipeline:

``` r
# Custom evaluation function
evaluate_module <- function(module, dataset, llm, scorer = detect_match()) {
  # Run predictions
  predictions <- run(
    module,
    !!!as.list(dataset[, -which(names(dataset) == "target")]),
    .llm = llm
  )

  # Combine with targets
  results <- tibble(
    input = dataset$input,
    target = dataset$target,
    prediction = predictions[[1]] # First output column
  )

  # Score
  results$correct <- results$prediction == results$target

  # Summary
  list(
    accuracy = mean(results$correct),
    n = nrow(results),
    failures = results[!results$correct, ],
    all = results
  )
}

# Use it
results <- evaluate_module(
  module = sentiment_module,
  dataset = test_data,
  llm = chat_openai()
)

cat("Accuracy:", scales::percent(results$accuracy), "\n")
cat("Failures:", nrow(results$failures), "\n")
```

## Summary

These recipes cover the most common integration patterns:

| Recipe             | Use When                           |
|--------------------|------------------------------------|
| Quick Start        | Getting started, simple evaluation |
| Train-Test Split   | Validating optimization results    |
| Different Scorers  | Matching scorer to task type       |
| Analyzing Results  | Understanding failures             |
| Comparing Variants | A/B testing modules                |
| Multiple Epochs    | Small datasets, need confidence    |
| Custom Pipeline    | Complex evaluation logic           |

For more details on the underlying APIs, see
[`vignette("vitals-integration")`](https://jameshwade.github.io/dsprrr/articles/vitals-integration.md).
