# Automatic Prompt Optimization with dsprrr

## Introduction

One of dsprrr’s most powerful features is **automatic prompt
optimization**. Instead of manually tweaking prompts and examples, you
can let dsprrr systematically optimize your LLM programs using your
training data and evaluation metrics.

This vignette covers:

- **Metrics**: How to evaluate LLM outputs
- **Teleprompters**: Optimization strategies for improving programs
- **Compilation**: The process of optimizing your modules
- **Evaluation**: Systematic testing of optimized programs

## Setup

``` r

library(dsprrr)
library(ellmer)

# Configure OpenAI (gpt-5-mini for cost-effective cassette recording)
llm <- chat_openai(model = "gpt-5-mini")
```

## Quick tour: optimizing a module with `optimize_grid()`

The new
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
helper lets you tune a module directly without going through a
teleprompter first. It evaluates a grid of configuration values (defined
either as a simple named list or via tidymodels `dials` parameters) and
records the results in `module$state$trials`.

``` r

sig_quick <- signature("review -> sentiment: string",
  instructions = "Respond with the sentiment label only."
)

sentiment_module <- module(
  signature = sig_quick,
  type = "predict",
  template = "Review: {review}\nSentiment:"
)

dev_reviews <- tibble::tibble(
  review = "I absolutely loved this!",
  target = "positive"
)

exact_sentiment <- function(prediction, expected_row) {
  as.numeric(tolower(prediction) == tolower(expected_row$target))
}

optimize_grid(
  sentiment_module,
  data = dev_reviews,
  metric = exact_sentiment,
  parameters = list(prompt_style = c("baseline", "energetic")),
  .llm = llm,
  control = list(progress = FALSE)
)

sentiment_module$state$trials[, c("trial_id", "score", "n_evaluated")]
sentiment_module$state$best_score
```

If you do have tidymodels installed, you can also describe the search
space with `dials`, build a grid (regular or random), and pass it to
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md).
The helper automatically captures the candidate grid, scores, and
metadata so teleprompters and downstream reports can reuse the trial
data.

## Tidymodels Integration Helpers

dsprrr provides three key helpers for integrating with the tidymodels
ecosystem:

### `module_parameters()`: Derive dials parameters

After running
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md),
you can extract a
[`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
set from the module. This is useful for further tuning or for generating
additional grid configurations:

``` r

library(dials)

# After optimization, extract discovered parameters
param_set <- module_parameters(sentiment_module)
print(param_set)

# Use the parameter set to generate a more refined grid
refined_grid <- grid_regular(param_set, levels = 5)

# Or generate a random grid for broader exploration
random_grid <- grid_random(param_set, size = 20)
```

The function automatically discovers parameters from: - Module
configuration (e.g., `temperature`, `top_p`) - Previous optimization
trials - Signature enum types (prefixed with `input_`) - Known LLM
parameter defaults

You can filter which parameters are included:

``` r

# Only include specific parameters
param_set <- module_parameters(
  sentiment_module,
  include = c("temperature", "prompt_style")
)

# Exclude certain parameters
param_set <- module_parameters(
  sentiment_module,
  exclude = c("id", "instructions", "max_output_tokens")
)
```

### `module_trials()`: Optimization overview

Get a tidy summary of all optimization trials:

``` r

summary <- module_trials(sentiment_module)

# Returns a tibble with:
# - n_trials: number of trials evaluated
# - best_trial: identifier of the best-performing trial
# - best_score: best score achieved
# - mean_score: mean across all scores
# - std_error: standard error of the scores
# - best_params: list-column containing the best parameter set
# - trials: list-column containing the full trials tibble

print(summary$n_trials)
print(summary$best_score)
print(summary$best_params[[1]])  # Best parameter combination

# Access the full trials data for detailed analysis
trials_df <- summary$trials[[1]]
```

### `module_metrics()`: Per-trial metrics with yardstick

For more detailed per-trial analysis, including integration with
yardstick metrics:

``` r

# Basic summary without yardstick
metrics <- module_metrics(sentiment_module)

# Returns a tibble with one row per trial:
# - trial_id, score: trial identifier and overall score
# - mean_score, median_score, std_dev: summary statistics
# - n_evaluated, n_errors: counts from evaluation
# - params: list-column with parameters used
# - scores: list-column with raw per-example scores
# - yardstick: list-column for yardstick results (if computed)

# With yardstick metrics for classification tasks
library(yardstick)

yardstick_metrics <- module_metrics(

  sentiment_module,
  metrics = metric_set(accuracy, precision, recall),
  truth = "target",
  estimate = "result"
)

# Access yardstick results for each trial
yardstick_metrics$yardstick[[1]]  # Metrics for trial 1
```

### Complete tidymodels workflow example

Here’s a comprehensive example showing how these helpers work together:

``` r

library(dials)
library(yardstick)
library(ggplot2)

# 1. Create and optimize module
sig <- signature("text -> label: enum('spam', 'ham')")
mod <- module(sig, type = "predict")

# Initial optimization with coarse grid
optimize_grid(
  mod,
  data = training_data,
  metric = metric_exact_match(),
  parameters = list(
    temperature = c(0.1, 0.5, 1.0),
    prompt_style = c("concise", "detailed")
  )
)

# 2. Review optimization results
summary <- module_trials(mod)
cat("Best score:", summary$best_score, "\n")
cat("Best params:", paste(names(summary$best_params[[1]]),
    summary$best_params[[1]], sep = "=", collapse = ", "), "\n")

# 3. Compute detailed metrics
detailed <- module_metrics(
  mod,
  metrics = metric_set(accuracy, f_meas),
  truth = "label",
  estimate = "result"
)

# 4. Visualize trial performance
ggplot(detailed, aes(x = trial_id, y = score)) +
 geom_col() +
  labs(title = "Score by Trial", x = "Trial", y = "Score")

# 5. Extract parameters for further tuning
params <- module_parameters(mod, include = c("temperature"))

# Generate a finer grid around the best temperature
fine_grid <- grid_regular(params, levels = 10)

# Continue optimization with refined grid
optimize_grid(
  mod,
  data = training_data,
  metric = metric_exact_match(),
  grid = fine_grid
)
```

## Part 1: Understanding Metrics

Metrics are functions that evaluate how well your LLM’s output matches
expected results. dsprrr provides several built-in metrics and allows
you to create custom ones.

### Built-in Metrics

#### Exact Match

The simplest metric checks if outputs match exactly:

``` r

# Basic exact match
metric <- metric_exact_match()

# Test the metric
metric("positive", "positive") # TRUE
metric("positive", "negative") # FALSE

# Case-insensitive matching
metric_ignore_case <- metric_exact_match(ignore_case = TRUE)
metric_ignore_case("Positive", "positive") # TRUE

# Extract specific field from structured output
metric_field <- metric_exact_match(field = "sentiment")
metric_field(
  list(sentiment = "positive", confidence = 0.9),
  list(sentiment = "positive", confidence = 0.8)
) # TRUE - only compares sentiment field
```

#### F1 Score

For text generation tasks, F1 score measures token overlap:

``` r

# Token-based F1 score
metric_f1_basic <- metric_f1()

# Calculate F1 between predictions and expected text
score <- metric_f1_basic(
  "The capital of France is Paris",
  "Paris is the capital city of France"
)
print(score) # ~0.67 (good overlap but not perfect)

# With field extraction
metric_f1_field <- metric_f1(field = "answer")
score <- metric_f1_field(
  list(answer = "The Eiffel Tower", confidence = "high"),
  list(answer = "Eiffel Tower in Paris", other = "data")
)
```

#### Contains Metric

Check if output contains specific patterns:

``` r

# Check for substring
metric_has_positive <- metric_contains("positive", ignore_case = TRUE)
metric_has_positive("The result is POSITIVE", NULL) # TRUE
metric_has_positive("The result is negative", NULL) # FALSE

# Regular expression patterns
metric_has_number <- metric_contains("\\d+", fixed = FALSE)
metric_has_number("The answer is 42", NULL) # TRUE
metric_has_number("No numbers here", NULL) # FALSE

# Check specific field
metric_field_contains <- metric_contains("urgent", field = "priority")
metric_field_contains(
  list(priority = "urgent - needs immediate attention"),
  NULL
) # TRUE
```

### Custom Metrics

Create your own evaluation logic:

``` r

# Simple boolean metric
length_check <- metric_custom(
  function(pred, exp) {
    nchar(as.character(pred)) <= 100
  },
  name = "length_limit"
)

# Numeric scoring metric
similarity_metric <- metric_custom(
  function(pred, exp) {
    # Calculate some similarity score (0-1)
    pred_lower <- tolower(as.character(pred))
    exp_lower <- tolower(as.character(exp))

    # Simple character overlap ratio
    common <- nchar(pred_lower[pred_lower == exp_lower])
    total <- max(nchar(pred_lower), nchar(exp_lower))
    if (total == 0) {
      return(1.0)
    }

    common / total
  },
  name = "char_similarity"
)

# Use the custom metric
score <- similarity_metric("Hello World", "hello world")
```

### Threshold Metrics

Convert numeric metrics to boolean pass/fail:

``` r

# F1 score must be at least 0.8 to pass
f1_threshold <- metric_threshold(
  metric_f1(),
  threshold = 0.8,
  comparison = ">="
)

# Returns TRUE/FALSE instead of numeric score
passes <- f1_threshold(
  "The quick brown fox jumps",
  "The quick brown fox jumps over"
) # FALSE (F1 < 0.8)
```

### Multi-Field Metrics

Evaluate multiple fields simultaneously:

``` r

# Check if all specified fields match
metric_all <- metric_field_match(
  c("sentiment", "confidence"),
  require_all = TRUE
)

pred <- list(sentiment = "positive", confidence = "high", extra = "ignored")
exp <- list(sentiment = "positive", confidence = "high", other = "data")
metric_all(pred, exp) # TRUE - both fields match

# Check if any field matches
metric_any <- metric_field_match(
  c("answer", "alternate_answer"),
  require_all = FALSE
)
```

## Part 2: Teleprompters - Optimization Strategies

Teleprompters are optimization strategies that improve your LLM
programs. dsprrr currently provides two main teleprompters:

### LabeledFewShot

The simplest optimization strategy adds labeled examples from your
training data as demonstrations:

``` r

# Create a sentiment classifier
classifier <- signature(
  "text -> sentiment: enum('positive', 'negative', 'neutral')"
) |>
  module(type = "predict", template = "Analyze sentiment: {text}\n\nSentiment:")

# Prepare training data
trainset <- tibble::tibble(
  text = c(
    "This product is amazing! Best purchase ever!",
    "Terrible experience. Would not recommend.",
    "It's okay, nothing special but works."
  ),
  sentiment = c(
    "positive",
    "negative",
    "neutral"
  )
)

# Create LabeledFewShot teleprompter
teleprompter <- LabeledFewShot(
  k = 2L, # Use 2 examples as demonstrations
  sample = TRUE, # Randomly sample from trainset
  seed = 42L # For reproducibility
)

# Compile the module
optimized_classifier <- compile_module(
  program = classifier,
  teleprompter = teleprompter,
  trainset = trainset
)

# The optimized module now includes demonstrations
print(optimized_classifier$demos) # Shows the 2 selected examples

# Use the optimized classifier
result <- optimized_classifier |>
  run(text = "This is fantastic!", .llm = llm)

print(result) # "positive"
```

### GridSearchTeleprompter

This teleprompter tests multiple instruction or template variants to
find the best performing one:

``` r

# Create base QA module
qa_module <- signature("context, question -> answer") |>
  module(type = "predict")

# Define instruction variants to test
variants <- data.frame(
  id = c("concise", "detailed", "analytical", "step_by_step"),
  instructions_suffix = c(
    ". Answer concisely.",
    ". Provide a detailed answer with context.",
    ". Analyze the context carefully before answering.",
    ". Think step-by-step: 1) Understand context 2) Identify relevant info 3) Answer."
  ),
  stringsAsFactors = FALSE
)

# Prepare QA training data
qa_trainset <- tibble::tibble(
  context = c(
    "The R language was created by Ross Ihaka and Robert Gentleman in 1992.",
    "Python was created by Guido van Rossum and first released in 1991.",
    "JavaScript was created by Brendan Eich in just 10 days in 1995."
  ),
  question = c(
    "Who created R?",
    "When was Python released?",
    "How long did it take to create JavaScript?"
  ),
  answer = c(
    "Ross Ihaka and Robert Gentleman",
    "1991",
    "10 days"
  )
)

# Create GridSearchTeleprompter with evaluation metric
grid_teleprompter <- GridSearchTeleprompter(
  variants = variants,
  metric = metric_f1(field = "answer"), # Evaluate using F1 score
  k = 2L, # Include 2 few-shot examples
  eval_sample_size = 10L, # Smaller sample for faster execution
  verbose = TRUE # Show progress
)

# Compile with grid search
optimized_qa <- compile_module(
  program = qa_module,
  teleprompter = grid_teleprompter,
  trainset = qa_trainset,
  .llm = llm
)

# Check which variant performed best
print(optimized_qa$config$best_variant) # e.g., "analytical"
print(optimized_qa$config$best_score) # e.g., 0.85
print(optimized_qa$config$all_scores) # Scores for all variants

# Use the optimized module
answer <- optimized_qa |>
  run(
    context = "Ruby was created by Yukihiro Matsumoto in 1995.",
    question = "Who created Ruby?",
    .llm = llm
  )
```

## Part 3: The Compilation Process

The
[`compile_module()`](https://jameshwade.github.io/dsprrr/reference/compile_module.md)
function is the main interface for optimizing your programs:

``` r

# Step 1: Create your base module
email_classifier <- signature(
  inputs = list(
    input("email", description = "Customer email text")
  ),
  output_type = type_object(
    category = type_enum(
      values = c("complaint", "inquiry", "feedback", "spam")
    ),
    priority = type_enum(values = c("urgent", "normal", "low")),
    needs_response = type_boolean()
  ),
  instructions = "Classify customer emails by category and priority."
) |>
  module(type = "predict")

# Step 2: Prepare comprehensive training data
email_trainset <- tibble::tibble(
  email = c(
    "Your product broke after 2 days! I want a refund immediately!",
    "Hi, I'm wondering if you ship to Canada?",
    "Just wanted to say your customer service is excellent.",
    "CLICK HERE FOR FREE PRIZES!!! Limited time offer!!!"
  ),
  category = c(
    "complaint",
    "inquiry",
    "feedback",
    "spam"
  ),
  priority = c(
    "urgent",
    "normal",
    "low",
    "low"
  ),
  needs_response = c(
    TRUE,
    TRUE,
    FALSE,
    FALSE
  )
)

# Step 3: Choose optimization strategy
# Option A: Simple few-shot learning
simple_optimizer <- LabeledFewShot(k = 3L)

optimized_simple <- compile_module(
  program = email_classifier,
  teleprompter = simple_optimizer,
  trainset = email_trainset
)

# Option B: Grid search with custom variants
email_variants <- data.frame(
  id = c("professional", "friendly", "analytical"),
  instructions_suffix = c(
    ". Use professional business judgment.",
    ". Consider customer satisfaction and relationship.",
    ". Analyze linguistic patterns and intent markers."
  ),
  stringsAsFactors = FALSE
)

grid_optimizer <- GridSearchTeleprompter(
  variants = email_variants,
  metric = metric_field_match(c("category", "priority")),
  k = 2L,
  verbose = TRUE
)

optimized_grid <- compile_module(
  program = email_classifier,
  teleprompter = grid_optimizer,
  trainset = email_trainset,
  .llm = llm
)

# Step 4: Use the optimized module
new_email <- "I've been waiting for 2 hours on hold! This is unacceptable!"
result <- optimized_grid |>
  run(email = new_email, .llm = llm)

print(result$category) # "complaint"
print(result$priority) # "urgent"
print(result$needs_response) # TRUE
```

## Part 4: Evaluation Framework

Once you’ve optimized your module, systematically evaluate its
performance:

``` r

# Prepare test dataset (separate from training)
test_emails <- tibble::tibble(
  email = c(
    "The package was damaged during shipping.",
    "Do you offer student discounts?",
    "Your team went above and beyond. Thank you!"
  ),
  category = c(
    "complaint",
    "inquiry",
    "feedback"
  ),
  priority = c(
    "normal",
    "low",
    "low"
  ),
  needs_response = c(
    TRUE,
    TRUE,
    FALSE
  )
)

# Evaluate with specific metric
category_metric <- metric_exact_match(field = "category")

results <- evaluate(
  module = optimized_grid,
  data = test_emails,
  metric = category_metric,
  .llm = llm,
  .progress = TRUE
)

# Examine results
print(results$mean_score) # e.g., 0.67 (2/3 correct)
print(results$scores) # Individual scores per example
print(results$n_evaluated) # 3
print(results$n_errors) # 0 (no runtime errors)

# Evaluate multiple aspects
multi_metric <- metric_field_match(
  c("category", "priority", "needs_response"),
  require_all = TRUE # All fields must match
)

full_results <- evaluate(
  module = optimized_grid,
  data = test_emails,
  metric = multi_metric,
  .llm = llm,
  .progress = TRUE
)

print(full_results$mean_score) # Lower, as all fields must match
```

## Part 5: Complete Workflow Example

Let’s build and optimize a complete document analysis system:

``` r

# 1. Define the task with structured output
doc_analyzer <- signature(
  inputs = list(
    input("document", description = "Document text to analyze")
  ),
  output_type = type_object(
    summary = type_string(
      description = "Concise summary of main points"
    ),
    topics = type_array(
      type_string(),
      description = "Key topics discussed (max 5)"
    ),
    sentiment = type_enum(
      values = c("positive", "negative", "neutral", "mixed"),
      description = "Overall document sentiment"
    ),
    key_facts = type_array(
      type_string(),
      description = "Important facts or claims"
    ),
    recommendation = type_enum(
      values = c("approve", "review", "reject"),
      description = "Recommended action"
    )
  ),
  instructions = "Analyze documents for key information and provide recommendations."
) |>
  module(type = "predict")

# 2. Create comprehensive training data
doc_trainset <- tibble::tibble(
  document = c(
    "Our Q3 revenue increased by 25% YoY, driven by strong product sales.
     Customer satisfaction scores reached an all-time high of 92%.
     We're expanding into three new markets next quarter.",

    "Project delays have caused budget overruns of 40%. Team morale is low.
     Three key engineers resigned last month. Urgent intervention needed.",

    "The new policy proposal includes both benefits and drawbacks.
     Cost savings of $2M are projected, but implementation challenges exist.
     Stakeholder feedback has been mixed, requiring further consultation.",

    "Annual environmental impact assessment shows 30% reduction in emissions.
     Water usage decreased by 15%. All regulatory requirements met.
     Received sustainability award from industry association."
  ),
  summary = c(
    "Q3 showed strong 25% revenue growth with record customer satisfaction at 92%, prompting expansion into three new markets.",
    "Project facing severe issues with 40% budget overrun, low team morale, and loss of three key engineers.",
    "Policy proposal offers $2M savings but faces implementation challenges and mixed stakeholder reception.",
    "Environmental performance improved with 30% emission reduction and 15% less water usage, earning industry recognition."
  ),
  topics = list(
    c("revenue growth", "customer satisfaction", "market expansion"),
    c("project delays", "budget overrun", "staff turnover", "morale"),
    c(
      "policy proposal",
      "cost savings",
      "implementation",
      "stakeholder feedback"
    ),
    c(
      "environmental impact",
      "emissions reduction",
      "sustainability",
      "compliance"
    )
  ),
  sentiment = c("positive", "negative", "mixed", "positive"),
  key_facts = list(
    c("25% YoY revenue increase", "92% customer satisfaction", "3 new markets"),
    c("40% budget overrun", "3 engineers resigned", "low morale"),
    c("$2M projected savings", "mixed stakeholder feedback"),
    c("30% emission reduction", "15% water usage decrease", "industry award")
  ),
  recommendation = c("approve", "reject", "review", "approve")
)

# 3. Define evaluation metrics
# Create a weighted composite metric
composite_metric <- function(pred, exp) {
  scores <- c(
    summary = metric_f1(field = "summary")(pred, exp),
    sentiment = metric_exact_match(field = "sentiment")(pred, exp),
    recommendation = metric_exact_match(field = "recommendation")(pred, exp)
  )

  # Weighted average
  weights <- c(summary = 0.3, sentiment = 0.3, recommendation = 0.4)
  sum(scores * weights)
}

wrapped_metric <- metric_custom(composite_metric, name = "composite_score")

# 4. Test multiple optimization strategies
# Strategy A: Few-shot with different k values
results_by_k <- list()

for (k in c(1L, 2L, 3L, 4L)) {
  teleprompter <- LabeledFewShot(k = k, seed = 42L)

  optimized <- compile_module(
    program = doc_analyzer,
    teleprompter = teleprompter,
    trainset = doc_trainset
  )

  # Evaluate on validation set
  eval_result <- evaluate(
    module = optimized,
    data = doc_trainset, # In practice, use separate validation set
    metric = wrapped_metric,
    .llm = llm,
    .progress = FALSE
  )

  results_by_k[[paste0("k_", k)]] <- eval_result$mean_score
}

print(results_by_k) # See which k works best

# Strategy B: Grid search over different instruction styles
instruction_variants <- data.frame(
  id = c("executive", "technical"),
  instructions_suffix = c(
    ". Focus on business impact and executive decision-making.",
    ". Emphasize technical accuracy and detailed analysis."
  ),
  template = c(
    "Executive Analysis Required:\n{document}\n\nProvide executive summary:",
    "Technical Document Analysis:\n{document}\n\nTechnical assessment:"
  ),
  stringsAsFactors = FALSE
)

grid_optimizer <- GridSearchTeleprompter(
  variants = instruction_variants,
  metric = wrapped_metric,
  k = 2L, # Also include 2 examples
  eval_sample_size = 10L,
  verbose = TRUE
)

best_analyzer <- compile_module(
  program = doc_analyzer,
  teleprompter = grid_optimizer,
  trainset = doc_trainset,
  .llm = llm
)

# 5. Use the optimized analyzer
new_document <- "
The merger proposal shows promising synergies with projected cost savings of $5M annually.
However, cultural integration challenges are significant. Employee surveys show 60% concern
about job security. Regulatory approval is likely but will take 6-8 months. The financial
benefits outweigh the risks, but careful change management will be critical.
"

analysis <- best_analyzer |>
  run(document = new_document, .llm = llm)

print(analysis$summary)
print(analysis$topics)
print(analysis$sentiment) # Likely "mixed"
print(analysis$recommendation) # Likely "review"
```

## Part 6: Advanced Patterns and Best Practices

### Pattern 1: Cross-Validation for Robust Optimization

Don’t optimize on your entire training set—use cross-validation:

``` r

# Split data for proper evaluation
split_data <- function(data, train_pct = 0.7) {
  n <- nrow(data)
  train_idx <- sample(n, size = floor(n * train_pct))

  list(
    train = data[train_idx, ],
    val = data[-train_idx, ]
  )
}

# Split your data
splits <- split_data(doc_trainset)

# Optimize on training set
optimizer <- GridSearchTeleprompter(
  variants = instruction_variants,
  metric = wrapped_metric,
  k = 2,
  verbose = FALSE
)

optimized_module <- compile_module(
  program = doc_analyzer,
  teleprompter = optimizer,
  trainset = splits$train
)

# Evaluate on validation set
val_results <- evaluate(
  module = optimized_module,
  data = splits$val,
  metric = wrapped_metric,
  .llm = llm
)

print(paste("Validation score:", val_results$mean_score))
```

### Pattern 2: Iterative Refinement

Optimize in stages for complex tasks:

``` r

# Stage 1: Optimize for accuracy
stage1_module <- compile_module(
  program = doc_analyzer,
  teleprompter = LabeledFewShot(k = 3),
  trainset = doc_trainset
)

# Stage 2: Further optimize with grid search
# Using stage 1 as starting point
stage2_variants <- data.frame(
  id = c("refined_1", "refined_2"),
  instructions_suffix = c(
    ". Prioritize accuracy over completeness.",
    ". Ensure all key points are captured."
  ),
  stringsAsFactors = FALSE
)

stage2_module <- compile_module(
  program = stage1_module, # Start from stage 1
  teleprompter = GridSearchTeleprompter(
    variants = stage2_variants,
    metric = wrapped_metric,
    k = 1 # Add one more example
  ),
  trainset = doc_trainset
)
```

### Pattern 3: Ensemble Methods

Combine multiple optimized modules:

``` r

# Create multiple optimized versions
modules <- list()

# Version 1: Optimized for sentiment
modules$sentiment <- compile_module(
  program = doc_analyzer,
  teleprompter = LabeledFewShot(k = 4),
  trainset = doc_trainset
)

# Version 2: Optimized for facts
modules$facts <- compile_module(
  program = doc_analyzer,
  teleprompter = GridSearchTeleprompter(
    variants = data.frame(
      id = c("fact_focused"),
      instructions_suffix = c(". Focus on extracting concrete facts and data."),
      stringsAsFactors = FALSE
    ),
    metric = metric_f1(field = "key_facts"),
    k = 2
  ),
  trainset = doc_trainset
)

# Ensemble function
ensemble_analyze <- function(document, modules, llm) {
  # Get predictions from all modules
  predictions <- lapply(modules, function(m) {
    run(m, document = document, .llm = llm)
  })

  # Aggregate results (example: majority vote for categorical fields)
  sentiment_votes <- sapply(predictions, function(p) p$sentiment)
  sentiment <- names(sort(table(sentiment_votes), decreasing = TRUE))[1]

  recommendation_votes <- sapply(predictions, function(p) p$recommendation)
  recommendation <- names(sort(table(recommendation_votes), decreasing = TRUE))[
    1
  ]

  # Combine all facts
  all_facts <- unique(unlist(lapply(predictions, function(p) p$key_facts)))

  list(
    sentiment = sentiment,
    recommendation = recommendation,
    key_facts = all_facts,
    ensemble_size = length(modules)
  )
}
```

### Pattern 4: Custom Teleprompters

While dsprrr currently provides `LabeledFewShot` and
`GridSearchTeleprompter`, you can extend the system:

``` r

# Example: A teleprompter that selects diverse examples
DiverseFewShot <- S7::new_class(
  "DiverseFewShot",
  parent = Teleprompter,
  properties = list(
    k = S7::new_property(S7::class_integer, default = 4L),
    diversity_field = S7::new_property(S7::class_character, default = "")
  )
)

# Implementation would select k examples that maximize diversity
# in the specified field (e.g., different categories)
```

## Part 7: Debugging and Troubleshooting

### Understanding Compilation Results

``` r

# Inspect what the compiler did
optimized <- compile_module(
  program = classifier,
  teleprompter = LabeledFewShot(k = 3),
  trainset = trainset
)

# Check compilation status
is_compiled(optimized) # TRUE

# View selected demonstrations
print(optimized$demos)

# Check configuration
print(optimized$config$teleprompter) # "LabeledFewShot"
print(optimized$config$compilation_k) # 3

# For GridSearch, see which variant won
if (optimized$config$teleprompter == "GridSearchTeleprompter") {
  print(optimized$config$best_variant)
  print(optimized$config$all_scores)
}
```

### Module State Management

``` r

# Create fresh copy without compilation
fresh_module <- reset_copy(optimized)
is_compiled(fresh_module) # FALSE
length(fresh_module$demos) # 0

# Create deep copy preserving everything
backup_module <- deepcopy(optimized)
is_compiled(backup_module) # TRUE
identical(backup_module$demos, optimized$demos) # TRUE

# Modify copy without affecting original
backup_module$config$custom <- "modified"
is.null(optimized$config$custom) # TRUE
```

### Common Issues and Solutions

``` r

# Issue 1: Metric returns unexpected values
# Solution: Test your metric separately
test_metric <- metric_exact_match(field = "category")
test_metric(
  list(category = "spam", other = "data"),
  list(category = "spam", different = "fields")
) # Should be TRUE

# Issue 2: Grid search takes too long
# Solution: Reduce eval_sample_size or number of variants
fast_grid <- GridSearchTeleprompter(
  variants = variants[1:2, ], # Test fewer variants
  metric = metric_exact_match(field = "sentiment"),
  eval_sample_size = 10L, # Smaller evaluation set
  verbose = FALSE
)

# Issue 3: Demonstrations not improving performance
# Solution: Ensure training data quality and diversity
# Check your training data
table(trainset$sentiment) # Balanced classes?
length(unique(trainset$text)) # All unique examples?
```

## Performance Tips

### 1. Choose the Right Metric

``` r

# For classification: exact match
classification_metric <- metric_exact_match(field = "label")

# For generation: F1 or custom similarity
generation_metric <- metric_f1(field = "text")

# For complex evaluation: composite metrics
composite <- function(pred, exp) {
  accuracy <- metric_exact_match(field = "category")(pred, exp)
  quality <- metric_f1(field = "description")(pred, exp)

  0.6 * accuracy + 0.4 * quality # Weighted combination
}
```

### 2. Optimize Training Data

``` r

# Ensure diversity in training examples
check_diversity <- function(trainset, field) {
  values <- trainset[[field]]
  unique_ratio <- length(unique(values)) / length(values)

  if (unique_ratio < 0.8) {
    warning(
      "Low diversity in ",
      field,
      ": ",
      round(unique_ratio * 100),
      "% unique"
    )
  }

  table(values)
}

check_diversity(trainset, "sentiment")

# Balance classes if needed
balance_trainset <- function(data, label_col, max_per_class = NULL) {
  labels <- unique(data[[label_col]])

  balanced <- lapply(labels, function(l) {
    subset_data <- data[data[[label_col]] == l, ]

    if (!is.null(max_per_class) && nrow(subset_data) > max_per_class) {
      subset_data[sample(nrow(subset_data), max_per_class), ]
    } else {
      subset_data
    }
  })

  do.call(rbind, balanced)
}
```

### 3. Efficient Grid Search

``` r

# Start with coarse grid, then refine
# Phase 1: Test major variants
coarse_variants <- data.frame(
  id = c("brief", "detailed"),
  instructions_suffix = c(". Be brief.", ". Be detailed."),
  stringsAsFactors = FALSE
)

coarse_result <- compile_module(
  program = module,
  teleprompter = GridSearchTeleprompter(
    variants = coarse_variants,
    metric = metric,
    eval_sample_size = 20L
  ),
  trainset = trainset
)

best_style <- coarse_result$config$best_variant

# Phase 2: Refine the winning approach
if (best_style == "brief") {
  fine_variants <- data.frame(
    id = c("very_brief", "somewhat_brief", "brief_professional"),
    instructions_suffix = c(
      ". Be extremely concise.",
      ". Be concise but complete.",
      ". Be brief and professional."
    ),
    stringsAsFactors = FALSE
  )
} else {
  fine_variants <- data.frame(
    id = c("detailed_technical", "detailed_accessible", "detailed_thorough"),
    instructions_suffix = c(
      ". Provide technical details.",
      ". Explain in accessible detail.",
      ". Be thoroughly detailed."
    ),
    stringsAsFactors = FALSE
  )
}

fine_result <- compile_module(
  program = module,
  teleprompter = GridSearchTeleprompter(
    variants = fine_variants,
    metric = metric,
    eval_sample_size = 50L
  ),
  trainset = trainset
)
```

## Summary

dsprrr’s compilation framework transforms the tedious process of prompt
engineering into systematic optimization:

1.  **Metrics** provide objective evaluation of LLM outputs
2.  **Teleprompters** implement optimization strategies (few-shot, grid
    search)
3.  **Compilation** automatically improves your modules using training
    data
4.  **Evaluation** systematically tests optimized programs

Key takeaways:

- Start simple with `LabeledFewShot` for quick improvements
- Use `GridSearchTeleprompter` when you have specific variants to test
- Always evaluate on held-out data, not training data
- Create custom metrics tailored to your specific task
- Consider ensemble methods for critical applications

The compilation framework makes your LLM programs:

- **More accurate** through systematic optimization
- **More maintainable** by separating logic from prompt tuning
- **More portable** across different LLMs and use cases
- **More efficient** by finding the best prompts automatically

## Further Reading

**Tutorials:** - [Improving with
Examples](https://jameshwade.github.io/dsprrr/articles/tutorial-improve-with-demos.md)
— Learn few-shot prompting step by step - [Finding Best
Configuration](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md)
— Hands-on grid search tutorial - [Taking to
Production](https://jameshwade.github.io/dsprrr/articles/tutorial-deploy-to-production.md)
— Deploy optimized modules

**How-to Guides:** - [Advanced Optimizer
Guide](https://jameshwade.github.io/dsprrr/articles/advanced-optimization.md)
— BootstrapFewShot, COPRO, MIPROv2, and more - [Evaluate with
Vitals](https://jameshwade.github.io/dsprrr/articles/vitals-recipes.md)
— Integration with vitals package

**Concepts:** - [How Optimization
Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)
— Theory behind teleprompters - [Why Metrics
Matter](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md)
— Choosing the right metric - [Understanding Signatures &
Modules](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)
— S7 vs R6 design choices

**Reference:** - [Quick
Reference](https://jameshwade.github.io/dsprrr/articles/cheatsheet.md) —
Metrics, signature syntax, and module types
