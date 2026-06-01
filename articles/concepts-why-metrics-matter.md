# Why Metrics Drive Better LLM Applications

This article explains why metrics are essential for building reliable
LLM applications and how dsprrr’s evaluation workflow enables systematic
improvement.

## The Problem with “It Looks Good”

Most LLM development follows this pattern:

1.  Write a prompt
2.  Try a few examples
3.  Outputs “look good”
4.  Ship it

This feels productive but hides a dangerous assumption: **your intuition
about output quality is reliable**. It isn’t.

### Why Intuition Fails

**Confirmation bias**: You notice examples that work and forget ones
that don’t. After tweaking a prompt, you test it on the example that was
failing. It works now. You don’t re-test the examples that were working
before.

**Incomplete coverage**: You test 5 examples manually. Your production
data has thousands of variations. The examples you tested aren’t
representative.

**Changing baselines**: You improve the prompt. But did it actually get
better, or does it just look different? Without quantitative comparison,
you can’t know.

**Subjective judgment**: “Good enough” varies by person and by mood.
What seems acceptable on Friday afternoon might look inadequate Monday
morning.

## Metrics as Ground Truth

A metric converts subjective judgment into objective measurement:

``` r

# Without metrics: "This looks right"
result <- run(mod, text = "I love this product!")
# result: "positive"  # Is this correct? Probably?

# With metrics: This IS right (or wrong)
score <- metric_exact_match()(result, "positive")
# score: TRUE or FALSE, no ambiguity
```

Metrics provide:

- **Reproducibility**: Same inputs → same scores, every time
- **Comparability**: Version A scores 0.78, Version B scores 0.82
- **Automation**: Evaluate thousands of examples without human review
- **Progress tracking**: Did this change help or hurt?

## Built-in Metrics

dsprrr provides common metrics out of the box:

### Exact Match

For classification and extraction tasks where the answer must be exactly
right:

``` r

metric <- metric_exact_match()
metric("positive", "positive")  # TRUE
metric("Positive", "positive")  # FALSE (case sensitive by default)

# Case insensitive
metric <- metric_exact_match(ignore_case = TRUE)
metric("Positive", "positive")  # TRUE

# Field extraction for structured outputs
metric <- metric_exact_match(field = "sentiment")
metric(
  list(sentiment = "positive", confidence = 0.9),
  list(sentiment = "positive")
)  # TRUE
```

### Contains

For tasks where the answer should include specific content:

``` r

metric <- metric_contains()
metric("The capital of France is Paris.", "Paris")  # TRUE
metric("The capital of France is Paris.", "London")  # FALSE
```

### F1 Score

For text generation where partial overlap matters:

``` r

metric <- metric_f1()
metric("the quick brown fox", "the fast brown fox")  # 0.75
# Tokens: {the, quick, brown, fox} vs {the, fast, brown, fox}
# Overlap: {the, brown, fox} = 3 tokens
# Precision: 3/4, Recall: 3/4, F1: 0.75
```

### Custom Metrics

For domain-specific evaluation:

``` r

# Check if output is valid JSON
metric_valid_json <- function(prediction, expected) {
  tryCatch({
    jsonlite::fromJSON(prediction)
    TRUE
  }, error = function(e) FALSE)
}

# Check if sentiment matches with confidence threshold
metric_confident_match <- function(prediction, expected) {
  if (!is.list(prediction)) return(FALSE)
  prediction$sentiment == expected$sentiment &&
    prediction$confidence >= 0.8
}
```

## The Evaluation Workflow

dsprrr’s
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
function runs your module on a dataset and computes metrics:

``` r

# Prepare labeled data
test_data <- tibble::tibble(
  text = c("Great product!", "Terrible service", "It's okay"),
  sentiment = c("positive", "negative", "neutral")
)

# Evaluate
result <- evaluate(
  mod,
  data = test_data,
  metric = metric_exact_match(field = "sentiment"),
  .llm = llm
)

result$mean_score    # 0.67 (2 out of 3 correct)
result$scores        # c(TRUE, TRUE, FALSE)
result$predictions   # What the model actually said
result$n_evaluated   # 3
result$n_errors      # 0
```

This single function call:

1.  Runs your module on every example
2.  Applies your metric to each prediction
3.  Aggregates results into actionable statistics

## Metric-Driven Development

With metrics in place, development becomes systematic:

### 1. Start with Evaluation

Before writing any prompt, create your evaluation dataset:

``` r

# Collect or create labeled examples
eval_data <- tibble::tibble(
  question = c(
    "What is 2+2?",
    "What is the capital of France?",
    "Who wrote Romeo and Juliet?"
  ),
  answer = c("4", "Paris", "Shakespeare")
)
```

This forces you to define success upfront. What counts as a correct
answer? What edge cases matter?

### 2. Establish a Baseline

Run evaluation with your initial prompt:

``` r

baseline <- evaluate(mod_v1, eval_data, metric_exact_match())
baseline$mean_score  # 0.65
```

Now you have a number to beat.

### 3. Iterate with Measurement

Every change gets measured:

``` r

# Try different instructions
mod_v2 <- module(
  signature("question -> answer", instructions = "Be concise."),
  type = "predict"
)
v2_result <- evaluate(mod_v2, eval_data, metric_exact_match())
v2_result$mean_score  # 0.70 - better!

# Try adding examples
mod_v3 <- mod_v2$clone()
mod_v3$demos <- list(
  list(inputs = list(question = "What is 3+3?"), output = "6")
)
v3_result <- evaluate(mod_v3, eval_data, metric_exact_match())
v3_result$mean_score  # 0.75 - even better!
```

No guessing. No “it feels better”. Just numbers.

### 4. Automate Optimization

Let the computer search for you:

``` r

mod$optimize_grid(
  devset = train_data,
  metric = metric_exact_match(),
  parameters = list(
    temperature = c(0, 0.3, 0.7),
    instructions = c("Be concise.", "Think step by step.", "Answer directly.")
  )
)

# Best configuration is automatically selected
mod$state$best_score  # 0.82
```

Optimization only works because metrics provide objective feedback.

## Choosing the Right Metric

Different tasks need different metrics:

| Task | Recommended Metric | Why |
|----|----|----|
| Classification | [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md) | Answer must be exactly right |
| Extraction | `metric_exact_match(field = "...")` | Extract specific field |
| Generation | [`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md) | Partial credit for overlap |
| Yes/No questions | `metric_exact_match(ignore_case = TRUE)` | “Yes” = “yes” |
| Contains keyword | [`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md) | Answer includes key info |
| Complex evaluation | Custom function | Domain-specific logic |

### When Exact Match is Too Strict

Sometimes equivalent answers look different:

``` r

# These are all correct, but exact match fails
metric_exact_match()("4", "four")           # FALSE
metric_exact_match()("Paris", "paris")      # FALSE
metric_exact_match()("$100", "100 dollars") # FALSE
```

Solutions: - Normalize outputs before comparison - Use
`ignore_case = TRUE` - Write a custom metric that handles variations -
Use F1 for partial credit

### When F1 is Too Lenient

F1 gives partial credit for word overlap, but sometimes that’s wrong:

``` r

metric_f1()("The answer is yes", "The answer is no")  # 0.75
# High score despite opposite meaning!
```

For tasks where meaning matters more than words, consider: - Exact match
on key fields - Semantic similarity metrics - Model-graded evaluation
(use another LLM to judge)

## The Data Split

Proper evaluation requires data discipline:

``` r

# Split your data
set.seed(42)
n <- nrow(all_data)

# 60% for training (used during optimization)
train_idx <- sample(n, 0.6 * n)

# 20% for validation (used to select best config)
remaining <- setdiff(1:n, train_idx)
val_idx <- sample(remaining, length(remaining) / 2)

# 20% for test (never touched until final evaluation)
test_idx <- setdiff(remaining, val_idx)

trainset <- all_data[train_idx, ]
valset <- all_data[val_idx, ]
testset <- all_data[test_idx, ]
```

**Training set**: Used to generate demonstrations and optimize prompts.

**Validation set**: Used to compare configurations and select the best.

**Test set**: Used once, at the end, to report final performance. Never
optimize against it.

If you optimize against your test set, you’re measuring how well you
overfit, not how well you’ll generalize.

## Common Pitfalls

### Testing on Training Data

``` r

# WRONG: Evaluate on the same data used for optimization
mod$optimize_grid(data, metric)
evaluate(mod, data, metric)  # Overfit score!

# RIGHT: Use held-out test data
mod$optimize_grid(trainset, metric)
evaluate(mod, testset, metric)  # Honest score
```

### Metric Doesn’t Match Task

``` r

# Task: Generate creative stories
# WRONG: Exact match (too strict)
evaluate(mod, data, metric_exact_match())

# RIGHT: Custom metric for creativity/coherence
evaluate(mod, data, metric_story_quality)
```

### Too Few Examples

``` r

# WRONG: 5 examples gives noisy estimates
test_tiny <- data[1:5, ]
evaluate(mod, test_tiny, metric)  # Could be 0.8 or 0.2 by chance

# RIGHT: 50+ examples for stable estimates
test_good <- data[1:50, ]
evaluate(mod, test_good, metric)  # Reliable signal
```

## The Payoff

Metric-driven development takes more upfront effort: - Collecting
labeled data - Choosing appropriate metrics - Maintaining train/val/test
splits

But it pays off:

- **Confidence**: You know when things work
- **Progress**: You can measure improvement
- **Debugging**: You can identify failure modes
- **Automation**: Optimization becomes possible
- **Communication**: Share concrete numbers, not vibes

## Further Reading

- **[Tutorial: Optimize Your
  Module](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md)** -
  Metrics in action
- **[How Optimization
  Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)** -
  Why metrics enable optimization
- **[API Reference:
  Metrics](https://jameshwade.github.io/dsprrr/articles/reference/metric_exact_match.md)** -
  Built-in metrics
