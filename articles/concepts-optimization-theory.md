# How Prompt Optimization Works

This article explains how dsprrr optimizes prompts automatically.
Understanding the theory helps you choose the right optimizer and
configure it effectively.

## The Optimization Problem

When you build an LLM application, you’re implicitly defining an
optimization problem:

**Given**: - A task specification (signature) - Training examples with
expected outputs - An evaluation metric

**Find**: - The prompt configuration that maximizes metric performance

Traditional prompt engineering solves this by hand: you write prompts,
test them, tweak them. dsprrr automates this search.

## What Gets Optimized

LLM performance depends on several factors that can be tuned:

### 1. Instructions

The system prompt or task description:

``` r
# Original
sig <- signature(
 "text -> sentiment",
 instructions = "Classify the sentiment."
)

# Variant 1
sig_detailed <- signature(
 "text -> sentiment",
 instructions = "Analyze the emotional tone. Consider word choice, context, and implicit meanings."
)

# Variant 2
sig_concise <- signature(
 "text -> sentiment",
 instructions = "positive, negative, or neutral?"
)
```

Different instructions can dramatically affect performance. The right
wording depends on the model, the task, and the data distribution.

### 2. Demonstrations (Few-Shot Examples)

Examples shown to the model before the actual input:

``` r
# No demos
mod$demos <- list()

# With demos
mod$demos <- list(
 list(inputs = list(text = "I love this!"), output = "positive"),
 list(inputs = list(text = "This is terrible."), output = "negative"),
 list(inputs = list(text = "It's okay."), output = "neutral")
)
```

More demos aren’t always better. The *right* demos matter more than the
*number* of demos. Demos that are: - Too similar may not generalize -
Too different may confuse the model - Poorly formatted may teach bad
patterns

### 3. Temperature and Other Parameters

Model generation parameters:

``` r
mod$config$temperature <- 0      # Deterministic
mod$config$temperature <- 0.7    # Some randomness
mod$config$temperature <- 1.0    # More creative
```

Lower temperature is typically better for classification tasks. Higher
temperature helps with generation tasks where diversity matters.

### 4. Prompt Structure

How inputs are formatted and combined:

``` r
# Template 1: Simple
mod$template <- "{input}"

# Template 2: Structured
mod$template <- "Input: {input}\n\nProvide your analysis:"

# Template 3: Role-based
mod$template <- "You are an expert analyst. Given: {input}\n\nYour assessment:"
```

## The Search Space

The combination of all tunable parameters forms a **search space**. For
a simple classifier:

| Parameter      | Possible Values          | Count |
|----------------|--------------------------|-------|
| Instructions   | 5 variants               | 5     |
| Temperature    | 0, 0.3, 0.7              | 3     |
| Demo count     | 0, 2, 4, 8               | 4     |
| Demo selection | Random, diverse, similar | 3     |

Total configurations: 5 × 3 × 4 × 3 = **180 combinations**

Evaluating each on a 100-example dataset means 18,000 LLM calls. This is
why smart search strategies matter.

## Teleprompters: The Optimization Strategies

dsprrr calls its optimizers **teleprompters** (from DSPy). Each
teleprompter implements a different search strategy.

### LabeledFewShot: The Simplest Approach

Just add k examples from your training set as demonstrations:

``` r
tp <- LabeledFewShot(k = 4L)
compiled <- compile(tp, mod, trainset)
```

**How it works**:

1.  Sample k examples from training data
2.  Format as demonstrations
3.  Add to module

**When to use**: - Quick baseline - When you have high-quality labeled
data - When the task is straightforward

**Limitations**: - Doesn’t optimize demo selection - Doesn’t tune other
parameters - May pick suboptimal examples

### BootstrapFewShot: Self-Improving Demonstrations

Uses the model to generate demonstrations, keeping only successful ones:

``` r
tp <- BootstrapFewShot(
 metric = metric_exact_match(),
 max_bootstrapped_demos = 4L,
 max_labeled_demos = 8L
)
compiled <- compile(tp, mod, trainset)
```

**How it works**:

1.  Start with labeled examples as initial demos
2.  Run teacher model on remaining training examples
3.  Score each prediction with your metric
4.  Keep predictions that score above threshold as new demos
5.  Optionally repeat for multiple rounds

**Why this works**: The model generates outputs in its own “voice”.
Demonstrations that match the model’s natural output style are more
effective than human-written examples.

**When to use**: - When you have a reliable metric - When labeled
examples don’t transfer well - When you want the model to learn its own
format

### GridSearchTeleprompter: Systematic Exploration

Tests every combination of instruction and template variants:

``` r
variants <- tibble::tibble(
 id = c("concise", "detailed", "structured"),
 instructions = c(
   "Classify sentiment briefly.",
   "Analyze the emotional tone considering context and nuance.",
   "Task: Sentiment classification\nOutput: positive, negative, or neutral"
 )
)

tp <- GridSearchTeleprompter(
 metric = metric_exact_match(),
 variants = variants
)
compiled <- compile(tp, mod, trainset)
```

**How it works**:

1.  Define a grid of parameter configurations
2.  Evaluate each on a validation set
3.  Select the best-performing configuration

**When to use**: - When you have specific hypotheses to test - When the
search space is small - When you need interpretable results

**Limitations**: - Scales poorly with parameters - May miss optimal
combinations not in grid

### BootstrapFewShotWithRandomSearch: Combined Strategy

Combines demo bootstrapping with parameter search:

``` r
tp <- BootstrapFewShotWithRandomSearch(
 metric = metric_exact_match(),
 num_candidate_programs = 8L,
 max_bootstrapped_demos = 4L
)
compiled <- compile(tp, mod, trainset)
```

**How it works**:

1.  Generate multiple demo configurations via bootstrapping
2.  Create candidate programs with different demo subsets
3.  Evaluate all candidates
4.  Return the best performer

**When to use**: - When BootstrapFewShot alone isn’t enough - When you
want to explore demo combinations - When you have compute budget for
more trials

### SIMBA: Embedding-Based Selection

Uses semantic similarity to select diverse, representative demos:

``` r
tp <- SIMBA(
 metric = metric_exact_match(),
 max_demos = 4L,
 embed_fn = embed_openai()
)
compiled <- compile(tp, mod, trainset)
```

**How it works**:

1.  Embed all training examples
2.  Cluster by semantic similarity
3.  Select diverse representatives from clusters
4.  Use as demonstrations

**Why this works**: Coverage matters more than quantity. A diverse set
of demos shows the model the range of possible inputs and appropriate
outputs.

**When to use**: - When you have many training examples - When examples
vary significantly - When you want coverage over the input space

### KNNFewShot: Dynamic Demo Selection

Selects demos based on similarity to each test input:

``` r
tp <- KNNFewShot(
 metric = metric_exact_match(),
 k = 3L,
 embed_fn = embed_openai()
)
compiled <- compile(tp, mod, trainset)
```

**How it works**:

1.  At compile time: embed all training examples
2.  At runtime: find k nearest neighbors to the input
3.  Use those neighbors as demonstrations

**Why this works**: Relevant examples are more helpful than random ones.
If the input is about sports, sports-related demos are more useful than
cooking demos.

**When to use**: - When inputs span diverse domains - When
context-specific demos help - When you can afford embedding costs at
inference

### COPRO: Instruction Optimization

Generates and refines instructions using the LLM itself:

``` r
tp <- COPRO(
 metric = metric_exact_match(),
 breadth = 5L,
 depth = 3L
)
compiled <- compile(tp, mod, trainset)
```

**How it works**:

1.  Generate breadth instruction candidates
2.  Evaluate each on training data
3.  Take top performers, ask LLM to improve them
4.  Repeat for depth iterations

**Why this works**: LLMs are good at understanding what makes
instructions effective. They can propose refinements humans wouldn’t
think of.

**When to use**: - When instructions matter more than demos - When you
have compute budget for exploration - When you want
automatically-generated instructions

### MIPROv2: Multi-Parameter Optimization

Jointly optimizes instructions and demonstrations:

``` r
tp <- MIPROv2(
 metric = metric_exact_match(),
 num_candidates = 10L,
 init_temperature = 1.4
)
compiled <- compile(tp, mod, trainset)
```

Uses Bayesian optimization to jointly search instruction and demo
combinations. The most general-purpose optimizer when you have compute
budget for state-of-the-art results.

### GEPA: Genetic Programming

Evolves prompts through mutation and crossover:

``` r
tp <- GEPA(
 metric = metric_exact_match(),
 population_size = 20L,
 generations = 10L
)
compiled <- compile(tp, mod, trainset)
```

Uses evolutionary algorithms (selection, mutation, crossover) to explore
the prompt space. Good when you have compute budget and want creative
exploration.

## Choosing an Optimizer

Use this decision tree:

``` mermaid
flowchart TB
  Start["Choose an optimizer"]
  Start -->|Quick baseline| Labeled["LabeledFewShot"]
  Start -->|Reliable metric, want better demos| Bootstrap["BootstrapFewShot"]
  Start -->|Specific instruction variants to test| Grid["GridSearchTeleprompter"]
  Start -->|Best results with compute budget| MIPRO["MIPROv2 or BootstrapFewShotWithRandomSearch"]
  Start -->|Diverse inputs, need adaptive demos| KNN["KNNFewShot"]
  Start -->|Explore creatively| GEPA["GEPA"]
```

## The Evaluation Loop

All optimizers share a common evaluation pattern:

``` r
# Pseudocode for optimizer loop
for (candidate in candidates) {
 # 1. Create candidate module
 mod_candidate <- copy_module(base_module)
 mod_candidate <- apply_config(mod_candidate, candidate)

 # 2. Evaluate on validation set
 result <- evaluate(mod_candidate, valset, metric)

 # 3. Track score
 scores[[candidate$id]] <- result$mean_score

 # 4. Update best if improved
 if (result$mean_score > best_score) {
   best_score <- result$mean_score
   best_candidate <- candidate
 }
}

# 5. Apply best configuration
final_module <- apply_config(base_module, best_candidate)
```

This loop is the core of all optimization. Different teleprompters
differ in how they generate candidates.

## Practical Considerations

### Data Requirements

| Optimizer        | Min Training Examples | Recommended |
|------------------|-----------------------|-------------|
| LabeledFewShot   | k (usually 4)         | 20+         |
| BootstrapFewShot | 10                    | 50+         |
| GridSearch       | 20                    | 50+         |
| MIPROv2          | 30                    | 100+        |

More data generally helps, but with diminishing returns after ~100
examples.

### Compute Costs

Optimization requires many LLM calls. Rough estimates per optimizer:

| Optimizer        | LLM Calls (100 examples)            |
|------------------|-------------------------------------|
| LabeledFewShot   | 0 (no evaluation)                   |
| BootstrapFewShot | 100-200                             |
| GridSearch       | variants × examples                 |
| MIPROv2          | 500-2000                            |
| GEPA             | population × generations × examples |

Use cheaper models during optimization, then evaluate final config on
your target model.

### Validation Strategy

Split your data:

- **Training set**: Used to generate demos
- **Validation set**: Used to evaluate candidates during optimization
- **Test set**: Used to evaluate final model (never touched during
  optimization)

``` r
# Good practice
set.seed(42)
n <- nrow(data)
train_idx <- sample(n, 0.6 * n)
val_idx <- sample(setdiff(1:n, train_idx), 0.2 * n)
test_idx <- setdiff(1:n, c(train_idx, val_idx))

trainset <- data[train_idx, ]
valset <- data[val_idx, ]
testset <- data[test_idx, ]

# Optimize
compiled <- compile(tp, mod, trainset, valset = valset)

# Final evaluation
evaluate(compiled, testset, metric)
```

### Reproducibility

Use seeds for reproducible optimization:

``` r
tp <- BootstrapFewShot(
 metric = metric_exact_match(),
 seed = 42L
)
```

## Why Optimization Works

Prompt optimization works because:

1.  **LLMs are sensitive**: Small prompt changes cause big output
    changes. This creates a navigable landscape where search can find
    improvements.

2.  **Metrics provide signal**: Evaluation scores guide the search
    toward better configurations. Without metrics, you’re optimizing
    blind.

3.  **The search space is structured**: Good prompts share
    characteristics. Optimization exploits this structure.

4.  **Transfer happens**: Prompts optimized on training data often
    generalize to test data. The improvements aren’t just memorization.

## Connection to Machine Learning

Prompt optimization parallels traditional ML:

| ML Concept       | Prompt Optimization     |
|------------------|-------------------------|
| Model parameters | Prompt text, demos      |
| Training data    | Labeled examples        |
| Loss function    | Metric (negated)        |
| Gradient descent | Teleprompter search     |
| Hyperparameters  | Temperature, demo count |
| Validation       | Held-out evaluation     |

The key difference: in ML, we update model weights. In prompt
optimization, we update the text we send to the model. The model itself
stays fixed.

## Further Reading

- **[Tutorial: Optimize Your
  Module](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md)** -
  Hands-on optimization
- **[Understanding Signatures &
  Modules](https://jameshwade.github.io/dsprrr/articles/concepts-signatures-modules.md)** -
  Core abstractions
- **[Why Metrics
  Matter](https://jameshwade.github.io/dsprrr/articles/concepts-why-metrics-matter.md)** -
  Choosing the right metric
- **[DSPy Paper](https://arxiv.org/abs/2310.03714)** - Academic
  foundations
