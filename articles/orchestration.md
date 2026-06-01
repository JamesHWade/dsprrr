# Production Workflows with dsprrr

## Introduction

Building LLM applications in development is one thing; running them
reliably in production is another. This vignette covers dsprrr’s
orchestration features that help you:

- **Persist** module configurations and traces across sessions
- **Orchestrate** complex pipelines with targets
- **Report** on experiments with Quarto
- **Validate** workflows before expensive LLM operations

These features integrate with the broader R ecosystem:

- **pins** for versioned artifact storage
- **targets** for reproducible pipelines
- **Quarto** for rich reporting

## The Challenge of Production LLM Workflows

LLM applications present unique challenges for production deployment:

1.  **Configuration Drift**: Optimized prompts and parameters can be
    lost between sessions
2.  **Cost Tracking**: LLM API calls are expensive and need monitoring
3.  **Reproducibility**: Results should be reproducible for debugging
    and auditing
4.  **Collaboration**: Team members need to share optimized modules

dsprrr’s orchestration helpers address these challenges by providing a
structured approach to persisting and sharing LLM workflow artifacts.

## Persisting Module Configurations with pins

The **pins** package provides a simple way to store and version R
objects. dsprrr integrates with pins to save module configurations,
making it easy to:

- Share optimized modules across projects
- Version control your LLM configurations
- Restore modules in new sessions without re-optimization

### Setting Up a Pins Board

First, create a pins board. You can use local storage, cloud providers
(S3, Azure, GCS), or Posit Connect:

``` r

library(pins)

# Local board for development
board <- board_folder("pins", versioned = TRUE)

# Cloud boards for production
# board <- board_s3("my-bucket/dsprrr-pins")
# board <- board_connect()  # Posit Connect
```

### Pinning a Module Configuration

After optimizing a module, save its configuration:

``` r

library(dsprrr)
library(ellmer)

# Create and optimize a module
mod <- signature("text -> sentiment: enum('positive', 'negative', 'neutral')") |>
  module(type = "predict")

# Run optimization (in practice, with real data)
# optimize_grid(mod, devset = train_data, metric = exact_match, .llm = llm)

# Pin the configuration
pin_module_config(
  board = board,
  name = "sentiment-classifier-v1",
  module = mod,
  description = "Production sentiment classifier, optimized on customer feedback"
)
```

The pinned configuration includes:

- **Signature**: Input/output specifications
- **Configuration**: Temperature, prompt style, template
- **Optimization state**: Best parameters, trial history
- **Metadata**: Package version, timestamps

### Restoring a Module

In a new session or different project, restore the module:

``` r

# Read the pinned configuration
config <- pin_read(board, "sentiment-classifier-v1")

# Restore the module
mod <- restore_module_config(config)

# Use immediately - no re-optimization needed!
result <- run(mod, text = "Great product!", .llm = llm)
```

### Versioning and Rollback

Pins automatically versions your configurations:

``` r

# List versions
pin_versions(board, "sentiment-classifier-v1")

# Read a specific version
old_config <- pin_read(board, "sentiment-classifier-v1", version = "20240115T120000Z")
old_mod <- restore_module_config(old_config)
```

## Saving Execution Traces

Traces capture detailed information about LLM calls: timing, token
usage, prompts, and outputs. Pinning traces enables:

- Cost analysis across experiments
- Performance debugging
- Audit trails for compliance

### Pinning Traces

After running predictions, save the traces:

``` r

# Run some predictions
results <- run(mod, text = test_texts, .llm = llm, .progress = TRUE)

# Pin traces with full details
pin_trace(
  board = board,
  name = "experiment-2024-01-traces",
  module = mod,
  include_prompts = TRUE,
  include_outputs = TRUE,
  description = "Production test run"
)
```

### Analyzing Traces

Load pinned traces for analysis:

``` r

library(dplyr)
library(ggplot2)

# Load trace data
trace_data <- pin_read(board, "experiment-2024-01-traces")

# Access the traces tibble
traces_df <- trace_data$traces

# Analyze token usage
traces_df |>
  summarize(
    total_tokens = sum(total_tokens),
    avg_latency = mean(latency_ms),
    total_cost = sum(cost, na.rm = TRUE)
  )

# Plot latency over time
ggplot(traces_df, aes(x = timestamp, y = latency_ms)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "Request Latency Over Time", y = "Latency (ms)")
```

## Saving Evaluation Results

Evaluation results from
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
or vitals Tasks can be pinned for tracking model performance over time:

``` r

# Run evaluation
eval_result <- evaluate(
  mod,
  dataset = test_data,
  metric = metric_exact_match(),
  .llm = llm
)

# Pin the results
pin_vitals_log(
  board = board,
  name = "sentiment-eval-2024-01",
  eval_result = eval_result,
  module = mod,
  description = "Monthly evaluation on customer feedback test set"
)
```

### Tracking Performance Over Time

Compare evaluations across time:

``` r

# Load multiple evaluation results
eval_jan <- pin_read(board, "sentiment-eval-2024-01")
eval_feb <- pin_read(board, "sentiment-eval-2024-02")

# Compare scores
tibble(
  month = c("January", "February"),
  accuracy = c(eval_jan$mean_score, eval_feb$mean_score),
  n_samples = c(eval_jan$n_evaluated, eval_feb$n_evaluated)
)
```

## Orchestrating Pipelines with targets

For complex workflows, the **targets** package provides a powerful
framework for:

- Dependency tracking and caching
- Parallel execution
- Reproducible pipelines

### Using the targets Template

dsprrr provides a ready-to-use targets template:

``` r

# Copy the template to your project
use_dsprrr_template("targets")

# This creates _targets.R with a complete pipeline
```

### Anatomy of the Pipeline

The template includes these stages:

``` r

# 1. Data Preparation
tar_target(train_data, load_training_data())
tar_target(test_data, load_test_data())

# 2. Module Definition
tar_target(module_definition, {
  signature("text -> sentiment") |>
    module(type = "predict")
})

# 3. Optimization
tar_target(optimized_module, {
  mod <- module_definition$clone(deep = TRUE)
  optimize_grid(mod, devset = train_data, ...)
  mod
})

# 4. Evaluation
tar_target(evaluation_results, {
  evaluate(optimized_module, dataset = test_data, ...)
})

# 5. Persistence
tar_target(pinned_config, {
  pin_module_config(board, "model", optimized_module)
})
```

### Running the Pipeline

Execute your pipeline with targets:

``` r

library(targets)

# Run the full pipeline
tar_make()

# Visualize dependencies
tar_visnetwork()

# Read specific targets
eval_results <- tar_read(evaluation_results)
```

### Incremental Updates

targets caches results and only reruns what’s changed:

``` r

# Modify only the test data
# targets will skip optimization and only rerun evaluation

tar_outdated()  # See what will rerun
tar_make()      # Only evaluation runs
```

## Generating Reports with Quarto

Quarto documents provide rich, reproducible reports. dsprrr’s template
generates professional experiment reports:

``` r

# Copy the Quarto template
use_dsprrr_template("quarto")

# This creates report.qmd
```

### Customizing the Report

The template reads from your pins board and generates:

- Module configuration summary
- Evaluation metrics and visualizations
- Trace analysis (latency, tokens, costs)
- Reproducibility information

Configure the report parameters in the YAML header:

``` yaml
params:
  pins_board_path: "pins"
  module_name: "sentiment-classifier"
  eval_name: "sentiment-eval-results"
  traces_name: "sentiment-eval-traces"
```

### Rendering the Report

Render your report:

``` r

# From R
quarto::quarto_render("report.qmd")

# From terminal
# quarto render report.qmd
```

### Integrating with targets

Add report rendering to your targets pipeline:

``` r

tar_quarto(
  report,
  path = "report.qmd",
  quiet = FALSE
)
```

## Validating Workflows

Before running expensive LLM operations, validate your workflow:

``` r

# Check that everything is configured correctly
validate_workflow(
  module = mod,
  dataset = test_data,
  board = board
)

# Output:
# -- Workflow Validation --------------------------------
# v module: Module type: PredictModule
# v signature: 1 input(s) defined
# v dataset: 100 rows, 1 required columns present
# v board: Board type: pins_board_folder
# v Workflow validation passed
```

This catches common issues:

- Missing required columns in datasets
- Invalid module configurations
- Inaccessible pins boards

## Complete Production Workflow

Here’s a complete example bringing everything together:

``` r

library(dsprrr)
library(ellmer)
library(pins)
library(targets)

# ---- Setup ----
board <- board_folder("pins", versioned = TRUE)
llm <- chat_claude()

# ---- Define Module ----
mod <- signature(
  "feedback -> sentiment: enum('positive', 'negative', 'neutral'), issues: array(string)",
  instructions = "Analyze customer feedback. Identify sentiment and extract specific issues mentioned."
) |>
  module(type = "predict")

# ---- Optimize ----
train_data <- read_csv("data/train.csv")

optimize_grid(
  mod,
  devset = train_data,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(temperature = c(0, 0.3, 0.7)),
  .llm = llm
)

# ---- Validate ----
test_data <- read_csv("data/test.csv")
validate_workflow(mod, dataset = test_data, board = board)

# ---- Evaluate ----
eval_result <- evaluate(
  mod,
  dataset = test_data,
  metric = metric_exact_match(field = "sentiment"),
  .llm = llm
)

# ---- Persist ----
pin_module_config(board, "feedback-analyzer-v2", mod)
pin_trace(board, "feedback-eval-traces", mod, include_prompts = TRUE)
pin_vitals_log(board, "feedback-eval-v2", eval_result, module = mod)

# ---- Report ----
use_dsprrr_template("quarto")
quarto::quarto_render("report.qmd")
```

## Best Practices

### 1. Version Your Configurations

Always use versioned pins boards:

``` r

board <- board_folder("pins", versioned = TRUE)
```

This enables rollback and audit trails.

### 2. Include Descriptive Metadata

Add descriptions to your pins:

``` r

pin_module_config(
  board, "classifier-v1", mod,
  description = "Trained on Q1 2024 data, optimized for precision"
)
```

### 3. Separate Development and Production

Use different boards for different environments:

``` r

dev_board <- board_folder("pins-dev")
prod_board <- board_s3("prod-bucket/dsprrr-pins")
```

### 4. Validate Before Running

Always validate workflows, especially in production:

``` r

validation <- validate_workflow(mod, dataset, board)
if (!validation$valid) {
  stop("Workflow validation failed")
}
```

### 5. Track Costs

Monitor token usage and costs via traces:

``` r

traces <- pin_read(board, "latest-traces")
total_cost <- sum(traces$traces$cost, na.rm = TRUE)
cli::cli_alert_info("Total API cost: ${total_cost}")
```

## Integration with vitals

dsprrr’s orchestration integrates seamlessly with the vitals package for
rigorous LLM evaluation. See
[`vignette("vitals-integration")`](https://jameshwade.github.io/dsprrr/articles/vitals-integration.md)
for details on:

- Converting dsprrr modules to vitals solvers
- Using vitals scorers in dsprrr metrics
- Combining vitals Tasks with dsprrr optimization

## Next Steps

- **Getting Started**:
  [`vignette("getting-started")`](https://jameshwade.github.io/dsprrr/articles/getting-started.md)
  for dsprrr basics
- **Optimization**:
  [`vignette("compilation-optimization")`](https://jameshwade.github.io/dsprrr/articles/compilation-optimization.md)
  for tuning modules
- **Vitals**:
  [`vignette("vitals-integration")`](https://jameshwade.github.io/dsprrr/articles/vitals-integration.md)
  for evaluation workflows

## Summary

dsprrr’s orchestration features enable production-ready LLM workflows:

| Feature | Purpose | Package |
|----|----|----|
| [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md) | Save/share optimized modules | pins |
| [`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md) | Persist execution traces | pins |
| [`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md) | Store evaluation results | pins |
| `use_dsprrr_template("targets")` | Pipeline orchestration | targets |
| `use_dsprrr_template("quarto")` | Experiment reporting | Quarto |
| [`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md) | Pre-flight checks | dsprrr |

These tools integrate dsprrr into the broader R ecosystem, making it
easy to build reliable, reproducible, and collaborative LLM
applications.
