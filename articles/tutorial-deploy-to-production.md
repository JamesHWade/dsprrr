# Tutorial 6: Taking to Production

You’ve built and optimized a module through Tutorials 1-5. Now what? You
need to: - Save the optimized configuration - Load it in production -
Track what happens in deployment - Monitor for issues

This tutorial shows you how.

**Time**: 30-35 minutes

## What You’ll Build

A production-ready module with: - Persistent configuration (survives R
restarts) - Execution traces for debugging - Validation before
deployment

## Prerequisites

- Completed [Tutorial
  5](https://jameshwade.github.io/dsprrr/articles/tutorial-optimize-your-module.md)
- `OPENAI_API_KEY` set in your environment

``` r
library(dsprrr)
#> 
#> Attaching package: 'dsprrr'
#> The following object is masked from 'package:stats':
#> 
#>     step
#> The following object is masked from 'package:methods':
#> 
#>     signature
library(ellmer)
library(pins)
library(tibble)

chat <- chat_openai()
#> Using model = "gpt-4.1".
```

## Step 1: Create an Optimized Module

Let’s start with a trained classifier:

``` r
sig <- signature(
  "review -> sentiment: enum('positive', 'negative', 'neutral')",
  instructions = "Classify the sentiment of this product review."
)

trainset <- dsp_trainset(
  review = c(
    "Love it!", "Hate it!", "It's okay.",
    "Amazing!", "Terrible!", "Meh.",
    "Best ever!", "Worst purchase!", "Fine I guess."
  ),
  sentiment = c(
    "positive", "negative", "neutral",
    "positive", "negative", "neutral",
    "positive", "negative", "neutral"
  )
)

# Optimize
classifier <- compile_module(
  program = module(sig, type = "predict"),
  teleprompter = LabeledFewShot(k = 3L),
  trainset = trainset
)

# Verify it works
run(classifier, review = "This product is fantastic!", .llm = chat)
#> $sentiment
#> [1] "positive"
```

## Step 2: Save Configuration with Pins

The `pins` package provides persistent storage. Save your module
configuration:

``` r
# Create a local board (folder on disk)
board <- board_folder(tempdir())

# Save the module configuration
pin_module_config(board, "sentiment-classifier", classifier)
#> Creating new version '20260214T023204Z-95c01'
#> Writing to pin 'sentiment-classifier'
#> ✔ Pinned module configuration: "sentiment-classifier"
#> ℹ Module type: <PredictModule>
#> ℹ Compiled: TRUE
```

Your optimized configuration—including demos, parameters, and
instructions—is now saved.

## Step 3: List Saved Modules

See what’s stored:

``` r
board |> pin_list()
#> [1] "sentiment-classifier"

# Get metadata
board |> pin_meta("sentiment-classifier")
#> List of 13
#>  $ file       : chr "sentiment-classifier.rds"
#>  $ file_size  : 'fs_bytes' int 492
#>  $ pin_hash   : chr "95c0183db9a93e10"
#>  $ type       : chr "rds"
#>  $ title      : chr "sentiment-classifier: a pinned list"
#>  $ description: chr "dsprrr module config: sentiment-classifier"
#>  $ tags       : NULL
#>  $ urls       : NULL
#>  $ created    : POSIXct[1:1], format: "2026-02-14 02:32:04"
#>  $ api_version: int 1
#>  $ user       : list()
#>  $ name       : chr "sentiment-classifier"
#>  $ local      :List of 3
#>   ..$ dir    : 'fs_path' chr "/tmp/RtmplUJjEb/sentiment-classifier/20260214T023204Z-95c01"
#>   ..$ url    : NULL
#>   ..$ version: chr "20260214T023204Z-95c01"
```

## Step 4: Restore in a New Session

Imagine you restart R or deploy to a different machine:

``` r
# Later, in a new session...
config <- pins::pin_read(board, "sentiment-classifier")
restored <- restore_module_config(config)
#> ✔ Restored module from configuration
#> ℹ Module type: <PredictModule>
#> ℹ Original dsprrr version: "0.0.0.9000"

# It works immediately
run(restored, review = "Worst product ever!", .llm = chat)
#> [1] "negative"
```

The restored module has all the optimization work preserved.

## Step 5: Version Your Modules

Pins automatically version your saves:

``` r
# Make some changes
improved <- compile_module(
  program = restored,
  teleprompter = LabeledFewShot(k = 4L),  # Try more examples
  trainset = trainset
)
#> Warning: Program appears to be already compiled
#> ℹ Previous teleprompter: LabeledFewShot
#> ℹ Recompiling with: dsprrr::LabeledFewShot

# Save again - creates new version
pin_module_config(board, "sentiment-classifier", improved)
#> Creating new version '20260214T023204Z-45a11'
#> Writing to pin 'sentiment-classifier'
#> ✔ Pinned module configuration: "sentiment-classifier"
#> ℹ Module type: <PredictModule>
#> ℹ Compiled: TRUE

# List versions
board |> pin_versions("sentiment-classifier")
#> # A tibble: 2 × 3
#>   version                created             hash 
#>   <chr>                  <dttm>              <chr>
#> 1 20260214T023204Z-45a11 2026-02-14 02:32:04 45a11
#> 2 20260214T023204Z-95c01 2026-02-14 02:32:04 95c01
```

## Step 6: Roll Back to Previous Version

If a new version performs worse:

``` r
# Get specific version
versions <- board |> pin_versions("sentiment-classifier")

# Restore the first version
if (nrow(versions) > 1) {
  config <- pins::pin_read(board, "sentiment-classifier", version = versions$version[1])
  original <- restore_module_config(config)
}
#> ✔ Restored module from configuration
#> ℹ Module type: <PredictModule>
#> ℹ Original dsprrr version: "0.0.0.9000"
```

## Step 7: Save Execution Traces

Track what your module does in production:

``` r
# Run some predictions
run(classifier, review = "Great product!", .llm = chat)
run(classifier, review = "Not worth the money.", .llm = chat)
run(classifier, review = "Does the job.", .llm = chat)

# Save traces for analysis
pin_trace(board, "sentiment-traces", classifier)
```

## Step 8: Analyze Traces

Load and examine traces:

``` r
# Export as tibble
traces <- export_traces(classifier, format = "tibble")
traces

# Summary statistics
classifier$trace_summary()
```

Traces include: - Input/output for each call - Token usage - Latency -
Errors (if any)

## Step 9: Validate Before Deployment

Use
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)
to check everything is ready:

``` r
# Check module is properly configured
validation <- validate_workflow(
  module = classifier,
  board = board
)
#> 
#> ── Workflow Validation ──
#> 
#> ✔ module: Module type: PredictModule
#> ✔ signature: 1 input(s) defined
#> ✔ board: Board type: pins_board_folder
#> ✔ Workflow validation passed

validation
#> $valid
#> [1] TRUE
#> 
#> $checks
#> $checks$module
#> $checks$module$passed
#> [1] TRUE
#> 
#> $checks$module$message
#> [1] "Module type: PredictModule"
#> 
#> 
#> $checks$signature
#> $checks$signature$passed
#> [1] TRUE
#> 
#> $checks$signature$message
#> [1] "1 input(s) defined"
#> 
#> 
#> $checks$board
#> $checks$board$passed
#> [1] TRUE
#> 
#> $checks$board$message
#> [1] "Board type: pins_board_folder"
```

This checks: - Module is a valid DSPrrr module - Signature has inputs
defined - Board is accessible (if provided)

## Step 10: Production Patterns

Here’s a complete production workflow:

``` r
# === DEVELOPMENT ===
# 1. Build and optimize
dev_module <- module(sig, type = "predict")
dev_module$optimize_grid(
  devset = trainset,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(temperature = c(0, 0.3, 0.7))
)

# 2. Compile with best settings + demos
optimized <- compile_module(
  program = dev_module,
  teleprompter = LabeledFewShot(k = 3L),
  trainset = trainset
)

# 3. Evaluate on test data
evaluate(optimized, testset, metric = metric_exact_match(field = "sentiment"))

# 4. Save if good enough
prod_board <- board_s3("my-bucket")  # Or board_connect(), board_folder()
pin_module_config(prod_board, "sentiment-v1", optimized)

# === PRODUCTION ===
# 1. Load the saved configuration
prod_module <- restore_module_config(prod_board, "sentiment-v1")

# 2. Use it
result <- run(prod_module, review = customer_review, .llm = chat_openai())

# 3. Periodically save traces for monitoring
pin_trace(prod_board, "sentiment-traces", prod_module)
```

## Step 11: Different Storage Backends

Pins supports multiple backends:

``` r
# Local folder
board_folder("path/to/folder")

# Posit Connect
board_connect()

# AWS S3
board_s3("bucket-name")

# Azure
board_azure("container-name")

# Google Cloud
board_gcs("bucket-name")
```

Choose based on your deployment environment.

## Step 12: Monitoring in Production

Set up regular checks:

``` r
# Daily: Check trace summary
module$trace_summary()

# Weekly: Evaluate on held-out samples
evaluate(module, weekly_sample, metric = metric_exact_match())

# Monthly: Compare to baseline
# If accuracy drops, investigate or retrain
```

## What You Learned

In this tutorial, you:

1.  Saved module configurations with
    [`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
2.  Restored modules with
    [`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)
3.  Used versioning for safe updates
4.  Rolled back to previous versions
5.  Saved and analyzed execution traces
6.  Validated modules before deployment
7.  Learned production workflow patterns

## The Production Checklist

Before deploying:

Module is optimized on representative data

Evaluated on held-out test data

Configuration saved to persistent storage

Versioning enabled for rollback

Trace logging configured

Monitoring plan in place

## Where to Go From Here

Congratulations! You’ve completed the core tutorial sequence. You now
know how to: - Make structured LLM calls - Build reusable modules -
Extract complex data - Improve with examples - Optimize parameters -
Deploy to production

### Advanced Tutorials

Build complete applications:

- **[Text Adventure
  Game](https://jameshwade.github.io/dsprrr/articles/text-adventure.md)**
  — Interactive AI with state management
- **[Generate
  llms.txt](https://jameshwade.github.io/dsprrr/articles/llms-txt.md)**
  — Multi-stage documentation pipeline

### How-To Guides

Solve specific problems:

- **[Build RAG
  Pipelines](https://jameshwade.github.io/dsprrr/articles/rag-workflows.md)**
  — Retrieval-augmented generation
- **[Evaluate with
  Vitals](https://jameshwade.github.io/dsprrr/articles/vitals-integration.md)**
  — Rigorous evaluation
- **[Production
  Orchestration](https://jameshwade.github.io/dsprrr/articles/orchestration.md)**
  — targets and Quarto integration

### Concepts

Understand the “why”:

- **[The DSPy
  Philosophy](https://jameshwade.github.io/dsprrr/articles/concepts-dspy-philosophy.md)**
  — Programs, not prompts
- **[How Optimization
  Works](https://jameshwade.github.io/dsprrr/articles/concepts-optimization-theory.md)**
  — Teleprompter theory
