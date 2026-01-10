#!/usr/bin/env Rscript
# Example: Using epochs for statistically significant optimization
# This demonstrates how to run evaluations multiple times to quantify variation

library(dsprrr)
library(tibble)

# Create a simple QA module
sig <- signature("question -> answer")
mod <- module(sig, type = "predict")

# Test dataset
dataset <- tibble(
  question = c(
    "What is 2+2?",
    "What is the capital of France?",
    "What color is the sky?"
  ),
  answer = c("4", "Paris", "blue")
)

# Metric to check correctness
metric <- metric_exact_match()

# Single epoch (baseline) - default behavior
result_single <- evaluate(
  mod,
  data = dataset,
  metric = metric,
  epochs = 1L
)

print(result_single)

# Multiple epochs to get confidence intervals
result_multi <- evaluate(
  mod,
  data = dataset,
  metric = metric,
  epochs = 3L
)

print(result_multi)

# Access epoch-specific results
cat("\nEpoch-by-epoch scores:\n")
for (i in seq_along(result_multi$epoch_scores)) {
  cat(sprintf(
    "Epoch %d: Mean = %.3f\n",
    i,
    mean(result_multi$epoch_scores[[i]], na.rm = TRUE)
  ))
}

cat(sprintf("\nOverall Mean: %.3f\n", result_multi$mean_score))
cat(sprintf("Standard Deviation: %.3f\n", result_multi$score_std))
cat(sprintf(
  "95%% CI: [%.3f, %.3f]\n",
  result_multi$ci_95[1],
  result_multi$ci_95[2]
))

# Use epochs with eval_program for optimization
ctrl <- optimizer_control(progress = TRUE)
eval_result <- eval_program(
  mod,
  dataset,
  metric = metric,
  control = ctrl,
  epochs = 5L
)

print(eval_result)
