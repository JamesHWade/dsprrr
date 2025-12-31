# Evaluate a Compiled Module

Evaluate the performance of a compiled module on a test dataset.

## Usage

``` r
evaluate_dsp(module, data, metric, .llm = NULL, verbose = TRUE)
```

## Arguments

- module:

  A DSPrrr module (compiled or not)

- data:

  Test data as a data frame or tibble.

- metric:

  A metric function from `metric_*()` functions

- .llm:

  Optional LLM connection for running the module

- verbose:

  Whether to show progress

## Value

A list with evaluation results including mean score and per-example
scores

## Examples

``` r
if (FALSE) { # \dontrun{
# Evaluate a module
results <- evaluate_dsp(
  module = optimized_classifier,
  data = test_data,
  metric = metric_exact_match(field = "sentiment"),
  .llm = llm_connection
)

print(results$mean_score)
} # }
```
