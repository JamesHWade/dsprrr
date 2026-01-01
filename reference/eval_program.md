# Evaluate a Program on a Dataset

Standard evaluation function for optimizers. Executes a module on a
dataset, applies a metric to each example, and returns detailed
per-example results plus aggregated statistics.

This is the core evaluation function used by all optimizers. It wraps
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
with enhanced output including:

- Per-example timing and error information

- Aggregated cost tracking

- Standard error computation

## Usage

``` r
eval_program(program, dataset, metric, .llm = NULL, control = NULL, ...)
```

## Arguments

- program:

  A DSPrrr module to evaluate.

- dataset:

  A data frame containing test examples.

- metric:

  A metric function for scoring predictions.

- .llm:

  Optional ellmer Chat object for LLM calls.

- control:

  An OptimizerControl object or NULL for defaults.

- ...:

  Additional arguments passed to
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md).

## Value

An EvalResult object containing:

- `examples`: tibble with per-example inputs, expected, predicted,
  score, error, latency

- `mean_score`: mean score across successful evaluations

- `std_error`: standard error of the mean

- `n_evaluated`: number of successful evaluations

- `n_errors`: number of failed evaluations

- `total_tokens`: total tokens used

- `total_cost`: total cost in USD

- `total_latency_ms`: total time in milliseconds

## Examples

``` r
if (FALSE) { # \dontrun{
sig <- signature("question -> answer")
mod <- module(sig, type = "predict")

dataset <- tibble::tibble(
  question = c("What is 2+2?", "What is 3+3?"),
  answer = c("4", "6")
)

result <- eval_program(
  mod,
  dataset,
  metric = metric_exact_match(field = "answer"),
  .llm = ellmer::chat_openai()
)

result@mean_score
result@examples
} # }
```
