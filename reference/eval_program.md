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

- Multi-epoch evaluation for statistical significance (when epochs \> 1)

## Usage

``` r
eval_program(
  program,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  epochs = 1L,
  ...,
  .trace_context = list()
)
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

  An object created by
  [`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
  or `NULL` for defaults.

- epochs:

  Integer; number of times to repeat evaluation for statistical
  significance. Defaults to 1L. When \> 1, computes std and confidence
  intervals.

- ...:

  Additional arguments passed to
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md).

- .trace_context:

  A named JSON-compatible correlation context copied to the returned
  `EvalResult` and all program execution traces.

## Value

An EvalResult object containing:

- `examples`: tibble with per-example row_id, score, error, predicted,
  feedback (textual feedback from feedback-aware metrics, see
  [`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md)),
  and input columns (prefixed with input\_\*)

- `mean_score`: mean score across successful evaluations

- `std_error`: standard error of per-example scores (SD / sqrt(n))

- `n_evaluated`: number of successful evaluations

- `n_errors`: number of failed evaluations

- `total_tokens`: total tokens used

- `total_cost`: total cost in USD

- `total_latency_ms`: total time in milliseconds

- `trace_context`: the validated correlation context for this evaluation

When `epochs > 1`, additional fields:

- `epochs`: number of epochs run

- `epoch_scores`: list of score vectors, one per epoch

- `score_std`: standard deviation of mean scores across epochs

- `ci_lower`, `ci_upper`: 95% confidence interval bounds

## Examples

``` r
if (FALSE) { # \dontrun{
sig <- signature("question -> answer")
mod <- module(sig)

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
