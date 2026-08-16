# Evaluate a DSPrrr module

Generic evaluation entry point for DSPrrr modules. Executes the module
on a dataset, applies a metric to each example, and returns aggregate
statistics together with the predictions and metadata required for
downstream analysis.

## Usage

``` r
evaluate(module, ...)
```

## Arguments

- module:

  A DSPrrr module created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- ...:

  Arguments passed to methods:

  - `data`: A data frame or tibble containing columns that match the
    module's signature inputs plus any expected fields used by metric.

  - `metric`: A function applied per example with signature
    `metric(prediction, expected_row)`, or a trace-aware metric created
    with
    [`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md).

  Additional arguments passed to
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md):

  - `.llm`: Optional ellmer chat object

  - `.parallel`: Logical; whether to allow parallel execution

  - `.concurrency`: A policy created by
    [`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md).
    Do not also pass `.parallel` when using an explicit policy.

  - `.progress`: Logical; whether to display progress while evaluating

  - `.trace_context`: A named, JSON-compatible list copied into
    evaluation results, row metadata, and traces. Credential-like fields
    and runtime objects are rejected before execution.

  - `.return_format`: Character; `"simple"` returns just scores and
    predictions, `"structured"` (default) includes full metadata and
    data

  - `epochs`: Integer; number of times to repeat evaluation for
    statistical significance (default = 1L). When \> 1, each sample is
    evaluated multiple times to quantify variation.

## Value

A list with elements. When `.return_format = "structured"` (default):

- `mean_score`: numeric mean over all attempted rows, with run or metric
  failures contributing zero. With repeated epochs, the mean covers
  every attempted row-epoch.

- `scores`: per-example numeric scores (coerced from logical metrics).

- `predictions`: list of model outputs.

- `metadata`: list of metadata captured from
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

- `n_evaluated`: number of successful evaluations.

- `n_errors`: number of rows with run or metric failures.

- `errors`: character vector with all error messages, when any.

- `n_run_errors`, `run_errors`: count and messages for module/LLM
  failures.

- `n_metric_errors`, `metric_errors`: count and messages for metric
  failures.

- `total_cost`: total evaluation cost, or `NA` when any call's cost is
  unknown.

- `feedbacks`: per-example textual feedback when the metric returns
  `list(score = , feedback = )` (see
  [`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md));
  `NA` otherwise.

- `traces`: per-example trace envelopes supplied to trace-aware metrics.
  Each contains `row_id`, `epoch`, `status`, ordered module `events`,
  and per-row `metadata`. Trace events can contain prompts, inputs, and
  model responses, so treat them as potentially sensitive.

- `program_artifact_id`, `trace_context`: the executable program
  identity and caller-supplied correlation context.

- `data`: input data augmented with prediction metadata.

When `epochs > 1`, additional fields are included:

- `epoch_scores`: list of numeric vectors, one per epoch

- `epoch_traces`: list of row-aligned trace lists, one per epoch

- `score_std`: standard deviation of mean scores across epochs

- `ci_95`: 95% confidence interval for the mean score (numeric vector of
  length 2)

When `.return_format = "simple"`:

- `mean_score`, `scores`, `predictions`, `n_evaluated`, `n_errors`,
  `errors` (omits `metadata` and `data` for lighter-weight results)

## See also

- [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
  executing without metrics

- [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
  for batch execution without metrics

- [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
  for parameter optimization

- [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
  [`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md)
  for built-in metrics

## Examples

``` r
if (FALSE) { # \dontrun{
classifier <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')"),
  type = "predict"
)

testset <- dsp_trainset(
  text = c("I love it!", "Awful.", "It's fine."),
  sentiment = c("positive", "negative", "neutral")
)

result <- evaluate(
  classifier,
  data = testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = ellmer::chat_openai()
)

result$mean_score # accuracy across the test set
result$scores # per-example scores
result$n_errors # examples where the metric failed
} # }
```
