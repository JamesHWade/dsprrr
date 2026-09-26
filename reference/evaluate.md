# Score a module on a dataset

`evaluate()` runs a module on every row of a data frame (as
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
does), scores each output with a metric, and returns the mean score
together with the per-row scores, predictions and errors.

## Usage

``` r
evaluate(module, ...)
```

## Arguments

- module:

  A module, such as one created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  or
  [`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md).

- ...:

  Arguments for the evaluation:

  - `data` (required, second argument): a data frame with one column per
    signature input, plus the columns the metric compares against.

  - `metric` (required): a function called as
    `metric(prediction, expected)` for each row, where `prediction` is
    the row's output (a named list, as returned by
    [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)) and
    `expected` is the whole data row as a one-row data frame. It returns
    a logical or numeric score, or `list(score = , feedback = )`.
    Built-in metrics take `field` to name the output and column to
    compare, as in `metric_exact_match(field = "sentiment")`. Metrics
    made with
    [`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md)
    also receive the row's execution trace.

  - `epochs`: how many times to run every row (default `1L`). Epochs
    after the first use their own cache partition, so each samples fresh
    responses. Repeating the same `evaluate()` call replays all epochs
    from the cache; pass `.cache = FALSE` to sample again.

  - `.cache`: `NULL` (the default) follows
    [`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md);
    `FALSE` skips the response cache.

  - `.llm`, `.concurrency`, `.progress`, `.trace_context`: as in
    [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md).

  - `.return_format`: `"structured"` (the default) or `"simple"`, which
    leaves out `metadata`, `traces`, `epoch_traces` and `data`.

## Value

A list of class `dsprrr_evaluation` with:

- `mean_score`: the mean score, with failed rows counted as 0. With
  `epochs > 1`, the mean over every row and epoch.

- `scores`: one score per row (logical scores become 0 or 1), `NA` for
  failed rows. With `epochs > 1`, each row's mean across epochs, `NA` if
  any epoch failed.

- `predictions`: one output per row.

- `n_evaluated`: rows with a score.

- `n_errors`: rows where the call or the metric failed, with the
  messages in `errors`. `n_run_errors`/`run_errors` and
  `n_metric_errors`/`metric_errors` separate the two kinds.

- `total_cost`: total cost of the model calls, `NA` when any cost is
  unknown.

- `feedbacks`: the feedback text from metrics that return
  `list(score = , feedback = )` (see
  [`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md)),
  otherwise `NA`.

- `program_artifact_id`, `trace_context`: the program's identity and the
  caller's correlation context.

- `metadata`: one list of call metadata per row.

- `traces`: one execution trace per row, as passed to trace-aware
  metrics. Traces can contain prompts, inputs and responses.

- `data`: the result of `run_dataset(.return_format = "structured")`.

With `epochs > 1`, the list also has `epoch_scores` (one score vector
per epoch), `score_std` (the standard deviation of the epoch means),
`ci_95` (a 95% confidence interval for `mean_score`) and `epoch_traces`;
the `predictions`, `metadata`, `traces` and `data` come from the last
epoch. An empty `data` gives a warning and an `NA` mean score.

## Details

A row whose call fails, or whose metric errors, gets an `NA` score and
is counted in `n_errors`; `mean_score` counts it as 0, so failures lower
the score instead of disappearing from it. Metric errors also raise a
warning.

## See also

[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
and
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
which use `evaluate()` to compare candidate programs.

Other execution:
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md),
[`predict.Module()`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md),
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md),
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)

Other metrics:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
[`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md),
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
[`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
[`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
# A keyword rule stands in for a model, so this example runs offline
rule <- module_fn(
  "text -> sentiment",
  function(text) {
    if (grepl("love|great", text, ignore.case = TRUE)) "positive" else "negative"
  }
)
testset <- data.frame(
  text = c("I love it!", "Awful.", "Great value", "Not great"),
  sentiment = c("positive", "negative", "positive", "negative")
)

result <- evaluate(rule, testset, metric = metric_exact_match(field = "sentiment"))
result
#> 
#> ── DSPrrr Evaluation Results 
#> ✔ Mean Score: 0.75
#> Evaluated: 4
#> Scores: 1, 1, 1, 0
result$scores
#> [1] 1 1 1 0

# A custom metric gets the prediction and the whole data row
same_label <- function(prediction, expected) {
  prediction$sentiment == expected$sentiment
}
evaluate(rule, testset, metric = same_label)$mean_score
#> [1] 0.75

if (FALSE) { # \dontrun{
classifier <- module(
  signature("text -> sentiment: enum('positive', 'negative')")
)
llm <- ellmer::chat_openai(model = "gpt-6-luna")
result <- evaluate(
  classifier,
  testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = llm
)
result$n_errors

# Run every row three times to see how much the score varies
repeated <- evaluate(
  classifier,
  testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = llm,
  epochs = 3L
)
repeated$ci_95
} # }
```
