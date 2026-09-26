# Metric that also sees the execution trace

`metric_with_trace()` makes a metric that scores both what a program
returned and how it got there. Besides the prediction and the expected
row, the function receives a `program_trace`: the row and epoch numbers,
a `status` (`"ok"`, `"error"` or `"untraced"`), the module's execution
`events` in order, and the row's call `metadata` (tokens, cost,
latency). Use it to penalize token use, latency, iterations or tool
calls alongside correctness.

Trace-aware metrics work with
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and the optimizers that use it, including
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md).
Called directly without a trace, they are an error.

## Usage

``` r
metric_with_trace(fn, field = NULL)
```

## Arguments

- fn:

  A function called as `fn(prediction, expected, program_trace)`. It
  returns a logical or numeric score, or `list(score = , feedback = )`.
  A formal argument named `program_trace` receives the trace by name
  (even after `...`); otherwise the trace is the third positional
  argument. Any other arguments need defaults.

- field:

  The name of the data column that holds the expected output, stored in
  the metric's `"field"` attribute for optimizers.

## Value

A metric function of class `dsprrr_trace_metric`.

## See also

Other metrics:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`metric_contains()`](https://jameshwade.github.io/dsprrr/reference/metric_contains.md),
[`metric_custom()`](https://jameshwade.github.io/dsprrr/reference/metric_custom.md),
[`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md),
[`metric_f1()`](https://jameshwade.github.io/dsprrr/reference/metric_f1.md),
[`metric_field_match()`](https://jameshwade.github.io/dsprrr/reference/metric_field_match.md),
[`metric_threshold()`](https://jameshwade.github.io/dsprrr/reference/metric_threshold.md),
[`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
# Correctness, minus up to 0.1 for token use
efficient <- metric_with_trace(
  function(prediction, expected, program_trace) {
    correct <- identical(prediction$answer, expected$answer)
    tokens <- program_trace$metadata$total_tokens
    if (is.null(tokens) || is.na(tokens)) tokens <- 0
    as.numeric(correct) - min(tokens / 10000, 0.1)
  },
  field = "answer"
)

# A function-backed module records no tokens, so only correctness counts
rule <- module_fn("question -> answer", function(question) "4")
quiz <- data.frame(question = c("2 + 2?", "3 + 3?"), answer = c("4", "6"))
evaluate(rule, quiz, metric = efficient)$scores
#> [1] 1 0
```
