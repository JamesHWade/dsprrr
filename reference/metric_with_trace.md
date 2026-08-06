# Create a Trace-Aware Metric

Wraps a metric so it can score both what a program returned and how the
result was produced. Trace-aware metrics receive a third `program_trace`
argument containing the row and epoch identifiers, the module's ordered
execution events, and per-row metadata. They work with
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
and every optimizer that delegates to it, including
[GEPA](https://jameshwade.github.io/dsprrr/reference/GEPA.md).

This makes quality-efficiency objectives explicit. For example, a metric
can penalize excessive token use, latency, iterations, or tool calls
while still returning textual feedback for reflective optimizers.

## Usage

``` r
metric_with_trace(fn, field = NULL)
```

## Arguments

- fn:

  A function with signature
  `function(prediction, expected, program_trace)`. It must return a
  numeric or logical score, or `list(score = , feedback = )`. An
  explicit `program_trace` formal (including after `...`) is matched by
  name; otherwise the trace is supplied as the third positional
  argument. Any additional formals must have defaults.

- field:

  Optional expected-output column name, stored like the `field`
  attribute on built-in metrics.

## Value

A metric function classed as `dsprrr_trace_metric`.

## Examples

``` r
metric <- metric_with_trace(function(prediction, expected, program_trace) {
  correct <- identical(prediction$answer, expected$answer)
  tokens <- program_trace$metadata$total_tokens
  if (is.null(tokens)) tokens <- 0
  as.numeric(correct) - min(tokens / 10000, 0.1)
}, field = "answer")

# evaluate(module, data, metric, .llm = llm)
```
