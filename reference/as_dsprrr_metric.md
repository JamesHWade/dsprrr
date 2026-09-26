# Use a vitals scorer as a dsprrr metric

`as_dsprrr_metric()` turns a vitals scorer into a per-example metric
that
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
and the optimizers can call. Each call builds a one-row vitals sample
from the prediction and the data row, runs the scorer, and converts its
grade to a number.

## Usage

``` r
as_dsprrr_metric(
  vitals_scorer,
  input_column = "input",
  target_column = "target",
  result_column = "result"
)
```

## Arguments

- vitals_scorer:

  A scorer function that takes a samples tibble and returns a list (or
  data frame) with a `score` element, such as
  [`vitals::detect_includes()`](https://vitals.tidyverse.org/reference/scorer_detect.html).

- input_column:

  Name of the input column, in both the data and the sample (default
  `"input"`).

- target_column:

  Name of the expected-answer column, in both the data and the sample
  (default `"target"`).

- result_column:

  Name of the sample column that receives the prediction (default
  `"result"`).

## Value

A metric function `function(prediction, expected_row)` that returns a
number, usually in `[0, 1]`, or `NA`.

## Details

The sample has three columns. The `input_column` and `target_column`
columns are copied from the data column of the same name (`NA` when the
row has no such column), and `result_column` holds the prediction.
vitals' built-in scorers read the columns `input`, `target` and
`result`, so keep the defaults with them and give your data a `target`
column (and an `input` column for model-graded scorers). The other names
are for scorers of your own that read different columns.

Grades are converted as follows: numbers are kept, `TRUE`/`FALSE` become
1/0, and `"C"`/`"correct"`/`"pass"`, `"I"`/`"incorrect"`/`"fail"` and
`"P"`/`"partial"` become 1, 0 and 0.5. Anything else gives `NA` with a
warning.

## See also

Other integrations:
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md),
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
[`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md),
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md),
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md),
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md),
[`register_dsprrr_engine()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_engine.md),
[`summarize_traces_df()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces_df.md),
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

Other metrics:
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
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
# A scorer written in the vitals style
exact_match <- function(samples) {
  list(score = as.numeric(samples$result[[1]] == samples$target[[1]]))
}
metric <- as_dsprrr_metric(exact_match)
metric("yes", data.frame(input = "Continue?", target = "yes"))
#> [1] 1

# A vitals scorer; the data needs a `target` column
includes <- as_dsprrr_metric(vitals::detect_includes())
includes("The capital is Paris.", data.frame(target = "Paris"))
#> [1] 1
```
