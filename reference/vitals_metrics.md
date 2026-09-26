# Metrics built on vitals scorers

These functions wrap vitals scorers with
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)
so they can be used directly as dsprrr metrics:

- `metric_model_graded_qa()` and `metric_model_graded_fact()` ask a
  model to grade the answer against the target.

- `metric_detect_match()`, `metric_detect_includes()` and
  `metric_detect_pattern()` compare strings without a model.

## Usage

``` r
metric_model_graded_qa(
  template = NULL,
  instructions = NULL,
  grade_pattern = "(?i)GRADE\\s*:\\s*([CPI])(.*)$",
  partial_credit = FALSE,
  scorer_chat = NULL,
  input_column = "input",
  target_column = "target",
  result_column = "result"
)

metric_model_graded_fact(
  template = NULL,
  instructions = NULL,
  grade_pattern = "(?i)GRADE\\s*:\\s*([CPI])(.*)$",
  partial_credit = FALSE,
  scorer_chat = NULL,
  input_column = "input",
  target_column = "target",
  result_column = "result"
)

metric_detect_match(
  location = c("end", "begin", "any", "exact"),
  case_sensitive = FALSE,
  input_column = "input",
  target_column = "target",
  result_column = "result"
)

metric_detect_includes(
  case_sensitive = FALSE,
  input_column = "input",
  target_column = "target",
  result_column = "result"
)

metric_detect_pattern(
  pattern,
  case_sensitive = FALSE,
  all = FALSE,
  input_column = "input",
  target_column = "target",
  result_column = "result"
)
```

## Arguments

- template:

  Grading prompt template, a glue string with `input`, `answer`,
  `criterion` and `instructions` fields. `NULL` uses the vitals default.

- instructions:

  Grading instructions. `NULL` uses the vitals default.

- grade_pattern:

  Regular expression that extracts the grade from the grader's reply.

- partial_credit:

  Whether the grader may award partial credit (0.5).

- scorer_chat:

  The ellmer Chat that grades, such as
  `ellmer::chat_openai(model = "gpt-6-luna")`. Required; see Details.

- input_column, target_column, result_column:

  Column names passed to
  [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md).
  Keep the defaults for vitals scorers.

- location:

  Where to look for the target in the result: `"end"` (the default),
  `"begin"`, `"any"` or `"exact"`.

- case_sensitive:

  Whether matching is case-sensitive (default `FALSE`).

- pattern:

  Regular expression with capture groups, such as `"([0-9]+)"`. The
  captured text is compared with the target.

- all:

  Whether every captured group must match the target (`TRUE`) or at
  least one (`FALSE`, the default).

## Value

A metric function `function(prediction, expected_row)`.

## Details

The underlying vitals scorers read the expected answer from a `target`
column, and the graded metrics also show the grader the question from an
`input` column. Add those columns to your data, for example
`transform(data, input = question, target = answer)`, and keep the
default column arguments; see
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md).

The graded metrics need `scorer_chat`: outside a vitals `Task`, there is
no solver chat for vitals to fall back on.

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
[`metric_with_trace()`](https://jameshwade.github.io/dsprrr/reference/metric_with_trace.md)

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
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
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)

## Examples

``` r
# String comparisons need no model
ends_with_answer <- metric_detect_match(location = "end")
ends_with_answer("The answer is Paris", data.frame(target = "Paris"))
#> [1] 1

mentions <- metric_detect_includes()
mentions("Paris is the capital", data.frame(target = "Paris"))
#> [1] 1

number <- metric_detect_pattern("([0-9]+)")
number("The total is 42", data.frame(target = "42"))
#> [1] 1

if (FALSE) { # \dontrun{
# Model-graded metrics call scorer_chat once per example
graded <- metric_model_graded_qa(
  scorer_chat = ellmer::chat_openai(model = "gpt-6-luna")
)
graded(
  "Paris",
  data.frame(input = "What is the capital of France?", target = "Paris")
)

fact <- metric_model_graded_fact(
  scorer_chat = ellmer::chat_anthropic(model = "claude-sonnet-4-5"),
  partial_credit = TRUE
)
} # }
```
