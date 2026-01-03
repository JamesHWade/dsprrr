# Pre-built Vitals-backed Metrics

These functions wrap common vitals scorers for direct use as dsprrr
metrics, eliminating the need to manually call
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md).

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

  Grading template (glue string with `input`, `answer`, `criterion`,
  `instructions` substitutions)

- instructions:

  Grading instructions

- grade_pattern:

  Regex pattern to extract grade from judge response

- partial_credit:

  Whether to allow partial credit

- scorer_chat:

  An ellmer chat for grading (e.g.,
  [`ellmer::chat_openai()`](https://ellmer.tidyverse.org/reference/chat_openai.html))

- input_column:

  Column name for input in vitals sample

- target_column:

  Column name for target in vitals sample

- result_column:

  Column name for result in vitals sample

- location:

  Where to look for the target in the result: "end", "begin", "any", or
  "exact"

- case_sensitive:

  Whether matching is case-sensitive

- pattern:

  Regex pattern with capture groups. The captured groups are extracted
  from the result and checked against the target. Use parentheses to
  define capture groups, e.g., `"([0-9]+)"` to extract numbers.

- all:

  Whether all captured groups must match the target (TRUE) or just one
  (FALSE, default).

## Value

A metric function with signature `function(prediction, expected_row)`

## Examples

``` r
if (FALSE) { # \dontrun{
# Model-graded QA metric
metric <- metric_model_graded_qa(scorer_chat = ellmer::chat_openai())
score <- metric("Paris", data.frame(target = "Paris"))

# With custom grading chat
metric <- metric_model_graded_fact(
  scorer_chat = ellmer::chat_claude(),
  partial_credit = TRUE
)
} # }
if (FALSE) { # \dontrun{
# String detection metrics
metric <- metric_detect_match(location = "end")
metric("The answer is Paris", data.frame(target = "Paris"))  # 1

metric <- metric_detect_includes()
metric("Paris is the capital", data.frame(target = "Paris"))  # 1
} # }
```
