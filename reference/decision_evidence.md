# Probability evidence behind decision outputs

**\[experimental\]**

Extracts the decoded evidence for decision outputs from structured
results.

## Usage

``` r
decision_evidence(x)
```

## Arguments

- x:

  A structured result from `run(..., .return_format = "structured")`, a
  list of such results from a batch call, or a tibble returned by
  `run_dataset(..., .return_format = "structured")` or
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)'s
  `metadata` element.

## Value

A tibble with one row per result and decision field. Columns: `row`,
`field`, `kind`, `value` (the decoded native value, a list-column),
`probability` (P(TRUE) for Boolean decisions), `score` (the continuous
mean level index for Score decisions), `level` (the zero-based selected
level), `confidence`, and `probabilities` (a list-column of named
probabilities for Score and Choice decisions).

For Boolean decisions, `confidence` is the distance from the threshold,
`abs(p - threshold) / max(threshold, 1 - threshold)`, not a calibrated
probability. For Score and Choice decisions it is the model's
self-reported confidence.

## See also

Other decisions:
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`decision_settings()`](https://jameshwade.github.io/dsprrr/reference/decision_settings.md),
[`decision_types`](https://jameshwade.github.io/dsprrr/reference/decision_types.md),
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sig <- signature(
  inputs = list(input("ticket", description = "Customer report")),
  output_type = ellmer::type_object(
    urgent = ellmer::type_boolean("Is the service blocked?")
  )
)
triage <- module(sig) |> with_decisions(urgent = decision_bool())

result <- run(
  triage,
  ticket = "Checkout fails for every customer since 9am.",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .return_format = "structured"
)
decision_evidence(result)
} # }
```
