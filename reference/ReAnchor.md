# ReAnchor: calibrate decision outputs against a metric

**\[experimental\]**

`ReAnchor` fits the numeric settings of a module's decision outputs
(Boolean `threshold`s, Score `cuts`, and Choice `weights`; see
[decision_types](https://jameshwade.github.io/dsprrr/reference/decision_types.md))
against a metric. It mirrors the experimental `ReAnchor` optimizer in
DSPy 3.4. It never changes instructions, demonstrations, or the
questions sent to the model.

## Usage

``` r
ReAnchor(
  metric = NULL,
  metric_threshold = NULL,
  max_errors = 5L,
  fields = NULL,
  folds = 5L,
  max_candidates = 40L
)
```

## Arguments

- metric:

  A metric function `function(prediction, expected_row)`, such as
  [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md).
  Required.

- metric_threshold, max_errors:

  Inherited teleprompter settings. They are not used by `ReAnchor`.

- fields:

  Optional character vector naming the output fields to calibrate.
  `NULL` (the default) calibrates every compatible field.

- folds:

  Maximum number of folds for the held-out acceptance check.

- max_candidates:

  Maximum number of candidate settings tried per search step.

## Value

A `ReAnchor` teleprompter for use with
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

Compilation runs in four steps.

1.  **Baseline.** The module runs on `trainset` as given, and the metric
    scores each row.

2.  **Evidence.** Every compatible output is switched to evidence
    decoding. That includes fields already configured with
    [`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md),
    plus described
    [`type_boolean()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
    and
    [`type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html)
    outputs, which become
    [`decision_bool()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md)
    and
    [`decision_choice()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md)
    decisions. The module runs once more and records the probabilities
    behind each decision.

3.  **Fitting.** Every setting is searched locally against that recorded
    evidence, so the search makes **no further provider calls**. A
    setting anywhere between two neighbouring observed values makes the
    same decisions, so the candidates are the midpoints of those gaps:
    between P(TRUE) values for a threshold, between mean level indexes
    for a cut, and between the points where an option's pick flips for a
    weight (on a log scale). At most `max_candidates` gaps are tried per
    step, thinned to evenly spaced quantiles. Among equal scores, the
    candidate in the widest gap wins.

4.  **Acceptance.** A new setting replaces the current one only if it
    scores strictly better *and* passes a fold check. The check splits
    `trainset` into up to `folds` parts. For each part, it picks a
    setting on the other parts and scores that pick on the held-out
    part. The combined held-out score must beat the current setting's.
    The fully fitted module must pass the same check against the step 1
    baseline. Otherwise the original decision configuration is restored
    unchanged.

`valset`, when given, is scored before and after calibration for the
report and is never used for fitting.

The fit is exact for a single Predict module, because the decoded
outputs depend only on the recorded evidence. Pipelines and other
composite programs are rejected, because an upstream decision can change
the requests made downstream.

The report is stored in the compiled module's optimization result:
`optimization_result(compiled)$extensions$re_anchor`. It contains the
train (and validation) scores before and after calibration, whether the
fitted settings were accepted, and one entry per field with the fitted
value, the number of candidates tried, and the fold-check outcomes. Use
[`decision_settings()`](https://jameshwade.github.io/dsprrr/reference/decision_settings.md)
to see the resulting settings.

## See also

[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md),
[`decision_settings()`](https://jameshwade.github.io/dsprrr/reference/decision_settings.md),
[`decision_evidence()`](https://jameshwade.github.io/dsprrr/reference/decision_evidence.md)

## Examples

``` r
if (FALSE) { # \dontrun{
sig <- signature(
  inputs = list(input("pair", description = "Two product listings")),
  output_type = ellmer::type_object(
    match = ellmer::type_boolean("Do the listings describe the same item?")
  )
)
matcher <- module(sig) |> with_decisions(match = decision_bool())

tuned <- compile(
  matcher,
  ReAnchor(metric = metric_exact_match(field = "match")),
  trainset,
  valset = valset,
  .llm = ellmer::chat_openai(model = "gpt-4.1-mini")
)
decision_settings(tuned)
optimization_result(tuned)$extensions$re_anchor
} # }
```
