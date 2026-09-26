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
  `metric_exact_match(field = "match")`. Required.

- metric_threshold, max_errors:

  Inherited teleprompter settings. They are not used by `ReAnchor`.

- fields:

  Optional character vector naming the output fields to calibrate.
  `NULL` (the default) calibrates every compatible field.

- folds:

  Integer maximum number of folds for the held-out acceptance check
  (default `5L`, at least `2L`).

- max_candidates:

  Integer maximum number of candidate settings tried per search step
  (default `40L`, at least `3L`).

## Value

A `ReAnchor` object to pass to
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md).

## Details

Compilation runs in four steps.

1.  Baseline: the module runs on `trainset` as given, and the metric
    scores each row.

2.  Evidence: every compatible output is switched to evidence decoding.
    That includes fields already configured with
    [`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md),
    plus described `type_boolean()` and `type_enum()` outputs, which
    become
    [`decision_bool()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md)
    and
    [`decision_choice()`](https://jameshwade.github.io/dsprrr/reference/decision_types.md)
    decisions. The module runs once more and records the probabilities
    behind each decision.

3.  Fitting: every setting is searched locally against that recorded
    evidence, so the search makes no further provider calls. A setting
    anywhere between two neighboring observed values makes the same
    decisions, so the candidates are the midpoints of those gaps:
    between P(TRUE) values for a threshold, between mean level indexes
    for a cut, and between the points where an option's pick flips for a
    weight (on a log scale). At most `max_candidates` gaps are tried per
    step, thinned to evenly spaced quantiles. Among equal scores, the
    candidate in the widest gap wins.

4.  Acceptance: a new setting replaces the current one only if it scores
    strictly better and passes a fold check. The check splits `trainset`
    into up to `folds` parts. For each part, it picks a setting on the
    other parts and scores that pick on the held-out part. The combined
    held-out score must beat the current setting's. The fully fitted
    module must pass the same check against the step 1 baseline.
    Otherwise the original decision configuration is restored unchanged.

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

Other teleprompters:
[`AutoResearch()`](https://jameshwade.github.io/dsprrr/reference/AutoResearch.md),
[`BetterTogether()`](https://jameshwade.github.io/dsprrr/reference/BetterTogether.md),
[`BootstrapFewShot()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShot.md),
[`BootstrapFewShotWithRandomSearch()`](https://jameshwade.github.io/dsprrr/reference/BootstrapFewShotWithRandomSearch.md),
[`COPRO()`](https://jameshwade.github.io/dsprrr/reference/COPRO.md),
[`GEPA()`](https://jameshwade.github.io/dsprrr/reference/GEPA.md),
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`KNNFewShot()`](https://jameshwade.github.io/dsprrr/reference/KNNFewShot.md),
[`LabeledFewShot()`](https://jameshwade.github.io/dsprrr/reference/LabeledFewShot.md),
[`MIPROv2()`](https://jameshwade.github.io/dsprrr/reference/MIPROv2.md),
[`MetaHarness()`](https://jameshwade.github.io/dsprrr/reference/MetaHarness.md),
[`Omni()`](https://jameshwade.github.io/dsprrr/reference/Omni.md),
[`SIMBA()`](https://jameshwade.github.io/dsprrr/reference/SIMBA.md),
[`Teleprompter()`](https://jameshwade.github.io/dsprrr/reference/Teleprompter.md),
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)

Other decisions:
[`decision_evidence()`](https://jameshwade.github.io/dsprrr/reference/decision_evidence.md),
[`decision_settings()`](https://jameshwade.github.io/dsprrr/reference/decision_settings.md),
[`decision_types`](https://jameshwade.github.io/dsprrr/reference/decision_types.md),
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md)

## Examples

``` r
ReAnchor(metric = metric_exact_match(field = "match"), folds = 3L)
#> <dsprrr::ReAnchor>
#>  @ metric          : function (prediction, expected)  
#>  .. - attr(*, "field")= chr "match"
#>  @ metric_threshold: NULL
#>  @ max_errors      : int 5
#>  @ fields          : NULL
#>  @ folds           : int 3
#>  @ max_candidates  : int 40

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
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
decision_settings(tuned)
optimization_result(tuned)$extensions$re_anchor
} # }
```
