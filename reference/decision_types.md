# Calibrated decision outputs

**\[experimental\]**

Decision outputs ask the model for *probability evidence* instead of a
bare answer, then decode that evidence locally. They mirror the
experimental decision types introduced in DSPy 3.4 (`Noul`, `Score`, and
`Choice`):

- `decision_bool()` for a `type_boolean()` output. The model reports
  P(TRUE), and the output is `TRUE` when that probability reaches
  `threshold`.

- `decision_score()` for an ordered `type_enum()` output (a rubric). The
  model reports a probability for every level. Their
  probability-weighted mean level index is a continuous score, and
  `cuts` map that score to a returned level.

- `decision_choice()` for an unordered `type_enum()` output. The model
  reports a probability for every option, and the option with the
  largest `probability * weight` is returned.

Attach these specifications to a module with
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md).
The signature keeps its ordinary output types, so predictions still
contain a native logical or character value that existing metrics can
compare. The probability evidence is available through
[`decision_evidence()`](https://jameshwade.github.io/dsprrr/reference/decision_evidence.md).

The numeric settings (`threshold`, `cuts`, and `weights`) are never sent
to the model. Changing them re-reads cached evidence without new
provider calls, which is what lets
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md)
fit them cheaply against a metric.

## Usage

``` r
decision_bool(threshold = 0.5, criteria = NULL, description = NULL)

decision_score(cuts = NULL, criteria = NULL, description = NULL)

decision_choice(weights = NULL, criteria = NULL, description = NULL)
```

## Arguments

- threshold:

  Probability in `[0, 1]` at or above which a Boolean decision is
  `TRUE`. Defaults to `0.5`.

- criteria:

  Optional descriptions of the outcomes, sent to the model. For
  `decision_bool()`, a list or character vector with elements named
  `true` and/or `false`. For `decision_score()`, one description per
  level, in level order. For `decision_choice()`, descriptions named by
  option.

- description:

  The question the model answers for this field. When `NULL`, the output
  type's own description is used. One of the two is required, because a
  decision needs an explicit question.

- cuts:

  Increasing boundaries on the continuous score, which runs from `0`
  (first level) to `N - 1` (last level). There must be `N - 1` cuts,
  each strictly inside that range. `NULL` (the default) uses the
  midpoints `0.5, 1.5, ...`, which select the level nearest the score.

- weights:

  Named non-negative multipliers for the option probabilities. Omitted
  options use `1`. A zero weight disables an option. `NULL` (the
  default) weights every option equally.

## Value

A `dsprrr_decision_spec` object for use with
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md).

## See also

Other decisions:
[`ReAnchor()`](https://jameshwade.github.io/dsprrr/reference/ReAnchor.md),
[`decision_evidence()`](https://jameshwade.github.io/dsprrr/reference/decision_evidence.md),
[`decision_settings()`](https://jameshwade.github.io/dsprrr/reference/decision_settings.md),
[`with_decisions()`](https://jameshwade.github.io/dsprrr/reference/with_decisions.md)

## Examples

``` r
decision_bool(threshold = 0.7, criteria = c(true = "Service blocked"))
#> <dsprrr_decision_spec> bool decision
#> threshold: 0.7
decision_score(criteria = c("Cosmetic", "Degraded", "Outage"))
#> <dsprrr_decision_spec> score decision
decision_choice(weights = c(other = 0.5))
#> <dsprrr_decision_spec> choice decision
#> weights: other = 0.5
```
