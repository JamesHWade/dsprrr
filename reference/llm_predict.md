# LLM model specification for parsnip

`llm_predict()` creates a parsnip model specification whose "dsprrr"
engine predicts with a dsprrr module, for classification or regression
on text columns. `temperature` and `top_p` can be marked for tuning with
`tune()`; see
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md)
and [`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md).

## Usage

``` r
llm_predict(
  mode = "classification",
  signature = NULL,
  temperature = NULL,
  top_p = NULL
)
```

## Arguments

- mode:

  `"classification"` (the default) or `"regression"`.

- signature:

  Optional signature string or
  [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
  object for the module. `NULL` derives one from the data when fitting.

- temperature:

  Sampling temperature for the module, or `tune()`.

- top_p:

  Nucleus sampling parameter for the module, or `tune()`.

## Value

A parsnip model specification of class `llm_predict`.

## Details

The engine is registered automatically when parsnip is loaded (see
[`register_dsprrr_engine()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_engine.md)).
Fitting builds a Predict module from the predictor columns and the
outcome: an enum output with the outcome's levels for classification, or
a number for regression, unless you give a `signature`. Prediction runs
the module on the new data with the default chat (see
[`dsp_configure()`](https://jameshwade.github.io/dsprrr/reference/dsp_configure.md));
a fitted model makes no model calls until it predicts.

Known issue: the engine is registered without parsnip's encoding
information, so
[`parsnip::fit()`](https://generics.r-lib.org/reference/fit.html) and
[`parsnip::fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
currently fail with "no applicable method for 'filter'". Until that is
fixed, the specification cannot be fitted through parsnip, workflows or
tune.

## See also

Other integrations:
[`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md),
[`as_dsprrr_traces()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_traces.md),
[`as_ellmer_tool()`](https://jameshwade.github.io/dsprrr/reference/as_ellmer_tool.md),
[`as_vitals_cost()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_cost.md),
[`as_vitals_samples()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_samples.md),
[`as_vitals_solver()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_solver.md),
[`as_vitals_task()`](https://jameshwade.github.io/dsprrr/reference/as_vitals_task.md),
[`create_search_tool()`](https://jameshwade.github.io/dsprrr/reference/create_search_tool.md),
[`ragnar_tool()`](https://jameshwade.github.io/dsprrr/reference/ragnar_tool.md),
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md),
[`register_dsprrr_engine()`](https://jameshwade.github.io/dsprrr/reference/register_dsprrr_engine.md),
[`summarize_traces_df()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces_df.md),
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md),
[`vitals_metrics`](https://jameshwade.github.io/dsprrr/reference/vitals_metrics.md)

## Examples

``` r
spec <- llm_predict(
  mode = "classification",
  signature = "text -> sentiment: enum('positive', 'negative', 'neutral')"
) |>
  parsnip::set_engine("dsprrr")
spec
#> 
#> ── LLM Predict Model Specification 
#> Mode: classification
#> Engine: dsprrr

# Mark a parameter for tuning
llm_predict(signature = "text -> sentiment", temperature = parsnip::tune())
#> 
#> ── LLM Predict Model Specification 
#> Mode: classification
```
