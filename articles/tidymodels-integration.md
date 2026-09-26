# Use dsprrr with tidymodels

This page shows how dsprrr fits with tidymodels today: score module
output with yardstick, and build grids for
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
with dials. It also explains what the parsnip model
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md)
does, and why it can’t be fitted yet.

``` r

library(dsprrr)
```

## Score predictions with yardstick

Run the module over a data frame with
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
which keeps every column and adds a `result` list column. Turn the
predicted field into a factor with the truth’s levels, and any yardstick
metric works:

``` r

library(yardstick)

reviews <- tibble::tibble(
  text = c("Arrived broken.", "Works great.", "It's a chair."),
  sentiment = factor(c("negative", "positive", "neutral"))
)
classify <- module(
  signature("text -> sentiment: enum('negative', 'neutral', 'positive')")
)

scored <- run_dataset(
  classify,
  reviews,
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
scored$.pred_class <- factor(
  vapply(
    scored$result,
    \(r) if (is.list(r)) r$sentiment else NA_character_,
    character(1)
  ),
  levels = levels(scored$sentiment)
)

accuracy(scored, truth = sentiment, estimate = .pred_class)
conf_mat(scored, truth = sentiment, estimate = .pred_class)
```

A row whose call failed holds `NA` in `result`, hence the
[`is.list()`](https://rdrr.io/r/base/list.html) check. Setting `levels`
matters: yardstick refuses a prediction factor whose levels differ from
the truth’s, which happens whenever some class is never predicted.

## Build grids with dials

[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md) and
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md)
are dials parameter objects.
[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md)
and [`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md)
default to the range 0 to 1:

``` r

temperature()
#> Temperature (quantitative)
#> Range: [0, 1]
reasoning_effort()
#> Reasoning Effort (qualitative)
#> 3 possible values include:
#> 'low', 'medium', and 'high'
dials::grid_regular(temperature(), levels = 3)
#> # A tibble: 3 × 1
#>   temperature
#>         <dbl>
#> 1         0  
#> 2         0.5
#> 3         1
```

[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
takes such a grid directly, or a
[`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
set to build one from. It tries each row on the data and keeps the best
settings in the module:

``` r

optimize_grid(
  classify,
  data = reviews,
  metric = metric_exact_match(field = "sentiment"),
  grid = dials::grid_regular(temperature(), levels = 3),
  # gpt-6-luna accepts temperature only with reasoning turned off
  .llm = ellmer::chat_openai(
    model = "gpt-6-luna",
    params = ellmer::params(reasoning_effort = "none")
  )
)
module_trials(classify)
```

`module_parameters(classify)` suggests a parameter set for a module. To
tune how much a reasoning model thinks, use a
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md)
grid instead; see [Models, providers and
streaming](https://jameshwade.github.io/dsprrr/articles/models-and-providers.md).

## The parsnip model

When parsnip is loaded, dsprrr registers a model type,
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md),
with the engine `"dsprrr"` for classification and regression:

``` r

library(parsnip)

spec <- llm_predict(
  mode = "classification",
  signature = "text -> sentiment: enum('negative', 'neutral', 'positive')",
  temperature = 0
) |>
  set_engine("dsprrr")
spec
#> 
#> ── LLM Predict Model Specification
#> Mode: classification
#> Engine: dsprrr
```

Fitting this model does not learn from the training data. The engine’s
fit step builds a zero-shot module from `signature` (or, without one,
from the predictor names and the outcome’s distinct values, such as
`text -> output: enum('negative', 'positive')`), puts `temperature` and
`top_p` in the module’s config, and records the predictor names. The
training rows are not used as examples.
[`predict()`](https://rdrr.io/r/stats/predict.html) then runs the module
on `new_data` with the default Chat, so call
[`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md)
first. It returns `.pred_class` for classification, as a factor with
only the levels that were predicted, or `.pred` for regression.

With parsnip 1.6.0,
[`fit()`](https://generics.r-lib.org/reference/fit.html) and
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) on this
specification currently stop with “no applicable method for ‘filter’
applied to an object of class”NULL”“, because the engine registers no
predictor encoding. The same failure stops
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html)
and workflows, which fit models through these functions. Until that is
fixed, the module and
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
approach at the top of this page gives the same zero-shot predictions.

For evaluation with graded or repeated samples, see [Evaluate with
vitals](https://jameshwade.github.io/dsprrr/articles/vitals-recipes.md).
