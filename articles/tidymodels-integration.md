# Use dsprrr with tidymodels

dsprrr integrates with tidymodels in three places: scoring module
predictions with `yardstick`, building parameter grids for
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
with `dials`, and wrapping zero-shot prediction modules in the
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md)
`parsnip` model specification.

``` r

library(dsprrr)
```

## Score predictions with yardstick

Run the module over a data frame with
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
which keeps every column and adds a `result` list-column. Turn the
predicted field into a factor with the truth’s levels, and any
`yardstick` metric works:

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
matters: `yardstick` refuses a prediction factor whose levels differ
from the truth’s, which happens whenever some class is never predicted.

## Build grids with dials

[`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md),
[`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md), and
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md)
are `dials` parameter objects
([`temperature()`](https://jameshwade.github.io/dsprrr/reference/temperature.md)
and [`top_p()`](https://jameshwade.github.io/dsprrr/reference/top_p.md)
default to `[0, 1]`):

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

[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md)
suggests a `dials` parameter set for a module, swapping
`temperature`/`top_p` for `reasoning_effort` when `model` looks like a
reasoning model (pass `include` to pick only the knobs you want):

``` r

classify <- module(
  signature("text -> sentiment: enum('negative', 'neutral', 'positive')")
)
knobs <- c("temperature", "top_p", "reasoning_effort")
module_parameters(classify, model = "claude-sonnet-4-5", include = knobs)
#> Collection of 2 parameters for tuning
#> 
#>   identifier        type    object
#>  temperature temperature dparam[+]
#>        top_p       top_p dparam[+]
#> 
module_parameters(classify, model = "gpt-6-luna", include = knobs)
#> Collection of 1 parameters for tuning
#>        identifier             type    object
#>  reasoning_effort reasoning_effort dparam[+]
#> 
```

[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
accepts a `dials` grid directly, or a
[`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
set to build one from. It evaluates each row and keeps the best settings
on the module:

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

To tune how much a reasoning model thinks instead, sweep a
[`reasoning_effort()`](https://jameshwade.github.io/dsprrr/reference/reasoning_effort.md)
grid; see [Models, providers and
streaming](https://jameshwade.github.io/dsprrr/articles/models-and-providers.md).

## The parsnip model

When `parsnip` is loaded, dsprrr registers a model type,
[`llm_predict()`](https://jameshwade.github.io/dsprrr/reference/llm_predict.md),
with the `"dsprrr"` engine for classification and regression:

``` r

library(parsnip)

reviews <- tibble::tibble(
  text = c("Arrived broken.", "Works great.", "It's a chair."),
  sentiment = factor(c("negative", "positive", "neutral"))
)

spec <- llm_predict(
  mode = "classification",
  signature = "text -> sentiment: enum('negative', 'neutral', 'positive')",
  temperature = 0
) |>
  set_engine("dsprrr")

fitted <- fit_xy(spec, x = reviews["text"], y = reviews$sentiment)
fitted
#> parsnip model object
#> ── PredictModule ──
#> 
#> ── Signature
#> 
#> ── Signature ──
#> 
#> ── Inputs
#> • text: "string" - Input: text
#> 
#> ── Output
#> Type: "object(sentiment: enum(negative, neutral, positive))"
#> 
#> ── Instructions
#> Given the fields `text`, produce the fields `sentiment`.
```

Fitting builds a zero-shot module from `signature` (or, without one,
derives it from the predictor names and outcome levels), stores
`temperature` and `top_p` in the module’s config, and makes no model
calls. Use
[`fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html) (or
[`workflows::add_variables()`](https://workflows.tidymodels.org/reference/add_variables.html))
rather than a formula for free-text columns: R’s formula interface
converts character predictors into factors with fixed training levels,
which rejects unseen text at prediction time.

[`predict()`](https://rdrr.io/r/stats/predict.html) runs the module on
`new_data` using the default Chat (set with
[`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md)
or
[`local_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md))
and returns `.pred_class` (preserving the training factor’s levels) or
`.pred`:

``` r

set_default_chat(
  ellmer::chat_openai(
    model = "gpt-6-luna",
    params = ellmer::params(reasoning_effort = "none")
  )
)
predict(fitted, new_data = tibble::tibble(text = c("Best purchase ever!", "Total junk.")))
#> # A tibble: 2 x 1
#>   .pred_class
#>   <fct>
#> 1 positive
#> 2 negative
```

For evaluation with graded or repeated samples, see [Evaluate with
vitals](https://jameshwade.github.io/dsprrr/articles/vitals-recipes.md).
