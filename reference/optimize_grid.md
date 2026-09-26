# Grid search over module settings

`optimize_grid()` evaluates a module once for every row of a grid of
settings, such as different `reasoning_effort` values or instructions,
and applies the best-scoring row to the module. It modifies the module
in place, unlike
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
which returns a new program.

## Usage

``` r
optimize_grid(module, ...)

# S3 method for class 'Module'
optimize_grid(
  module,
  data,
  metric = metric_exact_match(),
  grid = NULL,
  parameters = NULL,
  objective = c("maximize", "minimize"),
  .llm = NULL,
  control = list(),
  ...
)
```

## Arguments

- module:

  A module, such as one created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- ...:

  Further arguments passed to
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
  such as `.concurrency = concurrency_control(max_active = 4L)`.

- data:

  A data frame with the signature's input columns and the columns the
  metric compares.

- metric:

  A metric function called as `metric(prediction, expected)`. The
  default,
  [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md)
  without a `field`, compares the one output field that also names a
  column of `data`; pass `field` to choose the column explicitly. Use
  [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)
  to adapt a vitals scorer.

- grid:

  A data frame with one row per candidate, or a named list that is
  expanded like `parameters`.

- parameters:

  Used when `grid` is `NULL`: a named list of values to cross, or a
  tidymodels parameter set such as the one returned by
  [`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md).

- objective:

  `"maximize"` (the default) keeps the highest mean score; `"minimize"`
  keeps the lowest.

- .llm:

  Optional ellmer Chat used for every evaluation.

- control:

  A named list of options: `progress` (a progress bar over candidates;
  default [`interactive()`](https://rdrr.io/r/base/interactive.html)),
  `evaluation_progress` (a bar inside each evaluation; default `FALSE`),
  and, for tidymodels parameter sets, `grid_type` (`"regular"`, the
  default, or `"random"`), `grid_levels` (levels per parameter in a
  regular grid; default `3L`) and `grid_size` (candidates in a random
  grid; default `max(10L, grid_levels)`). A `parallel` entry is accepted
  but has no effect; pass `.concurrency` through `...` to evaluate rows
  concurrently.

## Value

The module, modified in place: the best row's settings are applied and
the trials are recorded. When no candidate produces a score, the
settings are left unchanged and a warning is raised.

## Details

Give the candidates as `grid`, a data frame with one row per candidate,
or as `parameters`, which is expanded into a grid: a named list is
crossed with [`expand.grid()`](https://rdrr.io/r/base/expand.grid.html),
and a tidymodels parameter set (see
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md))
is expanded with
[`dials::grid_regular()`](https://dials.tidymodels.org/reference/grid_regular.html)
or
[`dials::grid_random()`](https://dials.tidymodels.org/reference/grid_regular.html),
depending on `control`. One of the two is required.

These grid columns change what the module sends:

- Runtime settings, sent to the chat: `temperature`, `top_p`,
  `reasoning_effort`, `frequency_penalty`, `presence_penalty`,
  `max_tokens`, `max_output_tokens` and `service_tier`. Reasoning models
  restrict sampling settings: gpt-6-luna, for example, accepts
  `temperature` and `top_p` only when `reasoning_effort` is `"none"`.

- `instructions` replaces the signature's instructions, and
  `instructions_suffix` is appended to them.

- `template` replaces the prompt template.

Other columns are stored in `module$config` but do not change the
prompt.

Each candidate is evaluated on a copy of the module with
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
so a grid of `n` rows makes up to `n * nrow(data)` model calls. The
trials are stored on the module; read them with
[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)
or
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md).

## See also

Other grid search:
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md),
[`module_parameters()`](https://jameshwade.github.io/dsprrr/reference/module_parameters.md),
[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md)

## Examples

``` r
if (FALSE) { # \dontrun{
classifier <- module(signature("text -> sentiment"))
devset <- data.frame(
  text = c("I love it!", "Terrible experience", "It's okay"),
  sentiment = c("positive", "negative", "neutral")
)

optimize_grid(
  classifier,
  data = devset,
  metric = metric_exact_match(field = "sentiment"),
  grid = data.frame(reasoning_effort = c("none", "low", "medium")),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)

# `classifier` now carries the best setting and the trials
classifier$config$reasoning_effort
module_trials(classifier)

# Named lists are crossed into a grid
optimize_grid(
  classifier,
  data = devset,
  metric = metric_exact_match(field = "sentiment"),
  parameters = list(
    reasoning_effort = c("none", "low"),
    instructions_suffix = c("Answer with one word.", "Be decisive.")
  ),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
