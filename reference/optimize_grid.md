# Grid Search Optimisation

Optimise a DSPrrr module over a grid of candidate configurations.
Accepts either an explicit `grid` data frame or a set of parameter
definitions that can be expanded into a grid (named lists or tidymodels
parameter sets).

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

  A DSPrrr module (created via
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)).

- ...:

  Additional arguments forwarded to
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md).

- data:

  Development data containing columns required by the module's signature
  plus any fields consumed by the metric.

- metric:

  Metric function applied per example. Defaults to

  [`metric_exact_match()`](https://jameshwade.github.io/dsprrr/reference/metric_exact_match.md).
  Use
  [`as_dsprrr_metric()`](https://jameshwade.github.io/dsprrr/reference/as_dsprrr_metric.md)
  to adapt yardstick/vitals metrics.

- grid:

  Optional data frame/tibble of candidate configurations.

- parameters:

  Optional named list or tidymodels parameter set used to generate a
  grid when `grid` is not supplied.

- objective:

  Optimisation direction. `"maximize"` (default) selects the highest
  metric value; `"minimize"` selects the lowest.

- .llm:

  Optional ellmer chat object reused during optimisation.

- control:

  Named list of control options. Recognised entries: `progress`
  (logical), `parallel` (logical forwarded to
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)),
  `evaluation_progress` (logical), `grid_type` (`"regular"` or
  `"random"`), `grid_levels` (integer, for regular grids), and
  `grid_size` (integer, for random grids).

## Value

The optimised module (modified in place, invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
program <- module(signature("question -> answer"))
data <- data.frame(question = "2 + 2?", answer = "4")
grid <- data.frame(temperature = c(0, 0.5))
optimize_grid(program, data, metric_exact_match(), grid = grid)
} # }
```
