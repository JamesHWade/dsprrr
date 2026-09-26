# Build a tidymodels parameter set for a module

`module_parameters()` returns a
[`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
set describing values of a module that can be tuned. Pass it to
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
as `parameters` to search a regular or random grid over those values.

## Usage

``` r
module_parameters(
  module,
  model = NULL,
  include = NULL,
  exclude = c("id", "instructions", "instructions_suffix")
)
```

## Arguments

- module:

  A module, such as one created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- model:

  Optional model name, such as `"gpt-6-luna"`. For a reasoning model,
  `temperature` and `top_p` are replaced by `reasoning_effort`.

- include:

  Optional character vector of parameter names to keep.

- exclude:

  Character vector of parameter names to drop when `include` is `NULL`.
  The default drops `id`, `instructions` and `instructions_suffix`.

## Value

A
[`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
object, empty when nothing tunable is found.

## Details

Candidate parameters come from:

- single values in `module$config` and `module$config$params`;

- the parameters of trials recorded by
  [`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md);

- enum inputs of the signature, as `input_<name>` with the enum levels;

- runtime settings with default ranges: `temperature` and `top_p` in
  `[0, 1]`, `frequency_penalty` and `presence_penalty` in `[-2, 2]`, and
  `max_output_tokens` in `[32, 4096]`.

Numeric values become quantitative parameters spanning the observed
range (plus or minus 0.1 around a single value); character and logical
values become qualitative parameters. For a reasoning model (see
[`is_reasoning_model()`](https://jameshwade.github.io/dsprrr/reference/is_reasoning_model.md)),
`temperature` and `top_p` are dropped and `reasoning_effort` (`"low"`,
`"medium"`, `"high"`) is added.

Only runtime settings, `instructions`, `instructions_suffix` and
`template` change what
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)
sends to the model. Other parameters, such as `input_<name>` or internal
config fields like `.module_kind`, are stored in the module's config
without effect, so use `include` to keep the ones you mean to tune.

## See also

Other grid search:
[`GridSearchTeleprompter()`](https://jameshwade.github.io/dsprrr/reference/GridSearchTeleprompter.md),
[`module_metrics()`](https://jameshwade.github.io/dsprrr/reference/module_metrics.md),
[`module_trials()`](https://jameshwade.github.io/dsprrr/reference/module_trials.md),
[`optimize_grid()`](https://jameshwade.github.io/dsprrr/reference/optimize_grid.md)

## Examples

``` r
mod <- module(
  signature("text -> sentiment"),
  config = list(temperature = 0.2)
)
module_parameters(mod, include = c("temperature", "top_p"))
#> Collection of 2 parameters for tuning
#> 
#>   identifier        type    object
#>  temperature temperature nparam[+]
#>        top_p       top_p dparam[+]
#> 

# Reasoning models tune reasoning_effort instead of temperature
module_parameters(
  mod,
  model = "gpt-6-luna",
  include = c("temperature", "reasoning_effort")
)
#> Collection of 1 parameters for tuning
#> 
#>        identifier             type    object
#>  reasoning_effort reasoning_effort dparam[+]
#> 
```
