# Suggest tidymodels parameters for a module

Construct a
[`dials::parameters()`](https://dials.tidymodels.org/reference/parameters.html)
set from the information available on a module. Numeric parameters
(e.g., `temperature`, `top_p`) derive their ranges from observed
optimisation trials (if present) or fall back to sensible defaults.
Qualitative parameters (e.g., `prompt_style`) are converted to value
sets.

## Usage

``` r
module_parameters(
  module,
  include = NULL,
  exclude = c("id", "instructions", "instructions_suffix")
)
```

## Arguments

- module:

  A DSPrrr module (created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)).

- include:

  Optional character vector restricting which parameters are returned.
  Defaults to all parameters discovered in the module configuration and
  optimisation trials.

- exclude:

  Character vector of parameter names to ignore. Defaults to internal
  bookkeeping fields such as `id` and `instructions`.

## Value

A
[`dials::parameters`](https://dials.tidymodels.org/reference/parameters.html)
object describing the candidate tunables. Returns an empty parameter set
when no tunables are discovered.

## Examples

``` r
if (FALSE) { # \dontrun{
sig <- signature("text -> sentiment")
mod <- module(sig, type = "predict", config = list(temperature = 0.2))
optimize_grid(mod,
  devset = tibble::tibble(text = "sample", target = "positive"),
  parameters = list(temperature = c(0.1, 0.5))
)
module_parameters(mod)
} # }
```
