# Export Module Configuration as R Code

Generate R code containing the complete program artifact and its
restoration call. This preserves nested graphs and exact schemas without
hand-rendering module fields.

## Usage

``` r
export_module_code(
  module,
  name = "mod",
  include_demos = TRUE,
  file = NULL,
  registry = list(),
  trusted = FALSE
)
```

## Arguments

- module:

  A DSPrrr module to export.

- name:

  Character; variable name for the module in generated code. Default is
  "mod".

- include_demos:

  Logical; whether to include demonstration examples in the generated
  code. Default is TRUE.

- file:

  Optional file path to write the code to. If NULL (default), returns
  the code as a character string. Existing files are atomically replaced
  only after the staged output parses successfully.

- registry:

  Named runtime registry; see
  [program-artifact](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).

- trusted:

  Whether trusted runtime values may be embedded. Standalone code export
  rejects registry and embedded runtime references.

## Value

If `file` is NULL, returns the R code as a character string. If `file`
is specified, writes it atomically and returns the code invisibly.

## See also

Other optimizer accessors:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

## Examples

``` r
if (FALSE) {
mod <- module(signature("text -> sentiment"), type = "predict")
mod$optimize_grid(data, metric, parameters = list(temperature = c(0.3, 1.0)))

# Get code as string
code <- export_module_code(mod)
cat(code)

# Write to file
export_module_code(mod, file = "optimized_module.R")
}
```
