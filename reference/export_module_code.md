# Export a program as standalone R code

`export_module_code()` writes R code that rebuilds a program: the
complete program artifact (see
[`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md))
followed by a call to
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md).
Nested programs and exact output schemas are preserved.

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

  A module or composed program.

- name:

  Variable name for the program in the generated code (default `"mod"`).

- include_demos:

  Whether to include the demonstrations (default `TRUE`).

- file:

  Optional path. When given, the code is written there; an existing file
  is replaced only after the new code parses.

- registry, trusted:

  As in
  [`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).
  Standalone code cannot embed registry or trusted runtime references,
  so programs that need them are rejected; use
  [`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)
  for those.

## Value

The code as a single string, invisibly when `file` is given.

## See also

Other optimization results:
[`apply_best_config()`](https://jameshwade.github.io/dsprrr/reference/apply_best_config.md),
[`best_demos()`](https://jameshwade.github.io/dsprrr/reference/best_demos.md),
[`best_params()`](https://jameshwade.github.io/dsprrr/reference/best_params.md),
[`config_diff()`](https://jameshwade.github.io/dsprrr/reference/config_diff.md),
[`optimization_result()`](https://jameshwade.github.io/dsprrr/reference/optimization_result.md),
[`optimization_summary()`](https://jameshwade.github.io/dsprrr/reference/optimization_summary.md),
[`top_trials()`](https://jameshwade.github.io/dsprrr/reference/top_trials.md)

Other persistence:
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`program-artifact`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)

## Examples

``` r
mod <- module(signature("text -> sentiment"))
path <- tempfile(fileext = ".R")
export_module_code(mod, name = "sentiment_mod", file = path)
#> Module code written to /tmp/Rtmp4xxnp9/file1b1b738acbad.R

# Running the file rebuilds the program
source(path)
#> ✔ Restored program artifact
#> ℹ Root module: <PredictModule>
#> ℹ Artifact version: 6
sentiment_mod
#> 
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
#> Type: "object(sentiment: string)"
#> 
#> ── Instructions 
#> Given the fields `text`, produce the fields `sentiment`.
```
