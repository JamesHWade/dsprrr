# Use dsprrr Workflow Templates

Copy workflow templates (targets pipeline, Quarto report) to your
project. These templates provide starting points for production LLM
workflows.

## Usage

``` r
use_dsprrr_template(
  template = c("targets", "quarto", "all"),
  path = ".",
  overwrite = FALSE
)
```

## Arguments

- template:

  Which template to use: "targets", "quarto", or "all"

- path:

  Destination directory (default: current directory)

- overwrite:

  Logical; overwrite existing files (default FALSE)

## Value

Character vector of created file paths (invisibly)

## See also

Other orchestration:
[`orchestration`](https://jameshwade.github.io/dsprrr/reference/orchestration.md),
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Copy the targets pipeline template
use_dsprrr_template("targets")

# Copy all templates
use_dsprrr_template("all", path = "workflows/")
} # }
```
