# Restore a Module from Pinned Configuration

Reconstruct a module from a previously pinned configuration. This allows
you to load optimized modules in new sessions or different projects.

## Usage

``` r
restore_module_config(config, registry = list(), trusted = FALSE)
```

## Arguments

- config:

  A configuration list (from
  [`pins::pin_read()`](https://pins.rstudio.com/reference/pin_read.html))

- registry:

  Named runtime registry used to resolve stored IDs.

- trusted:

  Whether embedded runtime values may be restored. The default is
  `FALSE`.

## Value

A DSPrrr module with the restored configuration

## See also

Other orchestration:
[`orchestration`](https://jameshwade.github.io/dsprrr/reference/orchestration.md),
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Read pinned config and restore module
board <- pins::board_folder("pins")
config <- pins::pin_read(board, "sentiment-classifier-v1")
mod <- restore_module_config(config)

# Use the restored module
result <- run(mod, text = "This is great!", .llm = llm)
} # }
```
