# Validate Workflow Configuration

Check that a workflow has all required components configured correctly.
Useful for validating pipelines before running expensive LLM operations.

## Usage

``` r
validate_workflow(module, data = NULL, board = NULL)
```

## Arguments

- module:

  A DSPrrr module to validate

- data:

  Optional data to validate against the module's signature

- board:

  Optional pins board to check for accessibility

## Value

A list with validation results (invisibly). Prints a summary.

## See also

Other orchestration:
[`orchestration`](https://jameshwade.github.io/dsprrr/reference/orchestration.md),
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md)

## Examples

``` r
if (FALSE) { # \dontrun{
mod <- signature("text -> sentiment") |> module()
validate_workflow(mod, data = test_data)
} # }
```
