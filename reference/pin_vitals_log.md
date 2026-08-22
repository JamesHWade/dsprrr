# Pin Vitals Evaluation Log

Save evaluation results from a vitals Task run to a pins board. This
enables tracking model performance over time and across experiments.

## Usage

``` r
pin_vitals_log(
  board,
  name,
  eval_result,
  module = NULL,
  description = NULL,
  ...
)
```

## Arguments

- board:

  A pins board object

- name:

  Character name for the pin

- eval_result:

  Evaluation result from
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
  or a vitals Task

- module:

  Optional module that was evaluated (for additional metadata)

- description:

  Optional description for the pin

- ...:

  Additional arguments passed to
  [`pins::pin_write()`](https://pins.rstudio.com/reference/pin_read.html)

## Value

The pin name (invisibly)

## See also

Other orchestration:
[`orchestration`](https://jameshwade.github.io/dsprrr/reference/orchestration.md),
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)

## Examples

``` r
if (FALSE) { # \dontrun{
board <- pins::board_folder("pins")

# Evaluate module on test set
eval_result <- evaluate(mod, test_data, metric = metric_exact_match())

# Pin the evaluation results
pin_vitals_log(board, "sentiment-eval-v1", eval_result,
               module = mod,
               description = "Test set evaluation")
} # }
```
