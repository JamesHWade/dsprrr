# Pin evaluation results to a pins board

`pin_vitals_log()` saves evaluation results to a pins board so you can
track a module's performance across runs and experiments.

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

  A pins board.

- name:

  Name of the pin.

- eval_result:

  An
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
  result, or a list or data frame of results.

- module:

  Optional module that was evaluated, for metadata.

- description:

  Optional pin description.

- ...:

  Further arguments passed to
  [`pins::pin_write()`](https://pins.rstudio.com/reference/pin_read.html).

## Value

`name`, invisibly.

## Details

For an
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
result, the pin keeps the mean score, per-example scores, counts,
predictions and metadata. Any other list or data frame, such as the
samples from a vitals `Task`'s `$get_samples()`, is stored as it is
under `data`. A vitals `Task` object itself is not a list and is
rejected. The pin also records the dsprrr version, the time and, when
`module` is given, the module's class, compiled status and inputs.

## See also

Other persistence:
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`program-artifact`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)

## Examples

``` r
if (FALSE) { # \dontrun{
board <- pins::board_folder("pins")

eval_result <- evaluate(
  classifier,
  testset,
  metric = metric_exact_match(field = "sentiment"),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
pin_vitals_log(
  board,
  "sentiment-eval",
  eval_result,
  module = classifier,
  description = "Test set evaluation"
)
} # }
```
