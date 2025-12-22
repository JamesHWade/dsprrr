# Pin Module Traces

Save module execution traces to a pins board. Traces include timing,
token usage, and optionally the full prompts and outputs.

## Usage

``` r
pin_trace(
  board,
  name,
  module,
  include_prompts = FALSE,
  include_outputs = FALSE,
  description = NULL,
  ...
)
```

## Arguments

- board:

  A pins board object

- name:

  Character name for the pin

- module:

  A DSPrrr module with recorded traces

- include_prompts:

  Logical; include full prompts (default FALSE)

- include_outputs:

  Logical; include full outputs (default FALSE)

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
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)

## Examples

``` r
if (FALSE) { # \dontrun{
board <- pins::board_folder("pins")

# Run some predictions to generate traces
results <- run(mod, text = test_texts, .llm = llm)

# Save traces for later analysis
pin_trace(board, "experiment-2024-01-traces", mod,
          include_prompts = TRUE,
          description = "Production run traces")
} # }
```
