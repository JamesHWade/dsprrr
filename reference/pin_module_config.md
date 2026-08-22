# Pin a Module Configuration

Save a complete module program artifact to a pins board for later
retrieval. This uses the versioned manifest documented in
[program-artifact](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
including nested programs and shared module identity.

## Usage

``` r
pin_module_config(
  board,
  name,
  module,
  description = NULL,
  versioned = TRUE,
  ...,
  registry = list(),
  trusted = FALSE
)
```

## Arguments

- board:

  A pins board object (e.g., from
  [`pins::board_folder()`](https://pins.rstudio.com/reference/board_folder.html))

- name:

  Character name for the pin

- module:

  A DSPrrr module whose configuration should be saved

- description:

  Optional description for the pin

- versioned:

  Logical; whether to version the pin (default TRUE)

- ...:

  Additional arguments passed to
  [`pins::pin_write()`](https://pins.rstudio.com/reference/pin_read.html)

- registry:

  Named runtime registry; see
  [program-artifact](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).

- trusted:

  Whether trusted runtime values may be embedded. The default is
  `FALSE`.

## Value

The pin name (invisibly)

## Details

The pinned configuration includes:

- Signature specification (inputs, output type, instructions)

- Module configuration (temperature, prompt_style, etc.)

- Optimization state (best parameters, trials summary)

- Metadata (module type, creation timestamp, package version)

## See also

Other orchestration:
[`orchestration`](https://jameshwade.github.io/dsprrr/reference/orchestration.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md),
[`use_dsprrr_template()`](https://jameshwade.github.io/dsprrr/reference/use_dsprrr_template.md),
[`validate_workflow()`](https://jameshwade.github.io/dsprrr/reference/validate_workflow.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a board and pin a module configuration
board <- pins::board_folder("pins")

mod <- signature("text -> sentiment") |>
  module(type = "predict") |>
  optimize_grid(data = devset, metric = metric_exact_match())

pin_module_config(board, "sentiment-classifier-v1", mod,
                  description = "Optimized sentiment classifier")

# Later, retrieve and reconstruct the module
config <- pins::pin_read(board, "sentiment-classifier-v1")
restored_mod <- restore_module_config(config)
} # }
```
