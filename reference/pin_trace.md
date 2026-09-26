# Pin a module's traces to a pins board

`pin_trace()` saves a module's execution traces (timing, token use, cost
and, optionally, prompts and outputs) to a pins board, together with a
summary from
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md).
Use it to keep a record of a run for later analysis.

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

  A pins board.

- name:

  Name of the pin.

- module:

  A module that has been run.

- include_prompts:

  Whether to include the full prompts (default `FALSE`).

- include_outputs:

  Whether to include the full outputs (default `FALSE`).

- description:

  Optional pin description.

- ...:

  Further arguments passed to
  [`pins::pin_write()`](https://pins.rstudio.com/reference/pin_read.html).

## Value

`name`, invisibly.

## Details

The pin is a list with `traces` (from
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)),
`summary` and `metadata` (module class, number of traces, creation time
and the include flags). A module without traces is not pinned; a warning
is raised instead.

## See also

Other persistence:
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`program-artifact`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)

## Examples

``` r
if (FALSE) { # \dontrun{
board <- pins::board_folder("pins")
classifier <- module(signature("text -> sentiment"))
run(
  classifier,
  text = c("Great!", "Terrible."),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)

pin_trace(
  board,
  "sentiment-traces",
  classifier,
  include_prompts = TRUE,
  description = "Production run traces"
)
pins::pin_read(board, "sentiment-traces")$summary
} # }
```
