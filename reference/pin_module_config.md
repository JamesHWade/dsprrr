# Pin a program to a pins board

`pin_module_config()` saves a complete program, including nested
modules, demonstrations and optimization results, to a pins board as an
`.rds` pin. Read it back with
[`pins::pin_read()`](https://pins.rstudio.com/reference/pin_read.html)
and rebuild the program with
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md).

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

  A pins board, such as `pins::board_folder("pins")`.

- name:

  Name of the pin.

- module:

  The program to save.

- description:

  Optional pin description. The default is
  `"dsprrr program artifact: <name>"`.

- versioned:

  Whether pins keeps earlier versions (default `TRUE`).

- ...:

  Further arguments passed to
  [`pins::pin_write()`](https://pins.rstudio.com/reference/pin_read.html).

- registry:

  Named runtime registry; see
  [`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).

- trusted:

  Whether runtime values may be embedded (default `FALSE`); see
  [`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).

## Value

`name`, invisibly.

## Details

The pin holds the program artifact described in
[`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md):
signatures, configuration, demonstrations, optimization results and the
structure of composed programs. Chats, credentials, caches and traces
are not saved. Functions such as tools or retrievers are saved only as
names in `registry`, or embedded with `trusted = TRUE`.

## See also

Other persistence:
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`program-artifact`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md),
[`restore_module_config()`](https://jameshwade.github.io/dsprrr/reference/restore_module_config.md)

## Examples

``` r
classifier <- module(signature("text -> sentiment"))
trainset <- data.frame(
  text = c("I love it!", "Terrible experience", "It's okay"),
  sentiment = c("positive", "negative", "neutral")
)
compiled <- compile(classifier, LabeledFewShot(k = 2L), trainset)

board <- pins::board_temp()
pin_module_config(board, "sentiment-classifier", compiled)
#> Creating new version '20260930T002156Z-8e731'
#> Writing to pin 'sentiment-classifier'
#> ✔ Pinned program artifact: "sentiment-classifier"
#> ℹ Root module: <PredictModule>
#> ℹ Graph nodes: 1
#> ℹ Compiled: TRUE

# Later, or in another session
artifact <- pins::pin_read(board, "sentiment-classifier")
restored <- restore_module_config(artifact)
#> ✔ Restored program artifact
#> ℹ Root module: <PredictModule>
#> ℹ Artifact version: 6
length(restored$demos)
#> [1] 2
```
