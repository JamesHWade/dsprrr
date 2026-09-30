# Rebuild a program from a saved artifact

`restore_module_config()` rebuilds a program from a program artifact,
such as one read from a pin written by
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md)
or created by
[`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).
The restored program has the saved signatures, configuration and
demonstrations but no chat; pass one at run time.

## Usage

``` r
restore_module_config(config, registry = list(), trusted = FALSE)
```

## Arguments

- config:

  A program artifact, for example from
  [`pins::pin_read()`](https://pins.rstudio.com/reference/pin_read.html).

- registry:

  Named runtime registry used to resolve saved function names; see
  [`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).

- trusted:

  Whether embedded runtime values may be restored (default `FALSE`); see
  [`program_artifact()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md).

## Value

The restored program.

## See also

Other persistence:
[`export_module_code()`](https://jameshwade.github.io/dsprrr/reference/export_module_code.md),
[`pin_module_config()`](https://jameshwade.github.io/dsprrr/reference/pin_module_config.md),
[`pin_trace()`](https://jameshwade.github.io/dsprrr/reference/pin_trace.md),
[`pin_vitals_log()`](https://jameshwade.github.io/dsprrr/reference/pin_vitals_log.md),
[`program-artifact`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)

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
#> Creating new version '20260930T002207Z-86b92'
#> Writing to pin 'sentiment-classifier'
#> ✔ Pinned program artifact: "sentiment-classifier"
#> ℹ Root module: <PredictModule>
#> ℹ Graph nodes: 1
#> ℹ Compiled: TRUE

artifact <- pins::pin_read(board, "sentiment-classifier")
restored <- restore_module_config(artifact)
#> ✔ Restored program artifact
#> ℹ Root module: <PredictModule>
#> ℹ Artifact version: 6
restored$is_compiled()
#> [1] TRUE

if (FALSE) { # \dontrun{
run(
  restored,
  text = "This is great!",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
} # }
```
