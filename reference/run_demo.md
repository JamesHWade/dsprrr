# Run the interactive RLM demo

`run_demo()` opens a Shiny app that shows how recursive language models
([`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md))
work. It replays recorded traces of an RLM exploring the bslib source
code, with playback controls and annotations, and can also run your own
queries.

## Usage

``` r
run_demo(port = NULL, launch.browser = TRUE)
```

## Arguments

- port:

  The port to run the app on. With `NULL`, Shiny picks one.

- launch.browser:

  If `TRUE` (the default), open the app in a browser.

## Value

The value returned by
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) when the
app stops.

## Details

Replay mode, the default, needs no API key. Live mode runs your own RLM
queries and needs an OpenAI API key. The app needs the shiny package.

## See also

[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)

## Examples

``` r
if (FALSE) { # \dontrun{
run_demo()
} # }
```
