# Run the Interactive RLM Demo

Launch an interactive Shiny app that demonstrates how Recursive Language
Models (RLMs) work. The app includes pre-recorded traces showing RLM
exploration of the bslib source code, with playback controls,
educational annotations, and an optional live execution mode.

## Usage

``` r
run_demo(port = NULL, launch.browser = TRUE)
```

## Arguments

- port:

  Port to run the app on. If NULL, Shiny picks an available port.

- launch.browser:

  Whether to open the app in a browser (default TRUE).

## Value

Invisibly returns the Shiny app object.

## Details

The demo app has two modes:

- **Replay mode** (default): Watch pre-recorded RLM traces with playback
  controls. No API key needed.

- **Live mode**: Run your own RLM queries using an OpenAI API key.

## Examples

``` r
if (FALSE) { # \dontrun{
run_demo()
} # }
```
