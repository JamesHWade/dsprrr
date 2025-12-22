# Stream module output asynchronously

Streams text output from a module asynchronously. Returns a promise that
resolves to an async generator.

## Usage

``` r
stream_async(module, ..., .llm = NULL)
```

## Arguments

- module:

  A dsprrr Module object

- ...:

  Named inputs matching the module's signature

- .llm:

  Optional ellmer Chat object

## Value

A promise that resolves to an async generator

## Examples

``` r
if (FALSE) { # \dontrun{
stream_async(mod, question = "Write a story") |>
  promises::then(function(gen) {
    # Process async generator
  })
} # }
```
