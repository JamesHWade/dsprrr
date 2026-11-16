# Load conversation history from JSON

Load conversation history from JSON

## Usage

``` r
load_history(path)
```

## Arguments

- path:

  A file path to a JSON document written by save_history().

## Value

A validated dsprrr_history object.

## Examples

``` r
path <- tempfile(fileext = ".json")
history <- conversation_history(list(
  ellmer::UserTurn(list(ellmer::ContentText("Hello")))
))
save_history(history, path)
restored <- load_history(path)
length(history_turns(restored))
#> [1] 1
unlink(path)
```
