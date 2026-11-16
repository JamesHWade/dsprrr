# Save conversation history as JSON

Save conversation history as JSON

## Usage

``` r
save_history(history, path)
```

## Arguments

- history:

  An object created by conversation_history(), loaded by load_history(),
  or returned in structured result metadata.

- path:

  A file path for the JSON document.

## Value

The path, invisibly.

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
