# Clear Cached Default Chat

Clears any cached default Chat. Useful for testing or when environment
variables change.

## Usage

``` r
clear_default_chat()
```

## Value

Invisibly returns `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Clear cached chat (will re-detect on next use)
clear_default_chat()
} # }
```
