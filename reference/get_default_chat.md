# Get the Default Chat

Retrieves the default Chat object for dsprrr operations. If no default
is set, attempts to auto-detect from environment variables.

## Usage

``` r
get_default_chat(create = TRUE)
```

## Arguments

- create:

  Logical. If `TRUE` (default), create a Chat from environment variables
  if none is explicitly set. If `FALSE`, return `NULL` when no default
  is available.

## Value

An ellmer Chat object, or `NULL` if `create = FALSE` and no default is
available.

## Examples

``` r
if (FALSE) { # \dontrun{
# Set a default chat
set_default_chat(ellmer::chat_openai())

# Get the default chat
chat <- get_default_chat()

# Check if a default is available without creating one
chat <- get_default_chat(create = FALSE)
} # }
```
