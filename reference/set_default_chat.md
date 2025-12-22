# Set the Default Chat

Sets the default Chat object for dsprrr operations. This is stored in R
options and persists for the session.

## Usage

``` r
set_default_chat(chat)
```

## Arguments

- chat:

  An ellmer Chat object, or `NULL` to clear the default.

## Value

Invisibly returns the previous default Chat (if any).

## Examples

``` r
if (FALSE) { # \dontrun{
# Set a default OpenAI chat
set_default_chat(ellmer::chat_openai())

# Set a default Claude chat
set_default_chat(ellmer::chat_claude())

# Clear the default
set_default_chat(NULL)
} # }
```
