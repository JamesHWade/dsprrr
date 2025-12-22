# Create a Module from a Chat

Creates a dsprrr Module that wraps an ellmer Chat object. The Module can
be used for repeated predictions, batch processing, and optimization.

## Usage

``` r
as_module(x, ...)

# S3 method for class 'Chat'
as_module(x, signature, ...)

# S3 method for class 'character'
as_module(x, ...)

# S3 method for class '`dsprrr::Signature`'
as_module(x, ...)
```

## Arguments

- x:

  A Chat object, signature string, or Signature object.

- ...:

  Additional arguments passed to
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md).

- signature:

  When `x` is a Chat, the signature (string or Signature object).

## Value

A Module object (R6) that wraps the Chat.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create module from Chat
mod <- chat_openai() |> as_module("text -> sentiment")

# Use the module
mod$predict(text = "I love this!")

# Optimize
mod$optimize_grid(trainset, metric = metric_exact_match())

# Access underlying Chat
mod$chat$chat("Hello!")
} # }
```
