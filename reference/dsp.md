# Declarative Structured Prediction

The `dsp()` function provides a pipe-friendly interface for structured
LLM predictions using dsprrr signatures. It's designed to feel familiar
to ellmer users while providing the power of declarative signatures.

## Usage

``` r
dsp(x, ...)

# S3 method for class 'Chat'
dsp(x, signature, ..., .instructions = NULL, .echo = "none", .simplify = TRUE)

# S3 method for class 'character'
dsp(x, ..., .instructions = NULL, .echo = "none", .simplify = TRUE)

# S3 method for class '`dsprrr::Signature`'
dsp(x, ..., .instructions = NULL, .echo = "none", .simplify = TRUE)
```

## Arguments

- x:

  A Chat object, signature string, or Signature object.

- ...:

  Named arguments matching the signature's inputs.

- signature:

  When `x` is a Chat, the signature (string or Signature object).

- .instructions:

  Optional additional instructions (appended to signature instructions).

- .echo:

  Whether to echo output. See
  [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html) for
  options.

- .simplify:

  Logical. If `TRUE` (default), single-field outputs are simplified to
  just the value. If `FALSE`, always returns a named list.

## Value

The structured output according to the signature's output type. When
`.simplify = TRUE` (default): single-field outputs return just the
value, multi-field outputs return a named list. When
`.simplify = FALSE`: always returns a named list.

## Details

`dsp()` is the simplest way to use dsprrr. It takes a Chat object and a
signature, builds a prompt from the provided inputs, and returns the
structured output. For more control (optimization, tracing), use
[`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md)
to create a reusable Module.

The function dispatches on the first argument:

- `dsp.Chat`: When piping from an ellmer Chat object

- `dsp.character`: When providing a signature string (uses default Chat)

- `dsp.dsprrr::Signature`: When providing a Signature object

## See also

- [`signature()`](https://jameshwade.github.io/dsprrr/reference/signature.md)
  for creating signature objects

- [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  and [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)
  for reusable modules with optimization support

- [`as_module()`](https://jameshwade.github.io/dsprrr/reference/as_module.md)
  to convert a Chat to a module

- [`set_default_chat()`](https://jameshwade.github.io/dsprrr/reference/set_default_chat.md)
  to configure the default LLM

## Examples

``` r
if (FALSE) { # \dontrun{
# Pipe-friendly usage with Chat
chat <- ellmer::chat_openai()
chat |> dsp("question -> answer", question = "What is 2+2?")

# With complex output types
chat |> dsp(
  "text -> sentiment: enum('positive', 'negative', 'neutral')",
  text = "I love this package!"
)

# Using default Chat (auto-detected from env vars)
dsp("question -> answer", question = "What is the capital of France?")

# With additional instructions
chat |> dsp(
  "text -> summary",
  text = long_article,
  .instructions = "Keep the summary under 50 words"
)

# Control output simplification
dsp("q -> answer", q = "Hi", .simplify = TRUE)   # Returns: "Hello"
dsp("q -> answer", q = "Hi", .simplify = FALSE)  # Returns: list(answer = "Hello")
} # }
```
