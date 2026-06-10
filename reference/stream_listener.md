# Create a Stream Listener for a Module Output Field

Creates a listener that receives streamed content for a specific output
field during
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md).
This mirrors DSPy's `StreamListener`: attach a callback to the field you
care about (e.g., `"answer"`) and it fires as content for that field is
produced — token by token when the field can be token-streamed, or once
with the complete value otherwise.

## Usage

``` r
stream_listener(field, callback)
```

## Arguments

- field:

  Name of the output field to listen to (a single string).

- callback:

  A function called with each chunk of text (a single string). For
  non-streamable fields, called once with the full value.

## Value

A `dsprrr_stream_listener` object for use with
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md).

## Details

Token-level streaming is available when a module's output is a single
string field. In that case dsprrr streams the response as plain text and
treats the accumulated text as the field's value. Modules with multiple
or non-string output fields run normally and fire each matching listener
once with the completed value (chunked streaming of structured output is
not supported by the underlying structured-output API).

## Examples

``` r
if (FALSE) { # \dontrun{
listener <- stream_listener("answer", function(chunk) cat(chunk))
run_stream(mod, question = "Tell me a story", listeners = list(listener))
} # }
```
