# Stream a module's text output asynchronously

`stream_async()` sends a module's prompt to the model and returns
ellmer's async generator of text chunks, for use inside an asynchronous
function (for example with the coro package). The response is plain
streamed text: the signature's output types are not applied. For
per-field callbacks and structured results, use
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md).

## Usage

``` r
stream_async(module, ..., .llm = NULL)
```

## Arguments

- module:

  A prediction module from
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  or
  [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md).
  Other modules are rejected before any request; use
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
  them.

- ...:

  Inputs named after the signature's input fields.

- .llm:

  An ellmer Chat; see
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
  how it is chosen when omitted.

## Value

An async generator that yields the response text in chunks.

## Details

The generator comes from ellmer's `Chat$stream_async()`. The call does
not use the response cache and records no trace.

## See also

Other execution:
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`predict.Module()`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md),
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)

## Examples

``` r
if (FALSE) { # \dontrun{
storyteller <- module(signature("topic -> story"))
chunks <- stream_async(
  storyteller,
  topic = "a lighthouse keeper",
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)

print_chunks <- coro::async(function(generator) {
  for (chunk in coro::await_each(generator)) cat(chunk)
})
print_chunks(chunks)
} # }
```
