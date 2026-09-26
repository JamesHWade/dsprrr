# Run a module with streaming callbacks

`run_stream()` runs a module or pipeline like
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md), but
sends output text to
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)
callbacks as it is produced and reports progress through `on_status`. It
is dsprrr's counterpart to DSPy's `streamify()`, for showing progress in
Shiny apps or at the console. In a pipeline, listeners fire for matching
fields at every step, not only the last.

## Usage

``` r
run_stream(module, ..., .llm = NULL, listeners = list(), on_status = NULL)
```

## Arguments

- module:

  A module or a pipeline from
  [`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md).

- ...:

  Inputs named after the signature's input fields.

- .llm:

  An ellmer Chat; see
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
  how it is chosen when omitted.

- listeners:

  A
  [`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md),
  or a list of them.

- on_status:

  A function called with a list for each progress event (see below), or
  `NULL`.

## Value

The output, invisibly, in the same form as
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) returns
it.

## Details

### Streaming

A prediction module (from
[`module()`](https://jameshwade.github.io/dsprrr/reference/module.md) or
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md))
whose only output field is a string is streamed chunk by chunk when a
listener asks for that field; the collected text becomes the field's
value. This needs the coro package. Any other module runs normally, and
matching listeners receive the complete value once. When coro is
installed, a listener on the single string field of a module that is not
a prediction module (such as
[`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
or [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md))
is an error, raised before any request is made.

Streaming runs do not use the response cache and record no traces.

### Status events

`on_status` receives lists with these elements:

- `type`: `"step_start"`, `"field_start"`, `"field_end"` (around
  streamed text), `"field_complete"` (a field delivered in one piece) or
  `"step_end"`.

- `step` and `n_steps`: the step's position in a pipeline; both are 1
  for a single module.

- `module`: the class of the module running the step.

- `field`: the output field, for field events.

## See also

Other execution:
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`predict.Module()`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md),
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md),
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)

## Examples

``` r
if (FALSE) { # \dontrun{
writer <- module(signature("question -> answer"))
run_stream(
  writer,
  question = "Tell me a short story about a lighthouse.",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  listeners = stream_listener("answer", function(chunk) cat(chunk)),
  on_status = function(event) message("[", event$type, "] step ", event$step)
)
} # }
```
