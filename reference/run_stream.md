# Run a Module with Streaming Listeners and Status Events

Executes a module (or pipeline) while streaming output to per-field
listeners and emitting status events. This is dsprrr's analogue of
DSPy's `streamify()`: use it to surface intermediate progress and
incremental output in Shiny apps or console tools.

For pipelines, a status event is emitted as each step starts and ends,
and listeners fire for matching fields at any step — not just the final
one.

## Usage

``` r
run_stream(module, ..., .llm = NULL, listeners = list(), on_status = NULL)
```

## Arguments

- module:

  A dsprrr Module or pipeline.

- ...:

  Named inputs matching the module's signature.

- .llm:

  Optional ellmer Chat object.

- listeners:

  A
  [`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)
  or list of them.

- on_status:

  Optional function called with status event lists.

## Value

The final output (named list for structured outputs, character for plain
string outputs), invisibly.

## Details

### Streaming behavior

- Modules whose output is a single string field are token-streamed:
  matching listeners receive text chunks as they arrive, and the
  accumulated text becomes the field's value. Token streaming uses the
  provider's text mode and requires the `coro` package.

- Modules with multiple or non-string output fields run normally;
  matching listeners fire once with the completed value.

- Streaming execution does not record traces and bypasses the response
  cache.

### Specialized modules

One-shot fallback execution uses each module's own `forward()` method,
so it remains available to specialized modules when no matching token
listener is active or the output is not token-streamable. Actual token
streaming uses a direct provider path and is limited to ordinary
`PredictModule` steps. Unsupported token-stream requests are rejected
before provider work.

### Status events

When `on_status` is provided, it is called with a list describing each
event:

- `type`: one of `"step_start"`, `"field_start"`, `"field_end"`,
  `"field_complete"`, `"step_end"`

- `step`, `n_steps`: position within the pipeline (both `1` for a single
  module)

- `module`: class name of the executing module

- `field`: the output field name (field events only)

## Examples

``` r
if (FALSE) { # \dontrun{
sig <- signature("question -> answer")
mod <- module(sig, type = "predict")

run_stream(
  mod,
  question = "Tell me a story",
  .llm = ellmer::chat_openai(),
  listeners = stream_listener("answer", function(chunk) cat(chunk)),
  on_status = function(ev) message("[", ev$type, "] step ", ev$step)
)
} # }
```
