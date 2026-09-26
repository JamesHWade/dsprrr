# Run a module on named inputs

`run()` calls a module with inputs named after its signature's input
fields and returns the outputs. Give a vector instead of a single value
to run a batch: each element is one call, and length-1 inputs are
recycled.

## Usage

``` r
run(module, ...)
```

## Arguments

- module:

  A module, such as one created with
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
  [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
  [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
  [`module_fn()`](https://jameshwade.github.io/dsprrr/reference/module_fn.md)
  or
  [`pipeline()`](https://jameshwade.github.io/dsprrr/reference/pipeline.md).

- ...:

  Inputs named after the signature's input fields, followed by any of
  the runtime arguments described below. RLM modules
  ([`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md))
  treat every value as one context object whatever its length; use
  [`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
  to run them several times.

## Value

With `.return_format = "simple"`, the outputs as a named list with one
element per output field, for example `list(answer = "4")`. A signature
whose output type is a bare ellmer type (such as
[`ellmer::type_enum()`](https://ellmer.tidyverse.org/reference/type_boolean.html))
returns the bare value instead. A batch returns a list with one such
result per input element.

With `.return_format = "structured"`, a list of class `dsprrr_result`
with elements `output` (as above), `chat` (the ellmer Chat used) and
`metadata` (model, prompt, token counts, cost, latency, cache status and
error). A batch returns a list of these with class
`dsprrr_batch_result`. Use
[`get_output()`](https://jameshwade.github.io/dsprrr/reference/accessors.md),
[`get_metadata()`](https://jameshwade.github.io/dsprrr/reference/accessors.md)
or
[`get_cost()`](https://jameshwade.github.io/dsprrr/reference/accessors.md)
to read them.

## Details

ellmer retries failed requests (see `options(ellmer_max_tries = )`). A
failure that remains raises an error for a single input. In a batch, a
failed row becomes `NA` with a warning, and with
`.return_format = "structured"` its message is in `metadata$error`.

Batches must have inputs of one common length, or length 1. If every
input has length zero, `run()` returns an empty list without calling the
model. Modules with their own execution loop, such as
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
accept single inputs only; use
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
for them.

An input can also be an ellmer content object, such as
`ellmer::content_image_file("receipt.png")`; prediction modules send it
to the model along with the text of the prompt.

Each call records a trace on the module (see
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)).
Prediction modules also add every model call to the session's prompt
history (see
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md)).

## Runtime arguments

These arguments start with a dot so they cannot clash with input names.
Any other dot-prefixed name is an error.

- `.llm`:

  An ellmer Chat to use for this call. It takes precedence over the chat
  stored on the module, a chat set with
  [`with_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md)
  or
  [`local_lm()`](https://jameshwade.github.io/dsprrr/reference/with_lm.md),
  and the default chat; see
  [`get_default_chat()`](https://jameshwade.github.io/dsprrr/reference/get_default_chat.md)
  for the full order.

- `.cache`:

  `NULL` (the default) follows
  [`configure_cache()`](https://jameshwade.github.io/dsprrr/reference/configure_cache.md).
  `FALSE` skips the response cache for this call. `TRUE` uses it when
  caching is enabled globally and has no effect otherwise.

- `.concurrency`:

  A policy from
  [`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md)
  for batch inputs. The default runs rows one after another.

- `.return_format`:

  `"simple"` (the default) returns the outputs; `"structured"` also
  returns the Chat and call metadata (see Value).

- `.show_prompt`:

  If `TRUE`, print a preview before the call: the instructions (first
  200 characters), the input field names, the output type and the number
  of demos. It does not show the filled-in prompt; use
  [`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md)
  after the call for that.

- `.trace_context`:

  A named, JSON-compatible list copied into the call metadata and
  traces, for example `list(request_id = "abc")`. It is never sent to
  the model and is not part of cache keys. Credential-like field names
  and runtime objects are rejected.

- `.progress`:

  Show a progress bar for batch inputs. Default `TRUE`.

- `.verbose`:

  If `TRUE`, print the rendered input section of each prompt. Default
  `FALSE`.

## See also

Other execution:
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`predict.Module()`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md),
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md),
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)

## Examples

``` r
# A function-backed module runs without a model
shout <- module_fn("text -> reply", function(text) toupper(text))
run(shout, text = "hello")
#> $reply
#> [1] "HELLO"
#> 

if (FALSE) { # \dontrun{
llm <- ellmer::chat_openai(model = "gpt-6-luna")
classify <- module(
  signature("text -> sentiment: enum('positive', 'negative', 'neutral')")
)

# One input returns a named list
result <- run(classify, text = "I love this!", .llm = llm)
result$sentiment

# A vector runs a batch: one result per element
run(classify, text = c("I love this!", "This is bad"), .llm = llm)

# Structured results carry the Chat and call metadata
res <- run(classify, text = "Great!", .llm = llm, .return_format = "structured")
res$metadata$cost

# Skip the response cache for one call
run(classify, text = "Great!", .llm = llm, .cache = FALSE)
} # }
```
