# Run a module asynchronously

`run_async()` starts one call of a module and returns a promise instead
of waiting for the result, so a Shiny app or other event loop stays
responsive and several calls can be in flight at once. Handle the result
with the promises package, for example
[`promises::then()`](https://rstudio.github.io/promises/reference/then.html).

## Usage

``` r
run_async(
  module,
  ...,
  .llm = NULL,
  .return_format = c("simple", "structured"),
  .trace_context = list(),
  .history = NULL,
  .control = NULL
)
```

## Arguments

- module:

  A prediction module from
  [`module()`](https://jameshwade.github.io/dsprrr/reference/module.md)
  or
  [`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md),
  or a
  [`program_of_thought()`](https://jameshwade.github.io/dsprrr/reference/program_of_thought.md),
  [`code_act()`](https://jameshwade.github.io/dsprrr/reference/code_act.md)
  or
  [`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
  module configured with `interpreter_factory`.

- ...:

  Inputs named after the signature's input fields (single values).

- .llm:

  An ellmer Chat; see
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
  how it is chosen when omitted. Give concurrent calls separate chats,
  for example with `llm$clone()`.

- .return_format:

  Either `"simple"` (the default), which resolves to the output value,
  or `"structured"`, which resolves to a `dsprrr_result` with `output`,
  `chat`, and `metadata` fields.

- .trace_context:

  A named, JSON-compatible list, as in
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md). The
  promise carries it in its `dsprrr_trace_context` attribute. Direct
  Predict calls also record it in the module's traces.

- .history:

  Explicit conversation history is not supported by async execution; use
  synchronous
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) for
  scalar root ReAct continuation.

- .control:

  An optional
  [`react_async_control()`](https://jameshwade.github.io/dsprrr/reference/react_async_control.md)
  for cooperative cancellation and progress reporting. Supported only
  for
  [`react()`](https://jameshwade.github.io/dsprrr/reference/react.md)
  modules.

## Value

A promise that resolves to the module's output, matching
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md)'s
default result, or to a `dsprrr_result` when
`.return_format = "structured"`. Provider, decoding, and completion
failures reject the promise.

## Details

Prediction modules call ellmer's `chat_structured_async()` directly.
These calls bypass the response cache and record one trace on either
success or failure. Structured results include canonical usage,
decisions, and any provider-supplied native reasoning blocks recorded
for the current call; unavailable usage remains `NA` and absent native
reasoning remains `NULL`. ReAct failures and cancellations preserve the
partial native Chat history, observed tool results, and
requested-versus-result status in the trace. That partial history is not
a portable continuation or an instruction to replay tools.
Factory-backed code-running modules keep their simple result behavior;
structured results are rejected because that worker path does not yet
preserve the same trace contract. Modules bound to a caller-owned
`runner` are rejected, because one runner cannot serve concurrent calls.
Other specialized modules, such as pipelines, are rejected; use
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

Each direct provider call reserves its resolved Chat until the promise
settles. A concurrent call using that same Chat is rejected before
provider work; use separate Chats for overlapping calls.

ReAct modules use ellmer's sequential async tool loop and await their
structured follow-up when configured. Their optional `.control` reports
progress and supports cooperative cancellation between requests and
tools; it does not interrupt a request or tool already in progress.

## See also

Other execution:
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`predict.Module()`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md),
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md),
[`stream_listener()`](https://jameshwade.github.io/dsprrr/reference/stream_listener.md)

## Examples

``` r
if (FALSE) { # \dontrun{
llm <- ellmer::chat_openai(model = "gpt-6-luna")
summarize <- module(signature("text -> summary"))

first <- run_async(summarize, text = "First article ...", .llm = llm$clone())
second <- run_async(summarize, text = "Second article ...", .llm = llm$clone())

promises::promise_all(first, second) |>
  promises::then(function(results) {
    vapply(results, function(r) r$summary, character(1))
  })
} # }
```
