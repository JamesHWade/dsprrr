# Run a module asynchronously

`run_async()` starts one call of a module and returns a promise instead
of waiting for the result, so a Shiny app or other event loop stays
responsive and several calls can be in flight at once. Handle the result
with the promises package, for example
[`promises::then()`](https://rstudio.github.io/promises/reference/then.html).

## Usage

``` r
run_async(module, ..., .llm = NULL, .trace_context = list())
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

- .trace_context:

  A named, JSON-compatible list, as in
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md). The
  promise carries it in its `dsprrr_trace_context` attribute.

## Value

A promise that resolves to the module's output, the same value that
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) returns
by default.

## Details

Prediction modules call ellmer's `chat_structured_async()` directly: the
call does not use the response cache and records no trace or prompt
history. Code-running modules run their whole workflow in a separate
mirai process with a fresh interpreter; modules bound to a caller-owned
`runner` are rejected, because one runner cannot serve concurrent calls.
Other modules, such as
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md) or
pipelines, are rejected; use
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md).

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
