# List recent model calls

`inspect_history()` returns the most recent model calls of the session
as a tibble, across all modules, like DSPy's `dspy.inspect_history()`.
It can also write them to a plain-text transcript.

## Usage

``` r
inspect_history(
  n = 10,
  include_prompts = TRUE,
  include_responses = TRUE,
  file = NULL
)
```

## Arguments

- n:

  Number of recent calls to return.

- include_prompts:

  If `TRUE` (the default), include the full prompt text.

- include_responses:

  If `TRUE` (the default), include the full response text.

- file:

  A file path or writable connection. If given, the selected calls are
  also written there as a plain-text transcript.

## Value

A tibble with one row per call and columns `timestamp`, `source` (the
module class that made the call), `model`, `tokens_in`, `tokens_out`,
`cost` (in US dollars, when known), `duration_s`, `program_artifact_id`,
`trace_context`, and `prompt` and `response` when requested. With no
recorded calls, an empty tibble and a message.

## Details

The history is kept in memory for the session and holds the most recent
100 calls by default (`options(dsprrr.prompt_history_max = )`).
Prediction modules
([`module()`](https://jameshwade.github.io/dsprrr/reference/module.md),
[`chain_of_thought()`](https://jameshwade.github.io/dsprrr/reference/chain_of_thought.md))
and
[`rag_module()`](https://jameshwade.github.io/dsprrr/reference/rag_module.md),
[`react()`](https://jameshwade.github.io/dsprrr/reference/react.md),
[`rlm_module()`](https://jameshwade.github.io/dsprrr/reference/rlm_module.md)
and [`flex()`](https://jameshwade.github.io/dsprrr/reference/flex.md)
modules add their calls to it; calls made inside wrappers and pipelines,
and by
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md)
or
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
are not recorded.
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md)
empties it.

## See also

Other inspection:
[`accessors`](https://jameshwade.github.io/dsprrr/reference/accessors.md),
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md),
[`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md),
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
[`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md),
[`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md),
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md)

## Examples

``` r
# Empty until a module has called a model
inspect_history(n = 5)
#> No LLM calls recorded yet
#> # A tibble: 0 × 0

if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
run(qa, question = "What is 2 + 2?", .llm = ellmer::chat_openai(model = "gpt-6-luna"))

history <- inspect_history(n = 20)
sum(history$cost, na.rm = TRUE)

# A plain-text transcript for sharing
inspect_history(n = 20, file = tempfile(fileext = ".txt"))
} # }
```
