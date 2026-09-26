# Show the most recent prompt and response

`get_last_prompt()` returns the most recent model call in the prompt
history: the prompt that was sent, the response, and the model, tokens,
cost and duration. Printing it shows a "Prompt Inspection" block with
Prompt, Response and Metadata sections (long prompts and responses are
cut at 500 characters). Use it to see exactly what a module sent.

## Usage

``` r
get_last_prompt()
```

## Value

A list of class `dsprrr_prompt_inspection` with `prompt` (the full
prompt, including the instructions), `response`, `model`, `tokens_in`,
`tokens_out`, `cost` (in US dollars, when known), `duration_s`,
`timestamp`, `source` (the module class that made the call),
`program_artifact_id` and `trace_context`. If no call has been recorded,
`NULL`, invisibly, with a message.

## See also

Other inspection:
[`accessors`](https://jameshwade.github.io/dsprrr/reference/accessors.md),
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md),
[`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md),
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md),
[`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md),
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md)

## Examples

``` r
if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
run(qa, question = "What is 2 + 2?", .llm = ellmer::chat_openai(model = "gpt-6-luna"))
get_last_prompt()
get_last_prompt()$prompt
} # }
```
