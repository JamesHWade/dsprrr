# Listen to one output field while streaming

`stream_listener()` attaches a callback to an output field for
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
like DSPy's `StreamListener`. The callback receives the field's text as
it is produced: chunk by chunk when the field can be streamed, otherwise
once with the complete value.

## Usage

``` r
stream_listener(field, callback)
```

## Arguments

- field:

  The name of the output field, such as `"answer"`.

- callback:

  A function called with one string: each chunk of text, or the complete
  value of a field that cannot be streamed.

## Value

A listener (class `dsprrr_stream_listener`) for the `listeners` argument
of
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md).

## Details

A field can be streamed chunk by chunk when it is the only output field
of its module and is a string. Fields of modules with several outputs,
or non-string outputs, arrive once, when the module finishes, because
structured output cannot be streamed.

## See also

Other execution:
[`concurrency_control()`](https://jameshwade.github.io/dsprrr/reference/concurrency_control.md),
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md),
[`predict.Module()`](https://jameshwade.github.io/dsprrr/reference/predict.Module.md),
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md),
[`run_async()`](https://jameshwade.github.io/dsprrr/reference/run_async.md),
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md),
[`run_stream()`](https://jameshwade.github.io/dsprrr/reference/run_stream.md),
[`stream_async()`](https://jameshwade.github.io/dsprrr/reference/stream_async.md)

## Examples

``` r
show_answer <- stream_listener("answer", function(chunk) cat(chunk))

if (FALSE) { # \dontrun{
storyteller <- module(signature("question -> answer"))
run_stream(
  storyteller,
  question = "Tell me a short story about a lighthouse.",
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  listeners = show_answer
)
} # }
```
