# Extract outputs, metadata and costs from results

These functions read the parts of a result so you do not need to know
its structure. They work on results of
[`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) with
`.return_format = "structured"`, for single inputs and batches, and on
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
results.

- `get_output()` returns the outputs: the named list of output fields
  for a single result, a list of them for a batch, and the predictions
  of an evaluation.

- `get_metadata()` returns the call metadata: a list for a single result
  and one list per row for a batch or an evaluation.

- `get_tokens()` returns `input_tokens`, `output_tokens` and
  `total_tokens`.

- `get_cost()` returns the estimated cost in US dollars.

For
[`run_dataset()`](https://jameshwade.github.io/dsprrr/reference/run_dataset.md)
results, use the `result` and `.metadata` columns instead.

## Usage

``` r
get_output(x, ...)

get_metadata(x, ...)

get_tokens(x, ...)

get_cost(x, ...)
```

## Arguments

- x:

  A result: a `dsprrr_result` or `dsprrr_batch_result` from
  [`run()`](https://jameshwade.github.io/dsprrr/reference/run.md) with
  `.return_format = "structured"`, or a `dsprrr_evaluation` from
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md).
  For other objects, `get_output()` returns `x` itself and the other
  functions return empty or missing values.

- ...:

  Not used.

## Value

- `get_output()`: the outputs, as described above.

- `get_metadata()`: a list, or a list of lists; an empty list when `x`
  has no metadata.

- `get_tokens()`: a list of the three counts for a single result, or a
  tibble with columns `index`, `input_tokens`, `output_tokens` and
  `total_tokens` for a batch or an evaluation. Unknown counts are `NA`.

- `get_cost()`: a number (`NA` when unknown) for a single result. For a
  batch or an evaluation, a list of class `dsprrr_cost_summary` with
  `costs` (a tibble of `index` and `cost`), `total` and `n_missing`;
  missing costs give a warning and make `total` `NA` instead of counting
  as free.

## See also

Other inspection:
[`clear_prompt_history()`](https://jameshwade.github.io/dsprrr/reference/clear_prompt_history.md),
[`clear_traces()`](https://jameshwade.github.io/dsprrr/reference/clear_traces.md),
[`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md),
[`get_last_prompt()`](https://jameshwade.github.io/dsprrr/reference/get_last_prompt.md),
[`inspect_history()`](https://jameshwade.github.io/dsprrr/reference/inspect_history.md),
[`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md),
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md)

## Examples

``` r
shout <- module_fn("text -> reply", function(text) toupper(text))
result <- run(shout, text = "hello", .return_format = "structured")

get_output(result)
#> $reply
#> [1] "HELLO"
#> 
get_metadata(result)$latency_ms
#> [1] 0.06961823
# Function-backed modules make no model calls, so these are NA
get_tokens(result)
#> $input_tokens
#> [1] NA
#> 
#> $output_tokens
#> [1] NA
#> 
#> $total_tokens
#> [1] NA
#> 
get_cost(result)
#> [1] NA

if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
batch <- run(
  qa,
  question = c("What is 2 + 2?", "What is 3 + 3?"),
  .llm = ellmer::chat_openai(model = "gpt-6-luna"),
  .return_format = "structured"
)
get_tokens(batch)
get_cost(batch)$total
} # }
```
