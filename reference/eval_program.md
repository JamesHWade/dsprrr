# Evaluate a program with per-example detail

`eval_program()` runs a program on a dataset, scores every row with a
metric, and returns an `EvalResult` with per-example scores, errors,
predictions and feedback, plus totals for tokens, cost and time. It is
the evaluation the optimizers use internally. Use it when you build your
own optimizer or need the same detail; for everyday evaluation,
[`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md)
is simpler.

## Usage

``` r
eval_program(
  program,
  dataset,
  metric,
  .llm = NULL,
  control = NULL,
  epochs = 1L,
  ...,
  .trace_context = list()
)
```

## Arguments

- program:

  A module or composed program.

- dataset:

  A data frame with the signature's input columns and the columns the
  metric compares.

- metric:

  A metric function called as `metric(prediction, expected)`, such as
  `metric_exact_match(field = "answer")`.

- .llm:

  Optional ellmer Chat.

- control:

  An
  [`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md)
  object, or `NULL` for the defaults. Its `num_threads` sets how many
  rows run at the same time and `progress` whether to show a progress
  bar.

- epochs:

  Integer number of times each row is evaluated (default `1L`).

- ...:

  Further arguments passed to
  [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md).

- .trace_context:

  A named, JSON-compatible list of correlation fields (such as an
  experiment ID) copied to the result and to every execution trace.

## Value

An `EvalResult` S7 object. Read its properties with `@`:

- `examples`: a tibble with one row per example and columns `row_id`,
  `score`, `error`, `predicted`, `feedback` (from metrics made with
  [`metric_with_feedback()`](https://jameshwade.github.io/dsprrr/reference/metric_with_feedback.md)),
  `program_trace`, and one `input_<name>` column per input.

- `mean_score`, `std_error`: mean score of the successful rows and its
  standard error.

- `n_evaluated`, `n_errors`: counts of successful and failed rows.

- `input_tokens`, `output_tokens`, `total_tokens`, `total_cost`,
  `provider_calls`, `metric_calls`: usage totals (`NA` when unknown).

- `total_latency_ms`, `start_time`, `end_time`: timing.

- `epochs`, `epoch_scores`, `score_std`, `ci_lower`, `ci_upper`: with
  `epochs` above 1, the per-epoch scores, their standard deviation and a
  95% confidence interval.

- `trace_context`: the correlation fields.

## Details

The program is copied before it runs, so its traces are not changed.
With `epochs` above 1, every row is evaluated that many times, and the
result also reports the spread of the per-epoch mean scores.

## See also

Other optimizer building blocks:
[`TrialLog`](https://jameshwade.github.io/dsprrr/reference/TrialLog.md),
[`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md),
[`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md),
[`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md),
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
[`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md),
[`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md),
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md),
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
qa <- module(signature("question -> answer"))
dataset <- data.frame(
  question = c("What is 2+2?", "What is 3+3?"),
  answer = c("4", "6")
)

result <- eval_program(
  qa,
  dataset,
  metric = metric_exact_match(field = "answer"),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
result@mean_score
result@examples
} # }
```
