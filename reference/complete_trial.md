# Record evaluation results on a trial

`complete_trial()` copies the scores, token use, cost and timing of an
evaluation from
[`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md)
into a trial record and marks it `"completed"`.

## Usage

``` r
complete_trial(trial, eval_result, compiled_artifact_ref = NULL, notes = NULL)
```

## Arguments

- trial:

  A trial record from
  [`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md).

- eval_result:

  The `EvalResult` returned by
  [`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md).

- compiled_artifact_ref:

  Optional compiled program. When this trial is the best one in a
  [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
  with a `log_dir`, the log saves the program as `best_program.rds`.

- notes:

  Optional note that replaces the trial's note.

## Value

The updated trial record, with status `"completed"`.

## See also

Other optimizer building blocks:
[`TrialLog`](https://jameshwade.github.io/dsprrr/reference/TrialLog.md),
[`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md),
[`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md),
[`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md),
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
[`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md),
[`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md),
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md),
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)

## Examples

``` r
if (FALSE) { # \dontrun{
program <- module(signature("question -> answer"))
data <- data.frame(question = "2 + 2?", answer = "4")
result <- eval_program(
  program,
  data,
  metric_exact_match(field = "answer"),
  .llm = ellmer::chat_openai(model = "gpt-6-luna")
)
trial <- create_trial("my-search", params = list(variant = "baseline"))
complete_trial(trial, result, compiled_artifact_ref = program)
} # }
```
