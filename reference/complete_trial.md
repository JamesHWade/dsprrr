# Complete a Trial

Mark a trial as completed with evaluation results.

## Usage

``` r
complete_trial(trial, eval_result, compiled_artifact_ref = NULL, notes = NULL)
```

## Arguments

- trial:

  A trial record created by
  [`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md).

- eval_result:

  An EvalResult object from eval_program().

- compiled_artifact_ref:

  Optional compiled module to persist as the best safe program artifact
  when this trial wins.

- notes:

  Optional additional notes.

## Value

The updated trial record with status `"completed"`.
