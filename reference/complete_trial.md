# Complete a Trial

Mark a trial as completed with evaluation results.

## Usage

``` r
complete_trial(trial, eval_result, compiled_artifact_ref = NULL, notes = NULL)
```

## Arguments

- trial:

  A Trial object.

- eval_result:

  An EvalResult object from eval_program().

- compiled_artifact_ref:

  Optional reference to the compiled module.

- notes:

  Optional additional notes.

## Value

Updated Trial object with status "completed".
