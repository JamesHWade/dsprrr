# Trial Record

S7 class representing a single optimization trial. Captures all metadata
needed to reproduce and analyze the trial.

## Usage

``` r
Trial(
  trial_id = "",
  optimizer_name = "",
  params = list(),
  metric_summary = list(),
  cost_summary = list(),
  start_time = NULL,
  end_time = NULL,
  notes = "",
  compiled_artifact_ref = NULL,
  status = "pending"
)
```

## Arguments

- trial_id:

  Unique identifier for this trial.

- optimizer_name:

  Name of the optimizer that produced this trial.

- params:

  List of parameters used in this trial.

- metric_summary:

  List with mean_score, std_error, n_evaluated, n_errors.

- cost_summary:

  List with tokens_in, tokens_out, total_tokens, total_cost.

- start_time:

  POSIXct timestamp when trial started.

- end_time:

  POSIXct timestamp when trial ended.

- notes:

  Optional character string with additional notes.

- compiled_artifact_ref:

  Optional compiled module. The best module is persisted with
  [`save_program()`](https://jameshwade.github.io/dsprrr/reference/program-artifact.md)
  rather than serialized as a live R object.

- status:

  Trial status: "pending", "running", "completed", "failed".
