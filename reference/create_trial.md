# Create an optimization trial record

`create_trial()` starts a record of one optimizer trial: which optimizer
ran and with which parameters. Record the evaluation with
[`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md)
and collect records in a
[TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md).
You need these only when writing your own optimizer; the built-in
optimizers create trials themselves.

## Usage

``` r
create_trial(
  optimizer_name,
  params = list(),
  trial_id = NULL,
  notes = "",
  trace_context = list()
)
```

## Arguments

- optimizer_name:

  Name of the optimizer.

- params:

  Named list of the parameters tried.

- trial_id:

  Optional trial ID. `NULL` (the default) generates one from the time
  and a random suffix.

- notes:

  Optional note.

- trace_context:

  A named, JSON-compatible list of correlation fields. When omitted
  inside
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
  the compilation's context is used; supply
  [`list()`](https://rdrr.io/r/base/list.html) to clear it.

## Value

A trial record (a `Trial` S7 object) with status `"pending"`.

## See also

Other optimizer building blocks:
[`TrialLog`](https://jameshwade.github.io/dsprrr/reference/TrialLog.md),
[`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md),
[`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md),
[`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md),
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
[`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md),
[`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md),
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md),
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)

## Examples

``` r
trial <- create_trial(
  optimizer_name = "my-search",
  params = list(max_bootstrapped_demos = 4L, instructions = "Be brief.")
)
trial
#> 
#> ── Trial: trial_20260930_002139_ncnyz0 
#> • Status: pending
#> Optimizer: my-search
#> Params: max_bootstrapped_demos, instructions
#> Started: 2026-09-30 00:21:39
```
