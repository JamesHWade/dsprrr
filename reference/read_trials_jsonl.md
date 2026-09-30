# Read trial records from a JSON Lines file

`read_trials_jsonl()` reads the trial records written by
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)
or kept in a
[TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)'s
`trials.jsonl`.

## Usage

``` r
read_trials_jsonl(path)
```

## Arguments

- path:

  Path of the JSON Lines file.

## Value

A list of trial records.

## See also

Other optimizer building blocks:
[`TrialLog`](https://jameshwade.github.io/dsprrr/reference/TrialLog.md),
[`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md),
[`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md),
[`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md),
[`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md),
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
[`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md),
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md),
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)

## Examples

``` r
path <- tempfile(fileext = ".jsonl")
trials <- list(create_trial("my-search", params = list(k = 2L)))
write_trials_jsonl(trials, path)
trials <- read_trials_jsonl(path)
trials[[1]]
#> 
#> ── Trial: trial_20260930_002204_ncnyz0 
#> • Status: pending
#> Optimizer: my-search
#> Params: k
#> Started: 2026-09-30 00:22:04
```
