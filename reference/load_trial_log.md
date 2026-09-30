# Load a saved trial log

`load_trial_log()` reopens a
[TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md)
from a directory written by an optimizer's `log_dir` or by
`TrialLog$new(log_dir = )`.

## Usage

``` r
load_trial_log(log_dir)
```

## Arguments

- log_dir:

  Path to the log directory.

## Value

A [TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md).

## See also

Other optimizer building blocks:
[`TrialLog`](https://jameshwade.github.io/dsprrr/reference/TrialLog.md),
[`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md),
[`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md),
[`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md),
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
[`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md),
[`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md),
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md),
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)

## Examples

``` r
dir <- file.path(tempdir(), "load-trial-log-example")
log <- TrialLog$new("my-search", log_dir = dir)
log$add_trial(create_trial("my-search", params = list(k = 2L)))

restored <- load_trial_log(dir)
restored$as_tibble()[, c("trial_id", "status")]
#> # A tibble: 1 × 2
#>   trial_id                     status 
#>   <chr>                        <chr>  
#> 1 trial_20260930_174410_e01i28 pending
```
