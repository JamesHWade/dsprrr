# Write trial records to a JSON Lines file

`write_trials_jsonl()` writes trial records to a JSON Lines file, one
JSON object per trial and line.
[`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md)
reads them back.

## Usage

``` r
write_trials_jsonl(trials, path, append = FALSE)
```

## Arguments

- trials:

  A list of trial records from
  [`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md)
  or
  [`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md).

- path:

  Path of the file to write.

- append:

  Whether to append to an existing file (default `FALSE`).

## Value

`path`, invisibly.

## Details

The file follows the permission rules described in
[TrialLog](https://jameshwade.github.io/dsprrr/reference/TrialLog.md):
on Unix, an existing file must be owned by the current user with mode
`0600`, and every existing parent directory must be owned by root or the
current user. Unsafe paths are rejected rather than repaired. The
directory must already exist.

## See also

Other optimizer building blocks:
[`TrialLog`](https://jameshwade.github.io/dsprrr/reference/TrialLog.md),
[`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md),
[`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md),
[`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md),
[`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md),
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
[`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md),
[`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md),
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md)

## Examples

``` r
trials <- list(
  create_trial("my-search", params = list(k = 2L)),
  create_trial("my-search", params = list(k = 4L))
)
path <- tempfile(fileext = ".jsonl")
write_trials_jsonl(trials, path)
readLines(path, n = 1)
#> [1] "{\"schema_version\":1,\"trial_id\":\"trial_20260926_220110_ncnyz0\",\"optimizer_name\":\"my-search\",\"params\":{\"k\":2},\"metric_summary\":[],\"cost_summary\":[],\"start_time\":\"2026-09-26T22:01:10\",\"end_time\":null,\"notes\":\"\",\"status\":\"pending\",\"trace_context\":[]}"
```
