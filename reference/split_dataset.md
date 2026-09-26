# Split data into training and validation sets

`split_dataset()` randomly assigns `floor(prop * nrow(dataset))` rows to
a training set and the rest to a validation set, for example to get the
`trainset` and `valset` that
[`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md)
takes.

## Usage

``` r
split_dataset(dataset, prop = 0.8, seed = NULL)
```

## Arguments

- dataset:

  A data frame.

- prop:

  Share of rows for training, strictly between 0 and 1 (default `0.8`).

- seed:

  Random seed, or `NULL` (the default). With a seed, the session's
  random number stream is left as it was.

## Value

A list with data frames `train` and `val`.

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
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)

## Examples

``` r
df <- data.frame(x = 1:10)
split <- split_dataset(df, prop = 0.8, seed = 42)
nrow(split$train)
#> [1] 8
split$val
#>   x
#> 3 3
#> 7 7
```
