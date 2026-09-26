# Sample rows reproducibly

`sample_dataset()` draws `n` rows from a data frame. With a `seed`, the
draw is reproducible and the session's random number stream is left as
it was.

## Usage

``` r
sample_dataset(dataset, n = NULL, seed = NULL, replace = FALSE)
```

## Arguments

- dataset:

  A data frame.

- n:

  Number of rows to draw. When `n` is `NULL` or at least `nrow(dataset)`
  and `replace = FALSE`, the data is returned unchanged, in its original
  order.

- seed:

  Random seed, or `NULL` (the default) to use the current random number
  stream.

- replace:

  Whether to draw with replacement (default `FALSE`).

## Value

A data frame with the drawn rows.

## See also

Other optimizer building blocks:
[`TrialLog`](https://jameshwade.github.io/dsprrr/reference/TrialLog.md),
[`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md),
[`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md),
[`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md),
[`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md),
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
[`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md),
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md),
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)

## Examples

``` r
df <- data.frame(x = 1:10, y = letters[1:10])

sample_dataset(df, n = 3, seed = 42)
#>     x y
#> 1   1 a
#> 5   5 e
#> 10 10 j
identical(
  sample_dataset(df, n = 3, seed = 42),
  sample_dataset(df, n = 3, seed = 42)
)
#> [1] TRUE
```
