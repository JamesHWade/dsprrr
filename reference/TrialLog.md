# Record optimization trials in memory or on disk

A `TrialLog` collects the trial records of an optimizer run (see
[`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md)).
Without `log_dir` it lives in memory. With `log_dir` it writes every
trial to a JSON Lines journal, `trials.jsonl`, as it is added, so a long
run can be inspected or resumed later with
[`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md).
Optimizers create one when you give them a `log_dir`.

## Details

A log directory holds `trials.jsonl`, the authoritative journal, and
three derived files that are refreshed after each change:
`metadata.json`, `README.md` (a readable summary) and `best_program.rds`
(the best trial's program, when recorded). The derived files can lag
behind after an interruption; the next successful save rebuilds them.
Existing records in `log_dir` are loaded when the log is created. Adding
a trial whose ID is already present is a no-op when the records match
and an error when they differ.

### File permissions

Logs are private to the current user. On Unix, an existing log directory
must be owned by the effective user with mode `0700`, existing log files
must have mode `0600` without special bits, and every existing parent
directory must be owned by root or the effective user. Paths that break
these rules, and symbolic links, are rejected without being read or
repaired. New directories and files are created owner-only. On Windows,
where base R cannot verify owner-only access, the account's filesystem
ACLs apply, and logging fails if stable file identifiers are
unavailable.

## See also

Other optimizer building blocks:
[`complete_trial()`](https://jameshwade.github.io/dsprrr/reference/complete_trial.md),
[`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md),
[`eval_program()`](https://jameshwade.github.io/dsprrr/reference/eval_program.md),
[`load_trial_log()`](https://jameshwade.github.io/dsprrr/reference/load_trial_log.md),
[`optimizer_control()`](https://jameshwade.github.io/dsprrr/reference/optimizer_control.md),
[`read_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/read_trials_jsonl.md),
[`sample_dataset()`](https://jameshwade.github.io/dsprrr/reference/sample_dataset.md),
[`split_dataset()`](https://jameshwade.github.io/dsprrr/reference/split_dataset.md),
[`write_trials_jsonl()`](https://jameshwade.github.io/dsprrr/reference/write_trials_jsonl.md)

## Public fields

- `optimizer_name`:

  Name of the optimizer using this log.

- `log_dir`:

  Directory for persistence (NULL for in-memory only).

- `trials`:

  List of optimization trial records.

- `metadata`:

  Additional metadata about the optimization run.

## Methods

### Public methods

- [`TrialLog$new()`](#method-TrialLog-initialize)

- [`TrialLog$add_trial()`](#method-TrialLog-add_trial)

- [`TrialLog$n_trials()`](#method-TrialLog-n_trials)

- [`TrialLog$as_tibble()`](#method-TrialLog-as_tibble)

- [`TrialLog$best_trial()`](#method-TrialLog-best_trial)

- [`TrialLog$summary()`](#method-TrialLog-summary)

- [`TrialLog$save()`](#method-TrialLog-save)

- [`TrialLog$print()`](#method-TrialLog-print)

- [`TrialLog$clone()`](#method-TrialLog-clone)

------------------------------------------------------------------------

### `TrialLog$new()`

Create or resume a TrialLog. Existing JSONL records are loaded without
rewriting the file.

#### Usage

    TrialLog$new(optimizer_name, log_dir = NULL, metadata = NULL)

#### Arguments

- `optimizer_name`:

  Name of the optimizer.

- `log_dir`:

  Optional directory for persistence.

- `metadata`:

  Optional metadata list.

------------------------------------------------------------------------

### `TrialLog$add_trial()`

Add a trial to the log. Persisted trials atomically append one
authoritative JSONL record. Derived metadata, summaries, and the best
program are then refreshed independently on a best-effort basis.

#### Usage

    TrialLog$add_trial(trial, persist = TRUE)

#### Arguments

- `trial`:

  A trial record created by
  [`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md).

- `persist`:

  Whether to immediately persist to disk if log_dir is set.

------------------------------------------------------------------------

### `TrialLog$n_trials()`

Get the number of trials.

#### Usage

    TrialLog$n_trials()

#### Returns

Integer count of trials.

------------------------------------------------------------------------

### `TrialLog$as_tibble()`

Get trials as a tibble.

#### Usage

    TrialLog$as_tibble()

#### Returns

A tibble with one row per trial.

------------------------------------------------------------------------

### `TrialLog$best_trial()`

Get the best trial by score.

#### Usage

    TrialLog$best_trial(objective = "maximize")

#### Arguments

- `objective`:

  "maximize" or "minimize".

#### Returns

The best optimization trial record, or `NULL` if no trials have
completed.

------------------------------------------------------------------------

### `TrialLog$summary()`

Get summary statistics for all trials.

#### Usage

    TrialLog$summary()

#### Returns

A list with summary statistics.

------------------------------------------------------------------------

### `TrialLog$save()`

Save the trial log to disk. Only records missing from the destination
are appended to the authoritative journal. Derived files are
independently refreshed on a best-effort basis and may lag if that
refresh warns.

#### Usage

    TrialLog$save(dir = NULL)

#### Arguments

- `dir`:

  Optional directory override.

#### Returns

Invisibly returns self. Throws error on critical failure.

------------------------------------------------------------------------

### `TrialLog$print()`

Print the trial log summary.

#### Usage

    TrialLog$print()

------------------------------------------------------------------------

### `TrialLog$clone()`

The objects of this class are cloneable with this method.

#### Usage

    TrialLog$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
log <- TrialLog$new("my-search")
log$add_trial(create_trial("my-search", params = list(k = 2L)))
log$add_trial(create_trial("my-search", params = list(k = 4L)))
log$n_trials()
#> [1] 2
log$as_tibble()[, c("trial_id", "status", "mean_score")]
#> # A tibble: 2 × 3
#>   trial_id                     status  mean_score
#>   <chr>                        <chr>        <dbl>
#> 1 trial_20260930_002127_ncnyz0 pending         NA
#> 2 trial_20260930_002127_e01i28 pending         NA

# Persist to a directory and load it again
dir <- file.path(tempdir(), "trial-log-example")
saved <- TrialLog$new("my-search", log_dir = dir)
saved$add_trial(create_trial("my-search", params = list(k = 2L)))
list.files(dir)
#> [1] "README.md"     "metadata.json" "trials.jsonl" 
load_trial_log(dir)
#> 
#> ── Trial Log: my-search 
#> Trials: 1 (0 completed, 0 failed)
#> Log Dir: /tmp/RtmpQlgZnT/trial-log-example
```
