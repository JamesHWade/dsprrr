# Trial Log

R6 class for managing a collection of trials with optional persistence.
Existing JSONL records are loaded when `log_dir` already contains a log.
New trials are appended one record at a time; matching trial IDs are
idempotent, while conflicting records with the same ID are rejected. The
`trials.jsonl` journal is authoritative. `metadata.json`, `README.md`,
and `best_program.rds` are independently refreshed, best-effort derived
views; they may lag after an interruption and are rebuilt by a later
successful save. On Unix, a pre-existing private log directory must be
owned by the effective user with exactly mode `0700`, and every
pre-existing log file must have exactly mode `0600`; special mode bits
are rejected. Every existing ancestor must be owned by root or the
effective user, including sticky shared parents. Before initialization
or a save to another directory locks, reads, or mutates storage, dsprrr
preflights every known target: the lock, journal, metadata, summary, and
best-program artifact. Unsafe paths are rejected without repair or
reads. Directories and files created for the current operation are
enforced as owner-only. Non-symbolic regular files remain required.
Windows uses the account's filesystem ACLs, which base R cannot verify
as owner-only, and fails closed if stable device and file identifiers
are unavailable.

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
