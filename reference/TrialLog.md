# Trial Log

R6 class for managing a collection of trials with optional persistence.
Provides methods for adding trials, querying results, and saving to
disk.

## Public fields

- `optimizer_name`:

  Name of the optimizer using this log.

- `log_dir`:

  Directory for persistence (NULL for in-memory only).

- `trials`:

  List of Trial objects.

- `metadata`:

  Additional metadata about the optimization run.

## Methods

### Public methods

- [`TrialLog$new()`](#method-TrialLog-new)

- [`TrialLog$add_trial()`](#method-TrialLog-add_trial)

- [`TrialLog$n_trials()`](#method-TrialLog-n_trials)

- [`TrialLog$as_tibble()`](#method-TrialLog-as_tibble)

- [`TrialLog$best_trial()`](#method-TrialLog-best_trial)

- [`TrialLog$summary()`](#method-TrialLog-summary)

- [`TrialLog$save()`](#method-TrialLog-save)

- [`TrialLog$print()`](#method-TrialLog-print)

- [`TrialLog$clone()`](#method-TrialLog-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new TrialLog.

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

### Method `add_trial()`

Add a trial to the log.

#### Usage

    TrialLog$add_trial(trial, persist = TRUE)

#### Arguments

- `trial`:

  A Trial object.

- `persist`:

  Whether to immediately persist to disk if log_dir is set.

------------------------------------------------------------------------

### Method `n_trials()`

Get the number of trials.

#### Usage

    TrialLog$n_trials()

#### Returns

Integer count of trials.

------------------------------------------------------------------------

### Method `as_tibble()`

Get trials as a tibble.

#### Usage

    TrialLog$as_tibble()

#### Returns

A tibble with one row per trial.

------------------------------------------------------------------------

### Method `best_trial()`

Get the best trial by score.

#### Usage

    TrialLog$best_trial(objective = "maximize")

#### Arguments

- `objective`:

  "maximize" or "minimize".

#### Returns

The best Trial object, or NULL if no completed trials.

------------------------------------------------------------------------

### Method [`summary()`](https://rdrr.io/r/base/summary.html)

Get summary statistics for all trials.

#### Usage

    TrialLog$summary()

#### Returns

A list with summary statistics.

------------------------------------------------------------------------

### Method [`save()`](https://rdrr.io/r/base/save.html)

Save the trial log to disk.

#### Usage

    TrialLog$save(dir = NULL)

#### Arguments

- `dir`:

  Optional directory override.

#### Returns

Invisibly returns self. Throws error on critical failure.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the trial log summary.

#### Usage

    TrialLog$print()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TrialLog$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
