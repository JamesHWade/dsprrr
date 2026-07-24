# Create Optimizer Control

Convenience function to create an OptimizerControl object with defaults.

## Usage

``` r
optimizer_control(
  seed = NULL,
  max_trials = NULL,
  max_errors = 5L,
  max_metric_calls = NULL,
  max_provider_calls = NULL,
  max_input_tokens = NULL,
  max_output_tokens = NULL,
  max_total_tokens = NULL,
  max_cost = NULL,
  max_elapsed_seconds = NULL,
  num_threads = 1L,
  progress = NA,
  log_dir = NULL,
  checkpoint_path = NULL,
  resume = FALSE,
  checkpoint_registry = list(),
  verbose = FALSE
)
```

## Arguments

- seed:

  Random seed for reproducibility. Default is NULL (no seed).

- max_trials:

  Maximum number of trials to run. Default is NULL (unlimited).

- max_errors:

  Non-negative integer error budget. Optimizers report total errors
  while stopping on a separate consecutive-error streak; each success
  resets only that streak. A positive value stops on the failure that
  reaches the limit. Zero permits work to begin but stops after the
  first failure. When a completed evaluation returns multiple outcomes,
  all are included in the final counters even if the stop boundary was
  crossed partway through; the first stop reason remains unchanged and
  prevents scheduling new work.

- max_metric_calls:

  Maximum metric calls, or NULL for unlimited.

- max_provider_calls:

  Maximum verified provider calls, or NULL for unlimited. Ambiguous
  provider usage stops a run that has this cap.

- max_input_tokens:

  Maximum verified input tokens, or NULL for unlimited.

- max_output_tokens:

  Maximum verified output tokens, or NULL for unlimited.

- max_total_tokens:

  Maximum verified input plus output tokens, or NULL for unlimited.

- max_cost:

  Maximum known provider cost in US dollars, or NULL for unlimited.
  Unknown cost stops a run that has this cap.

- max_elapsed_seconds:

  Maximum active optimizer elapsed time in seconds, or NULL for
  unlimited. Checkpoint downtime is excluded.

- num_threads:

  Number of threads for parallel evaluation. Default is 1.

- progress:

  Whether to display progress. Default is TRUE in interactive sessions.

- log_dir:

  Directory for trial logging. Default is NULL (no logging).

- checkpoint_path:

  Optional optimizer checkpoint file.

- resume:

  Whether to resume from `checkpoint_path`.

- checkpoint_registry:

  Named runtime registry used by safe program artifacts stored in
  checkpoints.

- verbose:

  Whether to print detailed output. Default is FALSE

## Value

An OptimizerControl object

## Examples

``` r
# Default control
ctrl <- optimizer_control()

# With specific settings
ctrl <- optimizer_control(seed = 42L, max_trials = 100L, log_dir = "logs/")
```
