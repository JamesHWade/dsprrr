# Create Optimizer Control

Convenience function to create an OptimizerControl object with defaults.

## Usage

``` r
optimizer_control(
  seed = NULL,
  max_trials = NULL,
  max_errors = 5L,
  num_threads = 1L,
  progress = NA,
  log_dir = NULL,
  verbose = FALSE
)
```

## Arguments

- seed:

  Random seed for reproducibility. Default is NULL (no seed).

- max_trials:

  Maximum number of trials to run. Default is NULL (unlimited).

- max_errors:

  Maximum consecutive errors before stopping. Default is 5.

- num_threads:

  Number of threads for parallel evaluation. Default is 1.

- progress:

  Whether to display progress. Default is TRUE in interactive sessions.

- log_dir:

  Directory for trial logging. Default is NULL (no logging).

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
