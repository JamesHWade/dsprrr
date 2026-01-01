# Load Trial Log from Directory

Load a TrialLog from a directory that was previously saved.

## Usage

``` r
load_trial_log(log_dir)
```

## Arguments

- log_dir:

  Path to the log directory.

## Value

A TrialLog object.

## Examples

``` r
if (FALSE) { # \dontrun{
log <- load_trial_log("logs/my_optimizer/")
log$as_tibble()
} # }
```
