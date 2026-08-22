# Write Trials to JSONL File

Write a list of optimization trial records to a JSONL (JSON Lines) file.
Each trial is written as a single JSON object on its own line. On Unix,
an existing target must already be owned by the effective user with mode
exactly `0600`, without special bits, and every existing ancestor must
be owned by root or the effective user. Unsafe paths are rejected rather
than repaired.

## Usage

``` r
write_trials_jsonl(trials, path, append = FALSE)
```

## Arguments

- trials:

  Trial records created by
  [`create_trial()`](https://jameshwade.github.io/dsprrr/reference/create_trial.md).

- path:

  File path for the JSONL file.

- append:

  Whether to append to existing file. Default is FALSE.

## Value

Invisibly returns the path.

## Examples

``` r
if (FALSE) { # \dontrun{
trials <- list(
  create_trial("BootstrapFewShot", list(k = 4)),
  create_trial("BootstrapFewShot", list(k = 8))
)
write_trials_jsonl(trials, "trials.jsonl")
} # }
```
