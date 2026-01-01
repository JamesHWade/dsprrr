# Write Trials to JSONL File

Write a list of Trial objects to a JSONL (JSON Lines) file. Each trial
is written as a single JSON object on its own line.

## Usage

``` r
write_trials_jsonl(trials, path, append = FALSE)
```

## Arguments

- trials:

  List of Trial objects.

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
