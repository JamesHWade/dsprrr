# Create a Trial Record

Convenience function to create a Trial record with auto-generated ID.

## Usage

``` r
create_trial(
  optimizer_name,
  params = list(),
  trial_id = NULL,
  notes = "",
  trace_context = list()
)
```

## Arguments

- optimizer_name:

  Name of the optimizer.

- params:

  List of parameters for this trial.

- trial_id:

  Optional trial ID. If NULL, auto-generated.

- notes:

  Optional notes.

- trace_context:

  A named, JSON-compatible correlation context. When omitted during
  [`compile()`](https://jameshwade.github.io/dsprrr/reference/compile.md),
  the active compilation context is inherited; supply
  [`list()`](https://rdrr.io/r/base/list.html) explicitly to clear it.

## Value

A Trial object.

## Examples

``` r
trial <- create_trial(
  optimizer_name = "BootstrapFewShot",
  params = list(max_demos = 4, temperature = 0.7)
)
```
