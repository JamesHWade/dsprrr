# Convert dsprrr traces to vitals samples format

Converts dsprrr module traces to a tibble format compatible with vitals
`Task$get_samples()` output. This enables viewing dsprrr traces in
vitals' Inspect viewer or combining with vitals samples using
[`vitals_bind()`](https://vitals.tidyverse.org/reference/vitals_bind.html).

## Usage

``` r
as_vitals_samples(traces, input_column = NULL, include_chats = FALSE)
```

## Arguments

- traces:

  A traces tibble from
  [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md)
  or module `$get_traces()`.

- input_column:

  Column name containing the input prompts. If `NULL`, attempts to
  extract from the prompt field.

- include_chats:

  Logical; if `TRUE` and solver_chat column exists, include chat
  objects. Defaults to `FALSE`.

## Value

A tibble with columns matching vitals samples format:

- `id`: Unique identifier for each trace

- `input`: Input prompt text

- `result`: Model output

- `solver_metadata`: List column with trace metadata (latency, tokens,
  cost)

- `model`: Model name used

- `epoch`: Always 1 (dsprrr doesn't use epochs)

## Examples

``` r
if (FALSE) { # \dontrun{
# Export traces from a module and convert
traces <- export_traces(my_module, include_outputs = TRUE)
samples <- as_vitals_samples(traces)

# Use with vitals_bind for combined analysis
vitals::vitals_bind(
  task1 = task1,
  dsprrr = samples
)
} # }
```
