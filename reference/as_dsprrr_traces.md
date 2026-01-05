# Convert vitals samples to dsprrr traces format

Converts vitals samples tibble (from `Task$get_samples()` or
[`vitals_bind()`](https://vitals.tidyverse.org/reference/vitals_bind.html))
to dsprrr traces format for use with dsprrr analysis functions like
[`summarize_traces()`](https://jameshwade.github.io/dsprrr/reference/summarize_traces.md).

## Usage

``` r
as_dsprrr_traces(samples, include_prompts = TRUE, include_outputs = TRUE)
```

## Arguments

- samples:

  A tibble from `Task$get_samples()` or
  [`vitals_bind()`](https://vitals.tidyverse.org/reference/vitals_bind.html).

- include_prompts:

  Logical; whether to extract prompts from input column. Defaults to
  `TRUE`.

- include_outputs:

  Logical; whether to extract outputs from result column. Defaults to
  `TRUE`.

## Value

A tibble with dsprrr trace columns:

- `timestamp`: Extracted from metadata or set to current time

- `latency_ms`: Extracted from metadata or NA

- `input_tokens`: Extracted from metadata or NA

- `output_tokens`: Extracted from metadata or NA

- `total_tokens`: Calculated or extracted from metadata

- `cost`: Extracted from metadata or NA

- `model`: Model name if available

- `prompt_length`: Character length of prompt

- `prompt`: Input text (if include_prompts = TRUE)

- `output`: Result (if include_outputs = TRUE)

## Examples

``` r
if (FALSE) { # \dontrun{
# Get samples from a vitals task
samples <- task$get_samples()

# Convert to dsprrr traces format
traces <- as_dsprrr_traces(samples)

# Use dsprrr analysis functions
summary <- summarize_traces_df(traces)
} # }
```
