# Convert dsprrr cost data to vitals format

Converts dsprrr cost summaries, trace data, or session costs into a
tibble matching the format returned by vitals
[vitals::Task](https://vitals.tidyverse.org/reference/Task.html)`$get_cost()`.
This enables consistent cost reporting across dsprrr and vitals
workflows.

## Usage

``` r
as_vitals_cost(x, source = "solver", ...)
```

## Arguments

- x:

  A dsprrr cost object. Can be:

  - A `dsprrr_cost_summary` (from
    [`get_cost()`](https://jameshwade.github.io/dsprrr/reference/get_cost.md))

  - A `dsprrr_session_cost` (from
    [`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md))

  - A tibble of traces (from
    [`export_traces()`](https://jameshwade.github.io/dsprrr/reference/export_traces.md))

  - A `dsprrr_evaluation` result (from
    [`evaluate()`](https://jameshwade.github.io/dsprrr/reference/evaluate.md))

- source:

  Character string identifying the source of costs. Defaults to
  `"solver"` to match vitals convention.

- ...:

  Additional arguments (currently unused).

## Value

A tibble with columns matching vitals cost format:

- `source`: Character, either "solver" or "scorer"

- `provider`: Character, the API provider name

- `model`: Character, the model name

- `input`: Integer, input token count

- `output`: Integer, output token count

- `price`: Character, formatted cost string (e.g., "\$0.01")

## Examples

``` r
if (FALSE) { # \dontrun{
# From session cost
as_vitals_cost(session_cost())

# From evaluation result
eval_result <- evaluate(mod, test_data, metric = metric_exact_match())
as_vitals_cost(eval_result)

# From module traces
traces <- export_traces(my_module)
as_vitals_cost(traces)
} # }
```
