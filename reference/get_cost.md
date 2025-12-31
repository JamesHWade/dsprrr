# Get cost from a result

Extract cost information from a DSPrrr result object.

## Usage

``` r
get_cost(x, ...)
```

## Arguments

- x:

  A DSPrrr result object

- ...:

  Additional arguments (unused)

## Value

A numeric value (for single results) or a `dsprrr_cost_summary` object
(for batch results and evaluations) containing:

- `costs`: A tibble with per-item costs

- `total`: Total cost across all items

- `n_missing`: Count of items with missing cost data

## See also

[`session_cost()`](https://jameshwade.github.io/dsprrr/reference/session_cost.md)
for session-level cost tracking

## Examples

``` r
if (FALSE) { # \dontrun{
result <- run(mod, text = "hello", .return_format = "structured")
get_cost(result)
} # }
```
