# Majority Vote Reducer

Creates a reduce function that returns the most common output among the
ensemble members. For structured outputs (lists), compares by the first
field or a specified field.

## Usage

``` r
reduce_majority(field = NULL, tie_breaker = "first")
```

## Arguments

- field:

  Optional field name to use for voting when outputs are lists. If NULL,
  uses the first field of the output.

- tie_breaker:

  How to handle ties: "first" (default) returns the first occurrence,
  "random" picks randomly among tied values.

## Value

A reduce function for use with
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble_module.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic majority voting
ens <- ensemble(modules, reduce_fn = reduce_majority())

# Vote based on specific field
ens <- ensemble(modules, reduce_fn = reduce_majority(field = "sentiment"))
} # }
```
