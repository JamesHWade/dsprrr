# Weighted Vote Reducer

Creates a reduce function that uses weighted voting, where each module's
vote is weighted by its weight (typically validation score).

## Usage

``` r
reduce_weighted_vote(field = NULL)
```

## Arguments

- field:

  Optional field name to use for voting when outputs are lists. If NULL,
  uses the first field of the output.

## Value

A reduce function for use with
[`ensemble()`](https://jameshwade.github.io/dsprrr/reference/ensemble_module.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Weighted voting with validation scores
ens <- ensemble(
  modules,
  reduce_fn = reduce_weighted_vote(),
  weights = c(0.9, 0.85, 0.75)
)
} # }
```
