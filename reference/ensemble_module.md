# Create an Ensemble Module

Factory function to create an Ensemble module that combines multiple
modules using a reduce function.

## Usage

``` r
ensemble(modules, reduce_fn = NULL, weights = NULL, ...)
```

## Arguments

- modules:

  A list of Module objects to combine

- reduce_fn:

  Function to combine outputs. Default is
  [`reduce_majority()`](https://jameshwade.github.io/dsprrr/reference/reduce_majority.md).
  Should accept a list of outputs and optional weights parameter.

- weights:

  Optional numeric vector of weights for each module. Useful for
  weighted voting based on validation performance.

- ...:

  Additional arguments passed to module constructor

## Value

An EnsembleModule object

## Examples

``` r
# Create multiple compiled modules
sig <- signature("question -> answer")
mod1 <- module(sig)
mod2 <- module(sig)
mod3 <- module(sig)

# Combine with majority voting
ens <- ensemble(list(mod1, mod2, mod3))

# With weighted voting based on validation scores
ens <- ensemble(
  list(mod1, mod2, mod3),
  reduce_fn = reduce_weighted_vote(),
  weights = c(0.9, 0.85, 0.8)
)

# With custom reduce function
custom_reduce <- function(outputs, weights = NULL) {
  # Take the longest answer
  lengths <- vapply(outputs, function(o) nchar(o$answer %||% ""), integer(1))
  outputs[[which.max(lengths)]]
}
ens <- ensemble(list(mod1, mod2, mod3), reduce_fn = custom_reduce)
```
